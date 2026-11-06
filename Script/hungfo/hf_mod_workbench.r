##################
# HungFo 环节 13 — CodeBuddy 工作台 (HF-WORKBENCH)
# 职责：网页输入任务 → 本地 CodeBuddy CLI（headless）执行 → 过程、结果与会话 Token 消耗实时回传网页。
# 实现：processx 异步 spawn `codebuddy -p "<task>" -y --output-format stream-json`，
#       stdout/stderr 落日志文件；Shiny 侧 reactivePoll 轮询日志实时展示，
#       运行记录持久化到 hf_workbench_runs（支持历史追溯）；
#       从 stream-json 事件的 usage 字段汇总每次会话的 Token 消耗并写回运行记录。
# 说明：本环节 id 取 hf13，字符串排序排在「权限体系 hf12」之后、「配置 hf20」之前。
##################

HF_MOD_WORKBENCH <- list(
  id = "hf13", key = "workbench", name = "CodeBuddy", icon = "desktop",
  desc = "网页提交任务给本地 CodeBuddy CLI 执行，过程、结果与会话 Token 消耗实时回传网页。",
  api_desc = "run_task(task) 派发 CodeBuddy CLI · list_runs() 运行记录 · read_log(run_id) 读日志 · token_summary() 会话 Token 汇总",
  status = "scaffold", version = "v0.2"
)

# ── 建表（自包含，供 hf_ui.r 初始化时调用；CREATE IF NOT EXISTS 幂等）──
hf_workbench_init <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_workbench_runs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id TEXT UNIQUE,
      task TEXT,
      status TEXT DEFAULT 'running',   -- running / done / error
      cli TEXT DEFAULT '',
      log_path TEXT DEFAULT '',
      created_by INTEGER,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      finished_at TEXT
    )")
    # Token 列兼容：旧库逐列 ALTER 追加（幂等），不影响已有运行记录
    have <- dbGetQuery(con, "PRAGMA table_info(hf_workbench_runs)")$name
    token_cols <- list(
      token_in = "INTEGER DEFAULT 0", token_out = "INTEGER DEFAULT 0",
      token_cache_read = "INTEGER DEFAULT 0", token_cache_write = "INTEGER DEFAULT 0",
      token_total = "INTEGER DEFAULT 0",
      # 模型 / 成本 / 进程管控字段（旧库逐列 ALTER，不影响已有运行记录）
      model = "TEXT DEFAULT ''", effort = "TEXT DEFAULT ''",
      fallback_model = "TEXT DEFAULT ''", max_turns = "INTEGER DEFAULT 0",
      task_type = "TEXT DEFAULT ''", matched_categories = "TEXT DEFAULT ''",
      context_file = "TEXT DEFAULT ''", context_rules = "INTEGER DEFAULT 0",
      context_memory = "INTEGER DEFAULT 0", cwd = "TEXT DEFAULT ''",
      timeout_sec = "INTEGER DEFAULT 0", exit_code = "INTEGER",
      cost_usd = "REAL DEFAULT 0", num_turns = "INTEGER DEFAULT 0",
      duration_ms = "INTEGER DEFAULT 0")
    for (nm in names(token_cols)) {
      if (!nm %in% have)
        dbExecute(con, sprintf("ALTER TABLE hf_workbench_runs ADD COLUMN %s %s", nm, token_cols[[nm]]))
    }
    invisible(TRUE)
  }, error = function(e) {
    warning(paste("hf_workbench_init 建表失败:", e$message))
    invisible(FALSE)
  })
}

# ── 解析 CodeBuddy CLI 运行方式 ──
# 返回 list(command, pre)：command 为可执行文件（node.exe），pre 为前置参数（codebuddy bin 脚本）。
# 优先用 node 直接运行 codebuddy-code 的 bin 脚本，避免 .cmd 包装导致 stdout 重定向丢失；
# 找不到时回退到系统 PATH 里的 codebuddy(.cmd)。
hf_workbench_cli_spec <- function() {
  node <- Sys.which("node")
  appdata <- Sys.getenv("APPDATA", unset = "")
  bins <- character(0)
  if (nzchar(appdata)) {
    bins <- c(
      file.path(appdata, "npm", "node_modules", "@tencent-ai", "codebuddy-code", "bin", "codebuddy"),
      file.path(appdata, "npm", "node_modules", "@tencent-ai", "codebuddy-code", "bin", "codebuddy.js"))
  }
  if (nzchar(node)) {
    for (b in bins) if (nzchar(b) && file.exists(b)) return(list(command = node, pre = b))
  }
  for (nm in c("codebuddy", "codebuddy-code")) {
    p <- Sys.which(nm)
    if (nzchar(p)) return(list(command = p, pre = ""))
  }
  NULL
}

# ── CLI 描述（用于记录/展示）──
hf_workbench_cli <- function() {
  spec <- hf_workbench_cli_spec()
  if (is.null(spec)) return("")
  paste(c(spec$command, spec$pre), collapse = " ")
}

# ── CLI 可用模型清单 ──
# 说明：CodeBuddy CLI 没有 list-models 命令；`config get model` 在本机会因守护进程
# 端口占用（127.0.0.1:62541）报 EADDRINUSE，因此改为解析 `--help` 中 `--model` 一行的
# "Currently supported: (...)" 列表，随 CLI 版本自动更新；解析失败才回退到静态清单。
HF_FALLBACK_MODELS <- c(
  "fast-model", "balanced-model", "deep-model", "hy4free", "hy4std", "hy3", "hy3-x",
  "deepseek-v4.1-flash", "glm-5.3", "glm-5.3-flash", "glm-5.3-flashx", "glm-5.2",
  "space-bunny", "minimax-m3.1-flash", "minimax-m3-pay", "kimi-k3.1-pro-highspeed",
  "kimi-k3.1-pro", "kimi-k3-2", "kimi-k2.8-preview", "deepseek-v4-pro",
  "step-5-preview", "mimo-v2.6-pro",
  "custom-local:Tai_Qwen3_Coder_30B-A3B", "custom-local:Tai_DeepSeek-r1:1.5b")

hf_workbench_models <- function(force = FALSE) {
  if (!isTRUE(force) && !is.null(hf_bus$wb_models) && length(hf_bus$wb_models) > 0)
    return(hf_bus$wb_models)
  spec <- hf_workbench_cli_spec()
  ids <- character(0)
  if (!is.null(spec)) {
    call_args <- c(if (nzchar(spec$pre)) spec$pre else character(0), "--help")
    out <- tryCatch(
      suppressWarnings(system2(spec$command, call_args, stdout = TRUE, stderr = TRUE)),
      error = function(e) character(0))
    txt <- paste(as.character(out), collapse = "\n")
    hit <- regmatches(txt, regexpr("Currently supported:\\s*\\(([^)]*)\\)", txt, perl = TRUE))
    if (length(hit) > 0) {
      inner <- sub("\\)$", "", sub("^Currently supported:\\s*\\(", "", hit[1]))
      ids <- trimws(strsplit(inner, ",")[[1]])
      ids <- ids[nzchar(ids)]
    }
  }
  if (length(ids) == 0) ids <- HF_FALLBACK_MODELS
  hf_bus$wb_models <- ids
  ids
}

hf_workbench_model_choices <- function() {
  ids <- hf_workbench_models()
  c("跟随路由 / CLI 默认" = "", "（强制账号默认，不传 --model）" = "__none__",
    stats::setNames(ids, ids))
}

# ── 挑一个空闲本地端口 ──
# 背景：CodeBuddy CLI 与 WorkBuddy 共用一个守护进程端口（本机 127.0.0.1:62541）。
# 只要 WorkBuddy 在跑，新 spawn 的 headless CLI 就会报 EADDRINUSE 直接退出。
# CLI 的 --port 可为该进程单独指定端口，绕开冲突（已用真实 CLI 验证）。
hf_workbench_free_port <- function() {
  for (k in seq_len(40)) {
    p <- 63000L + sample.int(2000L, 1)
    busy <- tryCatch({
      sc <- socketConnection("127.0.0.1", port = p, blocking = FALSE, open = "a+b", timeout = 1)
      close(sc); TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(busy)) return(as.integer(p))
  }
  as.integer(63000L + sample.int(2000L, 1))
}

# ── 纯函数：拼装 CLI 参数（便于不启动进程也能做参数级验证）──
hf_workbench_build_args <- function(spec, task, opts = list()) {
  pre <- character(0)
  add <- function(flag, val) {
    v <- hf_or(val, "")
    if (nzchar(as.character(v))) pre <<- c(pre, flag, as.character(v))
  }
  # --port 必须放在 -p 之前（commander 在子命令上下文里不认全局选项）
  pv <- suppressWarnings(as.integer(hf_or(opts$port, 0)))
  if (!is.na(pv) && pv > 0) pre <- c(pre, "--port", as.character(pv))
  add("--model", opts$model)
  add("--fallback-model", opts$fallback_model)
  add("--effort", opts$effort)
  mt <- suppressWarnings(as.integer(hf_or(opts$max_turns, 0)))
  if (!is.na(mt) && mt > 0) pre <- c(pre, "--max-turns", as.character(mt))
  add("--system-prompt-file", opts$system_prompt_file)
  args <- c(spec$pre, pre, "-p", as.character(task), "-y", "--output-format", "stream-json")
  args[nzchar(args)]
}

# ── 运行日志目录（挂在 www 下，便于监控页/调试直接访问）──
hf_workbench_logdir <- function() {
  d <- file.path(hf_home(), "www", "workbench_logs")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# ── 进程句柄表（run_id -> processx 进程对象，跨 reactive 轮询状态）──
if (is.null(hf_bus$workbench_procs)) hf_bus$workbench_procs <- list()
if (is.null(hf_bus$workbench_meta))  hf_bus$workbench_meta  <- list()

# ── 派发任务：异步启动 CodeBuddy CLI，输出落日志文件 ──
# 新增能力：按任务类型路由模型/effort/降级模型/最大轮数、按分类装配规则+记忆
#   （--system-prompt-file）、显式 cwd、超时与取消、并发上限、运行记录成本留痕。
# 所有新参数均有默认值，旧调用 hf_workbench_run(task, created_by) 行为保持不变。
hf_workbench_run <- function(task, created_by = NULL, model = NULL, effort = NULL,
                             fallback_model = NULL, max_turns = NULL, task_type = NULL,
                             category = NULL, timeout_sec = NULL, cwd = NULL,
                             inject_context = NULL) {
  if (is.null(task) || !nzchar(trimws(task))) return(list(success = FALSE, message = "任务内容不能为空"))
  spec <- hf_workbench_cli_spec()
  if (is.null(spec)) {
    return(list(success = FALSE,
      message = "未检测到 codebuddy CLI，请先执行 npm install -g @tencent-ai/codebuddy-code 并 codebuddy login"))
  }
  hf_workbench_init()   # 幂等：确保新增列在旧库上存在

  # 1) 路由表补齐默认值（模型 / effort / 轮数 / 分类）
  tt <- hf_or(task_type, "default")
  if (!nzchar(tt)) tt <- "default"
  route <- tryCatch(hf_model_route_get(tt), error = function(e) NULL)
  dflt  <- tryCatch(hf_cb_defaults(), error = function(e)
    list(cwd = "", timeout_sec = 1800, max_concurrent = 3,
         inject_context = TRUE, include_memory = TRUE))
  if (is.null(route)) route <- list(model = "", effort = "", max_turns = 0, fallback_model = "")
  pick <- function(explicit, fallback) if (is.null(explicit)) hf_or(fallback, "") else as.character(explicit)
  model          <- pick(model, route$model)
  effort         <- pick(effort, route$effort)
  fallback_model <- pick(fallback_model, route$fallback_model)
  max_turns <- if (is.null(max_turns)) suppressWarnings(as.integer(hf_or(route$max_turns, 0)))
               else suppressWarnings(as.integer(max_turns))
  if (is.na(max_turns)) max_turns <- 0L
  timeout_sec <- if (is.null(timeout_sec)) as.integer(dflt$timeout_sec) else as.integer(timeout_sec)
  if (is.na(timeout_sec) || timeout_sec < 0) timeout_sec <- 0L
  workdir <- if (!is.null(cwd) && nzchar(cwd)) cwd else hf_or(dflt$cwd, "")
  if (!nzchar(workdir) || !dir.exists(workdir)) workdir <- getwd()
  do_ctx <- if (is.null(inject_context)) isTRUE(dflt$inject_context) else isTRUE(inject_context)

  # 2) 并发上限：避免多个 CLI 进程互相抢占守护端口 / 打爆机器
  con0 <- hf_db_connect()
  running_n <- tryCatch(dbGetQuery(con0,
    "SELECT COUNT(*) n FROM hf_workbench_runs WHERE status='running'")$n[1],
    error = function(e) 0)
  try(hf_db_disconnect(con0), silent = TRUE)
  lim <- suppressWarnings(as.integer(hf_or(dflt$max_concurrent, 3)))
  if (is.na(lim) || lim < 1) lim <- 3L
  if (!is.na(running_n) && running_n >= lim)
    return(list(success = FALSE,
      message = sprintf("当前已有 %d 个运行中任务（上限 %d），请先取消或等待完成", running_n, lim)))

  # 3) 按分类装配规则 + 记忆，写成 system prompt 文件精准注入
  ctx <- NULL; ctx_file <- ""; cats <- ""
  if (isTRUE(do_ctx)) {
    ctx <- tryCatch(
      hf_ai_context_text(task_type = tt, category = category,
                         include_memory = isTRUE(dflt$include_memory)),
      error = function(e) NULL)
    if (!is.null(ctx)) {
      cats <- paste(ctx$categories, collapse = "、")
      run_id_tmp <- sprintf("wb%s%03d", format(Sys.time(), "%Y%m%d%H%M%S"), sample.int(999, 1))
      ctx_file <- tryCatch(hf_ai_context_write(ctx$text, run_id_tmp), error = function(e) NULL)
      if (is.null(ctx_file)) ctx_file <- ""
    }
  }

  cli_desc <- paste(c(spec$command, spec$pre), collapse = " ")
  run_id <- if (exists("run_id_tmp", inherits = FALSE)) run_id_tmp else
    sprintf("wb%s%03d", format(Sys.time(), "%Y%m%d%H%M%S"), sample.int(999, 1))
  if (nzchar(ctx_file) && !grepl(run_id, basename(ctx_file), fixed = TRUE)) {
    # 上下文文件与 run_id 对齐（便于追溯）
    newp <- tryCatch(hf_ai_context_write(hf_or(ctx$text, ""), run_id), error = function(e) NULL)
    if (!is.null(newp)) ctx_file <- newp
  }
  logfile <- file.path(hf_workbench_logdir(), paste0(run_id, ".log"))

  # 4) 参数拼装（纯函数，便于测试）；auto 模式每次挑空闲端口避开守护端口冲突
  cli_port <- if (identical(as.character(hf_or(dflt$port_mode, "auto")), "auto"))
    hf_workbench_free_port() else 0L
  args <- hf_workbench_build_args(spec, task, list(
    port = cli_port, model = model, effort = effort, fallback_model = fallback_model,
    max_turns = max_turns, system_prompt_file = ctx_file))

  # 5) 落运行记录（所有字段留痕，便于成本/模型台账）
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  ins <- tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_workbench_runs
       (run_id,task,status,cli,log_path,created_by,model,effort,fallback_model,max_turns,
        task_type,matched_categories,context_file,context_rules,context_memory,cwd,timeout_sec)
       VALUES ('%s','%s','running','%s','%s',%s,'%s','%s','%s',%d,'%s','%s','%s',%d,%d,'%s',%d)",
      run_id, hf_esc(task), hf_esc(cli_desc), hf_esc(logfile),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by)),
      hf_esc(model), hf_esc(effort), hf_esc(fallback_model), as.integer(max_turns),
      hf_esc(tt), hf_esc(cats), hf_esc(ctx_file),
      if (is.null(ctx)) 0L else as.integer(ctx$rules_n),
      if (is.null(ctx)) 0L else as.integer(ctx$memory_n),
      hf_esc(workdir), as.integer(timeout_sec)))
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ins)) {
    # 旧库极端情况（列仍不存在）回退为最小 INSERT，不让派发中断
    tryCatch(dbExecute(con, sprintf(
      "INSERT INTO hf_workbench_runs (run_id,task,status,cli,log_path,created_by)
       VALUES ('%s','%s','running','%s','%s',%s)",
      run_id, hf_esc(task), hf_esc(cli_desc), hf_esc(logfile),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by)))),
      error = function(e) NULL)
  }

  # 6) 异步启动（显式 wd，避免规则加载依赖进程继承目录）
  pr <- tryCatch({
    if (requireNamespace("processx", quietly = TRUE)) {
      processx::process$new(spec$command, args,
        stdout = logfile, stderr = logfile, cleanup = FALSE, wd = workdir)
    } else {
      system2(spec$command, args, stdout = logfile, stderr = logfile, wait = FALSE)
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(pr)) {
    tryCatch(dbExecute(con, sprintf(
      "UPDATE hf_workbench_runs SET status='error', finished_at=datetime('now','localtime') WHERE run_id='%s'", run_id)),
      error = function(e) NULL)
    return(list(success = FALSE, message = "启动 CodeBuddy 进程失败，请确认 CLI 已安装并登录"))
  }
  hf_bus$workbench_procs[[run_id]] <- pr
  hf_bus$workbench_meta[[run_id]] <- list(
    started = as.numeric(Sys.time()), timeout_sec = as.integer(timeout_sec),
    model = model, task_type = tt, args = args, port = as.integer(cli_port))
  list(success = TRUE, run_id = run_id, message = paste0("已派发任务 ", run_id),
       model = model, task_type = tt, categories = cats,
       context_rules = if (is.null(ctx)) 0L else ctx$rules_n,
       context_memory = if (is.null(ctx)) 0L else ctx$memory_n,
       args = args)
}

# ── 取消运行中的任务（按钮调用；进程可能已结束，做幂等处理）──
hf_workbench_cancel <- function(run_id) {
  if (is.null(run_id) || !nzchar(run_id)) return(list(success = FALSE, message = "缺少 run_id"))
  p <- hf_bus$workbench_procs[[run_id]]
  killed <- FALSE
  if (!is.null(p)) {
    killed <- tryCatch({ p$kill(); TRUE }, error = function(e) FALSE)
  }
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbExecute(con, sprintf(
    "UPDATE hf_workbench_runs SET status='cancelled', finished_at=datetime('now','localtime')
     WHERE run_id='%s' AND status='running'", hf_esc(run_id))), error = function(e) NULL)
  hf_bus$workbench_procs[[run_id]] <- NULL
  hf_bus$workbench_meta[[run_id]] <- NULL
  list(success = TRUE, message = if (killed) "已终止该任务" else "任务已结束或进程句柄不可用，状态已更新")
}

# ── 从 stream-json 的 result 事件读取成本 / 轮数 / 耗时（真实数据，不估算）──
hf_workbench_result_meta <- function(logfile) {
  empty <- list(cost_usd = 0, num_turns = 0, duration_ms = 0, model = "")
  if (is.null(logfile) || is.na(logfile) || !nzchar(logfile) || !file.exists(logfile)) return(empty)
  lines <- tryCatch(readLines(logfile, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (length(lines) == 0) return(empty)
  res <- empty
  for (ln in lines) {
    if (!nzchar(ln) || substr(ln, 1, 1) != "{") next
    obj <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(obj)) next
    typ <- if (is.null(obj$type)) "" else as.character(obj$type)[1]
    if (identical(typ, "system") && identical(as.character(hf_or(obj$subtype, "")), "init")) {
      res$model <- as.character(hf_or(obj$model, ""))
    }
    if (identical(typ, "result")) {
      num <- function(x) { v <- suppressWarnings(as.numeric(hf_or(x, 0))); if (is.na(v)) 0 else v }
      res$cost_usd    <- num(obj$total_cost_usd)
      res$num_turns   <- num(obj$num_turns)
      res$duration_ms <- num(obj$duration_ms)
    }
  }
  res
}

# ── 查询运行记录（最近 50 条）──
hf_workbench_list <- function(limit = 50) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_workbench_runs ORDER BY id DESC LIMIT %d", as.integer(limit))),
    error = function(e) data.frame())
}

# ── 单条记录 ──
hf_workbench_get <- function(run_id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_workbench_runs WHERE run_id='%s'", hf_esc(run_id))),
    error = function(e) data.frame())
}

# ── 日志文件路径 ──
hf_workbench_logfile <- function(run_id) {
  r <- hf_workbench_get(run_id)
  if (is.null(r) || nrow(r) == 0) return(NULL)
  lp <- r$log_path[1]
  if (is.na(lp) || is.null(lp) || !nzchar(lp)) return(NULL)
  lp
}

# ── 运行状态 ──
hf_workbench_status <- function(run_id) {
  r <- hf_workbench_get(run_id)
  if (is.null(r) || nrow(r) == 0) return("")
  st <- r$status[1]
  if (is.na(st) || is.null(st)) "" else st
}

# ── 读取日志（末尾 N 行，UTF-8）──
hf_workbench_log <- function(run_id, max_lines = 500) {
  lp <- hf_workbench_logfile(run_id)
  if (is.null(lp) || !file.exists(lp)) return("")
  lines <- tryCatch(readLines(lp, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (length(lines) == 0) return("")
  if (length(lines) > max_lines) lines <- tail(lines, max_lines)
  paste(lines, collapse = "\n")
}

# ── 结构化解析 stream-json 日志（供网页以可读时间线展示，替代原始 JSON 纯文本）──
# 每行一个 JSON 事件，按 content 块拆成：思考 / 回复 / 工具调用 / 工具结果 / 结果汇总 / 会话元信息。
# 工具结果按 tool_use_id 回填到对应工具调用，形成「调用 → 结果」配对；长文本截断、事件限量。
# 解析结果按日志 mtime+size 缓存，仅在文件增长时重算，配合 reactivePoll 轮询不会重复解析。
if (is.null(hf_bus$workbench_events_cache)) hf_bus$workbench_events_cache <- list()

hf_workbench_events <- function(run_id, max_events = 100, text_limit = 600) {
  empty <- list(events = list(), dropped = 0L, nonjson = 0L, total_lines = 0L, log_url = "")
  lp <- hf_workbench_logfile(run_id)
  if (is.null(lp) || !file.exists(lp)) return(empty)
  info <- file.info(lp)
  sig <- paste0(as.numeric(info$mtime), "|", info$size)
  ck <- hf_bus$workbench_events_cache[[lp]]
  if (!is.null(ck) && identical(ck$sig, sig)) return(ck$value)

  lines <- tryCatch(readLines(lp, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (length(lines) == 0) return(empty)

  str1 <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    v <- x[[1]]
    if (is.list(v)) v <- paste(unlist(v, use.names = FALSE), collapse = " ")
    as.character(hf_or(v, ""))
  }
  num1 <- function(x) {
    if (is.null(x) || length(x) == 0) return(0)
    v <- suppressWarnings(as.numeric(x[[1]]))
    if (is.na(v)) 0 else v
  }
  clip <- function(s, n = text_limit) {
    s <- as.character(hf_or(s, ""))
    if (is.null(n)) return(list(text = s, omitted = 0))   # NULL = 全文直显，不截断
    k <- nchar(s)
    if (k > n) list(text = substr(s, 1, n), omitted = k - n) else list(text = s, omitted = 0)
  }
  # tool_result.content 可能是字符串，也可能是 [{type:text,text:...}] 数组
  block_text <- function(c) {
    if (is.null(c)) return("")
    if (is.character(c)) return(paste(c, collapse = "\n"))
    if (is.list(c)) {
      parts <- vapply(c, function(x) {
        if (is.list(x)) str1(x$text) else as.character(hf_or(x, ""))
      }, character(1))
      return(paste(parts, collapse = "\n"))
    }
    as.character(c)[1]
  }
  # 工具调用的一行摘要：优先展示命令/路径/检索词，便于快速扫读
  brief <- function(input) {
    if (!is.list(input)) return("")
    keys <- c("command", "file_path", "path", "pattern", "description", "query", "url", "skill", "tool_names")
    for (k in keys) {
      v <- input[[k]]
      if (is.null(v)) next
      v <- str1(v)
      if (!nzchar(v)) next
      v <- gsub("\\s+", " ", v)
      if (nchar(v) > 100) v <- paste0(substr(v, 1, 100), "…")
      return(paste0(k, ": ", v))
    }
    ""
  }

  events <- list()
  add <- function(e) events[[length(events) + 1L]] <<- e
  results <- list()
  nonjson <- 0L

  for (ln in lines) {
    s <- trimws(ln)
    if (!nzchar(s)) next
    if (substr(s, 1, 1) != "{") { nonjson <- nonjson + 1L; next }
    obj <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(obj)) { nonjson <- nonjson + 1L; next }
    typ <- str1(obj$type)

    if (identical(typ, "system") && identical(str1(obj$subtype), "init")) {
      add(list(kind = "meta", model = str1(obj$model), cwd = str1(obj$cwd),
               permission = str1(obj$permissionMode)))
    } else if (typ %in% c("assistant", "user")) {
      cont <- obj$message$content
      if (is.character(cont)) cont <- list(list(type = "text", text = cont))
      if (!is.list(cont)) next
      for (b in cont) {
        if (!is.list(b)) next
        bt <- str1(b$type)
        if (identical(bt, "thinking")) {
          # 思考过程全文直显：不折叠、不截断，无需再「查看详细日志」
          add(list(kind = "thinking", text = clip(str1(b$thinking), n = NULL)))
        } else if (identical(bt, "text")) {
          add(list(kind = if (identical(typ, "user")) "user_text" else "text",
                   text = clip(str1(b$text))))
        } else if (identical(bt, "tool_use")) {
          add(list(kind = "tool", id = str1(b$id), name = str1(b$name),
                   brief = brief(b$input),
                   input = clip(tryCatch(
                     as.character(jsonlite::toJSON(b$input, auto_unbox = TRUE, pretty = TRUE)),
                     error = function(e) ""))))
        } else if (identical(bt, "tool_result")) {
          tid <- str1(b$tool_use_id)
          if (nzchar(tid)) results[[tid]] <- list(
            text = clip(block_text(b$content)), is_error = isTRUE(b$is_error))
        }
      }
    } else if (identical(typ, "result")) {
      add(list(kind = "result", ok = !isTRUE(obj$is_error),
               text = clip(str1(obj$result), text_limit * 3),
               duration_ms = num1(obj$duration_ms), num_turns = num1(obj$num_turns),
               cost = str1(obj$total_cost_usd)))
    }
    # file-history-snapshot / system:status 等噪音事件不展示
  }

  # 工具结果回填到对应工具调用；未配对的（子代理/中途结束）单独成卡
  used <- character(0)
  if (length(events)) {
    for (i in seq_along(events)) {
      if (!identical(events[[i]]$kind, "tool")) next
      tid <- events[[i]]$id
      if (nzchar(tid) && !is.null(results[[tid]])) {
        events[[i]]$result <- results[[tid]]
        used <- c(used, tid)
      } else {
        events[[i]]$result <- NULL
      }
    }
  }
  for (tid in setdiff(names(results), used)) {
    add(list(kind = "orphan_result", id = tid, text = results[[tid]]$text,
             is_error = results[[tid]]$is_error))
  }

  dropped <- 0L
  if (length(events) > max_events) {
    dropped <- length(events) - max_events
    events <- tail(events, max_events)
  }
  res <- list(events = events, dropped = dropped, nonjson = nonjson,
              total_lines = length(lines),
              log_url = paste0("hungfo/workbench_logs/", basename(lp)))
  hf_bus$workbench_events_cache[[lp]] <- list(sig = sig, value = res)
  res
}

# ── 会话 Token 消耗：解析 CLI stream-json 日志中的 usage 字段 ──
# 每行一个 JSON 事件，assistant 事件的 message.usage 记录该轮请求的
# input / output / cache_read / cache_creation Token；累加得本次会话总消耗。
hf_workbench_usage_parse <- function(logfile) {
  zero <- list(token_in = 0, token_out = 0, token_cache_read = 0,
               token_cache_write = 0, token_total = 0)
  if (is.null(logfile) || is.na(logfile) || !nzchar(logfile) || !file.exists(logfile)) return(zero)
  lines <- tryCatch(readLines(logfile, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (length(lines) == 0) return(zero)
  num <- function(x) {
    if (is.null(x) || length(x) == 0) return(0)
    v <- suppressWarnings(as.numeric(x[1]))
    if (is.na(v)) 0 else v
  }
  tin <- 0; tout <- 0; tcr <- 0; tcw <- 0
  for (ln in lines) {
    if (!nzchar(ln) || substr(ln, 1, 1) != "{") next
    if (!grepl('"usage"', ln, fixed = TRUE)) next
    obj <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(obj)) next
    u <- if (is.list(obj$usage)) obj$usage else NULL
    if (is.null(u) && is.list(obj$message) && is.list(obj$message$usage)) u <- obj$message$usage
    if (is.null(u)) next
    tin <- tin + num(u$input_tokens)
    tout <- tout + num(u$output_tokens)
    tcr <- tcr + num(u$cache_read_input_tokens)
    tcw <- tcw + num(u$cache_creation_input_tokens)
  }
  list(token_in = tin, token_out = tout, token_cache_read = tcr,
       token_cache_write = tcw, token_total = tin + tout + tcr + tcw)
}

# ── 带缓存的用量读取：仅当日志文件 mtime+size 变化才重新解析 ──
if (is.null(hf_bus$workbench_usage_cache)) hf_bus$workbench_usage_cache <- list()

hf_workbench_usage <- function(logfile) {
  zero <- list(token_in = 0, token_out = 0, token_cache_read = 0,
               token_cache_write = 0, token_total = 0)
  if (is.null(logfile) || is.na(logfile) || !nzchar(logfile) || !file.exists(logfile)) return(zero)
  info <- file.info(logfile)
  sig <- paste0(as.numeric(info$mtime), "|", info$size)
  ck <- hf_bus$workbench_usage_cache[[logfile]]
  if (!is.null(ck) && identical(ck$sig, sig)) return(ck$usage)
  u <- hf_workbench_usage_parse(logfile)
  hf_bus$workbench_usage_cache[[logfile]] <- list(sig = sig, usage = u)
  u
}

# ── 会话 Token 汇总（历史全部 / 今日 / 本周 / 本月），供页面 KPI 展示 ──
# 口径说明（created_at 为 TEXT，写入值形如 'YYYY-MM-DD HH:MM:SS'，SQLite date()/strftime() 可直接解析）：
#   total = 历史全部：不加时间过滤（与旧版一致）
#   today = 今日：date(created_at)=date('now','localtime')（与旧版一致）
#   week  = 本周：采用「近 7 天含今日」口径 date(created_at) >= date('now','localtime','-6 day')
#           （若需改为自然周周一起算，把该条件换成
#             date(created_at) >= date('now','localtime','weekday 1','-7 day') 即可，其余不动）
#   month = 本月：strftime('%Y-%m', created_at) = strftime('%Y-%m','now','localtime')
# 返回 list(total, today, week, month)；每组均为
#   list(token_in, token_out, token_cache_read, token_cache_write, token_total, sessions)
# 保持 total / today 原有结构与字段不变，其它调用点（含 API 透传）不会崩。
hf_workbench_summary <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  mk <- function(s) list(
    token_in = s$tin[1], token_out = s$tout[1], token_cache_read = s$tcr[1],
    token_cache_write = s$tcw[1], token_total = s$tt[1], sessions = s$n[1])
  zero <- list(token_in = 0, token_out = 0, token_cache_read = 0,
               token_cache_write = 0, token_total = 0, sessions = 0)
  base <- "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COALESCE(SUM(token_cache_read),0) tcr, COALESCE(SUM(token_cache_write),0) tcw, COALESCE(SUM(token_total),0) tt, COUNT(*) n FROM hf_workbench_runs"
  tryCatch({
    a  <- dbGetQuery(con, base)
    d  <- dbGetQuery(con, paste(base, "WHERE date(created_at)=date('now','localtime')"))
    w  <- dbGetQuery(con, paste(base, "WHERE date(created_at) >= date('now','localtime','-6 day')"))
    mo <- dbGetQuery(con, paste(base, "WHERE strftime('%Y-%m', created_at) = strftime('%Y-%m','now','localtime')"))
    list(total = mk(a), today = mk(d), week = mk(w), month = mk(mo))
  }, error = function(e) list(total = zero, today = zero, week = zero, month = zero))
}

# ── 刷新运行状态：同步 Token 消耗 + 检查 running 进程是否结束 ──
hf_workbench_refresh_status <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    runs <- dbGetQuery(con, "SELECT * FROM hf_workbench_runs")
    if (is.null(runs) || nrow(runs) == 0) return(invisible(NULL))
    now_num <- as.numeric(Sys.time())
    for (i in seq_len(nrow(runs))) {
      rid <- runs$run_id[i]
      lp <- runs$log_path[i]
      # 1) Token 列实时同步（缓存解析，仅在数值变化时写库）
      u <- hf_workbench_usage(lp)
      if (u$token_total != hf_or(runs$token_total[i], 0)) {
        dbExecute(con, sprintf(
          "UPDATE hf_workbench_runs SET token_in=%.0f, token_out=%.0f, token_cache_read=%.0f, token_cache_write=%.0f, token_total=%.0f WHERE run_id='%s'",
          u$token_in, u$token_out, u$token_cache_read, u$token_cache_write,
          u$token_total, hf_esc(rid)))
      }
      # 2) 从 result 事件同步真实成本 / 轮数 / 耗时 / 实际模型
      rm <- hf_workbench_result_meta(lp)
      if (rm$num_turns > 0 || rm$cost_usd > 0 || rm$duration_ms > 0 || nzchar(rm$model)) {
        dbExecute(con, sprintf(
          "UPDATE hf_workbench_runs SET cost_usd=%.6f, num_turns=%d, duration_ms=%d,
           model=CASE WHEN model IS NULL OR model='' THEN '%s' ELSE model END WHERE run_id='%s'",
          rm$cost_usd, as.integer(rm$num_turns), as.integer(rm$duration_ms),
          hf_esc(rm$model), hf_esc(rid)))
      }
      # 3) 超时保护：超时即终止进程并标记 timeout，避免僵尸任务长期占并发额度
      meta <- hf_bus$workbench_meta[[rid]]
      if (identical(runs$status[i], "running") && !is.null(meta) &&
          is.numeric(meta$timeout_sec) && meta$timeout_sec > 0 &&
          (now_num - meta$started) > meta$timeout_sec) {
        p <- hf_bus$workbench_procs[[rid]]
        if (!is.null(p)) tryCatch(p$kill(), error = function(e) NULL)
        dbExecute(con, sprintf(
          "UPDATE hf_workbench_runs SET status='timeout', finished_at=datetime('now','localtime') WHERE run_id='%s'",
          hf_esc(rid)))
        hf_bus$workbench_procs[[rid]] <- NULL
        hf_bus$workbench_meta[[rid]] <- NULL
        next
      }
      # 4) 结束检测
      if (!identical(runs$status[i], "running")) next
      p <- hf_bus$workbench_procs[[rid]]
      alive <- if (!is.null(p)) tryCatch(isTRUE(p$is_alive()), error = function(e) TRUE) else FALSE
      if (alive) next
      has_log <- !is.na(lp) && nzchar(lp) && file.exists(lp) && file.info(lp)$size > 0
      st <- if (has_log) "done" else "error"
      ec <- if (!is.null(p)) tryCatch(p$get_exit_status(), error = function(e) NULL) else NULL
      dbExecute(con, sprintf(
        "UPDATE hf_workbench_runs SET status='%s', exit_code=%s, finished_at=datetime('now','localtime') WHERE run_id='%s'",
        st, if (is.null(ec) || is.na(ec)) "NULL" else as.character(as.integer(ec)), hf_esc(rid)))
      hf_bus$workbench_procs[[rid]] <- NULL
      hf_bus$workbench_meta[[rid]] <- NULL
    }
    invisible(NULL)
  }, error = function(e) NULL)
}

# ── 对外接口透传 ──
hf_workbench_api_run_task <- function(task, created_by = NULL, ...) hf_workbench_run(task, created_by, ...)
hf_workbench_api_list_runs <- function(limit = 50) hf_workbench_list(limit)
hf_workbench_api_read_log <- function(run_id) hf_workbench_log(run_id)
hf_workbench_api_token_summary <- function() hf_workbench_summary()
hf_workbench_api_models <- function(force = FALSE) hf_workbench_models(force)
hf_workbench_api_cancel <- function(run_id) hf_workbench_cancel(run_id)
hf_workbench_api_routes <- function() hf_model_routes()
hf_workbench_api_context <- function(task_type = NULL, category = NULL)
  hf_ai_context_text(task_type = task_type, category = category)

hf_register(HF_MOD_WORKBENCH, hf_make_api(
  "workbench",
  run_task = hf_workbench_api_run_task,
  list_runs = hf_workbench_api_list_runs,
  read_log = hf_workbench_api_read_log,
  token_summary = hf_workbench_api_token_summary,
  models = hf_workbench_api_models,
  cancel = hf_workbench_api_cancel,
  routes = hf_workbench_api_routes,
  context = hf_workbench_api_context
))
