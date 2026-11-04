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
      token_total = "INTEGER DEFAULT 0")
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

# ── 运行日志目录（挂在 www 下，便于监控页/调试直接访问）──
hf_workbench_logdir <- function() {
  d <- file.path(hf_home(), "www", "workbench_logs")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# ── 进程句柄表（run_id -> processx 进程对象，跨 reactive 轮询状态）──
if (is.null(hf_bus$workbench_procs)) hf_bus$workbench_procs <- list()

# ── 派发任务：异步启动 CodeBuddy CLI，输出落日志文件 ──
hf_workbench_run <- function(task, created_by = NULL) {
  if (is.null(task) || !nzchar(trimws(task))) return(list(success = FALSE, message = "任务内容不能为空"))
  spec <- hf_workbench_cli_spec()
  if (is.null(spec)) {
    return(list(success = FALSE,
      message = "未检测到 codebuddy CLI，请先执行 npm install -g @tencent-ai/codebuddy-code 并 codebuddy login"))
  }
  cli_desc <- paste(c(spec$command, spec$pre), collapse = " ")
  run_id <- sprintf("wb%s", format(Sys.time(), "%Y%m%d%H%M%S"))
  logfile <- file.path(hf_workbench_logdir(), paste0(run_id, ".log"))
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_workbench_runs (run_id, task, status, cli, log_path, created_by)
       VALUES ('%s','%s','running','%s','%s',%s)",
      run_id, hf_esc(task), hf_esc(cli_desc), hf_esc(logfile),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by))))
  }, error = function(e) return(list(success = FALSE, message = e$message)))
  # 异步启动 CodeBuddy CLI（processx 后台进程，stdout/stderr 落日志文件）
  pr <- tryCatch({
    args <- c(spec$pre, "-p", task, "-y", "--output-format", "stream-json")
    args <- args[nzchar(args)]
    if (requireNamespace("processx", quietly = TRUE)) {
      processx::process$new(spec$command, args,
        stdout = logfile, stderr = logfile, cleanup = FALSE)
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
  list(success = TRUE, run_id = run_id, message = paste0("已派发任务 ", run_id))
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
          add(list(kind = "thinking", text = clip(str1(b$thinking))))
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

# ── 会话 Token 汇总（全部 / 今日），供页面 KPI 展示 ──
hf_workbench_summary <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  mk <- function(s) list(
    token_in = s$tin[1], token_out = s$tout[1], token_cache_read = s$tcr[1],
    token_cache_write = s$tcw[1], token_total = s$tt[1], sessions = s$n[1])
  zero <- list(token_in = 0, token_out = 0, token_cache_read = 0,
               token_cache_write = 0, token_total = 0, sessions = 0)
  tryCatch({
    a <- dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COALESCE(SUM(token_cache_read),0) tcr, COALESCE(SUM(token_cache_write),0) tcw, COALESCE(SUM(token_total),0) tt, COUNT(*) n FROM hf_workbench_runs")
    d <- dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COALESCE(SUM(token_cache_read),0) tcr, COALESCE(SUM(token_cache_write),0) tcw, COALESCE(SUM(token_total),0) tt, COUNT(*) n FROM hf_workbench_runs WHERE date(created_at)=date('now','localtime')")
    list(total = mk(a), today = mk(d))
  }, error = function(e) list(total = zero, today = zero))
}

# ── 刷新运行状态：同步 Token 消耗 + 检查 running 进程是否结束 ──
hf_workbench_refresh_status <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    runs <- dbGetQuery(con, "SELECT run_id, status, log_path, token_total FROM hf_workbench_runs")
    if (is.null(runs) || nrow(runs) == 0) return(invisible(NULL))
    for (i in seq_len(nrow(runs))) {
      rid <- runs$run_id[i]
      lp <- runs$log_path[i]
      # 1) Token 列实时同步（缓存解析，仅在数值变化时写库）
      u <- hf_workbench_usage(lp)
      if (u$token_total != hf_or(runs$token_total[i], 0)) {
        dbExecute(con, sprintf(
          "UPDATE hf_workbench_runs SET token_in=%d, token_out=%d, token_cache_read=%d, token_cache_write=%d, token_total=%d WHERE run_id='%s'",
          as.integer(u$token_in), as.integer(u$token_out), as.integer(u$token_cache_read),
          as.integer(u$token_cache_write), as.integer(u$token_total), hf_esc(rid)))
      }
      # 2) 结束检测
      if (!identical(runs$status[i], "running")) next
      p <- hf_bus$workbench_procs[[rid]]
      alive <- if (!is.null(p)) tryCatch(isTRUE(p$is_alive()), error = function(e) TRUE) else FALSE
      if (alive) next
      has_log <- !is.na(lp) && nzchar(lp) && file.exists(lp) && file.info(lp)$size > 0
      st <- if (has_log) "done" else "error"
      dbExecute(con, sprintf(
        "UPDATE hf_workbench_runs SET status='%s', finished_at=datetime('now','localtime') WHERE run_id='%s'",
        st, hf_esc(rid)))
      hf_bus$workbench_procs[[rid]] <- NULL
    }
    invisible(NULL)
  }, error = function(e) NULL)
}

# ── 对外接口透传 ──
hf_workbench_api_run_task <- function(task, created_by = NULL) hf_workbench_run(task, created_by)
hf_workbench_api_list_runs <- function(limit = 50) hf_workbench_list(limit)
hf_workbench_api_read_log <- function(run_id) hf_workbench_log(run_id)
hf_workbench_api_token_summary <- function() hf_workbench_summary()

hf_register(HF_MOD_WORKBENCH, hf_make_api(
  "workbench",
  run_task = hf_workbench_api_run_task,
  list_runs = hf_workbench_api_list_runs,
  read_log = hf_workbench_api_read_log,
  token_summary = hf_workbench_api_token_summary
))
