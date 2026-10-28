##################
# HungFo 子项目 — 核心层
# 独立子项目（R + Shiny + SQLite），最终会独立出去，因此自包含、不依赖现有业务模块。
# 12 个环节：规则 / 爬取 / 数据 / 算法 / 展示 / 自动任务 / 回测 / 元任务 / 开发日志 / 自动架构 / 对外集成 / 权限体系
# 设计：每个环节 = 一个独立模块文件（hf_mod_*.r），彼此功能独立，通过「注册表 / 总线」互相提供接口。
##################

# ── HungFo 根目录解析 ──
# 独立运行（工作目录即 hungfo 目录）与父项目内嵌（工作目录为项目根）两种模式通用。
# 优先级：环境变量 HUNGFO_HOME > 当前目录含 hf_core.r（独立） > 当前目录的 Script/hungfo（父项目） > 当前目录兜底。
hf_home <- function() {
  env <- Sys.getenv("HUNGFO_HOME")
  if (nzchar(env)) return(env)
  if (file.exists(file.path(getwd(), "hf_core.r"))) return(getwd())
  if (file.exists(file.path(getwd(), "Script", "hungfo", "hf_core.r"))) return(file.path(getwd(), "Script", "hungfo"))
  getwd()
}

# ── 内存总线：环节注册表 & 接口表 ──
if (!exists("hf_bus", inherits = FALSE)) {
  hf_bus <- new.env(parent = emptyenv())
  hf_bus$modules <- list()    # id -> 模块定义
  hf_bus$apis    <- list()    # key -> 对外接口函数列表
  hf_bus$order   <- character(0)
}

# 空值兜底
hf_or <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# SQL 字符串转义
hf_esc <- function(x) gsub("'", "''", as.character(hf_or(x, "")))

# 注册一个环节（各 hf_mod_*.r 在 source 时调用）
hf_register <- function(def, api = NULL) {
  id <- def$id
  if (is.null(id) || !nzchar(id)) stop("hf_register: 模块 id 不能为空")
  if (is.null(hf_bus$modules[[id]])) hf_bus$order <- c(hf_bus$order, id)
  hf_bus$modules[[id]] <- def
  if (!is.null(api)) hf_bus$apis[[def$key]] <- api
  invisible(def)
}

# 列出全部环节（按 id 顺序，保证与 source 顺序无关）
hf_modules <- function() {
  m <- hf_bus$modules
  if (length(m) == 0) return(m)
  m[order(vapply(m, function(x) x$id, character(1)))]
}

# 获取某环节定义
hf_module <- function(key) {
  for (m in hf_bus$modules) if (identical(m$key, key)) return(m)
  NULL
}

# 跨环节调用接口（对外集成入口）
hf_call <- function(key, fn, ...) {
  api <- hf_bus$apis[[key]]
  if (is.null(api) || is.null(api[[fn]])) stop(sprintf("接口不存在: %s$%s", key, fn))
  api[[fn]](...)
}

# ── 数据库 ──
# 自包含建表，不修改 global.R 的 migrate_database()
hf_db_init <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_modules (
    id TEXT PRIMARY KEY,
    module_key TEXT,
    name TEXT,
    icon TEXT,
    description TEXT,
    api_desc TEXT,
    status TEXT DEFAULT 'scaffold',
    version TEXT DEFAULT 'v0.1',
    sort_order INTEGER DEFAULT 0,
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_records (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    module_key TEXT,
    rec_no TEXT,
    title TEXT,
    payload TEXT,
    status TEXT DEFAULT 'draft',
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  # Agent 任务表（自动任务模块作为任务来源，示例任务 / 正式任务共用一张表）
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_agent_tasks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    task_type TEXT DEFAULT 'sample',          -- sample=示例任务 / formal=正式任务
    nodes TEXT,                                -- 任务路径节点（JSON 数组字符串）
    enabled INTEGER DEFAULT 1,
    sort_order INTEGER DEFAULT 0,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  # 元任务队列表（WorkBuddy 定时任务读取并执行的待办队列，支持 cron 循环）
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_metatask_queue (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    payload TEXT,
    cron TEXT DEFAULT '',                      -- 空=只跑一次；非空=cron 循环
    status TEXT DEFAULT 'pending',             -- pending=待执行 / running=执行中 / done=已完成(一次性)
    last_run_at TEXT,
    next_run_at TEXT,
    result TEXT,
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  # 会话追踪字段：记录每次执行的隔离会话、调用 LLM 与资源消耗（兼容旧表，逐列 ALTER）
  for (col in list(
    c("session_id", "TEXT"),
    c("llm_name",   "TEXT"),
    c("token_in",   "INTEGER DEFAULT 0"),
    c("token_out",  "INTEGER DEFAULT 0"),
    c("duration_ms","INTEGER DEFAULT 0"),
    c("started_at", "TEXT"),
    c("finished_at","TEXT"))) {
    has <- dbGetQuery(con, sprintf("PRAGMA table_info(hf_metatask_queue)"))
    if (!(col[1] %in% has$name)) {
      dbExecute(con, sprintf("ALTER TABLE hf_metatask_queue ADD COLUMN %s %s", col[1], col[2]))
    }
  }
  # 执行会话明细表：每次元任务执行 = 一行（供监控页做任务追踪可视化）
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_task_sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    task_id INTEGER,                            -- 关联 hf_metatask_queue.id
    session_id TEXT,                            -- 隔离会话 ID
    title TEXT,
    llm_name TEXT,                              -- 调用的 LLM 名称
    token_in INTEGER DEFAULT 0,
    token_out INTEGER DEFAULT 0,
    duration_ms INTEGER DEFAULT 0,              -- 总耗时(ms)
    status TEXT DEFAULT 'running',              -- running/success/failed
    result TEXT,
    started_at TEXT,
    finished_at TEXT,
    created_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  invisible(TRUE)
}

# 将内存注册表同步到数据库
hf_sync_registry <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  mods <- hf_modules()
  for (i in seq_along(mods)) {
    m <- mods[[i]]
    ex <- dbGetQuery(con, sprintf("SELECT id FROM hf_modules WHERE id='%s'", hf_esc(m$id)))
    if (nrow(ex) == 0) {
      dbExecute(con, sprintf(
        "INSERT INTO hf_modules (id,module_key,name,icon,description,api_desc,status,version,sort_order) VALUES ('%s','%s','%s','%s','%s','%s','%s','%s',%d)",
        hf_esc(m$id), hf_esc(m$key), hf_esc(m$name), hf_esc(m$icon), hf_esc(m$desc),
        hf_esc(m$api_desc), hf_esc(hf_or(m$status, "scaffold")), hf_esc(hf_or(m$version, "v0.1")), i))
    } else {
      dbExecute(con, sprintf(
        "UPDATE hf_modules SET module_key='%s',name='%s',icon='%s',description='%s',api_desc='%s',status='%s',version='%s',sort_order=%d WHERE id='%s'",
        hf_esc(m$key), hf_esc(m$name), hf_esc(m$icon), hf_esc(m$desc), hf_esc(m$api_desc),
        hf_esc(hf_or(m$status, "scaffold")), hf_esc(hf_or(m$version, "v0.1")), i, hf_esc(m$id)))
    }
  }
  invisible(TRUE)
}

# ── 通用记录接口（各环节共享的默认数据通道）──
hf_record_add <- function(module_key, title, payload = "", created_by = NULL, rec_no = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    no <- hf_or(rec_no, sprintf("HF-%s-%s", toupper(module_key), format(Sys.time(), "%Y%m%d%H%M%S")))
    dbExecute(con, sprintf(
      "INSERT INTO hf_records (module_key,rec_no,title,payload,created_by) VALUES ('%s','%s','%s','%s',%s)",
      hf_esc(module_key), hf_esc(no), hf_esc(title), hf_esc(payload),
      if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, rec_no = no, message = paste0("已新增：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_records <- function(module_key) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT id,rec_no,title,substr(payload,1,60) AS payload,status,created_at FROM hf_records WHERE module_key='%s' ORDER BY id DESC LIMIT 200",
      hf_esc(module_key)))
  }, error = function(e) data.frame())
}

hf_records_count <- function(module_key) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_records WHERE module_key='%s'", hf_esc(module_key)))$n[1],
    error = function(e) 0)
}

# 完整记录（含全文 payload，供开发日志查看界面使用，不受 60 字截断）
hf_records_full <- function(module_key) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT id,rec_no,title,payload,status,created_at FROM hf_records WHERE module_key='%s' ORDER BY id DESC LIMIT 200",
      hf_esc(module_key)))
  }, error = function(e) data.frame())
}

# 记录统计（总数 + 今日新增），供开发日志头部徽章使用
hf_records_stats <- function(module_key) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    total <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_records WHERE module_key='%s'", hf_esc(module_key)))$n[1]
    today <- dbGetQuery(con, sprintf(
      "SELECT COUNT(*) n FROM hf_records WHERE module_key='%s' AND date(created_at)=date('now','localtime')",
      hf_esc(module_key)))$n[1]
    list(total = if (is.na(total)) 0 else total, today = if (is.na(today)) 0 else today)
  }, error = function(e) list(total = 0, today = 0))
}

# 当前登录用户 id（用于记录 created_by）
hf_current_uid <- function(rv) {
  if (is.null(rv$current_user) || nrow(rv$current_user) == 0) return(NULL)
  rv$current_user$id[1]
}

# ── 接口构造器 ──
# 各环节用它生成标准接口：run(payload) 执行 / fetch() 取数据，并可附加自定义接口
hf_make_api <- function(key, run = NULL, fetch = NULL, ...) {
  api <- list(
    run = hf_or(run, function(payload = list()) {
      hf_record_add(key, hf_or(payload$title, paste0("接口执行 @ ", format(Sys.time(), "%H:%M:%S"))), payload = "auto")
    }),
    fetch = hf_or(fetch, function() hf_records(key))
  )
  extra <- list(...)
  if (length(extra) > 0) for (nm in names(extra)) api[[nm]] <- extra[[nm]]
  api
}

# ══════════════════════════════════════════════════════════════════
# Agent 任务数据层（自动任务模块 = 未来任务来源）
# 任务通过这里下达给 Agent，示例任务 / 正式任务共用一张表，用 task_type 区分。
# ══════════════════════════════════════════════════════════════════

# 默认任务路径（节点 id 需与监控页 AGENTS 一致）
HF_DEFAULT_TASK_NODES <- list(
  research = c("ai_bot","chief_staff","orchestr","pm","web_res","crawler","code_rev","risk_check","qa_verify","validator"),
  codegen  = c("ai_bot","chief_staff","orchestr","pm","codegen","code_rev","risk_check","qa_verify","validator"),
  data     = c("ai_bot","chief_staff","orchestr","pm","data_agent","crawler","code_rev","risk_check","qa_verify","validator")
)

# 读取任务列表（task_type: sample/formal/NULL=全部）
hf_task_list <- function(task_type = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT id,name,task_type,nodes,enabled,sort_order,created_at FROM hf_agent_tasks"
    if (!is.null(task_type)) sql <- sprintf("%s WHERE task_type='%s'", sql, hf_esc(task_type))
    dbGetQuery(con, paste0(sql, " ORDER BY sort_order ASC, id ASC"))
  }, error = function(e) data.frame())
}

# 新增 / 更新任务
hf_task_save <- function(id = NULL, name = NULL, task_type = "sample", nodes = NULL, enabled = 1) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "任务名称不能为空"))
  nodes_json <- if (is.null(nodes)) "[]" else jsonlite::toJSON(as.character(nodes), auto_unbox = FALSE)
  tryCatch({
    if (is.null(id) || is.na(id) || id == "") {
      dbExecute(con, sprintf(
        "INSERT INTO hf_agent_tasks (name,task_type,nodes,enabled) VALUES ('%s','%s','%s',%d)",
        hf_esc(name), hf_esc(task_type), hf_esc(nodes_json), as.integer(enabled)))
      list(success = TRUE, message = paste0("已新增任务：", name))
    } else {
      dbExecute(con, sprintf(
        "UPDATE hf_agent_tasks SET name='%s',task_type='%s',nodes='%s',enabled=%d,updated_at=datetime('now','localtime') WHERE id=%s",
        hf_esc(name), hf_esc(task_type), hf_esc(nodes_json), as.integer(enabled), as.character(id)))
      list(success = TRUE, message = paste0("已更新任务：", name))
    }
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 删除任务
hf_task_delete <- function(id) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_agent_tasks WHERE id=%s", as.character(id)))
    list(success = TRUE, message = "已删除任务")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 切换任务类型（示例 ↔ 正式）
hf_task_toggle_type <- function(id) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_agent_tasks SET task_type = CASE task_type WHEN 'sample' THEN 'formal' ELSE 'sample' END, updated_at=datetime('now','localtime') WHERE id=%s",
      as.character(id)))
    list(success = TRUE, message = "已切换任务类型")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 将任务导出为前端 JS（监控页通过 <script> 引入读取 window.HF_TASKS）
# 写盘到 www/hungfo_tasks.js，供独立的 hungfo_agent_monitor.html 使用（自包含、无 R 接口依赖）
hf_write_tasks_js <- function() {
  tasks <- hf_task_list()
  js_tasks <- lapply(seq_len(nrow(tasks)), function(i) {
    nodes <- tryCatch(jsonlite::fromJSON(tasks$nodes[i]), error = function(e) character(0))
    list(
      id = tasks$id[i], name = tasks$name[i], type = tasks$task_type[i],
      nodes = as.character(nodes), enabled = tasks$enabled[i]
    )
  })
  body <- jsonlite::toJSON(js_tasks, auto_unbox = TRUE)
  out <- paste0("// 由 HungFo 自动任务模块生成（hf_write_tasks_js），勿手改\nwindow.HF_TASKS = ", body, ";\n")
  dir.create(file.path(hf_home(), "www"), showWarnings = FALSE, recursive = TRUE)
  writeLines(out, file.path(hf_home(), "www", "hungfo_tasks.js"), useBytes = TRUE)
  invisible(out)
}

# 初始化示例任务（首次建表后若无数据则播种，供监控页展示与编辑）
hf_task_seed_sample <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    n <- dbGetQuery(con, "SELECT COUNT(*) n FROM hf_agent_tasks")$n[1]
    if (n > 0) return(invisible(FALSE))
    seeds <- list(
      list(name = "竞品行情爬取", type = "sample", nodes = HF_DEFAULT_TASK_NODES$research),
      list(name = "代码生成与审查", type = "sample", nodes = HF_DEFAULT_TASK_NODES$codegen),
      list(name = "数据管道同步", type = "sample", nodes = HF_DEFAULT_TASK_NODES$data)
    )
    for (i in seq_along(seeds)) {
      s <- seeds[[i]]
      dbExecute(con, sprintf(
        "INSERT INTO hf_agent_tasks (name,task_type,nodes,sort_order) VALUES ('%s','%s','%s',%d)",
        hf_esc(s$name), hf_esc(s$type), hf_esc(jsonlite::toJSON(as.character(s$nodes), auto_unbox = FALSE)), i))
    }
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

# ══════════════════════════════════════════════════════════════════
# 元任务队列数据层（WorkBuddy 定时任务读取并执行）
# 状态机：pending(待执行) → running(执行中) → 有 cron 则回到 pending 并推进 next_run_at，
#         无 cron 则 done(一次性完成)。
# 同时把队列镜像导出为本地文本文件 Script/hungfo/MetaTasks.json，
# 供 WorkBuddy 定时任务直接读取（无需访问数据库）。
# ══════════════════════════════════════════════════════════════════

HF_METATASK_JSON <- file.path(hf_home(), "MetaTasks.json")
HF_SESSIONS_JSON <- file.path(hf_home(), "www", "hungfo_sessions.js")

# ── 基础 cron 解析（5 字段：分 时 日 月 周），支持 * 、*/n 、单值、逗号列表 ──
# 返回：从 from 时刻起，下一个满足 cron 的时刻（POSIXct）；解析失败返回 NULL
hf_cron_next <- function(cron, from = Sys.time()) {
  if (is.null(cron) || !nzchar(trimws(cron))) return(NULL)
  cron <- trimws(cron)
  f <- strsplit(cron, "\\s+")[[1]]
  if (length(f) != 5) return(NULL)

  parse_field <- function(s, lo, hi) {
    s <- trimws(s)
    if (s == "*") return(lo:hi)
    vals <- integer(0)
    for (part in strsplit(s, ",")[[1]]) {
      if (grepl("^\\*/", part)) {
        step <- as.integer(sub("^\\*/", "", part))
        vals <- c(vals, seq(lo, hi, by = step))
      } else if (grepl("-", part) && !grepl("^\\*", part)) {
        rng <- as.integer(strsplit(part, "-")[[1]])
        vals <- c(vals, seq(rng[1], rng[2], by = 1))
      } else {
        vals <- c(vals, as.integer(part))
      }
    }
    unique(vals[vals >= lo & vals <= hi])
  }

  mins <- parse_field(f[1], 0, 59)
  hrs  <- parse_field(f[2], 0, 23)
  days <- parse_field(f[3], 1, 31)
  mons <- parse_field(f[4], 1, 12)
  wds  <- parse_field(f[5], 0, 6)   # 0=周日

  if (length(mins) == 0 || length(hrs) == 0 || length(days) == 0 ||
      length(mons) == 0 || length(wds) == 0) return(NULL)

  # 从 from 之后逐分钟扫描（最多扫 366 天，避免死循环）
  t <- as.POSIXct(trunc(from, "mins")) + 60
  limit <- t + 366 * 24 * 3600
  while (t <= limit) {
    lt <- as.POSIXlt(t)
    ok_min <- lt$min %in% mins
    ok_hr  <- lt$hour %in% hrs
    ok_day <- lt$mday %in% days
    ok_mon <- (lt$mon + 1) %in% mons
    ok_wd  <- lt$wday %in% wds
    if (ok_min && ok_hr && ok_day && ok_mon && ok_wd) return(t)
    t <- t + 60
  }
  NULL
}

# ── 读取队列 ──
hf_metatask_list <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, "SELECT * FROM hf_metatask_queue ORDER BY id ASC"),
    error = function(e) data.frame())
}

# ── 新增元任务（含 cron，自动计算 next_run_at）──
hf_metatask_add <- function(title, payload = "", cron = "") {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "任务标题不能为空"))
  nxt <- if (nzchar(trimws(cron))) hf_cron_next(cron) else Sys.time()
  nxt_str <- if (is.null(nxt)) NULL else format(nxt, "%Y-%m-%d %H:%M:%S")
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_metatask_queue (title,payload,cron,status,next_run_at) VALUES ('%s','%s','%s','pending',%s)",
      hf_esc(title), hf_esc(payload), hf_esc(cron),
      if (is.null(nxt_str)) "NULL" else sprintf("'%s'", nxt_str)))
    hf_metatask_export()
    list(success = TRUE, message = paste0("已新增元任务：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 记录一次执行会话（供监控页任务追踪可视化 + Token/耗时统计）──
hf_session_record <- function(task_id, session_id = "", llm_name = "", token_in = 0,
                              token_out = 0, duration_ms = 0, status = "success",
                              result = "", title = "", started_at = NULL, finished_at = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO hf_task_sessions (task_id,session_id,title,llm_name,token_in,token_out,duration_ms,status,result,started_at,finished_at)
       VALUES (%s,'%s','%s','%s',%d,%d,%d,'%s','%s',%s,%s)",
      if (is.null(task_id) || is.na(task_id)) "NULL" else as.character(as.integer(task_id)),
      hf_esc(session_id), hf_esc(hf_or(title, "")), hf_esc(hf_or(llm_name, "")),
      as.integer(hf_or(token_in, 0)), as.integer(hf_or(token_out, 0)), as.integer(hf_or(duration_ms, 0)),
      hf_esc(hf_or(status, "success")), hf_esc(hf_or(result, "")),
      if (is.null(started_at)) sprintf("'%s'", now_str) else sprintf("'%s'", hf_esc(started_at)),
      if (is.null(finished_at)) sprintf("'%s'", now_str) else sprintf("'%s'", hf_esc(finished_at))))
    list(success = TRUE, message = "已记录会话")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 标记任务执行结果 ──
# 有 cron → 计算下一个 next_run_at，回到 pending（循环）；无 cron → done（一次性完成）
# 可附带会话元数据（session_id/llm_name/token_in/token_out/duration_ms），一并写回队列表 + 会话表
hf_metatask_mark_done <- function(id, result = "", session_id = "", llm_name = "",
                                  token_in = 0, token_out = 0, duration_ms = 0) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    row <- dbGetQuery(con, sprintf("SELECT * FROM hf_metatask_queue WHERE id=%s", as.character(id)))
    if (nrow(row) == 0) return(list(success = FALSE, message = "任务不存在"))
    now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cron <- hf_or(row$cron[1], "")
    # 会话追踪字段写回队列表（记录最近一次执行的元数据）
    meta_sql <- sprintf(
      "session_id='%s', llm_name='%s', token_in=%d, token_out=%d, duration_ms=%d, started_at='%s', finished_at='%s'",
      hf_esc(hf_or(session_id, "")), hf_esc(hf_or(llm_name, "")),
      as.integer(hf_or(token_in, 0)), as.integer(hf_or(token_out, 0)), as.integer(hf_or(duration_ms, 0)),
      hf_or(row$started_at[1], now_str), now_str)
    if (nzchar(trimws(cron))) {
      nxt <- hf_cron_next(cron)
      nxt_str <- if (is.null(nxt)) NULL else format(nxt, "%Y-%m-%d %H:%M:%S")
      dbExecute(con, sprintf(
        "UPDATE hf_metatask_queue SET status='pending', last_run_at='%s', next_run_at=%s, result='%s', %s, updated_at=datetime('now','localtime') WHERE id=%s",
        now_str, if (is.null(nxt_str)) "NULL" else sprintf("'%s'", nxt_str), hf_esc(result), meta_sql, as.character(id)))
    } else {
      dbExecute(con, sprintf(
        "UPDATE hf_metatask_queue SET status='done', last_run_at='%s', next_run_at=NULL, result='%s', %s, updated_at=datetime('now','localtime') WHERE id=%s",
        now_str, hf_esc(result), meta_sql, as.character(id)))
    }
    # 写执行会话明细
    hf_session_record(id, session_id, llm_name, token_in, token_out, duration_ms,
                      status = "success", result = result, title = hf_or(row$title[1], ""),
                      started_at = hf_or(row$started_at[1], now_str), finished_at = now_str)
    hf_metatask_export()
    hf_sessions_export()
    list(success = TRUE, message = "已更新任务状态")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 删除元任务 ──
hf_metatask_delete <- function(id) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_metatask_queue WHERE id=%s", as.character(id)))
    hf_metatask_export()
    list(success = TRUE, message = "已删除元任务")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 导出队列到 MetaTasks.json（WorkBuddy 定时任务读取的本地镜像）──
hf_metatask_export <- function() {
  q <- hf_metatask_list()
  tasks <- if (is.null(q) || nrow(q) == 0) list() else lapply(seq_len(nrow(q)), function(i) {
    list(
      id = q$id[i], title = q$title[i], payload = hf_or(q$payload[i], ""),
      cron = hf_or(q$cron[i], ""), status = hf_or(q$status[i], "pending"),
      last_run_at = if (is.na(q$last_run_at[i]) || is.null(q$last_run_at[i])) NULL else q$last_run_at[i],
      next_run_at = if (is.na(q$next_run_at[i]) || is.null(q$next_run_at[i])) NULL else q$next_run_at[i],
      result = hf_or(q$result[i], "")
    )
  })
  out <- list(
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    tasks = tasks
  )
  dir.create(hf_home(), showWarnings = FALSE, recursive = TRUE)
  writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             HF_METATASK_JSON, useBytes = TRUE)
  invisible(out)
}

# 元任务入库时自动导出镜像（供 UI 增删改后调用）
hf_metatask_refresh <- function() {
  hf_metatask_export()
  invisible(TRUE)
}

# ── 会话明细查询（供监控页展示执行记录）──
hf_sessions_list <- function(limit = 100) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf(
      "SELECT * FROM hf_task_sessions ORDER BY id DESC LIMIT %d", as.integer(limit))),
    error = function(e) data.frame())
}

# ── Token 汇总（当天 / 全部）──
hf_token_summary <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    all_s <- dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COUNT(*) n FROM hf_task_sessions")
    day_s <- dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COUNT(*) n FROM hf_task_sessions WHERE date(started_at)=date('now','localtime')")
    list(
      total = list(token_in = all_s$tin[1], token_out = all_s$tout[1], sessions = all_s$n[1]),
      today = list(token_in = day_s$tin[1], token_out = day_s$tout[1], sessions = day_s$n[1])
    )
  }, error = function(e) list(total = list(token_in = 0, token_out = 0, sessions = 0), today = list(token_in = 0, token_out = 0, sessions = 0)))
}

# ── 导出会话明细到 hungfo_sessions.js（监控页前端读取）──
# 输出格式：window.HF_SESSIONS = { summary: {...}, sessions: [...] }
hf_sessions_export <- function(limit = 200) {
  s <- hf_sessions_list(limit)
  sm <- hf_token_summary()
  sessions <- if (is.null(s) || nrow(s) == 0) list() else lapply(seq_len(nrow(s)), function(i) list(
    id = s$id[i], task_id = if (is.na(s$task_id[i]) || is.null(s$task_id[i])) NULL else s$task_id[i],
    session_id = hf_or(s$session_id[i], ""), title = hf_or(s$title[i], ""),
    llm_name = hf_or(s$llm_name[i], ""),
    token_in = if (is.na(s$token_in[i])) 0 else s$token_in[i],
    token_out = if (is.na(s$token_out[i])) 0 else s$token_out[i],
    duration_ms = if (is.na(s$duration_ms[i])) 0 else s$duration_ms[i],
    status = hf_or(s$status[i], "success"),
    started_at = if (is.na(s$started_at[i]) || is.null(s$started_at[i])) NULL else s$started_at[i],
    finished_at = if (is.na(s$finished_at[i]) || is.null(s$finished_at[i])) NULL else s$finished_at[i]
  ))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), summary = sm, sessions = sessions)
  dir.create(file.path(hf_home(), "www"), showWarnings = FALSE, recursive = TRUE)
  body <- jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(paste0("// 由 HungFo 生成（hf_sessions_export），勿手改\nwindow.HF_SESSIONS = ", body, ";\n"),
             HF_SESSIONS_JSON, useBytes = TRUE)
  invisible(out)
}
