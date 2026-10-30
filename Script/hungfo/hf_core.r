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

# ── 数据库（独立 HF_HungFo.DB，与父项目 GH_ITOM.db 逐步解耦）──
# 自包含建表，不修改 global.R 的 migrate_database()
# DB 路径统一由 hf_db_file() 解析：环境变量 HUNGFO_DB > 默认 hungfo/DB/HF_HungFo.DB。
# HungFo 一律走 hf_db_connect()/hf_db_disconnect() 连接独立库，不触碰父项目 db_connect / db_path。
hf_db_file <- function() {
  env <- Sys.getenv("HUNGFO_DB")
  if (nzchar(env)) return(env)
  file.path(hf_home(), "DB", "HF_HungFo.DB")
}

hf_db_connect <- function() {
  dir.create(dirname(hf_db_file()), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), hf_db_file())
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  DBI::dbExecute(con, "PRAGMA localtime = 1")
  con
}

hf_db_disconnect <- function(con) DBI::dbDisconnect(con)

hf_db_init <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
  # 规则表（规则模块专属：含「分类」字段，支持查看/编辑/删除）
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_rules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    rule_no TEXT,
    title TEXT NOT NULL,
    category TEXT DEFAULT '',                -- 规则分类（爬取频率/算法参数/风控阈值…）
    payload TEXT,                             -- 规则内容 / 判定逻辑
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
  # 元任务队列表（CodeBuddy 定时任务读取并执行的待办队列，支持 cron 循环）
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
    c("agent",      "TEXT"),
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
    agent TEXT,                                 -- 执行 Agent 来源（WorkBuddy / CodeBuddy）
    token_in INTEGER DEFAULT 0,
    token_out INTEGER DEFAULT 0,
    duration_ms INTEGER DEFAULT 0,              -- 总耗时(ms)
    status TEXT DEFAULT 'running',              -- running/success/failed
    result TEXT,
    started_at TEXT,
    finished_at TEXT,
    created_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  # 会话表兼容：旧库补 agent 字段（记录执行 Agent 来源 WorkBuddy / CodeBuddy）
  has_sess <- dbGetQuery(con, "PRAGMA table_info(hf_task_sessions)")
  if (!("agent" %in% has_sess$name)) {
    dbExecute(con, "ALTER TABLE hf_task_sessions ADD COLUMN agent TEXT")
  }
  # CodeBuddy 自动任务运行统计采集表：从本机 automations.db 采到的每个自动任务运行次数/成功率/耗时
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_cb_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    automation_id TEXT,                         -- CodeBuddy 自动化任务 ID
    name TEXT,                                  -- 任务名称
    schedule_type TEXT,                         -- recurring / once
    status TEXT,                                -- ACTIVE / PAUSED
    runs_total INTEGER DEFAULT 0,               -- 累计运行次数
    runs_success INTEGER DEFAULT 0,             -- 成功次数
    runs_failed INTEGER DEFAULT 0,              -- 失败次数
    success_rate REAL DEFAULT 0,                -- 成功率(0-100)
    avg_duration_ms INTEGER DEFAULT 0,          -- 平均耗时(ms)
    last_run_at TEXT,                           -- 最近运行时间(本地时间字符串)
    created_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  # CodeBuddy 会话清单采集表：从本机 codebuddy-sessions.vscdb 采到的会话元信息 + 手动补录的 Token 明细
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_cb_sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    conversation_id TEXT UNIQUE,                -- CodeBuddy 会话 ID
    title TEXT,                                 -- 会话标题
    cwd TEXT,                                   -- 工作目录
    status TEXT,                                -- Working / Completed
    created_at TEXT,                            -- 会话创建时间
    updated_at TEXT,                            -- 会话更新时间
    token_total REAL DEFAULT 0,                 -- Token 总计
    token_input REAL DEFAULT 0,                 -- 输入
    token_output REAL DEFAULT 0,                -- 输出
    token_cache_hit REAL DEFAULT 0,             -- 缓存命中
    token_cache_miss REAL DEFAULT 0,            -- 缓存未命中
    token_cache_write REAL DEFAULT 0,           -- 缓存写入
    token_thinking REAL DEFAULT 0,              -- 思考过程
    token_response REAL DEFAULT 0,              -- 回复内容
    token_cache_hit_rate REAL DEFAULT 0,        -- 缓存命中率(%)
    token_updated_at TEXT,                      -- Token 手动录入时间
    synced_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  invisible(TRUE)
}

# 将内存注册表同步到数据库
hf_sync_registry <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT id,rec_no,title,substr(payload,1,60) AS payload,status,created_at FROM hf_records WHERE module_key='%s' ORDER BY id DESC LIMIT 200",
      hf_esc(module_key)))
  }, error = function(e) data.frame())
}

hf_records_count <- function(module_key) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_records WHERE module_key='%s'", hf_esc(module_key)))$n[1],
    error = function(e) 0)
}

# 完整记录（含全文 payload，供开发日志查看界面使用，不受 60 字截断）
hf_records_full <- function(module_key) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT id,rec_no,title,payload,status,created_at FROM hf_records WHERE module_key='%s' ORDER BY id DESC LIMIT 200",
      hf_esc(module_key)))
  }, error = function(e) data.frame())
}

# 记录统计（总数 + 今日新增），供开发日志头部徽章使用
hf_records_stats <- function(module_key) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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

# ══════════════════════════════════════════════════════════════════
# 规则数据层（规则模块专属：含「分类」字段，支持查看/编辑/删除）
# ══════════════════════════════════════════════════════════════════

# 规则默认分类（业务规则，与规则模块职责「爬取频率/算法参数/风控阈值」对齐）
HF_RULES_CATEGORIES <- c("爬取频率", "算法参数", "风控阈值", "数据质量", "调度策略", "其它")

hf_rules_categories <- function() HF_RULES_CATEGORIES

# 规则列表（可按分类筛选）
hf_rules_list <- function(category = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT id,rule_no,title,category,payload,status,created_at,updated_at FROM hf_rules"
    if (!is.null(category) && nzchar(trimws(category))) {
      sql <- sprintf("%s WHERE category='%s'", sql, hf_esc(category))
    }
    dbGetQuery(con, paste0(sql, " ORDER BY id DESC LIMIT 500"))
  }, error = function(e) data.frame())
}

# 单条规则
hf_rules_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM hf_rules WHERE id=%s", as.character(as.integer(id))))
  }, error = function(e) data.frame())
}

# 规则编号生成（RUL+日期+3位序号）
hf_rules_generate_no <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    prefix <- paste0("RUL", format(Sys.Date(), "%Y%m%d"))
    ex <- dbGetQuery(con, sprintf("SELECT rule_no FROM hf_rules WHERE rule_no LIKE '%s%%' ORDER BY rule_no DESC LIMIT 1", prefix))
    if (nrow(ex) == 0) return(paste0(prefix, "001"))
    sprintf("%s%03d", prefix, as.integer(substr(ex$rule_no[1], 12, 14)) + 1L)
  }, error = function(e) paste0("RUL", format(Sys.time(), "%Y%m%d%H%M%S")))
}

# 新增规则
hf_rules_add <- function(title, category = "", payload = "", created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "规则名称不能为空"))
  no <- hf_rules_generate_no()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_rules (rule_no,title,category,payload,created_by) VALUES ('%s','%s','%s','%s',%s)",
      hf_esc(no), hf_esc(title), hf_esc(hf_or(category, "")), hf_esc(hf_or(payload, "")),
      if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, rule_no = no, message = paste0("已新增规则：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 更新规则
hf_rules_update <- function(id, title, category = "", payload = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "规则 id 无效"))
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "规则名称不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_rules SET title='%s',category='%s',payload='%s',updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(title), hf_esc(hf_or(category, "")), hf_esc(hf_or(payload, "")), as.character(as.integer(id))))
    list(success = TRUE, message = paste0("已更新规则：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 删除规则
hf_rules_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_rules WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已删除规则")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 规则计数
hf_rules_count <- function(category = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT COUNT(*) n FROM hf_rules"
    if (!is.null(category) && nzchar(trimws(category))) sql <- sprintf("%s WHERE category='%s'", sql, hf_esc(category))
    dbGetQuery(con, sql)$n[1]
  }, error = function(e) 0)
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT id,name,task_type,nodes,enabled,sort_order,created_at FROM hf_agent_tasks"
    if (!is.null(task_type)) sql <- sprintf("%s WHERE task_type='%s'", sql, hf_esc(task_type))
    dbGetQuery(con, paste0(sql, " ORDER BY sort_order ASC, id ASC"))
  }, error = function(e) data.frame())
}

# 新增 / 更新任务
hf_task_save <- function(id = NULL, name = NULL, task_type = "sample", nodes = NULL, enabled = 1) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_agent_tasks WHERE id=%s", as.character(id)))
    list(success = TRUE, message = "已删除任务")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 切换任务类型（示例 ↔ 正式）
hf_task_toggle_type <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
# 元任务队列数据层（CodeBuddy 定时任务读取并执行）
# 状态机：pending(待执行) → running(执行中) → 有 cron 则回到 pending 并推进 next_run_at，
#         无 cron 则 done(一次性完成)。
# 同时把队列镜像导出为本地文本文件 Script/hungfo/MetaTasks.json，
# 供 CodeBuddy 定时任务直接读取（无需访问数据库）。
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, "SELECT * FROM hf_metatask_queue ORDER BY id ASC"),
    error = function(e) data.frame())
}

# ── 新增元任务（含 cron，自动计算 next_run_at）──
hf_metatask_add <- function(title, payload = "", cron = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
                              result = "", title = "", started_at = NULL, finished_at = NULL,
                              agent = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO hf_task_sessions (task_id,session_id,title,llm_name,agent,token_in,token_out,duration_ms,status,result,started_at,finished_at)
       VALUES (%s,'%s','%s','%s','%s',%d,%d,%d,'%s','%s',%s,%s)",
      if (is.null(task_id) || is.na(task_id)) "NULL" else as.character(as.integer(task_id)),
      hf_esc(session_id), hf_esc(hf_or(title, "")), hf_esc(hf_or(llm_name, "")), hf_esc(hf_or(agent, "")),
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
                                  token_in = 0, token_out = 0, duration_ms = 0, agent = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    row <- dbGetQuery(con, sprintf("SELECT * FROM hf_metatask_queue WHERE id=%s", as.character(id)))
    if (nrow(row) == 0) return(list(success = FALSE, message = "任务不存在"))
    now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cron <- hf_or(row$cron[1], "")
    # 会话追踪字段写回队列表（记录最近一次执行的元数据）
    meta_sql <- sprintf(
      "session_id='%s', llm_name='%s', agent='%s', token_in=%d, token_out=%d, duration_ms=%d, started_at='%s', finished_at='%s'",
      hf_esc(hf_or(session_id, "")), hf_esc(hf_or(llm_name, "")), hf_esc(hf_or(agent, "")),
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
                      started_at = hf_or(row$started_at[1], now_str), finished_at = now_str,
                      agent = agent)
    hf_metatask_export()
    hf_sessions_export()
    list(success = TRUE, message = "已更新任务状态")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 删除元任务 ──
hf_metatask_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_metatask_queue WHERE id=%s", as.character(id)))
    hf_metatask_export()
    list(success = TRUE, message = "已删除元任务")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 编辑元任务（仅「待开发 pending / 开发异常 error·failed」状态可改）──
# 编辑后重置为待开发并置 next_run_at=当前，等待 CodeBuddy 下一轮重新执行。
hf_metatask_update <- function(id, title, payload = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "任务 id 无效"))
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "任务标题不能为空"))
  tryCatch({
    row <- dbGetQuery(con, sprintf("SELECT * FROM hf_metatask_queue WHERE id=%s", as.character(as.integer(id))))
    if (nrow(row) == 0) return(list(success = FALSE, message = "任务不存在"))
    st <- hf_or(row$status[1], "pending")
    if (!(st %in% c("pending", "error", "failed"))) {
      return(list(success = FALSE, message = "仅「待开发 / 开发异常」状态的需求可编辑"))
    }
    dbExecute(con, sprintf(
      "UPDATE hf_metatask_queue SET title='%s', payload='%s', status='pending', next_run_at=datetime('now','localtime'), result=NULL, updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(title), hf_esc(payload), as.character(as.integer(id))))
    hf_metatask_export()
    list(success = TRUE, message = paste0("已保存修改：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 导出队列到 MetaTasks.json（CodeBuddy 定时任务读取的本地镜像）──
hf_metatask_export <- function() {
  q <- hf_metatask_list()
  tasks <- if (is.null(q) || nrow(q) == 0) list() else lapply(seq_len(nrow(q)), function(i) {
    list(
      id = q$id[i], title = q$title[i], payload = hf_or(q$payload[i], ""),
      cron = hf_or(q$cron[i], ""), status = hf_or(q$status[i], "pending"),
      last_run_at = if (is.na(q$last_run_at[i]) || is.null(q$last_run_at[i])) NULL else q$last_run_at[i],
      next_run_at = if (is.na(q$next_run_at[i]) || is.null(q$next_run_at[i])) NULL else q$next_run_at[i],
      result = hf_or(q$result[i], ""),
      llm_name = if (is.null(q$llm_name) || length(q$llm_name) == 0 || is.na(q$llm_name[i])) "" else hf_or(q$llm_name[i], ""),
      agent = if (is.null(q$agent) || length(q$agent) == 0 || is.na(q$agent[i])) "" else hf_or(q$agent[i], "")
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
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf(
      "SELECT * FROM hf_task_sessions ORDER BY id DESC LIMIT %d", as.integer(limit))),
    error = function(e) data.frame())
}

# ── 每个元任务最近一次执行会话的 LLM / Agent 元数据（供需求队列展示「完成 LLM / Agent」）──
# 返回 data.frame: task_id, llm_name, agent（每个 task 取 id 最大的一条会话）
hf_metatask_session_meta <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con,
      "SELECT s.task_id, s.llm_name, s.agent
       FROM hf_task_sessions s
       JOIN (SELECT task_id, MAX(id) AS mid FROM hf_task_sessions WHERE task_id IS NOT NULL GROUP BY task_id) m
         ON s.id = m.mid")
  }, error = function(e) data.frame())
}

# ── Token 汇总（当天 / 全部）──
hf_token_summary <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
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
    llm_name = hf_or(s$llm_name[i], ""), agent = hf_or(s$agent[i], ""),
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

# ══════════════════════════════════════════════════════════════════════
# CodeBuddy 自动任务运行统计采集（从本机 automations.db 读取运行次数/成功率/耗时）
# 数据源：%APPDATA%/CodeBuddy CN/automations/automations.db 的 automations + automation_runs 表
#   - automations: 自动任务清单（id/name/status/schedule_type）
#   - automation_runs: 每次运行记录（automation_id/result_success/created_at/updated_at）
# 口径：runs=运行次数，success_rate=成功率(%)，avg_duration_ms=平均耗时(ms)
# ══════════════════════════════════════════════════════════════════════

# 定位本机 CodeBuddy automations.db
hf_cb_db_path <- function() {
  env <- Sys.getenv("CODEBUDDY_AUTOMATION_DB")
  if (nzchar(env) && file.exists(env)) return(env)
  appdata <- Sys.getenv("APPDATA", unset = Sys.getenv("USERPROFILE", unset = ""))
  cand <- c(
    file.path(appdata, "CodeBuddy CN", "automations", "automations.db"),
    file.path(appdata, "CodeBuddy", "automations", "automations.db")
  )
  for (p in cand) if (nzchar(p) && file.exists(p)) return(p)
  NULL
}

# 秒/毫秒时间戳 → 本地时间字符串
hf_cb_ts_str <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0 || is.na(x) || x <= 0) return("")
  secs <- if (x > 1e12) x / 1000 else x
  format(as.POSIXct(secs, origin = "1970-01-01", tz = "Asia/Shanghai"), "%Y-%m-%d %H:%M:%S")
}

# 从 CodeBuddy automations.db 读取每个自动任务的运行统计
# 返回 data.frame: automation_id, name, schedule_type, status, runs_total, runs_success, runs_failed, success_rate, avg_duration_ms, last_run_at
hf_cb_read_usage <- function() {
  path <- hf_cb_db_path()
  if (is.null(path)) return(data.frame())
  con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), path), error = function(e) NULL)
  if (is.null(con)) return(data.frame())
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    autos <- DBI::dbGetQuery(con, "SELECT id, name, status, schedule_type FROM automations ORDER BY name")
    runs <- DBI::dbGetQuery(con, "SELECT automation_id, result_success, created_at, updated_at FROM automation_runs")
    if (is.null(autos) || nrow(autos) == 0) return(data.frame())
    if (is.null(runs) || nrow(runs) == 0) runs <- data.frame(automation_id = character(0), result_success = integer(0), created_at = numeric(0), updated_at = numeric(0), stringsAsFactors = FALSE)
    # 聚合每个自动任务的运行统计（次数/成功率/耗时）
    rows <- lapply(seq_len(nrow(autos)), function(i) {
      aid <- as.character(autos$id[i])
      r <- runs[!is.na(runs$automation_id) & as.character(runs$automation_id) == aid, , drop = FALSE]
      total <- nrow(r)
      success <- if (total == 0) 0 else sum(as.integer(r$result_success) == 1, na.rm = TRUE)
      failed <- total - success
      rate <- if (total == 0) 0 else round(success / total * 100, 1)
      last_ts <- if (total == 0) NA else suppressWarnings(max(as.numeric(r$created_at), na.rm = TRUE))
      use_ms <- !is.na(last_ts) && last_ts > 1e12
      diff_units <- pmax(0, as.numeric(r$updated_at) - as.numeric(r$created_at), na.rm = TRUE)
      dur_ms <- if (total == 0) 0 else round(mean(diff_units, na.rm = TRUE) * ifelse(use_ms, 1, 1000))
      list(automation_id = aid, name = as.character(autos$name[i]),
           schedule_type = ifelse(is.na(autos$schedule_type[i]), "", as.character(autos$schedule_type[i])),
           status = ifelse(is.na(autos$status[i]), "", as.character(autos$status[i])),
           runs_total = total, runs_success = success, runs_failed = failed,
           success_rate = rate, avg_duration_ms = dur_ms, last_run_at = hf_cb_ts_str(last_ts))
    })
    df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
    df[order(df$last_run_at, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  }, error = function(e) data.frame())
}

# 采集 CodeBuddy 自动任务运行统计写入 hf_cb_usage（全量覆盖式刷新），并导出 hungfo_usage.js
hf_cb_usage_sync <- function() {
  df <- hf_cb_read_usage()
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(df) || nrow(df) == 0) {
    hf_cb_usage_export()
    return(list(success = TRUE, automations = 0, message = "未读取到 CodeBuddy 自动任务（automations.db 不存在或为空）"))
  }
  # 覆盖式刷新：先清空再插入
  dbExecute(con, "DELETE FROM hf_cb_usage")
  for (i in seq_len(nrow(df))) {
    dbExecute(con, sprintf(
      "INSERT INTO hf_cb_usage (automation_id,name,schedule_type,status,runs_total,runs_success,runs_failed,success_rate,avg_duration_ms,last_run_at)
       VALUES ('%s','%s','%s','%s',%d,%d,%d,%f,%d,'%s')",
      hf_esc(df$automation_id[i]), hf_esc(df$name[i]), hf_esc(df$schedule_type[i]), hf_esc(df$status[i]),
      as.integer(df$runs_total[i]), as.integer(df$runs_success[i]), as.integer(df$runs_failed[i]),
      as.numeric(df$success_rate[i]), as.integer(df$avg_duration_ms[i]), hf_esc(as.character(df$last_run_at[i]))))
  }
  hf_cb_usage_export()
  list(success = TRUE, automations = nrow(df), message = paste0("已采集 ", nrow(df), " 个 CodeBuddy 自动任务运行统计"))
}

# 查询采集的 CodeBuddy 自动任务运行统计（供界面/导出使用）
hf_cb_usage_list <- function(limit = 200) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbGetQuery(con, sprintf(
    "SELECT * FROM hf_cb_usage ORDER BY last_run_at DESC LIMIT %d", as.integer(limit))),
    error = function(e) data.frame())
}

# CodeBuddy 自动任务运行汇总（今日 / 全部：运行次数·成功率·成功/失败·任务数）
hf_cb_usage_summary <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  mk <- function(s) list(runs = s$runs[1], success = s$succ[1], failed = s$fail[1],
                         rate = if (s$runs[1] > 0) round(s$succ[1] / s$runs[1] * 100, 1) else 0,
                         automations = s$n[1])
  tryCatch({
    all_s <- dbGetQuery(con, "SELECT COALESCE(SUM(runs_total),0) runs, COALESCE(SUM(runs_success),0) succ, COALESCE(SUM(runs_failed),0) fail, COUNT(*) n FROM hf_cb_usage")
    day_s <- dbGetQuery(con, "SELECT COALESCE(SUM(runs_total),0) runs, COALESCE(SUM(runs_success),0) succ, COALESCE(SUM(runs_failed),0) fail, COUNT(*) n FROM hf_cb_usage WHERE date(last_run_at) = date('now','localtime')")
    list(total = mk(all_s), today = mk(day_s))
  }, error = function(e) list(
    total = list(runs = 0, success = 0, failed = 0, rate = 0, automations = 0),
    today = list(runs = 0, success = 0, failed = 0, rate = 0, automations = 0)))
}

# 导出 CodeBuddy 自动任务运行统计到 www/hungfo_usage.js（监控页前端读取）
hf_cb_usage_export <- function(limit = 200) {
  u <- hf_cb_usage_list(limit)
  sm <- hf_cb_usage_summary()
  usages <- if (is.null(u) || nrow(u) == 0) list() else lapply(seq_len(nrow(u)), function(i) list(
    automation_id = hf_or(u$automation_id[i], ""), name = hf_or(u$name[i], ""),
    schedule_type = hf_or(u$schedule_type[i], ""), status = hf_or(u$status[i], ""),
    runs_total = if (is.na(u$runs_total[i])) 0 else u$runs_total[i],
    runs_success = if (is.na(u$runs_success[i])) 0 else u$runs_success[i],
    runs_failed = if (is.na(u$runs_failed[i])) 0 else u$runs_failed[i],
    success_rate = if (is.na(u$success_rate[i])) 0 else u$success_rate[i],
    avg_duration_ms = if (is.na(u$avg_duration_ms[i])) 0 else u$avg_duration_ms[i],
    last_run_at = hf_or(u$last_run_at[i], "")
  ))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), summary = sm, usages = usages)
  dir.create(file.path(hf_home(), "www"), showWarnings = FALSE, recursive = TRUE)
  body <- jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(paste0("// 由 HungFo 生成（hf_cb_usage_export），勿手改\nwindow.HF_CB_USAGE = ", body, ";\n"),
             file.path(hf_home(), "www", "hungfo_usage.js"), useBytes = TRUE)
  invisible(out)
}

# 查询采集的 CodeBuddy 会话清单（供界面/导出使用）
hf_cb_sessions_list <- function(limit = 200) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbGetQuery(con, sprintf(
    "SELECT * FROM hf_cb_sessions ORDER BY COALESCE(token_updated_at, updated_at, created_at) DESC LIMIT %d", as.integer(limit))),
    error = function(e) data.frame())
}

# CodeBuddy 会话汇总（会话数 + Token 合计）
hf_cb_sessions_summary <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    s <- dbGetQuery(con, "SELECT COUNT(*) n, COALESCE(SUM(token_total),0) tok, COALESCE(SUM(CASE WHEN token_total>0 THEN 1 ELSE 0 END),0) tok_n FROM hf_cb_sessions")
    list(total_sessions = s$n[1], total_tokens = s$tok[1], token_sessions = s$tok_n[1])
  }, error = function(e) list(total_sessions = 0, total_tokens = 0, token_sessions = 0))
}

# ══════════════════════════════════════════════════════════════════
# 自动架构数据层（自动架构模块 = 数据驱动，随代码演进自动更新）
# 与父项目 system_architecture.r 的「手工硬编码文档」不同，这里从运行时真实状态
# 自动采集：环节注册表 / 模块文件 / 数据库表 / 对外接口，无需人工维护。
# ══════════════════════════════════════════════════════════════════

# 环节 → 颜色映射（与父项目 ARCH_COLORS 思路一致，但键按 HungFo 环节 key 约定）
HF_ARCH_COLORS <- c(
  "#3b82f6", "#10b981", "#ef4444", "#f59e0b", "#8b5cf6", "#6b7280",
  "#0ea5e9", "#ec4899", "#14b8a6", "#f97316", "#6366f1", "#84cc16"
)

# 自动扫描当前架构快照：环节 / 模块文件 / 数据库表 / 对外接口
hf_arch_scan <- function() {
  home <- hf_home()
  # 1) 环节注册表（内存总线，随 source 顺序动态）
  mods <- hf_modules()
  modules <- if (length(mods) == 0) list() else lapply(seq_along(mods), function(i) {
    m <- mods[[i]]
    list(id = hf_or(m$id, ""), key = hf_or(m$key, ""), name = hf_or(m$name, ""),
         icon = hf_or(m$icon, "circle"), status = hf_or(m$status, "scaffold"),
         version = hf_or(m$version, "v0.1"),
         desc = hf_or(m$desc, ""), api_desc = hf_or(m$api_desc, ""))
  })
  # 2) 模块文件（磁盘上的 hf_mod_*.r，反映真实源码结构）
  mod_files <- sort(list.files(home, pattern = "^hf_mod_.*\\.r$", full.names = FALSE))
  core_files <- sort(list.files(home, pattern = "^(hf_core|hf_config|hf_ui|app)\\.r$", full.names = FALSE))
  # 3) 数据库表（独立库 sqlite_master）
  tables <- character(0)
  con <- tryCatch(hf_db_connect(), error = function(e) NULL)
  if (!is.null(con)) {
    on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
    tt <- tryCatch(dbGetQuery(con,
      "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name"),
      error = function(e) data.frame())
    if (!is.null(tt) && nrow(tt) > 0) tables <- tt$name
  }
  # 4) 对外接口（每个环节注册到总线上的接口函数名）
  apis <- hf_bus$apis
  api_map <- list()
  if (length(apis) > 0) {
    api_map <- lapply(names(apis), function(k) {
      list(key = k, fns = sort(names(apis[[k]])))
    })
  }
  list(
    scanned_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    home = home,
    module_count = length(modules),
    modules = modules,
    mod_files = mod_files,
    core_files = core_files,
    tables = tables,
    api_map = api_map
  )
}

# 导出 CodeBuddy 会话清单到 www/hungfo_cb_sessions.js（监控页前端读取）
hf_cb_sessions_export <- function(limit = 200) {
  u <- hf_cb_sessions_list(limit)
  sm <- hf_cb_sessions_summary()
  sessions <- if (is.null(u) || nrow(u) == 0) list() else lapply(seq_len(nrow(u)), function(i) list(
    conversation_id = hf_or(u$conversation_id[i], ""), title = hf_or(u$title[i], ""),
    cwd = hf_or(u$cwd[i], ""), status = hf_or(u$status[i], ""),
    created_at = hf_or(u$created_at[i], ""), updated_at = hf_or(u$updated_at[i], ""),
    token_total = if (is.na(u$token_total[i])) 0 else u$token_total[i],
    token_input = if (is.na(u$token_input[i])) 0 else u$token_input[i],
    token_output = if (is.na(u$token_output[i])) 0 else u$token_output[i],
    token_cache_hit = if (is.na(u$token_cache_hit[i])) 0 else u$token_cache_hit[i],
    token_cache_miss = if (is.na(u$token_cache_miss[i])) 0 else u$token_cache_miss[i],
    token_cache_write = if (is.na(u$token_cache_write[i])) 0 else u$token_cache_write[i],
    token_thinking = if (is.na(u$token_thinking[i])) 0 else u$token_thinking[i],
    token_response = if (is.na(u$token_response[i])) 0 else u$token_response[i],
    token_cache_hit_rate = if (is.na(u$token_cache_hit_rate[i])) 0 else u$token_cache_hit_rate[i],
    token_updated_at = hf_or(u$token_updated_at[i], "")
  ))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), summary = sm, sessions = sessions)
  dir.create(file.path(hf_home(), "www"), showWarnings = FALSE, recursive = TRUE)
  body <- jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(paste0("// 由 HungFo 生成（hf_cb_sessions_export），勿手改\nwindow.HF_CB_SESSIONS = ", body, ";\n"),
             file.path(hf_home(), "www", "hungfo_cb_sessions.js"), useBytes = TRUE)
  invisible(out)
}
