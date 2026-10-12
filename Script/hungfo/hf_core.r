##################
# HungFo 子项目 — 核心层
# 独立子项目（R + Shiny + SQLite），最终会独立出去，因此自包含、不依赖现有业务模块。
# 12 个环节：规则 / 爬取 / 数据 / 算法 / 展示 / 自动任务 / 回测 / 元任务 / 开发日志 / 自动架构 / 对外集成 / 权限体系
# 设计：每个环节 = 一个独立模块文件（hf_mod_*.r），彼此功能独立，通过「注册表 / 总线」互相提供接口。
##################

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
