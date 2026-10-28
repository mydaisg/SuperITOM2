##################
# HungFo 子项目 — 独立配置层
# 职责：独立运行时加载依赖库、提供 DB 访问层与通用操作符。
# 仅在「独立运行」时由 app.R 加载；父项目内嵌时由 global.R 提供 db_connect / db_path / 库，
# 因此本文件内的定义一律用 exists() 守卫，避免与父项目冲突。
##################

suppressMessages({
  library(shiny)
  library(shinythemes)
  library(DT)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
})

# 空值兜底操作符（父项目由 process_engine.r 提供；独立运行在此补齐）
if (!exists("%||%", inherits = TRUE)) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

# 独立 DB 文件路径：优先环境变量 HUNGFO_DB，否则用 hungfo 目录内 DB/hungfo.db
hf_db_file <- function() {
  env <- Sys.getenv("HUNGFO_DB")
  if (nzchar(env)) return(env)
  file.path(hf_home(), "DB", "hungfo.db")
}

# DB 访问层（仅在父项目未提供时定义，避免覆盖父项目 global.R 的实现）
if (!exists("db_connect", inherits = TRUE)) {
  db_connect <- function() {
    dir.create(dirname(hf_db_file()), showWarnings = FALSE, recursive = TRUE)
    con <- dbConnect(RSQLite::SQLite(), hf_db_file())
    dbExecute(con, "PRAGMA busy_timeout = 5000")
    dbExecute(con, "PRAGMA localtime = 1")
    con
  }
}
if (!exists("db_disconnect", inherits = TRUE)) {
  db_disconnect <- function(con) dbDisconnect(con)
}
