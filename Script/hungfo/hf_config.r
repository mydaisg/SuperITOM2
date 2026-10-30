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

# ── 独立数据库配置（HF_HungFo.DB）──
# DB 路径由 hf_core.r 的 hf_db_file() 统一解析（环境变量 HUNGFO_DB > 默认 hungfo/DB/HF_HungFo.DB）。
# 这里仅保留向后兼容的 db_connect / db_disconnect 别名，内部转调 hf_db_connect / hf_db_disconnect，
# 保证老代码/旧调用仍可用，且一律指向 HungFo 独立库，不与父项目 GH_ITOM.db 冲突。
if (!exists("db_connect", inherits = TRUE)) {
  db_connect <- function() hf_db_connect()
}
if (!exists("db_disconnect", inherits = TRUE)) {
  db_disconnect <- function(con) hf_db_disconnect(con)
}
