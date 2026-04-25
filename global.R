library(shiny)
library(shinythemes)
library(DT)
library(RSQLite)
library(DBI)
library(ggplot2)
library(plotly)

# 加载配置管理器（必须在其他 source 之前，因为后续模块可能依赖配置）
source("config/config_loader.r")
load_config()

# 从配置中获取数据库路径
db_path <- get_db_path()

db_connect <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}

db_disconnect <- function(con) {
  dbDisconnect(con)
}

check_database <- function() {
  if (!file.exists(db_path)) {
    stop("数据库文件不存在: ", db_path)
  }
}

# 数据库迁移：确保 users 表包含 display_name 列
migrate_database <- function() {
  if (!isTRUE(get_config("features", "auto_migrate_db"))) {
    return()
  }
  con <- db_connect()
  tryCatch({
    columns <- dbGetQuery(con, "PRAGMA table_info(users)")
    if (!"display_name" %in% columns$name) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN display_name TEXT")
      cat("数据库迁移完成：已添加 display_name 列到 users 表\n")
    }
  }, error = function(e) {
    cat("数据库迁移失败:", e$message, "\n")
  }, finally = {
    db_disconnect(con)
  })
}

check_database()
migrate_database()
