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

# 数据库迁移：确保所有表和字段存在
migrate_database <- function() {
  if (!isTRUE(get_config("features", "auto_migrate_db"))) {
    return()
  }
  con <- db_connect()
  tryCatch({
    # 迁移1：users 表 display_name 列
    columns <- dbGetQuery(con, "PRAGMA table_info(users)")
    if (!"display_name" %in% columns$name) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN display_name TEXT")
      cat("数据库迁移完成：已添加 display_name 列到 users 表\n")
    }

    # 迁移2：项目管理相关表
    tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")$name

    if (!"projects" %in% tables) {
      dbExecute(con, "CREATE TABLE projects (
        id INTEGER PRIMARY KEY AUTOINCREMENT, project_no TEXT UNIQUE NOT NULL,
        name TEXT NOT NULL, description TEXT, status TEXT NOT NULL DEFAULT 'planning',
        priority TEXT DEFAULT '中', start_date TEXT, end_date TEXT,
        created_by INTEGER, created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
      cat("数据库迁移完成：已创建 projects 表\n")
    }

    if (!"project_phases" %in% tables) {
      dbExecute(con, "CREATE TABLE project_phases (
        id INTEGER PRIMARY KEY AUTOINCREMENT, project_id INTEGER NOT NULL,
        name TEXT NOT NULL, description TEXT, status TEXT NOT NULL DEFAULT 'pending',
        sort_order INTEGER DEFAULT 0, start_date TEXT, end_date TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP, updated_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
      cat("数据库迁移完成：已创建 project_phases 表\n")
    }

    if (!"project_work_packages" %in% tables) {
      dbExecute(con, "CREATE TABLE project_work_packages (
        id INTEGER PRIMARY KEY AUTOINCREMENT, phase_id INTEGER NOT NULL,
        project_id INTEGER NOT NULL, name TEXT NOT NULL, description TEXT,
        status TEXT NOT NULL DEFAULT 'pending', assigned_to INTEGER,
        sort_order INTEGER DEFAULT 0, start_date TEXT, end_date TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP, updated_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
      cat("数据库迁移完成：已创建 project_work_packages 表\n")
    }

    if (!"project_tasks" %in% tables) {
      dbExecute(con, "CREATE TABLE project_tasks (
        id INTEGER PRIMARY KEY AUTOINCREMENT, work_package_id INTEGER NOT NULL,
        phase_id INTEGER NOT NULL, project_id INTEGER NOT NULL,
        task_no TEXT, name TEXT NOT NULL, description TEXT,
        status TEXT NOT NULL DEFAULT 'pending', priority TEXT DEFAULT '中',
        assigned_to INTEGER, start_date TEXT, due_date TEXT,
        completed_at DATETIME, work_order_id INTEGER, created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP, updated_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
      cat("数据库迁移完成：已创建 project_tasks 表\n")
    }

    if (!"project_task_logs" %in% tables) {
      dbExecute(con, "CREATE TABLE project_task_logs (
        id INTEGER PRIMARY KEY AUTOINCREMENT, task_id INTEGER NOT NULL,
        log_type TEXT NOT NULL DEFAULT 'feedback', content TEXT NOT NULL,
        status_before TEXT, status_after TEXT, created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP)")
      cat("数据库迁移完成：已创建 project_task_logs 表\n")
    }

    if (!"std_hosts" %in% tables) {
      dbExecute(con, "CREATE TABLE std_hosts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        ip_address TEXT NOT NULL DEFAULT '',
        os TEXT NOT NULL DEFAULT '',
        username TEXT NOT NULL DEFAULT '',
        password TEXT NOT NULL DEFAULT '',
        computer_name TEXT DEFAULT '',
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )")
      cat("数据库迁移完成：已创建 std_hosts 表\n")
    }

    # 迁移：project_tasks 表添加 is_favorite 和 importance 列
    task_columns <- dbGetQuery(con, "PRAGMA table_info(project_tasks)")
    if (!"is_favorite" %in% task_columns$name) {
      dbExecute(con, "ALTER TABLE project_tasks ADD COLUMN is_favorite INTEGER DEFAULT 0")
      cat("数据库迁移完成：已添加 is_favorite 列到 project_tasks 表\n")
    }
    if (!"importance" %in% task_columns$name) {
      dbExecute(con, "ALTER TABLE project_tasks ADD COLUMN importance INTEGER DEFAULT 0")
      cat("数据库迁移完成：已添加 importance 列到 project_tasks 表\n")
    }

    # 迁移：添加字体大小配置默认值
    existing_font_cfg <- dbGetQuery(con, "SELECT config_key FROM system_config WHERE config_key = 'table_font_size'")
    if (nrow(existing_font_cfg) == 0) {
      dbExecute(con, "INSERT INTO system_config (config_key, config_value, description) VALUES ('table_font_size', '13', '列表表格字体大小(px)')")
      dbExecute(con, "INSERT INTO system_config (config_key, config_value, description) VALUES ('input_font_size', '13', '输入框和选择框字体大小(px)')")
      cat("数据库迁移完成：已添加字体大小配置项\n")
    }

    if (!"config_options" %in% tables) {
      dbExecute(con, "CREATE TABLE config_options (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        category TEXT NOT NULL,
        option_value TEXT NOT NULL,
        option_label TEXT NOT NULL,
        color TEXT DEFAULT '',
        sort_order INTEGER DEFAULT 0,
        is_default INTEGER DEFAULT 0,
        active INTEGER DEFAULT 1,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )")
      # 种子数据：project_status
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_status', 'planning', '规划中', '#5bc0de', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_status', 'active', '进行中', '#337ab7', 2)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_status', 'completed', '已完成', '#5cb85c', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_status', 'suspended', '已暂停', '#f0ad4e', 4)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_status', 'closed', '已关闭', '#777', 5)")
      # 种子数据：project_priority
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_priority', '低', '低', '#5cb85c', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default) VALUES ('project_priority', '中', '中', '#5bc0de', 2, 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_priority', '高', '高', '#f0ad4e', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('project_priority', '紧急', '紧急', '#d9534f', 4)")
      # 种子数据：phase_status
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('phase_status', 'pending', '待开始', '#f0ad4e', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('phase_status', 'active', '进行中', '#337ab7', 2)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('phase_status', 'completed', '已完成', '#5cb85c', 3)")
      # 种子数据：wp_status
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('wp_status', 'pending', '待开始', '#f0ad4e', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('wp_status', 'active', '进行中', '#337ab7', 2)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('wp_status', 'completed', '已完成', '#5cb85c', 3)")
      # 种子数据：task_status
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_status', 'pending', '待处理', '#f0ad4e', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_status', 'in_progress', '进行中', '#337ab7', 2)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_status', 'completed', '已完成', '#5cb85c', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_status', 'blocked', '已阻塞', '#d9534f', 4)")
      # 种子数据：task_priority
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_priority', '低', '低', '#5cb85c', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default) VALUES ('task_priority', '中', '中', '#5bc0de', 2, 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_priority', '高', '高', '#f0ad4e', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('task_priority', '紧急', '紧急', '#d9534f', 4)")
      cat("数据库迁移完成：已创建 config_options 表并初始化默认数据\n")
    }

  }, error = function(e) {
    cat("数据库迁移失败:", e$message, "\n")
  }, finally = {
    db_disconnect(con)
  })
}

check_database()
migrate_database()
