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
  # 等待最多5秒（而不是立即失败），解决并发写入时的 database is locked 错误
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  # 设置为本地时区，避免 CURRENT_TIMESTAMP 返回 UTC 时间
  dbExecute(con, "PRAGMA localtime = 1")
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
    
    # 迁移：确保 work_order_comments 表存在
    if (!"work_order_comments" %in% tables) {
      dbExecute(con, "CREATE TABLE work_order_comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        work_order_id INTEGER NOT NULL,
        comment TEXT NOT NULL,
        created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )")
      cat("数据库迁移完成：已创建 work_order_comments 表\n")
    } else {
      # 检查并添加缺失的 created_at 列
      comment_columns <- dbGetQuery(con, "PRAGMA table_info(work_order_comments)")
      if (!"created_at" %in% comment_columns$name) {
        dbExecute(con, "ALTER TABLE work_order_comments ADD COLUMN created_at DATETIME DEFAULT CURRENT_TIMESTAMP")
        cat("数据库迁移完成：已添加 created_at 列到 work_order_comments 表\n")
      }
    }

    # 迁移：修复 work_orders 表中的重复工单号（按日期顺序重新编号）
    if ("work_orders" %in% tables) {
      # 检查 order_no 是否有重复值
      dup_check <- dbGetQuery(con, "SELECT order_no, COUNT(*) as cnt FROM work_orders WHERE order_no IS NOT NULL AND order_no != '' GROUP BY order_no HAVING cnt > 1")
      if (nrow(dup_check) > 0) {
        cat("发现 work_orders 表中有重复工单号，正在按顺序重新编号...\n")
        # 获取今天日期前缀
        today <- format(Sys.Date(), "%Y%m%d")
        prefix <- paste0("ITS", today)
        
        # 找出今天前缀的最大序号
        max_seq_query <- sprintf("SELECT order_no FROM work_orders WHERE order_no LIKE '%s%%' AND order_no NOT LIKE '%%_copy%%' ORDER BY order_no DESC LIMIT 1", prefix)
        max_seq_result <- dbGetQuery(con, max_seq_query)
        
        if (nrow(max_seq_result) > 0) {
          last_seq <- as.integer(substr(max_seq_result$order_no[1], nchar(prefix) + 1, nchar(max_seq_result$order_no[1])))
        } else {
          last_seq <- 0
        }
        
        # 修复每个重复的工单号
        for (i in 1:nrow(dup_check)) {
          dup_order_no <- dup_check$order_no[i]
          # 获取该工单号的所有记录，按 ID 排序
          dup_records <- dbGetQuery(con, sprintf("SELECT id FROM work_orders WHERE order_no = '%s' ORDER BY id", dup_order_no))
          if (nrow(dup_records) > 1) {
            # 第一条保留，其他的需要更新为下一个序号
            for (j in 2:nrow(dup_records)) {
              old_id <- dup_records$id[j]
              last_seq <- last_seq + 1
              new_order_no <- sprintf("%s%03d", prefix, last_seq)
              dbExecute(con, sprintf("UPDATE work_orders SET order_no = '%s' WHERE id = %d", new_order_no, old_id))
              cat(sprintf("  修复：ID=%d 的工单号从 %s 改为 %s\n", old_id, dup_order_no, new_order_no))
            }
          }
        }
        cat("重复工单号修复完成\n")
      }
      cat("数据库迁移完成：work_orders 表检查完毕\n")
    }

    # 迁移：work_orders 表添加 request_user 列（请求用户/工单来源者）
    if ("work_orders" %in% tables) {
      wo_columns <- dbGetQuery(con, "PRAGMA table_info(work_orders)")
      if (!"request_user" %in% wo_columns$name) {
        dbExecute(con, "ALTER TABLE work_orders ADD COLUMN request_user TEXT")
        cat("数据库迁移完成：已添加 request_user 列到 work_orders 表\n")
      }
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

    # 迁移：添加工单配置默认值
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
      # 种子数据：work_order_status
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default) VALUES ('work_order_status', 'pending', '待处理', '#f0ad4e', 1, 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_status', 'assigned', '已派发', '#5bc0de', 2)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_status', 'processing', '处理中', '#ff9800', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_status', 'completed', '已完成', '#5cb85c', 4)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_status', 'closed', '已关闭', '#d9534f', 5)")
      # 种子数据：work_order_priority
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_priority', '低', '低', '#5cb85c', 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default) VALUES ('work_order_priority', '中', '中', '#f0ad4e', 2, 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_priority', '高', '高', '#ff9800', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_priority', '紧急', '紧急', '#d9534f', 4)")
      # 种子数据：work_order_category
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default) VALUES ('work_order_category', '一般', '一般', '#5bc0de', 1, 1)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_category', '硬件故障', '硬件故障', '#d9534f', 2)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_category', '软件故障', '软件故障', '#ff9800', 3)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_category', '网络问题', '网络问题', '#5f9ea0', 4)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_category', '系统维护', '系统维护', '#6a5acd', 5)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_category', '账号权限', '账号权限', '#9370db', 6)")
      dbExecute(con, "INSERT INTO config_options (category, option_value, option_label, color, sort_order) VALUES ('work_order_category', '其他', '其他', '#999', 7)")
      cat("数据库迁移完成：已创建 config_options 表并初始化默认数据\n")
    }

    # ===============================================
    # 巡检管理模块数据库表
    # ===============================================
    
    # 巡检计划表 (inspection_plans)
    if (!"inspection_plans" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_plans (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_no TEXT UNIQUE NOT NULL,
        name TEXT NOT NULL,
        description TEXT,
        category TEXT DEFAULT '日常巡检',
        inspection_category TEXT NOT NULL,
        cycle_type TEXT DEFAULT 'once',
        cycle_value TEXT,
        start_date TEXT,
        end_date TEXT,
        responsible_user INTEGER,
        status TEXT DEFAULT 'draft',
        created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )")
      cat("数据库迁移完成：已创建 inspection_plans 表\n")
    }
    
    # 巡检检查项表 (inspection_items)
    if (!"inspection_items" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_id INTEGER NOT NULL,
        category TEXT NOT NULL,
        item_name TEXT NOT NULL,
        item_description TEXT,
        check_standard TEXT,
        scoring_type TEXT DEFAULT 'pass_fail',
        max_score INTEGER DEFAULT 100,
        sort_order INTEGER DEFAULT 0,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (plan_id) REFERENCES inspection_plans(id)
      )")
      cat("数据库迁移完成：已创建 inspection_items 表\n")
    }
    
    # 巡检检查项模板表 (inspection_item_templates)
    if (!"inspection_item_templates" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_item_templates (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        category TEXT NOT NULL,
        item_name TEXT NOT NULL,
        item_description TEXT,
        check_standard TEXT,
        scoring_type TEXT DEFAULT 'pass_fail',
        max_score INTEGER DEFAULT 100,
        sort_order INTEGER DEFAULT 0,
        active INTEGER DEFAULT 1,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )")
      cat("数据库迁移完成：已创建 inspection_item_templates 表\n")
      
      # 初始化默认检查项模板
      templates <- list(
        # 数据中心巡检
        list(category = "数据中心巡检", name = "温湿度检查", desc = "检查数据中心温湿度", standard = "温度18-26℃，湿度40-60%"),
        list(category = "数据中心巡检", name = "消防设备检查", desc = "检查消防栓、灭火器", standard = "设备完好，在有效期内"),
        list(category = "数据中心巡检", name = "UPS运行检查", desc = "检查UPS状态", standard = "UPS运行正常，负载率<80%"),
        list(category = "数据中心巡检", name = "空调运行检查", desc = "检查精密空调", standard = "空调运行正常，无报警"),
        list(category = "数据中心巡检", name = "门禁系统检查", desc = "检查门禁记录", standard = "无异常进入记录"),
        # 电力机房巡检
        list(category = "电力机房巡检", name = "配电柜检查", desc = "检查配电柜状态", standard = "指示灯正常，无报警"),
        list(category = "电力机房巡检", name = "蓄电池检查", desc = "检查蓄电池", standard = "电压正常，无漏液"),
        list(category = "电力机房巡检", name = "市电输入检查", desc = "检查市电供电", standard = "市电供电正常"),
        list(category = "电力机房巡检", name = "发电机检查", desc = "检查发电机", standard = "油位正常，待机状态"),
        # 会议室巡检
        list(category = "会议室巡检", name = "投影设备检查", desc = "检查投影仪", standard = "投影清晰，遥控正常"),
        list(category = "会议室巡检", name = "音响设备检查", desc = "检查音响", standard = "音质正常，无杂音"),
        list(category = "会议室巡检", name = "视频会议检查", desc = "检查视频会议设备", standard = "画面清晰，麦克风正常"),
        list(category = "会议室巡检", name = "照明空调检查", desc = "检查照明和空调", standard = "设备正常，可用"),
        # 设备间巡检
        list(category = "设备间巡检", name = "网络设备检查", desc = "检查交换机、路由器", standard = "指示灯正常，无报错"),
        list(category = "设备间巡检", name = "服务器运行检查", desc = "检查服务器状态", standard = "系统运行正常，无告警"),
        list(category = "设备间巡检", name = "布线整理检查", desc = "检查线缆布线", standard = "线缆整齐，无脱落"),
        list(category = "设备间巡检", name = "环境卫生检查", desc = "检查设备间卫生", standard = "无灰尘，无杂物")
      )
      
      for (i in seq_along(templates)) {
        t <- templates[[i]]
        dbExecute(con, sprintf(
          "INSERT INTO inspection_item_templates (category, item_name, item_description, check_standard, scoring_type, max_score, sort_order) 
           VALUES ('%s', '%s', '%s', '%s', 'pass_fail', 100, %d)",
          t$category, gsub("'", "''", t$name), gsub("'", "''", t$desc), gsub("'", "''", t$standard), i
        ))
      }
      cat("数据库迁移完成：已初始化检查项模板数据\n")
    }
    
    # 迁移：为已存在的表添加新字段
    # inspection_plans 表添加 inspection_category 字段
    columns_plans <- dbGetQuery(con, "PRAGMA table_info(inspection_plans)")
    if ("inspection_category" %in% columns_plans$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_plans ADD COLUMN inspection_category TEXT")
        cat("数据库迁移完成：已添加 inspection_category 列到 inspection_plans 表\n")
      }, error = function(e) {
        cat("警告：添加 inspection_category 列失败:", e$message, "\n")
      })
    }
    
    # inspection_items 表添加 category 字段
    columns_items <- dbGetQuery(con, "PRAGMA table_info(inspection_items)")
    if ("category" %in% columns_items$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_items ADD COLUMN category TEXT DEFAULT ''")
        cat("数据库迁移完成：已添加 category 列到 inspection_items 表\n")
      }, error = function(e) {
        cat("警告：添加 category 列失败:", e$message, "\n")
      })
    }
    
    # 巡检任务表 (inspection_tasks)
    if (!"inspection_tasks" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_tasks (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        task_no TEXT UNIQUE NOT NULL,
        plan_id INTEGER NOT NULL,
        item_id INTEGER,
        item_name TEXT,
        item_description TEXT,
        check_standard TEXT,
        inspector INTEGER,
        scheduled_date TEXT,
        location TEXT,
        status TEXT DEFAULT 'pending',
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (plan_id) REFERENCES inspection_plans(id),
        FOREIGN KEY (inspector) REFERENCES users(id)
      )")
      cat("数据库迁移完成：已创建 inspection_tasks 表\n")
    }
    
    # 巡检记录表 (inspection_records)
    if (!"inspection_records" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_records (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        task_id INTEGER NOT NULL,
        inspector INTEGER,
        result_type TEXT NOT NULL,
        score INTEGER,
        remark TEXT,
        photos TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (task_id) REFERENCES inspection_tasks(id),
        FOREIGN KEY (inspector) REFERENCES users(id)
      )")
      cat("数据库迁移完成：已创建 inspection_records 表\n")
    }
    
    # 巡检问题/异常表 (inspection_issues)
    if (!"inspection_issues" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_issues (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        record_id INTEGER,
        task_id INTEGER,
        issue_type TEXT,
        issue_description TEXT,
        severity TEXT DEFAULT 'medium',
        photos TEXT,
        related_work_order_id INTEGER,
        status TEXT DEFAULT 'pending',
        created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (task_id) REFERENCES inspection_tasks(id),
        FOREIGN KEY (related_work_order_id) REFERENCES work_orders(id)
      )")
      cat("数据库迁移完成：已创建 inspection_issues 表\n")
    }

    # 巡检计划评论表 (inspection_plan_comments)
    if (!"inspection_plan_comments" %in% tables) {
      dbExecute(con, "CREATE TABLE inspection_plan_comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_id INTEGER NOT NULL,
        comment TEXT NOT NULL,
        created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (plan_id) REFERENCES inspection_plans(id)
      )")
      cat("数据库迁移完成：已创建 inspection_plan_comments 表\n")
    }

    # 检查 inspection_plans 表是否有 updated_at 字段（兼容旧数据库）
    columns_plans <- dbGetQuery(con, "PRAGMA table_info(inspection_plans)")
    if ("updated_at" %in% columns_plans$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_plans ADD COLUMN updated_at DATETIME DEFAULT CURRENT_TIMESTAMP")
        cat("数据库迁移完成：已添加 updated_at 列到 inspection_plans 表\n")
      }, error = function(e) {
        cat("警告：添加 updated_at 列失败:", e$message, "\n")
      })
    }
    
    # ===============================================
    # 巡检模块软删除支持
    # ===============================================
    
    # inspection_plans 表添加 is_deleted 字段
    if ("is_deleted" %in% columns_plans$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_plans ADD COLUMN is_deleted INTEGER DEFAULT 0")
        cat("数据库迁移完成：已添加 is_deleted 列到 inspection_plans 表\n")
      }, error = function(e) {
        cat("警告：添加 is_deleted 列失败:", e$message, "\n")
      })
    }
    
    # inspection_tasks 表添加 is_deleted 字段
    columns_tasks <- dbGetQuery(con, "PRAGMA table_info(inspection_tasks)")
    if ("is_deleted" %in% columns_tasks$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_tasks ADD COLUMN is_deleted INTEGER DEFAULT 0")
        cat("数据库迁移完成：已添加 is_deleted 列到 inspection_tasks 表\n")
      }, error = function(e) {
        cat("警告：添加 is_deleted 列失败:", e$message, "\n")
      })
    }
    
    # inspection_tasks 表添加新字段（支持多检查项JSON存储）
    columns_tasks <- dbGetQuery(con, "PRAGMA table_info(inspection_tasks)")
    if ("item_ids" %in% columns_tasks$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_tasks ADD COLUMN item_ids TEXT")
        cat("数据库迁移完成：已添加 item_ids 列到 inspection_tasks 表\n")
      }, error = function(e) {
        cat("警告：添加 item_ids 列失败:", e$message, "\n")
      })
    }
    if ("item_names" %in% columns_tasks$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_tasks ADD COLUMN item_names TEXT")
        cat("数据库迁移完成：已添加 item_names 列到 inspection_tasks 表\n")
      }, error = function(e) {
        cat("警告：添加 item_names 列失败:", e$message, "\n")
      })
    }
    if ("check_standards" %in% columns_tasks$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_tasks ADD COLUMN check_standards TEXT")
        cat("数据库迁移完成：已添加 check_standards 列到 inspection_tasks 表\n")
      }, error = function(e) {
        cat("警告：添加 check_standards 列失败:", e$message, "\n")
      })
    }
    
    # inspection_records 表添加 is_deleted 字段
    columns_records <- dbGetQuery(con, "PRAGMA table_info(inspection_records)")
    if ("is_deleted" %in% columns_records$name == FALSE) {
      tryCatch({
        dbExecute(con, "ALTER TABLE inspection_records ADD COLUMN is_deleted INTEGER DEFAULT 0")
        cat("数据库迁移完成：已添加 is_deleted 列到 inspection_records 表\n")
      }, error = function(e) {
        cat("警告：添加 is_deleted 列失败:", e$message, "\n")
      })
    }

    # ===============================================
    # 流程引擎模块数据库表
    # ===============================================
    
    if (!"process_definitions" %in% tables) {
      dbExecute(con, "CREATE TABLE process_definitions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        def_no TEXT NOT NULL,
        name TEXT NOT NULL,
        description TEXT,
        version INTEGER DEFAULT 1,
        definition TEXT,
        category TEXT DEFAULT 'general',
        status TEXT DEFAULT 'draft',
        created_by INTEGER,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime')),
        UNIQUE(def_no, version)
      )")
      cat("数据库迁移完成：已创建 process_definitions 表\n")
    }
    
    if (!"process_definition_versions" %in% tables) {
      dbExecute(con, "CREATE TABLE process_definition_versions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        def_id INTEGER NOT NULL,
        version INTEGER NOT NULL,
        definition TEXT NOT NULL,
        change_log TEXT,
        published_by INTEGER,
        published_at TEXT DEFAULT (datetime('now','localtime')),
        UNIQUE(def_id, version)
      )")
      cat("数据库迁移完成：已创建 process_definition_versions 表\n")
    }
    
    if (!"process_instances" %in% tables) {
      dbExecute(con, "CREATE TABLE process_instances (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_no TEXT NOT NULL UNIQUE,
        def_id INTEGER NOT NULL,
        def_version INTEGER DEFAULT 1,
        title TEXT,
        status TEXT DEFAULT 'running',
        priority TEXT DEFAULT 'normal',
        context_data TEXT,
        context_version INTEGER DEFAULT 0,
        current_node TEXT,
        started_by INTEGER,
        started_at TEXT DEFAULT (datetime('now','localtime')),
        completed_at TEXT,
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 process_instances 表\n")
    }
    
    if (!"process_nodes" %in% tables) {
      dbExecute(con, "CREATE TABLE process_nodes (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER NOT NULL,
        node_id TEXT NOT NULL,
        node_type TEXT NOT NULL,
        node_name TEXT,
        status TEXT DEFAULT 'pending',
        assignee INTEGER,
        auto_action TEXT,
        timeout_minutes INTEGER DEFAULT 0,
        timeout_action TEXT DEFAULT 'terminate',
        retry_count INTEGER DEFAULT 0,
        max_retries INTEGER DEFAULT 3,
        entered_at TEXT,
        completed_at TEXT,
        result TEXT,
        remark TEXT,
        UNIQUE(instance_id, node_id)
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pnode_inst ON process_nodes(instance_id)")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pnode_status ON process_nodes(status)")
      cat("数据库迁移完成：已创建 process_nodes 表及索引\n")
    }
    
    if (!"process_logs" %in% tables) {
      dbExecute(con, "CREATE TABLE process_logs (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER,
        node_id TEXT,
        log_level TEXT DEFAULT 'info',
        log_type TEXT,
        message TEXT NOT NULL,
        duration_ms INTEGER,
        detail TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_plog_inst ON process_logs(instance_id)")
      cat("数据库迁移完成：已创建 process_logs 表及索引\n")
    }
    
    if (!"process_events" %in% tables) {
      dbExecute(con, "CREATE TABLE process_events (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        event_type TEXT NOT NULL,
        instance_id INTEGER,
        node_id TEXT,
        source TEXT,
        status TEXT DEFAULT 'success',
        message TEXT,
        payload TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pevent_type ON process_events(event_type)")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pevent_inst ON process_events(instance_id)")
      cat("数据库迁移完成：已创建 process_events 表及索引\n")
    }
    
    if (!"process_context_history" %in% tables) {
      dbExecute(con, "CREATE TABLE process_context_history (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER NOT NULL,
        version INTEGER NOT NULL,
        context_data TEXT NOT NULL,
        changed_by TEXT,
        change_reason TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        UNIQUE(instance_id, version)
      )")
      cat("数据库迁移完成：已创建 process_context_history 表\n")
    }

    if (!"process_links" %in% tables) {
      dbExecute(con, "CREATE TABLE process_links (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER NOT NULL,
        module_type TEXT NOT NULL,
        module_id INTEGER NOT NULL,
        module_no TEXT,
        link_type TEXT DEFAULT 'source',
        created_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 process_links 表\n")
    }

    if (!"process_form_templates" %in% tables) {
      dbExecute(con, "CREATE TABLE process_form_templates (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        description TEXT,
        category TEXT DEFAULT 'general',
        created_by INTEGER,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 process_form_templates 表\n")
    }

    if (!"process_form_template_fields" %in% tables) {
      dbExecute(con, "CREATE TABLE process_form_template_fields (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        template_id INTEGER NOT NULL,
        field_key TEXT NOT NULL,
        field_label TEXT NOT NULL,
        field_type TEXT DEFAULT 'text',
        field_options TEXT,
        required INTEGER DEFAULT 0,
        sort_order INTEGER DEFAULT 0,
        default_value TEXT
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ff_tpl ON process_form_template_fields(template_id)")
      cat("数据库迁移完成：已创建 process_form_template_fields 表及索引\n")
    }

    # ===============================================
    # 绩效模块数据库表
    # ===============================================

    if (!"performance_sheets" %in% tables) {
      dbExecute(con, "CREATE TABLE performance_sheets (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        year_month TEXT NOT NULL UNIQUE,
        status TEXT DEFAULT 'draft',
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 performance_sheets 表\n")
    }

    if (!"performance_work_items" %in% tables) {
      dbExecute(con, "CREATE TABLE performance_work_items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        sheet_id INTEGER NOT NULL,
        employee_id INTEGER NOT NULL,
        indicator_code TEXT NOT NULL,
        source_type TEXT,
        source_id INTEGER,
        source_title TEXT,
        deduction_level INTEGER DEFAULT 0,
        created_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pwi_sheet ON performance_work_items(sheet_id)")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pwi_emp ON performance_work_items(employee_id)")
      cat("数据库迁移完成：已创建 performance_work_items 表及索引\n")
    }

    # ===============================================
    # 审批模块数据库表（企业微信风格）
    # ===============================================

    if (!"appr_templates" %in% tables) {
      dbExecute(con, "CREATE TABLE appr_templates (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        description TEXT,
        category TEXT DEFAULT 'general',
        icon TEXT DEFAULT 'file-text',
        form_fields TEXT,
        approver_config TEXT,
        cc_config TEXT,
        status TEXT DEFAULT 'draft',
        created_by INTEGER,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 appr_templates 表\n")
    }

    if (!"appr_instances" %in% tables) {
      dbExecute(con, "CREATE TABLE appr_instances (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_no TEXT UNIQUE NOT NULL,
        template_id INTEGER,
        template_name TEXT,
        title TEXT,
        form_data TEXT,
        current_step INTEGER DEFAULT 0,
        status TEXT DEFAULT 'pending',
        applicant_id INTEGER,
        started_at TEXT DEFAULT (datetime('now','localtime')),
        completed_at TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_appr_inst_applicant ON appr_instances(applicant_id)")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_appr_inst_status ON appr_instances(status)")
      cat("数据库迁移完成：已创建 appr_instances 表及索引\n")
    }

    if (!"appr_steps" %in% tables) {
      dbExecute(con, "CREATE TABLE appr_steps (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER NOT NULL,
        step_index INTEGER NOT NULL,
        step_type TEXT DEFAULT 'approver',
        operator_type TEXT DEFAULT 'fixed',
        operator_ids TEXT,
        approver_names TEXT,
        status TEXT DEFAULT 'pending',
        entered_at TEXT,
        UNIQUE(instance_id, step_index)
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_appr_step_inst ON appr_steps(instance_id)")
      cat("数据库迁移完成：已创建 appr_steps 表及索引\n")
    }

    if (!"appr_records" %in% tables) {
      dbExecute(con, "CREATE TABLE appr_records (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER NOT NULL,
        step_id INTEGER,
        operator_id INTEGER,
        operator_name TEXT,
        action TEXT NOT NULL,
        comment TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_appr_rec_inst ON appr_records(instance_id)")
      cat("数据库迁移完成：已创建 appr_records 表及索引\n")
    }

    if (!"appr_cc_records" %in% tables) {
      dbExecute(con, "CREATE TABLE appr_cc_records (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instance_id INTEGER NOT NULL,
        user_id INTEGER NOT NULL,
        user_name TEXT,
        is_read INTEGER DEFAULT 0,
        read_at TEXT
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_appr_cc_user ON appr_cc_records(user_id)")
      cat("数据库迁移完成：已创建 appr_cc_records 表及索引\n")
    }

    # ===============================================
    # 性能监控模块数据库表（无代理监控）
    # ===============================================

    if (!"sysmon_hosts" %in% tables) {
      dbExecute(con, "CREATE TABLE sysmon_hosts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        hostname TEXT NOT NULL,
        ip TEXT NOT NULL,
        port INTEGER DEFAULT 0,
        os_type TEXT DEFAULT 'windows',
        status TEXT DEFAULT 'unknown',
        credential_id INTEGER,
        last_check TEXT,
        last_online TEXT,
        response_time_ms INTEGER DEFAULT 0,
        is_active INTEGER DEFAULT 1,
        remark TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sysmon_ip ON sysmon_hosts(ip)")
      cat("数据库迁移完成：已创建 sysmon_hosts 表及索引\n")
    }

    if (!"sysmon_checks" %in% tables) {
      dbExecute(con, "CREATE TABLE sysmon_checks (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        host_id INTEGER NOT NULL,
        check_type TEXT DEFAULT 'ping',
        status TEXT DEFAULT 'unknown',
        response_time_ms INTEGER DEFAULT 0,
        detail TEXT,
        checked_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sysmon_chk_host ON sysmon_checks(host_id)")
      cat("数据库迁移完成：已创建 sysmon_checks 表及索引\n")
    }

    # ===============================================
    # 记事模块：note_comments 添加 status / completed_at 字段
    # ===============================================
    comment_cols <- dbGetQuery(con, "PRAGMA table_info(note_comments)")
    if (isTRUE("status" %in% comment_cols$name == FALSE)) {
      tryCatch({
        dbExecute(con, "ALTER TABLE note_comments ADD COLUMN status TEXT")
        cat("数据库迁移完成：已添加 status 列到 note_comments 表\n")
      }, error = function(e) {
        cat("警告：添加 status 列失败:", e$message, "\n")
      })
    }
    if (isTRUE("completed_at" %in% comment_cols$name == FALSE)) {
      tryCatch({
        dbExecute(con, "ALTER TABLE note_comments ADD COLUMN completed_at TEXT")
        cat("数据库迁移完成：已添加 completed_at 列到 note_comments 表\n")
      }, error = function(e) {
        cat("警告：添加 completed_at 列失败:", e$message, "\n")
      })
    }
    if (isTRUE("parent_id" %in% comment_cols$name == FALSE)) {
      tryCatch({
        dbExecute(con, "ALTER TABLE note_comments ADD COLUMN parent_id INTEGER")
        cat("数据库迁移完成：已添加 parent_id 列到 note_comments 表\n")
      }, error = function(e) {
        cat("警告：添加 parent_id 列失败:", e$message, "\n")
      })
    }
    # notes 表置顶
    note_cols <- dbGetQuery(con, "PRAGMA table_info(notes)")
    if (isTRUE("pinned" %in% note_cols$name == FALSE)) {
      tryCatch({
        dbExecute(con, "ALTER TABLE notes ADD COLUMN pinned INTEGER DEFAULT 0")
        cat("数据库迁移完成：已添加 pinned 列到 notes 表\n")
      }, error = function(e) {
        cat("警告：添加 pinned 列失败:", e$message, "\n")
      })
    }

    # ===============================================
    # 记事派发表（admin派发记事给多个user）
    # ===============================================
    if (!"note_dispatches" %in% tables) {
      dbExecute(con, "CREATE TABLE note_dispatches (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        note_id INTEGER NOT NULL,
        user_id INTEGER NOT NULL,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        FOREIGN KEY (note_id) REFERENCES notes(id),
        FOREIGN KEY (user_id) REFERENCES users(id),
        UNIQUE(note_id, user_id)
      )")
      cat("数据库迁移完成：已创建 note_dispatches 表\n")
    }

    # ===============================================
    # RBAC 授权管理（角色-权限-用户）
    # ===============================================
    if (!"rbac_permissions" %in% tables) {
      dbExecute(con, "CREATE TABLE rbac_permissions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        module TEXT NOT NULL,
        component TEXT DEFAULT '',
        code TEXT UNIQUE NOT NULL,
        name TEXT NOT NULL,
        description TEXT DEFAULT ''
      )")
      dbExecute(con, "CREATE TABLE rbac_roles (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT UNIQUE NOT NULL,
        description TEXT DEFAULT ''
      )")
      dbExecute(con, "CREATE TABLE rbac_role_permissions (
        role_id INTEGER NOT NULL,
        permission_id INTEGER NOT NULL,
        PRIMARY KEY (role_id, permission_id),
        FOREIGN KEY (role_id) REFERENCES rbac_roles(id),
        FOREIGN KEY (permission_id) REFERENCES rbac_permissions(id)
      )")
      dbExecute(con, "CREATE TABLE rbac_user_roles (
        user_id INTEGER NOT NULL,
        role_id INTEGER NOT NULL,
        PRIMARY KEY (user_id, role_id),
        FOREIGN KEY (user_id) REFERENCES users(id),
        FOREIGN KEY (role_id) REFERENCES rbac_roles(id)
      )")
      cat("数据库迁移完成：已创建 RBAC 授权管理表\n")
    }
    # 兼容：旧 rbac_permissions 表没有 component 列
    perm_cols <- dbGetQuery(con, "PRAGMA table_info(rbac_permissions)")
    if (nrow(perm_cols) > 0 && !("component" %in% perm_cols$name)) {
      tryCatch({
        dbExecute(con, "ALTER TABLE rbac_permissions ADD COLUMN component TEXT DEFAULT ''")
        cat("数据库迁移完成：已添加 component 列到 rbac_permissions 表\n")
      }, error = function(e) cat("警告：添加 component 列失败:", e$message, "\n"))
    }
    # 初始化种子权限（idempotent）
    perms_exist <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM rbac_permissions")$cnt[1]
    if (perms_exist == 0) {
      seed_perms <- rbind(
        data.frame(module="首页",   component="看板",     code="home_view",       name="查看", description="查看首页统计数据"),
        data.frame(module="记事",   component="看板",     code="note_view",       name="查看", description="查看记事看板"),
        data.frame(module="记事",   component="操作",     code="note_create",     name="创建", description="创建新记事"),
        data.frame(module="记事",   component="操作",     code="note_edit",       name="编辑", description="编辑自己的记事"),
        data.frame(module="记事",   component="操作",     code="note_delete",     name="删除", description="删除自己的记事"),
        data.frame(module="记事",   component="派发",     code="note_dispatch",   name="派发", description="派发记事给其他用户"),
        data.frame(module="工单",   component="列表",     code="wo_view",         name="查看", description="查看工单列表"),
        data.frame(module="工单",   component="列表",     code="wo_create",       name="创建", description="创建新工单"),
        data.frame(module="工单",   component="列表",     code="wo_edit",         name="编辑", description="编辑工单"),
        data.frame(module="工单",   component="操作",     code="wo_assign",       name="派发", description="派发和流转工单"),
        data.frame(module="工单",   component="操作",     code="wo_delete",       name="删除", description="删除工单"),
        data.frame(module="项目",   component="列表",     code="proj_view",       name="查看", description="查看项目列表"),
        data.frame(module="项目",   component="操作",     code="proj_create",     name="创建", description="创建新项目"),
        data.frame(module="项目",   component="操作",     code="proj_edit",       name="编辑", description="编辑项目"),
        data.frame(module="项目",   component="操作",     code="proj_delete",     name="删除", description="删除项目"),
        data.frame(module="项目",   component="钻入",     code="proj_manage",     name="管理", description="管理阶段/工作包/任务"),
        data.frame(module="巡检",   component="看板",     code="insp_view",       name="查看", description="查看巡检"),
        data.frame(module="巡检",   component="操作",     code="insp_create",     name="创建", description="创建巡检计划"),
        data.frame(module="巡检",   component="操作",     code="insp_execute",    name="执行", description="执行巡检任务"),
        data.frame(module="巡检",   component="操作",     code="insp_manage",     name="管理", description="管理计划和模板"),
        data.frame(module="日报",   component="报表",     code="dr_view",         name="查看", description="查看日报"),
        data.frame(module="岗职",   component="矩阵",     code="duty_view",       name="查看", description="查看岗位职责矩阵"),
        data.frame(module="岗职",   component="编辑",     code="duty_manage",     name="管理", description="编辑岗位/人员/职责"),
        data.frame(module="绩效",   component="报表",     code="perf_view",       name="查看", description="查看绩效数据"),
        data.frame(module="绩效",   component="编辑",     code="perf_create",     name="添加", description="手工添加工作项"),
        data.frame(module="绩效",   component="编辑",     code="perf_manage",     name="管理", description="评定和月表管理"),
        data.frame(module="资产",   component="列表",     code="asset_view",      name="查看", description="查看资产列表"),
        data.frame(module="资产",   component="编辑",     code="asset_manage",    name="管理", description="编辑资产信息"),
        data.frame(module="测试",   component="面板",     code="ntest_view",      name="查看", description="查看网络测试"),
        data.frame(module="测试",   component="面板",     code="ntest_run",       name="执行", description="执行网络测试"),
        data.frame(module="标准化", component="列表",     code="std_view",        name="查看", description="查看标准化管理"),
        data.frame(module="标准化", component="列表",     code="std_manage",      name="管理", description="管理主机和脚本"),
        data.frame(module="可视化", component="图表",     code="viz_view",        name="查看", description="查看可视化图表"),
        data.frame(module="管理",   component="用户",     code="admin_users",     name="管理", description="管理用户账号"),
        data.frame(module="管理",   component="系统",     code="admin_system",    name="设置", description="修改系统配置"),
        data.frame(module="管理",   component="配置",     code="admin_options",   name="选项", description="管理下拉选项"),
        data.frame(module="管理",   component="集成",     code="admin_github",    name="GitHub", description="GitHub提交/拉取"),
        data.frame(module="管理",   component="安全",     code="admin_rbac",      name="授权", description="管理角色和权限")
      )
      for (i in seq_len(nrow(seed_perms))) {
        dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_permissions (module, component, code, name, description) VALUES ('%s','%s','%s','%s','%s')",
          seed_perms$module[i], seed_perms$component[i], seed_perms$code[i], seed_perms$name[i], seed_perms$description[i]))
      }
      cat("数据库迁移完成：已初始化", nrow(seed_perms), "条 RBAC 权限\n")
      # 对已有权限更新 component（兼容旧数据）
      for (i in seq_len(nrow(seed_perms))) {
        dbExecute(con, sprintf("UPDATE rbac_permissions SET component='%s' WHERE code='%s' AND (component IS NULL OR component='')",
          seed_perms$component[i], seed_perms$code[i]))
      }
    }
    # 初始化种子角色（idempotent）
    roles_exist <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM rbac_roles")$cnt[1]
    if (roles_exist == 0) {
      dbExecute(con, "INSERT INTO rbac_roles (name, description) VALUES ('管理员', '系统管理员，拥有全部权限')")
      dbExecute(con, "INSERT INTO rbac_roles (name, description) VALUES ('普通用户', '基本用户，查看个人数据')")
      dbExecute(con, "INSERT INTO rbac_roles (name, description) VALUES ('IT工程师', 'IT工程师，处理工单和巡检')")
      cat("数据库迁移完成：已初始化 3 个 RBAC 角色\n")
    }
    # 自动分配管理员角色权限（全部权限）
    admin_role <- dbGetQuery(con, "SELECT id FROM rbac_roles WHERE name='管理员'")
    if (nrow(admin_role) > 0) {
      all_perms <- dbGetQuery(con, "SELECT id FROM rbac_permissions")
      for (pid in all_perms$id) {
        dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_role_permissions (role_id, permission_id) VALUES (%d, %d)", admin_role$id[1], pid))
      }
    }
    # 自动分配普通用户权限
    user_role <- dbGetQuery(con, "SELECT id FROM rbac_roles WHERE name='普通用户'")
    if (nrow(user_role) > 0) {
      user_perms <- c("home_view","note_view","note_create","note_edit","wo_view","wo_create",
                      "proj_view","proj_create","proj_edit","proj_manage",
                      "insp_view","insp_execute","dr_view","duty_view","perf_view","perf_create",
                      "asset_view","ntest_view","ntest_run","std_view","viz_view")
      for (code in user_perms) {
        pid <- dbGetQuery(con, sprintf("SELECT id FROM rbac_permissions WHERE code='%s'", code))
        if (nrow(pid) > 0) dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_role_permissions (role_id, permission_id) VALUES (%d, %d)", user_role$id[1], pid$id[1]))
      }
    }
    # 自动分配IT工程师权限
    eng_role <- dbGetQuery(con, "SELECT id FROM rbac_roles WHERE name='IT工程师'")
    if (nrow(eng_role) > 0) {
      eng_perms <- c("home_view","note_view","note_create","note_edit","wo_view","wo_create","wo_edit","wo_assign",
                     "proj_view","proj_create","proj_edit","proj_manage",
                     "insp_view","insp_create","insp_execute","dr_view","duty_view","perf_view","perf_create",
                     "asset_view","asset_manage","ntest_view","ntest_run","std_view","std_manage","viz_view")
      for (code in eng_perms) {
        pid <- dbGetQuery(con, sprintf("SELECT id FROM rbac_permissions WHERE code='%s'", code))
        if (nrow(pid) > 0) dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_role_permissions (role_id, permission_id) VALUES (%d, %d)", eng_role$id[1], pid$id[1]))
      }
    }
    # 迁移现有用户到 RBAC：admin→管理员, 其他→普通用户
    if (!"rbac_user_roles" %in% tables || dbGetQuery(con, "SELECT COUNT(*) as cnt FROM rbac_user_roles")$cnt == 0) {
      existing_users <- dbGetQuery(con, "SELECT id, role FROM users WHERE active = 1")
      for (i in seq_len(nrow(existing_users))) {
        uid <- existing_users$id[i]
        role_name <- if (existing_users$role[i] == "admin") "管理员" else "普通用户"
        rid <- dbGetQuery(con, sprintf("SELECT id FROM rbac_roles WHERE name='%s'", role_name))
        if (nrow(rid) > 0) dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_user_roles (user_id, role_id) VALUES (%d, %d)", uid, rid$id[1]))
      }
      cat("数据库迁移完成：已迁移现有用户到 RBAC\n")
    }

    # ===============================================
    # 岗职矩阵 — 二级任务项
    # ===============================================
    if (!"duty_sub_items" %in% tables) {
      dbExecute(con, "CREATE TABLE duty_sub_items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        duty_item_id INTEGER NOT NULL,
        name TEXT NOT NULL,
        category TEXT DEFAULT '',
        description TEXT DEFAULT '',
        sort_order INTEGER DEFAULT 0,
        FOREIGN KEY (duty_item_id) REFERENCES duty_items(id)
      )")
      cat("数据库迁移完成：已创建 duty_sub_items 表\n")
    }
    if (!"duty_sub_matrix" %in% tables) {
      dbExecute(con, "CREATE TABLE duty_sub_matrix (
        staff_id INTEGER NOT NULL,
        position_id INTEGER NOT NULL,
        duty_sub_item_id INTEGER NOT NULL,
        responsibility_level TEXT NOT NULL DEFAULT '执行',
        comment TEXT DEFAULT '',
        updated_at TEXT DEFAULT (datetime('now','localtime')),
        PRIMARY KEY (staff_id, position_id, duty_sub_item_id),
        FOREIGN KEY (staff_id) REFERENCES duty_staff(id),
        FOREIGN KEY (position_id) REFERENCES duty_positions(id),
        FOREIGN KEY (duty_sub_item_id) REFERENCES duty_sub_items(id)
      )")
      cat("数据库迁移完成：已创建 duty_sub_matrix 表\n")
    }
    # 清理旧数据：如果duty_matrix中存在已删除duty_item_id的记录
    if ("duty_matrix" %in% tables) {
      tryCatch({
        dbExecute(con, "DELETE FROM duty_matrix WHERE duty_item_id NOT IN (SELECT id FROM duty_items)")
      }, error = function(e) {})
    }

    # ===============================================
    # 资产管理模块
    # ===============================================
    if (!"assets" %in% tables) {
      dbExecute(con, "CREATE TABLE assets (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        asset_no TEXT UNIQUE NOT NULL,
        hostname TEXT NOT NULL,
        ip_address TEXT,
        os TEXT,
        cpu TEXT,
        ram TEXT,
        disk TEXT,
        manufacturer TEXT,
        model TEXT,
        serial_number TEXT,
        location TEXT,
        department TEXT,
        status TEXT DEFAULT 'active',
        notes TEXT,
        last_seen TEXT,
        created_by INTEGER,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 assets 表\n")
    }

    # 绩效结果评定表
    if (!"performance_results" %in% tables) {
      dbExecute(con, "CREATE TABLE performance_results (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        sheet_id INTEGER NOT NULL,
        employee_id INTEGER NOT NULL,
        result TEXT,
        benchmark TEXT,
        UNIQUE(sheet_id, employee_id)
      )")
      cat("数据库迁移完成：已创建 performance_results 表\n")
    }

    # 集成模块表
    if (!"integrations" %in% tables) {
      dbExecute(con, "CREATE TABLE integrations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        base_url TEXT,
        auth_header TEXT,
        auth_value TEXT,
        method TEXT DEFAULT 'POST',
        description TEXT,
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 integrations 表\n")
    }

    # ===============================================
    # 工位图模块（seat_map）
    # ===============================================
    if (!"seat_buildings" %in% tables) {
      dbExecute(con, "CREATE TABLE seat_buildings (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        description TEXT DEFAULT '',
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime'))
      )")
      cat("数据库迁移完成：已创建 seat_buildings 表\n")
    }
    if (!"seat_floors" %in% tables) {
      dbExecute(con, "CREATE TABLE seat_floors (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        building_id INTEGER NOT NULL,
        name TEXT NOT NULL,
        floor_number INTEGER,
        description TEXT DEFAULT '',
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime')),
        FOREIGN KEY (building_id) REFERENCES seat_buildings(id)
      )")
      cat("数据库迁移完成：已创建 seat_floors 表\n")
    }
    if (!"seat_zones" %in% tables) {
      dbExecute(con, "CREATE TABLE seat_zones (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        floor_id INTEGER NOT NULL,
        name TEXT NOT NULL,
        zone_type TEXT NOT NULL DEFAULT 'open_desk',
        row_start INTEGER DEFAULT 1,
        col_start INTEGER DEFAULT 1,
        row_span INTEGER DEFAULT 1,
        col_span INTEGER DEFAULT 1,
        description TEXT DEFAULT '',
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime')),
        FOREIGN KEY (floor_id) REFERENCES seat_floors(id)
      )")
      cat("数据库迁移完成：已创建 seat_zones 表\n")
    }
    if (!"seats" %in% tables) {
      dbExecute(con, "CREATE TABLE seats (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        floor_id INTEGER NOT NULL,
        zone_id INTEGER,
        seat_code TEXT NOT NULL,
        row_num INTEGER NOT NULL,
        col_num INTEGER NOT NULL,
        status TEXT NOT NULL DEFAULT 'vacant_no_pc',
        user_id INTEGER,
        asset_id INTEGER,
        description TEXT DEFAULT '',
        created_at TEXT DEFAULT (datetime('now','localtime')),
        updated_at TEXT DEFAULT (datetime('now','localtime')),
        FOREIGN KEY (floor_id) REFERENCES seat_floors(id),
        FOREIGN KEY (zone_id) REFERENCES seat_zones(id),
        FOREIGN KEY (user_id) REFERENCES users(id),
        FOREIGN KEY (asset_id) REFERENCES assets(id)
      )")
      cat("数据库迁移完成：已创建 seats 表\n")
    }

  }, error = function(e) {
    cat("数据库迁移失败:", e$message, "\n")
  }, finally = {
    db_disconnect(con)
  })
}

check_database()
migrate_database()
