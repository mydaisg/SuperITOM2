# 工单管理模块
# 包含工单的创建、查询、派发、处理、完成等功能

##################
# 权限辅助：非admin用户返回其ID用于过滤，admin返回NULL表示不过滤
##################
wo_visible_user_id <- function(current_user) {
  if (is.null(current_user) || nrow(current_user) == 0) return(NULL)
  if (current_user$role[1] == "admin") return(NULL)
  as.integer(current_user$id[1])
}

# 获取所有工单（用于列表显示，描述字段截断）
work_order_get_all <- function(status_filter = NULL, assigned_to = NULL, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- wo_visible_user_id(current_user)
    # 先查询工单基础数据
    query <- "SELECT wo.id, wo.order_no, wo.title,
              CASE
                WHEN LENGTH(wo.description) > 50 THEN SUBSTR(wo.description, 1, 50) || '...'
                ELSE wo.description
              END as description,
              wo.priority, wo.status, wo.category, wo.subcategory,
              wo.assigned_to, wo.assigned_by, wo.request_user, wo.assigned_at,
              wo.handled_by, wo.handled_at, wo.completed_at,
              wo.created_by, wo.created_at, wo.updated_at
              FROM work_orders wo"

    conditions <- c()
    # 非admin用户：只看自己创建/指派/处理的工单
    if (!is.null(uid)) {
      conditions <- c(conditions, sprintf("(wo.created_by = %d OR wo.assigned_to = %d OR wo.handled_by = %d)", uid, uid, uid))
    }
    # 处理状态筛选："all" 或 "" 或 NULL 都表示全部
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      conditions <- c(conditions, sprintf("wo.status = '%s'", status_filter))
    }
    if (!is.null(assigned_to) && assigned_to != "") {
      conditions <- c(conditions, sprintf("wo.assigned_to = %d", as.integer(assigned_to)))
    }

    if (length(conditions) > 0) {
      query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
    }

    query <- paste(query, "ORDER BY wo.created_at DESC")

    result <- dbGetQuery(con, query)

    # 单独查询用户信息并合并（优先使用显示名称）
    if (nrow(result) > 0) {
      users <- dbGetQuery(con, "SELECT id, username, display_name FROM users")
      udisplay <- function(uid, default = "未知") {
        if (is.na(uid)) return(NA_character_)
        r <- users[users$id == uid, ]
        if (nrow(r) == 0) return(default)
        dn <- r$display_name[1]
        if (!is.na(dn) && dn != "") dn else r$username[1]
      }

      result$creator_name <- sapply(result$created_by, udisplay)

      # 添加指派给名称
      result$assignee_name <- sapply(result$assigned_to, udisplay)

      # 添加指派人名称
      result$assigner_name <- sapply(result$assigned_by, udisplay)

      # 添加处理人名称
      result$handler_name <- sapply(result$handled_by, udisplay)

      # 添加请求用户名称（request_user 是用户名文本）
      result$request_user_name <- sapply(result$request_user, function(x) {
        if (is.na(x) || x == "") return(NA_character_)
        x
      })

      # 添加当前处理人名称
      result$current_handler <- sapply(result$assigned_to, function(x) {
        if (is.na(x)) return("未分配")
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else "未分配"
      })
    }

    return(result)
  }, error = function(e) {
    warning(paste("获取工单失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 按ID获取工单详情
work_order_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("SELECT * FROM work_orders WHERE id = %d", id)
    result <- dbGetQuery(con, query)

    # 单独查询用户信息并合并
    if (nrow(result) > 0) {
      users <- dbGetQuery(con, "SELECT id, username FROM users")

      # 添加创建人名称
      result$creator_name <- sapply(result$created_by, function(x) {
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else "未知"
      })

      # 添加指派给名称
      result$assignee_name <- sapply(result$assigned_to, function(x) {
        if (is.na(x)) return(NA)
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else NA
      })

      # 添加指派人名称
      result$assigner_name <- sapply(result$assigned_by, function(x) {
        if (is.na(x)) return(NA)
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else NA
      })

      # 添加处理人名称
      result$handler_name <- sapply(result$handled_by, function(x) {
        if (is.na(x)) return(NA)
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else NA
      })

      # 添加请求用户名称（request_user 是用户名文本）
      result$request_user_name <- sapply(result$request_user, function(x) {
        if (is.na(x) || x == "") return(NA)
        x
      })
    }

    return(result)
  }, error = function(e) {
    warning(paste("获取工单详情失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 生成工单号：ITS + YYYYMMDD + 3位流水号（防并发重复）
work_order_generate_number <- function() {
  con <- db_connect()
  tryCatch({
    # 获取今天的日期字符串
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("ITS", today)
    
    # 最大重试次数，防止无限循环
    max_retry <- 10
    for (retry in 1:max_retry) {
      # 使用 IMMEDIATE 事务获取写锁，防止并发
      query <- "BEGIN IMMEDIATE"
      dbExecute(con, query)
      
      tryCatch({
        # 查询今天已有的最大流水号（只匹配正确格式：ITSYYYYMMDD + 3位数字）
        # GLOB 模式匹配：prefix + 3个数字结尾
        glob_pattern <- paste0(prefix, "???")
        query <- sprintf("SELECT order_no FROM work_orders WHERE order_no GLOB '%s' ORDER BY order_no DESC LIMIT 1", glob_pattern)
        result <- dbGetQuery(con, query)
        
        if (nrow(result) > 0 && !is.na(result$order_no[1])) {
          # 提取流水号并+1（取最后3位）
          last_seq_str <- substr(result$order_no[1], nchar(prefix) + 1, nchar(prefix) + 3)
          last_seq <- as.integer(last_seq_str)
          new_seq <- last_seq + 1
        } else {
          # 今天第一个工单
          new_seq <- 1
        }
        
        # 生成新的工单号
        order_no <- sprintf("%s%03d", prefix, new_seq)
        
        # 检查工单号是否已存在（并发场景下的二次确认）
        check_query <- sprintf("SELECT COUNT(*) as cnt FROM work_orders WHERE order_no = '%s'", order_no)
        check_result <- dbGetQuery(con, check_query)
        
        if (check_result$cnt[1] == 0) {
          # 工单号不重复，提交事务并返回
          dbExecute(con, "COMMIT")
          return(order_no)
        }
        
        # 工单号已存在，回滚并重试
        dbExecute(con, "ROLLBACK")
        
      }, error = function(e2) {
        dbExecute(con, "ROLLBACK")
        stop(e2)
      })
    }
    
    # 达到最大重试次数，使用备用方案
    warning("工单号重试达到上限，使用备用方案")
    # 备用方案：使用时间戳生成3位伪随机流水号
    backup_seq <- (as.integer(Sys.time()) %% 1000) + 1
    return(sprintf("%s%03d", prefix, backup_seq))
  }, error = function(e) {
    warning(paste("生成工单号失败:", e$message))
    # 备用方案：使用时间戳生成3位伪随机流水号
    backup_seq <- (as.integer(Sys.time()) %% 1000) + 1
    return(sprintf("%s%03d", prefix, backup_seq))
  }, finally = {
    db_disconnect(con)
  })
}

# 创建工单
work_order_add <- function(title, description, priority = "中", category = "一般",
                           subcategory = "", request_user = NULL, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])

    # 生成工单号
    order_no <- work_order_generate_number()

    # 处理 request_user（请求用户）
    req_user_part <- ifelse(is.null(request_user) || request_user == "", "NULL",
                            sprintf("'%s'", gsub("'", "''", request_user)))

    query <- sprintf("INSERT INTO work_orders
                     (order_no, title, description, priority, status, category, subcategory, request_user, created_by, created_at)
                     VALUES ('%s', '%s', '%s', '%s', 'pending', '%s', '%s', %s, %d, CURRENT_TIMESTAMP)",
                     order_no, title, description, priority, category, subcategory, req_user_part, user_id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建工单", title, operator_name, 
                      sprintf("工单号: %s, 优先级: %s, 分类: %s", order_no, priority, category))
    
    return(list(success = TRUE, message = "工单创建成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("创建失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 派发工单
work_order_assign <- function(id, assigned_to, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    assigned_to <- as.integer(assigned_to)
    assigner_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    query <- sprintf("UPDATE work_orders 
                     SET assigned_to = %d, assigned_by = %d, assigned_at = CURRENT_TIMESTAMP,
                         status = 'assigned', updated_at = CURRENT_TIMESTAMP 
                     WHERE id = %d", 
                     assigned_to, assigner_id, id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("派发工单", sprintf("工单ID: %d", id), operator_name, 
                      sprintf("派发给用户ID: %d", assigned_to))
    
    return(list(success = TRUE, message = "工单派发成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("派发失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 开始处理工单
work_order_start_handle <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    handler_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    query <- sprintf("UPDATE work_orders 
                     SET handled_by = %d, handled_at = CURRENT_TIMESTAMP,
                         status = 'processing', updated_at = CURRENT_TIMESTAMP 
                     WHERE id = %d", 
                     handler_id, id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("开始处理工单", sprintf("工单ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "开始处理工单"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("操作失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 完成工单
work_order_complete <- function(id, resolution, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    handler_id <- ifelse(is.null(current_user), 1, current_user$id[1])

    query <- sprintf("UPDATE work_orders
                     SET status = 'completed', resolution = '%s',
                         completed_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP
                     WHERE id = %d",
                     resolution, id)
    dbExecute(con, query)

    # 同步更新关联的巡检异常状态为"已解决"
    dbExecute(con, sprintf(
      "UPDATE inspection_issues SET status = 'resolved', updated_at = CURRENT_TIMESTAMP 
       WHERE related_work_order_id = %d AND status IN ('pending', 'processing')",
      id))

    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("完成工单", sprintf("工单ID: %d", id), operator_name)

    return(list(success = TRUE, message = "工单已完成"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("操作失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 关闭工单（与完成不同，关闭可以是任何状态的终止）
work_order_close <- function(id, close_reason = "", current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)

    # 如果有关闭原因，追加到解决方案中
    if (close_reason != "") {
      resolution_text <- sprintf("[已关闭] %s", close_reason)
      query <- sprintf("UPDATE work_orders
                       SET status = 'closed', resolution = '%s',
                           completed_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP
                       WHERE id = %d",
                       resolution_text, id)
    } else {
      query <- sprintf("UPDATE work_orders
                       SET status = 'closed', updated_at = CURRENT_TIMESTAMP
                       WHERE id = %d", id)
    }
    dbExecute(con, query)

    # 同步更新关联的巡检异常状态为"已关闭"（如果关闭原因是"已处理和交付"则设为已解决）
    if (close_reason == "已处理和交付") {
      dbExecute(con, sprintf(
        "UPDATE inspection_issues SET status = 'resolved', updated_at = CURRENT_TIMESTAMP 
         WHERE related_work_order_id = %d AND status IN ('pending', 'processing')",
        id))
    } else {
      dbExecute(con, sprintf(
        "UPDATE inspection_issues SET status = 'closed', updated_at = CURRENT_TIMESTAMP 
         WHERE related_work_order_id = %d AND status IN ('pending', 'processing')",
        id))
    }

    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("关闭工单", sprintf("工单ID: %d", id), operator_name,
                      ifelse(close_reason != "", close_reason, ""))

    return(list(success = TRUE, message = "工单已关闭"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("操作失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 更新工单状态
work_order_update_status <- function(id, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    
    query <- sprintf("UPDATE work_orders SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     status, id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新工单状态", sprintf("工单ID: %d", id), operator_name, 
                      sprintf("新状态: %s", status))
    
    return(list(success = TRUE, message = "工单状态更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 删除工单
work_order_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("DELETE FROM work_orders WHERE id = %d", id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除工单", sprintf("工单ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "工单删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取可派发的用户列表（所有活跃用户）
work_order_get_assignable_users <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT id, username, display_name, role FROM users 
              WHERE active = 1 
              ORDER BY role, username"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    warning(paste("获取可派发用户失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# ★ 批量操作：删除工单
work_order_batch_delete <- function(ids, current_user = NULL) {
  if (length(ids) == 0) return(list(success = FALSE, message = "未选中工单"))
  con <- db_connect()
  tryCatch({
    ids_str <- paste(as.integer(ids), collapse = ",")
    dbExecute(con, sprintf("DELETE FROM work_orders WHERE id IN (%s)", ids_str))
    dbExecute(con, sprintf("DELETE FROM work_order_comments WHERE work_order_id IN (%s)", ids_str))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("批量删除工单", paste0("工单IDs: ", ids_str, " (", length(ids), "条)"), operator_name)
    list(success = TRUE, message = sprintf("已删除 %d 条工单", length(ids)))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# ★ 批量操作：激活工单（状态改为 pending 待处理）
work_order_batch_reopen <- function(ids, current_user = NULL) {
  if (length(ids) == 0) return(list(success = FALSE, message = "未选中工单"))
  con <- db_connect()
  tryCatch({
    ids_str <- paste(as.integer(ids), collapse = ",")
    dbExecute(con, sprintf(
      "UPDATE work_orders SET status='pending', completed_at=NULL, updated_at=datetime('now','localtime') WHERE id IN (%s)", ids_str))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("批量激活工单", paste0("工单IDs: ", ids_str, " (", length(ids), "条)"), operator_name)
    list(success = TRUE, message = sprintf("已激活 %d 条工单", length(ids)))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# ★ 批量操作：关闭工单（状态改为 closed）
work_order_batch_close <- function(ids, current_user = NULL) {
  if (length(ids) == 0) return(list(success = FALSE, message = "未选中工单"))
  con <- db_connect()
  tryCatch({
    ids_str <- paste(as.integer(ids), collapse = ",")
    dbExecute(con, sprintf(
      "UPDATE work_orders SET status='closed', completed_at=datetime('now','localtime'), updated_at=datetime('now','localtime') WHERE id IN (%s)", ids_str))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("批量关闭工单", paste0("工单IDs: ", ids_str, " (", length(ids), "条)"), operator_name)
    list(success = TRUE, message = sprintf("已关闭 %d 条工单", length(ids)))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 获取工单统计信息
work_order_get_stats <- function(current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- wo_visible_user_id(current_user)
    user_filter <- if (is.null(uid)) "" else sprintf("WHERE created_by = %d OR assigned_to = %d OR handled_by = %d", uid, uid, uid)
    query <- sprintf("SELECT
                COUNT(*) as total,
                SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) as pending,
                SUM(CASE WHEN status = 'assigned' THEN 1 ELSE 0 END) as assigned,
                SUM(CASE WHEN status = 'processing' THEN 1 ELSE 0 END) as processing,
                SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as completed,
                SUM(CASE WHEN status = 'closed' THEN 1 ELSE 0 END) as closed
              FROM work_orders %s", user_filter)
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    warning(paste("获取工单统计失败:", e$message))
    return(data.frame(total = 0, pending = 0, assigned = 0, processing = 0, completed = 0, closed = 0))
  }, finally = {
    db_disconnect(con)
  })
}

# 编辑工单（修改所有信息）
work_order_edit <- function(id, order_no, title, description, priority, category, status, assigned_to, request_user = NULL, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)

    # 处理assigned_to字段
    if (is.null(assigned_to) || assigned_to == "" || assigned_to == "unassigned") {
      assigned_part <- "assigned_to = NULL"
    } else {
      assigned_part <- sprintf("assigned_to = %d", as.integer(assigned_to))
    }

    # 处理 request_user 字段
    if (is.null(request_user) || request_user == "") {
      req_user_part <- "request_user = NULL"
    } else {
      req_user_part <- sprintf("request_user = '%s'", gsub("'", "''", request_user))
    }

    query <- sprintf("UPDATE work_orders
                     SET order_no = '%s', title = '%s', description = '%s', priority = '%s',
                         category = '%s', status = '%s', %s, %s, updated_at = CURRENT_TIMESTAMP
                     WHERE id = %d",
                     order_no, title, description, priority, category, status, assigned_part, req_user_part, id)
    dbExecute(con, query)

    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("编辑工单", sprintf("工单ID: %d", id), operator_name)

    return(list(success = TRUE, message = "工单编辑成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("编辑失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 添加工单评论
work_order_add_comment <- function(work_order_id, comment, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    work_order_id <- as.integer(work_order_id)
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])

    query <- sprintf("INSERT INTO work_order_comments (work_order_id, comment, created_by)
                     VALUES (%d, '%s', %d)",
                     work_order_id, comment, user_id)
    dbExecute(con, query)
    Sys.sleep(0.01)
    update_sql <- paste0("UPDATE work_order_comments SET created_at = datetime('now', 'localtime') WHERE id = (SELECT MAX(id) FROM work_order_comments WHERE work_order_id = ", work_order_id, ")")
    dbExecute(con, update_sql)

    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("添加工单评论", sprintf("工单ID: %d", work_order_id), operator_name)

    return(list(success = TRUE, message = "评论添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加评论失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取工单评论列表
work_order_get_comments <- function(work_order_id) {
  con <- db_connect()
  tryCatch({
    work_order_id <- as.integer(work_order_id)
    query <- sprintf("SELECT c.id, c.comment, c.created_at, u.username as creator_name
                      FROM work_order_comments c
                      LEFT JOIN users u ON c.created_by = u.id
                      WHERE c.work_order_id = %d
                      ORDER BY c.created_at DESC", work_order_id)
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    warning(paste("获取工单评论失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 解析快速工单格式文本
# 格式：
# IT服务请求 20260512 1110：
# 用户：谢芳材-供应链中心-副总经理
# 内容：两栋楼的"监控角度需要修正"...
# @韩荣昌-IT部-IT工程师(Sky)
#
# 返回：list(success, category, request_user, description, assignee_name, message)
work_order_parse_quick_text <- function(text) {
  if (is.null(text) || trimws(text) == "") {
    return(list(success = FALSE, message = "请输入工单内容"))
  }

  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]  # 移除空行

  if (length(lines) < 3) {
    return(list(success = FALSE, message = "格式不正确，至少需要：类别、用户、内容"))
  }

  tryCatch({
    # 第一行：类别 日期 时间
    first_line <- lines[1]
    # 使用正则提取：类别（可能有空格）、日期（8位）、时间（4位）
    # 例如："IT服务请求 20260512 1110：" 或 "IT服务请求 20260512 1110"
    first_line <- gsub("：$", "", first_line)  # 移除末尾冒号
    parts <- strsplit(trimws(first_line), "\\s+")[[1]]

    if (length(parts) < 3) {
      return(list(success = FALSE, message = "第一行格式错误，应为：类别 日期 时间（如：IT服务请求 20260512 1110）"))
    }

    # 类别是第一个到倒数第三个部分（日期和时间之间可能有空格）
    date_part <- parts[length(parts) - 1]
    time_part <- parts[length(parts)]

    # 验证日期格式（8位数字）
    if (!grepl("^\\d{8}$", date_part)) {
      return(list(success = FALSE, message = "日期格式错误，应为8位数字（如：20260512）"))
    }

    # 验证时间格式（4位数字）
    if (!grepl("^\\d{4}$", time_part)) {
      return(list(success = FALSE, message = "时间格式错误，应为4位数字（如：1110）"))
    }

    # 类别是从开始到日期之前的所有内容
    category <- paste(parts[-c(length(parts)-1, length(parts))], collapse = " ")

    # 第二行：用户
    second_line <- lines[2]
    if (!grepl("^用户[：:]", second_line)) {
      return(list(success = FALSE, message = "第二行应以'用户：'开头"))
    }
    request_user <- trimws(gsub("^用户[：:]", "", second_line))

    # 最后一行：@人员（分派人）
    last_line <- trimws(lines[length(lines)])
    assignee_name <- NULL
    assignee_full <- NULL  # 保存完整信息用于显示
    if (grepl("^@", last_line)) {
      assignee_full <- trimws(gsub("^@", "", last_line))
    } else {
      # 如果最后一行不是@开头，可能是内容的一部分
      # 查找包含@的行
      at_lines <- lines[grepl("@", lines)]
      if (length(at_lines) > 0) {
        for (l in rev(at_lines)) {
          if (grepl("^@", trimws(l))) {
            assignee_full <- trimws(gsub("^@", "", l))
            break
          }
        }
      }
    }
    
    # 从完整信息中提取姓名（取第一个"-"之前的部分）
    if (!is.null(assignee_full) && assignee_full != "") {
      # 韩荣昌-IT部-IT工程师(Sky) -> 韩荣昌
      if (grepl("-", assignee_full)) {
        assignee_name <- strsplit(assignee_full, "-")[[1]][1]
      } else {
        assignee_name <- assignee_full
      }
    }

    # 内容：从"内容："之后到最后@行之前的所有内容
    content_lines <- c()
    for (i in 3:length(lines)) {
      line <- lines[i]
      # 如果这行是@开头，跳过（内容已结束）
      if (grepl("^@", trimws(line))) {
        break
      }
      # 如果这行以"内容："开头，提取冒号后的内容
      if (grepl("^内容[：:]", line)) {
        content_lines <- c(content_lines, trimws(gsub("^内容[：:]", "", line)))
      } else {
        # 直接内容行
        content_lines <- c(content_lines, line)
      }
    }

    description <- paste(content_lines, collapse = "\n")

    if (description == "") {
      return(list(success = FALSE, message = "未找到工单内容"))
    }

    return(list(
      success = TRUE,
      category = category,
      request_user = request_user,
      description = description,
      assignee_name = assignee_name,
      assignee_full = assignee_full,
      message = "解析成功"
    ))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("解析失败:", e$message)))
  })
}

# 根据用户名查找用户ID（支持模糊匹配）
work_order_find_user_by_name <- function(name) {
  if (is.null(name) || name == "") {
    return(NULL)
  }

  con <- db_connect()
  tryCatch({
    # 尝试精确匹配
    query <- sprintf("SELECT id FROM users WHERE username = '%s' OR username LIKE '%%%s%%'", name, name)
    result <- dbGetQuery(con, query)

    if (nrow(result) > 0) {
      return(result$id[1])
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  }, finally = {
    db_disconnect(con)
  })
}

##################
# ★ 批量补工单：日报文本解析
#
# 输入示例：
#   韩荣昌 2026年6月23日 日报
#   
#   1. IT支持与故障处理
#   ● 处理蔡金萍反馈2号楼4楼打印报错问题...
#   ● 处理吕嘉俊电脑表格复制变空白的问题...
#   2. 权限管理与安全
#   ● 处理赖庆耀新更换的手机临时授权一天...
#
# 解析规则（详尽注释，便于后续调整）：
#   [规则1] 第一行：提取"姓名 日期 日报"格式
#           姓名 = 第一行中日期之前的中文部分（去尾空格）
#           日期 = 第一行中的"YYYY年M月D日"格式，转为 SQLite 日期字符串
#   [规则2] 跳过空行
#   [规则3] 跳过"数字. 分类标题"行（如"1. IT支持与故障处理"）
#   [规则4] ● 开头的行 = 一条工单记录
#   [规则5] ★ 人名提取（从每条●行中取第一个中文人名）：
#           匹配关键词后的2-3个连续中文字符
#           关键词列表：处理|新增|指导|为|协助|帮助|告诉|通知|给|帮|安排|协调|配合|受理|回复|跟进
#           若未匹配到，回退：取●行中第1段2-3汉字（排除常见非人名词）
#           仍未匹配到则使用处理人姓名作为请求用户
#   [规则6] 标题 = ●行完整内容（去除前导●和空格）
#   [规则7] 状态 = "closed"（已关闭）
#   [规则8] 分类 = "批量工单"
#   [规则9] 时间 = 日期 + 18:00:00（T18:00:00）
#   [规则10] 处理人 = 第一行提取的姓名（通过users表查找user_id）
##################
work_order_batch_parse <- function(text) {
  # ── 分割文本为行 ──
  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0) return(list(success = FALSE, message = "文本为空"))

  # ── 规则1：提取第一行的处理人和日期 ──
  first_line <- lines[1]
  date_pattern <- "\\d{4}年\\d{1,2}月\\d{1,2}日"
  date_match <- regmatches(first_line, regexpr(date_pattern, first_line, perl = TRUE))
  if (length(date_match) == 0) return(list(success = FALSE, message = "第一行未找到日期，格式应为：姓名 YYYY年M月D日 日报"))
  date_str <- date_match[1]
  # 日期前面的部分就是处理人姓名
  handler_name <- trimws(sub(paste0(date_str, ".*"), "", first_line))
  if (nchar(handler_name) == 0) handler_name <- "未知"
  # 将中文日期转为 SQLite 格式 "YYYY-MM-DD"
  date_parts <- regmatches(date_str, gregexpr("\\d+", date_str, perl = TRUE))[[1]]
  if (length(date_parts) < 3) return(list(success = FALSE, message = "日期格式解析失败"))
  formatted_date <- sprintf("%s-%02d-%02d", date_parts[1], as.integer(date_parts[2]), as.integer(date_parts[3]))
  # 统一时间为 18:00 → 规则9
  batch_time <- paste0(formatted_date, " 18:00:00")

  # ── 规则3+4：提取●开头的工单行，跳过分类标题行 ──
  chinese_char <- "[\u4e00-\u9fff]"
  # ★ 规则5：人名关键词列表（可扩展）
  #   "处理"匹配"处理张三xxx" → 张三
  #   "为"匹配"为李四xxx" → 李四（但需小心"为泛微实施人员叶明"这种嵌套情况）
  #   "给/帮"匹配"给王五xxx" → 王五
  # ★ 关键词顺序很重要："人员/用户/同事"必须在"为"前面，否则"为泛微实施人员叶明"
  #   会匹配"为"→"泛微实"而非"人员"→"叶明"
  name_keywords <- c("处理", "新增", "指导", "协助", "帮助", "告诉", "通知",
    "给", "帮", "安排", "协调", "配合", "受理", "回复", "跟进",
    "人员", "用户", "同事",  # 优先于"为"
    "为")
  keyword_pattern <- paste(name_keywords, collapse = "|")
  # 排除词：容易被误判为人名的常见词（可扩展）
  exclude_names <- c("反馈", "电脑", "打印", "手机", "显示", "监控", "会议", "办公",
    "设备", "系统", "软件", "硬件", "网络", "服务器", "数据", "文件", "流程",
    "财务部", "财务", "人事部", "行政部", "保安室", "管理处", "生产线", "研发部", "采购部",
    "泛微", "直流桩", "直流", "会议室", "打印机", "碳粉", "门禁", "显示器", "安装部署",
    "问题", "一体机", "一体", "显示屏", "部署", "需求", "项目", "任务", "报告",
    "实施", "运维", "测试", "开发", "设计", "配置", "调整", "记录",
    "生产", "安装", "交付", "恢复", "开放", "更换", "新增", "修改",
    "智能", "平板", "键盘", "鼠标", "网线", "电源", "插座", "线路",
    "电梯", "厨房", "显示")

  orders <- list()
  order_lines <- lines[grepl("^●", lines)]
  if (length(order_lines) == 0) return(list(success = FALSE, message = "未找到●开头的工单行"))

  for (i in seq_along(order_lines)) {
    full_text <- trimws(sub("^●\\s*", "", order_lines[i]))
    if (nchar(full_text) < 5) next  # 太短跳过

    # ★ 规则5：提取第一个中文人名
    #  策略A：逐个关键词匹配（优先2字名，再尝试3字名）
    #         若匹配到关键词但名字被排除 → 继续试下一个关键词
    #  策略B：无关键词时才模糊搜索 → 策略C：处理人回退
    request_user <- ""
    found_any_keyword <- FALSE

    try_extract_name <- function(txt, kw, nchars) {
      pat <- paste0(kw, "(", chinese_char, "{", nchars, "})")
      m <- regexec(pat, txt, perl = TRUE)
      caps <- regmatches(txt, m)[[1]]
      if (length(caps) >= 2 && nchar(caps[2]) >= 2 && !(caps[2] %in% exclude_names)) {
        return(caps[2])
      }
      return("")
    }

    for (kw in name_keywords) {
      # 优先试2字名（中文名常见2字），再试3字
      nm <- try_extract_name(full_text, kw, 2)
      if (nm != "") { request_user <- nm; break }
      nm <- try_extract_name(full_text, kw, 3)
      if (nm != "") { request_user <- nm; break }
      # 检查是否至少匹配到了关键词（名字被排除的情况）
      m <- regexec(paste0(kw, "(", chinese_char, "{2,3})"), full_text, perl = TRUE)
      if (length(regmatches(full_text, m)[[1]]) >= 2) found_any_keyword <- TRUE
    }

    # 策略B：仅当策略A完全没有匹配到任何关键词时，才模糊搜索2字人名
    # 　　   只用{2}不用{3}，避免跨词边界产生"安装部"这类虚假词组
    if (request_user == "" && !found_any_keyword) {
      stripped <- sub(paste0("^(", keyword_pattern, ")\\s*"), "", full_text)
      all_names <- regmatches(stripped, gregexpr(paste0(chinese_char, "{2}"), stripped, perl = TRUE))[[1]]
      for (n in all_names) {
        if (!(n %in% exclude_names)) {
          request_user <- n
          break
        }
      }
    }

    # 策略C：最终回退——用处理人姓名
    if (request_user == "") request_user <- handler_name

    # ── 构建工单数据 ──
    orders[[length(orders) + 1]] <- list(
      title       = full_text,
      description = full_text,
      category    = "批量工单",
      priority    = "中",
      status      = "closed",
      request_user = request_user,
      handler_name = handler_name,
      batch_time  = batch_time
    )
  }

  list(
    success = TRUE,
    handler_name = handler_name,
    batch_date  = formatted_date,
    batch_time  = batch_time,
    count       = length(orders),
    orders      = orders
  )
}

# ★ 批量创建工单（直接插入已关闭工单）
work_order_batch_create <- function(parsed, current_user = NULL) {
  if (!parsed$success) return(parsed)
  orders <- parsed$orders
  if (length(orders) == 0) return(list(success = FALSE, message = "无工单可创建"))

  # 查找处理人对应的 user_id
  handler_id <- work_order_find_user_by_name(parsed$handler_name)
  if (is.null(handler_id)) {
    con <- db_connect()
    handler_row <- tryCatch({
      dbGetQuery(con, sprintf("SELECT id FROM users WHERE display_name = '%s' AND active = 1 LIMIT 1",
        gsub("'", "''", parsed$handler_name)))
    }, error = function(e) data.frame(), finally = db_disconnect(con))
    if (nrow(handler_row) > 0) handler_id <- handler_row$id[1]
  }
  if (is.null(handler_id) && !is.null(current_user)) {
    handler_id <- current_user$id[1]
  }

  created_count <- 0
  errors <- c()

  for (i in seq_along(orders)) {
    o <- orders[[i]]
    con <- db_connect()
    tryCatch({
      today_str <- format(Sys.Date(), "%Y%m%d")
      seq_query <- sprintf("SELECT COUNT(*) as cnt FROM work_orders WHERE order_no LIKE 'ITS%s%%'", today_str)
      seq_result <- dbGetQuery(con, seq_query)
      seq_num <- seq_result$cnt[1] + 1
      order_no <- sprintf("ITS%s%03d", today_str, seq_num)

      uid_sql <- if (is.null(handler_id)) "NULL" else as.character(handler_id)
      title_safe <- gsub("'", "''", o$title)
      desc_safe <- gsub("'", "''", o$description)
      req_user_safe <- gsub("'", "''", o$request_user)

      dbExecute(con, sprintf(
        "INSERT INTO work_orders (order_no, title, description, category, priority, status,
         request_user, assigned_to, handled_by, created_by,
         created_at, updated_at, assigned_at, handled_at, completed_at)
         VALUES ('%s', '%s', '%s', '批量工单', '中', 'closed',
                 '%s', %s, %s, %s,
                 '%s', '%s', '%s', '%s', '%s')",
        order_no, title_safe, desc_safe,
        req_user_safe, uid_sql, uid_sql, uid_sql,
        parsed$batch_time, parsed$batch_time, parsed$batch_time, parsed$batch_time, parsed$batch_time
      ))
      created_count <- created_count + 1
    }, error = function(e) {
      errors <<- c(errors, paste0("第", i, "条:", e$message))
    }, finally = {
      db_disconnect(con)
    })
  }

  message <- sprintf("批量创建完成：成功 %d 条", created_count)
  if (length(errors) > 0) {
    message <- paste0(message, "，失败 ", length(errors), " 条：",
      paste(errors[1:min(3, length(errors))], collapse = "; "))
  }

  if (!is.null(current_user) && nrow(current_user) > 0) {
    log_user_operation("批量补工单",
      paste0(parsed$handler_name, " ", parsed$batch_date, "日报：", created_count, "条"),
      current_user$username[1] %||% "系统")
  }

  list(success = created_count > 0, message = message, count = created_count)
}
