# 工单管理模块
# 包含工单的创建、查询、派发、处理、完成等功能

# 获取所有工单（用于列表显示，描述字段截断）
work_order_get_all <- function(status_filter = NULL, assigned_to = NULL) {
  con <- db_connect()
  tryCatch({
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
        if (is.na(x)) return(NA_character_)
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else NA_character_
      })

      # 添加指派人名称
      result$assigner_name <- sapply(result$assigned_by, function(x) {
        if (is.na(x)) return(NA_character_)
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else NA_character_
      })

      # 添加处理人名称
      result$handler_name <- sapply(result$handled_by, function(x) {
        if (is.na(x)) return(NA_character_)
        name <- users$username[users$id == x]
        if (length(name) > 0) name[1] else NA_character_
      })

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

# 获取可派发的用户列表（IT服务台、IT服务工程师、IT系统工程师）
work_order_get_assignable_users <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT id, username, role FROM users 
              WHERE role IN ('it_desk', 'it_engineer', 'sys_engineer', 'admin') 
              AND active = 1 
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

# 获取工单统计信息
work_order_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT
                COUNT(*) as total,
                SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) as pending,
                SUM(CASE WHEN status = 'assigned' THEN 1 ELSE 0 END) as assigned,
                SUM(CASE WHEN status = 'processing' THEN 1 ELSE 0 END) as processing,
                SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as completed,
                SUM(CASE WHEN status = 'closed' THEN 1 ELSE 0 END) as closed
              FROM work_orders"
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
