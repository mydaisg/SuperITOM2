# ============================================================
# 巡检管理模块 - 数据层
# 流程：计划 -> 任务 -> 执行 -> 记录/异常 -> 整改工单
# ============================================================

# ----------------------------------------------------------
# 0. 辅助函数
# ----------------------------------------------------------

# 自动生成计划名称：从检查项提取关键字
generate_plan_name_from_items <- function(inspection_category, item_names) {
  # 获取当前月份
  month_str <- format(Sys.Date(), "%Y年%m月")
  
  # 获取周期类型中文名
  cycle_cn <- switch(inspection_category,
                     "数据中心巡检" = "数据中心",
                     "电力机房巡检" = "电力机房",
                     "会议室巡检" = "会议室",
                     "设备间巡检" = "设备间",
                     inspection_category)
  
  # 从检查项名称中提取关键词（取前3个，截取前6个字符）
  if (length(item_names) > 0) {
    keywords <- sapply(head(item_names, 3), function(x) {
      # 去除常见前缀，保留核心词
      x <- gsub("^(巡检|检查|测试|查看|记录|观察|测量|记录|确认|验证|检查|检测|测量|清洁|整理|记录)", "", x)
      substr(x, 1, 6)
    })
    keyword_str <- paste(keywords, collapse = "、")
    plan_name <- sprintf("%s%s巡检（%s...）", month_str, cycle_cn, keyword_str)
  } else {
    plan_name <- sprintf("%s%s巡检", month_str, cycle_cn)
  }
  
  # 限制名称长度
  if (nchar(plan_name) > 50) {
    plan_name <- substr(plan_name, 1, 47)
    plan_name <- paste0(plan_name, "...")
  }
  
  return(plan_name)
}

# ----------------------------------------------------------
# 1. 巡检计划 (inspection_plans)
# ----------------------------------------------------------

# 获取所有巡检计划
inspection_plan_get_all <- function(status_filter = NULL) {
  con <- db_connect()
  tryCatch({
    query <- "SELECT ip.*, u1.username as creator_name, u2.username as responsible_name
              FROM inspection_plans ip
              LEFT JOIN users u1 ON ip.created_by = u1.id
              LEFT JOIN users u2 ON ip.responsible_user = u2.id
              WHERE ip.is_deleted = 0"
    
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- sprintf("%s AND ip.status = '%s'", query, status_filter)
    }
    query <- paste(query, "ORDER BY ip.created_at DESC")
    
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    warning(paste("获取巡检计划失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 按ID获取巡检计划详情
inspection_plan_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("SELECT ip.*, u1.username as creator_name, u2.username as responsible_name
                     FROM inspection_plans ip
                     LEFT JOIN users u1 ON ip.created_by = u1.id
                     LEFT JOIN users u2 ON ip.responsible_user = u2.id
                     WHERE ip.id = %d", id)
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    warning(paste("获取巡检计划详情失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 生成巡检计划编号：INS-PLAN-YYYYMMDD-XXX
inspection_plan_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("INS-PLAN-", today, "-")
    
    query <- sprintf("SELECT plan_no FROM inspection_plans 
                      WHERE plan_no LIKE '%s%%' 
                      ORDER BY plan_no DESC LIMIT 1", prefix)
    result <- dbGetQuery(con, query)
    
    if (nrow(result) > 0) {
      last_seq <- as.integer(sub(".*-", "", result$plan_no[1]))
      new_seq <- last_seq + 1
    } else {
      new_seq <- 1
    }
    
    return(sprintf("%s%03d", prefix, new_seq))
  }, error = function(e) {
    return(sprintf("INS-PLAN-%s-001", format(Sys.Date(), "%Y%m%d")))
  }, finally = {
    db_disconnect(con)
  })
}

# 创建巡检计划
inspection_plan_add <- function(name, description, category, inspection_category, cycle_type, cycle_value,
                                 start_date, end_date, responsible_user, check_items, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    plan_no <- inspection_plan_generate_no()
    
    # 处理负责人
    resp_part <- ifelse(is.null(responsible_user) || responsible_user == "", 
                        "NULL", sprintf("%d", as.integer(responsible_user)))
    
    query <- sprintf(
      "INSERT INTO inspection_plans 
       (plan_no, name, description, category, inspection_category, cycle_type, cycle_value, start_date, end_date, responsible_user, created_by, created_at)
       VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', %s, %d, CURRENT_TIMESTAMP)",
      plan_no, name, description, category, inspection_category, cycle_type, cycle_value, start_date, end_date, resp_part, user_id
    )
    dbExecute(con, query)
    
    # 获取新插入的计划ID
    plan_id_query <- "SELECT last_insert_rowid() as id"
    plan_id <- dbGetQuery(con, plan_id_query)$id[1]
    
    # 插入检查项
    if (!is.null(check_items) && length(check_items) > 0) {
      for (i in seq_along(check_items)) {
        item <- check_items[[i]]
        if (!is.null(item$name) && item$name != "") {
          item_category <- ifelse(is.null(item$category), inspection_category, item$category)
          item_query <- sprintf(
            "INSERT INTO inspection_items (plan_id, category, item_name, item_description, check_standard, scoring_type, max_score, sort_order)
             VALUES (%d, '%s', '%s', '%s', '%s', '%s', %d, %d)",
            plan_id,
            gsub("'", "''", item_category),
            gsub("'", "''", item$name),
            gsub("'", "''", ifelse(is.null(item$description), "", item$description)),
            gsub("'", "''", ifelse(is.null(item$standard), "", item$standard)),
            ifelse(is.null(item$scoring_type), "pass_fail", item$scoring_type),
            ifelse(is.null(item$max_score), 100, as.integer(item$max_score)),
            as.integer(i)
          )
          dbExecute(con, item_query)
        }
      }
    }
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建巡检计划", name, operator_name,
                      sprintf("计划号: %s, 分类: %s, 周期: %s", plan_no, category, cycle_type))
    
    return(list(success = TRUE, message = "巡检计划创建成功", plan_id = plan_id))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("创建失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 更新巡检计划
inspection_plan_update <- function(id, name, description, category, inspection_category,
                                    cycle_type, start_date, end_date, responsible_user, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    
    resp_part <- ifelse(is.null(responsible_user) || responsible_user == "", 
                       "responsible_user = NULL", sprintf("responsible_user = %d", as.integer(responsible_user)))
    
    query <- sprintf(
      "UPDATE inspection_plans 
       SET name = '%s', description = '%s', category = '%s', inspection_category = '%s',
           cycle_type = '%s', cycle_value = '', 
           start_date = '%s', end_date = '%s', %s,
           status = '%s', updated_at = CURRENT_TIMESTAMP
       WHERE id = %d",
      gsub("'", "''", name), gsub("'", "''", description), 
      gsub("'", "''", category), gsub("'", "''", inspection_category),
      gsub("'", "''", cycle_type), 
      start_date, end_date, resp_part,
      status, id
    )
    dbExecute(con, query)
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新巡检计划", sprintf("ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "巡检计划更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 删除巡检计划（软删除，级联删除关联的任务和记录）
inspection_plan_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    
    # 软删除关联的记录（任务和记录）
    dbExecute(con, sprintf("UPDATE inspection_records SET is_deleted = 1 
                          WHERE task_id IN (SELECT id FROM inspection_tasks WHERE plan_id = %d AND is_deleted = 0)", id))
    dbExecute(con, sprintf("UPDATE inspection_tasks SET is_deleted = 1 WHERE plan_id = %d AND is_deleted = 0", id))
    # 软删除计划本身
    dbExecute(con, sprintf("UPDATE inspection_plans SET is_deleted = 1 WHERE id = %d", id))
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除巡检计划", sprintf("ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "巡检计划已删除"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# ----------------------------------------------------------
# 2. 巡检检查项 (inspection_items)
# ----------------------------------------------------------

# 获取检查项模板（按巡检项分类）
inspection_template_get_by_category <- function(category = NULL) {
  con <- db_connect()
  tryCatch({
    if (is.null(category) || category == "") {
      query <- "SELECT * FROM inspection_item_templates WHERE active = 1 ORDER BY category, sort_order"
      result <- dbGetQuery(con, query)
    } else {
      query <- sprintf("SELECT * FROM inspection_item_templates WHERE active = 1 AND category = '%s' ORDER BY sort_order", 
                       gsub("'", "''", category))
      result <- dbGetQuery(con, query)
    }
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取所有可用的巡检项分类
inspection_category_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT DISTINCT category FROM inspection_item_templates WHERE active = 1 ORDER BY category"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取计划的所有检查项
inspection_item_get_by_plan <- function(plan_id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT * FROM inspection_items WHERE plan_id = %d ORDER BY sort_order, id", 
                    as.integer(plan_id))
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 添加检查项到计划
inspection_item_add <- function(plan_id, item_name, item_description, check_standard,
                                 scoring_type = "pass_fail", max_score = 100, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    query <- sprintf(
      "INSERT INTO inspection_items (plan_id, item_name, item_description, check_standard, scoring_type, max_score)
       VALUES (%d, '%s', '%s', '%s', '%s', %d)",
      as.integer(plan_id), gsub("'", "''", item_name), gsub("'", "''", item_description),
      gsub("'", "''", check_standard), scoring_type, as.integer(max_score)
    )
    dbExecute(con, query)
    return(list(success = TRUE, message = "检查项添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 删除检查项
inspection_item_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM inspection_items WHERE id = %d", as.integer(id)))
    return(list(success = TRUE, message = "检查项删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# ----------------------------------------------------------
# 3. 巡检任务 (inspection_tasks) - 从计划生成的具体执行任务
# ----------------------------------------------------------

# 生成巡检任务编号：INS-TSK-YYYYMMDD-XXX
inspection_task_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("INS-TSK-", today, "-")
    
    query <- sprintf("SELECT task_no FROM inspection_tasks 
                      WHERE task_no LIKE '%s%%' 
                      ORDER BY task_no DESC LIMIT 1", prefix)
    result <- dbGetQuery(con, query)
    
    if (nrow(result) > 0) {
      last_seq <- as.integer(sub(".*-", "", result$task_no[1]))
      new_seq <- last_seq + 1
    } else {
      new_seq <- 1
    }
    
    return(sprintf("%s%03d", prefix, new_seq))
  }, error = function(e) {
    return(sprintf("INS-TSK-%s-001", format(Sys.Date(), "%Y%m%d")))
  }, finally = {
    db_disconnect(con)
  })
}

# 从计划生成任务（一个计划生成一个任务，包含所有检查项）
inspection_task_generate_from_plan <- function(plan_id, scheduled_date, inspector, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    # 获取计划的检查项
    items <- dbGetQuery(con, sprintf("SELECT * FROM inspection_items WHERE plan_id = %d ORDER BY sort_order", plan_id))
    
    if (nrow(items) == 0) {
      return(list(success = FALSE, message = "计划中没有检查项，无法生成任务"))
    }
    
    # 获取计划信息
    plan <- dbGetQuery(con, sprintf("SELECT * FROM inspection_plans WHERE id = %d", plan_id))
    if (nrow(plan) == 0) {
      return(list(success = FALSE, message = "计划不存在"))
    }
    plan <- plan[1, ]
    
    # 生成任务编号
    task_no <- inspection_task_generate_no()
    
    # 将检查项序列化为JSON存储在任务中
    items_json <- jsonlite::toJSON(apply(items, 1, function(row) {
      list(
        item_id = as.integer(row["id"]),
        item_name = as.character(row["item_name"]),
        item_description = ifelse(is.na(row["item_description"]), "", as.character(row["item_description"])),
        check_standard = ifelse(is.na(row["check_standard"]), "", as.character(row["check_standard"])),
        scoring_type = as.character(row["scoring_type"]),
        max_score = as.integer(row["max_score"])
      )
    }), auto_unbox = FALSE)
    
    query <- sprintf(
      "INSERT INTO inspection_tasks 
       (task_no, plan_id, item_ids, item_names, check_standards, 
        inspector, scheduled_date, status, created_at)
       VALUES ('%s', %d, '%s', '%s', '%s', %d, '%s', 'pending', CURRENT_TIMESTAMP)",
      task_no, plan_id,
      gsub("'", "''", items_json),
      gsub("'", "''", paste(items$item_name, collapse = " | ")),
      gsub("'", "''", paste(ifelse(is.na(items$check_standard), "", items$check_standard), collapse = " || ")),
      as.integer(inspector), scheduled_date
    )
    dbExecute(con, query)
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("生成巡检任务", sprintf("计划ID: %d", plan_id), operator_name,
                      sprintf("生成任务: %s，包含 %d 个检查项", task_no, nrow(items)))
    
    return(list(success = TRUE, message = sprintf("成功生成巡检任务：%s（包含 %d 个检查项）", task_no, nrow(items))))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("生成任务失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取我的巡检任务（待执行，排除已删除）
inspection_task_get_mine <- function(inspector_id, status_filter = NULL) {
  con <- db_connect()
  tryCatch({
    query <- sprintf(
      "SELECT t.id, t.task_no, t.plan_id, t.item_ids, t.item_names, t.check_standards,
              t.inspector, t.scheduled_date, t.status, t.created_at, t.updated_at, t.is_deleted,
              p.name as plan_name, p.category as plan_category,
              u.username as inspector_name, p.responsible_user
       FROM inspection_tasks t
       LEFT JOIN inspection_plans p ON t.plan_id = p.id
       LEFT JOIN users u ON t.inspector = u.id
       WHERE t.inspector = %d AND t.is_deleted = 0", as.integer(inspector_id))
    
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- sprintf("%s AND t.status = '%s'", query, status_filter)
    }
    
    query <- paste(query, "ORDER BY t.scheduled_date ASC, t.created_at ASC")
    result <- dbGetQuery(con, query)
    
    # 添加检查项显示格式
    if (nrow(result) > 0) {
      result$item_display <- sapply(result$item_names, function(x) {
        if (is.na(x) || x == "") return("—")
        items <- strsplit(x, " \\| ")[[1]]
        if (length(items) > 2) {
          paste0(items[1], " 等", length(items), "项")
        } else {
          paste(items, collapse = " | ")
        }
      })
    } else {
      result$item_display <- character(0)
    }
    
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取所有巡检任务
inspection_task_get_all <- function(status_filter = NULL, plan_filter = NULL) {
  con <- db_connect()
  tryCatch({
    query <- "SELECT t.id, t.task_no, t.plan_id, t.item_ids, t.item_names, t.check_standards,
                     t.inspector, t.scheduled_date, t.status, t.created_at, t.updated_at, t.is_deleted,
                     p.name as plan_name, p.category as plan_category,
                     u.username as inspector_name
              FROM inspection_tasks t
              LEFT JOIN inspection_plans p ON t.plan_id = p.id
              LEFT JOIN users u ON t.inspector = u.id
              WHERE t.is_deleted = 0"
    
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- sprintf("%s AND t.status = '%s'", query, status_filter)
    }
    if (!is.null(plan_filter) && plan_filter != "" && plan_filter != "all") {
      query <- sprintf("%s AND t.plan_id = %d", query, as.integer(plan_filter))
    }
    
    query <- paste(query, "ORDER BY t.scheduled_date DESC, t.created_at DESC")
    result <- dbGetQuery(con, query)
    
    # 添加检查项显示格式
    if (nrow(result) > 0) {
      result$item_display <- sapply(result$item_names, function(x) {
        if (is.na(x) || x == "") return("—")
        items <- strsplit(x, " \\| ")[[1]]
        if (length(items) > 2) {
          paste0(items[1], " 等", length(items), "项")
        } else {
          paste(items, collapse = " | ")
        }
      })
    } else {
      result$item_display <- character(0)
    }
    
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 按ID获取巡检任务详情（排除已删除）
inspection_task_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf(
      "SELECT t.*, p.name as plan_name, p.category as plan_category, p.description as plan_description,
              u.username as inspector_name, r.username as responsible_name
       FROM inspection_tasks t
       LEFT JOIN inspection_plans p ON t.plan_id = p.id
       LEFT JOIN users u ON t.inspector = u.id
       LEFT JOIN users r ON p.responsible_user = r.id
       WHERE t.id = %d AND t.is_deleted = 0", as.integer(id))
    result <- dbGetQuery(con, query)
    
    # 如果有 item_ids JSON 字段，解析出检查项详情
    if (nrow(result) > 0 && !is.na(result$item_ids[1]) && result$item_ids[1] != "") {
      tryCatch({
        items_list <- jsonlite::fromJSON(result$item_ids[1])
        # 获取原始检查项数据
        original_items <- dbGetQuery(con, sprintf(
          "SELECT * FROM inspection_items WHERE plan_id = %d ORDER BY sort_order", 
          as.integer(result$plan_id[1])))
        if (nrow(original_items) > 0) {
          # 合并原始数据到结果中
          result$parsed_items <- list(items_list)
          result$original_items <- list(original_items)
        } else {
          result$parsed_items <- list(NULL)
          result$original_items <- list(NULL)
        }
      }, error = function(e) {
        result$parsed_items <- list(NULL)
        result$original_items <- list(NULL)
      })
    } else {
      result$parsed_items <- list(NULL)
      result$original_items <- list(NULL)
    }
    
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 更新任务状态
inspection_task_update_status <- function(id, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    query <- sprintf(
      "UPDATE inspection_tasks SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d",
      status, as.integer(id)
    )
    dbExecute(con, query)
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新巡检任务状态", sprintf("任务ID: %d", id), operator_name,
                      sprintf("新状态: %s", status))
    
    return(list(success = TRUE, message = "任务状态更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 删除巡检任务（软删除）
inspection_task_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    # 软删除关联记录
    dbExecute(con, sprintf("UPDATE inspection_records SET is_deleted = 1 WHERE task_id = %d AND is_deleted = 0", id))
    # 软删除任务
    dbExecute(con, sprintf("UPDATE inspection_tasks SET is_deleted = 1 WHERE id = %d", id))
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除巡检任务", sprintf("ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "巡检任务已删除"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# ----------------------------------------------------------
# 4. 巡检记录 (inspection_records) - 检查执行结果
# ----------------------------------------------------------

# 提交巡检记录
inspection_record_add <- function(task_id, result_type, score, remark, photos_json, 
                                  current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    # 处理照片JSON
    photos_part <- ifelse(is.null(photos_json) || photos_json == "", 
                         "NULL", sprintf("'%s'", gsub("'", "''", photos_json)))
    
    query <- sprintf(
      "INSERT INTO inspection_records (task_id, inspector, result_type, score, remark, photos, created_at)
       VALUES (%d, %d, '%s', %d, '%s', %s, CURRENT_TIMESTAMP)",
      as.integer(task_id), user_id, result_type, 
      ifelse(is.null(score), "NULL", as.integer(score)),
      gsub("'", "''", ifelse(is.null(remark), "", remark)),
      photos_part
    )
    dbExecute(con, query)
    
    # 更新任务状态
    new_status <- ifelse(result_type == "abnormal", "abnormal", "completed")
    dbExecute(con, sprintf(
      "UPDATE inspection_tasks SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d",
      new_status, as.integer(task_id)
    ))
    
    # 获取新记录ID
    record_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("提交巡检记录", sprintf("任务ID: %d", task_id), operator_name,
                      sprintf("结果: %s, 评分: %s", result_type, score))
    
    return(list(success = TRUE, message = "巡检记录提交成功", record_id = record_id))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("提交失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 批量提交巡检记录（支持多检查项）
inspection_record_add_batch <- function(task_id, results_json, overall_result, total_score, 
                                        overall_remark, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    # 存储JSON结果
    query <- sprintf(
      "INSERT INTO inspection_records (task_id, inspector, result_type, score, remark, photos, created_at)
       VALUES (%d, %d, '%s', %d, '%s', '%s', CURRENT_TIMESTAMP)",
      as.integer(task_id), user_id, 
      overall_result,
      as.integer(total_score),
      gsub("'", "''", ifelse(is.null(overall_remark), "", overall_remark)),
      gsub("'", "''", results_json)
    )
    dbExecute(con, query)
    
    # 更新任务状态
    new_status <- ifelse(overall_result == "abnormal", "abnormal", "completed")
    dbExecute(con, sprintf(
      "UPDATE inspection_tasks SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d",
      new_status, as.integer(task_id)
    ))
    
    # 获取新记录ID
    record_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("批量提交巡检记录", sprintf("任务ID: %d", task_id), operator_name,
                      sprintf("总体结果: %s, 综合评分: %d", overall_result, total_score))
    
    return(list(success = TRUE, message = "巡检记录提交成功", record_id = record_id))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("提交失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取任务的巡检记录（排除已删除）
inspection_record_get_by_task <- function(task_id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf(
      "SELECT r.*, u.username as inspector_name
       FROM inspection_records r
       LEFT JOIN users u ON r.inspector = u.id
       WHERE r.task_id = %d AND r.is_deleted = 0
       ORDER BY r.created_at DESC", as.integer(task_id))
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取所有巡检记录
inspection_record_get_all <- function(date_from = NULL, date_to = NULL, plan_filter = NULL, status_filter = NULL) {
  con <- db_connect()
  tryCatch({
    query <- "SELECT r.*, t.task_no, t.item_names, t.plan_id,
                     p.name as plan_name, u.username as inspector_name,
                     t.status as task_status
              FROM inspection_records r
              LEFT JOIN inspection_tasks t ON r.task_id = t.id
              LEFT JOIN inspection_plans p ON t.plan_id = p.id
              LEFT JOIN users u ON r.inspector = u.id
              WHERE r.is_deleted = 0"
    
    if (!is.null(date_from) && date_from != "") {
      query <- sprintf("%s AND DATE(r.created_at) >= '%s'", query, date_from)
    }
    if (!is.null(date_to) && date_to != "") {
      query <- sprintf("%s AND DATE(r.created_at) <= '%s'", query, date_to)
    }
    if (!is.null(plan_filter) && plan_filter != "" && plan_filter != "all") {
      query <- sprintf("%s AND t.plan_id = %d", query, as.integer(plan_filter))
    }
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- sprintf("%s AND t.status = '%s'", query, status_filter)
    }
    
    query <- paste(query, "ORDER BY r.created_at DESC")
    result <- dbGetQuery(con, query)
    
    # 格式化检查项名称（处理多检查项情况）
    if (nrow(result) > 0 && !is.null(result$item_names)) {
      result$item_names_display <- sapply(result$item_names, function(x) {
        if (is.na(x) || x == "") return("—")
        items <- strsplit(x, " \\| ")[[1]]
        if (length(items) > 2) {
          paste0(items[1], " 等", length(items), "项")
        } else {
          paste(items, collapse = " | ")
        }
      })
    } else {
      result$item_names_display <- "—"
    }
    
    return(result)
  }, error = function(e) {
    warning(paste("获取巡检记录失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 删除巡检记录（软删除）
inspection_record_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    dbExecute(con, sprintf("UPDATE inspection_records SET is_deleted = 1 WHERE id = %d", id))
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除巡检记录", sprintf("ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "巡检记录已删除"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取已删除的巡检计划（Admin专用）
inspection_plan_get_deleted <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT ip.*, u1.username as creator_name, u2.username as responsible_name
              FROM inspection_plans ip
              LEFT JOIN users u1 ON ip.created_by = u1.id
              LEFT JOIN users u2 ON ip.responsible_user = u2.id
              WHERE ip.is_deleted = 1
              ORDER BY ip.updated_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取已删除的巡检记录（Admin专用）
inspection_record_get_deleted <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT r.*, t.task_no, t.item_names, t.plan_id,
                     p.name as plan_name, u.username as inspector_name
              FROM inspection_records r
              LEFT JOIN inspection_tasks t ON r.task_id = t.id
              LEFT JOIN inspection_plans p ON t.plan_id = p.id
              LEFT JOIN users u ON r.inspector = u.id
              WHERE r.is_deleted = 1
              ORDER BY r.created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# ----------------------------------------------------------
# 5. 巡检问题/异常 (inspection_issues) - 触发整改工单
# ----------------------------------------------------------

# 记录巡检异常
inspection_issue_add <- function(record_id, task_id, issue_type, issue_description,
                                  severity, photos_json, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    photos_part <- ifelse(is.null(photos_json) || photos_json == "",
                         "NULL", sprintf("'%s'", gsub("'", "''", photos_json)))
    
    query <- sprintf(
      "INSERT INTO inspection_issues (record_id, task_id, issue_type, issue_description, severity, photos, status, created_by, created_at)
       VALUES (%s, %d, '%s', '%s', '%s', %s, 'pending', %d, CURRENT_TIMESTAMP)",
      ifelse(is.null(record_id), "NULL", as.integer(record_id)),
      as.integer(task_id),
      gsub("'", "''", issue_type),
      gsub("'", "''", issue_description),
      severity,
      photos_part,
      user_id
    )
    dbExecute(con, query)
    
    issue_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    
    # 更新任务状态为异常
    dbExecute(con, sprintf(
      "UPDATE inspection_tasks SET status = 'abnormal', updated_at = CURRENT_TIMESTAMP WHERE id = %d",
      as.integer(task_id)
    ))
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("记录巡检异常", sprintf("任务ID: %d", task_id), operator_name,
                      sprintf("问题类型: %s, 严重程度: %s", issue_type, severity))
    
    return(list(success = TRUE, message = "异常已记录", issue_id = issue_id))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("记录异常失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 从异常创建整改工单（单个异常）
inspection_issue_create_work_order <- function(issue_id, current_user = NULL) {
  issue_id <- as.integer(issue_id)
  
  con <- db_connect()
  tryCatch({
    # 获取异常详情
    issue <- dbGetQuery(con, sprintf(
      "SELECT i.*, t.item_names, t.plan_id, p.name as plan_name
       FROM inspection_issues i
       LEFT JOIN inspection_tasks t ON i.task_id = t.id
       LEFT JOIN inspection_plans p ON t.plan_id = p.id
       WHERE i.id = %d", issue_id))
    
    if (nrow(issue) == 0) {
      return(list(success = FALSE, message = "异常记录不存在"))
    }
    
    issue <- issue[1, ]
    
    # 检查是否已有工单
    if (!is.na(issue$related_work_order_id) && !is.null(issue$related_work_order_id)) {
      existing_wo <- dbGetQuery(con, sprintf("SELECT order_no FROM work_orders WHERE id = %d", issue$related_work_order_id))
      if (nrow(existing_wo) > 0) {
        return(list(success = FALSE, message = sprintf("该异常已有工单: %s", existing_wo$order_no[1])))
      }
    }
    
    # 生成工单
    order_no <- work_order_generate_number()
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    # 工单标题（使用检查项名称）
    item_display <- issue$item_names
    if (!is.na(item_display) && item_display != "") {
      items <- strsplit(item_display, " \\| ")[[1]]
      if (length(items) > 2) {
        item_display <- paste0(items[1], " 等", length(items), "项")
      }
    } else {
      item_display <- "检查项"
    }
    
    title <- sprintf("[整改] %s - %s", 
                    ifelse(is.na(issue$plan_name), "巡检", issue$plan_name),
                    item_display)
    
    description <- sprintf("巡检异常描述：%s\n严重程度：%s\n问题类型：%s\n\n请及时处理并整改。",
                          ifelse(is.na(issue$issue_description), "", issue$issue_description),
                          issue$severity,
                          ifelse(is.na(issue$issue_type), "", issue$issue_type))
    
    query <- sprintf(
      "INSERT INTO work_orders (order_no, title, description, priority, status, category, created_by, created_at)
       VALUES ('%s', '%s', '%s', '高', 'pending', '巡检整改', %d, CURRENT_TIMESTAMP)",
      order_no, gsub("'", "''", title), gsub("'", "''", description), user_id
    )
    dbExecute(con, query)
    
    wo_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    
    # 更新异常记录关联工单ID
    dbExecute(con, sprintf(
      "UPDATE inspection_issues SET related_work_order_id = %d, status = 'processing' WHERE id = %d",
      wo_id, issue_id
    ))
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("从巡检异常创建整改工单", sprintf("异常ID: %d", issue_id), operator_name,
                      sprintf("工单号: %s", order_no))
    
    return(list(success = TRUE, message = sprintf("整改工单已创建: %s", order_no), work_order_id = wo_id, order_no = order_no))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("创建工单失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 从任务创建整改工单（包含该任务所有异常）
inspection_task_create_work_order <- function(task_id, current_user = NULL) {
  task_id <- as.integer(task_id)
  
  con <- db_connect()
  tryCatch({
    # 检查该任务是否已有工单
    existing <- dbGetQuery(con, sprintf(
      "SELECT DISTINCT related_work_order_id FROM inspection_issues WHERE task_id = %d AND related_work_order_id IS NOT NULL", 
      task_id))
    
    if (nrow(existing) > 0 && !is.na(existing$related_work_order_id[1])) {
      existing_wo <- dbGetQuery(con, sprintf("SELECT order_no FROM work_orders WHERE id = %d", existing$related_work_order_id[1]))
      if (nrow(existing_wo) > 0) {
        return(list(success = FALSE, message = sprintf("该任务已有工单: %s", existing_wo$order_no[1])))
      }
    }
    
    # 获取任务详情
    task <- dbGetQuery(con, sprintf(
      "SELECT t.*, p.name as plan_name FROM inspection_tasks t
       LEFT JOIN inspection_plans p ON t.plan_id = p.id
       WHERE t.id = %d", task_id))
    
    if (nrow(task) == 0) {
      return(list(success = FALSE, message = "任务不存在"))
    }
    task <- task[1, ]
    
    # 获取该任务的所有异常
    issues <- dbGetQuery(con, sprintf(
      "SELECT * FROM inspection_issues WHERE task_id = %d AND related_work_order_id IS NULL", task_id))
    
    if (nrow(issues) == 0) {
      return(list(success = FALSE, message = "该任务没有未关联工单的异常"))
    }
    
    # 生成工单
    order_no <- work_order_generate_number()
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    # 工单标题
    item_display <- ifelse(is.na(task$item_names), "检查项", task$item_names)
    items <- strsplit(item_display, " \\| ")[[1]]
    if (length(items) > 2) {
      item_display <- paste0(items[1], " 等", length(items), "项")
    }
    
    title <- sprintf("[整改] %s - %s (含%d项异常)", 
                    ifelse(is.na(task$plan_name), "巡检", task$plan_name),
                    item_display, nrow(issues))
    
    # 工单描述：列出所有异常
    issue_list <- c()
    for (i in 1:nrow(issues)) {
      issue <- issues[i, ]
      issue_text <- sprintf("[%d] %s - %s (%s)", i, 
                          ifelse(is.na(issue$issue_type), "问题", issue$issue_type),
                          ifelse(is.na(issue$issue_description), "无描述", substr(issue$issue_description, 1, 100)),
                          issue$severity)
      issue_list <- c(issue_list, issue_text)
    }
    
    description <- sprintf("该巡检任务共发现 %d 项异常：\n\n%s\n\n请及时处理并整改。",
                          nrow(issues), paste(issue_list, collapse = "\n"))
    
    query <- sprintf(
      "INSERT INTO work_orders (order_no, title, description, priority, status, category, created_by, created_at)
       VALUES ('%s', '%s', '%s', '高', 'pending', '巡检整改', %d, CURRENT_TIMESTAMP)",
      order_no, gsub("'", "''", title), gsub("'", "''", description), user_id
    )
    dbExecute(con, query)
    
    wo_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    
    # 更新该任务所有未关联工单的异常
    dbExecute(con, sprintf(
      "UPDATE inspection_issues SET related_work_order_id = %d, status = 'processing' WHERE task_id = %d AND related_work_order_id IS NULL",
      wo_id, task_id
    ))
    
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("从巡检任务创建整改工单", sprintf("任务ID: %d", task_id), operator_name,
                      sprintf("工单号: %s, 包含 %d 项异常", order_no, nrow(issues)))
    
    return(list(success = TRUE, message = sprintf("整改工单已创建: %s (含%d项异常)", order_no, nrow(issues)), 
                work_order_id = wo_id, order_no = order_no, issue_count = nrow(issues)))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("创建工单失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取所有巡检异常
inspection_issue_get_all <- function(status_filter = NULL) {
  con <- db_connect()
  tryCatch({
    query <- "SELECT i.*, t.task_no, t.item_names, p.name as plan_name,
                     u.username as creator_name, wo.order_no as work_order_no
              FROM inspection_issues i
              LEFT JOIN inspection_tasks t ON i.task_id = t.id
              LEFT JOIN inspection_plans p ON t.plan_id = p.id
              LEFT JOIN users u ON i.created_by = u.id
              LEFT JOIN work_orders wo ON i.related_work_order_id = wo.id
              WHERE 1=1"
    
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- sprintf("%s AND i.status = '%s'", query, status_filter)
    }
    
    query <- paste(query, "ORDER BY i.created_at DESC")
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取按任务分组的巡检异常（一个任务多个异常合并显示）
inspection_issue_get_grouped <- function(status_filter = NULL) {
  con <- db_connect()
  tryCatch({
    # 首先获取所有异常
    query <- "SELECT i.*, t.task_no, t.item_names, p.name as plan_name,
                     u.username as creator_name, wo.order_no as work_order_no,
                     wo.id as work_order_id
              FROM inspection_issues i
              LEFT JOIN inspection_tasks t ON i.task_id = t.id
              LEFT JOIN inspection_plans p ON t.plan_id = p.id
              LEFT JOIN users u ON i.created_by = u.id
              LEFT JOIN work_orders wo ON i.related_work_order_id = wo.id
              WHERE 1=1"
    
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- sprintf("%s AND i.status = '%s'", query, status_filter)
    }
    
    query <- paste(query, "ORDER BY i.task_id, i.created_at DESC")
    issues <- dbGetQuery(con, query)
    
    if (nrow(issues) == 0) {
      return(data.frame())
    }
    
    # 按任务ID分组
    grouped <- list()
    for (i in seq_len(nrow(issues))) {
      task_id <- issues$task_id[i]
      if (is.na(task_id)) next
      
      if (is.null(grouped[[as.character(task_id)]])) {
        grouped[[as.character(task_id)]] <- list(
          task_id = task_id,
          task_no = issues$task_no[i],
          plan_name = issues$plan_name[i],
          item_names = issues$item_names[i],
          work_order_no = issues$work_order_no[i],
          work_order_id = issues$work_order_id[i],
          creator_name = issues$creator_name[i],
          created_at = issues$created_at[i],
          issues = list()
        )
        # 获取该任务的状态（使用最新的异常状态或任务状态）
        status_query <- sprintf(
          "SELECT MAX(status) as max_status FROM inspection_issues WHERE task_id = %d", 
          task_id)
        max_status <- dbGetQuery(con, status_query)$max_status[1]
        grouped[[as.character(task_id)]]$status <- ifelse(is.na(max_status), "pending", max_status)
      }
      
      # 添加异常详情
      issue_info <- list(
        id = issues$id[i],
        issue_type = issues$issue_type[i],
        issue_description = issues$issue_description[i],
        severity = issues$severity[i],
        photos = issues$photos[i],
        status = issues$status[i]
      )
      grouped[[as.character(task_id)]]$issues[[length(grouped[[as.character(task_id)]]$issues) + 1]] <- issue_info
    }
    
    # 转换为数据框格式
    result_list <- lapply(names(grouped), function(task_id) {
      g <- grouped[[task_id]]
      # 汇总异常数量和严重程度
      issue_count <- length(g$issues)
      high_count <- sum(sapply(g$issues, function(x) x$severity == "high"), na.rm = TRUE)
      medium_count <- sum(sapply(g$issues, function(x) x$severity == "medium"), na.rm = TRUE)
      low_count <- sum(sapply(g$issues, function(x) x$severity == "low"), na.rm = TRUE)
      
      # 严重程度汇总
      severity_summary <- c(
        if (high_count > 0) sprintf("高:%d", high_count),
        if (medium_count > 0) sprintf("中:%d", medium_count),
        if (low_count > 0) sprintf("低:%d", low_count)
      )
      severity_str <- if (length(severity_summary) > 0) paste(severity_summary, collapse = ", ") else "—"
      
      # 检查项名称显示
      item_display <- if (!is.na(g$item_names) && g$item_names != "") {
        items <- strsplit(g$item_names, " \\| ")[[1]]
        if (length(items) > 2) {
          paste0(items[1], " 等", length(items), "项")
        } else {
          paste(items, collapse = " | ")
        }
      } else "—"
      
      data.frame(
        task_id = g$task_id,
        task_no = g$task_no,
        plan_name = ifelse(is.na(g$plan_name), "—", g$plan_name),
        item_names_display = item_display,
        issue_count = issue_count,
        severity_summary = severity_str,
        status = g$status,
        work_order_no = ifelse(is.na(g$work_order_no), "—", g$work_order_no),
        work_order_id = ifelse(is.na(g$work_order_id), NA, g$work_order_id),
        creator_name = ifelse(is.na(g$creator_name), "—", g$creator_name),
        created_at = substr(g$created_at, 1, 16),
        issues_json = jsonlite::toJSON(g$issues),
        stringsAsFactors = FALSE
      )
    })
    
    if (length(result_list) > 0) {
      result <- do.call(rbind, result_list)
      # 按创建时间倒序
      result <- result[order(-as.numeric(substr(result$created_at, 1, 4)), 
                            -as.numeric(substr(result$created_at, 6, 7)),
                            -as.numeric(substr(result$created_at, 9, 10)),
                            -as.numeric(substr(result$created_at, 12, 13)),
                            -as.numeric(substr(result$created_at, 15, 16))), ]
      return(result)
    } else {
      return(data.frame())
    }
  }, error = function(e) {
    warning(paste("获取分组异常失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 更新异常状态
inspection_issue_update_status <- function(id, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE inspection_issues SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d",
      status, as.integer(id)
    ))
    return(list(success = TRUE, message = "异常状态更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# ----------------------------------------------------------
# 6. 统计与辅助函数
# ----------------------------------------------------------

# 获取巡检统计（排除已删除的记录）
inspection_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT
               COUNT(DISTINCT p.id) as total_plans,
               COUNT(DISTINCT CASE WHEN p.status = 'active' THEN p.id END) as active_plans,
               COUNT(DISTINCT t.id) as total_tasks,
               COUNT(DISTINCT CASE WHEN t.status = 'pending' THEN t.id END) as pending_tasks,
               COUNT(DISTINCT CASE WHEN t.status = 'completed' THEN t.id END) as completed_tasks,
               COUNT(DISTINCT CASE WHEN t.status = 'abnormal' THEN t.id END) as abnormal_tasks,
               COUNT(DISTINCT i.id) as total_issues,
               COUNT(DISTINCT CASE WHEN i.status = 'pending' THEN i.id END) as pending_issues
              FROM inspection_plans p
              LEFT JOIN inspection_tasks t ON p.id = t.plan_id AND t.is_deleted = 0
              LEFT JOIN inspection_issues i ON t.id = i.task_id
              WHERE p.is_deleted = 0"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame(total_plans = 0, active_plans = 0, total_tasks = 0, 
                     pending_tasks = 0, completed_tasks = 0, abnormal_tasks = 0,
                     total_issues = 0, pending_issues = 0))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取可选择的巡检人员列表
inspection_get_inspectors <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT id, username, display_name, role FROM users 
              WHERE active = 1 AND role IN ('it_engineer', 'sys_engineer', 'admin')
              ORDER BY role, username"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# 获取可选择的负责人列表
inspection_get_responsibles <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT id, username, display_name, role FROM users 
              WHERE active = 1 AND role IN ('it_engineer', 'sys_engineer', 'admin', 'it_desk')
              ORDER BY role, username"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

# ----------------------------------------------------------
# 7. 巡检计划评论 (inspection_plan_comments)
# ----------------------------------------------------------

# 添加巡检计划评论
inspection_plan_add_comment <- function(plan_id, comment, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    plan_id <- as.integer(plan_id)
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    
    query <- sprintf("INSERT INTO inspection_plan_comments (plan_id, comment, created_by)
                     VALUES (%d, '%s', %d)",
                     plan_id, gsub("'", "''", comment), user_id)
    dbExecute(con, query)
    Sys.sleep(0.01)
    update_sql <- paste0("UPDATE inspection_plan_comments SET created_at = datetime('now', 'localtime') WHERE id = (SELECT MAX(id) FROM inspection_plan_comments WHERE plan_id = ", plan_id, ")")
    dbExecute(con, update_sql)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("添加巡检计划评论", sprintf("计划ID: %d", plan_id), operator_name)
    
    return(list(success = TRUE, message = "评论添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加评论失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取巡检计划评论列表
inspection_plan_get_comments <- function(plan_id) {
  con <- db_connect()
  tryCatch({
    plan_id <- as.integer(plan_id)
    query <- sprintf("SELECT c.id, c.comment, c.created_at, u.username as creator_name
                      FROM inspection_plan_comments c
                      LEFT JOIN users u ON c.created_by = u.id
                      WHERE c.plan_id = %d
                      ORDER BY c.created_at DESC", plan_id)
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    warning(paste("获取巡检计划评论失败:", e$message))
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}
