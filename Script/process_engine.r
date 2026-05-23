# 流程引擎核心模块 v2

`%||%` <- function(a, b) if (is.null(a)) b else a

##################
# 编号生成
##################

process_generate_no <- function(prefix = "PRC") {
  date_str <- format(Sys.Date(), "%Y%m%d")
  con <- db_connect()
  tryCatch({
    today_prefix <- sprintf("%s%s", prefix, date_str)
    max_no <- dbGetQuery(con, sprintf(
      "SELECT MAX(def_no) as max_no FROM process_definitions WHERE def_no LIKE '%s%%'", today_prefix))
    seq <- 1
    if (!is.na(max_no$max_no[1])) {
      last_seq <- as.integer(substr(max_no$max_no[1], nchar(today_prefix) + 1, nchar(today_prefix) + 3))
      if (!is.na(last_seq)) seq <- last_seq + 1
    }
    sprintf("%s%03d", today_prefix, seq)
  }, finally = { db_disconnect(con) })
}

process_instance_generate_no <- function() {
  date_str <- format(Sys.Date(), "%Y%m%d")
  con <- db_connect()
  tryCatch({
    today_prefix <- sprintf("PFI%s", date_str)
    max_no <- dbGetQuery(con, sprintf(
      "SELECT MAX(instance_no) as max_no FROM process_instances WHERE instance_no LIKE '%s%%'", today_prefix))
    seq <- 1
    if (!is.na(max_no$max_no[1])) {
      last_seq <- as.integer(substr(max_no$max_no[1], nchar(today_prefix) + 1, nchar(today_prefix) + 3))
      if (!is.na(last_seq)) seq <- last_seq + 1
    }
    sprintf("%s%03d", today_prefix, seq)
  }, finally = { db_disconnect(con) })
}

##################
# 日志与事件
##################

process_log_write <- function(instance_id, node_id = NULL,
                               log_level = "info", log_type = "general",
                               message, duration_ms = NULL, detail = NULL) {
  tryCatch({
    con <- db_connect()
    on.exit(db_disconnect(con))
    dbExecute(con, sprintf(
      "INSERT INTO process_logs (instance_id, node_id, log_level, log_type, message, duration_ms, detail)
       VALUES (%s, %s, '%s', '%s', '%s', %s, %s)",
      ifelse(is.null(instance_id), "NULL", as.character(instance_id)),
      ifelse(is.null(node_id), "NULL", sprintf("'%s'", gsub("'", "''", node_id))),
      log_level, log_type,
      gsub("'", "''", substr(message, 1, 500)),
      ifelse(is.null(duration_ms), "NULL", as.character(duration_ms)),
      ifelse(is.null(detail), "NULL", sprintf("'%s'", gsub("'", "''", detail)))
    ))
  }, error = function(e) { warning("日志写入失败:", e$message) })
}

process_event_record <- function(event_type, instance_id = NULL, node_id = NULL,
                                  source = "engine", status = "success",
                                  message = NULL, payload = NULL) {
  tryCatch({
    con <- db_connect()
    on.exit(db_disconnect(con))
    dbExecute(con, sprintf(
      "INSERT INTO process_events (event_type, instance_id, node_id, source, status, message, payload)
       VALUES ('%s', %s, %s, '%s', '%s', %s, %s)",
      event_type,
      ifelse(is.null(instance_id), "NULL", as.character(instance_id)),
      ifelse(is.null(node_id), "NULL", sprintf("'%s'", gsub("'", "''", node_id))),
      source, status,
      ifelse(is.null(message), "NULL", sprintf("'%s'", gsub("'", "''", message))),
      ifelse(is.null(payload), "NULL", sprintf("'%s'", gsub("'", "''", payload)))
    ))
  }, error = function(e) { warning("事件记录失败:", e$message) })
}

##################
# 流程定义管理
##################

process_def_create <- function(name, description = "", category = "general",
                                definition = "{}", created_by = NULL) {
  con <- db_connect()
  tryCatch({
    def_no <- process_generate_no()
    dbExecute(con, sprintf(
      "INSERT INTO process_definitions (def_no, name, description, category, definition, created_by)
       VALUES ('%s', '%s', '%s', '%s', '%s', %s)",
      def_no, gsub("'", "''", name), gsub("'", "''", description),
      category, gsub("'", "''", definition),
      ifelse(is.null(created_by), "NULL", as.character(created_by))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    process_log_write(NULL, NULL, "info", "def_create", sprintf("创建流程定义: %s (%s)", name, def_no))
    list(success = TRUE, id = id, def_no = def_no, message = sprintf("流程定义「%s」创建成功", name))
  }, error = function(e) {
    list(success = FALSE, message = paste("创建失败:", e$message))
  }, finally = { db_disconnect(con) })
}

process_def_list <- function(category = NULL, status = NULL) {
  con <- db_connect()
  tryCatch({
    where <- "WHERE 1=1"
    if (!is.null(category) && nchar(category) > 0) where <- paste0(where, sprintf(" AND category = '%s'", category))
    if (!is.null(status) && nchar(status) > 0) where <- paste0(where, sprintf(" AND status = '%s'", status))
    dbGetQuery(con, sprintf(
      "SELECT pd.*, u.username as creator_name
       FROM process_definitions pd LEFT JOIN users u ON pd.created_by = u.id
       %s ORDER BY pd.updated_at DESC", where))
  }, finally = { db_disconnect(con) })
}

process_def_get <- function(def_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf(
      "SELECT pd.*, u.username as creator_name
       FROM process_definitions pd LEFT JOIN users u ON pd.created_by = u.id
       WHERE pd.id = %d", as.integer(def_id)))
    if (nrow(result) == 0) return(NULL)
    result
  }, finally = { db_disconnect(con) })
}

process_def_publish <- function(def_id, change_log = "") {
  con <- db_connect()
  tryCatch({
    def <- process_def_get(def_id)
    if (is.null(def)) return(list(success = FALSE, message = "流程定义不存在"))
    if (def$status[1] == "published") return(list(success = TRUE, message = "已发布"))

    new_version <- def$version[1] + 1
    dbExecute(con, sprintf(
      "INSERT INTO process_definition_versions (def_id, version, definition, change_log)
       VALUES (%d, %d, '%s', '%s')",
      as.integer(def_id), new_version, gsub("'", "''", def$definition[1]), gsub("'", "''", change_log)))
    dbExecute(con, sprintf(
      "UPDATE process_definitions SET version = %d, status = 'published', updated_at = datetime('now','localtime')
       WHERE id = %d", new_version, as.integer(def_id)))
    process_log_write(NULL, NULL, "info", "def_publish", sprintf("发布流程定义: %s v%d", def$name[1], new_version))
    list(success = TRUE, message = sprintf("发布成功（v%d）", new_version))
  }, error = function(e) {
    list(success = FALSE, message = paste("发布失败:", e$message))
  }, finally = { db_disconnect(con) })
}

##################
# 流程实例管理
##################

process_instance_start <- function(def_id, title = NULL, context_data = NULL, started_by = NULL) {
  con <- db_connect()
  tryCatch({
    def <- process_def_get(def_id)
    if (is.null(def)) return(list(success = FALSE, message = "流程定义不存在"))
    if (def$status[1] != "published") return(list(success = FALSE, message = "流程定义未发布，请先发布"))

    instance_no <- process_instance_generate_no()
    if (is.null(title) || title == "") title <- def$name[1]
    if (is.null(context_data)) context_data <- list()
    context_json <- jsonlite::toJSON(context_data, auto_unbox = TRUE)

    dbExecute(con, sprintf(
      "INSERT INTO process_instances (instance_no, def_id, def_version, title, status, context_data, context_version, started_by)
       VALUES ('%s', %d, %d, '%s', 'running', '%s', 1, %s)",
      instance_no, as.integer(def_id), def$version[1],
      gsub("'", "''", title), gsub("'", "''", context_json),
      ifelse(is.null(started_by), "NULL", as.character(started_by))))
    instance_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]

    process_log_write(instance_id, NULL, "info", "instance_start",
                      sprintf("启动流程: %s (%s)", title, instance_no))
    process_event_record("instance_start", instance_id, NULL, source = "engine",
                         status = "success", message = sprintf("流程 %s 已启动", instance_no))

    # 解析定义并激活开始节点
    definition <- tryCatch(jsonlite::fromJSON(def$definition[1], simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(definition$nodes)) {
      for (node in definition$nodes) {
        if (!is.null(node$type) && node$type == "start") {
          process_activate_node(instance_id, node)
          break
        }
      }
    }

    list(success = TRUE, id = instance_id, instance_no = instance_no, def_name = def$name[1],
         message = sprintf("流程实例 %s 已启动", instance_no))
  }, error = function(e) {
    list(success = FALSE, message = paste("启动失败:", e$message))
  }, finally = { db_disconnect(con) })
}

process_instance_list <- function(status = NULL, user_id = NULL) {
  con <- db_connect()
  tryCatch({
    where <- "WHERE 1=1"
    if (!is.null(status) && nchar(status) > 0) where <- paste0(where, sprintf(" AND pi.status = '%s'", status))
    if (!is.null(user_id)) where <- paste0(where, sprintf(" AND pi.started_by = %d", as.integer(user_id)))
    dbGetQuery(con, sprintf(
      "SELECT pi.*, pd.name as def_name, u.username as started_by_name
       FROM process_instances pi
       LEFT JOIN process_definitions pd ON pi.def_id = pd.id
       LEFT JOIN users u ON pi.started_by = u.id
       %s ORDER BY pi.started_at DESC", where))
  }, finally = { db_disconnect(con) })
}

process_instance_get <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf(
      "SELECT pi.*, pd.name as def_name, pd.definition
       FROM process_instances pi
       LEFT JOIN process_definitions pd ON pi.def_id = pd.id
       WHERE pi.id = %d", as.integer(instance_id)))
    if (nrow(result) == 0) NULL else result
  }, finally = { db_disconnect(con) })
}

##################
# 节点管理
##################

process_activate_node <- function(instance_id, node_def) {
  tryCatch({
    existing <- process_get_node(instance_id, node_def$id)
    if (!is.null(existing)) return(TRUE)
    con <- db_connect()
    on.exit(db_disconnect(con))
    dbExecute(con, sprintf(
      "INSERT INTO process_nodes (instance_id, node_id, node_type, node_name, status,
        auto_action, timeout_minutes, timeout_action, max_retries, entered_at)
       VALUES (%d, '%s', '%s', '%s', 'active', %s, %d, '%s', %d, datetime('now','localtime'))",
      as.integer(instance_id), node_def$id, node_def$type,
      gsub("'", "''", node_def$label %||% node_def$id),
      if (is.null(node_def$action)) "NULL" else sprintf("'%s'", gsub("'", "''", jsonlite::toJSON(node_def$action, auto_unbox = TRUE))),
      node_def$timeout_minutes %||% 0, node_def$timeout_action %||% "terminate",
      node_def$max_retries %||% 3))
    dbExecute(con, sprintf(
      "UPDATE process_instances SET current_node = '%s', updated_at = datetime('now','localtime') WHERE id = %d",
      node_def$id, as.integer(instance_id)))
    TRUE
  }, error = function(e) {
    process_log_write(instance_id, node_def$id %||% "", "error", "node_error",
                      sprintf("激活节点失败: %s", e$message))
    FALSE
  })
}

process_get_node <- function(instance_id, node_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf(
      "SELECT * FROM process_nodes WHERE instance_id = %d AND node_id = '%s'",
      as.integer(instance_id), node_id))
    if (nrow(result) == 0) NULL else result
  }, finally = { db_disconnect(con) })
}

process_get_active_nodes <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM process_nodes WHERE instance_id = %d ORDER BY id", as.integer(instance_id)))
  }, finally = { db_disconnect(con) })
}

##################
# 流转引擎
##################

process_advance <- function(instance_id) {
  instance <- process_instance_get(instance_id)
  if (is.null(instance)) return(list(success = FALSE, message = "实例不存在"))
  if (instance$status[1] != "running") return(list(success = FALSE, message = "流程不在运行状态"))

  current_node_id <- instance$current_node[1]
  if (is.null(current_node_id) || is.na(current_node_id)) {
    return(list(success = FALSE, message = "无当前节点"))
  }

  definition <- tryCatch(jsonlite::fromJSON(instance$definition[1], simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(definition) || is.null(definition$nodes)) {
    return(list(success = FALSE, message = "流程定义JSON解析失败"))
  }

  # 查找当前节点定义
  current_node <- NULL
  for (n in definition$nodes) {
    if (!is.null(n$id) && n$id == current_node_id) { current_node <- n; break }
  }
  if (is.null(current_node)) return(list(success = FALSE, message = sprintf("节点 %s 未在定义中找到", current_node_id)))

  # 情况1：当前节点是结束节点 → 完成流程
  if (current_node$type == "end") {
    con <- db_connect()
    tryCatch({
      dbExecute(con, sprintf(
        "UPDATE process_instances SET status = 'completed', completed_at = datetime('now','localtime'),
         updated_at = datetime('now','localtime') WHERE id = %d", as.integer(instance_id)))
    }, finally = { db_disconnect(con) })
    process_log_write(instance_id, current_node_id, "info", "instance_complete", "流程已完成")
    process_event_record("instance_end", instance_id, current_node_id, source = "engine",
                         status = "success", message = "流程已完成")
    return(list(success = TRUE, message = "流程已完成", completed = TRUE, instance_id = instance_id))
  }

  # 查找符合条件的出线
  context_data <- tryCatch(jsonlite::fromJSON(instance$context_data[1], simplifyVector = FALSE), error = function(e) list())

  next_node_id <- NULL
  for (t in definition$transitions) {
    if (!is.null(t$from) && t$from == current_node_id) {
      cond <- t$condition %||% ""
      if (nchar(cond) == 0 || evaluate_condition(cond, context_data)) {
        next_node_id <- t$to
        process_log_write(instance_id, current_node_id, "info", "transition",
                          sprintf("流转: %s → %s", current_node_id, next_node_id))
        break
      }
    }
  }
  if (is.null(next_node_id)) {
    process_log_write(instance_id, current_node_id, "warn", "transition", "无匹配条件分支")
    return(list(success = FALSE, message = "无匹配条件分支"))
  }

  # 查找目标节点定义
  next_node_def <- NULL
  for (n in definition$nodes) {
    if (!is.null(n$id) && n$id == next_node_id) { next_node_def <- n; break }
  }
  if (is.null(next_node_def)) return(list(success = FALSE, message = sprintf("目标节点 %s 未定义", next_node_id)))

  # 标记当前节点已完成
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE process_nodes SET status = 'completed', completed_at = datetime('now','localtime')
       WHERE instance_id = %d AND node_id = '%s' AND status = 'active'",
      as.integer(instance_id), current_node_id))
  }, finally = { db_disconnect(con) })

  # 激活下一个节点
  process_activate_node(instance_id, next_node_def)

  node_label <- next_node_def$label %||% next_node_id
  process_log_write(instance_id, next_node_id, "info", "node_enter",
                    sprintf("进入节点: %s (%s)", node_label, next_node_def$type))
  process_event_record("node_activate", instance_id, next_node_id, source = "engine",
                       status = "success", message = sprintf("激活节点: %s", node_label))

  # 自动推进：自动节点和结束节点
  if (next_node_def$type %in% c("auto", "end")) {
    return(process_advance(instance_id))
  }

  list(success = TRUE, message = sprintf("已流转到「%s」（%s）", node_label, next_node_def$type),
       next_node = next_node_id, next_node_type = next_node_def$type, next_node_label = node_label)
}

##################
# 条件表达式评估
##################

evaluate_condition <- function(condition_str, context) {
  if (is.null(condition_str) || nchar(condition_str) == 0 || condition_str == "true") return(TRUE)
  sandbox_env <- new.env(parent = emptyenv())
  sandbox_env$context <- context
  if (!is.null(context$result)) sandbox_env$result <- context$result
  tryCatch(isTRUE(eval(parse(text = condition_str), envir = sandbox_env)), error = function(e) FALSE)
}

##################
# 查询
##################

process_get_todos <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT pn.id as node_instance_id, pn.node_name, pn.node_type, pn.entered_at, pn.timeout_minutes,
              pi.instance_no, pi.title as instance_title, pd.name as def_name, pi.id as instance_id
       FROM process_nodes pn
       JOIN process_instances pi ON pn.instance_id = pi.id
       JOIN process_definitions pd ON pi.def_id = pd.id
       WHERE pn.status = 'active' AND pn.node_type = 'task'
         AND (pn.assignee IS NULL OR pn.assignee = %d)
         AND pi.status = 'running'
       ORDER BY pn.entered_at DESC", as.integer(user_id)))
  }, finally = { db_disconnect(con) })
}

process_get_logs <- function(instance_id, limit = 100) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM process_logs WHERE instance_id = %d ORDER BY created_at DESC LIMIT %d",
      as.integer(instance_id), as.integer(limit)))
  }, finally = { db_disconnect(con) })
}

process_get_events <- function(instance_id, limit = 100) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM process_events WHERE instance_id = %d ORDER BY created_at DESC LIMIT %d",
      as.integer(instance_id), as.integer(limit)))
  }, finally = { db_disconnect(con) })
}

##################
# 示例流程 + 一键创建并启动
##################

#' 创建示例流程定义（自动发布）
process_create_demo_def <- function(created_by = NULL) {
  demo_json <- '{
    "nodes": [
      { "id": "start", "type": "start", "label": "开始" },
      { "id": "approve", "type": "task", "label": "审批确认", "timeout_minutes": 1440 },
      { "id": "end", "type": "end", "label": "结束" }
    ],
    "transitions": [
      { "from": "start", "to": "approve", "condition": "" },
      { "from": "approve", "to": "end", "condition": "" }
    ]
  }'
  result <- process_def_create("示例审批流程", "开始 → 审批确认 → 结束",
                                category = "审批", definition = demo_json, created_by = created_by)
  if (result$success) {
    pub <- process_def_publish(result$id, change_log = "自动发布")
  }
  result
}

#' 一键创建并启动示例流程
process_create_and_start_demo <- function(started_by = NULL) {
  def_result <- process_create_demo_def(created_by = started_by)
  if (!def_result$success) return(def_result)

  inst_result <- process_instance_start(def_id = def_result$id,
    title = sprintf("示例流程 - %s", format(Sys.time(), "%H:%M")),
    context_data = list(title = "示例审批", priority = "normal"),
    started_by = started_by)
  if (!inst_result$success) return(inst_result)

  # 从开始节点自动推进到审批节点
  advance <- process_advance(inst_result$id)

  list(success = TRUE,
       def_id = def_result$id, def_no = def_result$def_no,
       instance_id = inst_result$id, instance_no = inst_result$instance_no,
       advance = advance,
       def_name = "示例审批流程",
       message = sprintf("已创建并启动「示例审批流程」，请在「我的待办」中处理审批确认"))
}

##################
# 状态标签
##################

process_status_label <- function(status) {
  labels <- c("running" = "运行中", "completed" = "已完成",
              "terminated" = "已终止", "suspended" = "已暂停",
              "draft" = "草稿", "published" = "已发布", "archived" = "已归档",
              "active" = "进行中", "pending" = "待处理",
              "skipped" = "已跳过", "failed" = "失败", "timeout" = "超时")
  result <- labels[status]
  result[is.na(result)] <- status[is.na(result)]
  unname(result)
}
