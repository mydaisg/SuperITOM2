# 项目管理模块
# 数据层：项目 → 阶段 → 工作包 → 任务 → 执行反馈
# 包含所有数据库CRUD操作

# ================================================================
# 项目 CRUD
# ================================================================

# 获取所有项目
project_get_all <- function(status_filter = NULL) {
  con <- db_connect()
  tryCatch({
    query <- "SELECT p.*, u.username as creator_name FROM projects p
              LEFT JOIN users u ON p.created_by = u.id"
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- paste(query, sprintf("WHERE p.status = '%s'", status_filter))
    }
    query <- paste(query, "ORDER BY p.created_at DESC")
    dbGetQuery(con, query)
  }, error = function(e) {
    warning(paste("获取项目列表失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 按ID获取项目详情
project_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT p.*, u.username as creator_name FROM projects p
                      LEFT JOIN users u ON p.created_by = u.id
                      WHERE p.id = %d", as.integer(id))
    dbGetQuery(con, query)
  }, error = function(e) {
    warning(paste("获取项目详情失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 生成项目编号 PRJ + YYYYMMDD + 3位流水号
project_generate_number <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("PRJ", today)
    result <- dbGetQuery(con, sprintf(
      "SELECT project_no FROM projects WHERE project_no LIKE '%s%%' ORDER BY project_no DESC LIMIT 1", prefix))
    if (nrow(result) > 0 && !is.na(result$project_no[1])) {
      new_seq <- as.integer(substr(result$project_no[1], nchar(prefix) + 1, nchar(result$project_no[1]))) + 1
    } else {
      new_seq <- 1
    }
    sprintf("%s%03d", prefix, new_seq)
  }, error = function(e) {
    sprintf("PRJ%s001", format(Sys.Date(), "%Y%m%d"))
  }, finally = { db_disconnect(con) })
}

# 创建项目
project_add <- function(name, description, priority, start_date, end_date, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    project_no <- project_generate_number()
    dbExecute(con, sprintf(
      "INSERT INTO projects (project_no, name, description, priority, status, start_date, end_date, created_by)
       VALUES ('%s', '%s', '%s', '%s', 'planning', '%s', '%s', %d)",
      project_no, name, description, priority, start_date, end_date, user_id))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建项目", name, operator_name, sprintf("项目编号: %s", project_no))
    list(success = TRUE, message = sprintf("项目创建成功，编号: %s", project_no))
  }, error = function(e) {
    list(success = FALSE, message = paste("创建失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 更新项目
project_update <- function(id, name, description, priority, status, start_date, end_date, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE projects SET name='%s', description='%s', priority='%s', status='%s',
       start_date='%s', end_date='%s', updated_at=CURRENT_TIMESTAMP WHERE id=%d",
      name, description, priority, status, start_date, end_date, as.integer(id)))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新项目", sprintf("项目ID: %d", as.integer(id)), operator_name)
    list(success = TRUE, message = "项目更新成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 删除项目（级联删除阶段、工作包、任务）
project_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    # 级联删除：日志 → 任务 → 工作包 → 阶段 → 项目
    dbExecute(con, sprintf("DELETE FROM project_task_logs WHERE task_id IN (SELECT id FROM project_tasks WHERE project_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM project_tasks WHERE project_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM project_work_packages WHERE project_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM project_phases WHERE project_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM projects WHERE id = %d", id))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除项目", sprintf("项目ID: %d", id), operator_name)
    list(success = TRUE, message = "项目已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 项目统计
project_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT
      COUNT(*) as total,
      COALESCE(SUM(CASE WHEN status='planning' THEN 1 ELSE 0 END), 0) as planning,
      COALESCE(SUM(CASE WHEN status='active' THEN 1 ELSE 0 END), 0) as active,
      COALESCE(SUM(CASE WHEN status='completed' THEN 1 ELSE 0 END), 0) as completed,
      COALESCE(SUM(CASE WHEN status='suspended' THEN 1 ELSE 0 END), 0) as suspended,
      COALESCE(SUM(CASE WHEN status='closed' THEN 1 ELSE 0 END), 0) as closed
    FROM projects")
  }, error = function(e) {
    data.frame(total=0, planning=0, active=0, completed=0, suspended=0, closed=0)
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 阶段 CRUD
# ================================================================

phase_get_by_project <- function(project_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM project_phases WHERE project_id = %d ORDER BY sort_order, id",
      as.integer(project_id)))
  }, error = function(e) {
    warning(paste("获取阶段失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

phase_add <- function(project_id, name, description, sort_order = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO project_phases (project_id, name, description, sort_order)
       VALUES (%d, '%s', '%s', %d)",
      as.integer(project_id), name, description, as.integer(sort_order)))
    list(success = TRUE, message = "阶段添加成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("添加失败:", e$message))
  }, finally = { db_disconnect(con) })
}

phase_update <- function(id, name, description, status, sort_order) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE project_phases SET name='%s', description='%s', status='%s',
       sort_order=%d, updated_at=CURRENT_TIMESTAMP WHERE id=%d",
      name, description, status, as.integer(sort_order), as.integer(id)))
    list(success = TRUE, message = "阶段更新成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新失败:", e$message))
  }, finally = { db_disconnect(con) })
}

phase_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    dbExecute(con, sprintf("DELETE FROM project_task_logs WHERE task_id IN (SELECT id FROM project_tasks WHERE phase_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM project_tasks WHERE phase_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM project_work_packages WHERE phase_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM project_phases WHERE id = %d", id))
    list(success = TRUE, message = "阶段已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 工作包 CRUD
# ================================================================

wp_get_by_phase <- function(phase_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT wp.*, u.username as assignee_name FROM project_work_packages wp
       LEFT JOIN users u ON wp.assigned_to = u.id
       WHERE wp.phase_id = %d ORDER BY wp.sort_order, wp.id",
      as.integer(phase_id)))
  }, error = function(e) {
    warning(paste("获取工作包失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

wp_get_by_project <- function(project_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT wp.*, ph.name as phase_name, u.username as assignee_name
       FROM project_work_packages wp
       LEFT JOIN project_phases ph ON wp.phase_id = ph.id
       LEFT JOIN users u ON wp.assigned_to = u.id
       WHERE wp.project_id = %d ORDER BY ph.sort_order, wp.sort_order, wp.id",
      as.integer(project_id)))
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

wp_add <- function(phase_id, project_id, name, description, assigned_to = NULL, sort_order = 0) {
  con <- db_connect()
  tryCatch({
    if (is.null(assigned_to) || assigned_to == "") {
      dbExecute(con, sprintf(
        "INSERT INTO project_work_packages (phase_id, project_id, name, description, sort_order)
         VALUES (%d, %d, '%s', '%s', %d)",
        as.integer(phase_id), as.integer(project_id), name, description, as.integer(sort_order)))
    } else {
      dbExecute(con, sprintf(
        "INSERT INTO project_work_packages (phase_id, project_id, name, description, assigned_to, sort_order)
         VALUES (%d, %d, '%s', '%s', %d, %d)",
        as.integer(phase_id), as.integer(project_id), name, description, as.integer(assigned_to), as.integer(sort_order)))
    }
    list(success = TRUE, message = "工作包添加成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("添加失败:", e$message))
  }, finally = { db_disconnect(con) })
}

wp_update <- function(id, name, description, status, assigned_to = NULL) {
  con <- db_connect()
  tryCatch({
    if (is.null(assigned_to) || assigned_to == "") {
      dbExecute(con, sprintf(
        "UPDATE project_work_packages SET name='%s', description='%s', status='%s',
         assigned_to=NULL, updated_at=CURRENT_TIMESTAMP WHERE id=%d",
        name, description, status, as.integer(id)))
    } else {
      dbExecute(con, sprintf(
        "UPDATE project_work_packages SET name='%s', description='%s', status='%s',
         assigned_to=%d, updated_at=CURRENT_TIMESTAMP WHERE id=%d",
        name, description, status, as.integer(assigned_to), as.integer(id)))
    }
    list(success = TRUE, message = "工作包更新成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新失败:", e$message))
  }, finally = { db_disconnect(con) })
}

wp_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    dbExecute(con, sprintf("DELETE FROM project_task_logs WHERE task_id IN (SELECT id FROM project_tasks WHERE work_package_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM project_tasks WHERE work_package_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM project_work_packages WHERE id = %d", id))
    list(success = TRUE, message = "工作包已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 任务 CRUD
# ================================================================

task_get_by_wp <- function(work_package_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT t.*, u.username as assignee_name, u2.username as creator_name
       FROM project_tasks t
       LEFT JOIN users u ON t.assigned_to = u.id
       LEFT JOIN users u2 ON t.created_by = u2.id
       WHERE t.work_package_id = %d ORDER BY t.created_at DESC",
      as.integer(work_package_id)))
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

task_get_by_project <- function(project_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT t.*, u.username as assignee_name, u2.username as creator_name,
              ph.name as phase_name, wp.name as wp_name
       FROM project_tasks t
       LEFT JOIN users u ON t.assigned_to = u.id
       LEFT JOIN users u2 ON t.created_by = u2.id
       LEFT JOIN project_phases ph ON t.phase_id = ph.id
       LEFT JOIN project_work_packages wp ON t.work_package_id = wp.id
       WHERE t.project_id = %d ORDER BY t.created_at DESC",
      as.integer(project_id)))
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

task_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT t.*, u.username as assignee_name, u2.username as creator_name,
              p.name as project_name, p.project_no,
              ph.name as phase_name, wp.name as wp_name
       FROM project_tasks t
       LEFT JOIN users u ON t.assigned_to = u.id
       LEFT JOIN users u2 ON t.created_by = u2.id
       LEFT JOIN projects p ON t.project_id = p.id
       LEFT JOIN project_phases ph ON t.phase_id = ph.id
       LEFT JOIN project_work_packages wp ON t.work_package_id = wp.id
       WHERE t.id = %d", as.integer(id)))
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 生成任务编号 TSK + YYYYMMDD + 3位流水号
task_generate_number <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("TSK", today)
    result <- dbGetQuery(con, sprintf(
      "SELECT task_no FROM project_tasks WHERE task_no LIKE '%s%%' ORDER BY task_no DESC LIMIT 1", prefix))
    if (nrow(result) > 0 && !is.na(result$task_no[1])) {
      new_seq <- as.integer(substr(result$task_no[1], nchar(prefix) + 1, nchar(result$task_no[1]))) + 1
    } else {
      new_seq <- 1
    }
    sprintf("%s%03d", prefix, new_seq)
  }, error = function(e) {
    sprintf("TSK%s001", format(Sys.Date(), "%Y%m%d"))
  }, finally = { db_disconnect(con) })
}

task_add <- function(work_package_id, phase_id, project_id, name, description,
                     priority = "中", assigned_to = NULL, due_date = NULL, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    task_no <- task_generate_number()
    assign_sql <- ifelse(is.null(assigned_to) || assigned_to == "", "NULL", as.character(as.integer(assigned_to)))
    due_sql <- ifelse(is.null(due_date) || due_date == "", "NULL", sprintf("'%s'", due_date))
    dbExecute(con, sprintf(
      "INSERT INTO project_tasks (work_package_id, phase_id, project_id, task_no, name, description, priority, assigned_to, due_date, created_by)
       VALUES (%d, %d, %d, '%s', '%s', '%s', '%s', %s, %s, %d)",
      as.integer(work_package_id), as.integer(phase_id), as.integer(project_id),
      task_no, name, description, priority, assign_sql, due_sql, user_id))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建任务", name, operator_name, sprintf("任务编号: %s", task_no))
    list(success = TRUE, message = sprintf("任务创建成功，编号: %s", task_no))
  }, error = function(e) {
    list(success = FALSE, message = paste("创建失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 切换任务收藏状态
task_toggle_favorite <- function(id) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    current <- dbGetQuery(con, sprintf("SELECT is_favorite FROM project_tasks WHERE id = %d", id))
    new_val <- if (nrow(current) > 0 && !is.na(current$is_favorite[1]) && current$is_favorite[1] == 1) 0 else 1
    dbExecute(con, sprintf("UPDATE project_tasks SET is_favorite = %d, updated_at = CURRENT_TIMESTAMP WHERE id = %d", new_val, id))
    list(success = TRUE, message = ifelse(new_val == 1, "已收藏", "已取消收藏"), is_favorite = new_val)
  }, error = function(e) {
    list(success = FALSE, message = paste("操作失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 设置任务重要性（0~5红旗）
task_set_importance <- function(id, importance) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    importance <- max(0, min(5, as.integer(importance)))
    dbExecute(con, sprintf("UPDATE project_tasks SET importance = %d, updated_at = CURRENT_TIMESTAMP WHERE id = %d", importance, id))
    list(success = TRUE, message = sprintf("重要性已设为 %d", importance))
  }, error = function(e) {
    list(success = FALSE, message = paste("操作失败:", e$message))
  }, finally = { db_disconnect(con) })
}

task_update_status <- function(id, new_status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    old <- dbGetQuery(con, sprintf("SELECT status FROM project_tasks WHERE id = %d", id))
    old_status <- if (nrow(old) > 0) old$status[1] else "unknown"
    completed_sql <- if (new_status == "completed") ", completed_at=CURRENT_TIMESTAMP" else ""
    dbExecute(con, sprintf(
      "UPDATE project_tasks SET status='%s'%s, updated_at=CURRENT_TIMESTAMP WHERE id=%d",
      new_status, completed_sql, id))
    # 自动记录状态变更日志
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    status_cn <- function(s) switch(s, pending="待处理", in_progress="进行中", completed="已完成", blocked="已阻塞", s)
    dbExecute(con, sprintf(
      "INSERT INTO project_task_logs (task_id, log_type, content, status_before, status_after, created_by)
       VALUES (%d, 'status_change', '状态从 [%s] 变更为 [%s]', '%s', '%s', %d)",
      id, status_cn(old_status), status_cn(new_status), old_status, new_status, user_id))
    list(success = TRUE, message = "任务状态已更新")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新失败:", e$message))
  }, finally = { db_disconnect(con) })
}

task_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    dbExecute(con, sprintf("DELETE FROM project_task_logs WHERE task_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM project_tasks WHERE id = %d", id))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除任务", sprintf("任务ID: %d", id), operator_name)
    list(success = TRUE, message = "任务已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 任务转为工单
task_convert_to_work_order <- function(task_id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    task_id <- as.integer(task_id)
    task <- dbGetQuery(con, sprintf(
      "SELECT t.*, p.name as project_name, ph.name as phase_name, wp.name as wp_name
       FROM project_tasks t
       LEFT JOIN projects p ON t.project_id = p.id
       LEFT JOIN project_phases ph ON t.phase_id = ph.id
       LEFT JOIN project_work_packages wp ON t.work_package_id = wp.id
       WHERE t.id = %d", task_id))
    if (nrow(task) == 0) return(list(success = FALSE, message = "任务不存在"))
    if (!is.na(task$work_order_id[1])) return(list(success = FALSE, message = "该任务已关联工单"))

    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    title <- sprintf("[项目任务] %s", task$name[1])
    desc <- sprintf("来源项目: %s\n阶段: %s\n工作包: %s\n\n%s",
                    ifelse(is.na(task$project_name[1]), "", task$project_name[1]),
                    ifelse(is.na(task$phase_name[1]), "", task$phase_name[1]),
                    ifelse(is.na(task$wp_name[1]), "", task$wp_name[1]),
                    ifelse(is.na(task$description[1]), "", task$description[1]))
    order_no <- work_order_generate_number()
    dbExecute(con, sprintf(
      "INSERT INTO work_orders (order_no, title, description, priority, status, category, created_by, created_at)
       VALUES ('%s', '%s', '%s', '%s', 'pending', '项目任务', %d, CURRENT_TIMESTAMP)",
      order_no, title, desc, task$priority[1], user_id))
    wo_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    dbExecute(con, sprintf("UPDATE project_tasks SET work_order_id=%d, updated_at=CURRENT_TIMESTAMP WHERE id=%d", wo_id, task_id))

    # 记录日志
    dbExecute(con, sprintf(
      "INSERT INTO project_task_logs (task_id, log_type, content, created_by)
       VALUES (%d, 'status_change', '任务已转为工单 %s', %d)", task_id, order_no, user_id))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("任务转工单", sprintf("任务ID:%d → 工单:%s", task_id, order_no), operator_name)

    list(success = TRUE, message = sprintf("已创建工单 %s", order_no), work_order_id = wo_id)
  }, error = function(e) {
    list(success = FALSE, message = paste("转换失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 任务执行反馈日志 CRUD
# ================================================================

task_log_add <- function(task_id, log_type, content, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    dbExecute(con, sprintf(
      "INSERT INTO project_task_logs (task_id, log_type, content, created_by)
       VALUES (%d, '%s', '%s', %d)",
      as.integer(task_id), log_type, content, user_id))
    list(success = TRUE, message = "反馈记录已添加")
  }, error = function(e) {
    list(success = FALSE, message = paste("添加失败:", e$message))
  }, finally = { db_disconnect(con) })
}

task_log_get_by_task <- function(task_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT l.*, u.username as creator_name FROM project_task_logs l
       LEFT JOIN users u ON l.created_by = u.id
       WHERE l.task_id = %d ORDER BY l.created_at DESC",
      as.integer(task_id)))
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 获取可分配用户列表（复用工单模块逻辑）
project_get_assignable_users <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT id, username, display_name, role FROM users WHERE active = 1 ORDER BY username")
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 配置选项 CRUD
# ================================================================

# Get options by category
config_option_get <- function(category) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM config_options WHERE category = '%s' AND active = 1 ORDER BY sort_order, id",
      category))
  }, error = function(e) { data.frame() }, finally = { db_disconnect(con) })
}

# Get choices for selectInput (returns named vector: label=value)
config_option_choices <- function(category, include_all = FALSE) {
  opts <- config_option_get(category)
  if (nrow(opts) == 0) return(c())
  result <- setNames(opts$option_value, opts$option_label)
  if (include_all) result <- c("全部" = "all", result)
  result
}

# Get label for a value
config_option_label <- function(category, value) {
  opts <- config_option_get(category)
  if (nrow(opts) == 0) return(value)
  match <- opts[opts$option_value == value, ]
  if (nrow(match) > 0) match$option_label[1] else value
}

# Get color for a value
config_option_color <- function(category, value) {
  opts <- config_option_get(category)
  if (nrow(opts) == 0) return("#999")
  match <- opts[opts$option_value == value, ]
  if (nrow(match) > 0 && !is.na(match$color[1]) && match$color[1] != "") match$color[1] else "#999"
}

# Get default value for a category
config_option_default <- function(category) {
  opts <- config_option_get(category)
  defaults <- opts[opts$is_default == 1, ]
  if (nrow(defaults) > 0) defaults$option_value[1] else if (nrow(opts) > 0) opts$option_value[1] else ""
}

# Add option
config_option_add <- function(category, option_value, option_label, color = "", sort_order = 0, is_default = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default)
       VALUES ('%s', '%s', '%s', '%s', %d, %d)",
      category, option_value, option_label, color, as.integer(sort_order), as.integer(is_default)))
    list(success = TRUE, message = "配置项添加成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("添加失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# Update option
config_option_update <- function(id, option_value, option_label, color = "", sort_order = 0, is_default = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE config_options SET option_value='%s', option_label='%s', color='%s', sort_order=%d, is_default=%d WHERE id=%d",
      option_value, option_label, color, as.integer(sort_order), as.integer(is_default), as.integer(id)))
    list(success = TRUE, message = "配置项更新成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# Delete (soft: set active=0)
config_option_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE config_options SET active = 0 WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "配置项已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# Get all categories
config_option_categories <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT DISTINCT category FROM config_options WHERE active = 1 ORDER BY category")$category
  }, error = function(e) { character(0) }, finally = { db_disconnect(con) })
}

# ================================================================
# 工单配置选项初始化（如果不存在默认选项）
# ================================================================

# 初始化工单相关配置选项
init_work_order_config_options <- function() {
  con <- db_connect()
  tryCatch({
    # 检查是否已有工单配置
    existing <- dbGetQuery(con, "SELECT DISTINCT category FROM config_options WHERE category LIKE 'work_order_%'")$category
    
    # 工单状态选项
    if (!"work_order_status" %in% existing) {
      statuses <- list(
        list(value = "pending", label = "待处理", color = "#f0ad4e", sort = 1, is_default = 1),
        list(value = "assigned", label = "已派发", color = "#5bc0de", sort = 2, is_default = 0),
        list(value = "processing", label = "处理中", color = "#ff9800", sort = 3, is_default = 0),
        list(value = "completed", label = "已完成", color = "#5cb85c", sort = 4, is_default = 0),
        list(value = "closed", label = "已关闭", color = "#d9534f", sort = 5, is_default = 0)
      )
      for (s in statuses) {
        dbExecute(con, sprintf(
          "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default)
           VALUES ('work_order_status', '%s', '%s', '%s', %d, %d)",
          s$value, s$label, s$color, s$sort, s$is_default))
      }
    }
    
    # 工单优先级选项
    if (!"work_order_priority" %in% existing) {
      priorities <- list(
        list(value = "低", label = "低", color = "#5cb85c", sort = 1, is_default = 0),
        list(value = "中", label = "中", color = "#f0ad4e", sort = 2, is_default = 1),
        list(value = "高", label = "高", color = "#ff9800", sort = 3, is_default = 0),
        list(value = "紧急", label = "紧急", color = "#d9534f", sort = 4, is_default = 0)
      )
      for (p in priorities) {
        dbExecute(con, sprintf(
          "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default)
           VALUES ('work_order_priority', '%s', '%s', '%s', %d, %d)",
          p$value, p$label, p$color, p$sort, p$is_default))
      }
    }
    
    # 工单分类选项
    if (!"work_order_category" %in% existing) {
      categories <- list(
        list(value = "一般", label = "一般", color = "#5bc0de", sort = 1, is_default = 1),
        list(value = "硬件故障", label = "硬件故障", color = "#d9534f", sort = 2, is_default = 0),
        list(value = "软件故障", label = "软件故障", color = "#ff9800", sort = 3, is_default = 0),
        list(value = "网络问题", label = "网络问题", color = "#5f9ea0", sort = 4, is_default = 0),
        list(value = "系统维护", label = "系统维护", color = "#6a5acd", sort = 5, is_default = 0),
        list(value = "账号权限", label = "账号权限", color = "#9370db", sort = 6, is_default = 0),
        list(value = "其他", label = "其他", color = "#999", sort = 7, is_default = 0)
      )
      for (c in categories) {
        dbExecute(con, sprintf(
          "INSERT INTO config_options (category, option_value, option_label, color, sort_order, is_default)
           VALUES ('work_order_category', '%s', '%s', '%s', %d, %d)",
          c$value, c$label, c$color, c$sort, c$is_default))
      }
    }
    
    TRUE
  }, error = function(e) {
    warning(paste("初始化工单配置选项失败:", e$message))
    FALSE
  }, finally = { db_disconnect(con) })
}

# 获取工单状态选项（带"全部"选项）
work_order_status_choices <- function(include_all = TRUE) {
  config_option_choices("work_order_status", include_all = include_all)
}

# 获取工单状态颜色
work_order_status_color <- function(status) {
  config_option_color("work_order_status", status)
}

# 获取工单状态中文名
work_order_status_label <- function(status) {
  config_option_label("work_order_status", status)
}

# ================================================================
# 全局查询函数（跨项目视图）
# ================================================================

# Get ALL phases across all projects (with project name)
phase_get_all <- function(status_filter = "all") {
  con <- db_connect()
  tryCatch({
    query <- "SELECT ph.*, p.name as project_name, p.project_no
              FROM project_phases ph
              LEFT JOIN projects p ON ph.project_id = p.id
              WHERE 1=1"
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- paste(query, sprintf("AND ph.status = '%s'", status_filter))
    }
    query <- paste(query, "ORDER BY p.name, ph.sort_order, ph.id")
    dbGetQuery(con, query)
  }, error = function(e) { data.frame() }, finally = { db_disconnect(con) })
}

# Get ALL tasks across all projects (with project/phase/WP names)
task_get_all_global <- function(status_filter = "all", priority_filter = "all") {
  con <- db_connect()
  tryCatch({
    query <- "SELECT t.*, u.username as assignee_name, u2.username as creator_name,
              p.name as project_name, p.project_no,
              ph.name as phase_name, wp.name as wp_name
              FROM project_tasks t
              LEFT JOIN users u ON t.assigned_to = u.id
              LEFT JOIN users u2 ON t.created_by = u2.id
              LEFT JOIN projects p ON t.project_id = p.id
              LEFT JOIN project_phases ph ON t.phase_id = ph.id
              LEFT JOIN project_work_packages wp ON t.work_package_id = wp.id
              WHERE 1=1"
    if (!is.null(status_filter) && status_filter != "" && status_filter != "all") {
      query <- paste(query, sprintf("AND t.status = '%s'", status_filter))
    }
    if (!is.null(priority_filter) && priority_filter != "" && priority_filter != "all") {
      query <- paste(query, sprintf("AND t.priority = '%s'", priority_filter))
    }
    query <- paste(query, "ORDER BY COALESCE(t.importance, 0) DESC, t.created_at DESC")
    dbGetQuery(con, query)
  }, error = function(e) { data.frame() }, finally = { db_disconnect(con) })
}
