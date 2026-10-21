# ================================================================
# OPPM 一页纸项目管理 - 数据层
# 参照 HD-CBS-业务系统-OPPM 文档：项目基本信息 + 阶段/任务 + 周进度 + RACI 责任矩阵
# 表：oppm_plans / oppm_roles / oppm_tasks / oppm_task_raci / oppm_progress
# ================================================================

# 生成计划编号 OPP + YYYYMMDD + 3位流水
oppm_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("OPP", today)
    result <- dbGetQuery(con, sprintf(
      "SELECT plan_no FROM oppm_plans WHERE plan_no LIKE '%s%%' ORDER BY plan_no DESC LIMIT 1", prefix))
    if (nrow(result) > 0 && !is.na(result$plan_no[1])) {
      new_seq <- as.integer(substr(result$plan_no[1], nchar(prefix) + 1, nchar(result$plan_no[1]))) + 1
    } else {
      new_seq <- 1
    }
    sprintf("%s%03d", prefix, new_seq)
  }, error = function(e) {
    sprintf("OPP%s001", format(Sys.Date(), "%Y%m%d"))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 计划 CRUD
# ================================================================
oppm_plan_get_all <- function(project_id = NULL) {
  con <- db_connect()
  tryCatch({
    query <- "SELECT p.*, pr.name AS project_name FROM oppm_plans p
              LEFT JOIN projects pr ON p.project_id = pr.id"
    if (!is.null(project_id) && length(project_id) > 0) {
      ids <- paste(as.integer(project_id), collapse = ",")
      query <- paste0(query, " WHERE p.project_id IN (", ids, ")")
    }
    query <- paste0(query, " ORDER BY p.created_at DESC")
    dbGetQuery(con, query)
  }, error = function(e) {
    warning(paste("获取OPPM计划列表失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

oppm_plan_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT p.*, pr.name AS project_name FROM oppm_plans p
                      LEFT JOIN projects pr ON p.project_id = pr.id
                      WHERE p.id = %d", as.integer(id))
    dbGetQuery(con, query)
  }, error = function(e) {
    warning(paste("获取OPPM计划详情失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

oppm_plan_add <- function(project_id, project_leader, project_objective, start_date, week_count, version, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    user_id <- ifelse(is.null(current_user), 1, current_user$id[1])
    plan_no <- oppm_generate_no()
    dbExecute(con, sprintf(
      "INSERT INTO oppm_plans (plan_no, project_id, version, project_leader, project_objective, start_date, week_count, status, created_by)
       VALUES ('%s', %d, '%s', '%s', '%s', '%s', %d, 'active', %d)",
      plan_no, as.integer(project_id),
      gsub("'", "''", version),
      gsub("'", "''", project_leader),
      gsub("'", "''", project_objective),
      gsub("'", "''", start_date),
      as.integer(week_count), user_id))
    new_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建OPPM计划", project_leader, operator_name, sprintf("计划编号: %s", plan_no))
    list(success = TRUE, message = sprintf("OPPM 计划创建成功，编号: %s", plan_no), id = new_id)
  }, error = function(e) {
    list(success = FALSE, message = paste("创建失败:", e$message))
  }, finally = { db_disconnect(con) })
}

oppm_plan_update <- function(id, project_leader, project_objective, start_date, week_count, version, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE oppm_plans SET project_leader='%s', project_objective='%s', start_date='%s', week_count=%d, version='%s', updated_at=datetime('now','localtime') WHERE id=%d",
      gsub("'", "''", project_leader),
      gsub("'", "''", project_objective),
      gsub("'", "''", start_date),
      as.integer(week_count),
      gsub("'", "''", version),
      as.integer(id)))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新OPPM计划", sprintf("计划ID: %d", as.integer(id)), operator_name)
    list(success = TRUE, message = "OPPM 计划更新成功")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新失败:", e$message))
  }, finally = { db_disconnect(con) })
}

oppm_plan_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    dbExecute(con, sprintf("DELETE FROM oppm_progress WHERE task_id IN (SELECT id FROM oppm_tasks WHERE plan_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM oppm_task_raci WHERE task_id IN (SELECT id FROM oppm_tasks WHERE plan_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM oppm_tasks WHERE plan_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM oppm_roles WHERE plan_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM oppm_plans WHERE id = %d", id))
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除OPPM计划", sprintf("计划ID: %d", id), operator_name)
    list(success = TRUE, message = "OPPM 计划已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 角色 CRUD（RACI 责任矩阵列）
# ================================================================
oppm_role_get_by_plan <- function(plan_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM oppm_roles WHERE plan_id = %d ORDER BY sort_order, id", as.integer(plan_id)))
  }, error = function(e) {
    warning(paste("获取OPPM角色失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

oppm_role_add <- function(plan_id, role_name) {
  con <- db_connect()
  tryCatch({
    cur_max <- dbGetQuery(con, sprintf("SELECT COALESCE(MAX(sort_order), 0) AS m FROM oppm_roles WHERE plan_id = %d", as.integer(plan_id)))$m[1]
    dbExecute(con, sprintf("INSERT INTO oppm_roles (plan_id, role_name, sort_order) VALUES (%d, '%s', %d)",
      as.integer(plan_id), gsub("'", "''", role_name), cur_max + 1))
    list(success = TRUE, message = "角色已添加")
  }, error = function(e) {
    list(success = FALSE, message = paste("添加角色失败:", e$message))
  }, finally = { db_disconnect(con) })
}

oppm_role_update <- function(id, role_name) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE oppm_roles SET role_name='%s' WHERE id=%d", gsub("'", "''", role_name), as.integer(id)))
    list(success = TRUE, message = "角色已更新")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新角色失败:", e$message))
  }, finally = { db_disconnect(con) })
}

oppm_role_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM oppm_task_raci WHERE role_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM oppm_roles WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "角色已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除角色失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 任务 CRUD（阶段 is_phase=1 / 任务 is_phase=0，树形 parent_id）
# ================================================================
oppm_task_get_by_plan <- function(plan_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM oppm_tasks WHERE plan_id = %d ORDER BY sort_order, id", as.integer(plan_id)))
  }, error = function(e) {
    warning(paste("获取OPPM任务失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

oppm_task_add <- function(plan_id, parent_id, seq_no, task_name, is_phase = 0) {
  con <- db_connect()
  tryCatch({
    cur_max <- dbGetQuery(con, sprintf("SELECT COALESCE(MAX(sort_order), 0) AS m FROM oppm_tasks WHERE plan_id = %d", as.integer(plan_id)))$m[1]
    dbExecute(con, sprintf("INSERT INTO oppm_tasks (plan_id, parent_id, seq_no, task_name, is_phase, sort_order) VALUES (%d, %d, '%s', '%s', %d, %d)",
      as.integer(plan_id), as.integer(parent_id), gsub("'", "''", seq_no), gsub("'", "''", task_name), as.integer(is_phase), cur_max + 1))
    new_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    list(success = TRUE, message = "任务已添加", id = new_id)
  }, error = function(e) {
    list(success = FALSE, message = paste("添加任务失败:", e$message))
  }, finally = { db_disconnect(con) })
}

oppm_task_update <- function(id, seq_no, task_name) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE oppm_tasks SET seq_no='%s', task_name='%s' WHERE id=%d",
      gsub("'", "''", seq_no), gsub("'", "''", task_name), as.integer(id)))
    list(success = TRUE, message = "任务已更新")
  }, error = function(e) {
    list(success = FALSE, message = paste("更新任务失败:", e$message))
  }, finally = { db_disconnect(con) })
}

oppm_task_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    dbExecute(con, sprintf("DELETE FROM oppm_progress WHERE task_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM oppm_task_raci WHERE task_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM oppm_progress WHERE task_id IN (SELECT id FROM oppm_tasks WHERE parent_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM oppm_task_raci WHERE task_id IN (SELECT id FROM oppm_tasks WHERE parent_id = %d)", id))
    dbExecute(con, sprintf("DELETE FROM oppm_tasks WHERE parent_id = %d", id))
    dbExecute(con, sprintf("DELETE FROM oppm_tasks WHERE id = %d", id))
    list(success = TRUE, message = "任务已删除")
  }, error = function(e) {
    list(success = FALSE, message = paste("删除任务失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# RACI 分配（任务 × 角色 → A/R/C/I/S）
# ================================================================
oppm_raci_get_by_plan <- function(plan_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT r.* FROM oppm_task_raci r JOIN oppm_tasks t ON r.task_id = t.id WHERE t.plan_id = %d", as.integer(plan_id)))
  }, error = function(e) {
    warning(paste("获取RACI失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

oppm_raci_set <- function(task_id, role_id, raci_code) {
  con <- db_connect()
  tryCatch({
    raci_code <- toupper(trimws(raci_code))
    if (raci_code == "" || !raci_code %in% c("A", "R", "C", "I", "S")) {
      dbExecute(con, sprintf("DELETE FROM oppm_task_raci WHERE task_id = %d AND role_id = %d", as.integer(task_id), as.integer(role_id)))
    } else {
      dbExecute(con, sprintf("INSERT OR REPLACE INTO oppm_task_raci (task_id, role_id, raci_code) VALUES (%d, %d, '%s')",
        as.integer(task_id), as.integer(role_id), raci_code))
    }
    list(success = TRUE)
  }, error = function(e) {
    list(success = FALSE, message = paste("设置RACI失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# ================================================================
# 进度标记（任务 × 周 → mark 实心/空心/无 × color 绿/黄/红）
# ================================================================
oppm_progress_get_by_plan <- function(plan_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT p.* FROM oppm_progress p JOIN oppm_tasks t ON p.task_id = t.id WHERE t.plan_id = %d", as.integer(plan_id)))
  }, error = function(e) {
    warning(paste("获取进度失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

oppm_progress_set <- function(task_id, week_index, mark, color) {
  con <- db_connect()
  tryCatch({
    if (is.null(mark) || mark == "none") {
      dbExecute(con, sprintf("DELETE FROM oppm_progress WHERE task_id = %d AND week_index = %d", as.integer(task_id), as.integer(week_index)))
    } else {
      dbExecute(con, sprintf("INSERT OR REPLACE INTO oppm_progress (task_id, week_index, mark, color) VALUES (%d, %d, '%s', '%s')",
        as.integer(task_id), as.integer(week_index), mark, color))
    }
    list(success = TRUE)
  }, error = function(e) {
    list(success = FALSE, message = paste("设置进度失败:", e$message))
  }, finally = { db_disconnect(con) })
}
