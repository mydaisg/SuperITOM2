# 方案执行模块 - 数据层 v2

# ── 执行项目 CRUD ──
exec_project_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM exec_projects ORDER BY updated_at DESC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

exec_project_add <- function(name, description = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO exec_projects (name, description) VALUES ('%s', %s)",
      gsub("'","''",name),
      if(is.null(description)||description=="") "NULL" else sprintf("'%s'",gsub("'","''",description))))
    list(success = TRUE, message = "已创建")
  }, error = function(e) list(success = FALSE, message = paste("失败:", e$message)),
  finally = { db_disconnect(con) })
}

exec_project_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM exec_tasks WHERE project_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM exec_projects WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("失败:", e$message)),
  finally = { db_disconnect(con) })
}

# ── 执行任务 CRUD ──
exec_task_get_by_project <- function(project_id, task_type = NULL) {
  con <- db_connect()
  tryCatch({
    sql <- sprintf("SELECT * FROM exec_tasks WHERE project_id = %d", as.integer(project_id))
    if (!is.null(task_type)) sql <- paste0(sql, sprintf(" AND task_type = '%s'", task_type))
    dbGetQuery(con, paste0(sql, " ORDER BY sort_order, id"))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

exec_task_get_all_by_project <- function(project_id) {
  exec_task_get_by_project(project_id)
}

exec_task_add <- function(project_id, task_type, parent_id = 0, seq = NULL, module = NULL, content = NULL,
  target = NULL, duration = NULL, method = NULL, plan_date = NULL, responsible = NULL,
  department = NULL, status = NULL, priority = NULL, tester = NULL, test_date = NULL,
  issue_desc = NULL, remark = NULL, sort_order = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(paste0(
      "INSERT INTO exec_tasks (project_id, task_type, parent_id, seq, module, content, target, duration, method, plan_date, responsible, department, status, priority, tester, test_date, issue_desc, remark, sort_order) ",
      "VALUES (%d, '%s', %d, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %d)"),
      as.integer(project_id), task_type, as.integer(parent_id),
      if(is.null(seq)||seq=="") "NULL" else sprintf("'%s'",gsub("'","''",seq)),
      if(is.null(module)||module=="") "NULL" else sprintf("'%s'",gsub("'","''",module)),
      if(is.null(content)||content=="") "NULL" else sprintf("'%s'",gsub("'","''",content)),
      if(is.null(target)||target=="") "NULL" else sprintf("'%s'",gsub("'","''",target)),
      if(is.null(duration)||duration=="") "NULL" else sprintf("'%s'",gsub("'","''",duration)),
      if(is.null(method)||method=="") "NULL" else sprintf("'%s'",gsub("'","''",method)),
      if(is.null(plan_date)||plan_date=="") "NULL" else sprintf("'%s'",gsub("'","''",plan_date)),
      if(is.null(responsible)||responsible=="") "NULL" else sprintf("'%s'",gsub("'","''",responsible)),
      if(is.null(department)||department=="") "NULL" else sprintf("'%s'",gsub("'","''",department)),
      if(is.null(status)||status=="") "NULL" else sprintf("'%s'",gsub("'","''",status)),
      if(is.null(priority)||priority=="") "NULL" else sprintf("'%s'",gsub("'","''",priority)),
      if(is.null(tester)||tester=="") "NULL" else sprintf("'%s'",gsub("'","''",tester)),
      if(is.null(test_date)||test_date=="") "NULL" else sprintf("'%s'",gsub("'","''",test_date)),
      if(is.null(issue_desc)||issue_desc=="") "NULL" else sprintf("'%s'",gsub("'","''",issue_desc)),
      if(is.null(remark)||remark=="") "NULL" else sprintf("'%s'",gsub("'","''",remark)),
      as.integer(sort_order)))
    list(success = TRUE, message = "已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

exec_task_update <- function(id, ...) {
  con <- db_connect()
  tryCatch({
    args <- list(...)
    sets <- c("updated_at = datetime('now','localtime')")
    for (nm in names(args)) {
      val <- args[[nm]]
      if (is.null(val) || val == "") next
      sets <- c(sets, sprintf("%s='%s'", nm, gsub("'","''",as.character(val))))
    }
    if (length(sets) == 1) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE exec_tasks SET %s WHERE id = %d", paste(sets, collapse=", "), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

exec_task_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM exec_tasks WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

exec_task_get_stats <- function(project_id, task_type, status_field = "status") {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM exec_tasks WHERE project_id = %d AND task_type = '%s'",
      as.integer(project_id), task_type))$cnt[1]
    list(total = total)
  }, error = function(e) list(total = 0), finally = { db_disconnect(con) })
}

# ── 执行配置 CRUD ──
exec_config_get <- function(category) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM exec_config WHERE category = '%s' ORDER BY sort_order, id", category))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

exec_config_add <- function(category, value, color = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT OR IGNORE INTO exec_config (category, value, color) VALUES ('%s','%s','%s')",
      category, gsub("'","''",value), color))
    list(success = TRUE, message = "已添加")
  }, error = function(e) list(success = FALSE, message = paste("失败:", e$message)),
  finally = { db_disconnect(con) })
}

exec_config_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM exec_config WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("失败:", e$message)),
  finally = { db_disconnect(con) })
}
