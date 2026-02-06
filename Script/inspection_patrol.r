inspection_patrol_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM inspection_patrols ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

inspection_patrol_add <- function(name, type, schedule, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO inspection_patrols (name, type, schedule, created_by, created_at) VALUES ('%s', '%s', '%s', %d, CURRENT_TIMESTAMP)",
                   name, type, schedule, current_user$id[1])
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建巡检", name, operator_name, sprintf("类型: %s, 计划: %s", type, schedule))
    
    return(list(success = TRUE, message = "巡检创建成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("创建失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

inspection_patrol_update <- function(id, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("UPDATE inspection_patrols SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", status, id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新巡检状态", sprintf("巡检ID: %d", id), operator_name, sprintf("新状态: %s", status))
    
    return(list(success = TRUE, message = "巡检更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

inspection_patrol_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("DELETE FROM inspection_patrols WHERE id = %d", id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除巡检", sprintf("巡检ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "巡检删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
