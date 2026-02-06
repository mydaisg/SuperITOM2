work_order_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM work_orders ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

work_order_add <- function(title, description, priority, status = "pending", current_user = NULL) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO work_orders (title, description, priority, status, created_by, created_at) VALUES ('%s', '%s', '%s', '%s', %d, CURRENT_TIMESTAMP)",
                   title, description, priority, status, current_user$id[1])
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("创建工单", title, operator_name, sprintf("优先级: %s, 状态: %s", priority, status))
    
    return(list(success = TRUE, message = "工单创建成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("创建失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

work_order_update <- function(id, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("UPDATE work_orders SET status = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", status, id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新工单状态", sprintf("工单ID: %d", id), operator_name, sprintf("新状态: %s", status))
    
    return(list(success = TRUE, message = "工单更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

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
