info_collector_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM information_collectors ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

info_collector_add <- function(name, type, config, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO information_collectors (name, type, config, created_by, created_at) VALUES ('%s', '%s', '%s', %d, CURRENT_TIMESTAMP)",
                   name, type, config, current_user$id[1])
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("添加收集器", name, operator_name, sprintf("类型: %s", type))
    
    return(list(success = TRUE, message = "收集器添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

info_collector_update <- function(id, config, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("UPDATE information_collectors SET config = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", config, id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("更新收集器配置", sprintf("收集器ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "收集器更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

info_collector_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    id <- as.integer(id)
    query <- sprintf("DELETE FROM information_collectors WHERE id = %d", id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_operation("删除收集器", sprintf("收集器ID: %d", id), operator_name)
    
    return(list(success = TRUE, message = "收集器删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
