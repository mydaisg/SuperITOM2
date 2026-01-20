data_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM itom_data ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

data_add <- function(data_name, data_type, data_value, created_by = 1) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO itom_data (data_name, data_type, data_value, created_by) VALUES ('%s', '%s', '%s', %d)", 
                     data_name, data_type, data_value, created_by)
    dbExecute(con, query)
    return(list(success = TRUE, message = "数据添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

data_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("DELETE FROM itom_data WHERE id = %d", id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "数据删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

data_update <- function(id, data_name, data_type, data_value) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("UPDATE itom_data SET data_name = '%s', data_type = '%s', data_value = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     data_name, data_type, data_value, id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "数据更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
