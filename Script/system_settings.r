config_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM system_config ORDER BY config_key"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

config_get <- function(config_key) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT * FROM system_config WHERE config_key = '%s'", config_key)
    result <- dbGetQuery(con, query)
    if (nrow(result) > 0) {
      return(result$config_value[1])
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  }, finally = {
    db_disconnect(con)
  })
}

config_add <- function(config_key, config_value, description = "") {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('%s', '%s', '%s')", 
                     config_key, config_value, description)
    dbExecute(con, query)
    return(list(success = TRUE, message = "配置添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

config_update <- function(id, config_key, config_value, description = "") {
  con <- db_connect()
  tryCatch({
    query <- sprintf("UPDATE system_config SET config_key = '%s', config_value = '%s', description = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     config_key, config_value, description, id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "配置更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

config_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("DELETE FROM system_config WHERE id = %d", id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "配置删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
