user_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT id, username, role, created_at, updated_at FROM users ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

user_add <- function(username, password, role = "user") {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO users (username, password, role) VALUES ('%s', '%s', '%s')", 
                     username, password, role)
    dbExecute(con, query)
    return(list(success = TRUE, message = "用户添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("DELETE FROM users WHERE id = %d", id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "用户删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_update <- function(id, username, role) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("UPDATE users SET username = '%s', role = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     username, role, id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "用户更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_update_password <- function(id, password) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("UPDATE users SET password = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     password, id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "密码更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
