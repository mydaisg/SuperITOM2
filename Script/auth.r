auth_login <- function(username, password) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT * FROM users WHERE username = '%s' AND password = '%s'", username, password)
    result <- dbGetQuery(con, query)
    if (nrow(result) > 0) {
      # 检查用户是否被禁用
      if (exists('active', result) && result$active[1] == 0) {
        return(list(success = FALSE, message = "您的账号已被禁用，请联系管理员"))
      }
      return(list(success = TRUE, user = result))
    } else {
      return(list(success = FALSE, message = "用户名或密码错误"))
    }
  }, error = function(e) {
    return(list(success = FALSE, message = paste("登录失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

auth_register <- function(username, password, role = "user") {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO users (username, password, role) VALUES ('%s', '%s', '%s')", username, password, role)
    dbExecute(con, query)
    return(list(success = TRUE, message = "注册成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("注册失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

auth_check_user <- function(username) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
    result <- dbGetQuery(con, query)
    return(nrow(result) > 0)
  }, error = function(e) {
    return(FALSE)
  }, finally = {
    db_disconnect(con)
  })
}

auth_logout <- function() {
  return(list(success = TRUE, message = "注销成功"))
}
