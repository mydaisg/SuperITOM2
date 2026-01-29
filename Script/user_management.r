user_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT id, username, role, active, created_at, updated_at FROM users ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

user_check_permission <- function(current_user, required_role = "admin") {
  if (is.null(current_user) || nrow(current_user) <= 0) {
    return(list(success = FALSE, message = "未登录或用户信息无效"))
  }
  
  if (current_user$role[1] != required_role) {
    return(list(success = FALSE, message = sprintf("需要%s权限", required_role)))
  }
  
  return(list(success = TRUE))
}

user_add <- function(username, password, role = "user", current_user = NULL) {
  permission <- user_check_permission(current_user)
  if (!permission$success) {
    return(permission)
  }
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO users (username, password, role, active) VALUES ('%s', '%s', '%s', 1)", 
                     username, password, role)
    dbExecute(con, query)
    return(list(success = TRUE, message = "用户添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_delete <- function(id, current_user = NULL) {
  permission <- user_check_permission(current_user)
  if (!permission$success) {
    return(permission)
  }
  
  # 将id转换为数字类型
  id <- as.integer(id)
  
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

user_update <- function(id, username, role, password = NULL, current_user = NULL) {
  permission <- user_check_permission(current_user)
  if (!permission$success) {
    return(permission)
  }
  
  # 将id转换为数字类型
  id <- as.integer(id)
  
  con <- db_connect()
  tryCatch({
    if (!is.null(password) && password != "") {
      query <- sprintf("UPDATE users SET username = '%s', role = '%s', password = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                       username, role, password, id)
    } else {
      query <- sprintf("UPDATE users SET username = '%s', role = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                       username, role, id)
    }
    dbExecute(con, query)
    return(list(success = TRUE, message = "用户更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_update_password <- function(id, password, current_user = NULL) {
  if (is.null(current_user) || nrow(current_user) <= 0) {
    return(list(success = FALSE, message = "未登录或用户信息无效"))
  }
  
  # 将id转换为数字类型
  user_id <- as.integer(id)
  
  # 检查是否是管理员或者用户自己
  if (current_user$role[1] != "admin" && current_user$id[1] != user_id) {
    return(list(success = FALSE, message = "您只能更新自己的密码"))
  }
  
  con <- db_connect()
  tryCatch({
    query <- sprintf("UPDATE users SET password = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     password, user_id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "密码更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_toggle_active <- function(id, current_user = NULL) {
  permission <- user_check_permission(current_user)
  if (!permission$success) {
    return(permission)
  }
  
  # 将id转换为数字类型
  id <- as.integer(id)
  
  con <- db_connect()
  tryCatch({
    # 检查用户是否存在
    check_query <- sprintf("SELECT id FROM users WHERE id = %d", id)
    result <- dbGetQuery(con, check_query)
    
    if (nrow(result) == 0) {
      return(list(success = FALSE, message = "用户不存在"))
    }
    
    # 切换用户状态
    toggle_query <- sprintf("UPDATE users SET active = NOT active, updated_at = CURRENT_TIMESTAMP WHERE id = %d", id)
    dbExecute(con, toggle_query)
    
    return(list(success = TRUE, message = "用户状态切换成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("操作失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
