source("Script/log_user.r")

# ====================================
# 部门管理（树形结构）
# ====================================
dept_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM departments ORDER BY COALESCE(sort_order,0), name")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

dept_get_tree <- function() {
  depts <- dept_get_all()
  if (nrow(depts) == 0) return(data.frame())
  # 构建层级树（添加 depth 和 path 列）
  depts$depth <- 0L; depts$path <- depts$name
  for (i in seq_len(nrow(depts))) {
    d <- depts[i, ]; dep <- 0L
    pid <- d$parent_id[1]
    while (!is.na(pid) && pid > 0) {
      dep <- dep + 1L
      pidx <- which(depts$id == pid)
      if (length(pidx) > 0) pid <- depts$parent_id[pidx[1]] else pid <- NA
    }
    depts$depth[i] <- dep
    # path: 一级/二级
    paths <- d$name[1]
    pid <- d$parent_id[1]
    while (!is.na(pid) && pid > 0) {
      pidx <- which(depts$id == pid)
      if (length(pidx) > 0) { paths <- paste0(depts$name[pidx[1]], " / ", paths); pid <- depts$parent_id[pidx[1]] }
      else pid <- NA
    }
    depts$path[i] <- paths
  }
  depts
}

dept_add <- function(name, parent_id = NA, sort_order = 0, description = "") {
  con <- db_connect()
  tryCatch({
    parent_sql <- if (is.na(parent_id) || parent_id == "" || is.null(parent_id)) "NULL" else as.character(as.integer(parent_id))
    dbExecute(con, sprintf(
      "INSERT INTO departments (name, parent_id, sort_order, description) VALUES ('%s', %s, %d, '%s')",
      gsub("'","''",name), parent_sql, as.integer(sort_order), gsub("'","''",description)))
    list(success = TRUE, message = paste("已添加部门", name))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

dept_update <- function(id, name = NULL, parent_id = NULL, sort_order = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- character(0)
    if (!is.null(name))        sets <- c(sets, sprintf("name='%s'", gsub("'","''",name)))
    if (!is.null(parent_id))   sets <- c(sets, sprintf("parent_id=%s", if(is.na(parent_id)||parent_id=="") "NULL" else as.character(as.integer(parent_id))))
    if (!is.null(sort_order))  sets <- c(sets, sprintf("sort_order=%d", as.integer(sort_order)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无更新内容"))
    dbExecute(con, sprintf("UPDATE departments SET %s, updated_at=datetime('now','localtime') WHERE id=%d",
      paste(sets,collapse=","), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

dept_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    # 检查子部门
    kids <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM departments WHERE parent_id=%d", as.integer(id)))$n[1]
    if (kids > 0) return(list(success = FALSE, message = sprintf("该部门下有 %d 个子部门，请先删除", kids)))
    # 检查人员
    users <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM users WHERE department_id=%d", as.integer(id)))$n[1]
    if (users > 0) return(list(success = FALSE, message = sprintf("该部门下有 %d 名人员，请先移出", users)))
    dbExecute(con, sprintf("DELETE FROM departments WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 获取部门下的人员
dept_users <- function(dept_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT u.*, COALESCE(NULLIF(u.display_name,''), u.username) as display_label
       FROM users u WHERE u.department_id = %d ORDER BY u.username", as.integer(dept_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 设置用户的部门
user_set_department <- function(user_id, dept_id) {
  con <- db_connect()
  tryCatch({
    did <- if (is.na(dept_id) || is.null(dept_id) || dept_id == "") "NULL" else as.character(as.integer(dept_id))
    dbExecute(con, sprintf("UPDATE users SET department_id=%s, updated_at=datetime('now','localtime') WHERE id=%d",
      did, as.integer(user_id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

user_get_all <- function() {
  con <- db_connect()
  tryCatch({
    columns <- dbGetQuery(con, "PRAGMA table_info(users)")
    has_dn <- "display_name" %in% columns$name
    has_did <- "department_id" %in% columns$name
    base <- "SELECT u.id, u.username, u.role, u.active, u.created_at, u.updated_at"
    if (has_dn) base <- paste0(base, ", COALESCE(NULLIF(u.display_name,''), u.username) as display_name")
    else base <- paste0(base, ", u.username as display_name")
    if (has_did) base <- paste0(base, ", u.department_id, d.name as department_name, d2.name as parent_dept_name")
    else base <- paste0(base, ", NULL as department_id, NULL as department_name, NULL as parent_dept_name")
    base <- paste0(base, " FROM users u")
    if (has_did) base <- paste0(base, " LEFT JOIN departments d ON u.department_id = d.id LEFT JOIN departments d2 ON d.parent_id = d2.id")
    result <- dbGetQuery(con, paste0(base, " ORDER BY u.created_at DESC"))
    return(result)
  }, error = function(e) { return(data.frame()) },
  finally = { db_disconnect(con) })
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

user_add <- function(username, password, role = "user", display_name = NULL, department_id = NA, current_user = NULL) {
  permission <- user_check_permission(current_user)
  if (!permission$success) return(permission)
  con <- db_connect()
  tryCatch({
    columns <- dbGetQuery(con, "PRAGMA table_info(users)")
    hdn <- "display_name" %in% columns$name
    hdid <- "department_id" %in% columns$name
    did_str <- if (hdid && !is.na(department_id) && department_id != "") as.character(as.integer(department_id)) else "NULL"
    if (hdn && !is.null(display_name) && display_name != "") {
      if (hdid) query <- sprintf("INSERT INTO users (username, display_name, password, role, active, department_id) VALUES ('%s','%s','%s','%s',1,%s)", username, display_name, password, role, did_str)
      else query <- sprintf("INSERT INTO users (username, display_name, password, role, active) VALUES ('%s','%s','%s','%s',1)", username, display_name, password, role)
    } else {
      if (hdid) query <- sprintf("INSERT INTO users (username, password, role, active, department_id) VALUES ('%s','%s','%s',1,%s)", username, password, role, did_str)
      else query <- sprintf("INSERT INTO users (username, password, role, active) VALUES ('%s','%s','%s',1)", username, password, role)
    }
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_add(username, role, operator_name)
    
    return(list(success = TRUE, message = "用户添加成功"))
  }, error = function(e) {
    # 记录错误日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_error("添加用户", e$message, operator_name)
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
    # 获取被删除用户的用户名
    get_user_query <- sprintf("SELECT username FROM users WHERE id = %d", id)
    user_result <- dbGetQuery(con, get_user_query)
    username_to_delete <- ifelse(nrow(user_result) > 0, user_result$username[1], "unknown")
    
    query <- sprintf("DELETE FROM users WHERE id = %d", id)
    dbExecute(con, query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_delete(username_to_delete, operator_name)
    
    return(list(success = TRUE, message = "用户删除成功"))
  }, error = function(e) {
    # 记录错误日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_error("删除用户", e$message, operator_name)
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

user_update <- function(id, username, role, password = NULL, display_name = NULL, department_id = NULL, current_user = NULL) {
  permission <- user_check_permission(current_user)
  if (!permission$success) return(permission)
  id <- as.integer(id)
  con <- db_connect()
  tryCatch({
    columns <- dbGetQuery(con, "PRAGMA table_info(users)")
    has_dn <- "display_name" %in% columns$name
    has_did <- "department_id" %in% columns$name
    sets <- sprintf("username='%s', role='%s'", gsub("'","''",username), role)
    if (!is.null(password) && password != "") sets <- paste0(sets, sprintf(", password='%s'", gsub("'","''",password)))
    if (has_dn) sets <- paste0(sets, if (!is.null(display_name) && display_name != "") sprintf(", display_name='%s'", gsub("'","''",display_name)) else ", display_name=NULL")
    if (has_did && !is.null(department_id)) sets <- paste0(sets, sprintf(", department_id=%s", if(is.na(department_id)||department_id==""||department_id=="NA") "NULL" else as.character(as.integer(department_id))))
    sets <- paste0(sets, ", updated_at=datetime('now','localtime')")
    dbExecute(con, sprintf("UPDATE users SET %s WHERE id=%d", sets, id))
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_update(username, role, operator_name, !is.null(password) && password != "")
    
    return(list(success = TRUE, message = "用户更新成功"))
  }, error = function(e) {
    # 记录错误日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_error("更新用户", e$message, operator_name)
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
    
    # 获取被更新用户的用户名
    get_user_query <- sprintf("SELECT username FROM users WHERE id = %d", user_id)
    user_result <- dbGetQuery(con, get_user_query)
    username_updated <- ifelse(nrow(user_result) > 0, user_result$username[1], "unknown")
    
    # 记录日志
    operator_name <- current_user$username[1]
    log_user_update(username_updated, "", operator_name, TRUE)
    
    return(list(success = TRUE, message = "密码更新成功"))
  }, error = function(e) {
    # 记录错误日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_error("更新密码", e$message, operator_name)
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
    check_query <- sprintf("SELECT id, username, active FROM users WHERE id = %d", id)
    result <- dbGetQuery(con, check_query)
    
    if (nrow(result) == 0) {
      return(list(success = FALSE, message = "用户不存在"))
    }
    
    # 获取当前状态
    current_status <- result$active[1]
    new_status <- ifelse(current_status == 1, 0, 1)
    
    # 切换用户状态
    toggle_query <- sprintf("UPDATE users SET active = NOT active, updated_at = CURRENT_TIMESTAMP WHERE id = %d", id)
    dbExecute(con, toggle_query)
    
    # 记录日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_toggle_active(result$username[1], new_status, operator_name)
    
    return(list(success = TRUE, message = "用户状态切换成功"))
  }, error = function(e) {
    # 记录错误日志
    operator_name <- ifelse(is.null(current_user), "系统", current_user$username[1])
    log_user_error("切换用户状态", e$message, operator_name)
    return(list(success = FALSE, message = paste("操作失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
