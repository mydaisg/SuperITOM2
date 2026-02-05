
log_user_operation <- function(operation, username, operator, details = "") {
  # 确保Logs目录存在
  logs_dir <- file.path(getwd(), "Logs")
  if (!dir.exists(logs_dir)) {
    dir.create(logs_dir, recursive = TRUE)
  }
  
  # 创建日志文件路径
  log_file <- file.path(logs_dir, "user_operations.log")
  
  # 获取当前时间戳
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # 构建日志条目
  log_entry <- sprintf("[%s] 操作: %s, 用户: %s, 操作者: %s", 
                      timestamp, operation, username, operator)
  if (details != "") {
    log_entry <- paste(log_entry, sprintf(", 详情: %s", details))
  }
  
  # 写入日志文件
  tryCatch({
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    warning(sprintf("写入日志失败: %s", e$message))
  })
}

log_user_login <- function(username, success, ip_address = "unknown") {
  operation <- ifelse(success, "用户登录", "登录失败")
  details <- sprintf("IP地址: %s", ip_address)
  log_user_operation(operation, username, "系统", details)
}

log_user_add <- function(username, role, operator) {
  operation <- "添加用户"
  details <- sprintf("角色: %s", role)
  log_user_operation(operation, username, operator, details)
}

log_user_delete <- function(username, operator) {
  operation <- "删除用户"
  log_user_operation(operation, username, operator)
}

log_user_update <- function(username, new_role, operator, password_changed = FALSE) {
  operation <- "更新用户"
  details <- sprintf("新角色: %s", new_role)
  if (password_changed) {
    details <- paste(details, "密码已修改")
  }
  log_user_operation(operation, username, operator, details)
}

log_user_toggle_active <- function(username, new_status, operator) {
  operation <- "切换用户状态"
  status_text <- ifelse(new_status == 1, "启用", "禁用")
  details <- sprintf("新状态: %s", status_text)
  log_user_operation(operation, username, operator, details)
}

log_user_error <- function(operation, error_message, operator = "系统") {
  operation <- paste("操作失败", operation)
  details <- sprintf("错误: %s", error_message)
  log_user_operation(operation, "unknown", operator, details)
}
