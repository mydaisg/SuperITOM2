# 检查Git是否可用
# 返回布尔值表示Git是否已安装
is_git_available <- function() {
  tryCatch({
    result <- system2("git", args = c("--version"), stdout = TRUE, stderr = TRUE)
    return(length(result) > 0)
  }, error = function(e) {
    return(FALSE)
  })
}

# 检查当前目录是否是Git仓库
# 返回布尔值表示是否在Git仓库中
is_git_repository <- function() {
  tryCatch({
    result <- system2("git", args = c("rev-parse", "--is-inside-work-tree"), stdout = TRUE, stderr = TRUE)
    return(any(grepl("true", result, ignore.case = TRUE)))
  }, error = function(e) {
    return(FALSE)
  })
}

# 检查远程仓库是否存在
# 参数：remote_name - 远程仓库名称，默认为"origin"
# 返回布尔值表示远程仓库是否存在
has_remote_repository <- function(remote_name = "origin") {
  tryCatch({
    result <- system2("git", args = c("remote", "-v"), stdout = TRUE, stderr = TRUE)
    return(any(grepl(remote_name, result)))
  }, error = function(e) {
    return(FALSE)
  })
}

# 执行Git命令并返回结果
# 参数：
# - args: Git命令参数向量
# - capture_output: 是否捕获输出，默认为TRUE
# 返回：包含status（状态码）和output（输出）的列表
execute_git_command <- function(args, capture_output = TRUE) {
  tryCatch({
    # 打印执行的命令，用于调试
    cat("执行Git命令:", paste("git", paste(args, collapse = " "), "\n"))
    
    # 特殊处理commit命令，因为它包含带空格的消息
    if (length(args) >= 3 && args[1] == "commit" && args[2] == "-m") {
      # 构建完整的命令字符串
      commit_message <- paste(args[3:length(args)], collapse = " ")
      command <- sprintf("git commit -m \"%s\"", commit_message)
      
      # 使用system()执行命令
      output <- system(command, intern = TRUE, ignore.stderr = TRUE)
      
      # 处理返回值
      if (is.logical(output) && !output) {
        # 命令执行失败
        status <- attr(output, "status")
        if (is.null(status)) status <- 1
        return(list(
          status = status,
          output = c("命令执行失败")
        ))
      } else {
        # 命令执行成功
        return(list(
          status = 0,
          output = output
        ))
      }
    } else {
      # 对于其他命令，使用system()
      command <- paste("git", paste(args, collapse = " "))
      output <- system(command, intern = TRUE, ignore.stderr = TRUE)
      
      # 处理返回值
      if (is.logical(output) && !output) {
        # 命令执行失败
        status <- attr(output, "status")
        if (is.null(status)) status <- 1
        return(list(
          status = status,
          output = c("命令执行失败")
        ))
      } else {
        # 命令执行成功
        return(list(
          status = 0,
          output = output
        ))
      }
    }
  }, error = function(e) {
    return(list(
      status = 1,  # 表示执行失败
      output = c("执行命令时出错:", e$message)
    ))
  })
}

# 自动提交所有更改到GitHub
# 参数：
# - commit_message: 提交信息，默认为"Auto commit from R script"
# - branch: 分支名称，默认为"main"
# 返回：操作结果字符串
github_autosubmit <- function(commit_message = "Auto commit from R script", branch = "main") {
  
  output_text <- c()
  output_text <- c(output_text, "开始自动提交代码到 GitHub...\n")
  
  tryCatch({
    # 检查Git是否可用
    if (!is_git_available()) {
      output_text <- c(output_text, "错误: Git 未安装或不可用\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查当前目录是否是Git仓库
    if (!is_git_repository()) {
      output_text <- c(output_text, "错误: 当前目录不是 Git 仓库\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查远程仓库是否存在
    if (!has_remote_repository()) {
      output_text <- c(output_text, "警告: 未找到远程仓库 'origin'\n")
    }
    
    # 检查Git状态
    output_text <- c(output_text, "检查 Git 状态...\n")
    status_result <- execute_git_command(c("status"))
    output_text <- c(output_text, "Git 状态:\n")
    output_text <- c(output_text, paste(status_result$output, collapse = "\n"), "\n\n")
    
    # 检查是否有更改需要提交
    if (any(grepl("nothing to commit", status_result$output, ignore.case = TRUE))) {
      output_text <- c(output_text, "提示: 没有更改需要提交\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 添加所有更改到暂存区
    output_text <- c(output_text, "添加所有更改到暂存区...\n")
    add_result <- execute_git_command(c("add", "."))
    if (add_result$status != 0) {
      output_text <- c(output_text, "错误: 添加更改失败:\n")
      output_text <- c(output_text, paste(add_result$output, collapse = "\n"), "\n")
      return(paste(output_text, collapse = ""))
    }
    output_text <- c(output_text, "已成功添加所有更改到暂存区\n\n")
    
    # 提交更改
    output_text <- c(output_text, "提交更改...\n")
    # 确保commit_message作为单个参数传递
    commit_args <- c("commit", "-m", commit_message)
    output_text <- c(output_text, sprintf("执行命令: git %s\n", paste(commit_args, collapse = " ")))
    commit_result <- execute_git_command(commit_args)
    output_text <- c(output_text, "提交信息:\n")
    output_text <- c(output_text, paste(commit_result$output, collapse = "\n"), "\n\n")
    
    if (commit_result$status != 0) {
      output_text <- c(output_text, "错误: 提交失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 推送到远程仓库
    output_text <- c(output_text, "推送到远程仓库...\n")
    push_result <- execute_git_command(c("push", "origin", branch))
    output_text <- c(output_text, "推送到远程仓库:\n")
    output_text <- c(output_text, paste(push_result$output, collapse = "\n"), "\n\n")
    
    if (push_result$status != 0) {
      output_text <- c(output_text, "错误: 推送失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 所有操作成功完成
    output_text <- c(output_text, "代码已成功提交到 GitHub!\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

# 自动提交指定文件到GitHub
# 参数：
# - files: 要提交的文件路径向量
# - commit_message: 提交信息，默认为"Auto commit specific files"
# - branch: 分支名称，默认为"main"
# 返回：操作结果字符串
github_autosubmit_with_files <- function(files, commit_message = "Auto commit specific files", branch = "main") {
  
  output_text <- c()
  output_text <- c(output_text, "开始自动提交指定文件到 GitHub...\n")
  
  tryCatch({
    # 检查Git是否可用
    if (!is_git_available()) {
      output_text <- c(output_text, "错误: Git 未安装或不可用\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查当前目录是否是Git仓库
    if (!is_git_repository()) {
      output_text <- c(output_text, "错误: 当前目录不是 Git 仓库\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查远程仓库是否存在
    if (!has_remote_repository()) {
      output_text <- c(output_text, "警告: 未找到远程仓库 'origin'\n")
    }
    
    # 添加指定文件
    added_files <- 0
    for (file in files) {
      if (file.exists(file)) {
        add_result <- execute_git_command(c("add", file))
        if (add_result$status == 0) {
          output_text <- c(output_text, sprintf("已添加文件: %s\n", file))
          added_files <- added_files + 1
        } else {
          output_text <- c(output_text, sprintf("错误: 添加文件失败 %s:\n", file))
          output_text <- c(output_text, paste(add_result$output, collapse = "\n"), "\n")
        }
      } else {
        output_text <- c(output_text, sprintf("警告: 文件不存在: %s\n", file))
      }
    }
    
    output_text <- c(output_text, "\n")
    
    # 检查是否有文件被添加
    if (added_files == 0) {
      output_text <- c(output_text, "提示: 没有文件被添加，跳过提交\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 提交更改
    output_text <- c(output_text, "提交更改...\n")
    commit_result <- execute_git_command(c("commit", "-m", commit_message))
    output_text <- c(output_text, "提交信息:\n")
    output_text <- c(output_text, paste(commit_result$output, collapse = "\n"), "\n\n")
    
    if (commit_result$status != 0) {
      output_text <- c(output_text, "错误: 提交失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 推送到远程仓库
    output_text <- c(output_text, "推送到远程仓库...\n")
    push_result <- execute_git_command(c("push", "origin", branch))
    output_text <- c(output_text, "推送到远程仓库:\n")
    output_text <- c(output_text, paste(push_result$output, collapse = "\n"), "\n\n")
    
    if (push_result$status != 0) {
      output_text <- c(output_text, "错误: 推送失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 所有操作成功完成
    output_text <- c(output_text, "代码已成功提交到 GitHub!\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

# 创建新分支并推送到GitHub
# 参数：
# - branch_name: 新分支名称
# 返回：操作结果字符串
github_create_branch <- function(branch_name) {
  
  output_text <- c()
  output_text <- c(output_text, sprintf("创建新分支: %s\n", branch_name))
  
  tryCatch({
    # 检查Git是否可用
    if (!is_git_available()) {
      output_text <- c(output_text, "错误: Git 未安装或不可用\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查当前目录是否是Git仓库
    if (!is_git_repository()) {
      output_text <- c(output_text, "错误: 当前目录不是 Git 仓库\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查远程仓库是否存在
    if (!has_remote_repository()) {
      output_text <- c(output_text, "警告: 未找到远程仓库 'origin'\n")
    }
    
    # 创建并切换到新分支
    output_text <- c(output_text, "创建并切换到新分支...\n")
    checkout_result <- execute_git_command(c("checkout", "-b", branch_name))
    output_text <- c(output_text, paste(checkout_result$output, collapse = "\n"), "\n")
    
    if (checkout_result$status != 0) {
      output_text <- c(output_text, "错误: 创建分支失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 推送到远程仓库
    output_text <- c(output_text, "推送新分支到远程仓库...\n")
    push_result <- execute_git_command(c("push", "-u", "origin", branch_name))
    output_text <- c(output_text, "推送新分支到远程仓库:\n")
    output_text <- c(output_text, paste(push_result$output, collapse = "\n"), "\n\n")
    
    if (push_result$status != 0) {
      output_text <- c(output_text, "错误: 推送分支失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 所有操作成功完成
    output_text <- c(output_text, sprintf("分支 %s 已成功创建并推送到 GitHub!\n", branch_name))
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

# 从GitHub拉取最新代码
# 参数：
# - branch: 分支名称，默认为"main"
# 返回：操作结果字符串
github_pull <- function(branch = "main") {
  
  output_text <- c()
  output_text <- c(output_text, sprintf("从 GitHub 拉取最新代码 (分支: %s)...\n", branch))
  
  tryCatch({
    # 检查Git是否可用
    if (!is_git_available()) {
      output_text <- c(output_text, "错误: Git 未安装或不可用\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查当前目录是否是Git仓库
    if (!is_git_repository()) {
      output_text <- c(output_text, "错误: 当前目录不是 Git 仓库\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查远程仓库是否存在
    if (!has_remote_repository()) {
      output_text <- c(output_text, "错误: 未找到远程仓库 'origin'\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 拉取代码
    pull_result <- execute_git_command(c("pull", "origin", branch))
    output_text <- c(output_text, "拉取信息:\n")
    output_text <- c(output_text, paste(pull_result$output, collapse = "\n"), "\n\n")
    
    if (pull_result$status != 0) {
      output_text <- c(output_text, "错误: 拉取代码失败\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 操作成功完成
    output_text <- c(output_text, "代码已成功从 GitHub 拉取!\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

# 检查Git状态
# 返回：操作结果字符串
github_check_status <- function() {
  
  output_text <- c()
  output_text <- c(output_text, "检查 Git 状态...\n\n")
  
  tryCatch({
    # 检查Git是否可用
    if (!is_git_available()) {
      output_text <- c(output_text, "错误: Git 未安装或不可用\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查当前目录是否是Git仓库
    if (!is_git_repository()) {
      output_text <- c(output_text, "错误: 当前目录不是 Git 仓库\n")
      return(paste(output_text, collapse = ""))
    }
    
    # 检查Git状态
    status_result <- execute_git_command(c("status"))
    output_text <- c(output_text, "当前状态:\n")
    output_text <- c(output_text, paste(status_result$output, collapse = "\n"), "\n\n")
    
    # 检查当前分支
    branch_result <- execute_git_command(c("branch"))
    output_text <- c(output_text, "当前分支:\n")
    output_text <- c(output_text, paste(branch_result$output, collapse = "\n"), "\n\n")
    
    # 检查最近提交
    log_result <- execute_git_command(c("log", "--oneline", "-5"))
    output_text <- c(output_text, "最近5次提交:\n")
    output_text <- c(output_text, paste(log_result$output, collapse = "\n"), "\n\n")
    
    # 检查远程仓库
    remote_result <- execute_git_command(c("remote", "-v"))
    output_text <- c(output_text, "远程仓库:\n")
    output_text <- c(output_text, paste(remote_result$output, collapse = "\n"), "\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

# 使用时间戳自动提交到GitHub
# 参数：
# - prefix: 提交信息前缀，默认为"Auto commit"
# 返回：操作结果字符串
github_autosubmit_with_date <- function(prefix = "Auto commit") {
  
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  commit_message <- paste(prefix, "-", current_time)
  
  output_text <- c()
  output_text <- c(output_text, sprintf("使用时间戳提交信息: %s\n", commit_message))
  
  result <- github_autosubmit(commit_message = commit_message)
  output_text <- c(output_text, result)
  
  return(paste(output_text, collapse = ""))
}

if (FALSE) {
  github_autosubmit(commit_message = "Update Shiny application")
  
  github_autosubmit_with_files(
    files = c("ui.R", "server.R", "global.R"),
    commit_message = "Update core Shiny files"
  )
  
  github_create_branch("feature/new-functionality")
  
  github_pull()
  
  github_check_status()
  
  github_autosubmit_with_date(prefix = "Daily backup")
}
