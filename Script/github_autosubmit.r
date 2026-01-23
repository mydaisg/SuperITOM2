github_autosubmit <- function(commit_message = "Auto commit from R script", branch = "main") {
  
  output_text <- c()
  output_text <- c(output_text, "开始自动提交代码到 GitHub...\n")
  
  tryCatch({
    git_status <- system("git status", intern = TRUE, ignore.stderr = TRUE)
    output_text <- c(output_text, "Git 状态:\n")
    output_text <- c(output_text, paste(git_status, collapse = "\n"), "\n\n")
    
    git_add <- system("git add .", intern = TRUE, ignore.stderr = TRUE)
    output_text <- c(output_text, "已添加所有更改到暂存区\n\n")
    
    git_commit <- system2("git", args = c("commit", "-m", commit_message), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, "提交信息:\n")
    output_text <- c(output_text, paste(git_commit, collapse = "\n"), "\n\n")
    
    git_push <- system2("git", args = c("push", "origin", branch), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, "推送到远程仓库:\n")
    output_text <- c(output_text, paste(git_push, collapse = "\n"), "\n\n")
    
    output_text <- c(output_text, "代码已成功提交到 GitHub!\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

github_autosubmit_with_files <- function(files, commit_message = "Auto commit specific files", branch = "main") {
  
  output_text <- c()
  output_text <- c(output_text, "开始自动提交指定文件到 GitHub...\n")
  
  tryCatch({
    for (file in files) {
      if (file.exists(file)) {
        git_add <- system2("git", args = c("add", file), stdout = TRUE, stderr = TRUE)
        output_text <- c(output_text, sprintf("已添加文件: %s\n", file))
      } else {
        output_text <- c(output_text, sprintf("警告: 文件不存在: %s\n", file))
      }
    }
    
    output_text <- c(output_text, "\n")
    
    git_commit <- system2("git", args = c("commit", "-m", commit_message), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, "提交信息:\n")
    output_text <- c(output_text, paste(git_commit, collapse = "\n"), "\n\n")
    
    git_push <- system2("git", args = c("push", "origin", branch), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, "推送到远程仓库:\n")
    output_text <- c(output_text, paste(git_push, collapse = "\n"), "\n\n")
    
    output_text <- c(output_text, "代码已成功提交到 GitHub!\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

github_create_branch <- function(branch_name) {
  
  output_text <- c()
  output_text <- c(output_text, sprintf("创建新分支: %s\n", branch_name))
  
  tryCatch({
    git_checkout <- system2("git", args = c("checkout", "-b", branch_name), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, paste(git_checkout, collapse = "\n"), "\n")
    
    git_push <- system2("git", args = c("push", "-u", "origin", branch_name), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, "推送新分支到远程仓库:\n")
    output_text <- c(output_text, paste(git_push, collapse = "\n"), "\n\n")
    
    output_text <- c(output_text, sprintf("分支 %s 已创建并推送到 GitHub!\n", branch_name))
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

github_pull <- function(branch = "main") {
  
  output_text <- c()
  output_text <- c(output_text, sprintf("从 GitHub 拉取最新代码 (分支: %s)...\n", branch))
  
  tryCatch({
    git_pull <- system2("git", args = c("pull", "origin", branch), stdout = TRUE, stderr = TRUE)
    output_text <- c(output_text, "拉取信息:\n")
    output_text <- c(output_text, paste(git_pull, collapse = "\n"), "\n\n")
    
    output_text <- c(output_text, "代码已成功从 GitHub 拉取!\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

github_check_status <- function() {
  
  output_text <- c()
  output_text <- c(output_text, "检查 Git 状态...\n\n")
  
  tryCatch({
    git_status <- system("git status", intern = TRUE)
    output_text <- c(output_text, "当前状态:\n")
    output_text <- c(output_text, paste(git_status, collapse = "\n"), "\n\n")
    
    git_branch <- system("git branch", intern = TRUE)
    output_text <- c(output_text, "当前分支:\n")
    output_text <- c(output_text, paste(git_branch, collapse = "\n"), "\n\n")
    
    git_log <- system("git log --oneline -5", intern = TRUE)
    output_text <- c(output_text, "最近5次提交:\n")
    output_text <- c(output_text, paste(git_log, collapse = "\n"), "\n\n")
  }, error = function(e) {
    output_text <- c(output_text, "错误: ", e$message, "\n")
  })
  
  return(paste(output_text, collapse = ""))
}

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
