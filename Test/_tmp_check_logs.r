source("global.R")
con <- db_connect()
tryCatch({
  cat("project_task_logs 总行数:", dbGetQuery(con, "SELECT COUNT(*) n FROM project_task_logs")$n[1], "\n")
  cat("content 为 NULL 的行数:", dbGetQuery(con, "SELECT COUNT(*) n FROM project_task_logs WHERE content IS NULL")$n[1], "\n")
  cat("content 为空字符串的行数:", dbGetQuery(con, "SELECT COUNT(*) n FROM project_task_logs WHERE content = ''")$n[1], "\n")
  # 看一些 content 样例
  r <- dbGetQuery(con, "SELECT id, log_type, content, task_name, created_at FROM project_task_logs ORDER BY id DESC LIMIT 10")
  print(r)
}, finally = { db_disconnect(con) })
