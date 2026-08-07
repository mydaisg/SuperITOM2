# 标记元任务评论为已完成
con <- DBI::dbConnect(RSQLite::SQLite(), "D:/GitHub/SuperITOM2/DB/GH_ITOM.db")
DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")

# 标记评论 ID=1431 为 completed
DBI::dbExecute(con, "UPDATE note_comments SET status = 'completed' WHERE id = 1431")
cat("Marked comment 1431 as completed\n")

# 验证
result <- DBI::dbGetQuery(con, "SELECT id, status FROM note_comments WHERE id = 1431")
print(result)

DBI::dbDisconnect(con)
