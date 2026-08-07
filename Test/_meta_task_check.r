# 检查元任务（SuperITOM2优化记录）的未完成评论
con <- DBI::dbConnect(RSQLite::SQLite(), "D:/GitHub/SuperITOM2/DB/GH_ITOM.db")
DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")

nte <- DBI::dbGetQuery(con, "SELECT id FROM notes WHERE title LIKE '%SuperITOM2优化记录%'")

if (nrow(nte) > 0) {
  cat("Note ID:", nte$id[1], "\n")
  comments <- DBI::dbGetQuery(con, sprintf(
    "SELECT id, content, status, created_by, created_at FROM note_comments WHERE note_id = %d AND (status IS NULL OR status != 'completed') ORDER BY id",
    nte$id[1]
  ))
  if (nrow(comments) > 0) {
    print(comments)
  } else {
    cat("No pending comments found.\n")
  }
} else {
  cat("No meta note found.\n")
}

DBI::dbDisconnect(con)
