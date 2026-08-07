# 查找 NTE20260606002
con <- DBI::dbConnect(RSQLite::SQLite(), "D:/GitHub/SuperITOM2/DB/GH_ITOM.db")
DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")

# 查 NTE20260606002
nte <- DBI::dbGetQuery(con, "SELECT * FROM notes WHERE note_no = 'NTE20260606002'")
if (nrow(nte) > 0) {
  cat("=== Note found ===\n")
  cat("ID:", nte$id[1], "\n")
  cat("Title:", nte$title[1], "\n")
  cat("Content:", nte$content[1], "\n")
  
  comments <- DBI::dbGetQuery(con, sprintf(
    "SELECT id, content, status, created_by, created_at FROM note_comments WHERE note_id = %d ORDER BY id",
    nte$id[1]
  ))
  cat("\n=== All comments ===\n")
  print(comments)
  
  pending <- comments[is.na(comments$status) | comments$status != "completed", ]
  cat("\n=== Pending comments ===\n")
  if (nrow(pending) > 0) {
    print(pending)
  } else {
    cat("None\n")
  }
} else {
  cat("NTE20260606002 not found\n")
  # 尝试搜索 title 包含"优化"的
  opts <- DBI::dbGetQuery(con, "SELECT id, note_no, title FROM notes WHERE title LIKE '%优化%' OR title LIKE '%ITOM%平台%'")
  cat("\nNotes with '优化' or 'ITOM平台':\n")
  print(opts)
}

DBI::dbDisconnect(con)
