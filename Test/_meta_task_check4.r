source("global.R")
con <- DBI::dbConnect(RSQLite::SQLite(), "DB/GH_ITOM.db")
cat("=== NTE20260606002 未完成评论 ===\n")
comments <- DBI::dbGetQuery(con,
  "SELECT c.id, c.content, c.status, c.created_at, 
   COALESCE(NULLIF(u.display_name,''), u.username) as creator 
   FROM note_comments c 
   LEFT JOIN users u ON c.created_by = u.id 
   WHERE c.note_id = 6 
   AND (c.status IS NULL OR c.status != 'completed') 
   ORDER BY c.id ASC")
if (nrow(comments) == 0) {
  cat("没有未完成评论\n")
} else {
  print(comments, row.names = FALSE)
}
DBI::dbDisconnect(con)
