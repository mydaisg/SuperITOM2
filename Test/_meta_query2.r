# 查询元任务未完成评论
con <- DBI::dbConnect(RSQLite::SQLite(), "D:/GitHub/SuperITOM2/DB/GH_ITOM.db")
DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
out <- character()

note <- DBI::dbGetQuery(con, "SELECT id, note_no, title FROM notes WHERE note_no='NTE20260606002'")
nid <- if (nrow(note) > 0) note$id[1] else NA

comments <- DBI::dbGetQuery(con, sprintf(
  "SELECT id, parent_id, content, status, created_by, created_at FROM note_comments WHERE note_id = %d AND (status IS NULL OR status != 'completed') ORDER BY id", nid))

out <- c(out, paste("note_id =", nid), paste("pending =", nrow(comments)), "")
if (nrow(comments) > 0) {
  for (i in seq_len(nrow(comments))) {
    out <- c(out, sprintf("--- id=%s parent_id=%s status=%s created_by=%s created_at=%s ---",
      comments$id[i], comments$parent_id[i], comments$status[i], comments$created_by[i], comments$created_at[i]))
    out <- c(out, comments$content[i], "")
  }
}
DBI::dbDisconnect(con)
writeLines(out, "D:/GitHub/SuperITOM2/Test/_meta_query2.txt", useBytes = TRUE)
cat("DONE\n")
