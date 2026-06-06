con <- DBI::dbConnect(RSQLite::SQLite(), "DB/GH_ITOM.db")
c <- DBI::dbGetQuery(con, "SELECT id, content, status, created_at FROM note_comments WHERE note_id = (SELECT id FROM notes WHERE note_no = 'NTE20260606002') AND (status IS NULL OR status != 'completed') ORDER BY id")
DBI::dbDisconnect(con)
if (nrow(c) == 0) cat("无待执行任务。\n") else print(c)
