# 列出所有笔记标题，找元任务
con <- DBI::dbConnect(RSQLite::SQLite(), "D:/GitHub/SuperITOM2/DB/GH_ITOM.db")
DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")

notes <- DBI::dbGetQuery(con, "SELECT id, note_no, title, status, created_at FROM notes ORDER BY id DESC")
print(notes)

DBI::dbDisconnect(con)
