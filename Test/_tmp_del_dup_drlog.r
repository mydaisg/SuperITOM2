source("global.R")
con <- db_connect()
tryCatch({
  dbExecute(con, "DELETE FROM dev_logs WHERE log_no = 'DL20261008001'")
  r <- dbGetQuery(con, "SELECT log_no, title FROM dev_logs WHERE title LIKE '%日期筛选按键%'")
  print(r)
}, finally = { db_disconnect(con) })
