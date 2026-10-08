source("global.R")
con <- db_connect()
tryCatch({
  r <- dbGetQuery(con, "SELECT log_no, title FROM dev_logs WHERE title LIKE '%日期筛选按键%'")
  print(r)
}, finally = { db_disconnect(con) })
