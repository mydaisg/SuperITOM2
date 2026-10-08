source("global.R")
con <- db_connect()
tryCatch({
  r <- dbGetQuery(con, "SELECT log_no, title FROM dev_logs WHERE title LIKE '%本年度%' ORDER BY id DESC")
  print(r)
}, finally = { db_disconnect(con) })
