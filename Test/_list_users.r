source("global.R")
con <- DBI::dbConnect(RSQLite::SQLite(), "DB/GH_ITOM.db")
users <- DBI::dbGetQuery(con, 
  "SELECT id, username, display_name FROM users WHERE active=1 ORDER BY display_name")
print(users)
DBI::dbDisconnect(con)
