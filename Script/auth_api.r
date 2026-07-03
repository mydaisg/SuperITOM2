# 命令行认证: Rscript auth_api.r username password
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) { cat("FAIL"); quit() }

username <- args[1]
password <- args[2]

# 强制设置工作目录
setwd("D:/GitHub/SuperITOM2")

source('global.R')
con <- db_connect()
tryCatch({
  r <- dbGetQuery(con, sprintf(
    "SELECT id, username, role FROM users WHERE username='%s' AND password='%s' AND active=1",
    gsub("'","''",username), gsub("'","''",password)))
  if (nrow(r) > 0) {
    cat(paste0("OK:", r$role[1]))
  } else {
    cat("FAIL")
  }
}, error = function(e) cat("FAIL"), finally = { db_disconnect(con) })
