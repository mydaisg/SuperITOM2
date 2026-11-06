#!/usr/bin/env Rscript
# 将五段式修复 devlog 写入 PROD 正式库（复用 runner 的分段规范化）
Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(DBI); library(RSQLite) })
HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
Sys.setenv(HUNGFO_DB = file.path(HOME, "DB", "HF_HungFo.DB"))
setwd(HOME)
source("hf_core.r")
title <- "修复重启后点击登录无反应：元任务页 reactive 自激导致 CPU 100%"
content <- paste(readLines("D:/GitHub/SuperITOM2/Test/hf_devlog_fix.txt",
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")
content <- gsub("(?<!\\n)(?=【[^】]*】)", "\n", content, perl = TRUE)
content <- trimws(content)
con <- hf_db_connect()
on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
no <- sprintf("HF-DEVLOG-%s", format(Sys.time(), "%Y%m%d%H%M%S"))
dbExecute(con, sprintf(
  "INSERT INTO hf_records (module_key, rec_no, title, payload, status) VALUES ('devlog','%s','%s','%s','draft')",
  hf_esc(no), hf_esc(title), hf_esc(content)))
cat("已写入 devlog:", no, "\n")
quit(status = 0)
