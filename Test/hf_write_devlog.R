#!/usr/bin/env Rscript
##################
# 将五段式 devlog 写入 PROD 正式库 hf_records（module_key='devlog'），
# 复用与 metatask runner 相同的 【】段规范化逻辑（每段独立成行）。
##################
Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(DBI); library(RSQLite) })

HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
PROD <- file.path(HOME, "DB", "HF_HungFo.DB")
setwd(HOME)
Sys.setenv(HUNGFO_DB = PROD)
source("hf_core.r")

title <- "HungFo 全链改造：模型路由/配置下沉/层级规则+记忆/自动注入/入口统一 + 正式库迁移"
content <- paste(readLines("D:/GitHub/SuperITOM2/Test/hf_devlog_content.txt", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# 规范化：非行首的【xx】段标签前插入换行，确保分段独立成行
content <- gsub("(?<!\\n)(?=【[^】]*】)", "\n", content, perl = TRUE)
content <- trimws(content)

con <- hf_db_connect()
on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
no <- sprintf("HF-DEVLOG-%s", format(Sys.time(), "%Y%m%d%H%M%S"))
dbExecute(con, sprintf(
  "INSERT INTO hf_records (module_key, rec_no, title, payload, status) VALUES ('devlog','%s','%s','%s','draft')",
  hf_esc(no), hf_esc(title), hf_esc(content)))
cat("已写入 devlog:", no, " 标题:", title, "\n")
quit(status = 0)
