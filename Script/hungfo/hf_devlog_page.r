# HungFo 开发日志 — DSH 只读页面生成器
# 用途：从开发库导出开发日志为自包含 HTML，支持搜索 / 日期筛选 / 详情，双击即可打开。
# 用法：Rscript hf_devlog_page.r
# 输出：www/dsh_devlog.html
# 说明：只读数据库，绝不写库。数据库路径沿用 HUNGFO_DB 环境变量，默认开发库。
#       页面模板放在 www/dsh_devlog.tpl.html，用占位符替换，避免 R 字符串转义冲突。

args <- commandArgs(trailingOnly = FALSE)
f <- args[startsWith(args, "--file=")]
base <- if (length(f) > 0) dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/", mustWork = FALSE)) else getwd()
setwd(base)

suppressMessages({ library(DBI); library(RSQLite) })

db_path <- Sys.getenv("HUNGFO_DB")
if (!nzchar(db_path)) db_path <- file.path(base, "DB", "HF_HungFo_DSH.DB")
if (!file.exists(db_path)) stop("找不到数据库: ", db_path)

tpl_file <- file.path(base, "www", "dsh_devlog.tpl.html")
if (!file.exists(tpl_file)) stop("找不到模板: ", tpl_file)

con <- dbConnect(SQLite(), db_path)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

df <- dbGetQuery(con, "
  SELECT id, rec_no, title, payload, status, module_key,
         substr(created_at,1,16) AS created_at
  FROM hf_records
  ORDER BY id DESC")

cat("[devlog] 读取", nrow(df), "条记录\n")

esc <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

rec_list <- lapply(seq_len(nrow(df)), function(i) {
  list(id = df$id[i], no = esc(df$rec_no[i]), title = esc(df$title[i]),
       payload = esc(df$payload[i]), status = esc(df$status[i]),
       module = esc(df$module_key[i]), date = esc(df$created_at[i]))
})

json <- jsonlite::toJSON(rec_list, auto_unbox = TRUE, null = "null")

months <- sort(unique(substr(df$created_at, 1, 7)), decreasing = TRUE)
month_opts <- paste0('<option value="">全部月份</option>',
  paste(sprintf('<option value="%s">%s</option>', months, months), collapse = ""))

subtitle <- sprintf("开发库 %s · 共 %d 条 · 生成时间 %s",
                    basename(db_path), nrow(df), format(Sys.time(), "%Y-%m-%d %H:%M"))

html <- paste(readLines(tpl_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
html <- sub("__SUBTITLE__", subtitle, html, fixed = TRUE)
html <- sub("__MONTHS__", month_opts, html, fixed = TRUE)
html <- sub("__DATA__", json, html, fixed = TRUE)

out_file <- file.path(base, "www", "dsh_devlog.html")
writeLines(html, out_file, useBytes = TRUE)
cat("[devlog] 已生成:", out_file, "\n")
cat("[devlog] 大小:", round(file.size(out_file)/1024), "KB\n")
