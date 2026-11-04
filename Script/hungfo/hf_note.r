#!/usr/bin/env Rscript
##################
# HungFo 记事命令行工具（供 CodeBuddy 斜杠命令 /note 快捷记事调用）
# 独立于 Shiny，可直接 Rscript 调用，写入 HungFo 记事看板（hf_project_notes 表）。
#
# 用法：
#   Rscript hf_note.r add "<记事全文>"                          # 单段：首行为标题，全文为正文
#   Rscript hf_note.r add "<标题>" "<正文>"                      # 显式标题 + 正文
#   Rscript hf_note.r add "<标题>" "<正文>" --project <id> --status pending --priority 中
#   Rscript hf_note.r list [--project <id>] [--limit <n>]       # 查看最近记事
#
# 说明：DB 路径与 hf_core.r 一致（HUNGFO_DB > hungfo/DB/HF_HungFo.DB），
#       编号规则与 hf_project_generate_no 保持一致：NOTE + YYYYMMDD + 3位流水。
##################

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressMessages({ library(DBI); library(RSQLite); library(jsonlite) })

# ── HungFo 根目录解析（独立运行 / 父项目内嵌 通用）──
hungfo_home <- function() {
  env <- Sys.getenv("HUNGFO_HOME")
  if (nzchar(env)) return(env)
  if (file.exists(file.path(getwd(), "hf_core.r"))) return(getwd())
  if (file.exists(file.path(getwd(), "Script", "hungfo", "hf_core.r"))) return(file.path(getwd(), "Script", "hungfo"))
  getwd()
}
DB_PATH <- Sys.getenv("HUNGFO_DB", unset = "")
if (!nzchar(DB_PATH)) DB_PATH <- file.path(hungfo_home(), "DB", "HF_HungFo.DB")

db <- function() {
  dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
  con <- dbConnect(SQLite(), DB_PATH)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}
esc <- function(x) gsub("'", "''", as.character(ifelse(is.na(x), "", x)))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
hf_or <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# 幂等建表（与 hf_mod_project.r 的 hf_project_init 保持一致）
ensure_table <- function(con) {
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_project_notes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    note_no TEXT,
    project_id INTEGER,
    title TEXT NOT NULL,
    content TEXT DEFAULT '',
    status TEXT DEFAULT 'pending',
    priority TEXT DEFAULT '中',
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  invisible(TRUE)
}

# 编号：NOTE + YYYYMMDD + 3位流水（与 hf_project_generate_no 逻辑一致）
gen_note_no <- function(con) {
  day <- format(Sys.Date(), "%Y%m%d")
  pat <- paste0("NOTE", day)
  ex <- dbGetQuery(con, sprintf(
    "SELECT note_no FROM hf_project_notes WHERE note_no LIKE '%s%%' ORDER BY note_no DESC LIMIT 1", pat))
  if (nrow(ex) == 0 || is.na(ex$note_no[1]) || !nzchar(ex$note_no[1])) return(paste0(pat, "001"))
  last <- ex$note_no[1]
  seq <- as.integer(substr(last, nchar(pat) + 1, nchar(last))) + 1L
  sprintf("%s%03d", pat, seq)
}

note_add <- function(title, content = "", project_id = NULL, status = "pending", priority = "中") {
  if (is.null(title) || !nzchar(trimws(title))) {
    cat(toJSON(list(success = FALSE, message = "记事标题不能为空")))
    return(invisible(FALSE))
  }
  status <- match.arg(status, c("pending", "in_progress", "done"))
  con <- db(); on.exit(dbDisconnect(con))
  ensure_table(con)
  no <- gen_note_no(con)
  dbExecute(con, sprintf(
    "INSERT INTO hf_project_notes (note_no,project_id,title,content,status,priority)
     VALUES ('%s',%s,'%s','%s','%s','%s')",
    esc(no), if (is.null(project_id) || is.na(project_id) || project_id == "") "NULL" else as.character(as.integer(project_id)),
    esc(title), esc(hf_or(content, "")), esc(status), esc(priority)))
  cat(toJSON(list(success = TRUE, note_no = no, message = paste0("已新增记事：", no)), auto_unbox = TRUE))
  invisible(TRUE)
}

note_list <- function(project_id = NULL, limit = 20) {
  con <- db(); on.exit(dbDisconnect(con))
  ensure_table(con)
  sql <- "SELECT id,note_no,project_id,title,substr(content,1,60) AS content,status,priority,created_at FROM hf_project_notes"
  if (!is.null(project_id) && !is.na(project_id) && project_id != "") {
    sql <- sprintf("%s WHERE project_id=%s", sql, as.character(as.integer(project_id)))
  }
  sql <- sprintf("%s ORDER BY id DESC LIMIT %d", sql, as.integer(limit))
  r <- dbGetQuery(con, sql)
  cat(toJSON(r, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  invisible(TRUE)
}

# 解析 --key value 形式的名值参数
parse_flags <- function(a) {
  meta <- list(); rest <- character(0)
  i <- 1
  while (i <= length(a)) {
    if (grepl("^--", a[i])) {
      key <- sub("^--", "", a[i])
      if (i < length(a) && !grepl("^--", a[i + 1])) {
        meta[[key]] <- a[i + 1]; i <- i + 2
      } else {
        meta[[key]] <- TRUE; i <- i + 1
      }
    } else {
      rest <- c(rest, a[i]); i <- i + 1
    }
  }
  list(meta = meta, rest = rest)
}

# ── 主入口 ──
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("用法: add <标题> <正文> [--project id --status pending --priority 中] | list [--project id --limit n]\n")
  quit(status = 0)
}

cmd <- args[1]
if (cmd == "add" && length(args) >= 2) {
  pf <- parse_flags(args[-1])
  n <- length(pf$rest)
  if (n == 0) {
    cat(toJSON(list(success = FALSE, message = "缺少记事内容")))
  } else if (n == 1) {
    text <- pf$rest[1]
    first_line <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]][1])
    title <- if (nzchar(first_line)) substr(first_line, 1, 40) else "未命名记事"
    note_add(title, text, pf$meta$project, pf$meta$status %||% "pending", pf$meta$priority %||% "中")
  } else {
    note_add(pf$rest[1], paste(pf$rest[-1], collapse = " "),
             pf$meta$project, pf$meta$status %||% "pending", pf$meta$priority %||% "中")
  }
} else if (cmd == "list") {
  pf <- parse_flags(args[-1])
  note_list(pf$meta$project, as.integer(pf$meta$limit %||% 20))
} else {
  cat("未知命令:", cmd, "\n")
}
