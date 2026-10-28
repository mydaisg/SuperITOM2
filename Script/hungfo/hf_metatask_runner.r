#!/usr/bin/env Rscript
##################
# HungFo 元任务命令行工具（供 WorkBuddy 定时任务调用）
# 独立于 Shiny，可直接 Rscript 调用，负责元任务队列的状态管理与开发日志写入。
#
# 用法：
#   Rscript hf_metatask_runner.r peek              # 输出到期待执行任务(JSON)到 stdout
#   Rscript hf_metatask_runner.r done <id> "<结果>"  # 标记任务完成(写库+更新镜像)
#   Rscript hf_metatask_runner.r log "<标题>" "<内容>"  # 写一条开发日志(devlog)
#   Rscript hf_metatask_runner.r export            # 重新导出 MetaTasks.json 镜像
#   Rscript hf_metatask_runner.r cron "<expr>"     # 测试 cron 表达式的下一个触发时间
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
in_parent <- file.exists(file.path(getwd(), "Script", "hungfo", "hf_core.r")) &&
             !file.exists(file.path(getwd(), "hf_core.r"))

# 数据库路径：独立运行用 hungfo 目录内 DB/hungfo.db；父项目内嵌沿用 GH_ITOM.db
DB_PATH <- Sys.getenv("HUNGFO_DB", unset = "")
if (!nzchar(DB_PATH)) DB_PATH <- if (in_parent) "DB/GH_ITOM.db" else file.path(hungfo_home(), "DB", "hungfo.db")
META_JSON <- file.path(hungfo_home(), "MetaTasks.json")
SESSIONS_JS <- file.path(hungfo_home(), "www", "hungfo_sessions.js")

db <- function() {
  dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
  con <- dbConnect(SQLite(), DB_PATH)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

esc <- function(x) gsub("'", "''", as.character(ifelse(is.na(x), "", x)))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ── 基础 cron 解析（5 字段：分 时 日 月 周）──
cron_next <- function(cron, from = Sys.time()) {
  if (is.null(cron) || !nzchar(trimws(cron))) return(NULL)
  cron <- trimws(cron)
  f <- strsplit(cron, "\\s+")[[1]]
  if (length(f) != 5) return(NULL)
  parse_field <- function(s, lo, hi) {
    s <- trimws(s)
    if (s == "*") return(lo:hi)
    vals <- integer(0)
    for (part in strsplit(s, ",")[[1]]) {
      if (grepl("^\\*/", part)) {
        step <- as.integer(sub("^\\*/", "", part))
        vals <- c(vals, seq(lo, hi, by = step))
      } else if (grepl("-", part) && !grepl("^\\*", part)) {
        rng <- as.integer(strsplit(part, "-")[[1]])
        vals <- c(vals, seq(rng[1], rng[2], by = 1))
      } else {
        vals <- c(vals, as.integer(part))
      }
    }
    unique(vals[vals >= lo & vals <= hi])
  }
  mins <- parse_field(f[1], 0, 59); hrs <- parse_field(f[2], 0, 23)
  days <- parse_field(f[3], 1, 31); mons <- parse_field(f[4], 1, 12); wds <- parse_field(f[5], 0, 6)
  if (length(mins) == 0 || length(hrs) == 0 || length(days) == 0 || length(mons) == 0 || length(wds) == 0) return(NULL)
  t <- as.POSIXct(trunc(from, "mins")) + 60
  limit <- t + 366 * 24 * 3600
  while (t <= limit) {
    lt <- as.POSIXlt(t)
    if (lt$min %in% mins && lt$hour %in% hrs && lt$mday %in% days && (lt$mon + 1) %in% mons && lt$wday %in% wds) return(t)
    t <- t + 60
  }
  NULL
}

# ── 导出镜像 ──
export <- function() {
  con <- db(); on.exit(dbDisconnect(con))
  q <- dbGetQuery(con, "SELECT * FROM hf_metatask_queue ORDER BY id ASC")
  tasks <- if (nrow(q) == 0) list() else lapply(seq_len(nrow(q)), function(i) list(
    id = q$id[i], title = q$title[i], payload = ifelse(is.na(q$payload[i]), "", q$payload[i]),
    cron = ifelse(is.na(q$cron[i]), "", q$cron[i]), status = ifelse(is.na(q$status[i]), "pending", q$status[i]),
    last_run_at = if (is.na(q$last_run_at[i]) || is.null(q$last_run_at[i])) NULL else q$last_run_at[i],
    next_run_at = if (is.na(q$next_run_at[i]) || is.null(q$next_run_at[i])) NULL else q$next_run_at[i],
    result = ifelse(is.na(q$result[i]), "", q$result[i])))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), tasks = tasks)
  writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), META_JSON, useBytes = TRUE)
  invisible(out)
}

# ── peek：返回到期待执行任务 ──
peek <- function() {
  con <- db(); on.exit(dbDisconnect(con))
  q <- dbGetQuery(con, "SELECT * FROM hf_metatask_queue WHERE status='pending' ORDER BY id ASC")
  now <- Sys.time()
  due <- if (nrow(q) == 0) list() else lapply(seq_len(nrow(q)), function(i) {
    r <- q[i, , drop = FALSE]
    nxt <- r$next_run_at[1]
    due_flag <- if (is.na(nxt) || nxt == "") TRUE else (as.POSIXct(nxt, tz = "Asia/Shanghai") <= now)
    if (due_flag) list(id = r$id[1], title = r$title[1], payload = ifelse(is.na(r$payload[1]), "", r$payload[1]),
                       cron = ifelse(is.na(r$cron[1]), "", r$cron[1])) else NULL
  })
  due <- Filter(Negate(is.null), due)
  cat(toJSON(list(now = format(now, "%Y-%m-%d %H:%M:%S"), due = due), auto_unbox = TRUE, pretty = TRUE))
  invisible(due)
}

# ── done：标记任务完成（可附带会话元数据：--llm/--tin/--tout/--dur/--sid）──
# 用法：done <id> "<result>" [--llm <名>] [--tin <n>] [--tout <n>] [--dur <ms>] [--sid <会话ID>]
mark_done <- function(id, result = "", meta = list()) {
  con <- db(); on.exit(dbDisconnect(con))
  row <- dbGetQuery(con, sprintf("SELECT * FROM hf_metatask_queue WHERE id=%s", id))
  if (nrow(row) == 0) { cat(toJSON(list(success = FALSE, message = "任务不存在"))); return(invisible(FALSE)) }
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cron <- ifelse(is.na(row$cron[1]), "", row$cron[1])
  nxt_str <- NULL
  llm <- meta$llm %||% ""; tin <- as.integer(meta$tin %||% 0); tout <- as.integer(meta$tout %||% 0)
  dur <- as.integer(meta$dur %||% 0); sid <- meta$sid %||% ""
  meta_sql <- sprintf(
    "session_id='%s', llm_name='%s', token_in=%d, token_out=%d, duration_ms=%d, started_at='%s', finished_at='%s'",
    esc(sid), esc(llm), tin, tout, dur, now_str, now_str)
  if (nzchar(trimws(cron))) {
    nxt <- cron_next(cron)
    nxt_str <- if (is.null(nxt)) NULL else format(nxt, "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "UPDATE hf_metatask_queue SET status='pending', last_run_at='%s', next_run_at=%s, result='%s', %s, updated_at=datetime('now','localtime') WHERE id=%s",
      now_str, if (is.null(nxt_str)) "NULL" else sprintf("'%s'", nxt_str), esc(result), meta_sql, id))
  } else {
    dbExecute(con, sprintf(
      "UPDATE hf_metatask_queue SET status='done', last_run_at='%s', next_run_at=NULL, result='%s', %s, updated_at=datetime('now','localtime') WHERE id=%s",
      now_str, esc(result), meta_sql, id))
  }
  # 写执行会话明细
  dbExecute(con, sprintf(
    "INSERT INTO hf_task_sessions (task_id,session_id,title,llm_name,token_in,token_out,duration_ms,status,result,started_at,finished_at)
     VALUES (%s,'%s','%s','%s',%d,%d,%d,'success','%s','%s','%s')",
    as.character(id), esc(sid), esc(row$title[1]), esc(llm), tin, tout, dur, esc(result), now_str, now_str))
  export(); export_sessions()
  cat(toJSON(list(success = TRUE, message = "已更新任务状态", next_run_at = nxt_str)))
  invisible(TRUE)
}

# ── 导出会话明细到 hungfo_sessions.js ──
export_sessions <- function(limit = 200) {
  con <- db(); on.exit(dbDisconnect(con))
  s <- tryCatch(dbGetQuery(con, sprintf("SELECT * FROM hf_task_sessions ORDER BY id DESC LIMIT %d", limit)),
                error = function(e) data.frame())
  # token 汇总
  all_s <- tryCatch(dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COUNT(*) n FROM hf_task_sessions"), error = function(e) data.frame(tin=0,tout=0,n=0))
  day_s <- tryCatch(dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COUNT(*) n FROM hf_task_sessions WHERE date(started_at)=date('now','localtime')"), error = function(e) data.frame(tin=0,tout=0,n=0))
  summary <- list(
    total = list(token_in = all_s$tin[1], token_out = all_s$tout[1], sessions = all_s$n[1]),
    today = list(token_in = day_s$tin[1], token_out = day_s$tout[1], sessions = day_s$n[1]))
  sessions <- if (is.null(s) || nrow(s) == 0) list() else lapply(seq_len(nrow(s)), function(i) list(
    id = s$id[i], task_id = s$task_id[i], session_id = ifelse(is.na(s$session_id[i]), "", s$session_id[i]),
    title = ifelse(is.na(s$title[i]), "", s$title[i]), llm_name = ifelse(is.na(s$llm_name[i]), "", s$llm_name[i]),
    token_in = ifelse(is.na(s$token_in[i]), 0, s$token_in[i]), token_out = ifelse(is.na(s$token_out[i]), 0, s$token_out[i]),
    duration_ms = ifelse(is.na(s$duration_ms[i]), 0, s$duration_ms[i]),
    status = ifelse(is.na(s$status[i]), "success", s$status[i]),
    started_at = if (is.na(s$started_at[i]) || is.null(s$started_at[i])) NULL else s$started_at[i],
    finished_at = if (is.na(s$finished_at[i]) || is.null(s$finished_at[i])) NULL else s$finished_at[i]))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), summary = summary, sessions = sessions)
  dir.create(dirname(SESSIONS_JS), showWarnings = FALSE, recursive = TRUE)
  writeLines(paste0("// 由 HungFo 生成（hf_sessions_export），勿手改\nwindow.HF_SESSIONS = ", toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), ";\n"), SESSIONS_JS, useBytes = TRUE)
  invisible(out)
}

# ── log：写一条开发日志 ──
write_log <- function(title, content) {
  con <- db(); on.exit(dbDisconnect(con))
  no <- sprintf("HF-DEVLOG-%s", format(Sys.time(), "%Y%m%d%H%M%S"))
  dbExecute(con, sprintf(
    "INSERT INTO hf_records (module_key, rec_no, title, payload, status) VALUES ('devlog','%s','%s','%s','draft')",
    esc(no), esc(title), esc(content)))
  cat(toJSON(list(success = TRUE, message = paste0("已写开发日志 ", no))))
  invisible(TRUE)
}

# ── 主入口 ──
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) { cat("用法: peek | done <id> <result> [--llm n --tin n --tout n --dur ms --sid s] | log <title> <content> | export | sessions | cron <expr>\n"); quit(status = 0) }

# 解析 --key value 形式的名值参数（从 args 中提取）
parse_flags <- function(a) {
  meta <- list(); rest <- a
  i <- 1
  while (i <= length(a)) {
    if (grepl("^--", a[i]) && i < length(a)) {
      key <- sub("^--", "", a[i]); meta[[key]] <- a[i + 1]; rest <- rest[rest != a[i] & rest != a[i + 1]]
      i <- i + 2
    } else i <- i + 1
  }
  list(meta = meta, rest = rest)
}

cmd <- args[1]
if (cmd == "peek") {
  peek()
} else if (cmd == "done" && length(args) >= 2) {
  pf <- parse_flags(args[-1])            # 去掉 cmd 本身
  # rest: id + result（result 可能含空格，已被 shell 引号合并）
  if (length(pf$rest) >= 1) {
    id <- as.integer(pf$rest[1])
    result <- if (length(pf$rest) >= 2) paste(pf$rest[-1], collapse = " ") else ""
    mark_done(id, result, pf$meta)
  } else cat(toJSON(list(success = FALSE, message = "缺少任务 id")))
} else if (cmd == "log" && length(args) >= 3) {
  write_log(args[2], paste(args[-(1:2)], collapse = " "))
} else if (cmd == "export") {
  export(); cat(toJSON(list(success = TRUE, message = "镜像已导出")))
} else if (cmd == "sessions") {
  export_sessions(); cat(toJSON(list(success = TRUE, message = "会话明细已导出")))
} else if (cmd == "cron" && length(args) >= 2) {
  nxt <- cron_next(paste(args[-1], collapse = " "))
  cat(if (is.null(nxt)) "NULL" else format(nxt, "%Y-%m-%d %H:%M:%S"))
} else {
  cat("未知命令:", cmd, "\n")
}
