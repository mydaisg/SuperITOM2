#!/usr/bin/env Rscript
##################
# HungFo 元任务命令行工具（供 CodeBuddy 定时任务调用）
# 独立于 Shiny，可直接 Rscript 调用，负责元任务队列的状态管理与开发日志写入。
#
# 用法：
#   Rscript hf_metatask_runner.r peek              # 输出到期待执行任务(JSON)到 stdout
#   Rscript hf_metatask_runner.r done <id> "<结果>"  # 标记任务完成(写库+更新镜像)
#   Rscript hf_metatask_runner.r log "<标题>" "<内容>"  # 写一条开发日志(devlog)
#   Rscript hf_metatask_runner.r sessions          # 重新导出 hungfo_sessions.js
#   Rscript hf_metatask_runner.r usage             # 采集 CodeBuddy 自动任务运行统计 + 会话清单并导出
#   Rscript hf_metatask_runner.r token <cid> <total> <input> <output> <cacheHit> <cacheMiss> <cacheWrite> <thinking> <response>  # 手动录入某会话 Token 明细
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
# 数据库路径：一律使用 HungFo 独立库 DB/HF_HungFo.DB（与父项目 GH_ITOM.db 解耦）
# 可用环境变量 HUNGFO_DB 覆盖
DB_PATH <- Sys.getenv("HUNGFO_DB", unset = "")
if (!nzchar(DB_PATH)) DB_PATH <- file.path(hungfo_home(), "DB", "HF_HungFo.DB")
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

# 确保 agent 列存在（旧库兼容：列不存在则 ALTER 追加）
ensure_agent_cols <- function(con) {
  for (tbl in c("hf_metatask_queue", "hf_task_sessions")) {
    cols <- tryCatch(dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tbl)), error = function(e) data.frame())
    if (nrow(cols) > 0 && !("agent" %in% cols$name)) {
      dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN agent TEXT", tbl))
    }
  }
  invisible(TRUE)
}

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
  ensure_agent_cols(con)
  q <- dbGetQuery(con, "SELECT * FROM hf_metatask_queue ORDER BY id ASC")
  tasks <- if (nrow(q) == 0) list() else lapply(seq_len(nrow(q)), function(i) list(
    id = q$id[i], title = q$title[i], payload = ifelse(is.na(q$payload[i]), "", q$payload[i]),
    cron = ifelse(is.na(q$cron[i]), "", q$cron[i]), status = ifelse(is.na(q$status[i]), "pending", q$status[i]),
    last_run_at = if (is.na(q$last_run_at[i]) || is.null(q$last_run_at[i])) NULL else q$last_run_at[i],
    next_run_at = if (is.na(q$next_run_at[i]) || is.null(q$next_run_at[i])) NULL else q$next_run_at[i],
    result = ifelse(is.na(q$result[i]), "", q$result[i]),
    llm_name = if ("llm_name" %in% names(q)) ifelse(is.na(q$llm_name[i]), "", q$llm_name[i]) else "",
    agent = if ("agent" %in% names(q)) ifelse(is.na(q$agent[i]), "", q$agent[i]) else ""))
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

# ── done：标记任务完成（可附带会话元数据：--llm/--agent/--tin/--tout/--dur/--sid）──
# 用法：done <id> "<result>" [--llm <名>] [--agent CodeBuddy] [--tin <n>] [--tout <n>] [--dur <ms>] [--sid <会话ID>]
mark_done <- function(id, result = "", meta = list()) {
  con <- db(); on.exit(dbDisconnect(con))
  ensure_agent_cols(con)
  row <- dbGetQuery(con, sprintf("SELECT * FROM hf_metatask_queue WHERE id=%s", id))
  if (nrow(row) == 0) { cat(toJSON(list(success = FALSE, message = "任务不存在"))); return(invisible(FALSE)) }
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cron <- ifelse(is.na(row$cron[1]), "", row$cron[1])
  nxt_str <- NULL
  llm <- meta$llm %||% ""; agt <- meta$agent %||% ""
  tin <- as.integer(meta$tin %||% 0); tout <- as.integer(meta$tout %||% 0)
  dur <- as.integer(meta$dur %||% 0); sid <- meta$sid %||% ""
  meta_sql <- sprintf(
    "session_id='%s', llm_name='%s', agent='%s', token_in=%d, token_out=%d, duration_ms=%d, started_at='%s', finished_at='%s'",
    esc(sid), esc(llm), esc(agt), tin, tout, dur, now_str, now_str)
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
    "INSERT INTO hf_task_sessions (task_id,session_id,title,llm_name,agent,token_in,token_out,duration_ms,status,result,started_at,finished_at)
     VALUES (%s,'%s','%s','%s','%s',%d,%d,%d,'success','%s','%s','%s')",
    as.character(id), esc(sid), esc(row$title[1]), esc(llm), esc(agt), tin, tout, dur, esc(result), now_str, now_str))
  export(); export_sessions()
  cat(toJSON(list(success = TRUE, message = "已更新任务状态", next_run_at = nxt_str)))
  invisible(TRUE)
}

# ── 导出会话明细到 hungfo_sessions.js ──
export_sessions <- function(limit = 200) {
  con <- db(); on.exit(dbDisconnect(con))
  ensure_agent_cols(con)
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
    agent = if ("agent" %in% names(s)) ifelse(is.na(s$agent[i]), "", s$agent[i]) else "",
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

# ── usage：采集本机 CodeBuddy 自动任务运行统计（次数/成功率/耗时）写入 hf_cb_usage 并导出 js ──
CB_AUTOMATION_DB <- function() {
  env <- Sys.getenv("CODEBUDDY_AUTOMATION_DB")
  if (nzchar(env) && file.exists(env)) return(env)
  appdata <- Sys.getenv("APPDATA", unset = Sys.getenv("USERPROFILE", unset = ""))
  cand <- c(
    file.path(appdata, "CodeBuddy CN", "automations", "automations.db"),
    file.path(appdata, "CodeBuddy", "automations", "automations.db"))
  for (p in cand) if (nzchar(p) && file.exists(p)) return(p)
  NULL
}
# 秒/毫秒时间戳 → 本地时间字符串（兼容 SQLite INTEGER 时间戳）
cb_ts_str <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0 || is.na(x) || x <= 0) return("")
  secs <- if (x > 1e12) x / 1000 else x
  format(as.POSIXct(secs, origin = "1970-01-01", tz = "Asia/Shanghai"), "%Y-%m-%d %H:%M:%S")
}
collect_usage <- function() {
  cb <- CB_AUTOMATION_DB()
  if (is.null(cb)) { cat(toJSON(list(success = FALSE, message = "未找到本机 CodeBuddy automations.db"))); return(invisible(FALSE)) }
  ccon <- tryCatch(dbConnect(SQLite(), cb), error = function(e) NULL)
  if (is.null(ccon)) { cat(toJSON(list(success = FALSE, message = "无法打开 CodeBuddy automations.db"))); return(invisible(FALSE)) }
  on.exit(try(dbDisconnect(ccon), silent = TRUE), add = TRUE)
  autos <- tryCatch(dbGetQuery(ccon, "SELECT id, name, status, schedule_type FROM automations ORDER BY name"), error = function(e) data.frame())
  runs <- tryCatch(dbGetQuery(ccon, "SELECT automation_id, result_success, created_at, updated_at FROM automation_runs"), error = function(e) data.frame())
  # 聚合每个自动任务的运行统计（无任务也正常导出空镜像）
  if (is.null(autos) || nrow(autos) == 0) autos <- data.frame(id = character(0), name = character(0), status = character(0), schedule_type = character(0), stringsAsFactors = FALSE)
  if (is.null(runs) || nrow(runs) == 0) runs <- data.frame(automation_id = character(0), result_success = integer(0), created_at = numeric(0), updated_at = numeric(0), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(autos)), function(i) {
    aid <- as.character(autos$id[i])
    r <- runs[!is.na(runs$automation_id) & as.character(runs$automation_id) == aid, , drop = FALSE]
    total <- nrow(r)
    success <- if (total == 0) 0 else sum(as.integer(r$result_success) == 1, na.rm = TRUE)
    failed <- total - success
    rate <- if (total == 0) 0 else round(success / total * 100, 1)
    last_ts <- if (total == 0) NA else suppressWarnings(max(as.numeric(r$created_at), na.rm = TRUE))
    use_ms <- !is.na(last_ts) && last_ts > 1e12
    diff_units <- pmax(0, as.numeric(r$updated_at) - as.numeric(r$created_at), na.rm = TRUE)
    dur_ms <- if (total == 0) 0 else round(mean(diff_units, na.rm = TRUE) * ifelse(use_ms, 1, 1000))
    list(automation_id = aid, name = as.character(autos$name[i]),
         schedule_type = ifelse(is.na(autos$schedule_type[i]), "", as.character(autos$schedule_type[i])),
         status = ifelse(is.na(autos$status[i]), "", as.character(autos$status[i])),
         runs_total = total, runs_success = success, runs_failed = failed,
         success_rate = rate, avg_duration_ms = dur_ms, last_run_at = cb_ts_str(last_ts))
  })
  df <- do.call(rbind, lapply(rows, function(r) data.frame(r, stringsAsFactors = FALSE)))
  if (is.null(df)) df <- data.frame()
  # 写 HungFo 库（last_run_at 用 TEXT 存本地时间字符串）
  con <- db(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_cb_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT, automation_id TEXT, name TEXT, schedule_type TEXT,
    status TEXT, runs_total INTEGER DEFAULT 0, runs_success INTEGER DEFAULT 0,
    runs_failed INTEGER DEFAULT 0, success_rate REAL DEFAULT 0, avg_duration_ms INTEGER DEFAULT 0,
    last_run_at TEXT, created_at TEXT DEFAULT (datetime('now','localtime')))")
  dbExecute(con, "DELETE FROM hf_cb_usage")
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      dbExecute(con, sprintf(
        "INSERT INTO hf_cb_usage (automation_id,name,schedule_type,status,runs_total,runs_success,runs_failed,success_rate,avg_duration_ms,last_run_at)
         VALUES ('%s','%s','%s','%s',%d,%d,%d,%f,%d,'%s')",
        esc(df$automation_id[i]), esc(df$name[i]), esc(df$schedule_type[i]), esc(df$status[i]),
        as.integer(df$runs_total[i]), as.integer(df$runs_success[i]), as.integer(df$runs_failed[i]),
        as.numeric(df$success_rate[i]), as.integer(df$avg_duration_ms[i]), esc(as.character(df$last_run_at[i]))))
    }
  }
  # 导出 js（汇总：总/今日 运行次数·成功率·成功/失败·任务数）
  all_s <- dbGetQuery(con, "SELECT COALESCE(SUM(runs_total),0) runs, COALESCE(SUM(runs_success),0) succ, COALESCE(SUM(runs_failed),0) fail, COUNT(*) n FROM hf_cb_usage")
  day_s <- dbGetQuery(con, "SELECT COALESCE(SUM(runs_total),0) runs, COALESCE(SUM(runs_success),0) succ, COALESCE(SUM(runs_failed),0) fail, COUNT(*) n FROM hf_cb_usage WHERE date(last_run_at) = date('now','localtime')")
  mk <- function(s) list(runs = s$runs[1], success = s$succ[1], failed = s$fail[1],
                         rate = if (s$runs[1] > 0) round(s$succ[1] / s$runs[1] * 100, 1) else 0,
                         automations = s$n[1])
  usages <- if (nrow(df) == 0) list() else lapply(seq_len(nrow(df)), function(i) list(
    automation_id = df$automation_id[i], name = df$name[i], schedule_type = df$schedule_type[i],
    status = df$status[i], runs_total = df$runs_total[i], runs_success = df$runs_success[i],
    runs_failed = df$runs_failed[i], success_rate = df$success_rate[i],
    avg_duration_ms = df$avg_duration_ms[i], last_run_at = df$last_run_at[i]))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              summary = list(total = mk(all_s), today = mk(day_s)),
              usages = usages)
  js_path <- file.path(hungfo_home(), "www", "hungfo_usage.js")
  dir.create(dirname(js_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(paste0("// 由 HungFo 生成（collect_usage），勿手改\nwindow.HF_CB_USAGE = ", toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), ";\n"), js_path, useBytes = TRUE)
  cat(toJSON(list(success = TRUE, automations = nrow(df), message = paste0("已采集 ", nrow(df), " 个 CodeBuddy 自动任务运行统计，导出 hungfo_usage.js"))))
  invisible(TRUE)
}

# ── sessions 清单：采集本机 CodeBuddy 会话（标题/状态/时间）+ 手动补 Token 明细 ──
CB_SESSIONS_DB <- function() {
  env <- Sys.getenv("CODEBUDDY_SESSIONS_DB")
  if (nzchar(env) && file.exists(env)) return(env)
  appdata <- Sys.getenv("APPDATA", unset = Sys.getenv("USERPROFILE", unset = ""))
  cand <- c(
    file.path(appdata, "CodeBuddy CN", "codebuddy-sessions.vscdb"),
    file.path(appdata, "CodeBuddy", "codebuddy-sessions.vscdb"))
  for (p in cand) if (nzchar(p) && file.exists(p)) return(p)
  NULL
}
CB_SESSIONS_DDL <- "CREATE TABLE IF NOT EXISTS hf_cb_sessions (
  id INTEGER PRIMARY KEY AUTOINCREMENT, conversation_id TEXT UNIQUE, title TEXT, cwd TEXT,
  status TEXT, created_at TEXT, updated_at TEXT,
  token_total REAL DEFAULT 0, token_input REAL DEFAULT 0, token_output REAL DEFAULT 0,
  token_cache_hit REAL DEFAULT 0, token_cache_miss REAL DEFAULT 0, token_cache_write REAL DEFAULT 0,
  token_thinking REAL DEFAULT 0, token_response REAL DEFAULT 0, token_cache_hit_rate REAL DEFAULT 0,
  token_updated_at TEXT, synced_at TEXT DEFAULT (datetime('now','localtime')))"
# 导出会话清单（含 Token 明细）到 www/hungfo_cb_sessions.js
export_cb_sessions <- function(limit = 200) {
  con <- db(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, CB_SESSIONS_DDL)
  s <- tryCatch(dbGetQuery(con, sprintf(
    "SELECT * FROM hf_cb_sessions ORDER BY COALESCE(token_updated_at, updated_at, created_at) DESC LIMIT %d", as.integer(limit))),
    error = function(e) data.frame())
  all_s <- tryCatch(dbGetQuery(con, "SELECT COUNT(*) n, COALESCE(SUM(token_total),0) tok, COALESCE(SUM(CASE WHEN token_total>0 THEN 1 ELSE 0 END),0) tok_n FROM hf_cb_sessions"),
                    error = function(e) data.frame(n = 0, tok = 0, tok_n = 0))
  summary <- list(total_sessions = all_s$n[1], total_tokens = all_s$tok[1], token_sessions = all_s$tok_n[1])
  sessions <- if (is.null(s) || nrow(s) == 0) list() else lapply(seq_len(nrow(s)), function(i) list(
    conversation_id = ifelse(is.na(s$conversation_id[i]), "", s$conversation_id[i]),
    title = ifelse(is.na(s$title[i]), "", s$title[i]),
    cwd = ifelse(is.na(s$cwd[i]), "", s$cwd[i]),
    status = ifelse(is.na(s$status[i]), "", s$status[i]),
    created_at = ifelse(is.na(s$created_at[i]), "", s$created_at[i]),
    updated_at = ifelse(is.na(s$updated_at[i]), "", s$updated_at[i]),
    token_total = ifelse(is.na(s$token_total[i]), 0, s$token_total[i]),
    token_input = ifelse(is.na(s$token_input[i]), 0, s$token_input[i]),
    token_output = ifelse(is.na(s$token_output[i]), 0, s$token_output[i]),
    token_cache_hit = ifelse(is.na(s$token_cache_hit[i]), 0, s$token_cache_hit[i]),
    token_cache_miss = ifelse(is.na(s$token_cache_miss[i]), 0, s$token_cache_miss[i]),
    token_cache_write = ifelse(is.na(s$token_cache_write[i]), 0, s$token_cache_write[i]),
    token_thinking = ifelse(is.na(s$token_thinking[i]), 0, s$token_thinking[i]),
    token_response = ifelse(is.na(s$token_response[i]), 0, s$token_response[i]),
    token_cache_hit_rate = ifelse(is.na(s$token_cache_hit_rate[i]), 0, s$token_cache_hit_rate[i]),
    token_updated_at = ifelse(is.na(s$token_updated_at[i]), "", s$token_updated_at[i])))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), summary = summary, sessions = sessions)
  js_path <- file.path(hungfo_home(), "www", "hungfo_cb_sessions.js")
  dir.create(dirname(js_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(paste0("// 由 HungFo 生成（export_cb_sessions），勿手改\nwindow.HF_CB_SESSIONS = ", toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), ";\n"), js_path, useBytes = TRUE)
  invisible(out)
}
collect_sessions <- function() {
  sdb <- CB_SESSIONS_DB()
  if (is.null(sdb)) { cat(toJSON(list(success = FALSE, message = "未找到本机 codebuddy-sessions.vscdb"))); return(invisible(FALSE)) }
  scon <- tryCatch(dbConnect(SQLite(), sdb), error = function(e) NULL)
  if (is.null(scon)) { cat(toJSON(list(success = FALSE, message = "无法打开 codebuddy-sessions.vscdb"))); return(invisible(FALSE)) }
  on.exit(try(dbDisconnect(scon), silent = TRUE), add = TRUE)
  raw <- tryCatch(dbGetQuery(scon, "SELECT key, value FROM ItemTable WHERE key LIKE 'session:%'"), error = function(e) data.frame())
  if (is.null(raw) || nrow(raw) == 0) { cat(toJSON(list(success = FALSE, message = "codebuddy-sessions.vscdb 中无会话记录"))); return(invisible(FALSE)) }
  parsed <- lapply(seq_len(nrow(raw)), function(i) {
    j <- tryCatch(fromJSON(raw$value[i]), error = function(e) list())
    if (length(j) == 0) return(NULL)
    data.frame(
      conversation_id = if (is.null(j$conversationId)) "" else as.character(j$conversationId),
      title = if (is.null(j$title)) "" else as.character(j$title),
      cwd = if (is.null(j$cwd)) "" else as.character(j$cwd),
      status = if (is.null(j$status)) "" else as.character(j$status),
      created_at = cb_ts_str(if (is.null(j$createdAt)) NA else j$createdAt),
      updated_at = cb_ts_str(if (is.null(j$updatedAt)) NA else j$updatedAt),
      stringsAsFactors = FALSE)
  })
  parsed <- Filter(Negate(is.null), parsed)
  df <- if (length(parsed) == 0) data.frame() else do.call(rbind, parsed)
  con <- db(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, CB_SESSIONS_DDL)
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      dbExecute(con, sprintf(
        "INSERT OR IGNORE INTO hf_cb_sessions (conversation_id,title,cwd,status,created_at,updated_at) VALUES ('%s','%s','%s','%s','%s','%s')",
        esc(df$conversation_id[i]), esc(df$title[i]), esc(df$cwd[i]), esc(df$status[i]),
        esc(df$created_at[i]), esc(df$updated_at[i])))
      # 只更新元信息，不覆盖手动录入的 token 字段
      dbExecute(con, sprintf(
        "UPDATE hf_cb_sessions SET title='%s', cwd='%s', status='%s', updated_at='%s', synced_at=datetime('now','localtime') WHERE conversation_id='%s'",
        esc(df$title[i]), esc(df$cwd[i]), esc(df$status[i]), esc(df$updated_at[i]), esc(df$conversation_id[i])))
    }
  }
  export_cb_sessions()
  cat(toJSON(list(success = TRUE, sessions = nrow(df), message = paste0("已采集 ", nrow(df), " 个 CodeBuddy 会话清单，导出 hungfo_cb_sessions.js"))))
  invisible(TRUE)
}
# token：手动录入某会话的 Token 消耗明细（命中率自动计算）
# 用法：token <conversation_id> <total> <input> <output> <cacheHit> <cacheMiss> <cacheWrite> <thinking> <response>
mark_token <- function(cid, vals) {
  if (is.null(cid) || !nzchar(cid)) { cat(toJSON(list(success = FALSE, message = "缺少 conversation_id"))); return(invisible(FALSE)) }
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (is.na(x)) 0 else x }
  total <- num(vals[1]); input <- num(vals[2]); output <- num(vals[3])
  cache_hit <- num(vals[4]); cache_miss <- num(vals[5]); cache_write <- num(vals[6])
  thinking <- num(vals[7]); response <- num(vals[8])
  rate <- if (cache_hit + cache_miss + cache_write > 0) round(cache_hit / (cache_hit + cache_miss + cache_write) * 100, 1) else 0
  con <- db(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, CB_SESSIONS_DDL)
  dbExecute(con, sprintf("INSERT OR IGNORE INTO hf_cb_sessions (conversation_id, title, status) VALUES ('%s','%s','')", esc(cid), esc(cid)))
  dbExecute(con, sprintf(
    "UPDATE hf_cb_sessions SET token_total=%f, token_input=%f, token_output=%f, token_cache_hit=%f, token_cache_miss=%f, token_cache_write=%f, token_thinking=%f, token_response=%f, token_cache_hit_rate=%f, token_updated_at=datetime('now','localtime') WHERE conversation_id='%s'",
    total, input, output, cache_hit, cache_miss, cache_write, thinking, response, rate, esc(cid)))
  export_cb_sessions()
  cat(toJSON(list(success = TRUE, conversation_id = cid, message = paste0("已录入会话 ", cid, " 的 Token 明细（命中率 ", rate, "%）"))))
  invisible(TRUE)
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
if (length(args) == 0) { cat("用法: peek | done <id> <result> [--llm n --agent a --tin n --tout n --dur ms --sid s] | log <title> <content> | export | sessions | usage | token <cid> <total> <input> <output> <cacheHit> <cacheMiss> <cacheWrite> <thinking> <response> | cron <expr>\n"); quit(status = 0) }

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
} else if (cmd == "usage") {
  collect_usage()
  collect_sessions()
} else if (cmd == "token" && length(args) >= 2) {
  mark_token(args[2], args[-(1:2)])
} else if (cmd == "cron" && length(args) >= 2) {
  nxt <- cron_next(paste(args[-1], collapse = " "))
  cat(if (is.null(nxt)) "NULL" else format(nxt, "%Y-%m-%d %H:%M:%S"))
} else {
  cat("未知命令:", cmd, "\n")
}
