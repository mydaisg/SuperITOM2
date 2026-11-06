#!/usr/bin/env Rscript
##################
# HungFo 元任务命令行工具（供 CodeBuddy 定时任务调用）
# 独立于 Shiny，可直接 Rscript 调用，负责元任务队列的状态管理与开发日志写入。
#
# 用法：
#   Rscript hf_metatask_runner.r peek              # 输出到期待执行任务(JSON)到 stdout
#   Rscript hf_metatask_runner.r done <id> "<结果>"  # 标记任务完成(写库+更新镜像)
#   Rscript hf_metatask_runner.r log "<标题>" "<内容>"  # 写五段式开发日志（缺段自动补齐并标注）
#   Rscript hf_metatask_runner.r log5 "<标题>" "<需求>" "<分析>" "<方案>" "<结果>" "<记忆>"  # 显式五段式
#   Rscript hf_metatask_runner.r enqueue "<标题>" "<内容>" --source wecom --model fast-model --tasktype ui
#                                                 # 统一入队（web / wecom / cron / cb 走同一张表）
#   Rscript hf_metatask_runner.r context bugfix   # 按分类装配规则+记忆，输出 system prompt 文件路径
#   Rscript hf_metatask_runner.r sessions          # 重新导出 hungfo_sessions.js
#   Rscript hf_metatask_runner.r usage             # 采集 CodeBuddy 自动任务运行统计 + 会话清单并导出
#   Rscript hf_metatask_runner.r token <cid> <total> <input> <output> <cacheHit> <cacheMiss> <cacheWrite> <thinking> <response>  # 手动录入某会话 Token 明细
#   Rscript hf_metatask_runner.r cron "<expr>"     # 测试 cron 表达式的下一个触发时间
##################

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
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

# 确保 AI 上下文相关列 / 表存在（旧库兼容；独立 Rscript 可能先于 Shiny 启动）
# 与 hf_core.r::hf_db_init() 的 DDL 保持同构，全部幂等，不改写已有数据。
ensure_agent_cols <- function(con) {
  add_col <- function(tbl, nm, ddl) {
    cols <- tryCatch(dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tbl)), error = function(e) data.frame())
    if (nrow(cols) > 0 && !(nm %in% cols$name))
      dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", tbl, nm, ddl))
  }
  for (tbl in c("hf_metatask_queue", "hf_task_sessions")) add_col(tbl, "agent", "TEXT")
  add_col("hf_metatask_queue", "model",      "TEXT DEFAULT ''")
  add_col("hf_metatask_queue", "task_type",  "TEXT DEFAULT ''")
  add_col("hf_metatask_queue", "source",     "TEXT DEFAULT 'web'")
  add_col("hf_task_sessions",  "model",      "TEXT DEFAULT ''")
  # 分类路由列
  for (cc in list(c("applies_to","TEXT DEFAULT ''"), c("model","TEXT DEFAULT ''"),
                  c("effort","TEXT DEFAULT ''"), c("max_turns","INTEGER DEFAULT 0"),
                  c("enabled","INTEGER DEFAULT 1"), c("always_apply","INTEGER DEFAULT 0"),
                  c("description","TEXT DEFAULT ''")))
    add_col("hf_rules_categories", cc[1], cc[2])
  add_col("hf_rules", "parent_id", "INTEGER DEFAULT 0")
  add_col("hf_rules", "task_type", "TEXT DEFAULT ''")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_rule_items (
    id INTEGER PRIMARY KEY AUTOINCREMENT, rule_id INTEGER NOT NULL,
    parent_id INTEGER DEFAULT 0, content TEXT NOT NULL, version_no INTEGER DEFAULT 1,
    status TEXT DEFAULT 'effective', created_by INTEGER, created_by_name TEXT DEFAULT '',
    created_at TEXT DEFAULT (datetime('now','localtime')))")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_memory (
    id INTEGER PRIMARY KEY AUTOINCREMENT, topic TEXT NOT NULL,
    category TEXT DEFAULT '项目事实', content TEXT, source TEXT DEFAULT '', tags TEXT DEFAULT '',
    parent_id INTEGER DEFAULT 0, status TEXT DEFAULT 'effective', created_by INTEGER,
    created_by_name TEXT DEFAULT '', created_at TEXT DEFAULT (datetime('now','localtime')))")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_config_kv (
    cfg_key TEXT PRIMARY KEY, cfg_value TEXT, cfg_desc TEXT DEFAULT '',
    updated_at TEXT DEFAULT (datetime('now','localtime')))")
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
    agent = if ("agent" %in% names(q)) ifelse(is.na(q$agent[i]), "", q$agent[i]) else "",
    model = row_val(q[i, , drop = FALSE], "model"),
    task_type = row_val(q[i, , drop = FALSE], "task_type"),
    source = row_val(q[i, , drop = FALSE], "source", "web")))
  out <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), tasks = tasks)
  writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), META_JSON, useBytes = TRUE)
  invisible(out)
}

# 取一行的某列（旧库可能缺列，缺列时回退默认值）
row_val <- function(r, nm, def = "") {
  if (!(nm %in% names(r))) return(def)
  v <- r[[nm]][1]
  if (is.null(v) || is.na(v)) def else as.character(v)
}

# ── peek：返回到期待执行任务（含 model / task_type / source，供按模型路由与统一入口使用）──
peek <- function() {
  con <- db(); on.exit(dbDisconnect(con))
  ensure_agent_cols(con)
  q <- dbGetQuery(con, "SELECT * FROM hf_metatask_queue WHERE status='pending' ORDER BY id ASC")
  now <- Sys.time()
  due <- if (nrow(q) == 0) list() else lapply(seq_len(nrow(q)), function(i) {
    r <- q[i, , drop = FALSE]
    nxt <- r$next_run_at[1]
    due_flag <- if (is.na(nxt) || nxt == "") TRUE else (as.POSIXct(nxt, tz = "Asia/Shanghai") <= now)
    if (due_flag) list(
      id = r$id[1], title = r$title[1],
      payload = ifelse(is.na(r$payload[1]), "", r$payload[1]),
      cron = ifelse(is.na(r$cron[1]), "", r$cron[1]),
      model = row_val(r, "model"), task_type = row_val(r, "task_type"),
      source = row_val(r, "source", "web")) else NULL
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
  if (nrow(row) == 0) {
    cat(toJSON(list(success = FALSE, message = "任务不存在"), auto_unbox = TRUE))
    return(invisible(FALSE))
  }
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cron <- ifelse(is.na(row$cron[1]), "", row$cron[1])
  nxt_str <- NULL
  llm <- meta$llm %||% ""; agt <- meta$agent %||% ""
  mdl <- meta$model %||% ""
  # 未显式上报 model 时，用队列里任务指定的模型回填（保证台账是「实际指定」而非事后猜测）
  if (!nzchar(mdl)) mdl <- row_val(row, "model")
  # 未显式上报 llm 但已指定模型时，用模型名回填
  if (!nzchar(llm) && nzchar(mdl)) llm <- mdl
  tin <- as.integer(meta$tin %||% 0); tout <- as.integer(meta$tout %||% 0)
  dur <- as.integer(meta$dur %||% 0); sid <- meta$sid %||% ""
  meta_sql <- sprintf(
    "session_id='%s', llm_name='%s', agent='%s', model='%s', token_in=%d, token_out=%d, duration_ms=%d, started_at='%s', finished_at='%s'",
    esc(sid), esc(llm), esc(agt), esc(mdl), tin, tout, dur, now_str, now_str)
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
    "INSERT INTO hf_task_sessions (task_id,session_id,title,llm_name,agent,model,token_in,token_out,duration_ms,status,result,started_at,finished_at)
     VALUES (%s,'%s','%s','%s','%s','%s',%d,%d,%d,'success','%s','%s','%s')",
    as.character(id), esc(sid), esc(row$title[1]), esc(llm), esc(agt), esc(mdl),
    tin, tout, dur, esc(result), now_str, now_str))
  export(); export_sessions()
  cat(toJSON(list(success = TRUE, message = "已更新任务状态", next_run_at = nxt_str),
             auto_unbox = TRUE, null = "null"))
  invisible(TRUE)
}

# ── 导出会话明细到 hungfo_sessions.js ──
export_sessions <- function(limit = 200) {
  con <- db(); on.exit(dbDisconnect(con))
  ensure_agent_cols(con)
  s <- tryCatch(dbGetQuery(con, sprintf("SELECT * FROM hf_task_sessions ORDER BY id DESC LIMIT %d", limit)),
                error = function(e) data.frame())
  # 会话汇总（token / 会话数 / 成功失败 / 总耗时）
  mk_sm <- function(s) list(
    token_in = s$tin[1], token_out = s$tout[1], sessions = s$n[1],
    success = s$succ[1], failed = max(0, s$n[1] - s$succ[1]), duration_ms = s$dur[1])
  all_s <- tryCatch(dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COUNT(*) n, COALESCE(SUM(duration_ms),0) dur, COALESCE(SUM(CASE WHEN status='success' THEN 1 ELSE 0 END),0) succ FROM hf_task_sessions"), error = function(e) data.frame(tin=0,tout=0,n=0,dur=0,succ=0))
  day_s <- tryCatch(dbGetQuery(con, "SELECT COALESCE(SUM(token_in),0) tin, COALESCE(SUM(token_out),0) tout, COUNT(*) n, COALESCE(SUM(duration_ms),0) dur, COALESCE(SUM(CASE WHEN status='success' THEN 1 ELSE 0 END),0) succ FROM hf_task_sessions WHERE date(started_at)=date('now','localtime')"), error = function(e) data.frame(tin=0,tout=0,n=0,dur=0,succ=0))
  summary <- list(total = mk_sm(all_s), today = mk_sm(day_s))
  sessions <- if (is.null(s) || nrow(s) == 0) list() else lapply(seq_len(nrow(s)), function(i) list(
    id = s$id[i], task_id = s$task_id[i], session_id = ifelse(is.na(s$session_id[i]), "", s$session_id[i]),
    title = ifelse(is.na(s$title[i]), "", s$title[i]), llm_name = ifelse(is.na(s$llm_name[i]), "", s$llm_name[i]),
    agent = if ("agent" %in% names(s)) ifelse(is.na(s$agent[i]), "", s$agent[i]) else "",
    token_in = ifelse(is.na(s$token_in[i]), 0, s$token_in[i]), token_out = ifelse(is.na(s$token_out[i]), 0, s$token_out[i]),
    duration_ms = ifelse(is.na(s$duration_ms[i]), 0, s$duration_ms[i]),
    status = ifelse(is.na(s$status[i]), "success", s$status[i]),
    result = if ("result" %in% names(s)) ifelse(is.na(s$result[i]), "", s$result[i]) else "",
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

# ── log：写一条五段式开发日志【需求】【分析】【方案】【结果】【记忆】──
# 缺少的段落会补一条明确标注的占位行（不是编造内容），保证新日志一律五段式。
write_log <- function(title, content) {
  con <- db(); on.exit(dbDisconnect(con))
  no <- sprintf("HF-DEVLOG-%s", format(Sys.time(), "%Y%m%d%H%M%S"))
  # 规范化分段：在非行首的段标签（【xx】）前插入换行，确保每个【】段独立成行，
  # 与父项目开发日志的严格样式一致（避免内容挤在一行导致分段解析错乱）。
  content <- gsub("(?<!\n)(?=\u3010[^\u3011]*\u3011)", "\n", content, perl = TRUE)
  content <- trimws(content)
  need <- c("需求", "分析", "方案", "结果", "记忆")
  missing <- need[!vapply(need, function(s) grepl(paste0("【", s, "】"), content, fixed = TRUE), logical(1))]
  if (length(missing) > 0)
    content <- paste(c(content, vapply(missing, function(s)
      sprintf("【%s】（本段由 log 命令补齐，调用方未单独提供内容）", s), character(1))),
      collapse = "\n")
  dbExecute(con, sprintf(
    "INSERT INTO hf_records (module_key, rec_no, title, payload, status) VALUES ('devlog','%s','%s','%s','draft')",
    esc(no), esc(title), esc(content)))
  cat(toJSON(list(success = TRUE, message = paste0("已写开发日志 ", no), rec_no = no,
                  sections = length(need), auto_filled = missing), auto_unbox = TRUE))
  invisible(TRUE)
}

# ── log5：显式五段式写入（推荐给 AI 使用，避免占位段）──
# 用法：log5 "<标题>" "<需求>" "<分析>" "<方案>" "<结果>" "<记忆>"
write_log5 <- function(title, req, ana, plan, res, mem) {
  content <- paste(sprintf("【%s】%s", c("需求","分析","方案","结果","记忆"),
                           c(req, ana, plan, res, mem)), collapse = "\n")
  write_log(title, content)
}

# ── enqueue：统一入口入队（web / wecom / cron / cb 都写同一张队列表）──
# 用法：enqueue "<标题>" "<任务内容>" --source wecom --model fast-model --tasktype ui --cron "<expr>"
enqueue <- function(title, payload, meta = list()) {
  if (is.null(title) || !nzchar(trimws(title))) {
    cat(toJSON(list(success = FALSE, message = "任务标题不能为空"))); return(invisible(FALSE))
  }
  con <- db(); on.exit(dbDisconnect(con))
  ensure_agent_cols(con)
  src <- as.character(meta$source %||% "web")
  mdl <- as.character(meta$model %||% "")
  tt  <- as.character(meta$tasktype %||% "")
  cr  <- as.character(meta$cron %||% "")
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  nxt <- if (nzchar(trimws(cr))) {
    x <- cron_next(cr); if (is.null(x)) now else format(x, "%Y-%m-%d %H:%M:%S")
  } else now
  dbExecute(con, sprintf(
    "INSERT INTO hf_metatask_queue (title,payload,cron,status,next_run_at,created_at,updated_at,model,task_type,source)
     VALUES ('%s','%s','%s','pending','%s','%s','%s','%s','%s','%s')",
    esc(title), esc(payload), esc(cr), esc(nxt), now, now, esc(mdl), esc(tt), esc(src)))
  id <- dbGetQuery(con, "SELECT last_insert_rowid() id")$id[1]
  export()
  cat(toJSON(list(success = TRUE, id = id, message = sprintf("已从 %s 入口入队任务 #%s", src, id),
                  source = src, model = mdl, task_type = tt, next_run_at = nxt), auto_unbox = TRUE))
  invisible(TRUE)
}

# ── context：按任务类型/分类装配规则 + 记忆，生成 CLI 可直接用的 system prompt 文件 ──
# 这是「AI 每次执行自动分类读取」的落地：定时任务 / 微信入口拿到路径后
# 用 --system-prompt-file <path> 注入，只读取相关的规则与记忆。
build_context <- function(task_type = "default", category = "") {
  con <- db(); on.exit(dbDisconnect(con))
  ensure_agent_cols(con)
  tt <- if (is.null(task_type) || !nzchar(task_type)) "default" else task_type
  cats <- tryCatch(dbGetQuery(con, "SELECT * FROM hf_rules_categories"), error = function(e) data.frame())
  # 路由表（hf_config_kv.ai_model_routes）里的分类
  route_cats <- character(0)
  cfg <- tryCatch(dbGetQuery(con, "SELECT cfg_value FROM hf_config_kv WHERE cfg_key='ai_model_routes'"),
                  error = function(e) data.frame())
  if (nrow(cfg) > 0 && nzchar(cfg$cfg_value[1])) {
    rs <- tryCatch(fromJSON(cfg$cfg_value[1], simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(rs)) {
      if (!is.null(names(rs)) && "task_type" %in% names(rs)) rs <- list(rs)
      for (r in rs) if (identical(as.character(r$task_type %||% ""), tt))
        route_cats <- trimws(strsplit(as.character(r$categories %||% ""), "[,，;；]")[[1]])
    }
  }
  wanted <- character(0)
  if (nzchar(trimws(category))) wanted <- trimws(strsplit(category, "[,，;；]")[[1]])
  wanted <- c(wanted, route_cats)
  if (nrow(cats) > 0) for (i in seq_len(nrow(cats))) {
    aa <- as.integer(cats$always_apply[i] %||% 0); if (!is.na(aa) && aa == 1) wanted <- c(wanted, as.character(cats$name[i]))
    ap <- as.character(cats$applies_to[i] %||% ""); if (nzchar(ap) && grepl(tt, ap, fixed = TRUE)) wanted <- c(wanted, as.character(cats$name[i]))
  }
  wanted <- unique(wanted[nzchar(wanted)])

  lines <- c("# HungFo AI 执行上下文（元任务/外部入口自动生成）",
             sprintf("生成时间：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
             sprintf("任务类型：%s", tt),
             sprintf("匹配分类：%s", if (length(wanted)) paste(wanted, collapse = "、") else "（无）"),
             "", "## 一、必须遵守的规则（按分类自动装配）", "")
  nr <- 0L
  rules <- tryCatch(dbGetQuery(con, "SELECT * FROM hf_rules ORDER BY id DESC LIMIT 200"), error = function(e) data.frame())
  if (nrow(rules) > 0 && length(wanted) > 0) {
    sel <- rules[as.character(rules$category) %in% wanted, , drop = FALSE]
    for (i in seq_len(nrow(sel))) {
      rid <- sel$id[i]
      items <- tryCatch(dbGetQuery(con, sprintf(
        "SELECT * FROM hf_rule_items WHERE rule_id=%s ORDER BY id ASC", as.character(as.integer(rid)))),
        error = function(e) data.frame())
      body <- as.character(sel$payload[i] %||% "")
      ver <- 0L
      if (nrow(items) > 0) {
        eff <- items[!is.na(items$status) & items$status == "effective", , drop = FALSE]
        if (nrow(eff) == 0) eff <- items
        body <- as.character(eff$content[nrow(eff)]); ver <- as.integer(eff$version_no[nrow(eff)] %||% nrow(items))
      }
      nr <- nr + 1L
      lines <- c(lines, sprintf("### %s %s（当前生效 v%d）",
        as.character(sel$rule_no[i] %||% ""), as.character(sel$title[i] %||% ""), ver), "",
        gsub("\r", "", body), "")
    }
  }
  if (nr == 0) lines <- c(lines, "（本次任务无匹配规则）", "")

  lines <- c(lines, "## 二、项目与 AI 记忆", "")
  mem <- tryCatch(dbGetQuery(con,
    "SELECT * FROM hf_memory WHERE status='effective' ORDER BY id DESC LIMIT 20"),
    error = function(e) data.frame())
  nm <- 0L
  if (nrow(mem) > 0) for (i in seq_len(nrow(mem))) {
    nm <- nm + 1L
    lines <- c(lines, sprintf("- **[%s] %s**", as.character(mem$category[i] %||% ""),
                              as.character(mem$topic[i] %||% "")),
               paste0("  ", gsub("\n", "\n  ", as.character(mem$content[i] %||% ""))), "")
  }
  if (nm == 0) lines <- c(lines, "（暂无记忆）", "")
  lines <- c(lines, "## 三、执行纪律", "",
    "- 只改与任务直接相关的文件，不破坏已有功能与数据。",
    "- 完成后写五段式开发日志：【需求】【分析】【方案】【结果】【记忆】，结果段含 Token 等消耗数据。",
    "- 规则冲突以最新修订版本为准。", "")

  d <- file.path(hungfo_home(), "ai_context")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(d, sprintf("ctx_mt_%s.md", format(Sys.time(), "%Y%m%d%H%M%S")))
  writeLines(paste(lines, collapse = "\n"), f, useBytes = TRUE)
  cat(toJSON(list(success = TRUE, path = f, task_type = tt, categories = wanted,
                  rules = nr, memory = nm), auto_unbox = TRUE))
  invisible(f)
}

# ── 主入口 ──
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) { cat("用法: peek | done <id> <result> [--llm n --agent a --model m --tin n --tout n --dur ms --sid s] | log <title> <content> | log5 <title> <需求> <分析> <方案> <结果> <记忆> | enqueue <title> <payload> [--source web|wecom|cron|cb --model m --tasktype t --cron expr] | context <task_type> [--category c] | export | sessions | usage | token ... | cron <expr>\n"); quit(status = 0) }

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
} else if (cmd == "log5" && length(args) >= 7) {
  write_log5(args[2], args[3], args[4], args[5], args[6], paste(args[-(1:6)], collapse = " "))
} else if (cmd == "enqueue" && length(args) >= 3) {
  pf <- parse_flags(args[-1])
  ttl <- pf$rest[1]
  pld <- if (length(pf$rest) >= 2) paste(pf$rest[-1], collapse = " ") else ""
  enqueue(ttl, pld, pf$meta)
} else if (cmd == "context") {
  pf <- parse_flags(args[-1])
  tt <- if (length(pf$rest) >= 1) pf$rest[1] else "default"
  build_context(tt, as.character(pf$meta$category %||% ""))
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
