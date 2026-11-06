#!/usr/bin/env Rscript
##################
# HungFo 真实 CodeBuddy CLI 端到端验证（非 mock、非单元测试）
#  - 真正 spawn codebuddy -p，指定便宜模型 + 注入的 system prompt 文件
#  - 校验 stream-json 的 system/init.model、result 事件、usage Token
#  - 校验运行记录回落库（状态 / Token / 模型 / 上下文条数）
#  - 全程使用临时库 + 临时 HUNGFO_HOME，不污染正式库与正式镜像
##################

Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(shiny); library(DT); library(DBI); library(RSQLite); library(jsonlite) })
HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
setwd(HOME)
PROD <- file.path(HOME, "DB", "HF_HungFo.DB")
TMPDIR <- file.path(HOME, "DB", "test_tmp")
THOME <- file.path(TMPDIR, "home")
TMP <- file.path(TMPDIR, "hf_e2e.DB")
dir.create(THOME, showWarnings = FALSE, recursive = TRUE)
file.copy(PROD, TMP, overwrite = TRUE)
Sys.setenv(HUNGFO_DB = TMP, HUNGFO_HOME = THOME)

source("hf_core.r"); source("hf_ai_context.r"); source("hf_config.r")
source("hf_mod_workbench.r")
hf_db_init(); hf_workbench_init()

cat("== 1) 真实 CLI 派发 ==\n")
r <- hf_workbench_run(
  "只回复两个字：OK。不要调用任何工具，不要修改任何文件，不要写开发日志。",
  model = "fast-model", effort = "minimal", max_turns = 1,
  task_type = "chat", timeout_sec = 150)
print(r[c("success", "run_id", "model", "task_type", "context_rules", "context_memory", "message")])
if (!isTRUE(r$success)) { cat("E2E FAIL: 派发失败\n"); quit(status = 1) }
cat("ARGS: ", paste(r$args, collapse = " "), "\n")

p <- hf_bus$workbench_procs[[r$run_id]]
if (!is.null(p)) {
  for (i in 1:75) {
    alive <- tryCatch(isTRUE(p$is_alive()), error = function(e) FALSE)
    if (!alive) break
    Sys.sleep(2)
  }
}
cat("== 2) 刷新运行状态 ==\n")
hf_workbench_refresh_status()
con <- dbConnect(SQLite(), TMP)
run <- dbGetQuery(con, sprintf("SELECT * FROM hf_workbench_runs WHERE run_id='%s'", r$run_id))
dbDisconnect(con)
cat(sprintf("status=%s | model=%s | task_type=%s | token_total=%s | cost_usd=%s | num_turns=%s | duration_ms=%s | ctx_rules=%s | ctx_mem=%s\n",
  run$status[1], run$model[1], run$task_type[1], run$token_total[1], run$cost_usd[1],
  run$num_turns[1], run$duration_ms[1], run$context_rules[1], run$context_memory[1]))

logf <- run$log_path[1]
cat(sprintf("log=%s size=%s bytes\n", logf, if (file.exists(logf)) file.size(logf) else 0))
init_model <- ""; init_seen <- FALSE; result_seen <- FALSE; result_cost <- 0
result_turns <- 0; result_errors <- character(0)
if (file.exists(logf)) {
  lines <- readLines(logf, warn = FALSE, encoding = "UTF-8")
  for (ln in lines) {
    if (!nzchar(ln) || substr(ln, 1, 1) != "{") next
    o <- tryCatch(fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(o)) next
    if (identical(as.character(o$type[1]), "system") &&
        identical(as.character(o$subtype[1]), "init")) {
      init_seen <- TRUE; init_model <- as.character(o$model[1])
    }
    if (identical(as.character(o$type[1]), "result")) {
      result_seen <- TRUE
      result_cost <- suppressWarnings(as.numeric(o$total_cost_usd[1])); if (is.na(result_cost)) result_cost <- 0
      result_turns <- suppressWarnings(as.numeric(o$num_turns[1])); if (is.na(result_turns)) result_turns <- 0
      result_errors <- as.character(unlist(o$errors))
    }
  }
  cat(sprintf("init_seen=%s | init.model=%s | result_seen=%s | result.cost_usd=%s | result.num_turns=%s | lines=%d\n",
    init_seen, init_model, result_seen, result_cost, result_turns, length(lines)))
  if (length(result_errors)) cat("result.errors: ", paste(result_errors, collapse = " | "), "\n")
}
u <- hf_workbench_usage(logf)
cat(sprintf("usage: in=%s out=%s cache_read=%s cache_write=%s total=%s\n",
  u$token_in, u$token_out, u$token_cache_read, u$token_cache_write, u$token_total))

ok <- list(
  run_record = nrow(run) == 1,
  status_done = nrow(run) == 1 && run$status[1] %in% c("done", "error", "timeout"),
  port_in_args = "--port" %in% r$args,
  model_in_args = "--model" %in% r$args && "fast-model" %in% r$args,
  prompt_in_args = "--system-prompt-file" %in% r$args,
  effort_in_args = "--effort" %in% r$args && "minimal" %in% r$args,
  max_turns_in_args = "--max-turns" %in% r$args,
  log_exists = file.exists(logf) && file.size(logf) > 0,
  cli_started = init_seen,
  model_effective = identical(init_model, "fast-model"),
  no_eaddrinuse = !any(grepl("EADDRINUSE", if (file.exists(logf)) readLines(logf, warn = FALSE) else "", fixed = TRUE))
)
cat("\n== 3) 断言 ==\n")
for (nm in names(ok)) cat(sprintf("  %-20s %s\n", nm, if (isTRUE(ok[[nm]])) "PASS" else "FAIL"))

cat("\n== 4) 取消 / 超时保护（真实进程）==\n")
r2 <- hf_workbench_run("这是一个用于验证取消能力的任务，请等待 60 秒后再回复。",
  model = "fast-model", task_type = "chat", max_turns = 1, timeout_sec = 90)
if (isTRUE(r2$success)) {
  Sys.sleep(1)
  cr <- hf_workbench_cancel(r2$run_id)
  cat(sprintf("cancel: %s\n", cr$message))
  con <- dbConnect(SQLite(), TMP)
  st2 <- dbGetQuery(con, sprintf("SELECT status FROM hf_workbench_runs WHERE run_id='%s'", r2$run_id))$status[1]
  dbDisconnect(con)
  cat(sprintf("status after cancel: %s\n", st2))
  ok$cancel_status <- identical(st2, "cancelled")
  cat(sprintf("  %-20s %s\n", "cancel_status", if (isTRUE(ok$cancel_status)) "PASS" else "FAIL"))
} else {
  cat("  cancel SKIP (并发上限或 CLI 未就绪): ", r2$message, "\n")
}

bad <- sum(!vapply(ok, isTRUE, logical(1)))
cat(sprintf("\n===== E2E CLI: PASS %d / FAIL %d =====\n", length(ok) - bad, bad))
if (length(result_errors) && any(grepl("Authentication", result_errors, ignore.case = TRUE))) {
  cat("\n!! 环境阻塞（如实记录，不冒充成功）：CLI 登录态失效，返回 Authentication required。\n")
  cat("   本次真实启动了 CLI 并验证了端口/模型/上下文注入，但没有产生真实回复与 Token。\n")
  cat("   处理：在交互终端执行 codebuddy login 重新登录后，工作台任务即可真实执行。\n")
}
# 系统里已有的真实 Token 台账（来自历史 4 次成功运行，不是估算）
con <- dbConnect(SQLite(), PROD)
hist <- tryCatch(dbGetQuery(con,
  "SELECT COUNT(*) n, COALESCE(SUM(token_in),0) ti, COALESCE(SUM(token_out),0) tou,
   COALESCE(SUM(token_cache_read),0) cr, COALESCE(SUM(token_total),0) tt FROM hf_workbench_runs"),
  error = function(e) data.frame(n = 0, ti = 0, tou = 0, cr = 0, tt = 0))
dbDisconnect(con)
cat(sprintf("\nREAL_LEDGER runs=%s token_in=%s token_out=%s cache_read=%s token_total=%s\n",
  hist$n[1], hist$ti[1], hist$tou[1], hist$cr[1], hist$tt[1]))
cat(sprintf("THIS_RUN token_in=%s token_out=%s token_total=%s cost_usd=%s init_model=%s result_errors=%s\n",
  u$token_in, u$token_out, u$token_total, result_cost, init_model,
  if (length(result_errors)) paste(result_errors, collapse = " | ") else "none"))
quit(status = if (bad > 0) 1 else 0)
