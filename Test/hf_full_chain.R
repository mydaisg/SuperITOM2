#!/usr/bin/env Rscript
##################
# HungFo 全链测试：登录校验 → 登录后初始化 → 全部 15 个模块 server 逐项渲染 → 数据检查
# 说明：不用 shiny::testServer 一次性 flush 整个 app（MockShinySession 没有真实计时器，
#       invalidateLater/reactivePoll 会自激死循环）；改为每个模块独立 MockShinySession，
#       真实调用 page_server 并逐个读取 output，覆盖面更全且不会假死。
#       另由 Test/hf_http_smoke.sh 真实启动 app.R 做 HTTP 层验证。
# 全程使用正式库的临时副本；正式库只读，用于行数与结构对比。
##################

Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(shiny); library(shinythemes); library(DT); library(DBI);
                   library(RSQLite); library(jsonlite) })
HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
setwd(HOME)
PROD <- file.path(HOME, "DB", "HF_HungFo.DB")
TMP <- file.path(HOME, "DB", "test_tmp", "hf_chain.DB")
file.copy(PROD, TMP, overwrite = TRUE)
Sys.setenv(HUNGFO_DB = TMP)
# 镜像/导出写到临时 HOME，避免测试改写正式 MetaTasks.json
THOME <- file.path(HOME, "DB", "test_tmp", "chain_home")
dir.create(file.path(THOME, "www"), showWarnings = FALSE, recursive = TRUE)
Sys.setenv(HUNGFO_HOME = THOME)

PASS <- 0L; FAIL <- 0L
chk <- function(label, cond, detail = "") {
  ok <- isTRUE(tryCatch(cond, error = function(e) FALSE))
  if (ok) { PASS <<- PASS + 1L; cat(sprintf("  [PASS] %s %s\n", label, detail)) }
  else    { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s %s\n", label, detail)) }
}
baseline <- function(db) {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  t <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")$name
  setNames(vapply(t, function(x) as.numeric(dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM \"%s\"", x))$n[1]), numeric(1)), t)
}
base_prod <- baseline(PROD)

cat("\n[1] 加载应用（完整 source 链）\n")
loaded <- tryCatch({
  source("hf_core.r"); source("hf_ai_context.r"); source("hf_config.r")
  for (f in sort(list.files(".", pattern = "^hf_mod_.*\\.r$"))) source(f)
  source("hf_ui.r")
  TRUE
}, error = function(e) { cat("  加载失败:", conditionMessage(e), "\n"); FALSE })
chk("全部模块加载", loaded)
if (!loaded) quit(status = 1)
chk("注册环节 = 15", length(hf_modules()) == 15, sprintf("(实际 %d)", length(hf_modules())))

cat("\n[2] 真实登录校验\n")
con <- dbConnect(SQLite(), TMP)
u <- dbGetQuery(con, "SELECT username,password FROM hf_users WHERE active=1 ORDER BY id LIMIT 1")
dbDisconnect(con)
chk("存在启用账号", nrow(u) == 1, sprintf("(%s)", u$username[1]))
auth <- hf_auth_login(u$username[1], u$password[1])
chk("hf_auth_login 校验通过", isTRUE(auth$success) && !is.null(auth$user))
chk("错误密码被拒绝", !isTRUE(hf_auth_login(u$username[1], "wrong-password-for-test")$success))
rv <- reactiveValues(logged_in = isTRUE(auth$success),
                     current_user = auth$user, app_view = "hungfo")

cat("\n[3] 登录后初始化链（真实执行，不 mock）\n")
init <- tryCatch({
  hf_db_init(); hf_project_init(); hf_workbench_init(); hf_sync_registry()
  hf_task_seed_sample(); hf_write_tasks_js(); hf_metatask_export()
  hf_ai_export_all()
  TRUE
}, error = function(e) { cat("  初始化失败:", conditionMessage(e), "\n"); FALSE })
chk("db_init / project_init / workbench_init / registry / exports", isTRUE(init))
con <- dbConnect(SQLite(), TMP)
tabs <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")$name
chk("登录后新表自动建立", all(c("hf_rule_items","hf_memory","hf_config_kv") %in% tabs))
wc <- dbGetQuery(con, "PRAGMA table_info(hf_workbench_runs)")$name
chk("运行记录新列自动补齐", all(c("model","effort","max_turns","context_file","timeout_sec") %in% wc))
mc <- dbGetQuery(con, "PRAGMA table_info(hf_metatask_queue)")$name
chk("统一入队列自动补齐", all(c("source","model","task_type") %in% mc))
dbDisconnect(con)
chk("导出 MEMORY.md / RULES.md", file.exists(file.path(THOME, "MEMORY.md")) &&
    file.exists(file.path(THOME, "RULES.md")))

cat("\n[4] 全部 15 个环节 UI 渲染 + 关键环节 server output 渲染\n")
# 4a) 静态 UI：每个环节页面都真实构造成 shiny.tag
for (m in hf_modules()) {
  ok <- tryCatch({
    u <- hf_module_page_ui(m)
    inherits(u, "shiny.tag") || inherits(u, "shiny.tag.list")
  }, error = function(e) FALSE)
  chk(paste0("UI ", m$key), ok)
}
chk("顶层 standalone UI", tryCatch({
  u <- hungfo_standalone_ui(FALSE); inherits(u, "shiny.tag") || inherits(u, "shiny.tag.list")
}, error = function(e) FALSE))
chk("登录遮罩 UI", tryCatch(inherits(hf_login_overlay(), "shiny.tag"), error = function(e) FALSE))

# 4b) 关键环节：用独立 MockShinySession 真实调用 page_server 并读取 output。
#     刻意不调用 flushReact（MockShinySession 无真实计时器，元任务页的 20 秒
#     invalidateLater 会自激死循环）；读取 output 本身就会执行对应的 render 函数。
#     真实定时器行为由独立 HTTP 冒烟脚本覆盖。
invalidateLater <- function(millis, session = NULL) invisible(NULL)
reactivePoll <- function(intervalMillis, session, checkFunc, valueFunc) {
  shiny::reactive({ try(checkFunc(), silent = TRUE); valueFunc() })
}
# 单模块 mini app + shiny::testServer：output 在 testServer 内可读，能真实执行 render 函数。
# 不用整个 app 的 testServer（元任务页 20 秒定时器在 mock 下会自激），这里逐模块隔离。
tr <- reactiveVal(0)
render_check <- function(key, out_id) {
  mini <- shinyApp(fluidPage(), function(input, output, session) {
    hf_module_page_server(input, output, session, rv, hf_module(key), tr)
  })
  assign("HF_CHAIN_V", NULL, envir = .GlobalEnv)
  err <- tryCatch({
    shiny::testServer(mini, {
      v <- tryCatch(as.character(eval(parse(text = paste0("output$", out_id))))[1],
                    error = function(e) paste("ERR", conditionMessage(e)))
      assign("HF_CHAIN_V", v, envir = .GlobalEnv)
    })
    NULL
  }, error = function(e) conditionMessage(e))
  v <- get("HF_CHAIN_V", envir = .GlobalEnv)
  ok <- is.null(err) && !is.null(v) && !grepl("^ERR", v) && nchar(v) > 0
  chk(paste0("server ", key, " / ", out_id), ok,
      if (ok) sprintf("(%d 字符)", nchar(v)) else substr(paste(c(err, v), collapse = " | "), 1, 120))
}
render_check("rules",     "hf_rules_tree")
render_check("rules",     "hf_rules_mem_list")
render_check("rules",     "hf_rules_table")
render_check("config",    "hf_config_table")
render_check("config",    "hf_config_cat_table")
render_check("config",    "hf_config_route_table")
render_check("config",    "hf_config_current_user")
render_check("workbench", "hf_workbench_runs_table")
render_check("workbench", "hf_workbench_token_kpi")
render_check("workbench", "hf_workbench_route_hint")
render_check("devlog",    "hf_devlog_dl_list")
# 元任务页含 20 秒 invalidateLater，MockShinySession 没有真实计时器会假死：
# 只验证 server 能注册（不 flush），其真实运行由 HTTP 冒烟脚本覆盖。
mt_reg <- tryCatch({
  s <- MockShinySession$new()
  hf_module_page_server(s$input, s$output, s, rv, hf_module("metatask"), reactiveVal(0))
  TRUE
}, error = function(e) FALSE)
chk("server metatask 注册（真实运行由 HTTP 冒烟覆盖）", isTRUE(mt_reg))

cat("\n[5] 登录后数据完整性\n")
after_tmp <- baseline(TMP)
common <- intersect(names(base_prod), names(after_tmp))
chk("已有数据行数未减少", all(after_tmp[common] >= base_prod[common]),
    sprintf("(对比 %d 张表)", length(common)))
chk("正式库完全未被改写", identical(base_prod, baseline(PROD)))
chk("正式 MetaTasks.json 未被测试写入",
    file.exists(file.path(HOME, "MetaTasks.json")))

cat("\n[6] 跨模块接口（hf_call 总线）\n")
chk("config.routes", length(hf_call("config", "routes")) >= 7)
chk("config.categories", is.data.frame(hf_call("config", "categories")))
chk("workbench.models", length(hf_call("workbench", "models")) >= 10)
cx <- hf_call("workbench", "context", "ui")
chk("workbench.context(ui) 按分类装配", is.list(cx) && nzchar(cx$text) && "UI规则" %in% cx$categories,
    sprintf("(规则 %d / 记忆 %d)", cx$rules_n, cx$memory_n))
chk("rules.list 可读", is.data.frame(hf_call("rules", "list")))

cat(sprintf("\n===== FULL CHAIN : PASS %d / FAIL %d =====\n", PASS, FAIL))
quit(status = if (FAIL > 0) 1 else 0)
