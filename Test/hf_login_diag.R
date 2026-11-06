#!/usr/bin/env Rscript
##################
# 登录故障诊断：在【当前正式库的副本】上执行登录链路
#  1) hf_db_init() / hf_workbench_init() 是否抛错（登录时必调）
#  2) testServer 模拟点击 hf_login_btn，观察 rv$logged_in / 通知 / 错误
##################
Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(shiny); library(shinythemes); library(DT); library(DBI); library(RSQLite); library(jsonlite) })

HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
PROD <- file.path(HOME, "DB", "HF_HungFo.DB")
TMP <- file.path(HOME, "DB", "test_tmp", "hf_login_diag.DB")
THOME <- file.path(HOME, "DB", "test_tmp", "login_diag_home")
dir.create(file.path(THOME, "www"), showWarnings = FALSE, recursive = TRUE)
file.copy(list.files(file.path(HOME, "www"), full.names = TRUE), file.path(THOME, "www"), overwrite = TRUE)
file.copy(PROD, TMP, overwrite = TRUE)
Sys.setenv(HUNGFO_DB = TMP, HUNGFO_HOME = THOME)
setwd(HOME)

cat("== 0) 加载模块 ==\n")
source("hf_core.r"); source("hf_ai_context.r"); source("hf_config.r")
for (f in sort(list.files(".", pattern = "^hf_mod_.*\\.r$"))) source(f)
source("hf_ui.r")
cat("loaded OK\n")

cat("\n== 1) 登录时调用的 hf_db_init() 是否抛错 ==\n")
t0 <- Sys.time()
r1 <- tryCatch({ hf_db_init(); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("hf_db_init:", r1, " elapsed=", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s\n")
r2 <- tryCatch({ hf_workbench_init(); "OK" }, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("hf_workbench_init:", r2, "\n")

cat("\n== 2) 直接验证 hf_auth_login 凭据 ==\n")
u <- tryCatch(hf_auth_login("DaiSG", ""), error = function(e) list(success = FALSE, message = paste("ERROR:", conditionMessage(e))))
cat("empty pwd:", paste(names(u), collapse = ","), " success=", isTRUE(u$success), " msg=", hf_or(u$message, ""), "\n")
# 查一下真实用户名与密码存储形式（不打印密码明文）
con <- hf_db_connect()
users <- tryCatch(dbGetQuery(con, "SELECT id, username, display_name, active FROM hf_users"), error = function(e) data.frame())
cat("users:\n"); print(users)
pcols <- dbGetQuery(con, "PRAGMA table_info(hf_users)")$name
cat("hf_users cols:", paste(pcols, collapse = ", "), "\n")
hf_db_disconnect(con)

cat("\n== 3) testServer 模拟点击登录 ==\n")
rv <- reactiveValues(logged_in = FALSE, current_user = NULL, app_view = "hungfo")
app <- shinyApp(
  ui = hungfo_standalone_ui(show_back = FALSE),
  server = function(input, output, session) hungfo_server(input, output, session, rv)
)
res <- tryCatch({
  shiny::testServer(app, {
    session$setInputs(hf_login_username = "DaiSG", hf_login_password = "")
    session$setInputs(hf_login_btn = 1)
    cat("  after click: logged_in =", isTRUE(rv$logged_in), "\n")
  })
  "testServer OK"
}, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("testServer:", res, "\n")
cat("final logged_in =", isTRUE(rv$logged_in), "\n")
cat("DONE\n")
