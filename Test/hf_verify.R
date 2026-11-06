#!/usr/bin/env Rscript
##################
# HungFo 全链验证脚本（阶段性扩展）
# 用法：Rscript Test/hf_verify.R [stage1|stage2|stage3|full]
#
# 安全约束：
#  - 一律把正式库复制成临时库，设 HUNGFO_DB 指向临时库后再跑，绝不写正式库
#  - 只有导出类功能会写 hf_home()/.codebuddy/rules 与 MEMORY.md / RULES.md（均为新增文件）
#  - 结束时输出 PASS/FAIL 汇总；有 FAIL 时退出码 1
##################

Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(shiny); library(shinythemes); library(DT); library(DBI);
                   library(RSQLite); library(jsonlite) })

HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
setwd(HOME)
args <- commandArgs(trailingOnly = TRUE)
STAGE <- if (length(args) > 0) args[1] else "full"

PASS <- 0L; FAIL <- 0L; NOTES <- character(0)
chk <- function(label, cond, detail = "") {
  ok <- isTRUE(tryCatch(cond, error = function(e) FALSE))
  if (ok) { PASS <<- PASS + 1L; cat(sprintf("  [PASS] %s %s\n", label, detail)) }
  else    { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s %s\n", label, detail)) }
  invisible(ok)
}
note <- function(x) { NOTES <<- c(NOTES, x); cat("  [NOTE] ", x, "\n", sep = "") }

# ── 0) 临时库 ──
PROD <- file.path(HOME, "DB", "HF_HungFo.DB")
TMPDIR <- file.path(HOME, "DB", "test_tmp")
dir.create(TMPDIR, showWarnings = FALSE, recursive = TRUE)
TMP <- file.path(TMPDIR, "hf_verify.DB")
if (!file.copy(PROD, TMP, overwrite = TRUE)) stop("无法复制正式库为临时库")
Sys.setenv(HUNGFO_DB = TMP)
cat("== 临时库: ", TMP, " ==\n", sep = "")

# 记录基线行数（正式库）
baseline <- function(db) {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  tbls <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")$name
  setNames(vapply(tbls, function(t) as.numeric(dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM \"%s\"", t))$n[1]), numeric(1)), tbls)
}
base_before <- baseline(PROD)

# ── 1) 语法 ──
cat("\n[1] 全量语法检查\n")
rfiles <- list.files(HOME, pattern = "\\.r$", full.names = TRUE)
syntax_bad <- character(0)
for (f in rfiles) {
  r <- tryCatch({ parse(file = f); TRUE }, error = function(e) FALSE)
  if (!r) syntax_bad <- c(syntax_bad, basename(f))
}
chk("所有 .r 文件语法", length(syntax_bad) == 0,
    sprintf("(%d 个文件, 失败: %s)", length(rfiles),
            if (length(syntax_bad)) paste(syntax_bad, collapse = ",") else "无"))
srv <- tryCatch({ parse(file = "D:/GitHub/SuperITOM2/server.R"); TRUE }, error = function(e) FALSE)
chk("父项目 server.R 语法", srv)

# ── 2) 加载全部模块（模拟 app.R，不启动 shinyApp）──
cat("\n[2] 加载全部模块\n")
load_ok <- tryCatch({
  source("hf_core.r")
  source("hf_ai_context.r")
  source("hf_config.r")
  for (f in sort(list.files(".", pattern = "^hf_mod_.*\\.r$"))) source(f)
  source("hf_ui.r")
  TRUE
}, error = function(e) { note(paste("加载失败:", conditionMessage(e))); FALSE })
chk("core/ai_context/config/mod_*/ui 全部 source", load_ok)
if (!load_ok) { cat("加载失败，终止\n"); quit(status = 1) }
chk("注册表模块数 >= 14", length(hf_modules()) >= 14, sprintf("(实际 %d)", length(hf_modules())))
chk("关键函数存在", all(vapply(c("hf_ai_context_text", "hf_memory_add", "hf_model_routes",
                                 "hf_rule_item_add", "hf_workbench_build_args",
                                 "hf_workbench_cancel", "hf_ai_export_all"), exists, logical(1))))
chk("内嵌模式所需函数 hf_workbench_init 存在", exists("hf_workbench_init"))

# ── 3) 数据库迁移与数据完整性 ──
cat("\n[3] 数据库迁移与已有数据完整性\n")
init_ok <- tryCatch({ hf_db_init(); hf_project_init(); hf_workbench_init(); TRUE },
                    error = function(e) { note(paste("init 失败:", conditionMessage(e))); FALSE })
chk("hf_db_init/hf_project_init/hf_workbench_init", init_ok)
con <- dbConnect(SQLite(), TMP)
cols <- function(t) dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name
tbls <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")$name
chk("新表 hf_rule_items", "hf_rule_items" %in% tbls)
chk("新表 hf_memory", "hf_memory" %in% tbls)
chk("新表 hf_config_kv", "hf_config_kv" %in% tbls)
cc <- cols("hf_rules_categories")
chk("分类新列齐全", all(c("applies_to","model","effort","max_turns","enabled","always_apply","description") %in% cc))
rc <- cols("hf_rules")
chk("规则新列齐全", all(c("parent_id","task_type") %in% rc))
mc <- cols("hf_metatask_queue")
chk("元任务新列齐全", all(c("model","task_type","source") %in% mc))
wc <- cols("hf_workbench_runs")
chk("运行记录新列齐全", all(c("model","effort","fallback_model","max_turns","task_type",
                             "matched_categories","context_file","context_rules","context_memory",
                             "cwd","timeout_sec","exit_code","cost_usd","num_turns","duration_ms") %in% wc))
after <- baseline(TMP)
common <- intersect(names(base_before), names(after))
chk("已有表行数未减少", all(after[common] >= base_before[common]),
    sprintf("(对比 %d 张表)", length(common)))
dbDisconnect(con)

# ── 4) 通用配置与模型路由 ──
cat("\n[4] 配置 KV 与模型路由\n")
r <- hf_cfg_set("verify_key", list(a = 1, b = "中文"), desc = "验证")
chk("hf_cfg_set", isTRUE(r$success))
chk("hf_cfg_get 往返", identical(as.character(hf_cfg_get("verify_key")$b), "中文"))
chk("hf_cfg_list 含 key", "verify_key" %in% hf_cfg_list()$cfg_key)
rs <- hf_model_routes()
chk("默认路由 7 条", length(rs) == 7, sprintf("(实际 %d)", length(rs)))
ui_rt <- hf_model_route_get("ui")
chk("UI 任务走便宜模型", identical(as.character(ui_rt$model), "fast-model"),
    sprintf("(model=%s, effort=%s)", ui_rt$model, ui_rt$effort))
chk("路由分类=UI规则", identical(as.character(ui_rt$categories), "UI规则"))
chk("未知任务回退 default", identical(as.character(hf_model_route_get("not_exist")$task_type), "default"))
rs2 <- rs; rs2[[3]]$model <- "custom-local:Tai_DeepSeek-r1:1.5b"
chk("hf_model_routes_save", isTRUE(hf_model_routes_save(rs2)$success))
chk("路由保存后可读回", identical(as.character(hf_model_route_get("bugfix")$model),
                                  "custom-local:Tai_DeepSeek-r1:1.5b"))
chk("恢复默认路由", isTRUE(hf_model_routes_save(HF_DEFAULT_ROUTES)$success))

# ── 5) 模型清单与 CLI 参数构造 ──
cat("\n[5] 模型清单与 CLI 参数\n")
ids <- hf_workbench_models(force = TRUE)
chk("模型清单非空", length(ids) >= 10, sprintf("(%d 个, 含 fast-model=%s)", length(ids), "fast-model" %in% ids))
chk("模型下拉含 __none__", "__none__" %in% unname(hf_workbench_model_choices()))
spec <- list(command = "node", pre = "/c/fake/codebuddy")
a <- hf_workbench_build_args(spec, "修复一个 bug", list(
  model = "fast-model", effort = "low", max_turns = 12,
  fallback_model = "deep-model", system_prompt_file = "/tmp/ctx.md"))
chk("参数含 --model", "--model" %in% a && "fast-model" %in% a)
chk("参数含 --effort", "--effort" %in% a && "low" %in% a)
chk("参数含 --max-turns", "--max-turns" %in% a && "12" %in% a)
chk("参数含 --fallback-model", "--fallback-model" %in% a)
chk("参数含 --system-prompt-file", "--system-prompt-file" %in% a && "/tmp/ctx.md" %in% a)
chk("保留 -p/-y/stream-json", all(c("-p","-y","--output-format","stream-json") %in% a))
a0 <- hf_workbench_build_args(spec, "普通任务", list())
chk("空选项时不注入模型参数", !any(c("--model","--effort","--max-turns","--system-prompt-file") %in% a0))
ap <- hf_workbench_build_args(spec, "任务", list(port = 62631))
chk("参数含 --port（守护端口冲突修复）", "--port" %in% ap && "62631" %in% ap)
fp <- hf_workbench_free_port()
chk("空闲端口探测可用", is.numeric(fp) && fp > 0 && fp < 65536, sprintf("(选中 %s)", fp))
note(paste("真实 CLI 参数示例:", paste(a, collapse = " ")))

# ── 6) 规则分类元数据与按分类装配 ──
cat("\n[6] 分类元数据与 AI 上下文装配\n")
cdf <- hf_rules_categories_df()
chk("分类表可读", nrow(cdf) >= 1, sprintf("(%d 个分类)", nrow(cdf)))
cid <- cdf$id[1]
chk("hf_rules_category_set_meta", isTRUE(hf_rules_category_set_meta(
  cid, applies_to = "new_module,bugfix", model = "fast-model", effort = "low",
  max_turns = 15, enabled = 1, always_apply = 1, description = "验证用")$success))
cdf2 <- hf_rules_categories_df()
row <- cdf2[cdf2$id == cid, ]
chk("元数据写回正确", identical(as.character(row$applies_to), "new_module,bugfix") &&
    identical(as.character(row$model), "fast-model") && as.integer(row$always_apply) == 1)
cats <- hf_ai_categories_for("bugfix")
chk("按任务类型匹配分类", length(cats) >= 1 && row$name %in% cats,
    sprintf("(匹配: %s)", paste(cats, collapse = "、")))
ctx <- hf_ai_context_text(task_type = "bugfix")
chk("上下文文本含规则段", grepl("必须遵守的规则", ctx$text, fixed = TRUE))
chk("上下文文本含记忆段", grepl("项目与 AI 记忆", ctx$text, fixed = TRUE))
chk("上下文文本含五段式纪律", grepl("【需求】【分析】【方案】【结果】【记忆】", ctx$text, fixed = TRUE))
chk("上下文命中分类", length(ctx$categories) >= 1, sprintf("(规则 %d 条 / 记忆 %d 条)", ctx$rules_n, ctx$memory_n))
cf <- hf_ai_context_write(ctx$text, "verify_run")
chk("上下文文件写入", !is.null(cf) && file.exists(cf))

# ── 7) 导出（移植包）──
cat("\n[7] 规则与记忆导出\n")
ex <- tryCatch(hf_ai_export_all(), error = function(e) NULL)
chk("hf_ai_export_all 成功", !is.null(ex) && isTRUE(ex$rules$success) && isTRUE(ex$memory$success))
chk("生成 MEMORY.md", file.exists(file.path(HOME, "MEMORY.md")))
chk("生成 RULES.md", file.exists(file.path(HOME, "RULES.md")))
gen <- list.files(file.path(HOME, ".codebuddy", "rules"), pattern = "^hf-gen-.*\\.md$", full.names = TRUE)
chk("生成 hf-gen-* 规则文件", length(gen) >= 1, sprintf("(%d 个)", length(gen)))
chk("生成索引文件", file.exists(file.path(HOME, ".codebuddy", "rules", "hf-gen-index.md")))

# ── 8) UI 渲染（所有专属页面）──
cat("\n[8] UI 渲染\n")
ui_ok <- function(key) {
  m <- hf_module(key)
  r <- tryCatch({
    u <- hf_module_page_ui(m)
    inherits(u, "shiny.tag") || inherits(u, "shiny.tag.list")
  }, error = function(e) { note(paste0("UI ", key, " 失败: ", conditionMessage(e))); FALSE })
  r
}
for (k in c("rules","config","workbench","project","permission","metatask","devlog","arch","autotask","crawl"))
  chk(paste0("UI 渲染 ", k), ui_ok(k))
standalone <- tryCatch({
  u <- hungfo_standalone_ui(FALSE)
  inherits(u, "shiny.tag") || inherits(u, "shiny.tag.list")
}, error = function(e) FALSE)
chk("顶层 hungfo_standalone_ui 渲染", standalone)
login <- tryCatch(inherits(hf_login_overlay(), "shiny.tag"), error = function(e) FALSE)
chk("登录遮罩渲染", login)

# ── 9) 阶段二：层级规则修订 + 记忆 ──
cat("\n[9] 层级规则修订与项目记忆\n")
ra <- hf_rules_add("验证规则-层级", "架构规则", "原始内容 v0")
chk("hf_rules_add", isTRUE(ra$success))
con <- dbConnect(SQLite(), TMP)
rid <- dbGetQuery(con, sprintf("SELECT id FROM hf_rules WHERE rule_no='%s'", ra$rule_no))$id[1]
dbDisconnect(con)
chk("新增规则可查", length(rid) == 1 && !is.na(rid))
r1 <- hf_rule_item_add(rid, "第一版修订：新增模块必须写规则", created_by_name = "verify")
r2 <- hf_rule_item_add(rid, "第二版修订：修复功能只需读架构规则", created_by_name = "verify")
chk("追加修订 v1/v2", isTRUE(r1$success) && isTRUE(r2$success))
chk("修订数量 = 2", hf_rule_item_count(rid) == 2)
cur <- hf_rule_current(rid, "原始内容")
chk("当前生效 = 最新修订", grepl("第二版", cur$content, fixed = TRUE) && cur$revisions == 2)
chk("历史修订完整保留", nrow(hf_rule_items_list(rid)) == 2)
tr <- hf_rule_tree()
chk("层级树含分类一级节点", length(tr) >= 1, sprintf("(%d 个分类)", length(tr)))
chk("层级树含规则二级节点", any(vapply(tr, function(ct) ct$rule_count > 0, logical(1))))
has_items <- any(vapply(tr, function(ct) {
  if (ct$rule_count == 0) return(FALSE)
  any(vapply(ct$rules, function(r) !is.null(r$items) && nrow(r$items) > 0, logical(1)))
}, logical(1)))
chk("层级树含修订三级节点", has_items)
ctx2 <- hf_ai_context_text(task_type = "bugfix")
chk("上下文读到最新修订内容", grepl("第二版修订", ctx2$text, fixed = TRUE))

ma <- hf_memory_add("验证-项目事实", "项目事实", "HungFo 使用独立库 HF_HungFo.DB，与父项目 GH_ITOM.db 解耦",
                    source = "verify", tags = "bugfix,arch")
chk("hf_memory_add", isTRUE(ma$success))
mdf <- hf_memory_list(limit = 100)
mid <- mdf$id[match("验证-项目事实", mdf$topic)]
chk("记忆可查", length(mid) == 1 && !is.na(mid))
mr <- hf_memory_revise(mid, "补充：规则与记忆导出为 MEMORY.md / RULES.md 便于移植", created_by_name = "verify")
chk("hf_memory_revise", isTRUE(mr$success))
mdf2 <- hf_memory_list(limit = 100)
chk("修订后新增一行（append-only）", sum(mdf2$topic == "验证-项目事实") == 2)
chk("修订链 parent_id 指向根", any(!is.na(mdf2$parent_id) & mdf2$parent_id == mid))
ctx3 <- hf_ai_context_text(task_type = "bugfix")
chk("上下文含记忆段内容", grepl("项目事实", ctx3$text, fixed = TRUE) && ctx3$memory_n >= 1,
    sprintf("(记忆 %d 条)", ctx3$memory_n))
md <- hf_memory_deprecate(mid)
chk("停用记忆保留数据", isTRUE(md$success))
chk("停用后列表不再返回该行", !(mid %in% hf_memory_list(limit = 100)$id))
ex2 <- hf_memory_export_md()
chk("MEMORY.md 含新增记忆", file.exists(ex2$file) &&
    grepl("验证-项目事实", paste(readLines(ex2$file, warn = FALSE), collapse = "\n"), fixed = TRUE))

# UI 结构：规则模块内子 Tab（不是独立导航模块）
rules_html <- as.character(hf_rules_page_ui(hf_module("rules")))
chk("规则页含「记忆」子 Tab", grepl("记忆", rules_html, fixed = TRUE))
chk("规则页含层级视图", grepl("层级视图", rules_html, fixed = TRUE))
chk("规则页不再含分类管理卡片", !grepl("规则分类管理", rules_html, fixed = TRUE))
cfg_html <- as.character(hf_config_page_ui(hf_module("config")))
chk("配置页含规则分类子 Tab", grepl("规则分类", cfg_html, fixed = TRUE))
chk("配置页含模型路由子 Tab", grepl("模型路由", cfg_html, fixed = TRUE))
chk("导航模块数未因新功能增加", length(hf_modules()) == 15)
ui_src <- paste(readLines(file.path(HOME, "hf_ui.r"), warn = FALSE), collapse = "\n")
chk("devlog 五段式含「记忆」配色", grepl('"记忆" = list', ui_src, fixed = TRUE))

# ── 10) 阶段三：元任务 CLI / 统一入口 / 五段式 / 按分类上下文 ──
cat("\n[10] 元任务 CLI 与统一入口\n")
official_mirror <- file.path(HOME, "MetaTasks.json")
mirror_before <- if (file.exists(official_mirror)) paste(readLines(official_mirror, warn = FALSE), collapse = "\n") else ""
RS <- file.path(R.home("bin"), "Rscript.exe")
RUNNER <- file.path(HOME, "hf_metatask_runner.r")
THOME <- file.path(TMPDIR, "home")
dir.create(THOME, showWarnings = FALSE, recursive = TRUE)
old_home <- Sys.getenv("HUNGFO_HOME", unset = "")
Sys.setenv(HUNGFO_HOME = THOME)
run_cli <- function(...) {
  a <- c(shQuote(RUNNER), vapply(list(...), shQuote, character(1)))
  out <- tryCatch(suppressWarnings(system2(RS, a, stdout = TRUE, stderr = TRUE)),
                  error = function(e) character(0))
  lines <- as.character(out)
  # Rscript 启动时可能输出 locale 警告行，只保留从第一个 JSON 起始行开始的内容
  st <- grep("^\\s*\\{", lines)
  txt <- if (length(st) > 0) paste(lines[st[1]:length(lines)], collapse = "\n")
         else paste(lines, collapse = "\n")
  list(txt = txt, json = tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                                  error = function(e) NULL))
}
p0 <- run_cli("peek")
chk("runner peek 可解析 JSON", !is.null(p0$json) && !is.null(p0$json$due),
    sprintf("(到期 %d 条)", if (is.null(p0$json$due)) 0 else length(p0$json$due)))

en <- run_cli("enqueue", "验证统一入口任务", "从微信入口提交的一条验证任务",
              "--source", "wecom", "--model", "fast-model", "--tasktype", "ui")
chk("runner enqueue 成功", !is.null(en$json) && isTRUE(en$json$success),
    if (is.null(en$json)) en$txt else sprintf("(id=%s, source=%s)", en$json$id, en$json$source))
new_id <- if (is.null(en$json)) NULL else en$json$id

norm_due <- function(x) {
  if (is.null(x)) return(list())
  # auto_unbox=TRUE 时单元素数组会退化成对象，统一包回 list
  if (!is.null(names(x)) && "id" %in% names(x)) return(list(x))
  x
}
p1 <- run_cli("peek")
due1 <- norm_due(if (is.null(p1$json)) NULL else p1$json$due)
chk("peek 输出含 source/model/task_type", length(due1) >= 1 &&
    any(vapply(due1, function(d) identical(as.character(d$source), "wecom") &&
               identical(as.character(d$model), "fast-model") &&
               identical(as.character(d$task_type), "ui"), logical(1))),
    sprintf("(到期 %d 条)", length(due1)))

cx <- run_cli("context", "ui")
chk("runner context 生成 prompt 文件", !is.null(cx$json) && isTRUE(cx$json$success) &&
    file.exists(as.character(cx$json$path)),
    if (is.null(cx$json)) cx$txt else sprintf("(规则 %s 条 / 记忆 %s 条)", cx$json$rules, cx$json$memory))
if (!is.null(cx$json) && file.exists(as.character(cx$json$path))) {
  ctext <- paste(readLines(as.character(cx$json$path), warn = FALSE), collapse = "\n")
  chk("上下文含规则段与记忆段", grepl("必须遵守的规则", ctext, fixed = TRUE) &&
      grepl("项目与 AI 记忆", ctext, fixed = TRUE))
  chk("上下文按 UI 分类装配", grepl("UI规则|匹配分类", ctext))
}

l5 <- run_cli("log5", "验证五段式", "需求文本", "分析文本", "方案文本",
              "结果文本含 Token", "记忆文本：规则与记忆要可移植")
chk("runner log5 写入成功", !is.null(l5$json) && isTRUE(l5$json$success))
con <- dbConnect(SQLite(), TMP)
lg <- dbGetQuery(con, "SELECT payload FROM hf_records WHERE module_key='devlog' AND title='验证五段式' ORDER BY id DESC LIMIT 1")
dbDisconnect(con)
chk("devlog 五段齐全", nrow(lg) == 1 && all(vapply(c("需求","分析","方案","结果","记忆"),
    function(s) grepl(paste0("【", s, "】"), lg$payload[1], fixed = TRUE), logical(1))))

l4 <- run_cli("log", "验证自动补段", "【需求】A\n【分析】B\n【方案】C\n【结果】D")
chk("runner log 缺段自动补齐", !is.null(l4$json) && isTRUE(l4$json$success))
con <- dbConnect(SQLite(), TMP)
lg2 <- dbGetQuery(con, "SELECT payload FROM hf_records WHERE module_key='devlog' AND title='验证自动补段' ORDER BY id DESC LIMIT 1")
dbDisconnect(con)
chk("补齐后含【记忆】段", nrow(lg2) == 1 && grepl("【记忆】", lg2$payload[1], fixed = TRUE))

if (!is.null(new_id)) {
  dn <- run_cli("done", as.character(new_id), "验证完成", "--model", "fast-model",
                "--tin", "120", "--tout", "60", "--dur", "1500", "--sid", "verify-sid")
  chk("runner done 上报模型与 Token", !is.null(dn$json) && isTRUE(dn$json$success))
  con <- dbConnect(SQLite(), TMP)
  ss <- dbGetQuery(con, sprintf("SELECT model,llm_name,token_in,token_out FROM hf_task_sessions WHERE task_id=%s ORDER BY id DESC LIMIT 1", new_id))
  dbDisconnect(con)
  chk("会话表写入指定模型", nrow(ss) == 1 && identical(as.character(ss$model[1]), "fast-model") &&
      as.integer(ss$token_in[1]) == 120)
}
mirror_after <- if (file.exists(official_mirror)) paste(readLines(official_mirror, warn = FALSE), collapse = "\n") else ""
chk("正式 MetaTasks.json 未被测试写入", identical(mirror_before, mirror_after))
Sys.setenv(HUNGFO_HOME = old_home)

# 规则导出与移植包在阶段三的最终状态
chk("hf-gen-index.md 含五段式纪律",
    grepl("【需求】【分析】【方案】【结果】【记忆】",
      paste(readLines(file.path(HOME, ".codebuddy", "rules", "hf-gen-index.md"), warn = FALSE), collapse = "\n"),
      fixed = TRUE))
trig <- paste(readLines("D:/GitHub/SuperITOM2/.codebuddy/rules/05-metatask-trigger.md", warn = FALSE), collapse = "\n")
chk("触发规则已改五段式", grepl("【需求】【分析】【方案】【结果】【记忆】", trig, fixed = TRUE))
chk("触发规则已修正 DB 路径", grepl("Script/hungfo/DB/HF_HungFo.DB", trig, fixed = TRUE))
chk("触发规则含统一入口 enqueue", grepl("enqueue", trig, fixed = TRUE) && grepl("wecom", trig, fixed = TRUE))
chk("触发规则含按分类 context", grepl("context", trig, fixed = TRUE))

# ── 11) 防复发：reactive 自激防护（登录无响应 / CPU 100% 事故）──
# 事故根因：裸 observe 内调用 local_refresh()（读+写同一 reactiveVal）会订阅自身写入的值，
# 导致每轮 flush 立即失效重跑，Shiny timerCallbacks 表被 rbind 无限撑大 → CPU 100%、
# 页面点击无响应且后台无任何报错。修复：定时器 observe 整体 isolate + 所有 refresh 用 isolate 读。
ui_src2 <- paste(readLines(file.path(HOME, "hf_ui.r"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
cfg_src2 <- paste(readLines(file.path(HOME, "hf_mod_config.r"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_lines2 <- strsplit(ui_src2, "\n")[[1]]
 timer_line <- grep("invalidateLater\\(20000, session\\)", ui_lines2)[1]
chk("元任务 20s observe 已 isolate",
    length(timer_line) == 1 && !is.na(timer_line) &&
    any(grepl("isolate\\(\\{", ui_lines2[max(1, timer_line - 2):min(length(ui_lines2), timer_line + 3)])))
chk("hf_ui.r 无未隔离的 trigger 自增",
    !grepl("[a-z_]+_trigger\\([a-z_]+_trigger\\(\\) \\+ 1\\)", ui_src2))
chk("hf_mod_config.r 无未隔离的 trigger 自增",
    !grepl("[a-z_]+_trigger\\([a-z_]+_trigger\\(\\) \\+ 1\\)", cfg_src2))
chk("全项目 refresh 均使用 isolate 读",
    length(grep("_refresh <- function\\(\\) [a-z_]+_trigger\\(isolate\\(",
                c(strsplit(ui_src2, "\n")[[1]], strsplit(cfg_src2, "\n")[[1]]))) >= 14)

# ── 汇总 ──
cat(sprintf("\n===== STAGE %s : PASS %d / FAIL %d =====\n", STAGE, PASS, FAIL))
if (length(NOTES)) { cat("备注:\n"); for (n in NOTES) cat("  - ", n, "\n", sep = "") }
quit(status = if (FAIL > 0) 1 else 0)
