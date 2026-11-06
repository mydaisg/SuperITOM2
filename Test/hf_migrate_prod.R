#!/usr/bin/env Rscript
##################
# HungFo 正式库幂等迁移：对 PROD/HF_HungFo.DB 执行 hf_db_init() + hf_workbench_init()
# 补齐本次新增加的表 / 列（hf_rule_items / hf_memory / hf_config_kv / hf_rules 新列 /
# hf_rules_categories 新列 / hf_workbench_runs 的 token_* 与 cost_usd）。
# 仅建表/加列/种子，绝不删除或改写已有业务数据。
##################
Sys.setlocale("LC_ALL", "Chinese (Simplified)_China.65001")
suppressMessages({ library(DBI); library(RSQLite); library(jsonlite) })

HOME <- "D:/GitHub/SuperITOM2/Script/hungfo"
PROD <- file.path(HOME, "DB", "HF_HungFo.DB")
if (!file.exists(PROD)) { cat("PROD DB 不存在:", PROD, "\n"); quit(status = 1) }

Sys.setenv(HUNGFO_DB = PROD)
setwd(HOME)
source("hf_core.r")
source("hf_ai_context.r")
source("hf_config.r")
source("hf_mod_workbench.r")

cat("== 迁移前：表清单 ==\n")
before <- suppressWarnings(dbGetQuery(hf_db_connect(),
  "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name"))
cat(paste(before$name, collapse = ", "), "\n")
hf_db_disconnect(hf_db_connect())

cat("\n== 执行 hf_db_init() ==\n")
hf_db_init()
cat("== 执行 hf_workbench_init() ==\n")
hf_workbench_init()

cat("\n== 迁移后：校验关键对象 ==\n")
con <- hf_db_connect()
chk <- function(label, ok) cat(sprintf("  [%-4s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label))
tabs <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'")$name
chk("hf_memory 表存在", "hf_memory" %in% tabs)
chk("hf_config_kv 表存在", "hf_config_kv" %in% tabs)
chk("hf_rule_items 表存在", "hf_rule_items" %in% tabs)
rc <- dbGetQuery(con, "PRAGMA table_info(hf_rules_categories)")$name
chk("hf_rules_categories.applies_to 列存在", "applies_to" %in% rc)
chk("hf_rules_categories.model 列存在", "model" %in% rc)
chk("hf_rules_categories.effort 列存在", "effort" %in% rc)
chk("hf_rules_categories.max_turns 列存在", "max_turns" %in% rc)
chk("hf_rules_categories.enabled 列存在", "enabled" %in% rc)
chk("hf_rules_categories.always_apply 列存在", "always_apply" %in% rc)
rr <- dbGetQuery(con, "PRAGMA table_info(hf_rules)")$name
chk("hf_rules.parent_id 列存在", "parent_id" %in% rr)
chk("hf_rules.task_type 列存在", "task_type" %in% rr)
wr <- dbGetQuery(con, "PRAGMA table_info(hf_workbench_runs)")$name
chk("hf_workbench_runs.cost_usd 列存在", "cost_usd" %in% wr)
chk("hf_workbench_runs.token_total 列存在", "token_total" %in% wr)
chk("hf_workbench_runs.model 列存在", "model" %in% wr)
chk("hf_workbench_runs.task_type 列存在", "task_type" %in% wr)
cat("\n== 迁移后：表总数 =", length(tabs), "==\n")
hf_db_disconnect(con)
cat("迁移完成。\n")
quit(status = 0)
