##################
# HungFo 环节 01 — 规则 (HF-RULES)
# 职责：定义 / 存储 / 解析业务规则，为其它环节提供规则判定接口。
##################

HF_MOD_RULES <- list(
  id = "hf01", key = "rules", name = "规则", icon = "gavel",
  desc = "定义、存储、解析业务规则，供其它环节调用（爬取频率、算法参数、风控阈值等）。",
  api_desc = "fetch() 取规则 · evaluate(rule, data) 判定 · list/add/update/delete 维护规则 · categories 及 category_add/update 维护分类",
  status = "scaffold", version = "v0.2"
)

# 规则判定（占位实现，后续接入真实规则引擎）
hf_rules_evaluate <- function(rule = NULL, data = NULL) {
  list(success = TRUE, rule = rule, passed = TRUE, message = "规则判定接口（占位）")
}

# 规则分类（透传到 hf_core 的分类数据层）
hf_rules_api_categories <- function() hf_rules_categories()
hf_rules_api_category_add    <- function(name) hf_rules_category_add(name)
hf_rules_api_category_update <- function(id, name) hf_rules_category_update(id, name)

# 规则数据层透传接口
hf_rules_api_list   <- function(category = NULL) hf_rules_list(category)
hf_rules_api_get    <- function(id) hf_rules_get(id)
hf_rules_api_add    <- function(title, category = "", payload = "", created_by = NULL) hf_rules_add(title, category, payload, created_by)
hf_rules_api_update <- function(id, title, category = "", payload = "") hf_rules_update(id, title, category, payload)
hf_rules_api_delete <- function(id) hf_rules_delete(id)

hf_register(HF_MOD_RULES, hf_make_api(
  "rules",
  evaluate = hf_rules_evaluate,
  categories = hf_rules_api_categories,
  category_add = hf_rules_api_category_add,
  category_update = hf_rules_api_category_update,
  list = hf_rules_api_list,
  get = hf_rules_api_get,
  add = hf_rules_api_add,
  update = hf_rules_api_update,
  delete = hf_rules_api_delete
))
