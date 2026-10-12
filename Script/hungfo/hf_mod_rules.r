##################
# HungFo 环节 01 — 规则 (HF-RULES)
# 职责：定义 / 存储 / 解析业务规则，为其它环节提供规则判定接口。
##################

HF_MOD_RULES <- list(
  id = "hf01", key = "rules", name = "规则", icon = "gavel",
  desc = "定义、存储、解析业务规则，供其它环节调用（爬取频率、算法参数、风控阈值等）。",
  api_desc = "run(payload) 执行规则集 · fetch() 取规则 · evaluate(rule, data) 判定",
  status = "scaffold", version = "v0.1"
)

# 规则判定（占位实现，后续接入真实规则引擎）
hf_rules_evaluate <- function(rule = NULL, data = NULL) {
  list(success = TRUE, rule = rule, passed = TRUE, message = "规则判定接口（占位）")
}

hf_register(HF_MOD_RULES, hf_make_api("rules", evaluate = hf_rules_evaluate))
