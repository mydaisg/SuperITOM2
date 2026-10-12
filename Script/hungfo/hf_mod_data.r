##################
# HungFo 环节 03 — 数据 (HF-DATA)
# 职责：数据落地 / 清洗 / 存储 / 供数，是各环节之间的数据枢纽。
##################

HF_MOD_DATA <- list(
  id = "hf03", key = "data", name = "数据", icon = "database",
  desc = "原始数据落地、清洗、存储与供数，作为各环节之间的数据枢纽。",
  api_desc = "run(payload) 入库/清洗 · fetch() 取数据 · store(name, df) 存储",
  status = "scaffold", version = "v0.1"
)

# 存储一份数据（占位实现）
hf_data_store <- function(name = NULL, value = NULL) {
  list(success = TRUE, name = name, message = "数据存储接口（占位）")
}

hf_register(HF_MOD_DATA, hf_make_api("data", store = hf_data_store))
