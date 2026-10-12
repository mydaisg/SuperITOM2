##################
# HungFo 环节 08 — 元任务 (HF-METATASK)
# 职责：任务的任务——描述、生成、编排其它环节的任务；驱动自动化闭环。
##################

HF_MOD_METATASK <- list(
  id = "hf08", key = "metatask", name = "元任务", icon = "code-branch",
  desc = "『任务的任务』：描述、生成并编排其它环节的任务，驱动自动化闭环。",
  api_desc = "run(payload) 执行元任务 · fetch() 取元任务 · dispatch(task) 派发",
  status = "scaffold", version = "v0.1"
)

# 派发（占位实现）
hf_metatask_dispatch <- function(task = NULL) {
  list(success = TRUE, task = task, message = "元任务派发接口（占位）")
}

hf_register(HF_MOD_METATASK, hf_make_api("metatask", dispatch = hf_metatask_dispatch))
