##################
# HungFo 环节 08 — 元任务 (HF-METATASK)
# 职责：任务的任务——描述、生成、编排其它环节的任务；驱动自动化闭环。
# 本模块是 CodeBuddy 定时任务的任务来源：在此添加「任务记录」，
# CodeBuddy 定时任务按 cron 读取 MetaTasks.json 镜像，发现到期任务直接执行，
# 并把执行过程（分析/方案/结果）写入开发日志（devlog）。
##################

HF_MOD_METATASK <- list(
  id = "hf08", key = "metatask", name = "元任务", icon = "code-branch",
  desc = "『任务的任务』：在此添加任务记录，CodeBuddy 定时任务读取并执行，结果写入开发日志。",
  api_desc = "add(title,payload,cron) 新增 · list() 队列 · mark_done(id,result) 标记完成 · dispatch(task) 派发",
  status = "scaffold", version = "v0.1"
)

# 派发（占位实现）
hf_metatask_dispatch <- function(task = NULL) {
  list(success = TRUE, task = task, message = "元任务派发接口（占位）")
}

# 元任务队列接口（透传到 hf_core 的数据层）
hf_metatask_api_list <- function() hf_metatask_list()
hf_metatask_api_add  <- function(title = NULL, payload = "", cron = "") hf_metatask_add(title, payload, cron)

hf_register(HF_MOD_METATASK, hf_make_api(
  "metatask",
  dispatch = hf_metatask_dispatch,
  list = hf_metatask_api_list,
  add = hf_metatask_api_add
))
