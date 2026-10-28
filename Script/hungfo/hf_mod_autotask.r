##################
# HungFo 环节 06 — 自动任务 (HF-AUTOTASK)
# 职责：任务编排 / 调度 / 定时执行，串联其它环节形成自动化流程。
# 本模块同时作为「Agent 任务来源」：示例任务 / 正式任务统一在此编辑，
# 通过 hf_agent_tasks 表存储，下达给 Agent 们（监控页据此展示并切换 示例/正式）。
##################

HF_MOD_AUTOTASK <- list(
  id = "hf06", key = "autotask", name = "自动任务", icon = "clock",
  desc = "任务编排、调度与定时执行，把各环节串成自动化流程；同时是 Agent 任务来源（示例/正式任务统一在此编辑下达）。",
  api_desc = "run(payload) 立即执行 · fetch() 取任务 · schedule(task, cron) 排期 · tasks() 任务列表",
  status = "scaffold", version = "v0.1"
)

# 排期（占位实现）
hf_autotask_schedule <- function(task = NULL, cron = NULL) {
  list(success = TRUE, task = task, cron = cron, message = "任务排期接口（占位）")
}

# Agent 任务接口（透传到 hf_core 的任务数据层）
hf_autotask_tasks <- function(task_type = NULL) hf_task_list(task_type)

hf_register(HF_MOD_AUTOTASK, hf_make_api(
  "autotask",
  schedule = hf_autotask_schedule,
  tasks = hf_autotask_tasks
))
