# 模拟项目详情弹窗数据流：确认层级构建不报错
source("global.R")
source("Script/project_management.r")
source("Script/work_order.r")

cat("=== 模拟项目详情弹窗数据流 ===\n\n")

all_proj <- project_get_all("all")
if (nrow(all_proj) == 0) {
  cat("无项目数据，跳过\n"); q(save="no")
}

for (pi in 1:nrow(all_proj)) {
  p <- all_proj[pi, ]
  cat(sprintf("\n--- 项目: %s (%s) ---\n", p$name, p$project_no))
  
  tasks <- task_get_by_project(p$id)
  task_total <- nrow(tasks)
  task_done <- if (task_total > 0) sum(tasks$status == "completed") else 0
  cat(sprintf("任务进度: %d/%d\n", task_done, task_total))
  
  phases <- phase_get_by_project(p$id)
  cat(sprintf("阶段数: %d\n", nrow(phases)))
  
  if (nrow(phases) > 0) {
    for (i in 1:nrow(phases)) {
      ph <- phases[i, ]
      wps <- wp_get_by_phase(ph$id)
      ph_tasks <- if (nrow(tasks) > 0) tasks[tasks$phase_id == ph$id, , drop = FALSE] else data.frame()
      cat(sprintf("  阶段 %d: %s [%s] → %d个工作包, %d个任务\n",
          i, ph$name, ph$status, nrow(wps), nrow(ph_tasks)))
      
      if (nrow(wps) > 0) {
        for (j in 1:nrow(wps)) {
          wp <- wps[j, ]
          wp_tasks <- task_get_by_wp(wp$id)
          wp_task_done <- if (nrow(wp_tasks) > 0) sum(wp_tasks$status == "completed") else 0
          cat(sprintf("    工作包: %s → 任务 %d/%d\n",
              wp$name, wp_task_done, nrow(wp_tasks)))
        }
      }
    }
  }
}

cat("\n=== 数据流验证通过 ===\n")
