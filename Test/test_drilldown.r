# 验证钻入模式重写后的数据层完整性
# 需要先加载 global.R 中定义的 db_connect 等
source("global.R")
source("Script/user_management.r")
source("Script/project_management.r")
source("Script/work_order.r")

cat("=== 数据层验证 ===\n\n")

# 1. 检查所有项目是否能完整返回
cat("--- 测试1: 项目列表完整性 ---\n")
all_proj <- project_get_all("all")
cat(sprintf("总项目数: %d\n", nrow(all_proj)))
if (nrow(all_proj) > 0) {
  for (i in 1:nrow(all_proj)) {
    cat(sprintf("  [%d] ID=%s, 编号=%s, 名称=%s, 状态=%s\n",
        i, all_proj$id[i], all_proj$project_no[i], all_proj$name[i], all_proj$status[i]))
  }
}

# 2. 检查统计数据
cat("\n--- 测试2: 统计数据 ---\n")
stats <- project_get_stats()
cat(sprintf("统计: total=%d, planning=%d, active=%d, completed=%d, suspended=%d, closed=%d\n",
    stats$total, stats$planning, stats$active, stats$completed, stats$suspended, stats$closed))

# 3. 对每个项目检查阶段和工作包
cat("\n--- 测试3: 阶段 → 工作包 → 任务 层级 ---\n")
if (nrow(all_proj) > 0) {
  for (i in 1:nrow(all_proj)) {
    phases <- phase_get_by_project(all_proj$id[i])
    cat(sprintf("项目 '%s' (ID=%d): %d 个阶段\n", all_proj$name[i], all_proj$id[i], nrow(phases)))
    if (nrow(phases) > 0) {
      for (j in 1:nrow(phases)) {
        cat(sprintf("  阶段: %s (ID=%d)\n", phases$name[j], phases$id[j]))
        wps <- wp_get_by_phase(phases$id[j])
        cat(sprintf("    -> %d 个工作包\n", nrow(wps)))
        if (nrow(wps) > 0) {
          for (k in 1:nrow(wps)) {
            cat(sprintf("    工作包: %s (ID=%d)\n", wps$name[k], wps$id[k]))
            tasks <- task_get_by_wp(wps$id[k])
            cat(sprintf("      -> %d 个任务\n", nrow(tasks)))
          }
        }
      }
    }
  }
}

# 4. 验证可分配用户
cat("\n--- 测试4: 可分配用户 ---\n")
users <- project_get_assignable_users()
cat(sprintf("可分配用户数: %d\n", nrow(users)))
if (nrow(users) > 0) {
  for (i in 1:nrow(users)) {
    cat(sprintf("  %s (display=%s)\n", users$username[i],
        ifelse(is.na(users$display_name[i]), "NA", users$display_name[i])))
  }
}

cat("\n=== 验证完成 ===\n")
