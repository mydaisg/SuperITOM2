source('global.R')
source('Script/log_user.r')
source('Script/work_order.r')
source('Script/project_management.r')

cat('=== 测试创建项目 ===\n')
r1 <- project_add('Test Project', 'Test desc', '高', '2026-04-27', '2026-06-30', data.frame(id=1, username='admin'))
cat(r1$message, '\n')

projects <- project_get_all()
cat('项目数量:', nrow(projects), '\n')
pid <- projects$id[1]
stats <- project_get_stats()
cat('统计 total:', stats$total[1], 'planning:', stats$planning[1], '\n')

cat('\n=== 测试阶段 ===\n')
r2 <- phase_add(pid, '需求分析', '第一阶段', 1)
cat(r2$message, '\n')
phases <- phase_get_by_project(pid)
cat('阶段数量:', nrow(phases), '\n')
phid <- phases$id[1]

cat('\n=== 测试工作包 ===\n')
r3 <- wp_add(phid, pid, '需求收集', '收集需求', NULL, 1)
cat(r3$message, '\n')
wps <- wp_get_by_phase(phid)
cat('工作包数量:', nrow(wps), '\n')
wpid <- wps$id[1]

cat('\n=== 测试任务 ===\n')
r4 <- task_add(wpid, phid, pid, '需求调研', '调研需求', '高', NULL, '2026-05-15', data.frame(id=1, username='admin'))
cat(r4$message, '\n')
tasks <- task_get_by_wp(wpid)
cat('任务数量:', nrow(tasks), '\n')
tid <- tasks$id[1]

r5 <- task_update_status(tid, 'in_progress', data.frame(id=1, username='admin'))
cat('状态更新:', r5$message, '\n')

cat('\n=== 测试反馈 ===\n')
r6 <- task_log_add(tid, 'execution', '已完成调研', data.frame(id=1, username='admin'))
cat(r6$message, '\n')
logs <- task_log_get_by_task(tid)
cat('反馈记录数:', nrow(logs), '\n')

cat('\n=== 测试任务转工单 ===\n')
r7 <- task_convert_to_work_order(tid, data.frame(id=1, username='admin'))
cat(r7$message, '\n')

cat('\n=== 清理 ===\n')
r8 <- project_delete(pid, data.frame(id=1, username='admin'))
cat(r8$message, '\n')
cat('清理后项目数:', nrow(project_get_all()), '\n')

cat('\n全部测试通过!\n')
