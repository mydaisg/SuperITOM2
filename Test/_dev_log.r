source("global.R")
source("Script/log_user.r")
source("Script/dev_log_management.r")

r <- dev_log_add(
  module = "绩效管理",
  title = "导入7月份4位员工绩效明细",
  requirement = "把4份员工的明细补充到绩效模块7月份的工作项清单。对应好姓名、指标项、工作项。数据来源: D:\\Tai_LVCC_2026\\Tai_10_OrganizationMangement\\IT部_绩效管理(每月)\\7月",
  solution = "编写导入脚本 Test/_import_perf_july.r，使用readxl读取4个xlsx文件，自动检测明细Sheet，按姓名列过滤提取每人工作项，提取指标代码(B4/B5/B6/B7/B8/C9/C10)，调用perf_work_item_add()批量写入performance_work_items表。",
  result = "成功导入200条工作项：韩荣昌30项、吴时超15项、杨长湖48项、田予初107项。添加到2026-07绩效表(id=6)，4位员工已加入performance_sheet_employees。",
  code_snippet = "extract_indicator_code()从'B4-主导或参与IT项目'提取'B4'；match_employee()按姓名模糊匹配用户ID；perf_work_item_add(sheet_id,employee_id,indicator_code,source_type='manual',source_title=工作项名称)",
  files_changed = "Test/_import_perf_july.r(新增), Test/_meta_mark_done2.r(新增), DB/GH_ITOM.db(performance_work_items表+200行, performance_sheet_employees表+4行)"
)
cat("开发日志:", r$message, "\n")
