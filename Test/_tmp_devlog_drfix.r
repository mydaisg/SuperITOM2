# 临时脚本：写入「总结模块日期筛选按键失效」分析与修复开发日志
source("global.R")
source("Script/dev_log_management.r")

res <- dev_log_add(
  module = "总结",
  title = "总结模块日期筛选按键失效（仅今天正常）分析与修复",
  requirement = "总结模块（日报）点昨天/本周/上月/季度/年度等日期筛选按键后页面无反应，仍显示今天的数据，仅'今天'正常。",
  requirement_en = "Daily report date filter buttons (yesterday/week/month/etc.) don't refresh; only 'today' works.",
  solution = paste0(
    "根因：日期筛选依赖 updateDateInput 隐式更新 input$dr_date 来触发 observeEvent 重渲染，",
    "这条隐式触发链不可靠——'今天'之所以正确只是 dateInput 初始值恰好是今天（ignoreInit=FALSE 初始渲染一次）。",
    "点其它按键时 dr_month_mode(NULL) 保持 NULL，仅靠 input$dr_date 变化触发，链路失效导致不刷新。",
    "修复：引入显式状态 dr_selected_date（选中日期）+ dr_trigger（刷新触发器），",
    "统一 dr_apply() 封装按钮逻辑（设置日期+模式+递增触发器+同步dateInput），",
    "observeEvent 改为依赖 dr_trigger() 而非 input$dr_date，彻底解耦隐式触发；",
    "另加 observeEvent(input$dr_date) 同步手动改日期（带 identical 防循环）。"
  ),
  solution_en = "Add explicit dr_selected_date + dr_trigger state; decouple from implicit dateInput triggering.",
  result = "已修复，testServer 验证点'昨天'后 date 从 10-08 正确变 10-07 且 trigger 递增，待浏览器实际验证。",
  result_en = "Fixed; verified via testServer, awaiting browser check.",
  commit_msg = "fix: daily report date filter buttons",
  code_snippet = paste0(
    "dr_selected_date <- reactiveVal(Sys.Date()); dr_trigger <- reactiveVal(0)\n",
    "dr_apply <- function(sel_date, mode=NULL){ dr_selected_date(sel_date); dr_month_mode(mode); dr_trigger(dr_trigger()+1); updateDateInput(...) }\n",
    "observeEvent(list(dr_trigger(), ...), { report_date <- dr_selected_date(); ... })"
  ),
  files_changed = "Script/daily_report.r"
)
print(res)
