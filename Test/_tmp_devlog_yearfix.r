# 临时脚本：写入「总结模块本年度报错」修复开发日志
source("global.R")
source("Script/dev_log_management.r")

res <- dev_log_add(
  module = "总结",
  title = "总结模块「本年度」报错（display_name NA 导致筛选逻辑引入脏行）修复",
  requirement = "总结模块点'本年度'报错 missing value where TRUE/FALSE needed，堆栈指向 renderUI 第649行 nchar(lg$content)。",
  requirement_en = "Daily report 'this year' throws error: missing value where TRUE/FALSE needed in renderUI.",
  solution = paste0(
    "根因：反馈日志按人筛选时 user_logs 逻辑用了 task_logs$creator_name == u$display_name，",
    "但部分用户 display_name 为 NA（12个用户），NA 比较产生 NA 逻辑值，",
    "NA | FALSE = NA，导致 task_logs[NA, ] 引入 content 为 NA 的脏行，",
    "渲染时 nchar(NA)=NA，if(NA>60) 报 missing value where TRUE/FALSE needed。",
    "'今天/昨天'因当日无反馈日志(log_count=0)不触发，'本年度'汇集全年15条日志才暴露。",
    "修复：对 u$display_name/u$username 做 NA 安全化(转空串)再比较，避免 NA 索引。"
  ),
  solution_en = "Guard NA display_name/username before comparison to prevent NA logical index.",
  result = "已修复，模拟'本年度'渲染不再报错，正确筛选出反馈日志(戴诗贡5条/admin3条)。",
  result_en = "Fixed; year render no longer errors.",
  commit_msg = "fix: daily report NA display_name filter",
  code_snippet = "u_dn <- if (is.na(u$display_name)||is.null(u$display_name)) '' else u$display_name",
  files_changed = "Script/daily_report.r"
)
print(res)
