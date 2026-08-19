# 从 xlsx 生成流程实例数据看板 HTML
# 输入: LVCC_流程中心_试运行_8月1-6数据.xlsx
# 输出: 流程实例数据看板_20260807.html

library(readxl)
library(jsonlite)

# === 读取数据 ===
xlsx_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/LVCC_流程中心_试运行_8月1-6数据.xlsx"
df <- read_excel(xlsx_file, sheet = "sheet1")
cat("Total rows:", nrow(df), "\n")

# 列名: 流程名称, 当前节点, 发起人, 发起时间, 紧急程度

# === 判断流程是否已完成 ===
# "当前节点" 为 "完成" 或 "已完成" 或空表示已完成
df$is_completed <- df$当前节点 %in% c("完成", "已完成", "结束", "") | is.na(df$当前节点)
# 注意：有些流程"当前节点"显示"发起节点"表示刚开始，"直接上级"等表示审批中

# === 提取流程类型（从流程名称中提取） ===
# 流程名称格式如: "对公付款申请流程-曾霞-2026-08-07 DGFK202608043..."
# 提取 "-" 前面的部分作为流程类型
extract_flow_type <- function(name) {
  # 有些名字格式特殊，尝试提取主要类型
  # 去掉编号后缀 ITS/PRJ/DGFK/MRBPS 等
  parts <- strsplit(name, "-")[[1]]
  # 如果第一部分看起来像类型名，用它；否则尝试合并前几部分
  type_candidate <- parts[1]
  # 常见流程类型关键词
  known_types <- c("对公付款申请流程", "售后服务工单", "MRB评审单", "采购申请流程",
                   "设备切换平台", "IT服务工单", "通用用印流程", "服务费和手续费开票申请表",
                   "数据调取", "系统账户开通审批", "系统权限开通审批", "小区欠款处理表申请",
                   "接待客户审批", "平台结算打款申请表", "余额退款申请表", "资产入库申请流程",
                   "排产计划", "试用期员工工作情况跟踪", "试用期员工自评", "通用费用报销",
                   "业务招待费用报销", "合同审批单", "离职交接", "人员入职流程", "数智技术工厂服务工单",
                   "维修服务申请", "相对方信息新增或变更申请", "包厢预定", "合同用印流程",
                   "离职申请", "批量试产单", "员工调岗申请", "差旅费报销", "电子充电卡特殊收费",
                   "服务费更改申请表", "合同纸质回收流程", "计划投放申请表", "监控查看权限申请",
                   "软件OTA升级", "宿舍入住申请单", "物料涨价审批", "线下订单退货退款申请",
                   "资产派发流程", "资产入库申请流程")
  
  # 尝试匹配已知类型
  for (kt in known_types) {
    if (grepl(kt, name, fixed = TRUE)) {
      return(kt)
    }
  }
  
  # 没匹配到，取第一个 "-" 之前的部分，截断长度
  if (nchar(type_candidate) > 12) {
    type_candidate <- substr(type_candidate, 1, 12)
  }
  return(type_candidate)
}

df$flow_type <- sapply(df$流程名称, extract_flow_type)

# === 解析日期 ===
# 发起时间格式如 "2026-08-07 08:43:18" 或 "2026-08-07"
df$date <- as.Date(substr(df$发起时间, 1, 10))
cat("Date range:", as.character(min(df$date, na.rm = TRUE)), "to", as.character(max(df$date, na.rm = TRUE)), "\n")

# === 日期列表 ===
dates <- sort(unique(df$date))
date_labels <- format(dates, "%m-%d")
cat("Dates:", paste(date_labels, collapse = ", "), "\n")

# === 1. 每日趋势 ===
daily_total <- sapply(dates, function(d) sum(df$date == d, na.rm = TRUE))
daily_completed <- sapply(dates, function(d) sum(df$date == d & df$is_completed, na.rm = TRUE))
daily_active <- daily_total - daily_completed

cat("\n=== Daily Data ===\n")
cat("Total:", paste(daily_total, collapse = ", "), "\n")
cat("Completed:", paste(daily_completed, collapse = ", "), "\n")
cat("Active:", paste(daily_active, collapse = ", "), "\n")

# === 2. 流程类型分布 ===
flow_type_stats <- aggregate(
  list(total = df$flow_type),
  by = list(name = df$flow_type),
  FUN = length
)
flow_type_completed <- aggregate(
  list(completed = df$flow_type),
  by = list(name = df$flow_type),
  FUN = function(x) sum(df$is_completed[match(x, df$flow_type)] > 0)
)
# 重新计算
flow_type_stats$completed <- sapply(flow_type_stats$name, function(ft) {
  sum(df$flow_type == ft & df$is_completed, na.rm = TRUE)
})
flow_type_stats$active <- flow_type_stats$total - flow_type_stats$completed
flow_type_stats <- flow_type_stats[order(-flow_type_stats$total), ]

cat("\n=== Flow Type Stats ===\n")
print(head(flow_type_stats, 20))

# === 3. 阻塞节点（进行中流程的当前节点） ===
active_df <- df[!df$is_completed, ]
node_counts <- sort(table(active_df$当前节点), decreasing = TRUE)
cat("\n=== Blocking Nodes (Top 20) ===\n")
print(head(node_counts, 20))

blocking_nodes <- head(node_counts, 20)
blocking_list <- list()
for (i in seq_along(blocking_nodes)) {
  node_name <- names(blocking_nodes)[i]
  node_count <- as.integer(blocking_nodes[i])
  # 涉及类型
  types_involved <- active_df$flow_type[active_df$当前节点 == node_name]
  type_summary <- paste(names(sort(table(types_involved), decreasing = TRUE))[1:3], collapse = ", ")
  blocking_list[[i]] <- list(
    name = node_name,
    count = node_count,
    types = type_summary
  )
}

# === 4. 发起人排名 ===
initiator_counts <- sort(table(df$发起人), decreasing = TRUE)
cat("\n=== Initiator Stats (Top 15) ===\n")
print(head(initiator_counts, 15))

top_initiators <- head(initiator_counts, 15)
initiator_list <- list()
for (i in seq_along(top_initiators)) {
  name <- names(top_initiators)[i]
  total <- as.integer(top_initiators[i])
  completed <- sum(df$发起人 == name & df$is_completed, na.rm = TRUE)
  initiator_list[[i]] <- list(
    name = name,
    total = total,
    completed = completed,
    active = total - completed
  )
}

# === 5. 每日各类型堆叠 ===
# 取 Top 12 流程类型 + 其他
top_types <- flow_type_stats$name[1:min(12, nrow(flow_type_stats))]
other_types <- setdiff(unique(df$flow_type), top_types)

daily_type_data <- list()
for (ft in top_types) {
  daily_type_data[[ft]] <- as.integer(sapply(dates, function(d) {
    sum(df$date == d & df$flow_type == ft, na.rm = TRUE)
  }))
}
# 其他汇总
daily_type_data[["其他"]] <- as.integer(sapply(dates, function(d) {
  sum(df$date == d & df$flow_type %in% other_types, na.rm = TRUE)
}))

cat("\n=== Daily Type Data ===\n")
for (nm in names(daily_type_data)) {
  cat(nm, ":", paste(daily_type_data[[nm]], collapse = ", "), "\n")
}

# === 6. 计算 KPI ===
total_flows <- nrow(df)
completed_flows <- sum(df$is_completed)
active_flows <- total_flows - completed_flows
completion_rate <- round(completed_flows / total_flows * 100, 1)
avg_daily <- round(total_flows / length(dates), 0)
avg_daily_completed <- round(completed_flows / length(dates), 1)
num_types <- length(unique(df$flow_type))
num_initiators <- length(unique(df$发起人))

# Top 5 占比
top5_total <- sum(head(flow_type_stats$total, 5))
top5_pct <- round(top5_total / total_flows * 100, 1)

cat("\n=== KPI ===\n")
cat("Total:", total_flows, "\n")
cat("Completed:", completed_flows, "(", completion_rate, "%)\n")
cat("Active:", active_flows, "\n")
cat("Avg daily:", avg_daily, "\n")
cat("Types:", num_types, "\n")
cat("Initiators:", num_initiators, "\n")
cat("Top5 pct:", top5_pct, "%\n")

# === 生成 JSON 数据 ===
cat("\n=== Generating JSON ===\n")

# 每日数据 JSON
daily_json <- toJSON(list(
  dates = date_labels,
  total = as.integer(daily_total),
  completed = as.integer(daily_completed),
  active = as.integer(daily_active)
), auto_unbox = TRUE)
cat("dailyData:", daily_json, "\n\n")

# 流程类型 JSON
flow_type_json <- toJSON(
  lapply(1:nrow(flow_type_stats), function(i) {
    list(name = flow_type_stats$name[i],
         total = flow_type_stats$total[i],
         completed = flow_type_stats$completed[i],
         active = flow_type_stats$active[i])
  }), auto_unbox = TRUE
)
cat("flowTypeData:", flow_type_json, "\n\n")

# 阻塞节点 JSON
blocking_json <- toJSON(blocking_list, auto_unbox = TRUE)
cat("blockingNodes:", blocking_json, "\n\n")

# 发起人 JSON
initiator_json <- toJSON(initiator_list, auto_unbox = TRUE)
cat("initiatorData:", initiator_json, "\n\n")

# 每日各类型 JSON
daily_type_json <- toJSON(daily_type_data, auto_unbox = TRUE)
cat("dailyTypeData:", daily_type_json, "\n")

# === 生成 HTML ===
html_template <- readLines("D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/流程实例数据看板_20260802.html", warn = FALSE)

# 替换数据周期
html_template <- gsub("2026年7月27日 - 2026年8月1日",
                      paste0(format(min(dates), "%Y年%m月%d日"), " - ", format(max(dates), "%Y年%m月%d日")),
                      html_template, fixed = TRUE)
html_template <- gsub("总流程数：462",
                      paste0("总流程数：", total_flows),
                      html_template, fixed = TRUE)

# 替换 KPI 数字
html_template <- gsub(">462<", paste0(">", total_flows, "<"), html_template, fixed = TRUE)
html_template <- gsub(">250<", paste0(">", completed_flows, "<"), html_template, fixed = TRUE)
html_template <- gsub("完成率 54.1%", paste0("完成率 ", completion_rate, "%"), html_template, fixed = TRUE)
html_template <- gsub(">212<", paste0(">", active_flows, "<"), html_template, fixed = TRUE)
html_template <- gsub("待处理占比 45.9%", paste0("待处理占比 ", round(active_flows/total_flows*100, 1), "%"), html_template, fixed = TRUE)
html_template <- gsub(">77<", paste0(">", avg_daily, "<"), html_template, fixed = TRUE)
html_template <- gsub("日均完成 41.7", paste0("日均完成 ", avg_daily_completed), html_template, fixed = TRUE)
html_template <- gsub(">55<", paste0(">", num_types, "<"), html_template, fixed = TRUE)
html_template <- gsub("TOP5占比 57.1%", paste0("TOP5占比 ", top5_pct, "%"), html_template, fixed = TRUE)
html_template <- gsub(">188<", paste0(">", num_initiators, "<"), html_template, fixed = TRUE)
html_template <- gsub("发起人总数", "发起人总数", html_template, fixed = TRUE)
html_template <- gsub("全部53种", paste0("全部", num_types, "种"), html_template, fixed = TRUE)
html_template <- gsub("更新日期：2026年8月2日", paste0("更新日期：", format(Sys.Date(), "%Y年%m月%d日")), html_template, fixed = TRUE)
html_template <- gsub("流程实例20260802085103869.xlsx", "LVCC_流程中心_试运行_8月1-6数据.xlsx", html_template, fixed = TRUE)

# 替换 dailyData
# 找到 const dailyData = { 到 }; 之间的内容
pattern_daily <- "const dailyData = \\{[^}]*\\};"
replacement_daily <- paste0("const dailyData = ", daily_json, ";")
html_template <- gsub(pattern_daily, replacement_daily, html_template, perl = TRUE)

# 替换 flowTypeData
# flowTypeData 是一个数组
pattern_ft <- "const flowTypeData = \\[[\\s\\S]*?\\];"
replacement_ft <- paste0("const flowTypeData = ", flow_type_json, ";")
html_template <- sub(pattern_ft, replacement_ft, html_template, perl = TRUE)

# 替换 blockingNodes
pattern_bn <- "const blockingNodes = \\[[\\s\\S]*?\\];"
replacement_bn <- paste0("const blockingNodes = ", blocking_json, ";")
html_template <- sub(pattern_bn, replacement_bn, html_template, perl = TRUE)

# 替换 initiatorData
pattern_id <- "const initiatorData = \\[[\\s\\S]*?\\];"
replacement_id <- paste0("const initiatorData = ", initiator_json, ";")
html_template <- sub(pattern_id, replacement_id, html_template, perl = TRUE)

# 替换 dailyTypeData
pattern_dt <- "const dailyTypeData = \\{[\\s\\S]*?\\};"
replacement_dt <- paste0("const dailyTypeData = ", daily_type_json, ";")
html_template <- sub(pattern_dt, replacement_dt, html_template, perl = TRUE)

# 更新流程类型数量
# 替换 "其他(41种)" 中的数字
other_count <- num_types - min(12, nrow(flow_type_stats))
html_template <- gsub("其他\\(\\d+种\\)", paste0("其他(", other_count, "种)"), html_template, perl = TRUE)

# 更新完成率仪表盘的值 54.1
html_template <- gsub("value: 54\\.\\d+", paste0("value: ", completion_rate), html_template, perl = TRUE)

# 更新平均线 54.1
html_template <- gsub("平均 54\\.\\d+%", paste0("平均 ", completion_rate, "%"), html_template, perl = TRUE)

# 更新阻塞表格中 212 这个数字（进行中总数）
html_template <- gsub("node\\.count / 212", paste0("node.count / ", active_flows), html_template, fixed = TRUE)

# 写入新 HTML
output_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/流程实例数据看板_20260807.html"
writeLines(html_template, output_file)
cat("\n=== HTML written to:", output_file, "===\n")
cat("File size:", file.info(output_file)$size, "bytes\n")
