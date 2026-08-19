# 从 xlsx 生成流程实例数据看板 HTML（修正版）
# "结束节点" 或 "结束" = 已完成

library(readxl)
library(jsonlite)

# === 读取数据 ===
xlsx_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/LVCC_流程中心_试运行_8月1-6数据.xlsx"
df <- read_excel(xlsx_file, sheet = "sheet1")
cat("Total rows:", nrow(df), "\n")

# === 判断流程是否已完成 ===
# "结束节点" 或 "结束" 或 空 = 已完成
completed_keywords <- c("结束节点", "结束", "完成", "已完成")
df$is_completed <- df$当前节点 %in% completed_keywords | is.na(df$当前节点) | df$当前节点 == ""

# === 提取流程类型 ===
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
                 "资产派发流程", "资产申领流程", "补开发票申请", "资产入库流程", "1OA7",
                 "系统权限审批", "定制产品采购申请", "合同归档流程")

extract_flow_type <- function(name) {
  for (kt in known_types) {
    if (grepl(kt, name, fixed = TRUE)) {
      return(kt)
    }
  }
  # 没匹配到，取第一个 "-" 之前的部分，截断
  parts <- strsplit(name, "-")[[1]]
  type_candidate <- parts[1]
  if (nchar(type_candidate) > 12) type_candidate <- substr(type_candidate, 1, 12)
  return(type_candidate)
}

df$flow_type <- sapply(df$流程名称, extract_flow_type)

# === 解析日期 ===
df$date <- as.Date(substr(df$发起时间, 1, 10))
dates <- sort(unique(df$date))
date_labels <- format(dates, "%m-%d")
cat("Dates:", paste(date_labels, collapse = ", "), "\n")

# === 1. 每日趋势 ===
daily_total <- as.integer(sapply(dates, function(d) sum(df$date == d, na.rm = TRUE)))
daily_completed <- as.integer(sapply(dates, function(d) sum(df$date == d & df$is_completed, na.rm = TRUE)))
daily_active <- daily_total - daily_completed

cat("Daily total:", paste(daily_total, collapse = ", "), "\n")
cat("Daily completed:", paste(daily_completed, collapse = ", "), "\n")

# === 2. 流程类型分布 ===
type_names <- unique(df$flow_type)
flow_type_stats <- data.frame(
  name = type_names,
  total = sapply(type_names, function(t) sum(df$flow_type == t)),
  completed = sapply(type_names, function(t) sum(df$flow_type == t & df$is_completed)),
  stringsAsFactors = FALSE
)
flow_type_stats$active <- flow_type_stats$total - flow_type_stats$completed
flow_type_stats <- flow_type_stats[order(-flow_type_stats$total), ]

# === 3. 阻塞节点 ===
active_df <- df[!df$is_completed, ]
node_counts <- sort(table(active_df$当前节点), decreasing = TRUE)
blocking_list <- list()
for (i in seq_len(min(20, length(node_counts)))) {
  node_name <- names(node_counts)[i]
  node_count <- as.integer(node_counts[i])
  types_involved <- active_df$flow_type[active_df$当前节点 == node_name]
  type_sum <- names(sort(table(types_involved), decreasing = TRUE))
  type_summary <- paste(type_sum[1:min(3, length(type_sum))], collapse = ", ")
  blocking_list[[i]] <- list(name = node_name, count = node_count, types = type_summary)
}

# === 4. 发起人排名 ===
initiator_counts <- sort(table(df$发起人), decreasing = TRUE)
top_n <- min(15, length(initiator_counts))
top_initiators <- head(initiator_counts, top_n)
initiator_list <- list()
for (i in seq_along(top_initiators)) {
  name <- names(top_initiators)[i]
  total <- as.integer(top_initiators[i])
  completed <- sum(df$发起人 == name & df$is_completed, na.rm = TRUE)
  initiator_list[[i]] <- list(name = name, total = total, completed = completed, active = total - completed)
}

# === 5. 每日各类型堆叠 ===
top12_types <- head(flow_type_stats$name, 12)
other_types <- setdiff(unique(df$flow_type), top12_types)
daily_type_data <- list()
for (ft in top12_types) {
  daily_type_data[[ft]] <- as.integer(sapply(dates, function(d) sum(df$date == d & df$flow_type == ft, na.rm = TRUE)))
}
daily_type_data[["其他"]] <- as.integer(sapply(dates, function(d) sum(df$date == d & df$flow_type %in% other_types, na.rm = TRUE)))

# === 6. KPI ===
total_flows <- nrow(df)
completed_flows <- sum(df$is_completed)
active_flows <- total_flows - completed_flows
completion_rate <- round(completed_flows / total_flows * 100, 1)
avg_daily <- round(total_flows / length(dates), 0)
avg_daily_completed <- round(completed_flows / length(dates), 1)
num_types <- length(unique(df$flow_type))
num_initiators <- length(unique(df$发起人))
top5_total <- sum(head(flow_type_stats$total, 5))
top5_pct <- round(top5_total / total_flows * 100, 1)

cat("\n=== KPI ===\n")
cat("Total:", total_flows, "| Completed:", completed_flows, "(", completion_rate, "%) | Active:", active_flows, "\n")
cat("Avg daily:", avg_daily, "| Types:", num_types, "| Initiators:", num_initiators, "\n")

# === 生成 JSON ===
daily_json <- toJSON(list(
  dates = date_labels, total = daily_total,
  completed = daily_completed, active = daily_active
), auto_unbox = TRUE)

flow_type_json <- toJSON(
  lapply(1:nrow(flow_type_stats), function(i) {
    list(name = flow_type_stats$name[i], total = flow_type_stats$total[i],
         completed = flow_type_stats$completed[i], active = flow_type_stats$active[i])
  }), auto_unbox = TRUE
)

blocking_json <- toJSON(blocking_list, auto_unbox = TRUE)
initiator_json <- toJSON(initiator_list, auto_unbox = TRUE)
daily_type_json <- toJSON(daily_type_data, auto_unbox = TRUE)

# === 生成 HTML ===
html_template <- readLines("D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/流程实例数据看板_20260802.html", warn = FALSE)

# 替换日期和总数
html_template <- gsub("2026年7月27日 - 2026年8月1日",
                      paste0(format(min(dates), "%Y年%m月%d日"), " - ", format(max(dates), "%Y年%m月%d日")),
                      html_template, fixed = TRUE)
html_template <- gsub("总流程数：462", paste0("总流程数：", total_flows), html_template, fixed = TRUE)

# 替换 KPI 数字（用正则确保只替换 KPI 卡片中的）
html_template <- sub(">462<", paste0(">", total_flows, "<"), html_template, fixed = TRUE)
html_template <- sub(">250<", paste0(">", completed_flows, "<"), html_template, fixed = TRUE)
html_template <- sub("完成率 54\\.1%", paste0("完成率 ", completion_rate, "%"), html_template, perl = TRUE)
html_template <- sub(">212<", paste0(">", active_flows, "<"), html_template, fixed = TRUE)
html_template <- sub("待处理占比 45\\.9%", paste0("待处理占比 ", round(active_flows/total_flows*100, 1), "%"), html_template, perl = TRUE)
html_template <- sub(">77<", paste0(">", avg_daily, "<"), html_template, fixed = TRUE)
html_template <- sub("日均完成 41\\.7", paste0("日均完成 ", avg_daily_completed), html_template, perl = TRUE)
html_template <- sub(">55<", paste0(">", num_types, "<"), html_template, fixed = TRUE)
html_template <- sub("TOP5占比 57\\.1%", paste0("TOP5占比 ", top5_pct, "%"), html_template, perl = TRUE)
html_template <- sub(">188<", paste0(">", num_initiators, "<"), html_template, fixed = TRUE)
html_template <- sub("全部53种", paste0("全部", num_types, "种"), html_template, fixed = TRUE)
html_template <- gsub("更新日期：2026年8月2日", paste0("更新日期：", format(Sys.Date(), "%Y年%m月%d日")), html_template, fixed = TRUE)

# 替换数据块
html_template <- sub("const dailyData = \\{[^}]*\\};", paste0("const dailyData = ", daily_json, ";"), html_template, perl = TRUE)
html_template <- sub("const flowTypeData = \\[[\\s\\S]*?\\];", paste0("const flowTypeData = ", flow_type_json, ";"), html_template, perl = TRUE)
html_template <- sub("const blockingNodes = \\[[\\s\\S]*?\\];", paste0("const blockingNodes = ", blocking_json, ";"), html_template, perl = TRUE)
html_template <- sub("const initiatorData = \\[[\\s\\S]*?\\];", paste0("const initiatorData = ", initiator_json, ";"), html_template, perl = TRUE)
html_template <- sub("const dailyTypeData = \\{[\\s\\S]*?\\};", paste0("const dailyTypeData = ", daily_type_json, ";"), html_template, perl = TRUE)

# 更新"其他"种数
other_count <- num_types - 12
html_template <- gsub("其他\\(\\d+种\\)", paste0("其他(", other_count, "种)"), html_template, perl = TRUE)

# 更新完成率仪表盘
html_template <- gsub("value: 54\\.\\d+", paste0("value: ", completion_rate), html_template, perl = TRUE)

# 更新平均线
html_template <- gsub("平均 54\\.\\d+%", paste0("平均 ", completion_rate, "%"), html_template, perl = TRUE)

# 更新阻塞表格中 212（进行中总数）
html_template <- gsub("node\\.count / 212", paste0("node.count / ", active_flows), html_template, fixed = TRUE)

# 写入
output_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/流程实例数据看板_20260807.html"
writeLines(html_template, output_file)
cat("\n=== HTML written:", output_file, "===\n")
cat("Size:", file.info(output_file)$size, "bytes\n")
