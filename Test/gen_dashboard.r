# 流程监控数据看板生成脚本
# 源数据：流程监控20260814144725824.xlsx
# 输出：流程监控数据看板_20260814.html

library(readxl)
library(dplyr)
library(jsonlite)

data_dir <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据"
src_file <- file.path(data_dir, "流程监控20260814144725824.xlsx")
out_file <- file.path(data_dir, "流程监控数据看板_20260814.html")

df <- read_excel(src_file)

# ---- 预处理 ----
# 完成标志：当前节点含"结束"
df$is_done <- grepl("结束", df$当前节点)

# 流程类型：第一个 "-" 之前
df$type <- sub("-.*", "", df$流程名称)

# 发起日期
df$date <- substr(df$发起时间, 1, 10)

# ---- KPI 统计 ----
total_flows <- nrow(df)
completed_flows <- sum(df$is_done)
active_flows <- total_flows - completed_flows
completion_rate <- round(completed_flows / total_flows * 100, 1)

# 日期范围
dates <- sort(unique(df$date))
date_min <- min(dates)
date_max <- max(dates)
n_days <- length(dates)

# 日均发起
daily_avg_total <- round(total_flows / n_days)
daily_avg_done <- round(completed_flows / n_days)

# 流程类型数
type_count <- length(unique(df$type))

# 发起人数
initiator_count <- length(unique(df$发起人))

# ---- 每日趋势 ----
daily <- df %>%
  group_by(date) %>%
  summarise(
    total = n(),
    completed = sum(is_done),
    active = sum(!is_done),
    .groups = "drop"
  )
daily <- daily %>% arrange(date)

# 补全缺失日期（确保连续）
all_dates <- seq(as.Date(date_min), as.Date(date_max), by = "day")
all_dates <- format(all_dates, "%Y-%m-%d")
daily_full <- data.frame(date = all_dates, stringsAsFactors = FALSE)
daily_full <- left_join(daily_full, daily, by = "date")
daily_full[is.na(daily_full)] <- 0

dailyData <- list(
  dates = daily_full$date,
  total = as.integer(daily_full$total),
  completed = as.integer(daily_full$completed),
  active = as.integer(daily_full$active)
)

# ---- 流程类型分布 ----
flowTypeData <- df %>%
  group_by(type) %>%
  summarise(
    total = n(),
    completed = sum(is_done),
    active = sum(!is_done),
    .groups = "drop"
  ) %>%
  arrange(desc(total), desc(completed))

flowTypeData <- as.data.frame(flowTypeData)
names(flowTypeData)[1] <- "name"

# TOP5 占比
top5_total <- sum(head(flowTypeData$total, 5))
top5_pct <- round(top5_total / total_flows * 100, 1)

# ---- 阻塞节点（进行中流程的当前节点） ----
active_df <- df[!df$is_done, ]
blocking <- active_df %>%
  group_by(当前节点) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

blocking <- as.data.frame(blocking)
names(blocking) <- c("name", "count")

# 涉及流程类型（取每种节点下的前几个类型）
build_types <- function(node) {
  types <- active_df$type[active_df$当前节点 == node]
  tt <- sort(table(types), decreasing = TRUE)
  parts <- paste0(names(tt), "(", tt, ")")
  paste(head(parts, 5), collapse = " ")
}
blocking$types <- sapply(blocking$name, build_types)

blockingNodes <- blocking

# ---- 发起人排名 ----
initiatorData <- df %>%
  group_by(发起人) %>%
  summarise(
    total = n(),
    completed = sum(is_done),
    active = sum(!is_done),
    .groups = "drop"
  ) %>%
  arrange(desc(total)) %>%
  head(15)

initiatorData <- as.data.frame(initiatorData)
names(initiatorData)[1] <- "name"

# ---- 每日各类型堆叠 (Top 12 + 其他) ----
top12_types <- head(flowTypeData$name, 12)
type_names_full <- c(top12_types, "其他")

dailyTypeData <- list()
for (t in top12_types) {
  sub <- df[df$type == t, ]
  cnt <- sapply(all_dates, function(d) sum(sub$date == d))
  dailyTypeData[[t]] <- as.integer(cnt)
}
# 其他
sub_other <- df[!(df$type %in% top12_types), ]
cnt_other <- sapply(all_dates, function(d) sum(sub_other$date == d))
dailyTypeData[["其他"]] <- as.integer(cnt_other)

# ---- 各类型完成率 (Top 12) ----
typeCompletionData <- head(flowTypeData, 12)
typeCompletionData <- typeCompletionData[, c("name", "total", "completed", "active")]

# ---- 序列化 JSON ----
to_json <- function(x) toJSON(x, auto_unbox = TRUE)

dailyData_json <- to_json(dailyData)
flowTypeData_json <- to_json(flowTypeData[, c("name", "total", "completed", "active")])
blockingNodes_json <- to_json(blockingNodes[, c("name", "count", "types")])
initiatorData_json <- to_json(initiatorData[, c("name", "total", "completed", "active")])
dailyTypeData_json <- to_json(dailyTypeData)
typeNames_json <- to_json(type_names_full)
typeCompletionData_json <- to_json(typeCompletionData)

cat("=== 统计摘要 ===\n")
cat("总流程数:", total_flows, "\n")
cat("已完成:", completed_flows, "(", completion_rate, "%)\n")
cat("进行中:", active_flows, "\n")
cat("日期范围:", date_min, "~", date_max, "(", n_days, "天)\n")
cat("流程类型数:", type_count, "\n")
cat("发起人数:", initiator_count, "\n")
cat("TOP5占比:", top5_pct, "%\n")
cat("日均发起:", daily_avg_total, " 日均完成:", daily_avg_done, "\n")

# 保存中间数据供 HTML 生成使用
save(
  dailyData_json, flowTypeData_json, blockingNodes_json, initiatorData_json,
  dailyTypeData_json, typeNames_json, typeCompletionData_json,
  total_flows, completed_flows, active_flows, completion_rate,
  date_min, date_max, n_days, daily_avg_total, daily_avg_done,
  type_count, initiator_count, top5_pct,
  file = file.path("Test", "dashboard_data.RData")
)

cat("\n数据已保存到 Test/dashboard_data.RData\n")
