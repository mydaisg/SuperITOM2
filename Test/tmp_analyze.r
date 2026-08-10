library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"

# 6月主管明细 - 提取结构
detail <- read_excel(f6, sheet = "26.6主管明细")
d <- detail[[1]]

# 分析结构
cat("=== 6月主管明细 结构分析 ===\n")

# 找大标题行
sections <- list()
current_section <- NULL
subsection <- NULL
item_count <- 0
in_summary <- FALSE

# 统计各类数据
its_count <- 0
tsk_count <- 0
nte_count <- 0
nte_items <- c()
nte_item_counts <- c()

for(i in 1:length(d)) {
  line <- as.character(d[i])
  if(is.na(line)) next
  
  # 统计工单
  if(grepl("^已派发.*ITS", line) || grepl("^处理中.*ITS", line) || grepl("^已完成.*ITS", line) || grepl("^已关闭.*ITS", line)) {
    its_count <- its_count + 1
  }
  
  # 统计任务
  if(grepl("^待处理.*TSK", line) || grepl("^进行中.*TSK", line) || grepl("^已完成.*TSK", line)) {
    tsk_count <- tsk_count + 1
  }
  
  # 统计记事
  if(grepl("^[一二三四五六七八九十]、.*NTE", line)) {
    nte_count <- nte_count + 1
    # 提取NTE编号和标题
    m <- regmatches(line, regexec("(NTE\\d+).*?·\\s*(\\d+)条", line, perl=TRUE))[[1]]
    if(length(m) >= 3) {
      nte_items <- c(nte_items, m[2])
      nte_item_counts <- c(nte_item_counts, as.integer(m[3]))
    }
  }
}

cat(sprintf("工单(ITS)数量: %d\n", its_count))
cat(sprintf("任务(TSK)数量: %d\n", tsk_count))
cat(sprintf("记事(NTE)数量: %d\n", nte_count))
cat(sprintf("记事条目数: %s\n", paste(nte_item_counts, collapse=", ")))
cat(sprintf("记事总条目: %d\n", sum(nte_item_counts)))

# 找所有NTE标题
cat("\n=== 所有NTE标题 ===\n")
for(i in 1:length(d)) {
  line <- as.character(d[i])
  if(is.na(line)) next
  if(grepl("^[一二三四五六七八九十]、.*NTE", line)) {
    cat(sprintf("  %d: %s\n", i, line))
  }
}

# 找汇总行
cat("\n=== 汇总行 ===\n")
for(i in 1:length(d)) {
  line <- as.character(d[i])
  if(is.na(line)) next
  if(grepl("工单|任务|记事|合计|总计", line) && grepl("^\\s*工单|^\\s*任务|^\\s*记事|^\\s*合计|^\\s*总计", line)) {
    cat(sprintf("  %d: %s\n", i, line))
  }
}

# 头部
cat("\n=== 头部 (前10行) ===\n")
for(i in 1:min(10, length(d))) {
  cat(sprintf("  %d: %s\n", i, as.character(d[i])))
}
