library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

d <- read_excel(f7, sheet = "26.7主管明细")
lines <- as.character(d[[1]])
lines[is.na(lines)] <- ""

cat("=== 全部1533行（节选关键行）===\n")

# 1. 找头部汇总
cat("\n--- 头部 ---\n")
for(i in 1:10) cat(sprintf("%4d: %s\n", i, lines[i]))

# 2. 找所有NTE标题
cat("\n--- 所有NTE标题 ---\n")
nte_lines <- c()
for(i in 1:length(lines)) {
  if(grepl("^[一二三四五六七八九十]、.*NTE", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    nte_lines <- c(nte_lines, i)
  }
}

# 3. 找所有工单(ITS)
cat("\n--- 所有工单(ITS) ---\n")
its_count <- 0
for(i in 1:length(lines)) {
  if(grepl("ITS\\d{8,}", lines[i]) && !grepl("^[一二三四五六七八九十]", lines[i]) && !grepl("^📋", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    its_count <- its_count + 1
  }
}
cat(sprintf("工单总数: %d\n", its_count))

# 4. 找所有任务(TSK)
cat("\n--- 所有任务(TSK) ---\n")
tsk_count <- 0
for(i in 1:length(lines)) {
  if(grepl("TSK\\d{8,}", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    tsk_count <- tsk_count + 1
  }
}
cat(sprintf("任务总数: %d\n", tsk_count))

# 5. 找派发记事汇总后面的内容（独立工单清单）
cat("\n--- 派发记事汇总区块 ---\n")
for(i in 1:length(lines)) {
  if(grepl("派发记事汇总|辅助独立工单", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
  }
}

# 6. 找韩荣昌区块
cat("\n--- 韩荣昌区块 ---\n")
han_start <- 0
for(i in 1:length(lines)) {
  if(grepl("^韩荣昌$", lines[i]) && i > 1000) {
    cat(sprintf("韩荣昌出现在行: %d\n", i))
    han_start <- i
    # 打印后面20行
    for(j in i:min(i+20, length(lines))) {
      cat(sprintf("%4d: %s\n", j, lines[j]))
    }
    break
  }
}

# 找尾部的工单汇总
cat("\n--- 尾部工单汇总 ---\n")
for(i in (length(lines)-50):length(lines)) {
  line <- lines[i]
  if(grepl("工单|任务|记事|合计|总计", line)) {
    cat(sprintf("%4d: %s\n", i, line))
  }
}
