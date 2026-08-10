library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"

detail <- read_excel(f6, sheet = "26.6主管明细")
d <- detail[[1]]

# 看尾部
cat("=== 尾部 (最后30行) ===\n")
n <- length(d)
for(i in (n-29):n) {
  cat(sprintf("%4d: %s\n", i, as.character(d[i])))
}

# 找第二个工单区块 (行1236附近)
cat("\n=== 行1230-1260 ===\n")
for(i in 1230:min(1260, n)) {
  cat(sprintf("%4d: %s\n", i, as.character(d[i])))
}

# 看第二个工单区块的具体工单
cat("\n=== 第二个工单区块 全部工单 ===\n")
for(i in 1236:1420) {
  line <- as.character(d[i])
  if(is.na(line)) next
  if(grepl("^已派发|^处理中|^已完成|^已关闭|^待处理.*ITS", line)) {
    cat(sprintf("%4d: %s\n", i, line))
  }
}
