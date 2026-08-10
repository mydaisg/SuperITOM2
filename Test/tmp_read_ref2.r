library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"

# 全部74行
ref <- read_excel(f6, sheet = "26.6主管汇总数据")
for(i in 1:nrow(ref)) {
  cat(sprintf("%3d: %s\n", i, ref[[1]][i]))
}
