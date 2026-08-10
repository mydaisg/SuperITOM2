library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"

# 6月主管汇总数据 - 只看前几行
ref <- read_excel(f6, sheet = "26.6主管汇总数据")
cat("=== 6月 主管汇总数据 (前20行) ===\n")
print(head(as.data.frame(ref), 20))
cat("\n总行数:", nrow(ref), "总列数:", ncol(ref), "\n")
cat("\n所有列名:\n")
for(i in seq_along(colnames(ref))) cat(sprintf("  [%d] %s\n", i, colnames(ref)[i]))
