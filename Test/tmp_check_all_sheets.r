library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260807.xlsx"

sheets <- excel_sheets(f7)
cat("7月文件所有sheet:\n")
for(i in seq_along(sheets)) cat(sprintf("  [%d] '%s'\n", i, sheets[i]))

# 也看看6月文件
f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"
cat("\n6月文件所有sheet:\n")
sheets6 <- excel_sheets(f6)
for(i in seq_along(sheets6)) cat(sprintf("  [%d] '%s'\n", i, sheets6[i]))
