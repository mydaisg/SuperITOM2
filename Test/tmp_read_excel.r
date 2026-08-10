library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"
f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260807.xlsx"

# List sheets
cat("=== 6月文件 sheets ===\n")
print(excel_sheets(f6))
cat("\n=== 7月文件 sheets ===\n")
print(excel_sheets(f7))

# Read 6月主管汇总
cat("\n=== 6月 主管汇总数据 ===\n")
ref <- read_excel(f6, sheet = "26.6主管汇总数据")
print(colnames(ref))
cat("\n")
print(as.data.frame(ref))
cat("\nShape:", nrow(ref), "x", ncol(ref), "\n")

# Read 6月主管明细
cat("\n=== 6月 主管明细 ===\n")
ref_detail <- read_excel(f6, sheet = "26.6主管明细")
print(colnames(ref_detail))
cat("\n")
print(as.data.frame(ref_detail))
cat("\nShape:", nrow(ref_detail), "x", ncol(ref_detail), "\n")
