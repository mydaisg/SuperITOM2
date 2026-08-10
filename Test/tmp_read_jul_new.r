library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

cat("=== 新文件 sheets ===\n")
print(excel_sheets(f7))

# 读主管明细
d <- read_excel(f7, sheet = "26.7主管明细")
cat(sprintf("\nShape: %d x %d\n", nrow(d), ncol(d)))
cat("Columns:\n")
for(i in seq_along(colnames(d))) cat(sprintf("  [%d] %s\n", i, colnames(d)[i]))

cat("\n=== 全部内容 ===\n")
for(i in 1:nrow(d)) {
  val <- as.character(d[[1]][i])
  if(is.na(val)) val <- "(空)"
  cat(sprintf("%4d: %s\n", i, val))
}
