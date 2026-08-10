library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260807.xlsx"

jul <- read_excel(f7, sheet = "26.7主管明细")
cat("=== 7月 主管明细 ===\n")
cat("列名:\n")
for(i in seq_along(colnames(jul))) {
  cat(sprintf("  [%d] %s\n", i, colnames(jul)[i]))
}
cat("\n总行数:", nrow(jul), "总列数:", ncol(jul), "\n\n")

for(i in 1:nrow(jul)) {
  val <- as.character(jul[[1]][i])
  if(is.na(val)) val <- "(空)"
  cat(sprintf("%4d: %s\n", i, val))
}
