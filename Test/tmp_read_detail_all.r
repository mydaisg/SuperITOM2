library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"

ref_detail <- read_excel(f6, sheet = "26.6主管明细")
cat("=== 6月 主管明细 全部1445行 ===\n")
for(i in 1:nrow(ref_detail)) {
  val <- as.character(ref_detail[[1]][i])
  if(is.na(val)) val <- "(空)"
  cat(sprintf("%4d: %s\n", i, val))
}
