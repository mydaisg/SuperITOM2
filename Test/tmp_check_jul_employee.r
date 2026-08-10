library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260807.xlsx"

# 看7月员工明细全部
emp_detail <- read_excel(f7, sheet = "26.7员工明细")
cat("=== 26.7员工明细 全部 ===\n")
for(i in 1:nrow(emp_detail)) {
  row_str <- paste(sapply(1:ncol(emp_detail), function(j) {
    v <- as.character(emp_detail[[j]][i])
    if(is.na(v)) "" else v
  }), collapse = " | ")
  cat(sprintf("%3d: %s\n", i, row_str))
}

cat("\n\n=== 员工列唯一值 ===\n")
print(unique(emp_detail[[4]]))
