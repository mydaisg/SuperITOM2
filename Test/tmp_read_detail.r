library(readxl)

f6 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/LVCC_研发中心_IT部6月绩效明细_20260706.xlsx"

ref_detail <- read_excel(f6, sheet = "26.6主管明细")
cat("=== 6月 主管明细 列名 ===\n")
for(i in seq_along(colnames(ref_detail))) {
  cat(sprintf("  [%d] %s\n", i, colnames(ref_detail)[i]))
}
cat("\n总行数:", nrow(ref_detail), "总列数:", ncol(ref_detail), "\n")

# 看前10行
cat("\n=== 前10行 ===\n")
print(head(as.data.frame(ref_detail), 10))

# 看所有唯一值在关键列
for(cn in colnames(ref_detail)) {
  vals <- unique(ref_detail[[cn]])
  if(length(vals) <= 30) {
    cat(sprintf("\n列 [%s] 唯一值 (%d个):\n", cn, length(vals)))
    print(vals)
  } else {
    cat(sprintf("\n列 [%s] 唯一值 (%d个，太多不展示)\n", cn, length(vals)))
  }
}
