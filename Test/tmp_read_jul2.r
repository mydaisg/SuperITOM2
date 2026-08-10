library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260807.xlsx"

# 看每个sheet的前几行
for(sn in excel_sheets(f7)) {
  cat(sprintf("\n========== Sheet: %s ==========\n", sn))
  d <- read_excel(f7, sheet = sn)
  cat(sprintf("Shape: %d x %d\n", nrow(d), ncol(d)))
  cat("Columns:\n")
  for(i in seq_along(colnames(d))) cat(sprintf("  [%d] %s\n", i, colnames(d)[i]))
  cat("\n前5行:\n")
  for(i in 1:min(5, nrow(d))) {
    row_str <- paste(sapply(1:ncol(d), function(j) {
      v <- as.character(d[[j]][i])
      if(is.na(v)) "(空)" else v
    }), collapse = " | ")
    cat(sprintf("  %d: %s\n", i, row_str))
  }
}
