library(readxl)

dir_path <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月"
files <- list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[!grepl("~\\$", files)]

# 只看每个文件的明细Sheet
for (f in files) {
  cat("\n========================================\n")
  cat("=== FILE:", basename(f), "===\n")
  sheets <- excel_sheets(f)
  
  # 找出明细sheet
  detail_sheets <- sheets[grepl("明细|Sheet1|^1$", sheets, ignore.case=TRUE)]
  if (length(detail_sheets) == 0) detail_sheets <- sheets  # fallback
  
  for (sn in detail_sheets) {
    cat("--- Sheet:", sn, "---\n")
    df <- read_excel(f, sheet = sn, col_names = FALSE)
    cat("Shape:", nrow(df), "x", ncol(df), "\n")
    
    # 打印前几行原始数据
    for (i in 1:min(5, nrow(df))) {
      vals <- as.character(df[i, ])
      vals <- vals[!is.na(vals)]
      cat(sprintf("  Row %d: %s\n", i, paste(vals, collapse=" | ")))
    }
    
    # 打印数据行（跳过表头）
    cat("\n--- 数据行 (跳过前2行表头) ---\n")
    for (i in 3:min(40, nrow(df))) {
      vals <- as.character(df[i, ])
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        cat(sprintf("  Row %d: %s\n", i, paste(vals, collapse=" | ")))
      }
    }
  }
}
