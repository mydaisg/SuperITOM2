library(readxl)

dir_path <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月"
files <- list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[!grepl("~\\$", files)]

for (f in files) {
  cat("\n========================================\n")
  cat("=== FILE:", basename(f), "===\n")
  sheets <- excel_sheets(f)
  for (sn in sheets) {
    cat("--- Sheet:", sn, "---\n")
    df <- read_excel(f, sheet = sn)
    cat("Shape:", nrow(df), "x", ncol(df), "\n")
    cat("Columns:", paste(names(df), collapse = " | "), "\n")
    print(df, n = 100)
  }
}
