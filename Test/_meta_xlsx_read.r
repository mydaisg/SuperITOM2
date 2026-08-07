# 读取两个 xlsx 看看结构
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl", repos = "https://cloud.r-project.org")
}
library(readxl)

# 旧文件
old_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/流程监控20260802085103869.xlsx"
new_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/LVCC_流程中心_试运行_8月1-6数据.xlsx"

cat("=== Old file sheets ===\n")
print(excel_sheets(old_file))

cat("\n=== New file sheets ===\n")
print(excel_sheets(new_file))

# 读取每个 sheet 的前几行
for (s in excel_sheets(new_file)) {
  cat("\n=== New file sheet:", s, "===\n")
  df <- read_excel(new_file, sheet = s)
  cat("Dim:", nrow(df), "x", ncol(df), "\n")
  cat("Colnames:", paste(colnames(df), collapse = ", "), "\n")
  print(head(df, 5))
  cat("\n")
}
