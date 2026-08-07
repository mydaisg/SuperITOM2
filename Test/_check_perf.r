source("global.R")
con <- DBI::dbConnect(RSQLite::SQLite(), "DB/GH_ITOM.db")

# 检查 7 月绩效表
sheets <- DBI::dbGetQuery(con, "SELECT * FROM performance_sheets WHERE year_month='2026-07'")
cat("=== 7月绩效表 ===\n")
print(sheets)

if (nrow(sheets) > 0) {
  sid <- sheets$id[1]
  cat("\n=== 表内员工 ===\n")
  emp <- DBI::dbGetQuery(con, sprintf("SELECT pse.*, u.display_name, u.username FROM performance_sheet_employees pse LEFT JOIN users u ON pse.employee_id=u.id WHERE pse.sheet_id=%d", sid))
  print(emp)
  
  cat("\n=== 已有工作项数量 ===\n")
  cnt <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM performance_work_items WHERE sheet_id=%d", sid))
  print(cnt)
} else {
  cat("\n7月绩效表不存在，需要创建\n")
}

# 检查 performance_sheet_employees 表是否存在
cat("\n=== 检查 performance_sheet_employees 表 ===\n")
tables <- DBI::dbListTables(con)
if ("performance_sheet_employees" %in% tables) {
  cat("表存在\n")
} else {
  cat("表不存在！需要创建\n")
}

DBI::dbDisconnect(con)
