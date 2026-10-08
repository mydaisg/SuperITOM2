source("global.R")
source("Script/daily_report.r")

d <- Sys.Date()
cat("=== 今天 note_comments 列名 ===\n")
nc_today <- daily_report_get_note_comments(d)
print(names(nc_today))
cat("行数:", nrow(nc_today), "\n")
cat("有 parent_id>0 的行:", sum(!is.na(nc_today$parent_id) & nc_today$parent_id > 0), "\n")

cat("\n=== 昨天 note_comments 列名 ===\n")
nc_yday <- daily_report_get_note_comments(d - 1)
print(names(nc_yday))
cat("行数:", nrow(nc_yday), "\n")
cat("有 parent_id>0 的行:", sum(!is.na(nc_yday$parent_id) & nc_yday$parent_id > 0), "\n")

cat("\n=== 昨天 unique note_no ===\n")
print(unique(nc_yday$note_no))

# 检查是否存在 requirement_rainbow_colors 和 requirement_num_to_cn 函数
cat("\n=== 依赖函数是否存在 ===\n")
cat("requirement_rainbow_colors:", exists("requirement_rainbow_colors"), "\n")
cat("requirement_num_to_cn:", exists("requirement_num_to_cn"), "\n")
cat("carryover_extract_ym:", exists("carryover_extract_ym"), "\n")
