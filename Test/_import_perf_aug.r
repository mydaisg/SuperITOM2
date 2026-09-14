# 从 LVCC_研发中心_IT部8月绩效明细_20260904.xlsx 导入到 SuperITOM2 绩效模块
# 8月份工作项清单（4个员工Sheet在同一文件）
# 参考: DL20260807002 (7月导入)

library(readxl)
source("global.R")
source("Script/performance_management.r")

file_path <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/8月/LVCC_研发中心_IT部8月绩效明细_20260904.xlsx"

# 员工ID映射
emp_map <- list(
  "韩荣昌" = 16,
  "吴时超" = 17,
  "杨长湖" = 18,
  "田予初" = 19
)

# 指标代码提取：B4-xxx → B4, C10-xxx → C10
extract_indicator_code <- function(s) {
  s <- trimws(as.character(s))
  m <- regmatches(s, regexpr("^[ABC]\\d+", s))
  if (length(m) > 0 && m != "") return(m)
  return(NA_character_)
}

# 4个Sheet定义：sheet名 -> (姓名列索引, 工作项列索引, 指标列索引)
# 韩荣昌是5列(无"纳入"列)：序号|指标|工作项|姓名|计分 → 姓名列4
# 其他是6列：序号|指标|工作项|纳入|姓名|计分 → 姓名列5
sheet_configs <- list(
  list(sheet="26.8韩荣昌", name="韩荣昌", name_col=4, work_col=3, code_col=2),
  list(sheet="26.8田予初", name="田予初", name_col=5, work_col=3, code_col=2),
  list(sheet="26.8杨长湖", name="杨长湖", name_col=5, work_col=3, code_col=2),
  list(sheet="26.8吴时超", name="吴时超", name_col=5, work_col=3, code_col=2)
)

all_items <- data.frame(employee_id=integer(0), indicator_code=character(0), source_title=character(0), stringsAsFactors=FALSE)
c11_log <- c()  # 记录 C11 异常项

for (cfg in sheet_configs) {
  cat(sprintf("\n=== 处理: %s (Sheet: %s) ===\n", cfg$name, cfg$sheet))
  df <- read_excel(file_path, sheet = cfg$sheet, col_names = FALSE)
  # 跳过前2行（标题+表头）
  data_rows <- df[3:nrow(df), ]
  # 按姓名列过滤（该姓名）
  name_vals <- as.character(data_rows[[cfg$name_col]])
  rows <- data_rows[!is.na(name_vals) & grepl(cfg$name, name_vals, fixed=TRUE), ]
  cat(sprintf("  找到 %d 条工作项\n", nrow(rows)))

  codes <- vapply(seq_len(nrow(rows)), function(i) extract_indicator_code(rows[[cfg$code_col]][i]), character(1))
  titles <- vapply(seq_len(nrow(rows)), function(i) {
    x <- rows[[cfg$work_col]][i]
    if (is.na(x)) "" else trimws(as.character(x))
  }, character(1))

  items <- data.frame(
    employee_id = rep.int(emp_map[[cfg$name]], length(codes)),
    indicator_code = unname(codes),
    source_title = unname(titles),
    stringsAsFactors = FALSE
  )

  # 检查 C11 异常（系统只有 C9/C10，无 C11）
  c11_idx <- which(!is.na(items$indicator_code) & items$indicator_code == "C11")
  if (length(c11_idx) > 0) {
    for (idx in c11_idx) {
      c11_log <- c(c11_log, sprintf("[%s] C11 -> C10: %s", cfg$name, items$source_title[idx]))
      items$indicator_code[idx] <- "C10"  # 归入管理需求
    }
  }

  items <- items[!is.na(items$indicator_code) & items$source_title != "", ]
  cat(sprintf("  有效工作项: %d\n", nrow(items)))
  all_items <- rbind(all_items, items)
}

cat("\n\n===== C11 异常处理 =====\n")
if (length(c11_log) > 0) cat(paste(c11_log, collapse="\n"), "\n") else cat("无 C11 异常\n")

cat(sprintf("\n=== 总计: %d 条工作项 ===\n", nrow(all_items)))

# 汇总打印
for (eid in c(16,17,18,19)) {
  emp_name <- names(which(unlist(emp_map) == eid))
  ei <- all_items[all_items$employee_id == eid, ]
  cat(sprintf("\n%s (id=%d): %d 条\n", emp_name, eid, nrow(ei)))
  for (ic in sort(unique(ei$indicator_code))) {
    cat(sprintf("  %s: %d 项\n", ic, sum(ei$indicator_code == ic)))
  }
}

# ========================================
# 导入数据库
# ========================================
cat("\n\n=== 开始导入数据库 ===\n")

sheet <- perf_sheet_get_by_month("2026-08")
if (is.null(sheet)) {
  cat("创建 2026-08 绩效表...\n")
  r <- perf_sheet_create("2026-08")
  if (r$success) sheet_id <- r$id else stop(r$message)
  cat(sprintf("  已创建, id=%d\n", sheet_id))
} else {
  sheet_id <- sheet$id[1]
  cat(sprintf("使用已有绩效表, id=%d\n", sheet_id))
}

# 添加员工
cat("添加员工到绩效表...\n")
r <- perf_sheet_employee_add(sheet_id, c(16,17,18,19))
cat(sprintf("  %s\n", r$message))

# 清除已有工作项（幂等重导）
con <- db_connect()
tryCatch({
  cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) cnt FROM performance_work_items WHERE sheet_id=%d", sheet_id))$cnt[1]
  if (cnt > 0) {
    cat(sprintf("清除已有 %d 条工作项...\n", cnt))
    dbExecute(con, sprintf("DELETE FROM performance_work_items WHERE sheet_id=%d", sheet_id))
  }
}, finally = db_disconnect(con))

# 批量导入
imported <- 0; failed <- 0
for (i in seq_len(nrow(all_items))) {
  item <- all_items[i, ]
  r <- perf_work_item_add(
    sheet_id = sheet_id, employee_id = item$employee_id,
    indicator_code = item$indicator_code,
    source_type = "manual", source_id = NULL,
    source_title = item$source_title, deduction_level = 0)
  if (r$success) imported <- imported + 1 else { failed <- failed + 1; cat(sprintf("  失败 [%s] %s: %s\n", item$indicator_code, substr(item$source_title,1,50), r$message)) }
}
cat(sprintf("\n导入完成: 成功 %d, 失败 %d\n", imported, failed))

# 验证
con <- db_connect()
tryCatch({
  cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) cnt FROM performance_work_items WHERE sheet_id=%d", sheet_id))$cnt[1]
  cat(sprintf("数据库中工作项总数: %d\n", cnt))
  stats <- dbGetQuery(con, sprintf(
    "SELECT u.display_name, COUNT(*) cnt FROM performance_work_items pwi JOIN users u ON pwi.employee_id=u.id WHERE pwi.sheet_id=%d GROUP BY pwi.employee_id ORDER BY u.display_name", sheet_id))
  print(stats)
}, finally = db_disconnect(con))

cat("\n=== 导入完毕 ===\n")
