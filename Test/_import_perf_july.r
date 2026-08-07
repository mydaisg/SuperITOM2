# 从4份员工绩效明细 xlsx 导入到 SuperITOM2 绩效模块
# 7月份工作项清单

library(readxl)
source("global.R")
source("Script/performance_management.r")

dir_path <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月"

# 用户ID映射
emp_map <- list(
  "韩荣昌" = 16,
  "吴时超" = 17,
  "杨长湖" = 18,
  "田予初" = 19
)

# 指标代码提取：B4-xxx → B4, B5-xxx → B5, etc.
extract_indicator_code <- function(s) {
  s <- trimws(s)
  m <- regmatches(s, regexpr("^[ABC]\\d+", s))
  if (length(m) > 0 && m != "") return(m)
  return(NA_character_)
}

# 姓名匹配
match_employee <- function(name_str) {
  name_str <- trimws(name_str)
  for (n in names(emp_map)) {
    if (grepl(n, name_str, fixed = TRUE)) return(emp_map[[n]])
  }
  return(NA_integer_)
}

# ========================================
# 文件1: 杨长湖 - 明细Sheet "26.7明细"
# 格式: 序号 | 指标 | 工作项 | 姓名 | 计分
# ========================================
cat("\n=== 处理: 杨长湖 ===\n")
f1 <- file.path(dir_path, "LVCC_研发中心_IT部7月_杨长湖绩效明细_20260801.xlsx")
df1 <- read_excel(f1, sheet = "26.7明细", col_names = FALSE)
# 跳过第1行(标题)和第2行(表头)
data_rows1 <- df1[3:nrow(df1), ]
# 筛选杨长湖的行（姓名列包含"杨长湖"）
yang_rows <- data_rows1[grepl("杨长湖", as.character(data_rows1[[4]]), fixed = TRUE) & 
                        !is.na(data_rows1[[4]]), ]
cat(sprintf("  找到 %d 条工作项\n", nrow(yang_rows)))

items_yang <- data.frame(
  employee_id = 18,
  indicator_code = sapply(yang_rows[[2]], extract_indicator_code),
  source_title = sapply(yang_rows[[3]], function(x) if (is.na(x)) "" else trimws(as.character(x))),
  stringsAsFactors = FALSE
)
# 去掉空的工作项名和空指标码
items_yang <- items_yang[!is.na(items_yang$indicator_code) & items_yang$source_title != "", ]
cat(sprintf("  有效工作项: %d\n", nrow(items_yang)))

# ========================================
# 文件2: 韩荣昌 - 明细Sheet
# ========================================
cat("\n=== 处理: 韩荣昌 ===\n")
f2 <- file.path(dir_path, "LVCC_研发中心_IT部韩荣昌7月绩效明细_20260805.xlsx")
sheets2 <- excel_sheets(f2)
detail_sheet2 <- sheets2[grepl("明细", sheets2)][1]
df2 <- read_excel(f2, sheet = detail_sheet2, col_names = FALSE)
data_rows2 <- df2[3:nrow(df2), ]
han_rows <- data_rows2[grepl("韩荣昌", as.character(data_rows2[[4]]), fixed = TRUE) & 
                       !is.na(data_rows2[[4]]), ]
cat(sprintf("  找到 %d 条工作项\n", nrow(han_rows)))

items_han <- data.frame(
  employee_id = 16,
  indicator_code = sapply(han_rows[[2]], extract_indicator_code),
  source_title = sapply(han_rows[[3]], function(x) if (is.na(x)) "" else trimws(as.character(x))),
  stringsAsFactors = FALSE
)
items_han <- items_han[!is.na(items_han$indicator_code) & items_han$source_title != "", ]
cat(sprintf("  有效工作项: %d\n", nrow(items_han)))

# ========================================
# 文件3: 田予初 - 明细Sheet（自动检测）
# ========================================
cat("\n=== 处理: 田予初 ===\n")
f3 <- file.path(dir_path, "LVCC_研发中心_IT部田予初7月绩效明细_20260701 - 副本.xlsx")
sheets3 <- excel_sheets(f3)
detail_sheet3 <- sheets3[grepl("明细", sheets3)][1]
cat(sprintf("  使用Sheet: %s\n", detail_sheet3))
df3 <- read_excel(f3, sheet = detail_sheet3, col_names = FALSE)
data_rows3 <- df3[3:nrow(df3), ]
tian_rows <- data_rows3[grepl("田予初", as.character(data_rows3[[4]]), fixed = TRUE) & 
                        !is.na(data_rows3[[4]]), ]
cat(sprintf("  找到 %d 条工作项\n", nrow(tian_rows)))

items_tian <- data.frame(
  employee_id = 19,
  indicator_code = sapply(tian_rows[[2]], extract_indicator_code),
  source_title = sapply(tian_rows[[3]], function(x) if (is.na(x)) "" else trimws(as.character(x))),
  stringsAsFactors = FALSE
)
items_tian <- items_tian[!is.na(items_tian$indicator_code) & items_tian$source_title != "", ]
cat(sprintf("  有效工作项: %d\n", nrow(items_tian)))

# ========================================
# 文件4: 吴时超 - Sheet "1" (只有序号|分类|分类名称|项目/工作名称)
# 注意：分类列是 B4/B5/C9/C10 等代码，分类名称列是中文描述
# ========================================
cat("\n=== 处理: 吴时超 ===\n")
f4 <- file.path(dir_path, "LVCC_研发中心_吴时超7月绩效明细.xlsx")
df4 <- read_excel(f4, sheet = "1", col_names = FALSE)
# 跳过第1行(表头)
data_rows4 <- df4[2:nrow(df4), ]
cat(sprintf("  找到 %d 条工作项\n", nrow(data_rows4)))

items_wu <- data.frame(
  employee_id = 17,
  indicator_code = sapply(data_rows4[[2]], extract_indicator_code),
  source_title = sapply(data_rows4[[4]], function(x) if (is.na(x)) "" else trimws(as.character(x))),
  stringsAsFactors = FALSE
)
items_wu <- items_wu[!is.na(items_wu$indicator_code) & items_wu$source_title != "", ]
cat(sprintf("  有效工作项: %d\n", nrow(items_wu)))

# ========================================
# 合并所有工作项
# ========================================
all_items <- rbind(items_yang, items_han, items_tian, items_wu)
cat(sprintf("\n=== 总计: %d 条工作项 ===\n", nrow(all_items)))

# 打印汇总
for (eid in c(16, 17, 18, 19)) {
  emp_name <- names(which(emp_map == eid))
  emp_items <- all_items[all_items$employee_id == eid, ]
  cat(sprintf("\n%s (id=%d): %d 条\n", emp_name, eid, nrow(emp_items)))
  for (ic in unique(emp_items$indicator_code)) {
    cnt <- sum(emp_items$indicator_code == ic)
    cat(sprintf("  %s: %d 项\n", ic, cnt))
  }
}

# ========================================
# 导入数据库
# ========================================
cat("\n=== 开始导入数据库 ===\n")

# 获取7月绩效表
sheet <- perf_sheet_get_by_month("2026-07")
if (is.null(sheet)) {
  cat("创建 2026-07 绩效表...\n")
  r <- perf_sheet_create("2026-07")
  if (r$success) {
    sheet_id <- r$id
    cat(sprintf("  已创建, id=%d\n", sheet_id))
  } else {
    stop(r$message)
  }
} else {
  sheet_id <- sheet$id[1]
  cat(sprintf("使用已有绩效表, id=%d\n", sheet_id))
}

# 添加员工到绩效表
cat("添加员工到绩效表...\n")
r <- perf_sheet_employee_add(sheet_id, c(16, 17, 18, 19))
cat(sprintf("  %s\n", r$message))

# 清除已有工作项（重新导入）
con <- db_connect()
tryCatch({
  cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM performance_work_items WHERE sheet_id=%d", sheet_id))$cnt[1]
  if (cnt > 0) {
    cat(sprintf("清除已有 %d 条工作项...\n", cnt))
    dbExecute(con, sprintf("DELETE FROM performance_work_items WHERE sheet_id=%d", sheet_id))
  }
}, finally = { db_disconnect(con) })

# 批量导入工作项
imported <- 0
failed <- 0
for (i in 1:nrow(all_items)) {
  item <- all_items[i, ]
  r <- perf_work_item_add(
    sheet_id = sheet_id,
    employee_id = item$employee_id,
    indicator_code = item$indicator_code,
    source_type = "manual",
    source_id = NULL,
    source_title = item$source_title,
    deduction_level = 0
  )
  if (r$success) {
    imported <- imported + 1
  } else {
    failed <- failed + 1
    cat(sprintf("  失败 [%s] %s: %s\n", item$indicator_code, substr(item$source_title,1,50), r$message))
  }
}

cat(sprintf("\n导入完成: 成功 %d, 失败 %d\n", imported, failed))

# 验证
con <- db_connect()
tryCatch({
  cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM performance_work_items WHERE sheet_id=%d", sheet_id))$cnt[1]
  cat(sprintf("数据库中工作项总数: %d\n", cnt))
  
  # 按员工统计
  stats <- dbGetQuery(con, sprintf(
    "SELECT u.display_name, COUNT(*) as cnt 
     FROM performance_work_items pwi 
     JOIN users u ON pwi.employee_id = u.id 
     WHERE pwi.sheet_id = %d 
     GROUP BY pwi.employee_id 
     ORDER BY u.display_name", sheet_id))
  print(stats)
}, finally = { db_disconnect(con) })

cat("\n=== 导入完毕 ===\n")
