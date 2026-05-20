library(RSQLite)
library(DT)

con <- dbConnect(SQLite(), "D:/GitHub/SuperITOM2/DB/GH_ITOM.db")

cat("=== 1. 直接数据库查询 ===\n")
data <- dbGetQuery(con, "SELECT p.*, u.username as creator_name FROM projects p LEFT JOIN users u ON p.created_by = u.id ORDER BY p.created_at DESC")
cat(sprintf("行数: %d, 列数: %d\n", nrow(data), ncol(data)))
cat("列名:", paste(names(data), collapse=", "), "\n")
print(data.frame(id=data$id, no=data$project_no, name=data$name, status=data$status))

cat("\n=== 2. 构建 Display data.frame ===\n")
if (nrow(data) > 0) {
  display <- data.frame(
    `操作` = sprintf('BTN'),
    `项目编号` = data$project_no,
    `项目名称` = data$name,
    `优先级` = data$priority,
    `状态` = data$status,
    `创建人` = ifelse(is.na(data$creator_name), "未知", data$creator_name),
    `开始` = ifelse(is.na(data$start_date), "-", data$start_date),
    `结束` = ifelse(is.na(data$end_date), "-", data$end_date),
    `更新时间` = ifelse(is.na(data$updated_at), "-", data$updated_at),
    stringsAsFactors = FALSE, check.names = FALSE)
  cat("display 行数:", nrow(display), "\n")
  cat("display 列数:", ncol(display), "\n")
  cat("display 列名:", paste(names(display), collapse=" | "), "\n")
  print(head(display))
}

dbDisconnect(con)
