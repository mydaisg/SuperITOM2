source("global.R")
source("Script/daily_report.r")

# 检查 daily_report_get_task_logs 返回结构
d <- Sys.Date()
r <- daily_report_get_task_logs(d)
cat("单日 task_logs 行数:", nrow(r), "\n")
cat("列名:", paste(names(r), collapse=", "), "\n")
if (nrow(r) > 0) {
  cat("content 类型:", class(r$content), "\n")
  cat("content 值:", paste(r$content, collapse=" | "), "\n")
  cat("task_name 类型:", class(r$task_name), "\n")
  str(r)
}

# 本年度模式：逐日合并
y <- as.integer(format(d, "%Y"))
dates <- seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by="day")
cat("\n本年度天数:", length(dates), "\n")
tl <- do.call(rbind, lapply(dates, daily_report_get_task_logs))
cat("合并后 task_logs 行数:", nrow(tl), "\n")
cat("合并后列名:", paste(names(tl), collapse=", "), "\n")
if (nrow(tl) > 0) {
  cat("content 类型:", class(tl$content), "\n")
  cat("content 是否有 NA:", sum(is.na(tl$content)), "\n")
  # 模拟第649行
  for (i in 1:min(3, nrow(tl))) {
    lg <- tl[i, ]
    cat("行", i, "content=", lg$content, " nchar=", nchar(lg$content), "\n")
  }
}
