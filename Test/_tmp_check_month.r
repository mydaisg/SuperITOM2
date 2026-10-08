# 检查月报模式各日期范围实际取数
source("global.R")
source("Script/daily_report.r")

test_month_mode <- function(label, start, end) {
  dates <- seq(as.Date(start), as.Date(end), by = "day")
  cat("\n=== ", label, " : ", format(dates[1]), " ~ ", format(dates[length(dates)]), " (共", length(dates), "天) ===\n")
  wo <- do.call(rbind, lapply(dates, daily_report_get_work_orders))
  tk <- do.call(rbind, lapply(dates, daily_report_get_tasks))
  tl <- do.call(rbind, lapply(dates, daily_report_get_task_logs))
  nc <- do.call(rbind, lapply(dates, daily_report_get_note_comments))
  cat("工单:", if(is.null(wo)) 0 else nrow(wo), "\n")
  cat("任务:", if(is.null(tk)) 0 else nrow(tk), "\n")
  cat("反馈日志:", if(is.null(tl)) 0 else nrow(tl), "\n")
  cat("记事评论:", if(is.null(nc)) 0 else nrow(nc), "\n")
}

d <- Sys.Date()
# 昨天（日模式应该和月模式单日一致）
test_month_mode("昨天(单日)", d-1, d-1)
# 本周
monday <- d - as.integer(format(d, "%u")) + 1
test_month_mode("本周", monday, d)
# 本月
dm <- as.Date(format(d, "%Y-%m-01"))
test_month_mode("本月", dm, seq(dm, by="month", length.out=2)[2]-1)
# 上月
dlm <- as.Date(format(d, "%Y-%m-01")) - 1
dlm <- as.Date(format(dlm, "%Y-%m-01"))
test_month_mode("上月", dlm, seq(dlm, by="month", length.out=2)[2]-1)

# 单独看昨天 note_comments 的内容细节
cat("\n=== 昨天 note_comments 前5条 ===\n")
nc1 <- daily_report_get_note_comments(d-1)
if (nrow(nc1) > 0) print(head(nc1[, c("id","note_no","title","content","status","parent_id")], 5))
