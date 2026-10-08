source("global.R")
source("Script/daily_report.r")

users <- daily_report_get_users()
cat("users 行数:", nrow(users), "\n")
cat("users 列:", paste(names(users), collapse=", "), "\n")
print(users)
cat("\ndisplay_name 是 NA 的行:", which(is.na(users$display_name)), "\n")
cat("username 是 NA 的行:", which(is.na(users$username)), "\n")

# 检查 task_logs 的 creator_name 和 users 的匹配
d <- Sys.Date()
y <- as.integer(format(d, "%Y"))
dates <- seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by="day")
tl <- do.call(rbind, lapply(dates, daily_report_get_task_logs))
cat("\ntask_logs creator_name 唯一值:\n")
print(unique(tl$creator_name))
cat("creator_name 是 NA:", sum(is.na(tl$creator_name)), "\n")
