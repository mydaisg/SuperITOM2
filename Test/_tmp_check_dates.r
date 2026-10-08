# 检查日报各筛选按键的日期范围与数据分布
source("global.R")
source("Script/daily_report.r")

con <- db_connect()
cat("=== 数据分布（近14天）===\n")
cat("--- work_orders ---\n")
print(dbGetQuery(con, "SELECT substr(created_at,1,10) d, COUNT(*) n FROM work_orders GROUP BY d ORDER BY d DESC LIMIT 14"))
cat("--- project_tasks ---\n")
print(dbGetQuery(con, "SELECT substr(created_at,1,10) d, COUNT(*) n FROM project_tasks GROUP BY d ORDER BY d DESC LIMIT 10"))
cat("--- note_comments ---\n")
print(dbGetQuery(con, "SELECT substr(created_at,1,10) d, COUNT(*) n FROM note_comments GROUP BY d ORDER BY d DESC LIMIT 10"))
dbDisconnect(con)

cat("\n=== 各按键日期范围计算结果 ===\n")
d <- Sys.Date()
cat("今天:", format(d), "\n")
cat("昨天:", format(d - 1), "\n")

# 本周
monday <- d - as.integer(format(d, "%u")) + 1
sunday <- min(monday + 6, d)
cat("本周:", format(monday), "~", format(sunday), "\n")

# 上周
d2 <- d - 7
monday2 <- d2 - as.integer(format(d2, "%u")) + 1
sunday2 <- monday2 + 6
cat("上周:", format(monday2), "~", format(sunday2), "\n")

# 本月
dm <- as.Date(format(d, "%Y-%m-01"))
cat("本月:", format(dm), "~", format(seq(dm, by="month", length.out=2)[2] - 1), "\n")

# 上月
dlm <- as.Date(format(d, "%Y-%m-01")) - 1
dlm <- as.Date(format(dlm, "%Y-%m-01"))
cat("上月:", format(dlm), "~", format(seq(dlm, by="month", length.out=2)[2] - 1), "\n")

# 本季度
y <- as.integer(format(d, "%Y"))
q <- (as.integer(format(d, "%m")) - 1) %/% 3 + 1
sm <- (q - 1) * 3 + 1
qs <- as.Date(sprintf("%d-%02d-01", y, sm))
qe <- seq(qs, by="month", length.out=4)[4] - 1
cat("本季度(Q", q, "):", format(qs), "~", format(qe), "\n")

# 本年度
cat("本年度:", sprintf("%d-01-01", y), "~", sprintf("%d-12-31", y), "\n")

# 验证数据层函数在"昨天"能否取到数据
cat("\n=== 昨天数据量验证 ===\n")
yday <- d - 1
wo <- daily_report_get_work_orders(yday)
tk <- daily_report_get_tasks(yday)
nc <- daily_report_get_note_comments(yday)
cat("昨天工单:", nrow(wo), " 任务:", nrow(tk), " 记事评论:", nrow(nc), "\n")
