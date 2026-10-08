source("global.R")
source("Script/daily_report.r")

d <- Sys.Date()
dm <- as.Date(format(d, "%Y-%m-01"))
dlm <- as.Date(format(d, "%Y-%m-01")) - 1
dlm <- as.Date(format(dlm, "%Y-%m-01"))

# 上月 30 天，验证 rbind 合并
dates <- seq(dlm, seq(dlm, by="month", length.out=2)[2]-1, by="day")
cat("上月天数:", length(dates), "\n")

res_list <- lapply(dates, daily_report_get_work_orders)
# 检查每个返回的列名是否一致
cat("各日返回的列数（前几个非空的）:\n")
nonempty <- which(sapply(res_list, function(x) nrow(x) > 0))
cat("非空日期索引数:", length(nonempty), "\n")
if (length(nonempty) > 0) {
  cat("非空日期列名:\n")
  print(names(res_list[[nonempty[1]]]))
}

wo <- do.call(rbind, res_list)
cat("合并后 work_orders 行数:", nrow(wo), " 列数:", ncol(wo), "\n")
cat("合并后列名:\n")
print(names(wo))

# 检查去重后
wo2 <- wo[!duplicated(wo$id), ]
cat("去重后行数:", nrow(wo2), "\n")

# 检查 id 列是否正常
cat("id 列前5个:", head(wo$id), "\n")
