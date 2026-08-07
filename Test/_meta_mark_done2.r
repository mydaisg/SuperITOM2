source("global.R")
source("Script/note_management.r")

# 标记评论1432为已完成
r <- note_comment_mark_status(1432, "completed")
cat("标记结果:", r$message, "\n")

# 添加完成评论
r2 <- note_comment_add(6, sprintf("已完成: 4位员工7月绩效明细已导入，共200条工作项。\n- 韩荣昌: 30项\n- 吴时超: 15项\n- 杨长湖: 48项\n- 田予初: 107项\n完成时间: %s", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat("评论结果:", r2$message, "\n")
