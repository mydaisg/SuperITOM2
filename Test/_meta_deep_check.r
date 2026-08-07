# 深入检查数据：看看"当前节点"的分布和"已完成"的判断逻辑
library(readxl)

xlsx_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/LVCC_流程中心_试运行_8月1-6数据.xlsx"
df <- read_excel(xlsx_file, sheet = "sheet1")

# 当前节点分布
cat("=== 当前节点 分布 (Top 30) ===\n")
node_tbl <- sort(table(df$当前节点), decreasing = TRUE)
print(head(node_tbl, 30))

# 看看"结束节点"的流程，是否真的都完成了
ended <- df[df$当前节点 == "结束节点", ]
cat("\n=== 结束节点的流程示例 ===\n")
print(head(ended[, c("流程名称", "当前节点", "发起人")], 10))

# 也看看旧的 xlsx
old_file <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据/流程监控20260802085103869.xlsx"
old_df <- read_excel(old_file, sheet = "sheet1")
cat("\n=== 旧数据 当前节点分布 ===\n")
old_nodes <- sort(table(old_df$当前节点), decreasing = TRUE)
print(head(old_nodes, 20))

# 旧数据中完成率是 54.1%，看看是怎么算的
cat("\n=== 旧数据总行数:", nrow(old_df), "===\n")
cat("旧数据 结束节点 数量:", sum(old_df$当前节点 == "结束节点"), "\n")
cat("旧数据中 '完成' 为节点名:", sum(old_df$当前节点 == "完成"), "\n")
cat("旧数据中 '已完成' 为节点名:", sum(old_df$当前节点 == "已完成"), "\n")

# 看来"结束节点"在旧数据中也很多
# 我们看看旧HTML中 completed=250 是怎么来的
# 可能是所有非"进行中"状态的节点都算完成？
# 比如：结束节点、完成、已完成 都算
cat("\n旧数据中可能算完成的节点:\n")
for (nd in names(head(old_nodes, 10))) {
  cat("  ", nd, ":", old_nodes[nd], "\n")
}

# 实际上，旧看板的已完成=250，总=462
# 结束节点: 看看旧数据
ended_old <- sum(old_df$当前节点 == "结束节点")
cat("\n旧数据结束节点:", ended_old, "\n")

# 也许看板里 "已完成" = 流程总数 - 进行中
# 而进行中就是当前节点不等于"结束节点"等的
# 我们试试：把所有节点分类
completed_keywords <- c("结束节点", "完成", "已完成", "结束")
old_df$is_done <- old_df$当前节点 %in% completed_keywords | is.na(old_df$当前节点) | old_df$当前节点 == ""
cat("旧数据 is_done:", sum(old_df$is_done), "(should be 250?)\n")

# 462-250=212 进行中
# 旧数据 结束节点: ?
# 等等，看看看板里旧数据那行：
# 已完成流程 250, 进行中 212, 总数 462
# 250+212=462
# 那250是怎么来的？不是结束节点，而是... 
# 让我直接看看看板中的旧数据daily:
# completed: [26, 35, 35, 62, 68, 24] = 250
# 这个不是从"结束节点"算的，而是从每天的数据中统计的

# 也许"已完成"不是看节点，而是看流程名称中是否有特定标记？
# 或者更简单：流程有"结束节点"就说明已经走完了
# 但旧数据中结束节点可能是...
cat("\n旧数据 结束节点 数量:", sum(old_df$当前节点 == "结束节点"), "\n")
cat("旧数据总数:", nrow(old_df), "\n")
cat("如果 completed=结束节点数量:", sum(old_df$当前节点 == "结束节点"), " vs 250\n")
