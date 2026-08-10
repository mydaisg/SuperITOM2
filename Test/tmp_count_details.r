library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

d <- read_excel(f7, sheet = "26.7主管明细")
lines <- as.character(d[[1]])
lines[is.na(lines)] <- ""

# IT服务工单 (行819-1157, 169条)
cat("=== IT服务工单 内容(行819-1157) ===\n")
# 统计工单条目
work_order_count <- 0
for(i in 819:1157) {
  line <- lines[i]
  if(grepl("^工作\\d+$", line)) {
    work_order_count <- work_order_count + 1
    cat(sprintf("%4d: %s\n", i, line))
    if(i+1 <= 1157) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}
cat(sprintf("\nIT服务工单工作项数: %d\n", work_order_count))

# 系统权限审批 (行632-766, 67条)
cat("\n=== 系统权限审批 工作项数 ===\n")
perm_count <- 0
for(i in 632:766) {
  if(grepl("^工作\\d+$", lines[i])) perm_count <- perm_count + 1
}
cat(sprintf("权限审批工作项数: %d\n", perm_count))

# 采购申请 (行773-779, 3条)
cat("\n=== 采购申请 ===\n")
for(i in 773:779) {
  if(grepl("^工作\\d+$", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    if(i+1 <= 779) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}

# 付款申请 (行780-784, 2条)
cat("\n=== 付款申请 ===\n")
for(i in 780:784) {
  if(grepl("^工作\\d+$", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    if(i+1 <= 784) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}

# 问题管理 (行593-609, 8条)
cat("\n=== IT问题管理 ===\n")
for(i in 593:609) {
  if(grepl("^工作\\d+$", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    if(i+1 <= 609) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}

# 事件管理 (行610-614, 2条)
cat("\n=== IT事件管理 ===\n")
for(i in 610:614) {
  if(grepl("^工作\\d+$", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    if(i+1 <= 614) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}

# 资产管理 (行785-805, 10条)
cat("\n=== IT资产管理 ===\n")
for(i in 785:805) {
  if(grepl("^工作\\d+$", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
    if(i+1 <= 805) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}
