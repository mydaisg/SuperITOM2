library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

d <- read_excel(f7, sheet = "26.7主管明细")
lines <- as.character(d[[1]])
lines[is.na(lines)] <- ""
n <- length(lines)

# 找第一个大区块结束位置（最后一个NTE标题之前）
cat("=== NTE标题位置 ===\n")
nte_info <- list()
for(i in 1:n) {
  if(grepl("^[一二三四五六七八九十]、.*NTE", lines[i])) {
    m <- regmatches(lines[i], regexec("(NTE\\d+).*?·\\s*(\\d+)条", lines[i], perl=TRUE))[[1]]
    if(length(m) >= 3) {
      nte_info[[length(nte_info)+1]] <- list(line=i, nte=m[2], title=lines[i], count=as.integer(m[3]))
    }
    cat(sprintf("%4d: %s\n", i, lines[i]))
  }
}

# 统计NTE总条数
total_nte <- sum(sapply(nte_info, function(x) x$count))
cat(sprintf("\nNTE总记事条数: %d\n", total_nte))

# 找第二个区块起始（可能有个"韩"字或名字开头）
cat("\n=== 找第二个名字区块 ===\n")
for(i in 1:n) {
  if(grepl("^韩荣昌$|^韩$", lines[i])) {
    cat(sprintf("找到: 行%d: %s\n", i, lines[i]))
    for(j in i:min(i+10, n)) cat(sprintf("%4d: %s\n", j, lines[j]))
    break
  }
}

# 打印行1463到尾部
cat("\n=== 行1460-1533 ===\n")
for(i in 1460:n) cat(sprintf("%4d: %s\n", i, lines[i]))

# 统计第一个大区块（戴诗贡本人）的内容
# 第一个NTE到第十个NTE结束位置
cat("\n=== 第一大区块 NTE统计 ===\n")
for(x in nte_info) {
  if(x$line < 500) cat(sprintf("  %s: %d条\n", x$nte, x$count))
}

# 第二大区块 NTE
cat("\n=== 第二大区块 NTE统计 ===\n")
for(x in nte_info) {
  if(x$line > 1400) cat(sprintf("  %s: %d条\n", x$nte, x$count))
}
