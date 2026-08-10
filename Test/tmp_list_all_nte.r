library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

d <- read_excel(f7, sheet = "26.7主管明细")
lines <- as.character(d[[1]])
lines[is.na(lines)] <- ""

# 列出所有NTE标题及其位置
cat("=== 全部NTE标题 ===\n")
nte_list <- list()
for(i in 1:length(lines)) {
  if(grepl("^[一二三四五六七八九十]+、.*NTE", lines[i])) {
    m <- regmatches(lines[i], regexec("(NTE\\d+).*?·\\s*(\\d+)条", lines[i], perl=TRUE))[[1]]
    if(length(m) >= 3) {
      nte_list[[length(nte_list)+1]] <- list(line=i, nte=m[2], title=lines[i], count=as.integer(m[3]))
    }
    cat(sprintf("%4d: %s\n", i, lines[i]))
  }
}

# 统计总数
total <- sum(sapply(nte_list, function(x) x$count))
cat(sprintf("\nNTE总数: %d个, 总条目: %d\n", length(nte_list), total))

# 看行393-1460中间还有哪些NTE
cat("\n=== 行393-1460 中的NTE ===\n")
for(i in 393:1460) {
  if(grepl("^[一二三四五六七八九十]+、.*NTE", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
  }
}
