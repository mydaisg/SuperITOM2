library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

d <- read_excel(f7, sheet = "26.7主管明细")
lines <- as.character(d[[1]])
lines[is.na(lines)] <- ""

# 行48-370: 协同平台 168条
cat("=== 协同平台 NTE (行48-370) 工作项标题 ===\n")
for(i in 48:370) {
  line <- lines[i]
  if(grepl("^工作\\d+$", line) && !grepl("沟通", line)) {
    # 打印下一行看内容
    cat(sprintf("%4d: %s\n", i, line))
    if(i+1 <= 370) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}

# 行393-1460: 智慧工厂 16条
cat("\n=== 智慧工厂 NTE (行393-1460) 工作项标题 ===\n")
for(i in 393:min(1460, length(lines))) {
  line <- lines[i]
  if(grepl("^工作\\d+$", line)) {
    cat(sprintf("%4d: %s\n", i, line))
    if(i+1 <= 1460) cat(sprintf("       -> %s\n", substr(lines[i+1], 1, 120)))
  }
}

# 派发记事汇总里的内容
cat("\n=== 派发记事汇总内容 (行1495-1533) ===\n")
for(i in 1495:length(lines)) {
  line <- lines[i]
  if(grepl("^📋", line)) cat(sprintf("%4d: %s\n", i, line))
}
