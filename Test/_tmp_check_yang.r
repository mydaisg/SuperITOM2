library(readxl)
f <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/9月/LVCC_研发中心_IT部_9月工作明细.xlsx"
df <- read_excel(f, sheet = "26.9杨长湖", col_names = FALSE)
cat("维度:", nrow(df), "x", ncol(df), "\n\n")

# 打印所有行: 序号/指标/工作项前40字/纳入/姓名/计分
for (i in seq_len(nrow(df))) {
  no <- df[[1]][i]; code <- df[[2]][i]; title <- df[[3]][i]
  incl <- if (ncol(df)>=4) df[[4]][i] else NA
  nm <- if (ncol(df)>=5) df[[5]][i] else NA
  sc <- if (ncol(df)>=6) df[[6]][i] else NA
  t <- if (is.na(title)) "" else trimws(substr(as.character(title),1,40))
  cat(sprintf("行%3d 序号%-4s 指标%-22s 纳入%-3s 姓名%-5s 计分%-5s | %s\n", i,
    ifelse(is.na(no),"-",no), ifelse(is.na(code),"-",substr(as.character(code),1,20)),
    ifelse(is.na(incl),"-",as.character(incl)),
    ifelse(is.na(nm),"-",as.character(nm)),
    ifelse(is.na(sc),"-",as.character(sc)), t))
}
