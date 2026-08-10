library(readxl)

f7 <- "D:/Tai_LVCC_2026/Tai_10_OrganizationMangement/IT部_绩效管理(每月)/7月/LVCC_研发中心_IT部7月绩效明细_20260808.xlsx"

d <- read_excel(f7, sheet = "26.7主管明细")
lines <- as.character(d[[1]])
lines[is.na(lines)] <- ""

# 找行393之后所有📋 开头的行
cat("=== 行393之后所有📋行 ===\n")
for(i in 393:length(lines)) {
  if(grepl("^📋", lines[i])) {
    cat(sprintf("%4d: %s\n", i, lines[i]))
  }
}

# 看看393-1460之间的大结构
cat("\n=== 行393-1460 找所有标题行（不以工作/沟通开头的重要行）===\n")
for(i in 393:1460) {
  line <- lines[i]
  # 找包含关键字的行
  if(grepl("人力规划|绩效|培训|周报|请假|显示器|域控|采购|合同|付款|用印|审批|流程|服务器|网络|机房|监控|门禁|打印机|安全|EDR|DLP|加密|ERP|金蝶|WMS|CPQ|HRM|SCRM|SDWAN|专线|桌面云|VDI|数字合同|结构工程|新媒体|企微|售后|ISO|审计|智慧工厂|数智平台|HzB|Hertz|ITOM|PVE|存储|AP|无线|交换机|考勤|道闸|车牌|人脸|地图|高德|钉钉|泛微|E10|协同平台|合同管理|表单|流程|企业微信|工作台|待办|消息|组织|人事|部门|工资|HR|签到|灰度|AI|生图|WPS|乐享|外部联系人|WhatsApp|海外|SD-WAN|移动|电信|联通|深信服|厂商|拜访|会议|董事长|董总|肖总|谢芳材|张梦姣|李涛|邬|研发|营销|供应链|财务|行政|客服|厂房|综合楼|园区|消防|消控|漏水|腐蚀|钢结构|小程序|评价", lines[i])) {
    # 排除太细的沟通行
    if(!grepl("^沟通|^工作\\d|^\\+|^From|^To|^用户|^关于.*：$", lines[i])) {
      cat(sprintf("%4d: %s\n", i, substr(line, 1, 150)))
    }
  }
}
