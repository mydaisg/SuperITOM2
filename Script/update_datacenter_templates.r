# 数据中心巡检模板更新脚本
# 使用方法：在R Console中运行 source("Script/update_datacenter_templates.r")

update_datacenter_templates <- function() {
  source("global.R")
  con <- db_connect()
  
  tryCatch({
    # 先删除旧的数据中心巡检模板
    dbExecute(con, "DELETE FROM inspection_item_templates WHERE category = '数据中心巡检'")
    cat("已删除旧的数据中心巡检模板\n")
    
    # 定义新的模板
    templates <- list(
      list(name = "温度检测", desc = "检测数据中心环境温度", standard = "标准范围18-26C", sort = 1),
      list(name = "湿度检测", desc = "检测数据中心环境湿度", standard = "标准范围40-60%", sort = 2),
      list(name = "灭火器检查", desc = "检查手提灭火器", standard = "压力正常、在有效期内、位置正确、无遮挡", sort = 3),
      list(name = "消防栓检查", desc = "检查室内消防栓", standard = "组件齐全、水带完好、无漏水", sort = 4),
      list(name = "烟雾探测器检查", desc = "检查烟感探测器", standard = "指示灯正常、灵敏度测试通过", sort = 5),
      list(name = "消防通道检查", desc = "检查消防逃生通道", standard = "畅通无阻、标识清晰", sort = 6),
      list(name = "UPS主机状态", desc = "检查UPS主机运行状态", standard = "市电正常或电池供电、运行指示灯绿色", sort = 7),
      list(name = "UPS负载率", desc = "检查UPS当前负载", standard = "负载率低于80%", sort = 8),
      list(name = "UPS电池状态", desc = "检查电池组状态", standard = "无告警、容量正常、连接牢固", sort = 9),
      list(name = "精密空调运行", desc = "检查精密空调运行状态", standard = "运行正常、无告警、温度设定正确", sort = 10),
      list(name = "空调回风温度", desc = "检测回风温度", standard = "在设定范围内18-24C", sort = 11),
      list(name = "空调滤网检查", desc = "检查滤网状态", standard = "滤网干净、需清洗更换时及时处理", sort = 12),
      list(name = "空调冷凝水", desc = "检查冷凝水排水", standard = "排水畅通、无积水、无漏水", sort = 13),
      list(name = "服务器指示灯", desc = "检查服务器前面板指示灯", standard = "所有指示灯绿色正常、无红灯或黄灯告警", sort = 14),
      list(name = "服务器系统状态", desc = "检查服务器操作系统", standard = "系统运行正常、无蓝屏或死机", sort = 15),
      list(name = "服务器资源使用", desc = "检查CPU/内存/磁盘使用", standard = "CPU低于80%、内存低于85%、磁盘低于90%", sort = 16),
      list(name = "服务器硬件告警", desc = "检查硬件监控告警", standard = "无硬件故障告警、iLO或IDRAC正常", sort = 17),
      list(name = "交换机指示灯", desc = "检查交换机端口指示灯", standard = "电源灯绿色、端口灯正常闪烁、无告警灯", sort = 18),
      list(name = "交换机端口状态", desc = "检查端口连接状态", standard = "端口UP、无err-disable、无丢弃", sort = 19),
      list(name = "交换机温度", desc = "检查交换机温度", standard = "温度正常、无过热告警", sort = 20),
      list(name = "交换机带宽使用", desc = "检查端口带宽利用率", standard = "带宽利用率低于70%、无网络拥塞", sort = 21),
      list(name = "路由器指示灯", desc = "检查路由器指示灯状态", standard = "电源正常、运行指示灯绿色、WAN/LAN口正常", sort = 22),
      list(name = "路由器端口状态", desc = "检查WAN/LAN端口", standard = "端口UP、协议正常、无错误计数", sort = 23),
      list(name = "路由器路由表", desc = "检查路由表状态", standard = "路由表正常、无黑洞路由", sort = 24),
      list(name = "路由器CPU/内存", desc = "检查路由器资源", standard = "CPU低于70%、内存低于80%", sort = 25),
      list(name = "存储设备指示灯", desc = "检查存储阵列指示灯", standard = "所有控制器绿灯、无故障灯告警", sort = 26),
      list(name = "存储控制器状态", desc = "检查存储控制器", standard = "双控运行正常、缓存保护正常", sort = 27),
      list(name = "存储磁盘状态", desc = "检查磁盘状态", standard = "无坏盘、无重构、无降级告警", sort = 28),
      list(name = "存储容量使用", desc = "检查存储使用率", standard = "容量使用低于85%、有足够快照空间", sort = 29),
      list(name = "门禁系统检查", desc = "检查门禁系统", standard = "门禁正常运行、开门记录正常、无异常闯入", sort = 30),
      list(name = "动力环境监控", desc = "检查动环监控系统", standard = "监控数据正常、无告警、无脱管设备", sort = 31),
      list(name = "场地环境检查", desc = "检查机房环境卫生", standard = "无漏水、无鼠患、无异味、地面干净", sort = 32),
      list(name = "强电配电检查", desc = "检查配电柜或PDU", standard = "配电正常、无异味、无过热、负载均衡", sort = 33)
    )
    
    for (t in templates) {
      sql <- sprintf(
        "INSERT INTO inspection_item_templates (category, item_name, item_description, check_standard, scoring_type, max_score, sort_order) 
         VALUES ('数据中心巡检', '%s', '%s', '%s', 'pass_fail', 100, %d)",
        gsub("'", "''", t$name),
        gsub("'", "''", t$desc),
        gsub("'", "''", t$standard),
        t$sort
      )
      dbExecute(con, sql)
    }
    
    cat(sprintf("已添加 %d 个数据中心巡检检查项\n", length(templates)))
    
    # 显示结果
    result <- dbGetQuery(con, "SELECT sort_order, item_name, check_standard FROM inspection_item_templates WHERE category = '数据中心巡检' ORDER BY sort_order")
    print(result)
    
  }, error = function(e) {
    cat("错误:", e$message, "\n")
  }, finally = {
    db_disconnect(con)
  })
}

cat("运行 update_datacenter_templates() 开始更新模板\n")
