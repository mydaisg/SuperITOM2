# 性能监控服务端（无代理监控）

sysmon_server <- function(input, output, session, rv) {
  sysmon_trigger <- reactiveVal(0)
  # 扫描中止标志
  scan_stop <- reactiveVal(FALSE)
  # 扫描结果文本
  scan_result_text <- reactiveVal("")
  # 本地IP
  local_ip <- ""
  local_subnet <- "192.168.1"

  # 初始化：获取本机IP，自动添加本机监控
  observe({
    local_ip <<- sysmon_get_local_ip()
    local_subnet <<- sysmon_get_subnet(local_ip)
    hosts <- sysmon_host_list()
    if (!(local_ip %in% hosts$ip)) {
      hostname <- Sys.info()["nodename"]
      result <- sysmon_host_add(hostname=hostname, ip=local_ip, os_type="windows", remark="本机")
      if (result$success) {
        check <- sysmon_ping_check(local_ip)
        sysmon_check_log(result$id, "ping", ifelse(check$success,"success","fail"), check$ms, check$detail)
        sysmon_host_update_status(result$id, ifelse(check$success,"online","offline"), check$ms)
        sysmon_trigger(sysmon_trigger()+1)
      }
    }
  })

  # 统计卡片
  output$sysmon_stat_cards <- renderUI({
    sysmon_trigger()
    s <- sysmon_stats()
    fluidRow(
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#e3f2fd;",
        div(style="font-size:13px;color:#666;","监控主机"),
        div(style="font-size:28px;font-weight:bold;color:#1565c0;",s$total))),
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#e8f5e9;",
        div(style="font-size:13px;color:#666;","在线"),
        div(style="font-size:28px;font-weight:bold;color:#2e7d32;",s$online))),
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#ffebee;",
        div(style="font-size:13px;color:#666;","离线"),
        div(style="font-size:28px;font-weight:bold;color:#c62828;",s$offline))),
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#fff3e0;",
        div(style="font-size:13px;color:#666;","未知"),
        div(style="font-size:28px;font-weight:bold;color:#e65100;",s$unknown)))
    )
  })

  # 主机列表
  output$sysmon_host_table <- DT::renderDT({
    sysmon_trigger()
    hosts <- sysmon_host_list()
    if (nrow(hosts)==0) return(DT::datatable(data.frame(信息="暂无监控主机"),options=list(dom='t')))
    status_icons <- c("online"='<span style="color:#27ae60;font-weight:bold;">● 在线</span>',
      "offline"='<span style="color:#e74c3c;font-weight:bold;">● 离线</span>',
      "unknown"='<span style="color:#95a5a6;font-weight:bold;">● 未知</span>')
    display <- data.frame(
      主机名=hosts$hostname, IP=hosts$ip, 系统=hosts$os_type,
      状态=status_icons[hosts$status]%||%hosts$status,
      响应时间=sprintf("%d ms",hosts$response_time_ms),
      最后检测=hosts$last_check%||%"-",
      操作=sprintf(
        '<button class="btn btn-success btn-xs sysmon-check-btn" data-id="%s">检测</button>
         <button class="btn btn-info btn-xs sysmon-history-btn" data-id="%s">历史</button>
         <button class="btn btn-danger btn-xs sysmon-del-btn" data-id="%s">移除</button>',
        hosts$id, hosts$id, hosts$id),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=25,dom='rtip',scrollX=TRUE,
      columnDefs=list(list(targets=6,orderable=FALSE),list(targets=4,className='dt-center'),list(targets=3,className='dt-center'))),
      rownames=FALSE,class='cell-border stripe hover')
  })

  # 刷新
  observeEvent(input$sysmon_refresh, { sysmon_trigger(sysmon_trigger()+1) })

  # 添加主机
  observeEvent(input$sysmon_add, {
    showModal(modalDialog(title="添加监控主机",
      textInput("sysmon_new_name","主机名",placeholder="如: DC-Server"),
      textInput("sysmon_new_ip","IP地址",placeholder="如: 10.10.50.1"),
      selectInput("sysmon_new_os","操作系统",choices=c("Windows"="windows","Linux"="linux","其它"="other")),
      textInput("sysmon_new_port","端口(可选)",placeholder="留空仅Ping检测"),
      textInput("sysmon_new_remark","备注",placeholder="选填"),
      footer=tagList(modalButton("取消"),actionButton("sysmon_save_host","添加",class="btn-primary")),easyClose=TRUE))
  })

  observeEvent(input$sysmon_save_host, {
    req(input$sysmon_new_name, input$sysmon_new_ip)
    port <- if (nchar(trimws(input$sysmon_new_port))>0) as.integer(input$sysmon_new_port) else 0
    result <- sysmon_host_add(hostname=input$sysmon_new_name, ip=input$sysmon_new_ip,
      port=port, os_type=input$sysmon_new_os, remark=input$sysmon_new_remark%||%"")
    removeModal()
    if (result$success) {
      sysmon_trigger(sysmon_trigger()+1)
      showNotification(result$message,type="message")
      check <- sysmon_ping_check(input$sysmon_new_ip)
      sysmon_check_log(result$id, "ping", ifelse(check$success,"success","fail"), check$ms, check$detail)
      sysmon_host_update_status(result$id, ifelse(check$success,"online","offline"), check$ms)
      sysmon_trigger(sysmon_trigger()+1)
    } else showNotification(result$message,type="error")
  })

  # 扫描网络（实时 + 中止按钮）
  observeEvent(input$sysmon_scan, {
    scan_stop(FALSE)
    scan_result_text("")
    subnet <- local_subnet
    showModal(modalDialog(title="扫描网络",size="l",easyClose=TRUE,
      p("输入网段和起止IP，实时扫描发现存活主机。" ,style="font-size:12px;color:#666;"),
      fluidRow(
        column(4, textInput("sysmon_scan_subnet","网段",value=subnet,placeholder="如: 192.168.1")),
        column(3, numericInput("sysmon_scan_start","起始IP",value=1,min=1,max=254)),
        column(3, numericInput("sysmon_scan_end","结束IP",value=254,min=1,max=254))
      ),
      div(style="text-align:center;margin:8px 0;",
        actionButton("sysmon_start_scan","开始扫描",class="btn-primary",icon=icon("play")),
        actionButton("sysmon_stop_scan","中止",class="btn-danger",icon=icon("stop"),style="margin-left:10px;")),
      hr(),
      div(style="max-height:300px;overflow-y:auto;background:#1e1e1e;color:#d4d4d4;padding:10px;border-radius:4px;font-family:monospace;font-size:12px;",
        tags$pre(textOutput("sysmon_scan_progress"),style="margin:0;background:transparent;border:none;color:inherit;white-space:pre-wrap;"))))
  })

  # 实时扫描进度
  output$sysmon_scan_progress <- renderText({
    scan_result_text()
  })

  observeEvent(input$sysmon_start_scan, {
    subnet <- trimws(input$sysmon_scan_subnet)
    start_ip <- input$sysmon_scan_start%||%1
    end_ip <- input$sysmon_scan_end%||%254
    if (subnet=="") { showNotification("请输入网段",type="warning"); return() }
    scan_stop(FALSE)
    scan_result_text("准备开始扫描...\n")
    # 回调函数：每检测一个IP更新结果
    progress_cb <- function(ip, success, ms, detail) {
      status_str <- if (success) "● 存活" else "○ 无响应"
      current <- isolate(scan_result_text())
      scan_result_text(paste0(current, sprintf("[%d ms] %s  %-15s %s\n", ms, status_str, ip, detail)))
    }
    showNotification("开始扫描，可随时点击「中止」停止",type="message",id="scan_start_msg")
    hosts <- sysmon_scan_subnet(subnet, start_ip, end_ip, progress_callback=progress_cb, stop_flag=scan_stop)
    removeNotification("scan_start_msg")
    scan_result_text(paste0(isolate(scan_result_text()), sprintf("\n--- 扫描完成，发现 %d 台存活主机 ---\n",length(hosts))))
    if (length(hosts)>0) {
      added <- 0
      existing <- sysmon_host_list()
      for (h in hosts) {
        if (!(h$ip %in% existing$ip)) {
          result <- sysmon_host_add(hostname=h$hostname, ip=h$ip, os_type="windows")
          if (result$success) {
            sysmon_check_log(result$id, "ping", "success", h$ms, "扫描发现")
            sysmon_host_update_status(result$id, "online", h$ms)
            added <- added + 1
          }
        }
      }
      scan_result_text(paste0(isolate(scan_result_text()), sprintf("新增 %d 台到监控列表\n",added)))
      sysmon_trigger(sysmon_trigger()+1)
    }
  })

  # 中止扫描
  observeEvent(input$sysmon_stop_scan, {
    scan_stop(TRUE)
    scan_result_text(paste0(isolate(scan_result_text()), "\n--- 用户中止扫描 ---\n"))
  })

  # 检测单台
  observeEvent(input$sysmon_check_click, {
    host_id <- as.integer(input$sysmon_check_click)
    host <- sysmon_host_get(host_id)
    if (is.null(host)) return()
    check <- sysmon_ping_check(host$ip[1])
    status <- ifelse(check$success,"online","offline")
    sysmon_check_log(host_id, "ping", ifelse(check$success,"success","fail"), check$ms, check$detail)
    sysmon_host_update_status(host_id, status, check$ms)
    if (!is.null(host$port[1]) && host$port[1]>0) {
      port_check <- sysmon_port_check(host$ip[1], host$port[1])
      sysmon_check_log(host_id, "port", ifelse(port_check$success,"success","fail"), port_check$ms, port_check$detail)
    }
    sysmon_trigger(sysmon_trigger()+1)
    showNotification(sprintf("%s: %s (%d ms)", host$hostname[1], status, check$ms),type=ifelse(check$success,"message","warning"))
  })

  # 检测全部
  observeEvent(input$sysmon_check_all, {
    hosts <- sysmon_host_list()
    if (nrow(hosts)==0) { showNotification("无主机可检测",type="warning"); return() }
    showNotification(sprintf("正在检测 %d 台主机...",nrow(hosts)),type="message",duration=NULL,id="check_all_msg")
    online_count <- 0
    for (i in 1:nrow(hosts)) {
      h <- hosts[i, ]
      check <- sysmon_ping_check(h$ip)
      status <- ifelse(check$success,"online","offline")
      if (check$success) online_count <- online_count + 1
      sysmon_check_log(h$id, "ping", ifelse(check$success,"success","fail"), check$ms, check$detail)
      sysmon_host_update_status(h$id, status, check$ms)
      if (!is.null(h$port) && !is.na(h$port) && h$port>0) {
        port_check <- sysmon_port_check(h$ip, h$port)
        sysmon_check_log(h$id, "port", ifelse(port_check$success,"success","fail"), port_check$ms, port_check$detail)
      }
    }
    removeNotification("check_all_msg")
    sysmon_trigger(sysmon_trigger()+1)
    showNotification(sprintf("检测完成: %d/%d 在线",online_count,nrow(hosts)),type="message",duration=8)
  })

  # 历史记录
  observeEvent(input$sysmon_history_click, {
    host_id <- as.integer(input$sysmon_history_click)
    host <- sysmon_host_get(host_id)
    if (is.null(host)) return()
    logs <- sysmon_check_history(host_id, 50)
    if (nrow(logs)==0) { showNotification("暂无历史记录",type="message"); return() }
    log_text <- paste(
      sprintf("=== %s (%s) 检测历史 ===\n", host$hostname[1], host$ip[1]),
      paste(apply(logs,1,function(r) sprintf("[%s] %s: %s (%d ms) %s",
        r["checked_at"],r["check_type"],r["status"],as.integer(r["response_time_ms"]),r["detail"])),collapse="\n"),
      sep="\n")
    showModal(modalDialog(title=sprintf("检测历史: %s",host$hostname[1]),
      pre(log_text,style="font-size:12px;max-height:400px;overflow:auto;background:#f5f5f5;padding:10px;"),
      footer=modalButton("关闭"),size="l",easyClose=TRUE))
  })

  # 删除主机
  observeEvent(input$sysmon_del_click, {
    sysmon_host_delete(as.integer(input$sysmon_del_click))
    sysmon_trigger(sysmon_trigger()+1)
    showNotification("已移除",type="message")
  })

  # ========== 自动检测（每3分钟） ==========
  observe({
    invalidateLater(180000, session)
    hosts <- sysmon_host_list()
    if (nrow(hosts) == 0) return()
    for (i in seq_len(nrow(hosts))) {
      h <- hosts[i, ]
      check <- sysmon_ping_check(h$ip)
      status <- ifelse(check$success, "online", "offline")
      sysmon_check_log(h$id, "ping", ifelse(check$success,"success","fail"), check$ms, check$detail)
      sysmon_host_update_status(h$id, status, check$ms)
    }
    sysmon_trigger(sysmon_trigger()+1)
  })
}
