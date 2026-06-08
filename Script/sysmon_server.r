# 性能监控服务端 — 参照 note_server 完全重写
# 所有 UI 通过 renderUI 动态渲染，避免静态 DTOutput 冲突

sysmon_server <- function(input, output, session, rv) {
  sysmon_trigger <- reactiveVal(0)
  
  ##################
  # 统计卡片
  ##################
  output$sysmon_stat_cards <- renderUI({
    req(rv$logged_in); sysmon_trigger()
    s <- sysmon_stats()
    fluidRow(
      column(3, div(class="well well-sm", style="text-align:center;padding:12px;background:#e3f2fd;",
        div(style="font-size:13px;color:#666;","监控主机"),
        div(style="font-size:28px;font-weight:bold;color:#1565c0;",s$total))),
      column(3, div(class="well well-sm", style="text-align:center;padding:12px;background:#e8f5e9;",
        div(style="font-size:13px;color:#666;","在线"),
        div(style="font-size:28px;font-weight:bold;color:#2e7d32;",s$online))),
      column(3, div(class="well well-sm", style="text-align:center;padding:12px;background:#ffebee;",
        div(style="font-size:13px;color:#666;","离线"),
        div(style="font-size:28px;font-weight:bold;color:#c62828;",s$offline))),
      column(3, div(class="well well-sm", style="text-align:center;padding:12px;background:#fff3e0;",
        div(style="font-size:13px;color:#666;","未知"),
        div(style="font-size:28px;font-weight:bold;color:#e65100;",s$unknown)))
    )
  })

  ##################
  # 主机列表表格（通过 renderUI 动态渲染）
  ##################
  output$sysmon_host_table_render <- renderUI({
    req(rv$logged_in)
    DT::DTOutput("sysmon_host_table")
  })
  output$sysmon_host_table <- DT::renderDT({
    req(rv$logged_in); sysmon_trigger()
    hosts <- sysmon_host_list()
    if (nrow(hosts)==0) return(DT::datatable(data.frame(信息="暂无监控主机"),options=list(dom='t')))
    status_icons <- c("online"='<span style="color:#27ae60;font-weight:bold;">● 在线</span>',
      "offline"='<span style="color:#e74c3c;font-weight:bold;">● 离线</span>',
      "unknown"='<span style="color:#95a5a6;font-weight:bold;">● 未知</span>')
    display <- data.frame(
      主机名=hosts$hostname, IP=hosts$ip, 系统=hosts$os_type,
      状态=status_icons[hosts$status] %||% hosts$status,
      响应时间=sprintf("%d ms",hosts$response_time_ms),
      最后检测=hosts$last_check %||% "-",
      操作=sprintf(
        '<button class="btn btn-success btn-xs sysmon-check-btn" data-id="%s">检测</button> <button class="btn btn-info btn-xs sysmon-hist-btn" data-id="%s">历史</button> <button class="btn btn-danger btn-xs sysmon-del-btn" data-id="%s">移除</button>',
        hosts$id, hosts$id, hosts$id),
      stringsAsFactors=FALSE)
    DT::datatable(display, escape=FALSE,
      options=list(pageLength=25, dom='rtip', scrollX=TRUE,
        columnDefs=list(list(targets=6,orderable=FALSE))),
      rownames=FALSE, class='cell-border stripe hover')
  })

  ##################
  # 主界面（通过 renderUI 动态渲染，参照 note_server 模式）
  ##################
  output$sysmon_main <- renderUI({
    req(rv$logged_in); sysmon_trigger()
    tagList(
      div(style="text-align:center;margin:10px 0;",
        h2(icon("heartbeat")," 性能监控"),
        p(style="color:#7f8c8d;font-size:12px;","无代理监控 · 连通性检测 · 可用性展示")),
      uiOutput("sysmon_stat_cards"),
      fluidRow(
        column(2, actionButton("sysmon_add_btn","添加主机",class="btn-primary",icon=icon("plus"))),
        column(2, actionButton("sysmon_check_all_btn","检测全部",class="btn-warning",icon=icon("play"))),
        column(2, actionButton("sysmon_refresh_btn","刷新",class="btn-default btn-sm"))
      ),
      br(),
      uiOutput("sysmon_host_table_render")
    )
  })

  ##################
  # 操作事件
  ##################
  observeEvent(input$sysmon_refresh_btn, { req(rv$logged_in); sysmon_trigger(sysmon_trigger()+1) })

  observeEvent(input$sysmon_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(title="添加监控主机",
      textInput("sysmon_new_name","主机名"),
      textInput("sysmon_new_ip","IP地址"),
      selectInput("sysmon_new_os","操作系统",choices=c("Windows"="windows","Linux"="linux","其它"="other")),
      textInput("sysmon_new_port","端口(可选)"),
      footer=tagList(modalButton("取消"),actionButton("sysmon_save_host","添加",class="btn-primary")),
      easyClose=TRUE))
  })

  observeEvent(input$sysmon_save_host, {
    req(rv$logged_in, input$sysmon_new_name, input$sysmon_new_ip)
    port <- if (isTRUE(nchar(trimws(input$sysmon_new_port))>0)) as.integer(input$sysmon_new_port) else 0
    result <- sysmon_host_add(hostname=input$sysmon_new_name, ip=input$sysmon_new_ip, port=port, os_type=input$sysmon_new_os)
    removeModal(); sysmon_trigger(sysmon_trigger()+1)
    showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$sysmon_check, {
    req(rv$logged_in)
    host_id <- as.integer(input$sysmon_check)
    host <- sysmon_host_get(host_id)
    if (is.null(host)) return()
    check <- sysmon_smart_check(host$ip[1])
    status <- ifelse(check$success,"online","offline")
    sysmon_check_log(host_id,"ping",ifelse(check$success,"success","fail"),check$ms,check$detail)
    sysmon_host_update_status(host_id,status,check$ms)
    sysmon_trigger(sysmon_trigger()+1)
    showNotification(sprintf("%s: %s — %s",host$hostname[1],ifelse(check$success,"在线","离线"),check$detail),
      type=ifelse(check$success,"message","warning"))
  })

  observeEvent(input$sysmon_check_all_btn, {
    req(rv$logged_in)
    hosts <- sysmon_host_list()
    if (nrow(hosts)==0) { showNotification("无主机可检测",type="warning"); return() }
    online_cnt <- 0; offline_cnt <- 0
    for (i in seq_len(nrow(hosts))) {
      h <- hosts[i,]
      check <- sysmon_smart_check(h$ip)
      status <- ifelse(check$success,"online","offline")
      if (check$success) online_cnt <- online_cnt + 1 else offline_cnt <- offline_cnt + 1
      sysmon_check_log(h$id,"ping",ifelse(check$success,"success","fail"),check$ms,check$detail)
      sysmon_host_update_status(h$id,status,check$ms)
    }
    sysmon_trigger(sysmon_trigger()+1)
    showNotification(sprintf("检测完成: %d在线 %d离线",online_cnt,offline_cnt),type="message")
  })

  # 自动定时检测（isolate 防止死循环）
  sysmon_timer_skip <- TRUE
  observe({
    req(rv$logged_in)
    invalidateLater(300000)
    if (sysmon_timer_skip) { sysmon_timer_skip <<- FALSE; return() }
    hosts <- tryCatch(sysmon_host_list(), error=function(e) data.frame())
    if (nrow(hosts) == 0) return()
    for (i in seq_len(nrow(hosts))) {
      h <- hosts[i,]
      check <- sysmon_smart_check(h$ip)
      status <- ifelse(check$success,"online","offline")
      sysmon_check_log(h$id,"ping",ifelse(check$success,"success","fail"),check$ms,check$detail)
      sysmon_host_update_status(h$id,status,check$ms)
    }
    isolate(sysmon_trigger(sysmon_trigger() + 1))
  })

  observeEvent(input$sysmon_history, {
    req(rv$logged_in)
    host <- sysmon_host_get(as.integer(input$sysmon_history))
    if (is.null(host)) return()
    logs <- sysmon_check_history(as.integer(input$sysmon_history), 50)
    if (nrow(logs)>0) {
      log_html <- paste(apply(logs,1,function(r) {
        st <- r["status"]; detail <- r["detail"] %||% ""
        clr <- if(st=="success") "#27ae60" else "#e74c3c"
        sprintf('<div style="padding:2px 0; border-bottom:1px solid #f0f0f0;">
          <span style="color:#999;font-size:10px;">%s</span>
          <span style="color:%s;font-weight:bold;margin-left:6px;">●</span>
          <span style="margin-left:4px;">%s (%dms)</span>
          <span style="color:#888;font-size:10px;margin-left:6px;">%s</span></div>',
          substr(r["checked_at"],1,16), clr, r["status"], as.integer(r["response_time_ms"]), detail)
      }),collapse="\n")
    } else {
      log_html <- '<p style="color:#999;">暂无记录</p>'
    }
    showModal(modalDialog(title=sprintf("历史: %s — %s",host$hostname[1],host$ip[1]),
      tags$div(HTML(log_html),style="font-size:12px;max-height:400px;overflow:auto;"),
      footer=modalButton("关闭"),easyClose=TRUE))
  })

  observeEvent(input$sysmon_del, {
    req(rv$logged_in)
    sysmon_host_delete(as.integer(input$sysmon_del))
    sysmon_trigger(sysmon_trigger()+1)
    showNotification("已移除",type="message")
  })
}
