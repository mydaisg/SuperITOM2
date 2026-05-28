# 性能监控 UI（无代理监控）

sysmon_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.sysmon-del-btn',function(e){e.preventDefault();if(confirm('确定移除此主机？'))Shiny.setInputValue('sysmon_del_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.sysmon-check-btn',function(e){e.preventDefault();Shiny.setInputValue('sysmon_check_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.sysmon-history-btn',function(e){e.preventDefault();Shiny.setInputValue('sysmon_history_click',$(this).data('id'),{priority:'event'});});
    ")),
    fluidPage(
      div(style="text-align:center;margin:8px 0;",
        h2(icon("heartbeat")," 性能监控"),
        p(style="color:#7f8c8d;font-size:12px;","无代理监控 · 连通性检测 · 可用性展示")),
      uiOutput("sysmon_stat_cards"),
      fluidRow(
        column(2, actionButton("sysmon_add","添加主机",class="btn-primary",icon=icon("plus"))),
        column(2, actionButton("sysmon_scan","扫描网络",class="btn-info",icon=icon("search"),style="margin-left:5px;")),
        column(2, actionButton("sysmon_check_all","检测全部",class="btn-warning",icon=icon("play"),style="margin-left:5px;")),
        column(2, div(style="margin-top:0;margin-left:5px;",actionButton("sysmon_refresh","刷新",class="btn-default btn-sm")))
      ), br(),
      DT::DTOutput("sysmon_host_table")
    )
  )
}
