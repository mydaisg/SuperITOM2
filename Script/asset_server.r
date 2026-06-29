# 资产管理模块 - 服务端

asset_server <- function(input, output, session, rv) {
  
  asset_trigger <- reactiveVal(0)
  
  ##################
  # 统计栏
  ##################
  output$asset_stats <- renderUI({
    asset_trigger()
    req(rv$logged_in)
    items <- asset_get_all()
    tags$div(style = "display:flex; gap:10px; margin-bottom:8px;",
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#d4edda;",
        tags$div(class = "num", style = "color:#155724;", nrow(items)),
        tags$div(class = "lbl", style = "color:#155724;", "全部资产")
      ),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#d4edda;",
        tags$div(class = "num", style = "color:#155724;", sum(items$status == "active", na.rm = TRUE)),
        tags$div(class = "lbl", style = "color:#155724;", "使用中")
      ),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#fff3cd;",
        tags$div(class = "num", style = "color:#856404;", sum(items$status == "maintenance", na.rm = TRUE)),
        tags$div(class = "lbl", style = "color:#856404;", "维护中")
      ),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#f8d7da;",
        tags$div(class = "num", style = "color:#721c24;", sum(items$status == "retired", na.rm = TRUE)),
        tags$div(class = "lbl", style = "color:#721c24;", "已退役")
      )
    )
  })
  outputOptions(output, "asset_stats", suspendWhenHidden = FALSE)
  
  ##################
  # 资产表格
  ##################
  output$asset_table <- renderDT({
    asset_trigger()
    req(rv$logged_in)
    items <- asset_get_all()
    if (nrow(items) == 0) return(datatable(data.frame(提示="暂无资产"), options = list(dom="t")))
    
    # 搜索过滤
    kw <- trimws(input$asset_search)
    if (length(kw) > 0 && kw != "") {
      items <- items[grepl(kw, items$hostname, ignore.case = TRUE) | grepl(kw, items$ip_address %||% "", ignore.case = TRUE), ]
    }
    sf <- input$asset_status_filter
    if (length(sf) > 0 && sf != "全部") items <- items[items$status == sf, ]
    
    display <- data.frame(
      资产编号 = items$asset_no,
      主机名 = items$hostname,
      IP地址 = items$ip_address %||% "",
      操作系统 = items$os %||% "",
      CPU = items$cpu %||% "",
      内存 = items$ram %||% "",
      磁盘 = items$disk %||% "",
      状态 = sapply(items$status, function(s) {
        if (s == "active") '<span class="badge-active">使用中</span>'
        else if (s == "maintenance") '<span class="badge-maintenance">维护中</span>'
        else '<span class="badge-retired">已退役</span>'
      }),
      位置 = items$location %||% "",
      操作 = sprintf('<button class="btn btn-xs btn-info asset-edit-btn" data-id="%d">编辑</button> <button class="btn btn-xs btn-danger asset-del-btn" data-id="%d">删除</button>', items$id, items$id),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    
    datatable(display, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(pageLength = 20, autoWidth = TRUE, dom = "ltip",
        columnDefs = list(list(targets = 9, orderable = FALSE))))
  })
  outputOptions(output, "asset_table", suspendWhenHidden = FALSE)
  
  ##################
  # 刷新
  ##################
  observeEvent(input$asset_refresh, {
    asset_trigger(asset_trigger() + 1)
  })
  
  ##################
  # 添加弹窗
  ##################
  observeEvent(input$asset_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "添加资产",
      size = "m",
      easyClose = TRUE,
      textInput("asset_add_hostname", "主机名 *", placeholder = "必填"),
      textInput("asset_add_ip", "IP 地址"),
      fluidRow(
        column(6, textInput("asset_add_os", "操作系统")),
        column(6, selectInput("asset_add_status", "状态", choices = c("active"="使用中","maintenance"="维护中","retired"="已退役"), selected = "active"))
      ),
      fluidRow(
        column(4, textInput("asset_add_cpu", "CPU")),
        column(4, textInput("asset_add_ram", "内存")),
        column(4, textInput("asset_add_disk", "磁盘"))
      ),
      fluidRow(
        column(6, textInput("asset_add_manufacturer", "厂商")),
        column(6, textInput("asset_add_model", "型号"))
      ),
      textInput("asset_add_sn", "序列号"),
      fluidRow(
        column(6, textInput("asset_add_location", "位置")),
        column(6, textInput("asset_add_dept", "部门"))
      ),
      textAreaInput("asset_add_notes", "备注", rows = 2),
      footer = tagList(
        modalButton("取消"),
        actionButton("asset_add_confirm", "确认添加", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$asset_add_confirm, {
    req(rv$logged_in, input$asset_add_hostname)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- asset_add(
      hostname = input$asset_add_hostname, ip_address = input$asset_add_ip,
      os = input$asset_add_os, cpu = input$asset_add_cpu, ram = input$asset_add_ram,
      disk = input$asset_add_disk, manufacturer = input$asset_add_manufacturer,
      model = input$asset_add_model, serial_number = input$asset_add_sn,
      location = input$asset_add_location, department = input$asset_add_dept,
      notes = input$asset_add_notes, created_by = uid
    )
    if (result$success) removeModal()
    asset_trigger(asset_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  
  ##################
  # 编辑弹窗（表格按钮 JS 触发）
  ##################
  observeEvent(input$asset_edit_click, {
    req(rv$logged_in)
    item <- asset_get_by_id(as.integer(input$asset_edit_click))
    if (is.null(item) || nrow(item) == 0) return()
    rv$asset_edit_id <- item$id[1]
    showModal(modalDialog(
      title = paste("编辑资产 -", item$asset_no[1]),
      size = "m", easyClose = TRUE,
      textInput("asset_edit_hostname", "主机名 *", value = item$hostname[1]),
      textInput("asset_edit_ip", "IP 地址", value = item$ip_address[1] %||% ""),
      fluidRow(
        column(6, textInput("asset_edit_os", "操作系统", value = item$os[1] %||% "")),
        column(6, selectInput("asset_edit_status", "状态",
          choices = c("active"="使用中","maintenance"="维护中","retired"="已退役"),
          selected = item$status[1] %||% "active"))
      ),
      fluidRow(
        column(4, textInput("asset_edit_cpu", "CPU", value = item$cpu[1] %||% "")),
        column(4, textInput("asset_edit_ram", "内存", value = item$ram[1] %||% "")),
        column(4, textInput("asset_edit_disk", "磁盘", value = item$disk[1] %||% ""))
      ),
      fluidRow(
        column(6, textInput("asset_edit_manufacturer", "厂商", value = item$manufacturer[1] %||% "")),
        column(6, textInput("asset_edit_model", "型号", value = item$model[1] %||% ""))
      ),
      textInput("asset_edit_sn", "序列号", value = item$serial_number[1] %||% ""),
      fluidRow(
        column(6, textInput("asset_edit_location", "位置", value = item$location[1] %||% "")),
        column(6, textInput("asset_edit_dept", "部门", value = item$department[1] %||% ""))
      ),
      textAreaInput("asset_edit_notes", "备注", rows = 2, value = item$notes[1] %||% ""),
      footer = tagList(modalButton("取消"), actionButton("asset_edit_confirm", "保存", class = "btn-primary"))
    ))
  })
  
  observeEvent(input$asset_edit_confirm, {
    req(rv$logged_in, rv$asset_edit_id, input$asset_edit_hostname)
    result <- asset_update(rv$asset_edit_id,
      hostname = input$asset_edit_hostname, ip_address = input$asset_edit_ip,
      os = input$asset_edit_os, cpu = input$asset_edit_cpu, ram = input$asset_edit_ram,
      disk = input$asset_edit_disk, manufacturer = input$asset_edit_manufacturer,
      model = input$asset_edit_model, serial_number = input$asset_edit_sn,
      location = input$asset_edit_location, department = input$asset_edit_dept,
      status = input$asset_edit_status, notes = input$asset_edit_notes
    )
    if (result$success) removeModal()
    asset_trigger(asset_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  
  ##################
  # 删除（Modal 确认 + 显示资产详情）
  ##################
  observeEvent(input$asset_del_click, {
    req(rv$logged_in)
    aid <- as.integer(input$asset_del_click)
    a <- asset_get_by_id(aid)
    if (is.null(a) || nrow(a) == 0) return()
    showModal(modalDialog(
      title = "确认删除资产",
      tags$div(style = "font-size:13px;",
        tags$p(tags$b("即将删除以下资产，操作不可恢复：")),
        tags$table(class = "table table-bordered table-sm", style = "font-size:12px;",
          tags$thead(tags$tr(tags$th("属性"), tags$th("值"))),
          tags$tbody(
            tags$tr(tags$td("资产编号"), tags$td(tags$b(a$asset_no[1] %||% "—"))),
            tags$tr(tags$td("主机名"), tags$td(a$hostname[1] %||% "—")),
            tags$tr(tags$td("IP 地址"), tags$td(a$ip_address[1] %||% "—")),
            tags$tr(tags$td("操作系统"), tags$td(a$os[1] %||% "—")),
            tags$tr(tags$td("位置"), tags$td(a$location[1] %||% "—")),
            tags$tr(tags$td("部门"), tags$td(a$department[1] %||% "—")),
            tags$tr(tags$td("状态"), tags$td(a$status[1] %||% "—"))
          )
        )
      ),
      footer = tagList(modalButton("取消"),
        actionButton("asset_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$asset_del_confirm, {
    req(rv$logged_in)
    aid <- as.integer(input$asset_del_click)
    result <- asset_delete(aid)
    removeModal()
    asset_trigger(asset_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
}
