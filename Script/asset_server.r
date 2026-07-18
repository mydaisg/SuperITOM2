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

  ##################
  # 考勤设备模块
  ##################
  att_trigger <- reactiveVal(0)

  # 统计栏
  output$attendance_device_stats <- renderUI({
    att_trigger()
    stats <- attendance_device_get_stats()
    fluidRow(
      column(2, div(class = "asset-stat-box", style = "background:#e3f2fd;",
        div(class = "num", style = "color:#1565c0;", stats$total), div(class = "lbl", "设备点位"))),
      column(2, div(class = "asset-stat-box", style = "background:#e8f5e9;",
        div(class = "num", style = "color:#2e7d32;", stats$total_qty), div(class = "lbl", "设备总数"))),
      column(2, div(class = "asset-stat-box", style = "background:#fff3e0;",
        div(class = "num", style = "color:#e65100;", stats$face_count), div(class = "lbl", "人脸识别"))),
      column(2, div(class = "asset-stat-box", style = "background:#fce4ec;",
        div(class = "num", style = "color:#c62828;", stats$fingerprint_count), div(class = "lbl", "指纹机"))),
      column(2, div(class = "asset-stat-box", style = "background:#f3e5f5;",
        div(class = "num", style = "color:#6a1b9a;", stats$zk_count), div(class = "lbl", "中控"))),
      column(2, div(class = "asset-stat-box", style = "background:#e0f2f1;",
        div(class = "num", style = "color:#00695c;", stats$dd_count), div(class = "lbl", "钉钉")))
    )
  })

  # 更新筛选下拉
  observe({
    req(rv$logged_in)
    att_trigger()
    items <- attendance_device_get_all()
    if (nrow(items) > 0) {
      areas <- unique(c("全部", items$area[!is.na(items$area) & items$area != ""]))
      updateSelectInput(session, "attendance_filter_area", choices = areas, selected = "全部")
    }
  })

  # 设备表格
  output$attendance_device_table <- DT::renderDataTable({
    att_trigger()
    items <- attendance_device_get_all()
    if (nrow(items) == 0) return(data.frame(提示 = "暂无考勤设备数据"))
    # 筛选
    if (!is.null(input$attendance_filter_area) && input$attendance_filter_area != "全部") {
      items <- items[items$area == input$attendance_filter_area | (is.na(items$area) & input$attendance_filter_area == ""), ]
    }
    if (!is.null(input$attendance_filter_type) && input$attendance_filter_type != "全部") {
      items <- items[grepl(input$attendance_filter_type, items$device_type, fixed = TRUE), ]
    }
    if (!is.null(input$attendance_filter_brand) && input$attendance_filter_brand != "全部") {
      items <- items[items$brand == input$attendance_filter_brand, ]
    }
    if (nrow(items) == 0) return(data.frame(提示 = "无匹配数据"))
    # 汇总行
    total_qty <- sum(items$quantity %||% 0, na.rm = TRUE)
    summary_row <- data.frame(
      ID = NA, 区域 = "", 位置 = sprintf("【合计 %d 条记录】", nrow(items)),
      设备类型 = "", 品牌 = "", 数量 = total_qty,
      适用人员 = "", 特殊人员 = "", 备注 = "", 操作 = "",
      stringsAsFactors = FALSE, check.names = FALSE
    )
    disp <- rbind(data.frame(
      ID = items$id,
      区域 = items$area %||% "",
      位置 = items$location %||% "",
      设备类型 = items$device_type %||% "",
      品牌 = items$brand %||% "",
      数量 = items$quantity %||% 1,
      适用人员 = items$applicable_users %||% "",
      特殊人员 = items$special_users %||% "",
      备注 = items$remark %||% "",
      操作 = sprintf('<button class="btn btn-xs btn-warning att-edit-btn" data-id="%d">编辑</button> <button class="btn btn-xs btn-danger att-del-btn" data-id="%d">删除</button>', items$id, items$id),
      stringsAsFactors = FALSE, check.names = FALSE
    ), summary_row)
    DT::datatable(disp, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 50, dom = "ltip", scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))),
      class = "cell-border stripe compact") %>%
      DT::formatStyle("位置", target = "row",
        fontWeight = DT::styleEqual(sprintf("【合计 %d 条记录】", nrow(items)), "bold"),
        backgroundColor = DT::styleEqual(sprintf("【合计 %d 条记录】", nrow(items)), "#f5f5f5"))
  })

  # 添加弹窗
  observeEvent(input$attendance_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(title = "添加考勤设备", size = "m",
      textInput("att_new_area", "区域", placeholder = "如: 综合楼一楼"),
      textInput("att_new_location", "位置", placeholder = "如: 正门出入口"),
      selectInput("att_new_type", "设备类型", choices = c("人脸识别","指纹机")),
      selectInput("att_new_brand", "品牌", choices = c("中控","钉钉")),
      numericInput("att_new_qty", "数量", value = 1, min = 1),
      textAreaInput("att_new_users", "适用人员范围", rows = 2),
      textInput("att_new_special", "特殊人员范围"),
      textAreaInput("att_new_remark", "备注", rows = 2),
      footer = tagList(modalButton("取消"), actionButton("att_save_btn", "保存", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$att_save_btn, {
    req(rv$logged_in)
    result <- attendance_device_add(
      area = input$att_new_area, location = input$att_new_location,
      device_type = input$att_new_type, brand = input$att_new_brand,
      quantity = input$att_new_qty, applicable_users = input$att_new_users,
      special_users = input$att_new_special, remark = input$att_new_remark
    )
    if (result$success) removeModal()
    att_trigger(att_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # 编辑/删除 JS 事件
  observeEvent(input$att_edit_click, {
    req(rv$logged_in)
    dev <- attendance_device_get_by_id(as.integer(input$att_edit_click))
    if (is.null(dev)) return()
    rv$att_edit_id <- dev$id[1]
    showModal(modalDialog(title = "编辑考勤设备", size = "m",
      textInput("att_edit_area", "区域", value = dev$area[1] %||% ""),
      textInput("att_edit_location", "位置", value = dev$location[1] %||% ""),
      selectInput("att_edit_type", "设备类型", choices = c("人脸识别","指纹机"), selected = dev$device_type[1] %||% "人脸识别"),
      selectInput("att_edit_brand", "品牌", choices = c("中控","钉钉"), selected = dev$brand[1] %||% "中控"),
      numericInput("att_edit_qty", "数量", value = dev$quantity[1] %||% 1, min = 1),
      textAreaInput("att_edit_users", "适用人员范围", value = dev$applicable_users[1] %||% "", rows = 2),
      textInput("att_edit_special", "特殊人员范围", value = dev$special_users[1] %||% ""),
      textAreaInput("att_edit_remark", "备注", value = dev$remark[1] %||% "", rows = 2),
      footer = tagList(modalButton("取消"), actionButton("att_update_btn", "保存", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$att_update_btn, {
    req(rv$logged_in, rv$att_edit_id)
    result <- attendance_device_update(rv$att_edit_id,
      area = input$att_edit_area, location = input$att_edit_location,
      device_type = input$att_edit_type, brand = input$att_edit_brand,
      quantity = input$att_edit_qty, applicable_users = input$att_edit_users,
      special_users = input$att_edit_special, remark = input$att_edit_remark
    )
    if (result$success) removeModal()
    att_trigger(att_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  observeEvent(input$att_del_click, {
    req(rv$logged_in)
    did <- as.integer(input$att_del_click)
    dev <- attendance_device_get_by_id(did)
    if (is.null(dev)) return()
    showModal(modalDialog(
      title = "确认删除考勤设备",
      tags$p(sprintf("确定要删除「%s - %s」吗？", dev$area[1] %||% "", dev$location[1] %||% "")),
      footer = tagList(modalButton("取消"), actionButton("att_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$att_del_confirm, {
    req(rv$logged_in)
    did <- as.integer(input$att_del_click)
    result <- attendance_device_delete(did)
    removeModal()
    att_trigger(att_trigger() + 1)
    showNotification(result$message, type = "warning")
  })

  observeEvent(input$attendance_refresh, {
    att_trigger(att_trigger() + 1)
  })
}
