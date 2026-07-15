# 集成模块 - 服务端

integration_server <- function(input, output, session, rv) {

  integ_refresh <- reactiveVal(0)
  integ_selected_id <- reactiveVal(NULL)

  output$integ_selected <- reactive({ !is.null(integ_selected_id()) })
  outputOptions(output, "integ_selected", suspendWhenHidden=FALSE)

  # 配置列表
  output$integ_config_list <- renderUI({
    integ_refresh()
    items <- integration_get_all()
    if (nrow(items) == 0) return(tags$p(style="color:#999;", "暂无配置，点下方新增"))
    tagList(lapply(1:nrow(items), function(i) {
      r <- items[i,]
      is_active <- !is.null(integ_selected_id()) && integ_selected_id() == r$id
      desc_short <- r$description[1] %||% ""
      if (nchar(desc_short) > 60) desc_short <- paste0(substr(desc_short,1,60),"...")
      tags$div(class=paste("int-card", if(is_active) "active" else ""),
        onclick=sprintf("Shiny.setInputValue('integ_select',%d,{priority:'event'})", r$id),
        tags$div(class="int-name", r$name),
        tags$div(class="int-desc", if(desc_short!="") desc_short else paste(r$method, substring(r$base_url,1,40))))
    }))
  })

  observeEvent(input$integ_select, {
    integ_selected_id(as.integer(input$integ_select))
    integ <- integration_get_by_id(as.integer(input$integ_select))
    output$integ_method <- renderText(integ$method[1])
    output$integ_url <- renderText(integ$base_url[1])
  })

  # 新增配置弹窗
  observeEvent(input$integ_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(title="新增集成配置", size="m",
      textInput("integ_new_name","名称*"),
      textInput("integ_new_url","URL*", placeholder="https://api.example.com/v1/..."),
      selectInput("integ_new_method","方法", choices=c("POST","GET")),
      textInput("integ_new_auth_h","认证头", placeholder="如: Authorization"),
      textInput("integ_new_auth_v","认证值", placeholder="如: Bearer xxx"),
      textAreaInput("integ_new_desc","描述", rows=2),
      footer=tagList(modalButton("取消"), actionButton("integ_new_save","保存",class="btn-primary")),
      easyClose=TRUE
    ))
  })

  observeEvent(input$integ_new_save, {
    req(rv$logged_in, input$integ_new_name, input$integ_new_url)
    result <- integration_add(input$integ_new_name, input$integ_new_url,
      input$integ_new_auth_h, input$integ_new_auth_v, input$integ_new_method, input$integ_new_desc)
    removeModal(); integ_refresh(integ_refresh()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 编辑
  observeEvent(input$integ_edit_btn, {
    req(rv$logged_in, integ_selected_id())
    integ <- integration_get_by_id(integ_selected_id())
    if (is.null(integ)) return()
    showModal(modalDialog(title="编辑集成配置", size="m",
      textInput("integ_edit_name","名称", value=integ$name[1]),
      textInput("integ_edit_url","URL", value=integ$base_url[1]),
      selectInput("integ_edit_method","方法", choices=c("POST","GET"), selected=integ$method[1]),
      textInput("integ_edit_auth_h","认证头", value=integ$auth_header[1] %||% ""),
      textInput("integ_edit_auth_v","认证值", value=integ$auth_value[1] %||% ""),
      textAreaInput("integ_edit_desc","描述", value=integ$description[1] %||% "", rows=2),
      footer=tagList(modalButton("取消"), actionButton("integ_edit_save","保存",class="btn-primary")),
      easyClose=TRUE
    ))
  })

  observeEvent(input$integ_edit_save, {
    req(rv$logged_in, integ_selected_id())
    result <- integration_update(integ_selected_id(),
      input$integ_edit_name, input$integ_edit_url, input$integ_edit_auth_h,
      input$integ_edit_auth_v, input$integ_edit_method, input$integ_edit_desc)
    removeModal(); integ_refresh(integ_refresh()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 删除（Modal 确认 + 显示详情）
  observeEvent(input$integ_del_btn, {
    req(rv$logged_in, integ_selected_id())
    intg <- integration_get_by_id(integ_selected_id())
    if (is.null(intg) || nrow(intg) == 0) return()
    showModal(modalDialog(
      title = "确认删除集成",
      tags$div(style = "font-size:13px;",
        tags$p(tags$b("即将删除以下集成配置，操作不可恢复：")),
        tags$table(class = "table table-bordered table-sm", style = "font-size:12px;",
          tags$thead(tags$tr(tags$th("属性"), tags$th("值"))),
          tags$tbody(
            tags$tr(tags$td("名称"), tags$td(tags$b(intg$name[1] %||% "—"))),
            tags$tr(tags$td("URL"), tags$td(intg$base_url[1] %||% "—")),
            tags$tr(tags$td("方法"), tags$td(intg$method[1] %||% "—")),
            tags$tr(tags$td("描述"), tags$td(intg$description[1] %||% "—"))
          )
        )
      ),
      footer = tagList(modalButton("取消"),
        actionButton("integ_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$integ_del_confirm, {
    req(integ_selected_id())
    result <- integration_delete(integ_selected_id())
    removeModal()
    integ_selected_id(NULL)
    integ_refresh(integ_refresh()+1)
    showNotification(result$message, type="warning")
  })

  # 执行
  observeEvent(input$integ_exec_btn, {
    req(rv$logged_in, integ_selected_id(), input$integ_json_input)
    json <- input$integ_json_input
    # 验证 JSON 格式
    if (!jsonlite::validate(json)) {
      output$integ_resp_status <- renderText("JSON 格式错误")
      output$integ_resp_body <- renderText("请检查 JSON 语法")
      output$integ_resp_time <- renderText("—")
      return()
    }
    result <- integration_execute(integ_selected_id(), json)
    if (result$success) {
      output$integ_resp_status <- renderText({
        HTML(sprintf('<span class="int-status-ok">%d OK</span>', result$status_code))
      })
      output$integ_resp_time <- renderText(sprintf("%d ms", result$elapsed_ms))
      # 尝试格式化 JSON
      body <- result$body
      tryCatch({
        parsed <- jsonlite::fromJSON(body, simplifyVector=FALSE)
        body <- jsonlite::toJSON(parsed, pretty=TRUE, auto_unbox=TRUE)
      }, error=function(e) NULL)
      output$integ_resp_body <- renderText(body)
    } else {
      output$integ_resp_status <- renderText({
        HTML(sprintf('<span class="int-status-err">错误</span>'))
      })
      output$integ_resp_time <- renderText("—")
      output$integ_resp_body <- renderText(result$message)
    }
  })

  ##################
  # 大屏数据展示 v2：存储历史 + 增量 + K线图
  ##################

  # 大屏数据获取函数
  bigscreen_fetch_data <- function() {
    if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
    tryCatch({
      resp <- httr::GET("https://lvcchong.com/factoryBi/charge/0/realTimeData",
        httr::add_headers("Accept" = "application/json"),
        httr::timeout(10))
      if (httr::status_code(resp) != 200) return(NULL)
      body <- httr::content(resp, "text", encoding = "UTF-8")
      jsonlite::fromJSON(body, simplifyVector = FALSE)
    }, error = function(e) NULL)
  }

  # 存入本地数据库
  bigscreen_save_snapshot <- function(d) {
    con <- db_connect()
    tryCatch({
      dbExecute(con, sprintf(
        "INSERT INTO bigscreen_snapshots (total_device,total_port,total_user,total_power_consumption,total_carbon_emission_reduction,total_oil_saving,today_order_number,today_charge_order_number,today_pay_order_number,today_carbon_emission_reduction,today_turnover,snapshot_time) VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s',datetime('now','localtime'))",
        d$totalDevice %||% 0, d$totalPort %||% 0, d$totalUser %||% 0,
        d$totalPowerConsumption %||% 0, d$totalCarbonEmissionReduction %||% 0, d$totalOilSaving %||% 0,
        d$todayOrderNumber %||% 0, d$todayChargeOrderNumber %||% 0, d$todayPayOrderNumber %||% 0,
        d$todayCarbonEmissionReduction %||% 0, d$todayTurnover %||% 0))
    }, error = function(e) NULL,
    finally = { db_disconnect(con) })
  }

  bigscreen_data <- reactiveVal(NULL)
  bigscreen_time <- reactiveVal("")
  bigscreen_history <- reactiveVal(data.frame())

  # 字段名映射（snapshot字段 → 卡片key）
  BS_FIELDS <- list(
    totalDevice = "设备总数", totalPort = "端口总数", totalUser = "用户总数",
    totalPowerConsumption = "总耗电量", totalCarbonEmissionReduction = "累计碳减排",
    totalOilSaving = "累计节油量", todayOrderNumber = "今日订单",
    todayChargeOrderNumber = "今日充电订单", todayPayOrderNumber = "今日支付订单",
    todayCarbonEmissionReduction = "今日碳减排", todayTurnover = "今日营业额"
  )

  # 格式化大数字
  .fmt <- function(x) {
    if (is.null(x) || length(x) == 0) return("—")
    x <- as.numeric(x)
    if (length(x) == 0 || is.na(x)) return("—")
    if (x >= 1e8) return(sprintf("%.2f亿", x / 1e8))
    if (x >= 1e4) return(sprintf("%.2f万", x / 1e4))
    format(round(x), big.mark = ",")
  }

  # 计算增量（当前值 vs 上一个快照值的差值）
  .delta <- function(field, data_list, snapshots_df) {
    cur <- as.numeric(data_list[[field]] %||% 0)
    if (nrow(snapshots_df) < 2) return(NA_real_)
    prev <- as.numeric(snapshots_df[2, field])
    if (is.na(prev)) return(NA_real_)
    cur - prev
  }

  # 从历史快照计算多时间维度增量
  .deltas <- function(field, snapshots_df) {
    if (nrow(snapshots_df) == 0) return(list(h=NA_real_,daily=NA_real_,weekly=NA_real_,monthly=NA_real_,yearly=NA_real_))
    cur <- as.numeric(snapshots_df[1, field])
    if (length(cur) == 0 || is.na(cur)) return(list(h=NA_real_,daily=NA_real_,weekly=NA_real_,monthly=NA_real_,yearly=NA_real_))
    now <- Sys.time()
    times <- as.POSIXct(snapshots_df$snapshot_time)
    vals <- as.numeric(snapshots_df[[field]])
    .prev_at <- function(seconds) {
      target <- now - seconds
      idx <- which(times <= target)
      if (length(idx) == 0) return(NA_real_)
      vals[idx[1]]
    }
    list(
      h       = .prev_at(3600),
      daily   = .prev_at(86400),
      weekly  = .prev_at(604800),
      monthly = .prev_at(2592000),
      yearly  = .prev_at(31536000)
    )
  }

  # 增量格式化
  .delta_fmt <- function(d) {
    if (length(d) == 0 || is.na(d)) return("")
    if (d > 0) return(sprintf('<span style="color:#4caf50;font-size:10px;">+%s</span>', .fmt(d)))
    if (d < 0) return(sprintf('<span style="color:#e53e3e;font-size:10px;">%s</span>', .fmt(d)))
    return("")
  }

  # 加载历史数据
  load_history <- function() {
    con <- db_connect()
    tryCatch({
      dbGetQuery(con, "SELECT * FROM bigscreen_snapshots ORDER BY snapshot_time DESC")
    }, error = function(e) data.frame(),
    finally = { db_disconnect(con) })
  }

  # 初始化加载
  observe({
    req(rv$logged_in)
    isolate({
      data <- bigscreen_fetch_data()
      if (!is.null(data) && isTRUE(data$success)) {
        bigscreen_data(data$data)
        bigscreen_time(format(Sys.time(), "%H:%M:%S"))
        bigscreen_save_snapshot(data$data)
        bigscreen_history(load_history())
      }
    })
  })

  # 刷新按钮
  observeEvent(input$integ_bigscreen_refresh, {
    req(rv$logged_in)
    data <- bigscreen_fetch_data()
    if (!is.null(data) && isTRUE(data$success)) {
      bigscreen_data(data$data)
      bigscreen_time(format(Sys.time(), "%H:%M:%S"))
      bigscreen_save_snapshot(data$data)
      bigscreen_history(load_history())
      showNotification("数据已刷新", type = "message", duration = 1.5)
    } else {
      showNotification("获取数据失败", type = "error", duration = 3)
    }
  })

  output$integ_bigscreen_time <- renderText({
    t <- bigscreen_time()
    if (t == "") "" else paste0(" 更新于 ", t)
  })

  # 渲染大屏卡片（含增量）
  output$integ_bigscreen_cards <- renderUI({
    data <- bigscreen_data()
    if (is.null(data)) {
      return(tags$div(style = "text-align:center; padding:60px; color:#999;",
        icon("spinner", class = "fa-spin fa-2x"), tags$br(), tags$br(),
        "正在获取数据..."))
    }
    snap <- bigscreen_history()
    card_defs <- list(
      list(field="totalDevice",                  label="设备总数",       unit="台",   cls="c1"),
      list(field="totalPort",                    label="端口总数",       unit="个",   cls="c2"),
      list(field="totalUser",                    label="用户总数",       unit="人",   cls="c3"),
      list(field="totalPowerConsumption",        label="总耗电量",       unit="kWh",  cls="c4"),
      list(field="totalCarbonEmissionReduction", label="累计碳减排",     unit="吨",   cls="c5"),
      list(field="totalOilSaving",               label="累计节油量",     unit="升",   cls="c6"),
      list(field="todayOrderNumber",             label="今日订单",       unit="单",   cls="c7"),
      list(field="todayChargeOrderNumber",       label="今日充电订单",   unit="单",   cls="c8"),
      list(field="todayPayOrderNumber",          label="今日支付订单",   unit="单",   cls="c9"),
      list(field="todayCarbonEmissionReduction", label="今日碳减排",     unit="吨",   cls="c10"),
      list(field="todayTurnover",                label="今日营业额",     unit="元",   cls="c11")
    )
    tagList(
      tags$div(class = "bigscreen-grid",
        lapply(card_defs, function(card) {
          val <- .fmt(data[[card$field]])
          dl <- .deltas(card$field, snap)
          tags$div(class = paste("bigscreen-card", card$cls),
            tags$div(class = "bs-value", HTML(val)),
            tags$div(class = "bs-label", card$label),
            tags$div(class = "bs-sub", paste("单位:", card$unit)),
            tags$div(style = "margin-top:4px; line-height:1.3; font-size:10px;",
              if (!is.na(dl$h))     tags$div(HTML(paste0("时增: ", .delta_fmt(as.numeric(data[[card$field]] %||% 0) - dl$h)))),
              if (!is.na(dl$daily)) tags$div(HTML(paste0("日增: ", .delta_fmt(as.numeric(data[[card$field]] %||% 0) - dl$daily)))),
              if (!is.na(dl$weekly))tags$div(HTML(paste0("周增: ", .delta_fmt(as.numeric(data[[card$field]] %||% 0) - dl$weekly)))),
              if (!is.na(dl$monthly))tags$div(HTML(paste0("月增: ", .delta_fmt(as.numeric(data[[card$field]] %||% 0) - dl$monthly)))),
              if (!is.na(dl$yearly))tags$div(HTML(paste0("年增: ", .delta_fmt(as.numeric(data[[card$field]] %||% 0) - dl$yearly))))
            )
          )
        })
      )
    )
  })

  # K线图：展示历史趋势
  output$integ_bigscreen_chart <- renderPlotly({
    snap <- bigscreen_history()
    if (nrow(snap) < 2) {
      return(plotly::plot_ly() %>%
        plotly::layout(title = "数据不足（需要至少2次刷新）"))
    }
    df <- snap[order(snap$snapshot_time), ]
    df$snapshot_time <- as.POSIXct(df$snapshot_time)
    # 安全取值：列可能不存在或全为空字符串
    raw <- df[["todayTurnover"]]
    if (is.null(raw)) raw <- rep("0", nrow(df))
    y_val <- suppressWarnings(as.numeric(raw))
    y_val[is.na(y_val) | length(y_val) == 0] <- 0
    if (all(y_val == 0)) {
      return(plotly::plot_ly() %>%
        plotly::layout(title = "暂无有效数据（请多次刷新积累历史）"))
    }
    df$y_val <- y_val
    plotly::plot_ly(df, x = ~snapshot_time, y = ~y_val,
      type = "scatter", mode = "lines+markers",
      line = list(color = "#764ba2", width = 2),
      marker = list(color = "#667eea", size = 4),
      hoverinfo = "text",
      text = ~paste("时间:", snapshot_time, "<br>营业额:", .fmt(y_val), "元")) %>%
      plotly::layout(
        title = "今日营业额 历史趋势",
        xaxis = list(title = ""),
        yaxis = list(title = "营业额 (元)", tickformat = ",d"),
        margin = list(t = 40, b = 40, l = 80, r = 20),
        paper_bgcolor = "#fafafa"
      )
  })
}
