# 流程数据可视化模块 — 服务端

flow_viz_server <- function(input, output, session, rv) {

  # 历史刷新触发器
  fvz_hist_trigger <- reactiveVal(0)

  # 生成结果（reactiveVal 存储，避免在 observeEvent 内定义 output）
  fvz_result <- reactiveVal(NULL)       # 流程数据看板结果
  fvz_log_result <- reactiveVal(NULL)   # 流程日志效率图结果

  # 操作人
  fvz_operator <- function() {
    op <- "系统"
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      op_name <- rv$current_user$display_name[1]
      if (is.null(op_name) || nchar(trimws(op_name)) == 0) op_name <- rv$current_user$username[1]
      op <- op_name
    }
    op
  }

  # 生成流程数据看板
  observeEvent(input$fvz_generate, {
    req(rv$logged_in)
    f <- input$fvz_file
    if (is.null(f)) {
      showNotification("请先选择要上传的 Excel 文件", type = "warning")
      return()
    }

    operator <- fvz_operator()
    showNotification("正在生成看板...", type = "message", duration = NULL, id = "fvz_working")
    res <- flow_viz_generate(f$datapath, f$name, operator)
    removeNotification(id = "fvz_working")

    if (!isTRUE(res$success)) {
      showNotification(res$message, type = "error", duration = 8)
      return()
    }

    # 保存历史（不存 html_content 大字段，文件已在磁盘 www/flow_viz/）
    record_no <- flow_viz_generate_no()
    flow_viz_add_record(record_no, f$name, res$out_name, res$stats, operator, html_content = NULL)

    # 更新结果 + 刷新历史
    fvz_result(res)
    fvz_hist_trigger(fvz_hist_trigger() + 1)

    s <- res$stats
    showNotification(
      sprintf("看板生成成功：%d 条流程，完成率 %s%%", s$total, s$completion_rate),
      type = "message", duration = 5)
  })

  # 生成流程日志效率图
  observeEvent(input$fvz_log_generate, {
    req(rv$logged_in)
    f <- input$fvz_log_file
    if (is.null(f)) {
      showNotification("请先选择要上传的流程日志 Excel 文件", type = "warning")
      return()
    }

    operator <- fvz_operator()
    showNotification("正在解析流程日志并生成效率图...", type = "message", duration = NULL, id = "fvz_log_working")
    res <- flow_log_generate(f$datapath, f$name, operator)
    removeNotification(id = "fvz_log_working")

    if (!isTRUE(res$success)) {
      showNotification(res$message, type = "error", duration = 8)
      return()
    }

    # 保存历史（不存 html_content）
    record_no <- flow_viz_generate_no()
    flow_log_add_record(record_no, f$name, res$out_name, res$stats, operator, html_content = NULL)

    # 更新结果 + 刷新历史
    fvz_log_result(res)
    fvz_hist_trigger(fvz_hist_trigger() + 1)

    s <- res$stats
    showNotification(
      sprintf("效率图生成成功：%d 个节点，未完成 %s 个", s$node_count, s$undone_nodes),
      type = "message", duration = 5)
  })

  # 流程数据看板结果展示（顶层定义）
  output$fvz_result <- renderUI({
    req(rv$logged_in)
    res <- fvz_result()
    req(res)
    s <- res$stats
    tagList(
      h5("生成结果", style = "margin-top:10px;"),
      div(class = "fvz-path-box",
        icon("folder-open"), " ", res$html_path),
      br(),
      tags$a(href = paste0("www/flow_viz/", res$out_name), target = "_blank",
             class = "fvz-link", icon("external-link-alt"), " 在浏览器新窗口打开看板"),
      br(), br(),
      tags$small(style = "color:#999;",
        sprintf("数据周期 %s ~ %s | 已完成 %d | 进行中 %d | 类型 %d | 发起人 %d",
                s$date_min, s$date_max, s$completed, s$active,
                s$type_count, s$initiator_count))
    )
  })

  # 流程日志效率图结果展示（顶层定义）
  output$fvz_log_result <- renderUI({
    req(rv$logged_in)
    res <- fvz_log_result()
    req(res)
    s <- res$stats
    tagList(
      h5("生成结果", style = "margin-top:10px;"),
      div(class = "fvz-path-box",
        icon("folder-open"), " ", res$html_path),
      br(),
      tags$a(href = paste0("www/flow_viz/", res$out_name), target = "_blank",
             class = "fvz-link", icon("external-link-alt"), " 在浏览器新窗口打开效率图"),
      br(), br(),
      tags$small(style = "color:#999;",
        sprintf("%s | 节点 %d | 已完成 %d | 未完成 %s | 总人次 %s",
                s$title, s$node_count, s$done_nodes, s$undone_nodes, s$total_person))
    )
  })

  # 历史数据（reactive 缓存，避免 renderUI 反复查库）
  fvz_hist_data <- reactive({
    req(rv$logged_in)
    fvz_hist_trigger()
    flow_viz_get_history()
  })

  # 历史列表（DT 服务端渲染，快 + 分页）
  output$fvz_history <- DT::renderDataTable({
    hist <- fvz_hist_data()
    if (is.null(hist) || nrow(hist) == 0) return(data.frame())
    # 构建展示列：类型、源文件、统计、时间、操作
    df <- data.frame(
      "类型" = ifelse(!is.na(hist$kind) & hist$kind == "log", "流程日志", "数据看板"),
      "源文件" = hist$src_name,
      "统计" = ifelse(!is.na(hist$kind) & hist$kind == "log",
        sprintf("节点%d 完成%d 未完成%d", hist$total_flows, hist$completed_flows, hist$active_flows),
        sprintf("总%d 完成%d 进行中%d 完成率%s%%", hist$total_flows, hist$completed_flows, hist$active_flows, hist$completion_rate)),
      "时间" = hist$created_at,
      "操作" = sprintf(
        '<a href="www/flow_viz/%s" target="_blank" class="btn btn-xs btn-primary">打开</a> <button type="button" class="btn btn-xs btn-warning fvz-export-btn" data-id="%d">重新导出</button>',
        hist$out_name, hist$id),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(pageLength = 10, autoWidth = FALSE,
                     columnDefs = list(list(orderable = FALSE, targets = 4)),
                     language = list(
                       search = "搜索:", lengthMenu = "显示 _MENU_ 条",
                       info = "第 _START_ - _END_ 条，共 _TOTAL_ 条",
                       paginate = list(previous = "上一页", `next` = "下一页"),
                       emptyTable = "暂无数据", zeroRecords = "无匹配记录")))
  })
  # 避免 tab 切换时（display:none 容器）挂起导致首次显示延迟
  outputOptions(output, "fvz_history", suspendWhenHidden = FALSE)

  # 手动刷新
  observeEvent(input$fvz_refresh, {
    req(rv$logged_in)
    fvz_hist_trigger(fvz_hist_trigger() + 1)
  })

  # 从 DB 重新导出 HTML 文件
  observeEvent(input$fvz_export, {
    req(rv$logged_in)
    rid <- as.integer(input$fvz_export$id)
    if (is.na(rid) || rid <= 0) return()
    res <- flow_viz_export_html(rid)
    if (isTRUE(res$success)) {
      showNotification(
        paste("已重新导出 HTML 文件：", basename(res$out_path)),
        type = "message", duration = 5)
    } else {
      showNotification("该记录无 HTML 备份，无法重新导出", type = "warning", duration = 5)
    }
  })
}
