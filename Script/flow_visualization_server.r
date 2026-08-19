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

  # 历史列表
  output$fvz_history <- renderUI({
    req(rv$logged_in)
    fvz_hist_trigger()
    hist <- flow_viz_get_history()
    if (nrow(hist) == 0) {
      return(tags$p(style = "color:#999; text-align:center; padding:20px 0;", "暂无转换记录"))
    }
    do.call(tagList, lapply(seq_len(nrow(hist)), function(i) {
      r <- hist[i, ]
      out_name <- r$out_name
      href <- paste0("www/flow_viz/", out_name)
      is_log <- !is.na(r$kind) && r$kind == "log"
      # 类型标签
      kind_badge <- if (is_log) {
        tags$span(style = "font-size:11px; background:#8e44ad; color:#fff; padding:1px 8px; border-radius:10px;",
          "流程日志")
      } else {
        tags$span(style = "font-size:11px; background:#337ab7; color:#fff; padding:1px 8px; border-radius:10px;",
          "数据看板")
      }
      # 统计信息（根据类型区分字段语义）
      stat_text <- if (is_log) {
        sprintf("节点%d 完成%d 未完成%d", r$total_flows, r$completed_flows, r$active_flows)
      } else {
        sprintf("总%d 完成%d 进行中%d 完成率%s%%",
          r$total_flows, r$completed_flows, r$active_flows, r$completion_rate)
      }
      tags$div(class = "fvz-hist-row",
        kind_badge,
        tags$span(style = "font-weight:600; color:#333;", r$src_name),
        tags$span(style = "font-size:11px; color:#999;", stat_text),
        tags$span(style = "font-size:11px; color:#999;", r$created_at),
        tags$a(href = href, target = "_blank", class = "btn btn-xs btn-primary",
          icon("external-link-alt"), " 打开"),
        tags$button(type = "button", class = "btn btn-xs btn-warning fvz-export-btn",
          `data-id` = r$id, icon("download"), " 重新导出")
      )
    }))
  })

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
