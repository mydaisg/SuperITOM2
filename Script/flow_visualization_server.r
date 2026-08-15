# 流程数据可视化模块 — 服务端

flow_viz_server <- function(input, output, session, rv) {

  # 历史刷新触发器
  fvz_hist_trigger <- reactiveVal(0)

  # 生成看板
  observeEvent(input$fvz_generate, {
    req(rv$logged_in)
    f <- input$fvz_file
    if (is.null(f)) {
      showNotification("请先选择要上传的 Excel 文件", type = "warning")
      return()
    }

    # 操作人
    operator <- "系统"
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      op_name <- rv$current_user$display_name[1]
      if (is.null(op_name) || nchar(trimws(op_name)) == 0) op_name <- rv$current_user$username[1]
      operator <- op_name
    }

    # 显示处理中
    showNotification("正在生成看板...", type = "message", duration = NULL, id = "fvz_working")

    res <- flow_viz_generate(f$datapath, f$name, operator)
    removeNotification(id = "fvz_working")

    if (!isTRUE(res$success)) {
      showNotification(res$message, type = "error", duration = 8)
      return()
    }

    # 保存历史（含 HTML 内容备份）
    record_no <- flow_viz_generate_no()
    flow_viz_add_record(record_no, f$name, res$out_name, res$stats, operator,
                        html_content = res$html_content)

    # 刷新历史
    fvz_hist_trigger(fvz_hist_trigger() + 1)

    s <- res$stats
    showNotification(
      sprintf("看板生成成功：%d 条流程，完成率 %s%%", s$total, s$completion_rate),
      type = "message", duration = 5)

    # 结果展示：路径 + 点击链接
    output$fvz_result <- renderUI({
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
      tags$div(class = "fvz-hist-row",
        tags$span(style = "font-weight:600; color:#333;", r$src_name),
        tags$span(style = "font-size:11px; color:#999;", sprintf("总%d 完成%d 进行中%d 完成率%s%%",
          r$total_flows, r$completed_flows, r$active_flows, r$completion_rate)),
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
