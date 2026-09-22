# 行业模块 Server（框架）
industry_server <- function(input, output, session, rv) {
  ind_trigger <- reactiveVal(0)

  # 情报列表
  output$ind_list <- renderUI({
    req(rv$logged_in); ind_trigger()
    kw <- trimws(input$ind_search %||% "")
    cat <- trimws(input$ind_filter_cat %||% "")
    items <- industry_get_all(category = cat, keyword = kw)
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无行业情报，点击「新增情报」开始（框架）"))
    }
    parts <- c()
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      cat_html <- if (!is.na(r$category) && nchar(r$category) > 0)
        sprintf('<span style="display:inline-block;padding:1px 8px;border-radius:10px;font-size:11px;background:#e8eaf6;color:#3949ab;margin-left:8px;">%s</span>', r$category) else ""
      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:10px;">
          <div style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</div>
          <b style="font-size:15px;color:#1a237e;">%s</b>%s
          <div style="font-size:12px;color:#555;margin-top:6px;">%s</div>
          <div style="font-size:11px;color:#999;margin-top:6px;">来源：%s · %s</div>
        </div>',
        r$intel_no, r$title, cat_html, substr(r$content %||% "", 1, 200),
        r$source %||% "", substr(r$created_at %||% "", 1, 16)))
    }
    HTML(paste(parts, collapse = ""))
  })

  # 新增情报弹窗
  observeEvent(input$ind_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新增行业情报",
      textInput("ind_edit_title", "标题", width = "100%"),
      selectInput("ind_edit_cat", "分类", choices = industry_get_categories(), width = "100%"),
      textInput("ind_edit_source", "来源", width = "100%", placeholder = "如：公司官网、行业报告..."),
      textInput("ind_edit_url", "原文链接", width = "100%", placeholder = "https://...（可选）"),
      textAreaInput("ind_edit_content", "内容", rows = 6, width = "100%"),
      footer = tagList(
        actionButton("ind_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$ind_save_btn, {
    req(rv$logged_in, input$ind_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- industry_add(input$ind_edit_title, input$ind_edit_content,
      input$ind_edit_cat, input$ind_edit_source, input$ind_edit_url, uid)
    if (result$success) {
      removeModal()
      ind_trigger(ind_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  outputOptions(output, "ind_list", suspendWhenHidden = FALSE)
}
