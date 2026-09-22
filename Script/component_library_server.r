# 组件库模块 Server（框架）
component_library_server <- function(input, output, session, rv) {
  cpt_trigger <- reactiveVal(0)

  # 组件列表
  output$cpt_list <- renderUI({
    req(rv$logged_in); cpt_trigger()
    cat <- trimws(input$cpt_filter_cat %||% "")
    kw <- trimws(input$cpt_search %||% "")
    items <- component_get_all(category = cat)
    if (!is.null(kw) && nzchar(kw)) {
      items <- items[grepl(kw, items$name, ignore.case = TRUE) |
                       grepl(kw, items$description %||% "", ignore.case = TRUE), , drop = FALSE]
    }
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无组件，点击「登记组件」开始（框架）"))
    }
    parts <- c()
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      cat_html <- if (!is.na(r$category) && nchar(r$category) > 0)
        sprintf('<span style="display:inline-block;padding:1px 8px;border-radius:10px;font-size:11px;background:#f3e5f5;color:#4a148c;margin-left:8px;">%s</span>', r$category) else ""
      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:10px;">
          <div style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</div>
          <b style="font-size:15px;color:#4a148c;">%s</b>%s
          <div style="font-size:12px;color:#555;margin-top:6px;">%s</div>
          <div style="font-size:11px;color:#999;margin-top:6px;">%s</div>
        </div>',
        r$comp_no, r$name, cat_html, substr(r$description %||% "", 1, 200), substr(r$created_at %||% "", 1, 16)))
    }
    HTML(paste(parts, collapse = ""))
  })

  # 登记组件弹窗
  observeEvent(input$cpt_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "登记组件",
      textInput("cpt_edit_name", "组件名称", width = "100%"),
      selectInput("cpt_edit_cat", "组件分类", choices = component_get_categories(), width = "100%"),
      textAreaInput("cpt_edit_desc", "描述", rows = 3, width = "100%"),
      textAreaInput("cpt_edit_usage", "调用方式", rows = 3, width = "100%",
        placeholder = "如：component_call('xxx', config)"),
      textAreaInput("cpt_edit_schema", "配置 Schema(JSON)", rows = 3, width = "100%",
        placeholder = "{ \"独立配置\": true }"),
      footer = tagList(
        actionButton("cpt_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$cpt_save_btn, {
    req(rv$logged_in, input$cpt_edit_name)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- component_add(input$cpt_edit_name, input$cpt_edit_cat,
      input$cpt_edit_desc, input$cpt_edit_usage, input$cpt_edit_schema, uid)
    if (result$success) {
      removeModal()
      cpt_trigger(cpt_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  outputOptions(output, "cpt_list", suspendWhenHidden = FALSE)
}
