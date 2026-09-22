# 治理模块 Server（框架）
governance_server <- function(input, output, session, rv) {
  gov_trigger <- reactiveVal(0)

  # 治理条目列表
  output$gov_list <- renderUI({
    req(rv$logged_in); gov_trigger()
    fw <- trimws(input$gov_filter_fw %||% "")
    kw <- trimws(input$gov_search %||% "")
    items <- governance_get_all(framework = fw)
    if (!is.null(kw) && nzchar(kw)) {
      items <- items[grepl(kw, items$title, ignore.case = TRUE), , drop = FALSE]
    }
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无治理条目，点击「新增治理条目」开始（框架）"))
    }
    parts <- c()
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      fw_html <- if (!is.na(r$framework) && nchar(r$framework) > 0)
        sprintf('<span style="display:inline-block;padding:1px 8px;border-radius:10px;font-size:11px;background:#e0f2f1;color:#00695c;margin-left:8px;">%s</span>', r$framework) else ""
      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:10px;">
          <div style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</div>
          <b style="font-size:15px;color:#00695c;">%s</b>%s
          <div style="font-size:12px;color:#555;margin-top:6px;">%s</div>
          <div style="font-size:11px;color:#999;margin-top:6px;">%s</div>
        </div>',
        r$gov_no, r$title, fw_html, substr(r$analysis %||% "", 1, 200), substr(r$created_at %||% "", 1, 16)))
    }
    HTML(paste(parts, collapse = ""))
  })

  # 新增治理条目弹窗
  observeEvent(input$gov_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新增治理条目",
      textInput("gov_edit_title", "标题", width = "100%"),
      selectInput("gov_edit_fw", "分析框架", choices = governance_get_frameworks(), width = "100%"),
      textAreaInput("gov_edit_analysis", "分析", rows = 5, width = "100%"),
      textAreaInput("gov_edit_action", "行动", rows = 3, width = "100%"),
      textAreaInput("gov_edit_review", "复盘", rows = 3, width = "100%"),
      footer = tagList(
        actionButton("gov_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$gov_save_btn, {
    req(rv$logged_in, input$gov_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- governance_add(input$gov_edit_title, input$gov_edit_fw,
      input$gov_edit_analysis, input$gov_edit_action, input$gov_edit_review, uid)
    if (result$success) {
      removeModal()
      gov_trigger(gov_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  outputOptions(output, "gov_list", suspendWhenHidden = FALSE)
}
