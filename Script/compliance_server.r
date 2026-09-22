# 合规模块 Server
compliance_server <- function(input, output, session, rv) {
  cmp_trigger <- reactiveVal(0)

  # 规则列表（按分类分组展示）
  output$cmp_list <- renderUI({
    req(rv$logged_in); cmp_trigger()
    cat <- trimws(input$cmp_filter_cat %||% "")
    kw <- trimws(input$cmp_search %||% "")
    items <- compliance_get_all(category = cat)
    if (!is.null(kw) && nzchar(kw)) {
      items <- items[grepl(kw, items$title, ignore.case = TRUE) |
                       grepl(kw, items$rule_content %||% "", ignore.case = TRUE) |
                       grepl(kw, items$law_basis %||% "", ignore.case = TRUE), , drop = FALSE]
    }
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无合规规则，点击「新增合规规则」添加"))
    }

    # 按分类分组
    groups <- split(seq_len(nrow(items)), items$category)
    parts <- c()
    for (gname in names(groups)) {
      idx <- groups[[gname]]
      parts <- c(parts, sprintf(
        '<div style="background:#e3f2fd;border-left:4px solid #1565c0;padding:8px 12px;margin:16px 0 8px;border-radius:0 4px 4px 0;">
          <b style="color:#1565c0;font-size:14px;">%s</b>
          <span style="color:#666;font-size:12px;margin-left:8px;">（%d 条）</span>
        </div>', gname, length(idx)))
      for (i in idx) {
        r <- items[i, ]
        law_html <- if (!is.na(r$law_basis) && nzchar(r$law_basis))
          sprintf('<div style="font-size:12px;color:#7b1fa2;margin-top:4px;"><b>法规依据：</b>%s</div>', r$law_basis) else ""
        parts <- c(parts, sprintf(
          '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:8px;">
            <div style="display:flex;align-items:center;justify-content:space-between;">
              <div>
                <span style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</span>
                <b style="font-size:15px;color:#1565c0;margin-left:8px;">%s</b>
              </div>
            </div>
            <div style="font-size:13px;color:#333;margin-top:6px;line-height:1.6;">%s</div>
            %s
          </div>',
          r$comp_no, r$title, r$rule_content %||% "", law_html))
      }
    }
    HTML(paste(parts, collapse = ""))
  })

  # 新增规则弹窗
  observeEvent(input$cmp_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新增合规规则",
      textInput("cmp_edit_title", "规则名称", width = "100%"),
      selectInput("cmp_edit_cat", "分类", choices = compliance_get_categories(), width = "100%"),
      textInput("cmp_edit_law", "法规依据", width = "100%", placeholder = "如：《网络安全法》第二十一条"),
      textAreaInput("cmp_edit_rule", "要求要点", rows = 4, width = "100%"),
      footer = tagList(
        actionButton("cmp_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$cmp_save_btn, {
    req(rv$logged_in, input$cmp_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- compliance_add(input$cmp_edit_title, input$cmp_edit_cat,
      input$cmp_edit_rule, law_basis = input$cmp_edit_law, created_by = uid)
    if (result$success) {
      removeModal()
      cmp_trigger(cmp_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  outputOptions(output, "cmp_list", suspendWhenHidden = FALSE)
}
