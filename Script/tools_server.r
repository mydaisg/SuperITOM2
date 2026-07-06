# 工具模块 Server
tools_server <- function(input, output, session, rv) {

  # 文本格式化
  observeEvent(input$tool_format_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    sep <- input$tool_sep %||% ","
    quote_it <- isTRUE(input$tool_quote)
    lines <- strsplit(txt, "\\r?\\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    if (length(lines) == 0) { showNotification("无有效文本行", type = "warning"); return() }
    if (quote_it) lines <- paste0('"', gsub('"', '""', lines), '"')
    result <- paste(lines, collapse = sep)
    output$tool_text_out <- renderText({ result })
  })

  # 反向：分隔符文本 → 多行
  observeEvent(input$tool_reverse_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    sep <- input$tool_sep %||% ","
    # 按分隔符拆分
    parts <- strsplit(txt, sep, fixed = TRUE)[[1]]
    parts <- trimws(parts)
    result <- paste(parts, collapse = "\n")
    output$tool_text_out <- renderText({ result })
  })

  # 清空
  observeEvent(input$tool_clear_btn, {
    updateTextAreaInput(session, "tool_text_in", value = "")
    output$tool_text_out <- renderText({ "" })
  })

  # 拼音：累积输出历史
  py_outputs <- reactiveVal(list())
  py_output_labels <- reactiveVal(list())  # 记录每次的 mode 标签

  observeEvent(input$tool_py_result, {
    req(rv$logged_in)
    result <- input$tool_py_result
    if (!is.null(result$html) && nchar(result$html) > 0) {
      mode_label <- switch(result$mode,
        above = "上部拼音", pure = "纯拼音", num = "数字调", char = "逐字", mixed = "混合")
      py_output_labels(c(py_output_labels(), mode_label))
      py_outputs(c(py_outputs(), result$html))
    }
  })

  # 渲染累积输出
  output$tool_py_out <- renderUI({
    req(rv$logged_in)
    items <- py_outputs()
    labels <- py_output_labels()
    if (length(items) == 0) return(tags$p(style = "color:#999; text-align:center; padding:40px 0; margin:0;", "点击左侧按钮生成拼音"))
    tagList(lapply(seq_along(items), function(i) {
      idx <- length(items) - i + 1L
      item <- items[[idx]]
      lbl <- labels[[idx]]
      tags$div(class = "py-block",
        style = "border-bottom:1px dashed #ddd; padding:4px 0;",
        tags$div(style = "font-size:10px; color:#888; margin-bottom:1px;", lbl),
        if (isTRUE(lbl == "上部拼音")) HTML(item) else tags$pre(item)
      )
    }))
  })

  # 清空拼音输入
  observeEvent(input$tool_py_clear_in, {
    updateTextAreaInput(session, "tool_py_in", value = "")
  })
  # 清空拼音输出
  observeEvent(input$tool_py_clear_out, {
    py_outputs(list())
    py_output_labels(list())
  })

  observeEvent(input$tool_py_above, { session$sendCustomMessage("doPinyin", "above") })
  observeEvent(input$tool_py_pure,  { session$sendCustomMessage("doPinyin", "pure") })
  observeEvent(input$tool_py_num,   { session$sendCustomMessage("doPinyin", "num") })
  observeEvent(input$tool_py_char,  { session$sendCustomMessage("doPinyin", "char") })
  observeEvent(input$tool_py_mixed, { session$sendCustomMessage("doPinyin", "mixed") })
}
