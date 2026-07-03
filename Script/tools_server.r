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
}
