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

  # 数字序号 1、2、... → +
  observeEvent(input$tool_num2plus_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    lines <- strsplit(txt, "\\r?\\n")[[1]]
    lines <- trimws(lines, which = "left")
    lines <- gsub("^\\d+[、，]\\s*", "+", lines)
    result <- paste(lines, collapse = "\n")
    output$tool_text_out <- renderText({ result })
  })

  # 加序号
  observeEvent(input$tool_addnum_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    lines <- strsplit(txt, "\\r?\\n")[[1]]
    lines <- lines[trimws(lines) != ""]
    result <- paste(sprintf("%d、%s", seq_along(lines), lines), collapse = "\n")
    output$tool_text_out <- renderText({ result })
  })

  # 去序号
  observeEvent(input$tool_delnum_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    lines <- strsplit(txt, "\\r?\\n")[[1]]
    lines <- gsub("^\\d+[、，.)）]\\s*", "", lines)
    lines <- gsub("^[+]+\\d+[、，.)）]?\\s*", "", lines)
    result <- paste(lines, collapse = "\n")
    output$tool_text_out <- renderText({ result })
  })

  # 去空格
  observeEvent(input$tool_nospc_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    result <- gsub("\\s+", "", txt)
    output$tool_text_out <- renderText({ result })
  })

  # 加前缀
  observeEvent(input$tool_prefix_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    showModal(modalDialog(
      title = "加固定前缀",
      textInput("tool_prefix_val", "前缀内容", placeholder = "例如：+、●、- "),
      footer = tagList(
        actionButton("tool_prefix_confirm", "确定", class = "btn-primary"),
        modalButton("取消")
      ), size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$tool_prefix_confirm, {
    req(rv$logged_in, input$tool_prefix_val)
    txt <- input$tool_text_in %||% ""
    prefix <- input$tool_prefix_val
    lines <- strsplit(txt, "\\r?\\n")[[1]]
    lines <- lines[trimws(lines) != ""]
    result <- paste(paste0(prefix, lines), collapse = "\n")
    output$tool_text_out <- renderText({ result })
    removeModal()
  })

  # 奇偶合并：奇数行加:，偶数行拼接到上一行后面
  observeEvent(input$tool_merge_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    lines <- strsplit(txt, "\\r?\\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    result <- c()
    for (i in seq(1, length(lines), by = 2)) {
      odd <- lines[i]
      if (i + 1 <= length(lines)) {
        result <- c(result, paste0(odd, ":", lines[i+1]))
      } else {
        result <- c(result, paste0(odd, ":"))
      }
    }
    output$tool_text_out <- renderText({ paste(result, collapse = "\n") })
  })

  # ● / · → +
  observeEvent(input$tool_dot2plus_btn, {
    req(rv$logged_in)
    txt <- input$tool_text_in %||% ""
    if (nchar(trimws(txt)) == 0) { showNotification("请先输入文本", type = "warning"); return() }
    result <- gsub("[●·]", "+", txt)
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

  # ═══════════════════ 记算 ═══════════════════
  calc_total <- reactiveVal(0)
  calc_history <- reactiveVal(list())

  # 解析每行：提取数字，-前缀转负，支持基本算术表达式
  parse_line <- function(line) {
    line <- trimws(line)
    if (nchar(line) == 0) return(numeric(0))
    # 尝试直接 eval 算术表达式（如 5*8+10）
    if (grepl("[*+/]", line) && !grepl("^[-]?[0-9. ]+$", line)) {
      result <- tryCatch(eval(parse(text = line)), error = function(e) NULL)
      if (is.numeric(result)) return(result)
    }
    # 空格分隔的数字（含负数）
    parts <- strsplit(line, "\\s+")[[1]]
    nums <- suppressWarnings(as.numeric(parts))
    nums[!is.na(nums)]
  }

  do_calc <- function(mode) {
    req(rv$logged_in)
    txt <- input$tool_calc_in %||% ""
    lines <- strsplit(txt, "\n")[[1]]
    all_nums <- numeric(0)
    line_exprs <- character(0)
    for (line in lines) {
      nums <- parse_line(line)
      if (length(nums) > 0) {
        all_nums <- c(all_nums, nums)
        line_exprs <- c(line_exprs, paste(nums, collapse = " + "))
      }
    }
    if (length(all_nums) == 0) {
      output$tool_calc_out <- renderText("无有效数字")
      return()
    }
    result <- switch(mode,
      sum = sum(all_nums),
      avg = mean(all_nums),
      mul = prod(all_nums),
      max = max(all_nums),
      min = min(all_nums),
      count = length(all_nums),
      sum
    )
    # 输出表达式
    expr <- switch(mode,
      sum = paste(line_exprs, collapse = "\n+ "),
      avg = sprintf("(%s) / %d", paste(all_nums, collapse = " + "), length(all_nums)),
      mul = paste(paste(all_nums, collapse = " x "), "\n= "),
      max = paste("max(", paste(all_nums, collapse = ", "), ")"),
      min = paste("min(", paste(all_nums, collapse = ", "), ")"),
      count = sprintf("共 %d 个数字", length(all_nums)),
      ""
    )
    output$tool_calc_out <- renderText(sprintf("%s\n= %s", expr, format(result, scientific = FALSE)))
    calc_total(result)
  }

  observeEvent(input$tool_calc_sum,   { do_calc("sum") })
  observeEvent(input$tool_calc_avg,   { do_calc("avg") })
  observeEvent(input$tool_calc_mul,   { do_calc("mul") })
  observeEvent(input$tool_calc_max,   { do_calc("max") })
  observeEvent(input$tool_calc_min,   { do_calc("min") })
  observeEvent(input$tool_calc_count, { do_calc("count") })

  # 直接计算：每行 eval 算术表达式（如 18000*0.3*0.1951）
  observeEvent(input$tool_calc_direct, {
    req(rv$logged_in)
    txt <- input$tool_calc_in %||% ""
    lines <- strsplit(txt, "\n")[[1]]
    results <- character(0)
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) == 0) next
      # 只有数字和运算符的表达式直接 eval
      if (grepl("^[0-9.+\\-*/^()% ]+$", line)) {
        v <- tryCatch(eval(parse(text = line)), error = function(e) NULL)
        if (is.numeric(v)) {
          results <- c(results, sprintf("%s = %s", line, format(v, scientific = FALSE)))
        } else {
          results <- c(results, sprintf("%s = ERROR", line))
        }
      } else {
        results <- c(results, sprintf("%s = (skip: not a pure expression)", line))
      }
    }
    if (length(results) == 0) {
      output$tool_calc_out <- renderText("无有效表达式")
      return()
    }
    out <- paste(results, collapse = "\n")
    output$tool_calc_out <- renderText(out)
    # 记录历史
    h <- calc_history()
    h <- c(h, list(list(value = out, time = format(Sys.time(), "%H:%M:%S"), expr = "直接计算")))
    if (length(h) > 50) h <- h[(length(h) - 49):length(h)]
    calc_history(h)
  })

  # 添加当前结果到累积
  observeEvent(input$tool_calc_reuse, {
    req(rv$logged_in)
    v <- calc_total()
    cur <- input$tool_calc_in %||% ""
    new_txt <- if (nchar(trimws(cur)) == 0) as.character(v) else paste0(cur, "\n", v)
    updateTextAreaInput(session, "tool_calc_in", value = new_txt)
    # 记录历史
    h <- calc_history()
    h <- c(h, list(list(value = v, time = format(Sys.time(), "%H:%M:%S"), expr = paste0("+ ", v))))
    if (length(h) > 50) h <- h[(length(h) - 49):length(h)]
    calc_history(h)
  })

  observeEvent(input$tool_calc_reset, {
    calc_total(0)
    calc_history(list())
  })

  observeEvent(input$tool_calc_clear, {
    updateTextAreaInput(session, "tool_calc_in", value = "")
    calc_total(0)
    calc_history(list())
    output$tool_calc_out <- renderText("")
  })

  output$tool_calc_history <- renderUI({
    req(rv$logged_in)
    h <- calc_history()
    if (length(h) == 0) return(tags$p(style = "color:#999;font-size:12px;", "暂无历史"))
    acc <- 0
    do.call(tagList, lapply(rev(seq_along(h)), function(i) {
      acc <<- acc + h[[i]]$value
      tags$div(style = "font-size:12px;padding:2px 0;border-bottom:1px dotted #eee;display:flex;justify-content:space-between;",
        tags$span(h[[i]]$expr),
        tags$span(style = "color:#2e7d32;font-weight:bold;", sprintf("累积: %s", format(acc, scientific = FALSE)))
      )
    }))
  })
}
