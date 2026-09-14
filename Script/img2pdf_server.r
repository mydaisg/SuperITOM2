# 图片合并 PDF 工具 — 服务端
img2pdf_server <- function(input, output, session, rv) {

  i2p_trigger <- reactiveVal(0)

  i2p_operator <- function() {
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      op <- rv$current_user$display_name[1]
      if (is.null(op) || nchar(trimws(op)) == 0) op <- rv$current_user$username[1]
      op
    } else "系统"
  }

  # 按钮灰化：有文件才可点
  observe({
    has_file <- !is.null(input$i2p_file) && nrow(input$i2p_file) > 0
    session$sendCustomMessage("toggleBtn", list(id = "i2p_generate", disabled = !isTRUE(has_file)))
  })

  # 显示已选文件列表
  output$i2p_file_list <- renderUI({
    f <- input$i2p_file
    if (is.null(f) || nrow(f) == 0) {
      return(tags$i("尚未选择图片"))
    }
    tagList(lapply(seq_len(nrow(f)), function(i) {
      tags$div(sprintf("%d. %s", i, f$name[i]))
    }))
  })

  # 生成 PDF
  observeEvent(input$i2p_generate, {
    req(rv$logged_in)
    f <- input$i2p_file
    if (is.null(f) || nrow(f) == 0) {
      showNotification("请先选择图片", type = "warning")
      return()
    }
    # 保存上传文件到持久化目录（生成前的文件存储地址）
    # ★ 用 .bin 后缀：避免匹配 shiny.autoreload 监视模式 .(r|html|js|css|png|jpg|gif) 触发页面刷新回首页
    img2pdf_ensure_dirs()
    up_dir <- img2pdf_upload_dir()
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    saved_paths <- character(0)
    for (i in seq_len(nrow(f))) {
      # 落盘统一 .bin（magick 可自动 sniff 图片格式，避免 .png/.jpg 触发 autoreload）
      dest <- file.path(up_dir, sprintf("%s_%03d.bin", ts, i))
      ok <- file.copy(f$datapath[i], dest, overwrite = TRUE)
      if (ok) saved_paths <- c(saved_paths, dest)
    }
    if (length(saved_paths) == 0) {
      showNotification("图片保存失败", type = "error")
      return()
    }
    # 输出 PDF 路径
    out_dir <- img2pdf_output_dir()
    out_path <- file.path(out_dir, sprintf("merge_%s.pdf", ts))
    # 水印参数（多行 → 多条）
    wm_raw <- input$i2p_watermark %||% ""
    wm_lines <- strsplit(wm_raw, "\\r?\\n")[[1]]
    wm_lines <- trimws(wm_lines)
    wm_lines <- wm_lines[nchar(wm_lines) > 0]
    wm <- if (length(wm_lines) == 0) NULL else wm_lines
    wm_color <- input$i2p_wm_color %||% "#CCCCCC"
    wm_alpha <- as.numeric(input$i2p_wm_alpha %||% 0.3)
    wm_size <- as.integer(input$i2p_wm_size %||% 40)
    wm_angles <- as.numeric(input$i2p_wm_angles %||% 0)
    if (length(wm_angles) == 0) wm_angles <- 0
    wm_cols <- as.integer(input$i2p_wm_cols %||% 3)
    wm_rows <- as.integer(input$i2p_wm_rows %||% 3)
    wm_spacing <- as.numeric(input$i2p_wm_spacing %||% 0.2)
    stack <- isTRUE(input$i2p_stack %||% "horizontal" == "vertical")

    showNotification("正在合并生成 PDF ...", type = "message", duration = NULL, id = "i2p_working")
    res <- img2pdf_merge(saved_paths, out_path, watermark = wm,
                         wm_color = wm_color, wm_alpha = wm_alpha, wm_size = wm_size,
                         wm_angles = wm_angles, wm_cols = wm_cols, wm_rows = wm_rows,
                         wm_spacing = wm_spacing, stack = stack)
    removeNotification(id = "i2p_working")

    if (!isTRUE(res$success)) {
      output$i2p_result <- renderUI({
        tags$div(style = "color:#d9534f;", icon("times-circle"), " ", res$message)
      })
      return()
    }
    # 写入记录（生成前地址 + 生成后路径）
    rec <- img2pdf_add_record(saved_paths, out_path,
                              watermark_text = paste(wm %||% "", collapse = "\n"),
                              watermark_color = wm_color,
                              operator = i2p_operator())
    # 下载链接（downloadHandler，避免写 www 触发 devmode 热重载）
    output$i2p_result <- renderUI({
      tags$div(style = "color:#2e7d32;",
        icon("check-circle"),
        sprintf(" 生成成功（合并为 %d 页），记录号 %s", res$page_count, rec$record_no %||% ""),
        br(),
        downloadButton("i2p_download", "下载 PDF", class = "btn-sm btn-primary"),
        if (!is.null(wm)) tags$div(style = "font-size:11px; color:#999;", "水印：", paste(wm, collapse = "、"))
      )
    })
    # 保存最近生成的 PDF 路径供 downloadHandler 使用
    rv$i2p_last_pdf <- out_path
    i2p_trigger(i2p_trigger() + 1)
  })

  # 下载生成的 PDF（从 DB/img2pdf 目录读取，不经过 www 静态资源）
  output$i2p_download <- downloadHandler(
    filename = function() {
      p <- rv$i2p_last_pdf
      if (is.null(p) || !file.exists(p)) return("merge.pdf")
      basename(p)
    },
    content = function(file) {
      p <- rv$i2p_last_pdf
      if (is.null(p) || !file.exists(p)) return(NULL)
      file.copy(p, file, overwrite = TRUE)
    }
  )

  # 历史记录表
  output$i2p_history_table <- renderDT({
    req(rv$logged_in)
    i2p_trigger()
    recs <- img2pdf_get_records(50)
    if (nrow(recs) == 0) {
      return(datatable(data.frame(提示 = "暂无记录"), options = list(dom = "t"), rownames = FALSE))
    }
    # 历史表：显示完整存储路径（生成前图片地址 + 生成后 PDF 路径）
    df <- data.frame(
      记录号 = recs$record_no,
      页数 = recs$page_count,
      水印 = recs$watermark_text,
      颜色 = recs$watermark_color,
      操作人 = recs$created_by,
      时间 = recs$created_at,
      生成前图片地址 = recs$source_paths,
      生成后PDF路径 = recs$output_path,
      stringsAsFactors = FALSE
    )
    datatable(df, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 10, dom = "tip", autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = c(0, 1, 2, 3, 4, 5)),
          list(width = "280px", targets = 6),
          list(width = "280px", targets = 7)
        )))
  })
}
