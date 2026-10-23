##################
# 知识库模块 — 服务端
##################

knowledge_server <- function(input, output, session, rv) {
  kb_trigger <- reactiveVal(0)
  kb_refresh <- function() kb_trigger(kb_trigger() + 1)

  # 当前分类筛选（NULL=全部）
  kb_cat_filter <- reactiveVal(NULL)
  # 当前标签筛选（NULL=全部）
  kb_tag_filter <- reactiveVal(NULL)
  # 当前视图：list(type="list") 或 list(type="meta"/"chunk", df=data.frame)
  kb_view <- reactiveVal(list(type = "list"))

  # 初始化建表
  observe({
    req(rv$logged_in)
    kb_table_init()
  })

  # 读取文本文件（UTF-8 → GBK → Latin1 兜底）
  kb_read_text_file <- function(path) {
    txt <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    txt <- paste(txt, collapse = "\n")
    if (!nzchar(txt) || grepl("�", txt)) {
      txt2 <- tryCatch(iconv(readBin(path, what = "raw", n = file.info(path)$size), from = "GBK", to = "UTF-8", sub = ""), error = function(e) "")
      if (is.character(txt2) && length(txt2) == 1 && nzchar(txt2)) txt <- txt2
    }
    txt
  }

  # ── 分类列表（可点击筛选）──
  output$kb_cat_list <- renderUI({
    kb_trigger()
    req(rv$logged_in)
    cats <- kb_cat_get_all()
    items <- list(tags$a(class = paste("kb-cat-item", if (is.null(kb_cat_filter())) " active" else ""),
      `data-id` = "", icon("inbox"), " 全部", tags$span(style = "float:right;color:#999;", sum(cats$doc_count %||% 0))))
    if (nrow(cats) > 0) {
      for (i in seq_len(nrow(cats))) {
        active <- !is.null(kb_cat_filter()) && kb_cat_filter() == cats$id[i]
        items[[length(items) + 1]] <- tags$a(class = paste("kb-cat-item", if (active) " active" else ""),
          `data-id` = cats$id[i], icon("folder"), " ", cats$name[i],
          tags$span(style = "float:right;color:#999;", cats$doc_count[i] %||% 0))
      }
    }
    tagList(items)
  })
  outputOptions(output, "kb_cat_list", suspendWhenHidden = FALSE)

  # 点击分类筛选
  observeEvent(input$kb_cat_click, {
    id <- input$kb_cat_click
    kb_cat_filter(if (is.null(id) || id == "") NULL else as.integer(id))
    rv$kb_view <- list(type = "list")
    kb_refresh()
  })

  # ── 标签云 ──
  output$kb_tag_cloud <- renderUI({
    kb_trigger()
    req(rv$logged_in)
    tag_list <- kb_tag_get_all()
    if (length(tag_list) == 0) return(tags$p(style = "color:#999;", "暂无标签"))
    tagList(lapply(tag_list, function(t) {
      tags$span(class = "kb-tag-chip", `data-tag` = t, t)
    }))
  })
  outputOptions(output, "kb_tag_cloud", suspendWhenHidden = FALSE)

  observeEvent(input$kb_tag_click, {
    kb_tag_filter(input$kb_tag_click)
    rv$kb_view <- list(type = "list")
    kb_refresh()
  })

  # ── 统计 ──
  output$kb_stats <- renderUI({
    kb_trigger()
    req(rv$logged_in)
    docs <- kb_doc_get_all()
    cats <- kb_cat_get_all()
    tags$div(style = "font-size:13px; color:#555;",
      tags$b(nrow(docs)), " 篇文档 · ",
      tags$b(nrow(cats)), " 个分类")
  })
  outputOptions(output, "kb_stats", suspendWhenHidden = FALSE)

  # ── 结果标题 ──
  output$kb_result_title <- renderUI({
    kb_trigger()
    v <- kb_view()
    if (v$type == "list") return(NULL)
    if (v$type == "chunk") {
      tags$div(style = "margin:4px 0 8px; color:#1565c0; font-weight:bold;",
        icon("search"), " 正文检索结果（", nrow(v$df), " 条片段）")
    } else {
      tags$div(style = "margin:4px 0 8px; color:#1565c0; font-weight:bold;",
        icon("search"), " 检索结果（", nrow(v$df), " 篇文档）")
    }
  })
  outputOptions(output, "kb_result_title", suspendWhenHidden = FALSE)

  # ── 主表格（文档列表 / 检索结果）──
  output$kb_doc_table <- renderDT({
    kb_trigger()
    req(rv$logged_in)
    v <- kb_view()

    if (v$type == "chunk") {
      df <- v$df
      if (nrow(df) == 0) return(datatable(data.frame(提示 = "无匹配片段"), options = list(dom = "t")))
      display <- data.frame(
        来源文档 = sprintf('%s <button class="btn btn-xs btn-link kb-goto-btn" data-id="%d">打开</button>', df$title %||% "", df$document_id),
        分类 = df$category_name %||% "",
        片段 = df$content %||% "",
        stringsAsFactors = FALSE, check.names = FALSE
      )
      return(datatable(display, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(pageLength = 20, autoWidth = TRUE, dom = "ltip", scrollX = TRUE),
        class = "cell-border stripe compact"))
    }

    # list / meta 视图
    if (v$type == "meta") {
      df <- v$df
    } else {
      df <- kb_doc_get_all(kb_cat_filter())
      # 标签筛选
      tg <- kb_tag_filter()
      if (!is.null(tg) && nzchar(tg)) {
        df <- df[grepl(tg, df$tags %||% "", fixed = TRUE), , drop = FALSE]
      }
    }
    if (nrow(df) == 0) return(datatable(data.frame(提示 = "暂无文档"), options = list(dom = "t")))

    display <- data.frame(
      编号 = df$doc_no %||% "",
      文档名称 = sprintf('%s <button class="btn btn-xs btn-link kb-view-btn" data-id="%d">预览</button>', df$title %||% "", df$id),
      分类 = df$category_name %||% "",
      标签 = df$tags %||% "",
      更新时间 = substr(df$updated_at %||% "", 1, 16),
      大小 = sapply(df$file_size %||% 0, function(s) {
        if (is.na(s) || s == 0) return("-")
        if (s < 1024) return(paste0(s, " B"))
        if (s < 1048576) return(paste0(round(s / 1024, 1), " KB"))
        paste0(round(s / 1048576, 1), " MB")
      }),
      操作 = paste0(
        sprintf('<button class="btn btn-xs btn-info kb-edit-btn" data-id="%d">编辑</button> ', df$id),
        sprintf('<button class="btn btn-xs btn-warning kb-rechunk-btn" data-id="%d" title="重新分片">分片</button> ', df$id),
        sprintf('<button class="btn btn-xs btn-danger kb-del-btn" data-id="%d">删除</button>', df$id)
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )

    op_idx <- ncol(display) - 1
    datatable(display, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(pageLength = 20, autoWidth = TRUE, dom = "ltip", scrollX = TRUE,
        columnDefs = list(list(targets = op_idx, orderable = FALSE))),
      class = "cell-border stripe compact")
  })
  outputOptions(output, "kb_doc_table", suspendWhenHidden = FALSE)

  # ── 检索 ──
  observeEvent(input$kb_search_btn, {
    req(rv$logged_in)
    kw <- trimws(input$kb_search %||% "")
    if (!nzchar(kw)) { rv$kb_view <- list(type = "list"); kb_refresh(); return() }
    kb_log("检索", kw, if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL, paste("模式:", input$kb_search_mode))
    if (identical(input$kb_search_mode, "chunk")) {
      cfg <- kb_get_config()
      r <- kb_search_chunks(kw, cfg$topn)
      rv$kb_view <- list(type = "chunk", df = r)
    } else {
      r <- kb_search_docs(kw, kb_cat_filter())
      rv$kb_view <- list(type = "meta", df = r)
    }
    kb_refresh()
  })

  # ── 上传文档 ──
  observeEvent(input$kb_upload_btn, {
    req(rv$logged_in)
    cats <- kb_cat_get_all()
    cat_choices <- c("未分类" = "")
    if (nrow(cats) > 0) cat_choices <- c(cat_choices, setNames(as.character(cats$id), cats$name))
    showModal(modalDialog(
      title = "上传文档",
      size = "m", easyClose = TRUE,
      fileInput("kb_upload_file", "选择文件（.txt / .md）", accept = c(".txt", ".md", "text/plain", "text/markdown")),
      textInput("kb_upload_title", "文档名称（留空用文件名）"),
      selectInput("kb_upload_cat", "分类", choices = cat_choices),
      textInput("kb_upload_tags", "标签（多个用逗号分隔）"),
      textInput("kb_upload_summary", "简介（选填）"),
      tags$p(style = "color:#e65100; font-size:12px;", "PDF/DOCX 解析将在后续迭代支持（需登记 pdftools/officer 依赖）"),
      footer = tagList(modalButton("取消"), actionButton("kb_upload_confirm", "上传", class = "btn-primary"))
    ))
  })

  observeEvent(input$kb_upload_confirm, {
    req(rv$logged_in, input$kb_upload_file)
    f <- input$kb_upload_file
    ext <- tolower(tools::file_ext(f$name))
    if (!ext %in% c("txt", "md")) {
      showNotification("暂仅支持 .txt / .md，PDF/DOCX 请手动录入或等待后续迭代", type = "error", duration = 6)
      return()
    }
    content <- kb_read_text_file(f$datapath)
    if (!nzchar(content)) { showNotification("文件内容为空或解析失败", type = "error"); return() }
    title <- trimws(input$kb_upload_title %||% "")
    if (!nzchar(title)) title <- sub("\\.[^.]+$", "", f$name)
    # 保存原始文件
    dir <- kb_file_dir()
    dest <- file.path(dir, f$name)
    file.copy(f$datapath, dest, overwrite = TRUE)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- kb_doc_add(title = title, category_id = input$kb_upload_cat,
      tags = input$kb_upload_tags, summary = input$kb_upload_summary, content = content,
      source_type = "upload", file_name = f$name, file_path = dest, file_size = f$size, created_by = uid)
    if (result$success) { removeModal(); rv$kb_view <- list(type = "list") }
    kb_refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 手动录入 ──
  observeEvent(input$kb_manual_btn, {
    req(rv$logged_in)
    cats <- kb_cat_get_all()
    cat_choices <- c("未分类" = "")
    if (nrow(cats) > 0) cat_choices <- c(cat_choices, setNames(as.character(cats$id), cats$name))
    showModal(modalDialog(
      title = "录入文档",
      size = "l", easyClose = TRUE,
      textInput("kb_manual_title", "文档名称 *"),
      selectInput("kb_manual_cat", "分类", choices = cat_choices),
      textInput("kb_manual_tags", "标签（多个用逗号分隔）"),
      textInput("kb_manual_summary", "简介（选填）"),
      textAreaInput("kb_manual_content", "正文（支持 Markdown）", rows = 12, width = "100%"),
      footer = tagList(modalButton("取消"), actionButton("kb_manual_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$kb_manual_confirm, {
    req(rv$logged_in, input$kb_manual_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- kb_doc_add(title = input$kb_manual_title, category_id = input$kb_manual_cat,
      tags = input$kb_manual_tags, summary = input$kb_manual_summary, content = input$kb_manual_content,
      source_type = "manual", created_by = uid)
    if (result$success) { removeModal(); rv$kb_view <- list(type = "list") }
    kb_refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 预览 ──
  observeEvent(input$kb_view_click, {
    req(rv$logged_in)
    doc <- kb_doc_get_by_id(as.integer(input$kb_view_click))
    if (is.null(doc) || nrow(doc) == 0) return()
    body <- doc$content[1] %||% ""
    showModal(modalDialog(
      title = doc$title[1] %||% "",
      size = "l", easyClose = TRUE,
      tags$div(style = "font-size:12px; color:#888; margin-bottom:8px;",
        "分类：", doc$category_name[1] %||% "未分类", " · 标签：", doc$tags[1] %||% "-",
        " · 更新：", doc$updated_at[1] %||% ""),
      tags$pre(style = "white-space:pre-wrap; max-height:500px; overflow-y:auto; background:#fafbfc; padding:12px; border-radius:6px; border:1px solid #e0e4e8;", body),
      footer = tagList(modalButton("关闭"))
    ))
  })

  # ── 编辑 ──
  observeEvent(input$kb_edit_click, {
    req(rv$logged_in)
    doc <- kb_doc_get_by_id(as.integer(input$kb_edit_click))
    if (is.null(doc) || nrow(doc) == 0) return()
    rv$kb_edit_id <- doc$id[1]
    cats <- kb_cat_get_all()
    cat_choices <- c("未分类" = "")
    if (nrow(cats) > 0) cat_choices <- c(cat_choices, setNames(as.character(cats$id), cats$name))
    manual <- identical(doc$source_type[1], "manual")
    showModal(modalDialog(
      title = paste("编辑文档 -", doc$title[1] %||% ""),
      size = "l", easyClose = TRUE,
      textInput("kb_edit_title", "文档名称 *", value = doc$title[1] %||% ""),
      selectInput("kb_edit_cat", "分类", choices = cat_choices,
        selected = if (is.na(doc$category_id[1]) || doc$category_id[1] == "") "" else as.character(doc$category_id[1])),
      textInput("kb_edit_tags", "标签（多个用逗号分隔）", value = doc$tags[1] %||% ""),
      textInput("kb_edit_summary", "简介", value = doc$summary[1] %||% ""),
      if (manual) textAreaInput("kb_edit_content", "正文", value = doc$content[1] %||% "", rows = 10, width = "100%")
        else tags$p(style = "color:#999; font-size:12px;", "上传文档仅可编辑元信息，正文请在原文件修改后重新上传。"),
      footer = tagList(modalButton("取消"), actionButton("kb_edit_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$kb_edit_confirm, {
    req(rv$logged_in, rv$kb_edit_id, input$kb_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    manual <- NULL
    if (!is.null(input$kb_edit_content)) manual <- input$kb_edit_content
    result <- kb_doc_update(rv$kb_edit_id, title = input$kb_edit_title,
      category_id = input$kb_edit_cat, tags = input$kb_edit_tags,
      summary = input$kb_edit_summary, content = manual, changed_by = uid)
    if (result$success) removeModal()
    kb_refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 删除 ──
  observeEvent(input$kb_del_click, {
    req(rv$logged_in)
    cid <- as.integer(input$kb_del_click)
    doc <- kb_doc_get_by_id(cid)
    if (is.null(doc) || nrow(doc) == 0) return()
    showModal(modalDialog(
      title = "确认删除文档",
      tags$p(sprintf("确定删除「%s」吗？原始文件、文本分片将一并删除。", doc$title[1] %||% "")),
      footer = tagList(modalButton("取消"), actionButton("kb_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$kb_del_confirm, {
    req(rv$logged_in)
    cid <- as.integer(input$kb_del_click)
    kb_log("删除", cid, if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL, "")
    result <- kb_doc_delete(cid)
    removeModal()
    kb_refresh()
    showNotification(result$message, type = "warning")
  })

  # ── 重新分片 ──
  observeEvent(input$kb_rechunk_click, {
    req(rv$logged_in)
    cid <- as.integer(input$kb_rechunk_click)
    result <- kb_doc_rechunk(cid)
    kb_refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 检索结果跳转 ──
  observeEvent(input$kb_goto_click, {
    req(rv$logged_in)
    doc <- kb_doc_get_by_id(as.integer(input$kb_goto_click))
    if (is.null(doc) || nrow(doc) == 0) return()
    showModal(modalDialog(
      title = doc$title[1] %||% "",
      size = "l", easyClose = TRUE,
      tags$pre(style = "white-space:pre-wrap; max-height:500px; overflow-y:auto; background:#fafbfc; padding:12px; border-radius:6px;", doc$content[1] %||% ""),
      footer = tagList(modalButton("关闭"))
    ))
  })

  # ── 分类管理 ──
  observeEvent(input$kb_cat_manage_btn, {
    req(rv$logged_in)
    cats <- kb_cat_get_all()
    rows_html <- if (nrow(cats) == 0) {
      tags$p(style = "color:#999;", "暂无分类")
    } else {
      tagList(lapply(seq_len(nrow(cats)), function(i) {
        c <- cats[i, ]
        tags$div(style = "display:flex; align-items:center; gap:6px; padding:4px 0; border-bottom:1px dotted #eee;",
          tags$span(style = "flex:1;", c$name, tags$span(style = "color:#999; font-size:11px;", sprintf("（%d 篇）", c$doc_count %||% 0))),
          tags$button(class = "btn btn-xs btn-warning kbcat-edit-btn", `data-id` = c$id, "改名"),
          tags$button(class = "btn btn-xs btn-danger kbcat-del-btn", `data-id` = c$id, "删除")
        )
      }))
    }
    showModal(modalDialog(
      title = "分类管理",
      size = "m", easyClose = TRUE,
      rows_html,
      hr(),
      textInput("kbcat_new_name", NULL, placeholder = "新分类名称"),
      footer = tagList(
        actionButton("kbcat_add_btn", "添加分类", class = "btn-primary"),
        modalButton("关闭")
      )
    ))
  })

  observeEvent(input$kbcat_add_btn, {
    req(rv$logged_in, input$kbcat_new_name)
    result <- kb_cat_add(input$kbcat_new_name)
    kb_refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) removeModal()
  })

  observeEvent(input$kbcat_edit_click, {
    req(rv$logged_in)
    cid <- as.integer(input$kbcat_edit_click)
    cats <- kb_cat_get_all()
    cur <- cats[cats$id == cid, ]
    cur_name <- if (nrow(cur) > 0) cur$name[1] %||% "" else ""
    showModal(modalDialog(
      title = "分类改名",
      size = "s", easyClose = TRUE,
      textInput("kbcat_edit_name", "名称", value = cur_name),
      footer = tagList(modalButton("取消"), actionButton("kbcat_edit_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$kbcat_edit_confirm, {
    req(rv$logged_in, input$kbcat_edit_click)
    result <- kb_cat_update(as.integer(input$kbcat_edit_click), name = input$kbcat_edit_name)
    removeModal()
    kb_refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  observeEvent(input$kbcat_del_click, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "删除分类",
      size = "s", easyClose = TRUE,
      tags$p("删除分类时，该分类下文档将一并删除。确定继续吗？"),
      footer = tagList(modalButton("取消"), actionButton("kbcat_del_confirm", "确认删除", class = "btn-danger"))
    ))
  })

  observeEvent(input$kbcat_del_confirm, {
    req(rv$logged_in, input$kbcat_del_click)
    result <- kb_cat_delete(as.integer(input$kbcat_del_click), mode = "delete")
    removeModal()
    kb_refresh()
    showNotification(result$message, type = "warning")
  })

  # ── 设置 ──
  observeEvent(input$kb_settings_btn, {
    req(rv$logged_in)
    cfg <- kb_get_config()
    showModal(modalDialog(
      title = "知识库设置",
      size = "m", easyClose = TRUE,
      fluidRow(
        column(6, numericInput("kb_set_chunk", "分片长度（字符）", value = cfg$chunk_size, min = 100, step = 50)),
        column(6, numericInput("kb_set_overlap", "分片重叠（字符）", value = cfg$chunk_overlap, min = 0, step = 10))
      ),
      fluidRow(
        column(6, numericInput("kb_set_topn", "检索返回条数 topN", value = cfg$topn, min = 1, max = 50)),
        column(6, textInput("kb_set_dir", "文件存储目录", value = cfg$file_dir))
      ),
      footer = tagList(modalButton("取消"), actionButton("kb_set_save", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$kb_set_save, {
    req(rv$logged_in)
    result <- kb_set_config(chunk_size = input$kb_set_chunk, chunk_overlap = input$kb_set_overlap,
      topn = input$kb_set_topn, file_dir = input$kb_set_dir)
    removeModal()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 日志查看 ──
  observeEvent(input$kb_log_btn, {
    req(rv$logged_in)
    lines <- kb_log_get_recent(200)
    txt <- if (length(lines) == 0) "暂无日志" else paste(lines, collapse = "\n")
    showModal(modalDialog(
      title = "知识库操作日志（最近 200 条）",
      size = "l", easyClose = TRUE,
      tags$pre(style = "white-space:pre-wrap; max-height:500px; overflow-y:auto; background:#1e1e1e; color:#d4d4d4; padding:12px; border-radius:6px; font-size:12px;", txt),
      footer = tagList(modalButton("关闭"))
    ))
  })
}
