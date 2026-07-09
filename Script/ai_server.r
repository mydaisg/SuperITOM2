# AI 模块 Server — 动态配置驱动
ai_server <- function(input, output, session, rv) {

  # Admin 权限
  is_admin <- reactive({
    !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
  })
  output$ai_is_admin <- reactive({ is_admin() })
  outputOptions(output, "ai_is_admin", suspendWhenHidden = FALSE)

  # 初始化种子数据
  observe({
    req(rv$logged_in)
    ai_init_seed()
  })

  # 当前搜索引擎列表
  ai_search_list <- reactive({
    req(rv$logged_in)
    tryCatch(ai_get_search_engines(), error = function(e) list())
  })

  # 当前 AI 工具列表
  ai_chat_list <- reactive({
    req(rv$logged_in)
    tryCatch(ai_get_chat_tools(), error = function(e) list())
  })

  # ─── 搜索 iframe 动态渲染 ───
  output$ai_search_frames <- renderUI({
    req(rv$logged_in)
    sl <- ai_search_list()
    enabled <- Filter(function(x) isTRUE(x$enabled), sl)
    if (length(enabled) == 0)
      return(tags$div(style = "text-align:center;padding:30px;color:#999;",
        "暂无可用搜索引擎，请在「配置」中添加"))

    cols_per_row <- if (length(enabled) >= 4) 2 else if (length(enabled) >= 2) 2 else 1
    min_w <- if (cols_per_row == 2) "45%" else "100%"
    rows <- split(enabled, ceiling(seq_along(enabled) / cols_per_row))

    tagList(lapply(rows, function(row_items) {
      tags$div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:12px;",
        lapply(row_items, function(se) {
          frame_id <- paste0("ai_search_", se$id)
          tags$div(style = sprintf("flex:1; min-width:%s;", min_w),
            tags$b(icon(se$icon %||% "search"), " ", se$name), tags$br(),
            tags$iframe(id = frame_id, style = sprintf("width:100%%;height:%dpx;border:1px solid #ddd;border-radius:4px;", se$height %||% 400))
          )
        })
      )
    }))
  })

  # ─── AI 对话 iframe 动态渲染 ───
  output$ai_chat_frames <- renderUI({
    req(rv$logged_in)
    cl <- ai_chat_list()
    enabled <- Filter(function(x) isTRUE(x$enabled), cl)
    if (length(enabled) == 0)
      return(tags$div(style = "text-align:center;padding:30px;color:#999;",
        "暂无可用 AI 工具，请在「配置」中添加"))

    min_w <- if (length(enabled) >= 3) "30%" else if (length(enabled) == 2) "45%" else "100%"

    tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
      lapply(enabled, function(ct) {
        frame_id <- paste0("ai_chat_", ct$id)
        tags$div(style = sprintf("flex:1; min-width:%s;", min_w),
          tags$b(icon(ct$icon %||% "robot"), " ", ct$name), tags$br(),
          tags$iframe(id = frame_id, style = sprintf("width:100%%;height:%dpx;border:1px solid #ddd;border-radius:4px;", ct$height %||% 500))
        )
      })
    )
  })

  # ─── 全网搜索：批量设置 iframe src ───
  observeEvent(input$ai_search_btn, {
    req(rv$logged_in)
    kw <- trimws(input$ai_search_kw %||% "")
    if (nchar(kw) == 0) { showNotification("请输入搜索内容", type = "warning"); return() }
    ekw <- URLencode(kw, reserved = TRUE)
    sl <- ai_search_list()
    jscalls <- sapply(sl, function(se) {
      if (!isTRUE(se$enabled)) return("")
      frame_id <- paste0("ai_search_", se$id)
      url <- gsub("{query}", ekw, se$url, fixed = TRUE)
      sprintf("var f=document.getElementById('%s');if(f)f.src='%s';", frame_id, url)
    })
    js <- paste(jscalls[nchar(jscalls) > 0], collapse = "\n")
    if (nchar(js) > 0) session$sendCustomMessage("runjs", js)
  })

  # ─── 全网AI：批量设置 iframe src ───
  observeEvent(input$ai_chat_btn, {
    req(rv$logged_in)
    txt <- trimws(input$ai_chat_input %||% "")
    if (nchar(txt) == 0) { showNotification("请输入问题", type = "warning"); return() }
    etxt <- URLencode(txt, reserved = TRUE)
    cl <- ai_chat_list()
    jscalls <- sapply(cl, function(ct) {
      if (!isTRUE(ct$enabled)) return("")
      frame_id <- paste0("ai_chat_", ct$id)
      url <- gsub("{query}", etxt, ct$url, fixed = TRUE)
      sprintf("var f=document.getElementById('%s');if(f)f.src='%s';", frame_id, url)
    })
    js <- paste(jscalls[nchar(jscalls) > 0], collapse = "\n")
    if (nchar(js) > 0) session$sendCustomMessage("runjs", js)
    showNotification("已加载 AI 对话窗口，请在各窗口内输入问题", type = "message", duration = 5)
  })

  # ═══════ 配置管理（Admin 专属） ═══════

  # ─── 搜索引擎配置 UI ───
  output$ai_search_config <- renderUI({
    req(rv$logged_in)
    sl <- ai_search_list()

    tagList(
      lapply(seq_along(sl), function(i) {
        se <- sl[[i]]
        tags$div(style = "display:flex; gap:8px; align-items:center; margin-bottom:6px; padding:8px; background:#f9f9f9; border-radius:6px;",
          tags$span(style = "font-size:18px; width:30px; text-align:center;", if (isTRUE(se$enabled)) "✅" else "❌"),
          textInput(paste0("ai_se_name_", i), NULL, value = se$name %||% "", width = "120px", placeholder = "名称"),
          textInput(paste0("ai_se_url_", i), NULL, value = se$url %||% "", width = "300px", placeholder = "URL模板，{query}=搜索词"),
          textInput(paste0("ai_se_icon_", i), NULL, value = se$icon %||% "search", width = "80px", placeholder = "图标"),
          tags$input(type = "number", id = paste0("ai_se_height_", i), value = se$height %||% 400,
            style = "width:70px; height:32px; padding:4px; border:1px solid #ccc; border-radius:4px;", placeholder = "高度"),
          tags$div(style = "margin:0 4px;",
            tags$input(type = "checkbox", id = paste0("ai_se_enabled_", i), checked = isTRUE(se$enabled),
              onchange = sprintf("Shiny.setInputValue('ai_se_toggle_%d',this.checked,{priority:'event'})", i))
          ),
          actionButton(paste0("ai_se_del_", i), NULL, icon = icon("trash"), class = "btn-danger btn-xs")
        )
      }),
      tags$div(style = "margin-top:8px;",
        actionButton("ai_se_add", "添加搜索引擎", icon = icon("plus"), class = "btn-info btn-sm"),
        actionButton("ai_se_save", "保存配置", icon = icon("save"), class = "btn-primary btn-sm")
      )
    )
  })

  # ─── AI 工具配置 UI ───
  output$ai_chat_config <- renderUI({
    req(rv$logged_in)
    cl <- ai_chat_list()

    tagList(
      lapply(seq_along(cl), function(i) {
        ct <- cl[[i]]
        tags$div(style = "display:flex; gap:8px; align-items:center; margin-bottom:6px; padding:8px; background:#f9f9f9; border-radius:6px;",
          tags$span(style = "font-size:18px; width:30px; text-align:center;", if (isTRUE(ct$enabled)) "✅" else "❌"),
          textInput(paste0("ai_ct_name_", i), NULL, value = ct$name %||% "", width = "120px", placeholder = "名称"),
          textInput(paste0("ai_ct_url_", i), NULL, value = ct$url %||% "", width = "300px", placeholder = "URL模板，{query}=问题"),
          textInput(paste0("ai_ct_icon_", i), NULL, value = ct$icon %||% "robot", width = "80px", placeholder = "图标"),
          tags$input(type = "number", id = paste0("ai_ct_height_", i), value = ct$height %||% 500,
            style = "width:70px; height:32px; padding:4px; border:1px solid #ccc; border-radius:4px;", placeholder = "高度"),
          tags$div(style = "margin:0 4px;",
            tags$input(type = "checkbox", id = paste0("ai_ct_enabled_", i), checked = isTRUE(ct$enabled),
              onchange = sprintf("Shiny.setInputValue('ai_ct_toggle_%d',this.checked,{priority:'event'})", i))
          ),
          actionButton(paste0("ai_ct_del_", i), NULL, icon = icon("trash"), class = "btn-danger btn-xs")
        )
      }),
      tags$div(style = "margin-top:8px;",
        actionButton("ai_ct_add", "添加 AI 工具", icon = icon("plus"), class = "btn-info btn-sm"),
        actionButton("ai_ct_save", "保存配置", icon = icon("save"), class = "btn-primary btn-sm")
      )
    )
  })

  # ─── 搜索引擎：添加 / 删除 / 保存 ───
  ai_se_counter <- reactiveVal(0)
  observeEvent(input$ai_se_add, {
    ai_se_counter(ai_se_counter() + 1)
    sl <- ai_search_list()
    new_item <- list(id = paste0("custom_", format(Sys.time(), "%H%M%S")),
      name = "", icon = "search", url = "", height = 400, enabled = TRUE)
    ai_save_search_engines(c(sl, list(new_item)))
    showNotification("已添加新搜索引擎，请填写配置后保存", type = "message")
  })

  observe({
    req(rv$logged_in)
    sl <- ai_search_list()
    lapply(seq_along(sl), function(i) {
      observeEvent(input[[paste0("ai_se_del_", i)]], {
        new_list <- sl[-i]
        ai_save_search_engines(if (length(new_list) == 0) list() else new_list)
        showNotification("已删除", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  observeEvent(input$ai_se_save, {
    req(rv$logged_in)
    sl <- ai_search_list()
    new_list <- lapply(seq_along(sl), function(i) {
      sl[[i]]$name <- input[[paste0("ai_se_name_", i)]] %||% sl[[i]]$name
      sl[[i]]$url <- input[[paste0("ai_se_url_", i)]] %||% sl[[i]]$url
      sl[[i]]$icon <- input[[paste0("ai_se_icon_", i)]] %||% sl[[i]]$icon
      sl[[i]]$height <- as.integer(input[[paste0("ai_se_height_", i)]] %||% sl[[i]]$height)
      if (is.na(sl[[i]]$height)) sl[[i]]$height <- 400
      sl[[i]]
    })
    result <- ai_save_search_engines(new_list)
    if (result$success) showNotification("搜索引擎配置已保存", type = "message")
    else showNotification(result$message, type = "error")
  })

  # ─── AI 工具：添加 / 删除 / 保存 ───
  ai_ct_counter <- reactiveVal(0)
  observeEvent(input$ai_ct_add, {
    ai_ct_counter(ai_ct_counter() + 1)
    cl <- ai_chat_list()
    new_item <- list(id = paste0("custom_", format(Sys.time(), "%H%M%S")),
      name = "", icon = "robot", url = "", height = 500, enabled = TRUE)
    ai_save_chat_tools(c(cl, list(new_item)))
    showNotification("已添加新 AI 工具，请填写配置后保存", type = "message")
  })

  observe({
    req(rv$logged_in)
    cl <- ai_chat_list()
    lapply(seq_along(cl), function(i) {
      observeEvent(input[[paste0("ai_ct_del_", i)]], {
        new_list <- cl[-i]
        ai_save_chat_tools(if (length(new_list) == 0) list() else new_list)
        showNotification("已删除", type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  observeEvent(input$ai_ct_save, {
    req(rv$logged_in)
    cl <- ai_chat_list()
    new_list <- lapply(seq_along(cl), function(i) {
      cl[[i]]$name <- input[[paste0("ai_ct_name_", i)]] %||% cl[[i]]$name
      cl[[i]]$url <- input[[paste0("ai_ct_url_", i)]] %||% cl[[i]]$url
      cl[[i]]$icon <- input[[paste0("ai_ct_icon_", i)]] %||% cl[[i]]$icon
      cl[[i]]$height <- as.integer(input[[paste0("ai_ct_height_", i)]] %||% cl[[i]]$height)
      if (is.na(cl[[i]]$height)) cl[[i]]$height <- 500
      cl[[i]]
    })
    result <- ai_save_chat_tools(new_list)
    if (result$success) showNotification("AI 工具配置已保存", type = "message")
    else showNotification(result$message, type = "error")
  })
}
