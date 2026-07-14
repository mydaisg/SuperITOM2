# 方案模块 Server
solution_server <- function(input, output, session, rv) {
  sol_trigger <- reactiveVal(0)
  sol_expand <- reactiveVal(0L)   # 当前展开的方案ID，0=全部收起

  # 统计
  output$sol_stats <- renderUI({
    req(rv$logged_in); sol_trigger()
    s <- tryCatch(solution_get_all(), error = function(e) data.frame())
    tags$div(style = "font-size:12px; color:#888;",
      sprintf("共 %d 条方案", nrow(s))
    )
  })

  # 列表 + 内联详情
  output$sol_list <- renderUI({
    req(rv$logged_in); sol_trigger(); sol_expand()
    kw <- trimws(input$sol_search %||% "")
    cat <- trimws(input$sol_filter_cat %||% "")
    items <- tryCatch(solution_get_all(), error = function(e) data.frame())
    if (kw != "") {
      items <- items[grepl(kw, items$title, ignore.case = TRUE) | grepl(kw, items$content %||% "", ignore.case = TRUE), , drop = FALSE]
    }
    if (cat != "") {
      items <- items[items$category == cat, , drop = FALSE]
    }
    if (nrow(items) == 0) return(tags$div(style = "text-align:center; padding:40px; color:#999;", "暂无方案，点击「新建方案」开始"))

    expanded_id <- sol_expand()

    lapply(seq_len(nrow(items)), function(i) {
      r <- items[i,]
      is_open <- (expanded_id == -1L || expanded_id == r$id)
      cat_tag <- if (!is.na(r$category) && nchar(r$category) > 0) {
        tags$span(style = "display:inline-block; padding:1px 8px; border-radius:10px; font-size:11px; background:#e8f5e9; color:#2e7d32; margin-left:8px;", r$category)
      } else ""
      # 预览内容：去除 script/style 标签及内容 + 去除所有 HTML 标签
      preview_text <- if (!is.na(r$content) && nchar(r$content) > 0) {
        txt <- r$content
        txt <- gsub("(?s)<script[^>]*>.*?</script>", "", txt, perl = TRUE, ignore.case = TRUE)
        txt <- gsub("(?s)<style[^>]*>.*?</style>", "", txt, perl = TRUE, ignore.case = TRUE)
        txt <- gsub("<[^>]+>", "", txt)
        txt <- gsub("\\s+", " ", txt)
        trimws(txt)
      } else ""

      toggle_js <- sprintf("Shiny.setInputValue('sol_expand_click',%d,{priority:'event'});", r$id)

      # 详情区
      detail_panel <- if (is_open) {
        cleaned <- sol_sanitize_html(r$content %||% "")
        tags$div(style = "margin-top:12px; padding:16px; border-top:2px solid #337ab7; background:#fafafa; border-radius:0 0 8px 8px; overflow-x:auto;",
          # 元信息行
          tags$div(style = "font-size:12px; color:#888; margin-bottom:12px; display:flex; gap:20px; flex-wrap:wrap;",
            tags$span(tags$b("编号："), tags$span(style="font-family:Consolas,monospace;", r$sol_no)),
            tags$span(tags$b("分类："), r$category %||% "未分类"),
            tags$span(tags$b("关联项目："), r$related_project %||% "无"),
            tags$span(tags$b("创建人："), r$creator_name %||% ""),
            tags$span(tags$b("更新时间："), substr(r$updated_at %||% r$created_at, 1, 16))
          ),
          # CSS 通过 <style> 标签注入（Shiny 外层的 style 标签有效）
          if (nchar(cleaned$styles) > 0) tags$style(HTML(cleaned$styles)),
          # HTML 内容
          tags$div(class = "sol-detail-content", style = "line-height:1.7;", HTML(cleaned$body)),
          # 底部操作
          tags$div(style = "margin-top:14px; padding-top:10px; border-top:1px dashed #ddd; display:flex; gap:6px;",
            tags$button(class = "btn btn-sm btn-primary", "✏ 编辑",
              onclick = sprintf("Shiny.setInputValue('sol_edit_click',%d,{priority:'event'});", r$id)),
            tags$button(class = "btn btn-sm btn-default", "📁 收起",
              onclick = sprintf("Shiny.setInputValue('sol_expand_click',%d,{priority:'event'});", r$id)),
            tags$button(class = "btn btn-sm btn-info", "📂 展开全部",
              onclick = "var bd=this.parentElement.parentElement.querySelectorAll('.sec-bd');for(var i=0;i<bd.length;i++)bd[i].classList.add('open');")
          )
        )
      }

      tags$div(style = "background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:14px; margin-bottom:10px;",
        # 标题行
        tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
          tags$div(style = "cursor:pointer;", onclick = toggle_js,
            tags$span(style = "font-size:11px; color:#888; font-family:Consolas,monospace;", r$sol_no),
            tags$b(style = "font-size:15px; margin-left:8px; color:#337ab7;", r$title),
            cat_tag,
            if (is_open) tags$span(style = "font-size:11px; color:#337ab7; margin-left:6px;", "▴") else tags$span(style = "font-size:11px; color:#999; margin-left:6px;", "▾")
          ),
          tags$div(style = "display:flex; gap:4px; flex-shrink:0;",
            tags$button(class = "btn btn-xs btn-info", "✏", title = "编辑",
              onclick = sprintf("Shiny.setInputValue('sol_edit_click',%d,{priority:'event'});", r$id)),
            tags$button(class = "btn btn-xs btn-danger", "🗑", title = "删除",
              onclick = sprintf("if(confirm('确认删除此方案？'))Shiny.setInputValue('sol_delete_click',%d,{priority:'event'});", r$id))
          )
        ),
        # 预览（收起时显示）
        if (!is_open && nchar(preview_text) > 0) {
          tags$div(style = "font-size:12px; color:#555; margin-top:6px; white-space:pre-wrap; line-height:1.5;",
            substr(preview_text, 1, 300), if (nchar(preview_text) > 300) "...")
        },
        # 内联详情
        detail_panel
      )
    }) -> cards
    tagList(cards)
  })

  # 点击标题 → 展开/收起
  observeEvent(input$sol_expand_click, {
    req(rv$logged_in, input$sol_expand_click)
    id <- as.integer(input$sol_expand_click)
    sol_expand(if (sol_expand() == id) 0L else id)
  })

  # 全部展开
  observeEvent(input$sol_expand_all, {
    req(rv$logged_in)
    sol_expand(-1L)  # -1 表示全部展开
  })

  # 全部收起
  observeEvent(input$sol_collapse_all, {
    req(rv$logged_in)
    sol_expand(0L)
  })

  # 更新分类下拉
  observe({
    req(rv$logged_in); sol_trigger()
    items <- tryCatch(solution_get_all(), error = function(e) data.frame())
    cats <- unique(items$category)
    cats <- cats[!is.na(cats) & cats != ""]
    updateSelectInput(session, "sol_filter_cat", choices = c("全部分类" = "", cats))
  })

  # 新建弹窗
  observeEvent(input$sol_create_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新建方案",
      textInput("sol_edit_title", "方案标题", width = "100%"),
      textInput("sol_edit_cat", "分类", width = "100%", placeholder = "如：IT基础设施、信息安全..."),
      textAreaInput("sol_edit_content", "方案内容", rows = 10, width = "100%"),
      textInput("sol_edit_rel", "关联项目号", width = "100%", placeholder = "PRJ20260101...（可选）"),
      footer = tagList(
        actionButton("sol_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$sol_save_btn, {
    req(rv$logged_in, input$sol_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- solution_add(input$sol_edit_title, input$sol_edit_content,
      input$sol_edit_cat, input$sol_edit_rel, uid)
    if (result$success) {
      removeModal()
      sol_trigger(sol_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 编辑弹窗
  observeEvent(input$sol_edit_click, {
    req(rv$logged_in)
    sol <- tryCatch({
      con <- db_connect()
      on.exit(db_disconnect(con))
      dbGetQuery(con, sprintf("SELECT * FROM solutions WHERE id=%d", as.integer(input$sol_edit_click)))
    }, error = function(e) NULL)
    if (is.null(sol) || nrow(sol) == 0) return()
    sol <- sol[1,]
    showModal(modalDialog(
      title = paste("编辑方案", sol$sol_no),
      textInput("sol_edit_title_m", "方案标题", value = sol$title, width = "100%"),
      textInput("sol_edit_cat_m", "分类", value = sol$category %||% "", width = "100%"),
      textAreaInput("sol_edit_content_m", "方案内容", value = sol$content %||% "", rows = 10, width = "100%"),
      textInput("sol_edit_rel_m", "关联项目号", value = sol$related_project %||% "", width = "100%"),
      footer = tagList(
        actionButton("sol_update_btn", "更新", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$sol_update_btn, {
    req(rv$logged_in, input$sol_edit_title_m)
    id <- as.integer(input$sol_edit_click)
    result <- solution_update(id, input$sol_edit_title_m, input$sol_edit_content_m,
      input$sol_edit_cat_m, input$sol_edit_rel_m)
    if (result$success) {
      removeModal()
      sol_trigger(sol_trigger() + 1)
      showNotification("已更新", type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 删除
  observeEvent(input$sol_delete_click, {
    req(rv$logged_in)
    id <- as.integer(input$sol_delete_click)
    result <- solution_delete(id)
    if (result$success) {
      sol_expand(0L)
      sol_trigger(sol_trigger() + 1)
      showNotification("已删除", type = "message")
    }
  })
}


