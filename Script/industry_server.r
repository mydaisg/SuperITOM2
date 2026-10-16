# 行业模块 Server（框架）
industry_server <- function(input, output, session, rv) {
  ind_trigger <- reactiveVal(0)

  # 情报列表
  output$ind_list <- renderUI({
    req(rv$logged_in); ind_trigger()
    kw <- trimws(input$ind_search %||% "")
    cat <- trimws(input$ind_filter_cat %||% "")
    items <- industry_get_all(category = cat, keyword = kw)
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无行业情报，点击「新增情报」开始（框架）"))
    }
    parts <- c()
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      cat_html <- if (!is.na(r$category) && nchar(r$category) > 0)
        sprintf('<span style="display:inline-block;padding:1px 8px;border-radius:10px;font-size:11px;background:#e8eaf6;color:#3949ab;margin-left:8px;">%s</span>', r$category) else ""
      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:10px;">
          <div style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</div>
          <b style="font-size:15px;color:#1a237e;">%s</b>%s
          <div style="font-size:12px;color:#555;margin-top:6px;">%s</div>
          <div style="font-size:11px;color:#999;margin-top:6px;">来源：%s · %s</div>
        </div>',
        r$intel_no, r$title, cat_html, substr(r$content %||% "", 1, 200),
        r$source %||% "", substr(r$created_at %||% "", 1, 16)))
    }
    HTML(paste(parts, collapse = ""))
  })

  # 新增情报弹窗
  observeEvent(input$ind_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新增行业情报",
      textInput("ind_edit_title", "标题", width = "100%"),
      selectInput("ind_edit_cat", "分类", choices = industry_get_categories(), width = "100%"),
      textInput("ind_edit_source", "来源", width = "100%", placeholder = "如：公司官网、行业报告..."),
      textInput("ind_edit_url", "原文链接", width = "100%", placeholder = "https://...（可选）"),
      textAreaInput("ind_edit_content", "内容", rows = 6, width = "100%"),
      footer = tagList(
        actionButton("ind_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$ind_save_btn, {
    req(rv$logged_in, input$ind_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- industry_add(input$ind_edit_title, input$ind_edit_content,
      input$ind_edit_cat, input$ind_edit_source, input$ind_edit_url, uid)
    if (result$success) {
      removeModal()
      ind_trigger(ind_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  outputOptions(output, "ind_list", suspendWhenHidden = FALSE)

  # ══════════ Idea ══════════
  idea_trigger <- reactiveVal(0)
  idea_expand <- reactiveVal(0L)

  # 统计
  output$idea_stats <- renderText({
    req(rv$logged_in); idea_trigger()
    s <- idea_get_stats()
    n <- s$total %||% 0
    if (n == 0) return("<span style='font-size:12px;color:#888;'>暂无 Idea</span>")
    by <- s$by_status
    breakdown <- ""
    if (!is.null(by) && nrow(by) > 0 && "status" %in% names(by)) {
      breakdown <- paste(sprintf("%s %d", sapply(by$status, idea_status_label), by$n), collapse = " · ")
    }
    sprintf("<span style='font-size:12px;color:#888;'>共 %d 条 Idea%s%s</span>",
      n, if (nzchar(breakdown)) " · " else "", breakdown)
  })

  # 列表
  output$idea_list <- renderUI({
    req(rv$logged_in); idea_trigger(); idea_expand()
    items <- idea_get_all()
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无 Idea，点击「新增 Idea」开始记录你的想法"))
    }
    expanded_id <- idea_expand()
    parts <- c()
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      is_open <- (expanded_id == r$id)
      st <- r$status %||% "idea"
      status_html <- sprintf('<span style="display:inline-block;padding:1px 8px;border-radius:10px;font-size:11px;background:%s;color:#fff;margin-left:8px;">%s</span>',
        idea_status_color(st), idea_status_label(st))
      ver_html <- sprintf('<span style="font-size:11px;color:#999;margin-left:8px;">v%d · 共%d版</span>',
        r$current_version %||% 1, r$version_count %||% 0)
      rel_html <- if (!is.na(r$related_project) && nzchar(r$related_project))
        sprintf('<span style="font-size:11px;color:#6a1b9a;margin-left:8px;">项目：%s</span>', r$related_project) else ""
      cat_html <- if (!is.na(r$category) && nzchar(r$category))
        sprintf('<span style="font-size:11px;color:#999;margin-left:8px;">%s</span>', r$category) else ""

      detail_html <- ""
      if (is_open) {
        vers <- idea_get_versions(r$id)
        ver_parts <- c()
        if (!is.null(vers) && nrow(vers) > 0) {
          for (j in seq_len(nrow(vers))) {
            v <- vers[j, ]
            cnt <- gsub("<", "&lt;", v$content %||% "")
            cnt <- gsub(">", "&gt;", cnt)
            change <- if (!is.na(v$change_note) && nzchar(v$change_note))
              sprintf('<div style="font-size:11px;color:#888;margin-top:2px;">变更说明：%s</div>', v$change_note) else ""
            ver_parts <- c(ver_parts, sprintf(
              '<div style="border-left:3px solid #e0e0e0;padding:8px 12px;margin-bottom:8px;background:#fafafa;">
                <div style="font-size:11px;color:#888;"><b>v%d</b> · %s</div>
                <div style="font-size:12px;color:#555;white-space:pre-wrap;line-height:1.6;margin-top:4px;">%s</div>%s
              </div>',
              v$version, substr(v$created_at %||% "", 1, 16), cnt, change))
          }
        }
        detail_html <- sprintf(
          '<div style="margin-top:12px;padding:14px;border-top:2px solid #337ab7;background:#fafafa;border-radius:0 0 8px 8px;">
            <div style="font-size:12px;color:#888;margin-bottom:10px;display:flex;gap:18px;flex-wrap:wrap;">
              <span><b>编号：</b><span style="font-family:Consolas,monospace;">%s</span></span>
              <span><b>状态：</b>%s</span>
              <span><b>更新时间：</b>%s</span>
            </div>
            <div style="font-weight:600;color:#337ab7;margin-bottom:6px;">版本历史</div>
            %s
          </div>',
          r$idea_no, idea_status_label(st), substr(r$updated_at %||% r$created_at, 1, 16),
          paste(ver_parts, collapse = ""))
      }

      arrow <- if (is_open) '<span style="font-size:11px;color:#337ab7;margin-left:4px;">▴</span>' else '<span style="font-size:11px;color:#999;margin-left:4px;">▾</span>'

      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:10px;">
          <div style="display:flex;justify-content:space-between;align-items:center;">
            <div style="cursor:pointer;" onclick="Shiny.setInputValue(\'idea_expand_click\',%d,{priority:\'event\'});">
              <span style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</span>
              <b style="font-size:15px;margin-left:8px;color:#1a237e;">%s</b>%s%s%s%s%s
            </div>
            <div style="display:flex;gap:4px;flex-shrink:0;">
              <button class="btn btn-xs btn-info" title="新增版本" onclick="Shiny.setInputValue(\'idea_version_click\',%d,{priority:\'event\'});">＋版本</button>
              <button class="btn btn-xs btn-success" title="转方案" onclick="if(confirm(\'将最新版本转为方案？\'))Shiny.setInputValue(\'idea_to_sol_click\',%d,{priority:\'event\'});">💡转方案</button>
              <button class="btn btn-xs btn-dark" title="删除" onclick="if(confirm(\'确认删除此 Idea？\'))Shiny.setInputValue(\'idea_delete_click\',%d,{priority:\'event\'});">🗑</button>
            </div>
          </div>
          %s
        </div>',
        r$id, r$idea_no, r$title, status_html, ver_html, rel_html, cat_html, arrow,
        r$id, r$id, r$id, detail_html))
    }
    HTML(paste(parts, collapse = ""))
  })

  # 新增 Idea 弹窗
  observeEvent(input$idea_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新增 Idea",
      textInput("idea_edit_title", "标题", width = "100%"),
      textInput("idea_edit_cat", "分类", width = "100%", placeholder = "如：产品、工具、子项目..."),
      selectInput("idea_edit_status", "状态", choices = idea_status_choices(), width = "100%"),
      textInput("idea_edit_rel", "关联项目", width = "100%", placeholder = "如：HungFo（可选）"),
      textAreaInput("idea_edit_content", "内容", rows = 8, width = "100%", placeholder = "记录你的 Idea..."),
      footer = tagList(
        actionButton("idea_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$idea_save_btn, {
    req(rv$logged_in, input$idea_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- idea_add(input$idea_edit_title, input$idea_edit_content,
      input$idea_edit_cat, input$idea_edit_rel, input$idea_edit_status, uid)
    if (result$success) {
      removeModal()
      idea_trigger(idea_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 展开/收起
  observeEvent(input$idea_expand_click, {
    req(rv$logged_in, input$idea_expand_click)
    id <- as.integer(input$idea_expand_click)
    idea_expand(if (idea_expand() == id) 0L else id)
  })

  # 新增版本弹窗
  observeEvent(input$idea_version_click, {
    req(rv$logged_in)
    idea <- idea_get_by_id(as.integer(input$idea_version_click))
    if (is.null(idea) || nrow(idea) == 0) return()
    idea <- idea[1, ]
    latest <- ""
    vers <- idea_get_versions(idea$id)
    if (!is.null(vers) && nrow(vers) > 0) latest <- vers$content[1] %||% ""
    showModal(modalDialog(
      title = paste("新增版本 -", idea$idea_no, "（当前 v", idea$current_version, "）"),
      textInput("idea_ver_title", "标题", value = idea$title, width = "100%"),
      textAreaInput("idea_ver_content", "内容", value = latest, rows = 8, width = "100%"),
      textInput("idea_ver_change", "变更说明", width = "100%", placeholder = "本次更新了什么..."),
      footer = tagList(
        actionButton("idea_version_save_btn", "保存新版本", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$idea_version_save_btn, {
    req(rv$logged_in, input$idea_version_click)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- idea_add_version(as.integer(input$idea_version_click), input$idea_ver_content,
      input$idea_ver_title, input$idea_ver_change, uid)
    if (result$success) {
      removeModal()
      idea_expand(as.integer(input$idea_version_click))
      idea_trigger(idea_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 转方案
  observeEvent(input$idea_to_sol_click, {
    req(rv$logged_in)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- idea_to_solution(as.integer(input$idea_to_sol_click), uid)
    if (result$success) {
      idea_trigger(idea_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 删除
  observeEvent(input$idea_delete_click, {
    req(rv$logged_in)
    result <- idea_delete(as.integer(input$idea_delete_click))
    if (result$success) {
      idea_expand(0L)
      idea_trigger(idea_trigger() + 1)
      showNotification("已删除", type = "message")
    } else showNotification(result$message, type = "error")
  })

  outputOptions(output, "idea_list", suspendWhenHidden = FALSE)
  outputOptions(output, "idea_stats", suspendWhenHidden = FALSE)
}
