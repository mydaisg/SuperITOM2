# 记事模块 - 服务端 (v2: Trello 看板)

note_server <- function(input, output, session, rv) {
  
  note_trigger <- reactiveVal(0)
  
  ##################
  # Trello 看板
  ##################
  output$note_board <- renderUI({
    note_trigger()
    req(rv$logged_in)
    items <- note_get_all()
    
    build_col <- function(status, label, items) {
      subset <- if (nrow(items) > 0) items[items$status == status, ] else items
      cards <- list()
      if (nrow(subset) > 0) {
        for (i in 1:nrow(subset)) {
          r <- subset[i, ]
          # 重要性小旗
          flags <- sapply(1:5, function(f) {
            cls <- if (f <= (r$importance %||% 0)) "note-flag active" else "note-flag"
            sprintf('<a class="%s" data-id="%d">🚩</a>', cls, r$id)
          }) |> paste(collapse="")
          
          # 时间信息
          created <- if (!is.na(r$created_at) && nchar(r$created_at) > 10) substr(r$created_at, 1, 16) else r$created_at
          reminder <- if (!is.na(r$reminder_at) && nchar(r$reminder_at) > 10) substr(r$reminder_at, 1, 16) else ""
          due <- if (!is.na(r$due_at) && nchar(r$due_at) > 10) substr(r$due_at, 1, 16) else ""
          
          # 是否逾期
          due_cls <- ""
          if (!is.na(r$due_at) && r$due_at != "") {
            due_dt <- tryCatch(as.POSIXct(r$due_at), error = function(e) NULL)
            if (!is.null(due_dt) && due_dt < Sys.time()) due_cls <- "note-due-overdue"
          }
          
          # 内容预览（去掉首行标题）
          body <- r$content %||% ""
          lines <- strsplit(body, "\n")[[1]]
          if (length(lines) > 1) {
            body <- paste(lines[-1], collapse = "\n")
          } else {
            body <- ""
          }
          if (nchar(body) > 120) body <- paste0(substr(body, 1, 120), "...")
          
          # 最后一条评论
          last_comment <- note_comment_get_last(r$id)
          comment_html <- ""
          if (!is.null(last_comment) && nrow(last_comment) > 0) {
            ct <- last_comment$content[1]
            cn <- last_comment$creator_name[1] %||% "匿名"
            if (nchar(ct) > 60) ct <- paste0(substr(ct, 1, 60), "...")
            comment_html <- tags$div(style="font-size:11px; color:#5e6c84; margin-top:6px; padding:4px 6px; background:#f4f5f7; border-radius:3px;",
              tags$span(style="color:#999;", cn, ": "), tags$span(ct))
          }
          
          cards[[i]] <- tags$div(class = "note-card", `data-id` = r$id,
            tags$div(class = "note-title", HTML(flags), " ", r$title),
            if (nchar(body) > 0) tags$div(class = "note-body", body) else "",
            tags$div(class = "note-meta",
              tags$span(icon("clock"), created),
              if (reminder != "") tags$span(icon("bell"), reminder),
              if (due != "") tags$span(class = due_cls, icon("calendar-check"), due)
            ),
            comment_html,
            tags$div(class = "note-actions",
              tags$button(class = "btn btn-success btn-xs note-wo-btn", `data-id` = r$id, "📋转工单"),
              tags$button(class = "btn btn-warning btn-xs note-report-btn", `data-id` = r$id, "📅日报"),
              tags$button(class = "btn btn-danger btn-xs note-del-btn", `data-id` = r$id, "🗑删除")
            )
          )
        }
      }
      do.call(tagList, cards)
    }
    
    tags$div(class = "trello-board",
      tags$div(class = "trello-col pending",
        tags$h4(sprintf("📋 待处理 (%d)", sum(items$status == "pending", na.rm = TRUE))),
        build_col("pending", "待处理", items)
      ),
      tags$div(class = "trello-col active",
        tags$h4(sprintf("🔄 进行中 (%d)", sum(items$status == "in_progress", na.rm = TRUE))),
        build_col("in_progress", "进行中", items)
      ),
      tags$div(class = "trello-col done",
        tags$h4(sprintf("✅ 已完成 (%d)", sum(items$status == "completed", na.rm = TRUE))),
        build_col("completed", "已完成", items)
      )
    )
  })
  
  ##################
  # 初始化时间默认值（从 system_config 读取）
  ##################
  observe({
    req(rv$logged_in)
    updateNumericInput(session, "note_reminder_hours", 
      value = as.numeric(config_get_value("note_reminder_hours", "3")))
    updateNumericInput(session, "note_due_hour", 
      value = as.integer(config_get_value("note_due_hour", "18")))
  })

  ##################
  # 添加记事
  ##################
  observeEvent(input$note_add, {
    req(rv$logged_in, input$note_new_text)
    if (trimws(input$note_new_text) == "") return()
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- note_add(input$note_new_text, uid,
      reminder_hours = input$note_reminder_hours,
      due_hour = input$note_due_hour)
    if (result$success) updateTextAreaInput(session, "note_new_text", value = "")
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  
  ##################
  # 移动状态（弹窗中用，卡片按钮已移除）
  ##################
  observeEvent(input$note_move_click, {
    req(rv$logged_in)
    result <- note_patch(as.integer(input$note_move_click$id), status = input$note_move_click$to)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 2)
  })

  ##################
  # 编辑弹窗 v3：默认只读 + 修改按钮切换 + ID/状态/小旗 + 彩虹评论
  ##################
  observeEvent(input$note_edit_click, {
    req(rv$logged_in)
    note <- note_get_by_id(as.integer(input$note_edit_click))
    if (is.null(note) || nrow(note) == 0) return()
    rv$note_edit_id <- note$id[1]
    
    rem_val <- note$reminder_at[1] %||% ""
    due_val <- note$due_at[1] %||% ""
    if (nchar(rem_val) > 16) rem_val <- substr(rem_val, 1, 16)
    if (nchar(due_val) > 16) due_val <- substr(due_val, 1, 16)
    
    # 状态按钮
    st <- note$status[1]
    status_btns <- ""
    if (st == "pending") {
      status_btns <- sprintf('<button class="btn btn-info btn-sm note-move-btn" data-id="%d" data-to="in_progress">▶ 开始处理</button>', note$id[1])
    } else if (st == "in_progress") {
      status_btns <- sprintf('<button class="btn btn-success btn-sm note-move-btn" data-id="%d" data-to="completed">✓ 标记完成</button>
                              <button class="btn btn-warning btn-sm note-move-btn" data-id="%d" data-to="pending">◀ 退回待处理</button>', note$id[1], note$id[1])
    } else {
      status_btns <- sprintf('<button class="btn btn-warning btn-sm note-move-btn" data-id="%d" data-to="pending">◀ 重新打开</button>', note$id[1])
    }
    
    # 重要性小旗（可加减）
    imp <- note$importance[1] %||% 0
    flags_html <- ""
    for (fi in 1:5) {
      flags_html <- paste0(flags_html,
        sprintf('<a class="note-flag-btn" style="font-size:20px;cursor:pointer;text-decoration:none;margin-right:2px;" data-id="%d" data-val="%d">%s</a>',
          note$id[1], fi, if(fi <= imp) "🚩" else "🏳"))
    }
    
    # 彩虹色评论
    rainbow_colors <- c("#5bc0de","#f0ad4e","#5cb85c","#d9534f","#9370db","#337ab7","#f7a8b8","#a8d8ea")
    comments <- note_comment_get_all(note$id[1])
    comment_html <- ""
    if (!is.null(comments) && nrow(comments) > 0) {
      for (ci in 1:nrow(comments)) {
        c <- comments[ci, ]
        cn <- c$creator_name[1] %||% "匿名"
        ca <- if (!is.na(c$created_at) && nchar(c$created_at) > 10) substr(c$created_at, 1, 16) else c$created_at
        clr <- rainbow_colors[((ci - 1) %% length(rainbow_colors)) + 1]
        comment_html <- paste0(comment_html, sprintf(
          '<div style="background:#fafafa; padding:8px 12px; margin-bottom:6px; border-radius:6px; border-left:4px solid %s;">
            <div style="font-size:11px; color:#999; margin-bottom:4px;">
              <span style="font-weight:bold; color:%s;">%s</span>
              <span style="margin-left:8px;">%s</span>
            </div>
            <div style="font-size:13px; line-height:1.5; white-space:pre-wrap;">%s</div>
          </div>', clr, clr, cn, ca, c$content))
      }
    }
    
    modal_body <- tagList(
      # ID + 状态 + 小旗
      tags$div(style="background:#f5f5f5; padding:10px; border-radius:6px; margin-bottom:10px;",
        tags$div(style="display:flex; justify-content:space-between; align-items:center;",
          tags$span(style="font-size:12px; color:#999;", sprintf("ID: %d | 状态: %s | 创建: %s", note$id[1], st, substr(note$created_at[1] %||% "", 1, 16))),
          HTML(flags_html)
        ),
        tags$div(style="margin-top:6px;", HTML(status_btns))
      ),
      
      # 内容区（只读）
      tags$div(id = "note_content_ro",
        tags$div(style="font-size:16px; font-weight:bold; margin-bottom:8px;", note$title[1]),
        tags$div(style="font-size:13px; color:#333; white-space:pre-wrap; line-height:1.6; max-height:200px; overflow-y:auto;", note$content[1] %||% ""),
        tags$div(style="font-size:12px; color:#999; margin-top:6px;", 
                 sprintf("⏰ 提醒: %s  |  📅 到期: %s", rem_val, due_val))
      ),
      
      # 编辑区（初始隐藏）
      tags$div(id = "note_content_ed", style="display:none;",
        textAreaInput("note_edit_content_m", "内容（首行为标题）", rows = 4, value = note$content[1] %||% ""),
        fluidRow(
          column(6, textInput("note_edit_reminder_m", "⏰ 提醒时间", placeholder = "YYYY-MM-DD HH:MM", value = rem_val)),
          column(6, textInput("note_edit_due_m", "📅 到期时间", placeholder = "YYYY-MM-DD HH:MM", value = due_val))
        )
      ),
      
      # 修改/保存切换按钮
      tags$div(style="margin-top:10px;",
        actionButton("note_toggle_edit", "✏ 修改", class = "btn-warning btn-sm", icon = icon("edit")),
        actionButton("note_cancel_edit", "取消修改", class = "btn-default btn-sm", style = "display:none;"),
        actionButton("note_do_save", "💾 保存", class = "btn-primary btn-sm", style = "display:none;")
      ),
      
      tags$hr(),
      
      # 评论历史
      if (nchar(comment_html) > 0) tags$div(
        style = "max-height:200px; overflow-y:auto; margin-bottom:8px;",
        tags$h5("💬 评论"),
        HTML(comment_html)
      ) else tags$p(style="color:#999; font-size:12px;", "💬 暂无评论"),
      
      # 评论输入（最底部）
      textAreaInput("note_comment_new_m", NULL, rows = 2, placeholder = "添加评论..."),
      actionButton("note_add_comment_m", "发表评论", class = "btn-info btn-sm")
    )
    
    showModal(modalDialog(
      title = tags$span("记事详情 #", note$id[1]),
      size = "l",
      modal_body,
      footer = modalButton("关闭"),
      easyClose = TRUE
    ))
  })
  
  # 切换编辑模式（用 sendCustomMessage 替代 runjs，不依赖 shinyjs）
  observeEvent(input$note_toggle_edit, {
    session$sendCustomMessage(type = "noteEditMode", message = list(mode = "edit"))
  })
  observeEvent(input$note_cancel_edit, {
    session$sendCustomMessage(type = "noteEditMode", message = list(mode = "view"))
  })
  
  # 保存修改
  observeEvent(input$note_do_save, {
    req(rv$logged_in, rv$note_edit_id, input$note_edit_content_m)
    lines <- strsplit(trimws(input$note_edit_content_m), "\n")[[1]]
    title <- if (length(lines) > 0) lines[1] else "未命名"
    result <- note_update(rv$note_edit_id,
      title = title, content = input$note_edit_content_m,
      reminder_at = if (trimws(input$note_edit_reminder_m) != "") input$note_edit_reminder_m else NULL,
      due_at = if (trimws(input$note_edit_due_m) != "") input$note_edit_due_m else NULL)
    removeModal()
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  
  # 小旗设置（弹窗中用）
  observeEvent(input$note_flag_click, {
    req(rv$logged_in)
    id <- as.integer(input$note_flag_click)
    note <- note_get_by_id(id)
    if (is.null(note)) return()
    imp <- (note$importance[1] %||% 0) + 1
    if (imp > 5) imp <- 0
    result <- note_patch(id, importance = imp)
    note_trigger(note_trigger() + 1)
  })
  
  # 弹窗内小旗（可加减，通过 action 参数）
  # 点击卡片上的 🚩/🏳 循环递增；弹窗内的 🚩/🏳 直接设置
  observeEvent(input$note_flag_set, {
    req(rv$logged_in)
    parts <- strsplit(as.character(input$note_flag_set), ":")[[1]]
    id <- as.integer(parts[1]); val <- as.integer(parts[2])
    result <- note_patch(id, importance = val)
    note_trigger(note_trigger() + 1)
  })
  
  # 弹窗内评论
  observeEvent(input$note_add_comment_m, {
    req(rv$logged_in, rv$note_edit_id, input$note_comment_new_m)
    if (trimws(input$note_comment_new_m) == "") return()
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    note_comment_add(rv$note_edit_id, input$note_comment_new_m, uid)
    updateTextAreaInput(session, "note_comment_new_m", value = "")
    note_trigger(note_trigger() + 1)
    # 关弹窗提示，卡片已刷新（显示最后评论）
    removeModal()
    showNotification("评论已添加", type = "message", duration = 2)
  })
  
  ##################
  # 转工单
  ##################
  observeEvent(input$note_to_wo_click, {
    req(rv$logged_in)
    result <- note_convert_to_work_order(as.integer(input$note_to_wo_click), rv$current_user)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  
  ##################
  # 记日报
  ##################
  observeEvent(input$note_report_click, {
    req(rv$logged_in)
    result <- note_patch(as.integer(input$note_report_click), reported_to_daily = 1)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 2)
  })
  
  ##################
  # 删除
  ##################
  observeEvent(input$note_del_click, {
    req(rv$logged_in)
    result <- note_delete(as.integer(input$note_del_click))
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
}
