# 记事模块 - 服务端 v4 (编号 + footer按钮 + 评论编辑/删除)

note_server <- function(input, output, session, rv) {
  
  # 补充旧数据缺失的 note_no
  tryCatch({ note_fill_missing_no() }, error = function(e) message("[NOTE-INIT] 补充编号失败: ", e$message))
  
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
          imp_card <- r$importance %||% 0
          if (imp_card > 0) {
            flags <- sprintf('<span class="note-importance" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击切换重要性 (当前: %d)">%s</span>',
              r$id, imp_card, paste(rep("🚩", imp_card), collapse = ""))
          } else {
            flags <- sprintf('<span class="note-importance-empty" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击添加重要性">🏳</span>', r$id)
          }
          
          created <- if (!is.na(r$created_at) && nchar(r$created_at) > 10) substr(r$created_at, 1, 16) else r$created_at
          reminder <- if (!is.na(r$reminder_at) && nchar(r$reminder_at) > 10) substr(r$reminder_at, 1, 16) else ""
          due <- if (!is.na(r$due_at) && nchar(r$due_at) > 10) substr(r$due_at, 1, 16) else ""
          note_no <- r$note_no[1] %||% ""
          
          due_cls <- ""
          if (!is.na(r$due_at) && r$due_at != "") {
            due_dt <- tryCatch(as.POSIXct(r$due_at), error = function(e) NULL)
            if (!is.null(due_dt) && due_dt < Sys.time()) due_cls <- "note-due-overdue"
          }
          
          body <- r$content %||% ""
          body_lines <- strsplit(body, "\n")[[1]]
          if (length(body_lines) > 1) body <- paste(body_lines[-1], collapse = "\n") else body <- ""
          if (nchar(body) > 100) body <- paste0(substr(body, 1, 100), "...")
          
          last_comment <- note_comment_get_last(r$id)
          comment_html <- ""
          if (!is.null(last_comment) && nrow(last_comment) > 0) {
            ct <- last_comment$content[1]; cn <- last_comment$creator_name[1] %||% "匿名"
            if (isTRUE(nchar(ct) > 50)) ct <- paste0(substr(ct, 1, 50), "...")
            comment_html <- as.character(tags$div(style="font-size:11px; color:#5e6c84; margin-top:6px; padding:4px 6px; background:#f4f5f7; border-radius:3px;",
              tags$span(style="color:#999;", cn, ": "), tags$span(ct)))
          }
          
          cards[[i]] <- tags$div(class = "note-card", `data-id` = r$id,
            tags$div(class = "note-title",
              if (isTRUE(note_no != "")) tags$span(style="color:#337ab7;font-size:11px;margin-right:6px;", note_no),
              HTML(flags), " ", r$title),
            if (isTRUE(nchar(body) > 0)) tags$div(class = "note-body", body) else "",
            tags$div(class = "note-meta",
              tags$span(icon("clock"), created),
              if (isTRUE(reminder != "")) tags$span(icon("bell"), reminder),
              if (isTRUE(due != "")) tags$span(class = due_cls, icon("calendar-check"), due)
            ),
            HTML(comment_html)
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
  # 初始化时间默认值
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
  # 编辑弹窗 v4：评论不关 + footer按钮 + 编号 + 评论编辑/删除
  ##################
  observeEvent(input$note_edit_click, {
    req(rv$logged_in)
    note <- note_get_by_id(as.integer(input$note_edit_click))
    if (is.null(note) || nrow(note) == 0) return()
    rv$note_edit_id <- note$id[1]
    
    note_no <- note$note_no[1] %||% sprintf("NTE%s%03d", format(Sys.Date(),"%Y%m%d"), note$id[1])
    rem_val <- note$reminder_at[1] %||% ""
    due_val <- note$due_at[1] %||% ""
    if (nchar(rem_val) > 16) rem_val <- substr(rem_val, 1, 16)
    if (nchar(due_val) > 16) due_val <- substr(due_val, 1, 16)
    st <- note$status[1]; imp <- note$importance[1] %||% 0
    
    # 小旗：项目风格，直接点击切换
    if (imp > 0) {
      flags_html <- sprintf('<span class="note-importance" style="font-size:18px;cursor:pointer;" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击切换 (当前: %d)">%s</span>',
        note$id[1], imp, paste(rep("🚩", imp), collapse = ""))
    } else {
      flags_html <- sprintf('<span class="note-importance-empty" style="font-size:18px;cursor:pointer;" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击添加重要性">🏳</span>', note$id[1])
    }
    
    # 状态按钮 HTML（放 footer）
    status_btns <- ""
    if (st == "pending") {
      status_btns <- sprintf('<button class="btn btn-info btn-sm note-move-btn" data-id="%d" data-to="in_progress" style="margin-right:4px;">▶ 开始处理</button>', note$id[1])
    } else if (st == "in_progress") {
      status_btns <- sprintf('<button class="btn btn-success btn-sm note-move-btn" data-id="%d" data-to="completed" style="margin-right:4px;">✓ 完成</button>
                              <button class="btn btn-warning btn-sm note-move-btn" data-id="%d" data-to="pending" style="margin-right:4px;">◀ 退回</button>', note$id[1], note$id[1])
    } else {
      status_btns <- sprintf('<button class="btn btn-warning btn-sm note-move-btn" data-id="%d" data-to="pending" style="margin-right:4px;">◀ 重新打开</button>', note$id[1])
    }
    
    # 彩虹评论（每条带编辑/删除按钮，默认隐藏，修改模式显示）
    rainbow_colors <- c("#e74c3c","#e67e22","#f1c40f","#2ecc71","#1abc9c","#3498db","#9b59b6","#e91e63")
    comments <- note_comment_get_all(note$id[1])
    comment_html <- ""
    if (!is.null(comments) && nrow(comments) > 0) {
      for (ci in 1:nrow(comments)) {
        c <- comments[ci, ]
        cn <- c$creator_name[1] %||% "匿名"
        ca <- if (!is.na(c$created_at) && nchar(c$created_at) > 10) substr(c$created_at, 1, 16) else c$created_at
        clr <- rainbow_colors[((ci - 1) %% length(rainbow_colors)) + 1]
        comment_html <- paste0(comment_html, sprintf(
          '<div class="comment-item" id="comment-%d" style="background:#fafafa; padding:8px 12px; margin-bottom:6px; border-radius:6px; border-left:4px solid %s;">
            <div style="display:flex; justify-content:space-between; align-items:flex-start;">
              <div style="flex:1; min-width:0;">
                <div style="font-size:11px; color:#999; margin-bottom:4px;">
                  <span style="font-weight:bold; color:%s;">%s</span>
                  <span style="margin-left:8px;">%s</span>
                </div>
                <div class="comment-text" style="font-size:13px; line-height:1.5; white-space:pre-wrap; word-break:break-word;">%s</div>
                <div class="comment-edit-area" style="display:none; margin-top:4px;">
                  <textarea class="form-control comment-edit-input" style="font-size:13px;" rows="2">%s</textarea>
                  <button class="btn btn-xs btn-primary comment-save-btn" style="margin-top:4px;" data-id="%d">保存</button>
                  <button class="btn btn-xs btn-default comment-cancel-btn" style="margin-top:4px;">取消</button>
                </div>
              </div>
              <div class="comment-actions" style="display:none; margin-left:8px; white-space:nowrap; flex-shrink:0;">
                <button class="btn btn-xs btn-info comment-edit-btn" data-id="%d">✏</button>
                <button class="btn btn-xs btn-danger comment-del-btn" data-id="%d">🗑</button>
              </div>
            </div>
          </div>', c$id, clr, clr, cn, ca, c$content, c$content, c$id, c$id, c$id))
      }
    }
    
    modal_body <- tagList(
      # 编号 + 小旗
      tags$div(style="background:#f5f5f5; padding:10px; border-radius:6px; margin-bottom:10px;",
        tags$div(style="display:flex; justify-content:space-between; align-items:center;",
          tags$div(
            tags$b(style="color:#337ab7; font-size:15px;", note_no),
            tags$span(style="font-size:12px; color:#999; margin-left:10px;",
              sprintf("状态: %s | 创建: %s", st, substr(note$created_at[1] %||% "", 1, 16)))
          ),
          HTML(flags_html)
        )
      ),
      
      # 内容只读
      tags$div(id = "note_content_ro",
        tags$div(style="font-size:15px; font-weight:bold; margin-bottom:6px;", note$title[1]),
        tags$div(style="font-size:13px; color:#333; white-space:pre-wrap; line-height:1.6; max-height:150px; overflow-y:auto;",
          note$content[1] %||% ""),
        tags$div(style="font-size:12px; color:#999; margin-top:6px;",
          sprintf("⏰ 提醒: %s  |  📅 到期: %s", rem_val, due_val))
      ),
      
      # 内容编辑（初始隐藏）
      tags$div(id = "note_content_ed", style="display:none;",
        fluidRow(
          column(6, textInput("note_edit_no_m", "编号", value = note_no)),
          column(6, textInput("note_edit_created_m", "创建时间", value = substr(note$created_at[1] %||% format(Sys.time(),"%Y-%m-%d %H:%M"), 1, 16)))
        ),
        textAreaInput("note_edit_content_m", "内容（首行为标题）", rows = 4, value = note$content[1] %||% ""),
        fluidRow(
          column(6, textInput("note_edit_reminder_m", "⏰ 提醒时间", placeholder = "YYYY-MM-DD HH:MM", value = rem_val)),
          column(6, textInput("note_edit_due_m", "📅 到期时间", placeholder = "YYYY-MM-DD HH:MM", value = due_val))
        )
      ),
      
      tags$hr(),
      
      # 评论
      tags$h5("💬 评论"),
      if (nchar(comment_html) > 0) tags$div(
        style = "max-height:250px; overflow-y:auto; margin-bottom:8px;",
        HTML(comment_html)
      ) else tags$p(style="color:#999; font-size:12px;", "暂无评论"),
      
      # 评论输入 + 按钮
      textAreaInput("note_comment_new_m", NULL, rows = 2, placeholder = "添加评论..."),
      div(style="text-align:right; margin-top:4px; margin-bottom:8px;",
        actionButton("note_add_comment_m", "发表评论", class = "btn-info btn-sm", icon = icon("comment")))
    )
    
    showModal(modalDialog(
      title = tags$span(icon("sticky-note"), " ", note_no, " — ", note$title[1]),
      size = "l",
      modal_body,
      footer = tagList(
        HTML(status_btns),
        actionButton("note_toggle_edit", "✏ 修改", class = "btn-warning btn-sm", style = "margin-right:4px;"),
        actionButton("note_cancel_edit", "取消修改", class = "btn-default btn-sm", style = "display:none; margin-right:4px;"),
        actionButton("note_do_save", "💾 保存", class = "btn-primary btn-sm", style = "display:none; margin-right:4px;"),
        tags$button(class = "btn btn-info btn-sm note-wo-btn", `data-id` = note$id[1], style = "margin-right:4px;", "📋转工单"),
        tags$button(class = "btn btn-warning btn-sm note-report-btn", `data-id` = note$id[1], style = "margin-right:4px;", "📅日报"),
        tags$button(class = "btn btn-danger btn-sm note-del-btn", `data-id` = note$id[1], style = "margin-right:4px;", "🗑删除"),
        modalButton("关闭")
      ),
      easyClose = TRUE
    ))
  })

  ##################
  # 切换编辑模式：显示编辑区 + 显示评论编辑按钮
  ##################
  observeEvent(input$note_toggle_edit, {
    session$sendCustomMessage(type = "noteEditMode", message = list(mode = "edit"))
  })
  observeEvent(input$note_cancel_edit, {
    session$sendCustomMessage(type = "noteEditMode", message = list(mode = "view"))
  })

  ##################
  # 保存修改
  ##################
  observeEvent(input$note_do_save, {
    req(rv$logged_in, rv$note_edit_id, input$note_edit_content_m)
    lines <- strsplit(trimws(input$note_edit_content_m), "\n")[[1]]
    title <- if (length(lines) > 0) lines[1] else "未命名"
    result <- note_update(rv$note_edit_id,
      title = title, content = input$note_edit_content_m,
      note_no = if (trimws(input$note_edit_no_m) != "") input$note_edit_no_m else NULL,
      created_at = if (trimws(input$note_edit_created_m) != "") input$note_edit_created_m else NULL,
      reminder_at = if (trimws(input$note_edit_reminder_m) != "") input$note_edit_reminder_m else NULL,
      due_at = if (trimws(input$note_edit_due_m) != "") input$note_edit_due_m else NULL)
    removeModal()
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 状态移动（无论是否编辑模式都可用）
  ##################
  observeEvent(input$note_move_click, {
    req(rv$logged_in)
    result <- note_patch(as.integer(input$note_move_click$id), status = input$note_move_click$to)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 2)
  })

  ##################
  # 卡片小旗（循环递增）
  ##################
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

  ##################
  # 弹窗内小旗（直接设置）
  ##################
  observeEvent(input$note_flag_set, {
    req(rv$logged_in)
    parts <- strsplit(as.character(input$note_flag_set), ":")[[1]]
    id <- as.integer(parts[1]); val <- as.integer(parts[2])
    result <- note_patch(id, importance = val)
    note_trigger(note_trigger() + 1)
  })

  ##################
  # 添加评论（不关弹窗）
  ##################
  observeEvent(input$note_add_comment_m, {
    req(rv$logged_in, rv$note_edit_id, input$note_comment_new_m)
    if (trimws(input$note_comment_new_m) == "") return()
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    note_comment_add(rv$note_edit_id, input$note_comment_new_m, uid)
    updateTextAreaInput(session, "note_comment_new_m", value = "")
    note_trigger(note_trigger() + 1)
    showNotification("评论已添加", type = "message", duration = 1.5)
  })

  ##################
  # 评论编辑/删除（JS 触发）
  ##################
  observeEvent(input$note_comment_edit, {
    req(rv$logged_in)
    parts <- strsplit(as.character(input$note_comment_edit), ":")[[1]]
    result <- note_comment_update(as.integer(parts[1]), parts[2])
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 1.5)
  })

  observeEvent(input$note_comment_delete, {
    req(rv$logged_in)
    result <- note_comment_delete(as.integer(input$note_comment_delete))
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 1.5)
  })

  ##################
  # 转工单 / 记日报 / 删除
  ##################
  observeEvent(input$note_to_wo_click, {
    req(rv$logged_in)
    result <- note_convert_to_work_order(as.integer(input$note_to_wo_click), rv$current_user)
    removeModal()
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  observeEvent(input$note_report_click, {
    req(rv$logged_in)
    result <- note_patch(as.integer(input$note_report_click), reported_to_daily = 1)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 2)
  })
  observeEvent(input$note_del_click, {
    req(rv$logged_in)
    result <- note_delete(as.integer(input$note_del_click))
    removeModal()
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
}
