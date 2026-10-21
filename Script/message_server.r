# 消息机制 Server — 消息中心 + 动作流模板（类似泛微 ESB 动作流）

# 动作类型 → 中文标签
msg_action_label <- function(action) {
  switch(action,
    send = "发送", read = "已读", reply = "回复", forward = "转发",
    complete = "完成", revoke = "撤回", process = "处理", advance = "推进",
    action)
}

msg_status_color <- function(status) {
  switch(status,
    "已发送" = "#337ab7", "流转中" = "#f0ad4e", "已完成" = "#5cb85c",
    "已撤回" = "#999999", "草稿" = "#999999", "#337ab7")
}

msg_priority_color <- function(priority) {
  switch(priority, "高" = "#d9534f", "中" = "#f0ad4e", "低" = "#5bc0de", "#999999")
}

# 用户下拉 label（display_name（username），处理 NA）
msg_user_labels <- function(users) {
  if (is.null(users) || nrow(users) == 0) return(character(0))
  lbl <- ifelse(!is.na(users$display_name) & users$display_name != "",
    paste0(users$display_name, "（", users$username, "）"),
    users$username)
  setNames(as.character(users$id), lbl)
}

# 消息卡片列表 HTML
msg_render_cards <- function(items, type) {
  parts <- c()
  for (i in seq_len(nrow(items))) {
    m <- items[i, ]
    unread_cls <- ""
    if (type == "inbox" && !is.null(m$is_read) && !is.na(m$is_read) && m$is_read == 0) unread_cls <- " unread"
    pri_col <- msg_priority_color(m$priority %||% "中")
    st_col <- msg_status_color(m$status %||% "")
    sub <- if (type == "inbox") sprintf("来自：%s", m$sender_name %||% "") else sprintf("收件 %d 人", m$rcpt_count %||% 0)
    content_preview <- substr(trimws(m$content %||% ""), 1, 80)
    created <- if (!is.null(m$created_at) && !is.na(m$created_at)) substr(m$created_at, 1, 16) else ""
    parts <- c(parts, sprintf(
      '<div class="msg-card%s" style="border-left-color:%s;" onclick="Shiny.setInputValue(\'msg_open_click\',%d,{priority:\'event\'});">
        <div style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</div>
        <div style="margin-top:2px;display:flex;justify-content:space-between;align-items:center;gap:8px;">
          <b style="font-size:14px;color:#1a237e;"><span class="msg-pri" style="background:%s;"></span>%s</b>
          <span class="msg-badge" style="background:%s;">%s</span>
        </div>
        %s
        <div style="font-size:11px;color:#999;margin-top:6px;">%s · %s · %s</div>
      </div>',
      unread_cls, pri_col, m$id,
      m$msg_no, pri_col, m$title, st_col, m$status %||% "",
      if (nchar(content_preview) > 0) sprintf('<div style="font-size:12px;color:#666;margin-top:4px;">%s</div>', content_preview) else "",
      m$msg_type %||% "", sub, created))
  }
  paste(parts, collapse = "")
}

# 动作流时间线 HTML（返回字符）
msg_render_timeline <- function(actions) {
  if (is.null(actions) || nrow(actions) == 0) {
    return('<div style="color:#999;padding:8px 0;">暂无动作记录</div>')
  }
  parts <- c()
  for (i in seq_len(nrow(actions))) {
    a <- actions[i, ]
    act_label <- msg_action_label(a$action %||% "")
    actor <- a$actor_name %||% ""
    node_html <- if (!is.null(a$node_name) && !is.na(a$node_name) && a$node_name != "") sprintf('<span class="af-node">%s</span>', a$node_name) else ""
    tm <- if (!is.null(a$created_at) && !is.na(a$created_at)) substr(a$created_at, 1, 16) else ""
    comment_html <- if (!is.null(a$comment) && !is.na(a$comment) && a$comment != "") sprintf('<div class="af-comment">%s</div>', a$comment) else ""
    parts <- c(parts, sprintf(
      '<div class="af-item">
        <div class="af-hd"><span class="af-act">%s</span>%s%s%s</div>
        %s
      </div>',
      act_label, node_html, if (actor != "") sprintf('<b>%s</b>', actor) else "",
      if (tm != "") sprintf('<span style="color:#999;">%s</span>', tm) else "",
      comment_html))
  }
  paste(parts, collapse = "")
}

# 动作流模板节点链 HTML（返回字符）
msg_render_flow_chain <- function(nodes) {
  if (is.null(nodes) || length(nodes) == 0) return('<span style="color:#999;">（无节点）</span>')
  parts <- c()
  for (i in seq_along(nodes)) {
    if (i > 1) parts <- c(parts, '<span class="flow-arrow">→</span>')
    n <- nodes[[i]]
    parts <- c(parts, sprintf('<span class="flow-node">%s</span>', n$name %||% sprintf("节点%d", i)))
  }
  paste(parts, collapse = "")
}

message_server <- function(input, output, session, rv) {
  msg_trigger <- reactiveVal(0)
  msg_flow_trigger <- reactiveVal(0)
  msg_detail_trigger <- reactiveVal(0)
  msg_tab <- reactiveVal("inbox")   # inbox / outbox
  rv$msg_open_id <- NULL
  rv$msg_flow_edit_id <- NULL

  cur_uid <- reactive({
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
  })
  cur_name <- reactive({
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      u <- rv$current_user
      if (!is.null(u$display_name) && !is.na(u$display_name[1]) && u$display_name[1] != "") u$display_name[1] else u$username[1]
    } else ""
  })

  # ========== 消息中心 ==========
  output$msg_center_ui <- renderUI({
    req(rv$logged_in); msg_trigger()
    uid <- cur_uid()
    if (is.null(uid)) return(NULL)
    unread <- message_unread_count(uid)
    tab <- msg_tab()
    top <- tagList(
      fluidRow(
        column(3, actionButton("msg_new_btn", "新建消息", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;")),
        column(3, actionButton("msg_refresh_btn", "刷新", icon = icon("sync"), class = "btn-info btn-sm")),
        column(6, div(style = "text-align:right;font-size:12px;color:#888;padding-top:6px;",
          sprintf("未读 <b style='color:#337ab7;'>%d</b> 条", unread)))
      ),
      br(),
      div(style = "margin-bottom:8px;",
        actionButton("msg_tab_inbox", "收件箱", class = paste("btn-sm", if (tab == "inbox") "btn-primary" else "btn-default")),
        actionButton("msg_tab_outbox", "发件箱", class = paste("btn-sm", if (tab == "outbox") "btn-primary" else "btn-default"))
      ),
      hr()
    )
    if (tab == "inbox") {
      items <- message_list_inbox(uid)
      body <- if (is.null(items) || nrow(items) == 0) {
        div(style = "text-align:center;padding:40px;color:#999;", "收件箱为空")
      } else HTML(msg_render_cards(items, "inbox"))
    } else {
      items <- message_list_outbox(uid)
      body <- if (is.null(items) || nrow(items) == 0) {
        div(style = "text-align:center;padding:40px;color:#999;", "发件箱为空")
      } else HTML(msg_render_cards(items, "outbox"))
    }
    tagList(top, body)
  })

  observeEvent(input$msg_refresh_btn, { msg_trigger(msg_trigger() + 1) })
  observeEvent(input$msg_tab_inbox, { msg_tab("inbox"); msg_trigger(msg_trigger() + 1) })
  observeEvent(input$msg_tab_outbox, { msg_tab("outbox"); msg_trigger(msg_trigger() + 1) })

  # 新建消息弹窗
  observeEvent(input$msg_new_btn, {
    req(rv$logged_in)
    users <- message_get_users()
    user_choices <- msg_user_labels(users)
    flows <- message_flow_list()
    flow_choices <- c("不绑定动作流" = "")
    if (!is.null(flows) && nrow(flows) > 0) {
      flow_choices <- c(flow_choices, setNames(as.character(flows$id), flows$name))
    }
    showModal(modalDialog(
      title = "新建消息",
      textInput("msg_edit_title", "标题", width = "100%"),
      textAreaInput("msg_edit_content", "内容", rows = 5, width = "100%"),
      fluidRow(
        column(6, selectInput("msg_edit_type", "类型", choices = c("站内信", "公告", "通知", "事件"), selected = "站内信")),
        column(6, selectInput("msg_edit_priority", "优先级", choices = c("高", "中", "低"), selected = "中"))
      ),
      selectizeInput("msg_edit_recipients", "接收人（可多选）", choices = user_choices, multiple = TRUE, width = "100%"),
      selectInput("msg_edit_flow", "动作流模板", choices = flow_choices, width = "100%"),
      footer = tagList(
        actionButton("msg_send_btn", "发送", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$msg_send_btn, {
    req(rv$logged_in, input$msg_edit_title)
    uid <- cur_uid()
    flow_id <- if (is.null(input$msg_edit_flow) || input$msg_edit_flow == "") NULL else as.integer(input$msg_edit_flow)
    result <- message_add(
      input$msg_edit_title, input$msg_edit_content, input$msg_edit_type, input$msg_edit_priority,
      sender_id = uid, recipient_ids = as.integer(input$msg_edit_recipients), flow_id = flow_id)
    if (isTRUE(result$success)) {
      removeModal()
      msg_trigger(msg_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 打开消息详情
  observeEvent(input$msg_open_click, {
    req(rv$logged_in, input$msg_open_click)
    mid <- as.integer(input$msg_open_click)
    rv$msg_open_id <- mid
    uid <- cur_uid()
    if (!is.null(uid)) message_mark_read(mid, uid)
    msg_detail_trigger(msg_detail_trigger() + 1)
    msg_trigger(msg_trigger() + 1)
    showModal(modalDialog(
      title = "消息详情",
      uiOutput("msg_detail_content"),
      hr(),
      uiOutput("msg_detail_actions"),
      footer = tagList(modalButton("关闭")),
      size = "l", easyClose = TRUE
    ))
  })

  # 详情内容
  output$msg_detail_content <- renderUI({
    msg_detail_trigger()
    mid <- rv$msg_open_id
    if (is.null(mid)) return(NULL)
    m <- message_get_by_id(mid)
    if (is.null(m) || nrow(m) == 0) return(div("消息不存在"))
    m <- m[1, ]
    rcpts <- message_get_recipients(mid)
    actions <- message_actions_get(mid)
    flow_name <- ""
    if (!is.null(m$flow_id) && !is.na(m$flow_id)) {
      f <- message_flow_get(m$flow_id)
      if (!is.null(f) && nrow(f) > 0) flow_name <- f$name[1]
    }
    rcpt_labels <- if (nrow(rcpts) > 0) paste(rcpts$display_name, collapse = "、") else "（无）"
    tagList(
      div(style = "font-size:11px;color:#888;font-family:Consolas,monospace;margin-bottom:4px;",
        sprintf("%s · %s · 优先级:%s", m$msg_no, m$msg_type %||% "", m$priority %||% "")),
      h4(style = "margin-top:0;color:#1a237e;", m$title),
      div(style = "font-size:13px;color:#333;white-space:pre-wrap;margin-bottom:12px;", m$content %||% ""),
      div(style = "font-size:12px;color:#666;margin-bottom:12px;",
        sprintf("发件人：%s　·　收件人：%s　·　动作流：%s",
          m$sender_name %||% "系统", rcpt_labels, if (flow_name != "") flow_name else "自由消息")),
      h5(style = "color:#4338ca;", icon("stream"), " 动作流轨迹"),
      div(class = "af-timeline", HTML(msg_render_timeline(actions)))
    )
  })

  # 详情操作区
  output$msg_detail_actions <- renderUI({
    msg_detail_trigger()
    mid <- rv$msg_open_id
    if (is.null(mid)) return(NULL)
    m <- message_get_by_id(mid)
    if (is.null(m) || nrow(m) == 0) return(NULL)
    m <- m[1, ]
    uid <- cur_uid()
    is_sender <- !is.null(uid) && !is.null(m$sender_id) && !is.na(m$sender_id) && m$sender_id == uid
    is_flow <- !is.null(m$flow_id) && !is.na(m$flow_id)
    done <- (m$status %||% "") %in% c("已完成", "已撤回")
    fluidRow(
      column(7,
        div(style = "display:flex;gap:4px;align-items:center;",
          textInput("msg_reply_text", NULL, placeholder = "回复内容...", width = "100%"),
          actionButton("msg_reply_btn", "回复", class = "btn-sm btn-primary"))
      ),
      column(5,
        div(style = "display:flex;gap:6px;justify-content:flex-end;",
          if (is_flow && !done) actionButton("msg_advance_btn", "推进动作流", class = "btn-sm btn-warning") else NULL,
          actionButton("msg_forward_btn", "转发", class = "btn-sm btn-info"),
          if (!done) actionButton("msg_complete_btn", "完成", class = "btn-sm btn-success") else NULL,
          if (is_sender && !done) actionButton("msg_revoke_btn", "撤回", class = "btn-sm btn-danger") else NULL
        )
      )
    )
  })

  observeEvent(input$msg_reply_btn, {
    req(rv$logged_in)
    mid <- rv$msg_open_id
    if (is.null(mid)) return()
    comment <- trimws(input$msg_reply_text %||% "")
    if (comment == "") { showNotification("请输入回复内容", type = "warning"); return() }
    res <- message_reply(mid, cur_uid(), cur_name(), comment)
    updateTextInput(session, "msg_reply_text", value = "")
    msg_detail_trigger(msg_detail_trigger() + 1)
    showNotification(if (isTRUE(res$success)) "已回复" else res$message, type = if (isTRUE(res$success)) "message" else "error")
  })

  observeEvent(input$msg_advance_btn, {
    req(rv$logged_in)
    mid <- rv$msg_open_id
    if (is.null(mid)) return()
    res <- message_advance_flow(mid, cur_uid(), cur_name())
    msg_detail_trigger(msg_detail_trigger() + 1)
    msg_trigger(msg_trigger() + 1)
    showNotification(res$message, type = if (isTRUE(res$success)) "message" else "error")
  })

  observeEvent(input$msg_complete_btn, {
    req(rv$logged_in)
    mid <- rv$msg_open_id
    if (is.null(mid)) return()
    res <- message_complete(mid, cur_uid(), cur_name())
    msg_detail_trigger(msg_detail_trigger() + 1)
    msg_trigger(msg_trigger() + 1)
    showNotification(res$message, type = if (isTRUE(res$success)) "message" else "error")
  })

  observeEvent(input$msg_revoke_btn, {
    req(rv$logged_in)
    mid <- rv$msg_open_id
    if (is.null(mid)) return()
    res <- message_revoke(mid, cur_uid(), cur_name())
    msg_detail_trigger(msg_detail_trigger() + 1)
    msg_trigger(msg_trigger() + 1)
    showNotification(res$message, type = if (isTRUE(res$success)) "message" else "error")
  })

  # 转发弹窗
  observeEvent(input$msg_forward_btn, {
    req(rv$logged_in)
    users <- message_get_users()
    user_choices <- msg_user_labels(users)
    showModal(modalDialog(
      title = "转发消息",
      selectizeInput("msg_forward_recipients", "转发给（可多选）", choices = user_choices, multiple = TRUE, width = "100%"),
      textInput("msg_forward_comment", "备注", width = "100%"),
      footer = tagList(
        actionButton("msg_forward_confirm", "转发", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input$msg_forward_confirm, {
    req(rv$logged_in)
    mid <- rv$msg_open_id
    if (is.null(mid)) return()
    targets <- as.integer(input$msg_forward_recipients)
    res <- message_forward(mid, cur_uid(), cur_name(), targets, input$msg_forward_comment)
    if (isTRUE(res$success)) {
      removeModal()
      msg_detail_trigger(msg_detail_trigger() + 1)
      showNotification("已转发", type = "message")
    } else showNotification(res$message, type = "error")
  })

  # ========== 动作流模板 ==========
  output$msg_flow_ui <- renderUI({
    req(rv$logged_in); msg_flow_trigger()
    flows <- message_flow_list()
    top <- fluidRow(
      column(3, actionButton("msg_flow_new_btn", "新建动作流模板", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;")),
      column(9, div(style = "font-size:12px;color:#888;padding-top:6px;", "动作流模板定义消息的流转节点链（如：发送 → 处理 → 审批 → 归档）"))
    )
    br()
    if (is.null(flows) || nrow(flows) == 0) {
      body <- div(style = "text-align:center;padding:40px;color:#999;", "暂无动作流模板，点击「新建动作流模板」开始")
      return(tagList(top, body))
    }
    parts <- c()
    for (i in seq_len(nrow(flows))) {
      f <- flows[i, ]
      nodes <- tryCatch(jsonlite::fromJSON(f$nodes[1], simplifyVector = FALSE), error = function(e) list())
      parts <- c(parts, sprintf(
        '<div class="flow-card">
          <div style="display:flex;justify-content:space-between;align-items:center;">
            <b style="font-size:14px;color:#1a237e;">%s</b>
            <span>
              <button class="btn btn-xs btn-info" onclick="Shiny.setInputValue(\'msg_flow_edit_click\',%d,{priority:\'event\'});">✏</button>
              <button class="btn btn-xs btn-danger" onclick="if(confirm(\'删除此动作流模板？\'))Shiny.setInputValue(\'msg_flow_del_click\',%d,{priority:\'event\'});">🗑</button>
            </span>
          </div>
          <div style="font-size:12px;color:#666;margin-top:4px;">%s</div>
          <div style="margin-top:8px;">%s</div>
        </div>',
        f$name, f$id, f$id,
        f$description %||% "",
        msg_render_flow_chain(nodes)))
    }
    tagList(top, HTML(paste(parts, collapse = "")))
  })

  observeEvent(input$msg_flow_new_btn, {
    req(rv$logged_in)
    rv$msg_flow_edit_id <- NULL
    showModal(modalDialog(
      title = "新建动作流模板",
      textInput("msg_flow_name", "模板名称", width = "100%", placeholder = "如：常规审批流"),
      textInput("msg_flow_desc", "描述", width = "100%"),
      textAreaInput("msg_flow_nodes_text", "动作节点（每行一个）", rows = 6, width = "100%",
        value = "发送\n处理\n归档"),
      footer = tagList(
        actionButton("msg_flow_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input$msg_flow_edit_click, {
    req(rv$logged_in, input$msg_flow_edit_click)
    fid <- as.integer(input$msg_flow_edit_click)
    f <- message_flow_get(fid)
    if (is.null(f) || nrow(f) == 0) return()
    f <- f[1, ]
    rv$msg_flow_edit_id <- fid
    nodes <- tryCatch(jsonlite::fromJSON(f$nodes[1], simplifyVector = FALSE), error = function(e) list())
    nodes_text <- paste(sapply(nodes, function(n) n$name %||% ""), collapse = "\n")
    showModal(modalDialog(
      title = sprintf("编辑动作流模板（#%d）", fid),
      textInput("msg_flow_name_m", "模板名称", value = f$name, width = "100%"),
      textInput("msg_flow_desc_m", "描述", value = f$description %||% "", width = "100%"),
      textAreaInput("msg_flow_nodes_text_m", "动作节点（每行一个）", rows = 6, width = "100%", value = nodes_text),
      footer = tagList(
        actionButton("msg_flow_update_btn", "更新", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input$msg_flow_save_btn, {
    req(rv$logged_in, input$msg_flow_name)
    nodes_text <- trimws(input$msg_flow_nodes_text %||% "")
    lines <- strsplit(nodes_text, "\n")[[1]]
    lines <- trimws(lines); lines <- lines[lines != ""]
    if (length(lines) == 0) { showNotification("请至少填写一个动作节点", type = "warning"); return() }
    nodes <- lapply(lines, function(nm) list(name = nm, action_type = "process", assignee = ""))
    res <- message_flow_add(input$msg_flow_name, input$msg_flow_desc, nodes)
    if (isTRUE(res$success)) {
      removeModal()
      msg_flow_trigger(msg_flow_trigger() + 1)
      showNotification(res$message, type = "message")
    } else showNotification(res$message, type = "error")
  })

  observeEvent(input$msg_flow_update_btn, {
    req(rv$logged_in, input$msg_flow_name_m)
    fid <- rv$msg_flow_edit_id
    if (is.null(fid)) return()
    nodes_text <- trimws(input$msg_flow_nodes_text_m %||% "")
    lines <- strsplit(nodes_text, "\n")[[1]]
    lines <- trimws(lines); lines <- lines[lines != ""]
    if (length(lines) == 0) { showNotification("请至少填写一个动作节点", type = "warning"); return() }
    nodes <- lapply(lines, function(nm) list(name = nm, action_type = "process", assignee = ""))
    res <- message_flow_update(fid, input$msg_flow_name_m, input$msg_flow_desc_m, nodes)
    if (isTRUE(res$success)) {
      removeModal()
      msg_flow_trigger(msg_flow_trigger() + 1)
      showNotification(res$message, type = "message")
    } else showNotification(res$message, type = "error")
  })

  observeEvent(input$msg_flow_del_click, {
    req(rv$logged_in, input$msg_flow_del_click)
    res <- message_flow_delete(as.integer(input$msg_flow_del_click))
    if (isTRUE(res$success)) {
      msg_flow_trigger(msg_flow_trigger() + 1)
      showNotification("已删除", type = "message")
    } else showNotification(res$message, type = "error")
  })
}
