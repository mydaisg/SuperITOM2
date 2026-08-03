# 流程模块 V2 - 服务端
# 完全复用 process_engine.r 数据层，仅重写 UI+交互逻辑

process_v2_server <- function(input, output, session, rv) {

  v2_trigger <- reactiveVal(0)
  refresh_all <- function() v2_trigger(v2_trigger() + 1)

  current_uid <- reactive({
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
  })
  current_name <- reactive({
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0)
      (rv$current_user$display_name %||% rv$current_user$username)[1] else ""
  })

  ##################
  # 统计卡片
  ##################
  output$prv2_stat_pending <- renderUI({
    v2_trigger()
    req(current_uid())
    n <- nrow(appr_pending_list(current_uid()))
    div(class = "prv2-stat-card",
      div(class = "icon-wrap", style = "background:#e6f7ff; color:#1890ff;", icon("bell")),
      div(div(class = "num", style = "color:#1890ff;", n),
          div(class = "lbl", "我的待审批")))
  })

  output$prv2_stat_done <- renderUI({
    v2_trigger()
    s <- appr_stats()
    div(class = "prv2-stat-card",
      div(class = "icon-wrap", style = "background:#f6ffed; color:#52c41a;", icon("check-circle")),
      div(div(class = "num", style = "color:#52c41a;", s$approved),
          div(class = "lbl", "总通过数量")))
  })

  output$prv2_stat_cc <- renderUI({
    v2_trigger()
    n <- if (!is.null(current_uid())) nrow(appr_cc_list(current_uid())) else 0
    div(class = "prv2-stat-card",
      div(class = "icon-wrap", style = "background:#fff7e6; color:#fa8c16;", icon("share-square")),
      div(div(class = "num", style = "color:#fa8c16;", n),
          div(class = "lbl", "抄送我的")))
  })

  output$prv2_stat_tpl <- renderUI({
    v2_trigger()
    s <- appr_stats()
    div(class = "prv2-stat-card",
      div(class = "icon-wrap", style = "background:#f9f0ff; color:#722ed1;", icon("sitemap")),
      div(div(class = "num", style = "color:#722ed1;", s$tpls),
          div(class = "lbl", "审批模板")))
  })

  ##################
  # 待审批列表（卡片式）
  ##################
  output$prv2_pending_list <- renderUI({
    v2_trigger()
    req(current_uid(), rv$logged_in)
    items <- appr_pending_list(current_uid())
    if (nrow(items) == 0) {
      return(div(style = "text-align:center; padding:60px 20px; color:#bbb;",
        icon("inbox", "fa-3x"), br(), br(), "暂无待审批"))
    }
    cards <- lapply(seq_len(nrow(items)), function(i) {
      it <- items[i, ]
      # 获取步骤信息
      steps <- tryCatch(appr_steps_get(it$id), error = function(e) data.frame())
      active_step <- if (nrow(steps) > 0) steps[steps$status == "active", ] else steps[1, ]
      step_idx <- if (nrow(steps) > 0 && nrow(active_step) > 0) which(steps$id == active_step$id[1]) else 1

      div(class = "prv2-card-tpl",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          div(style = "flex:1;",
            div(class = "tpl-name", icon("file-text-o"), " ", it$title %||% "—"),
            div(class = "tpl-desc",
              sprintf("模板: %s · 申请人: %s · 第%d步",
                it$template_name %||% "—",
                it$applicant_name %||% it$applicant_username %||% "—",
                step_idx)),
            div(class = "tpl-meta",
              tags$span(icon("clock"), " ", it$entered_at %||% "—"),
              tags$span(icon("hashtag"), " ", it$instance_no %||% "—"))),
          div(style = "display:flex; gap:6px; flex-direction:column;",
            tags$button(class = "btn btn-success btn-xs prv2-view", `data-id` = it$id,
              `data-mode` = "instance",
              type = "button", list(icon("edit"), "处理")))))
    })
    tagList(cards)
  })

  ##################
  # 我发起的列表
  ##################
  output$prv2_my_list <- renderUI({
    v2_trigger()
    req(current_uid(), rv$logged_in)
    items <- appr_inst_list(applicant_id = current_uid(), status = input$prv2_my_status)
    if (nrow(items) == 0) {
      return(div(style = "text-align:center; padding:60px 20px; color:#bbb;",
        "暂无记录"))
    }
    status_cls <- c("pending" = "pending", "approved" = "approved",
                    "rejected" = "rejected", "withdrawn" = "withdrawn")
    status_text <- c("pending" = "审批中", "approved" = "已通过",
                     "rejected" = "已驳回", "withdrawn" = "已撤销")
    cards <- lapply(seq_len(nrow(items)), function(i) {
      it <- items[i, ]
      st <- it$status[1]
      div(class = "prv2-card-tpl",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          div(style = "flex:1;",
            div(class = "tpl-name", icon("paper-plane"), " ", it$title %||% "—"),
            div(class = "tpl-desc",
              sprintf("模板: %s · 编号: %s", it$template_name %||% "—", it$instance_no %||% "—")),
            div(class = "tpl-meta",
              tags$span(icon("clock"), " ", it$started_at %||% "—"))),
          div(style = "display:flex; gap:6px; flex-direction:column; align-items:flex-end;",
            span(class = paste("prv2-badge", status_cls[st] %||% ""),
              status_text[st] %||% st),
            div(style = "display:flex; gap:4px;",
              tags$button(class = "btn btn-info btn-xs prv2-view", `data-id` = it$id,
                `data-mode` = "instance", type = "button", list(icon("eye"), "详情")),
              if (st == "pending") tagList(
                tags$button(class = "btn btn-warning btn-xs prv2-act", `data-id` = it$id,
                  `data-action` = "urge", type = "button", list(icon("bell"), "催办")),
                tags$button(class = "btn btn-default btn-xs prv2-act", `data-id` = it$id,
                  `data-action` = "withdraw", `data-confirm` = "确定撤销此审批？",
                  type = "button", list(icon("undo"), "撤销")))))))
    })
    tagList(cards)
  })

  ##################
  # 我处理的列表
  ##################
  output$prv2_done_list <- renderUI({
    v2_trigger()
    req(current_uid(), rv$logged_in)
    items <- appr_done_list(current_uid())
    if (nrow(items) == 0) {
      return(div(style = "text-align:center; padding:60px 20px; color:#bbb;",
        "暂无处理记录"))
    }
    action_text <- c("approve" = "已通过", "reject" = "已驳回",
                     "withdraw" = "已撤销", "urge" = "催办")
    cards <- lapply(seq_len(nrow(items)), function(i) {
      it <- items[i, ]
      act <- it$my_action[1]
      div(class = "prv2-card-tpl",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          div(style = "flex:1;",
            div(class = "tpl-name", icon("check"), " ", it$title %||% "—"),
            div(class = "tpl-desc",
              sprintf("模板: %s · 申请人: %s", it$template_name %||% "—",
                it$applicant_name %||% it$applicant_username %||% "—")),
            div(class = "tpl-meta",
              tags$span(icon("user"), " 我的操作：", action_text[act] %||% act),
              tags$span(icon("clock"), " ", it$my_done_at %||% "—"))),
          div(style = "display:flex; gap:6px;",
            tags$button(class = "btn btn-info btn-xs prv2-view", `data-id` = it$id,
              `data-mode` = "instance", type = "button", list(icon("eye"), "详情")))))
    })
    tagList(cards)
  })

  ##################
  # 抄送我的
  ##################
  output$prv2_cc_list <- renderUI({
    v2_trigger()
    req(current_uid(), rv$logged_in)
    items <- appr_cc_list(current_uid())
    if (nrow(items) == 0) {
      return(div(style = "text-align:center; padding:60px 20px; color:#bbb;",
        "暂无抄送"))
    }
    status_cls <- c("pending" = "pending", "approved" = "approved",
                    "rejected" = "rejected", "withdrawn" = "withdrawn")
    status_text <- c("pending" = "审批中", "approved" = "已通过",
                     "rejected" = "已驳回", "withdrawn" = "已撤销")
    cards <- lapply(seq_len(nrow(items)), function(i) {
      it <- items[i, ]
      st <- it$status[1]
      div(class = "prv2-card-tpl",
        div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
          div(style = "flex:1;",
            div(class = "tpl-name", icon("share-square"), " ", it$title %||% "—"),
            div(class = "tpl-desc",
              sprintf("模板: %s · 申请人: %s", it$template_name %||% "—",
                it$applicant_name %||% it$applicant_username %||% "—")),
            div(class = "tpl-meta",
              tags$span(icon("clock"), " ", it$started_at %||% "—"))),
          div(style = "display:flex; gap:6px; align-items:center;",
            span(class = paste("prv2-badge", status_cls[st] %||% ""),
              status_text[st] %||% st),
            tags$button(class = "btn btn-info btn-xs prv2-view", `data-id` = it$id,
              `data-mode` = "instance", type = "button", list(icon("eye"), "详情")))))
    })
    tagList(cards)
  })

  ##################
  # 模板列表（卡片式）
  ##################
  output$prv2_tpl_list <- renderUI({
    v2_trigger()
    tpls <- appr_tpl_list()
    if (nrow(tpls) == 0) {
      return(div(style = "text-align:center; padding:60px 20px; color:#bbb;",
        icon("file-o", "fa-3x"), br(), br(),
        "暂无模板，点击「新建模板」或「示例模板」开始"))
    }
    cards <- lapply(seq_len(nrow(tpls)), function(i) prv2_tpl_card_ui(tpls[i, ]))
    tagList(cards)
  })

  ##################
  # 详情弹窗（点击查看/处理）
  ##################
  observeEvent(input$prv2_view, {
    req(rv$logged_in)
    id <- as.integer(input$prv2_view$id)
    inst <- appr_inst_get(id)
    if (is.null(inst)) return()

    steps <- appr_steps_get(id)
    records <- appr_records_get(id)
    form_data <- tryCatch(jsonlite::fromJSON(inst$form_data[1]),
      error = function(e) list())

    # 判断当前用户是否在 active 步骤
    uid <- current_uid()
    can_approve <- FALSE
    if (inst$status[1] == "pending" && nrow(steps) > 0) {
      active_step <- steps[steps$status == "active", ]
      if (nrow(active_step) > 0) {
        ops <- tryCatch(jsonlite::fromJSON(active_step$operator_ids[1]),
          error = function(e) c())
        if (!is.null(uid) && uid %in% ops) can_approve <- TRUE
      }
    }

    # 保存当前查看的实例 ID，供后续操作使用
    rv$prv2_view_id <- id

    tpl <- NULL  # V2 不需要 tpl 数据，UI 已渲染基本信息
    showModal(modalDialog(
      title = NULL, easyClose = TRUE, size = "l",
      style = "max-width:1100px;",
      prv2_detail_modal(inst, tpl, steps, records, form_data, can_approve)
    ))
  })

  ##################
  # 操作：通过 / 驳回 / 催办 / 撤销
  ##################
  observeEvent(input$prv2_do_approve, {
    req(rv$logged_in, rv$prv2_view_id)
    steps <- appr_steps_get(rv$prv2_view_id)
    active_step <- steps[steps$status == "active", ]
    if (nrow(active_step) == 0) {
      showNotification("没有活跃的审批步骤", type = "warning"); return()
    }
    result <- appr_approve(rv$prv2_view_id, active_step$id[1],
      current_uid(), current_name(), input$prv2_comment %||% "")
    removeModal(); refresh_all()
    showNotification(result$message,
      type = ifelse(result$success, "message", "error"))
  })

  observeEvent(input$prv2_do_reject, {
    req(rv$logged_in, rv$prv2_view_id)
    steps <- appr_steps_get(rv$prv2_view_id)
    active_step <- steps[steps$status == "active", ]
    if (nrow(active_step) == 0) {
      showNotification("没有活跃的审批步骤", type = "warning"); return()
    }
    result <- appr_reject(rv$prv2_view_id, active_step$id[1],
      current_uid(), current_name(), input$prv2_comment %||% "")
    removeModal(); refresh_all()
    showNotification(result$message,
      type = ifelse(result$success, "message", "error"))
  })

  observeEvent(input$prv2_do_withdraw, {
    req(rv$logged_in, rv$prv2_view_id)
    result <- appr_withdraw(rv$prv2_view_id, current_uid())
    removeModal(); refresh_all()
    showNotification(result$message,
      type = ifelse(result$success, "message", "error"))
  })

  observeEvent(input$prv2_do_urge, {
    req(rv$logged_in, rv$prv2_view_id)
    result <- appr_urge(rv$prv2_view_id, current_uid())
    showNotification(result$message, type = "message")
  })

  observeEvent(input$prv2_close, { removeModal() })

  ##################
  # 行内通用动作（催办/撤销）
  ##################
  observeEvent(input$prv2_act, {
    req(rv$logged_in, input$prv2_act$action, input$prv2_act$id)
    action <- input$prv2_act$action
    id <- as.integer(input$prv2_act$id)
    result <- switch(action,
      "urge"     = appr_urge(id, current_uid()),
      "withdraw" = appr_withdraw(id, current_uid()),
      list(success = FALSE, message = "未知操作"))
    refresh_all()
    showNotification(result$message,
      type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 模板操作（发起/发布/编辑/删除）
  ##################
  # 通过动态生成的按钮（prv2_tpl_start_X / prv2_tpl_pub_X / prv2_tpl_edit_X / prv2_tpl_del_X）
  observe({
    req(rv$logged_in)
    v2_trigger()
    tpls <- appr_tpl_list()
    lapply(seq_len(nrow(tpls)), function(i) {
      tid <- tpls$id[i]
      local({
        # 发起
        observeEvent(input[[paste0("prv2_tpl_start_", tid)]], {
          tpl <- appr_tpl_get(tid)
          if (is.null(tpl) || tpl$status[1] != "published") {
            showNotification("模板未发布", type = "warning"); return()
          }
          fields <- tryCatch(jsonlite::fromJSON(tpl$form_fields[1], simplifyVector = FALSE),
            error = function(e) list())
          modal_body <- tagList(
            h4(sprintf("发起审批: %s", tpl$name[1])),
            p(tpl$description[1] %||% "", style = "color:#7f8c8d;font-size:12px;"),
            hr(),
            textInput("prv2_start_title", "审批标题", value = tpl$name[1]))
          for (f in fields) {
            fid <- paste0("prv2_f_", f$key)
            label <- f$label %||% f$key
            if (f$type == "textarea")
              modal_body <- tagList(modal_body,
                textAreaInput(fid, label, rows = 3, placeholder = f$placeholder %||% ""))
            else if (f$type == "select")
              modal_body <- tagList(modal_body,
                selectInput(fid, label, choices = unlist(f$options %||% list())))
            else
              modal_body <- tagList(modal_body,
                textInput(fid, label, placeholder = f$placeholder %||% ""))
          }
          rv$prv2_tpl_id_for_start <- tid
          showModal(modalDialog(title = sprintf("发起审批: %s", tpl$name[1]),
            modal_body,
            footer = tagList(modalButton("取消"),
              actionButton("prv2_confirm_start", "提交审批", class = "btn-primary")),
            size = "m", easyClose = TRUE))
        }, ignoreInit = TRUE)

        # 发布
        observeEvent(input[[paste0("prv2_tpl_pub_", tid)]], {
          result <- appr_tpl_publish(tid)
          refresh_all()
          showNotification(result$message,
            type = ifelse(result$success, "message", "error"))
        }, ignoreInit = TRUE)

        # 编辑
        observeEvent(input[[paste0("prv2_tpl_edit_", tid)]], {
          tpl <- appr_tpl_get(tid)
          if (is.null(tpl)) return()
          fields <- tryCatch(jsonlite::fromJSON(tpl$form_fields[1]),
            error = function(e) list())
          fstr <- paste(sapply(fields, function(f)
            paste(f$key, f$label, f$type %||% "text", sep = "|")), collapse = "\n")
          apprs <- tryCatch(jsonlite::fromJSON(tpl$approver_config[1]),
            error = function(e) list())
          astr <- paste(sapply(apprs, function(a)
            paste(a$step_name %||% "", paste(unlist(a$approver_ids), collapse = ","), sep = "|")),
            collapse = "\n")
          cc <- tryCatch(jsonlite::fromJSON(tpl$cc_config[1]),
            error = function(e) list())
          cstr <- ""
          if (length(cc) > 0 && !is.null(cc[[1]]$user_ids)) {
            cstr <- paste(sapply(seq_along(cc[[1]]$user_ids), function(i) {
              uid_val <- cc[[1]]$user_ids[[i]]
              uname <- if (length(cc[[1]]$user_names) >= i) cc[[1]]$user_names[[i]] else ""
              paste(uid_val, uname, sep = "|")
            }), collapse = "\n")
          }
          rv$prv2_edit_tpl_id <- tid
          showModal(modalDialog(title = sprintf("编辑模板: %s", tpl$name[1]),
            size = "l", easyClose = TRUE,
            textInput("prv2_edit_name", "模板名称", value = tpl$name[1]),
            textInput("prv2_edit_desc", "描述", value = tpl$description[1] %||% ""),
            selectInput("prv2_edit_cat", "分类",
              choices = c("通用" = "general", "请假" = "leave", "报销" = "expense",
                          "加班" = "overtime", "出差" = "travel", "审批" = "approval"),
              selected = tpl$category[1] %||% "general"),
            div(class = "prv2-tpl-builder",
              h6("表单字段（每行: key|标签|类型）"),
              textAreaInput("prv2_edit_fields", NULL, rows = 4, value = fstr)),
            div(class = "prv2-tpl-builder",
              h6("审批人（每行: 步骤名|用户ID逗号分隔）"),
              textAreaInput("prv2_edit_approvers", NULL, rows = 3, value = astr)),
            div(class = "prv2-tpl-builder",
              h6("抄送人（每行: 用户ID|姓名，可选）"),
              textAreaInput("prv2_edit_cc", NULL, rows = 2, value = cstr)),
            footer = tagList(modalButton("取消"),
              actionButton("prv2_save_edit", "保存", class = "btn-primary"))))
        }, ignoreInit = TRUE)

        # 删除
        observeEvent(input[[paste0("prv2_tpl_del_", tid)]], {
          showModal(modalDialog(title = "确认删除模板",
            tags$p(sprintf("确定删除模板「%s」吗？", tpls$name[i])),
            tags$p(style = "color:#d9534f;font-size:12px;",
              "已有审批记录的模板请谨慎删除"),
            footer = tagList(modalButton("取消"),
              actionButton("prv2_confirm_del_tpl", "确认删除", class = "btn-dark")),
            size = "s", easyClose = TRUE))
          rv$prv2_del_tpl_id <- tid
        }, ignoreInit = TRUE)
      })
    })
  })

  # 提交发起审批
  observeEvent(input$prv2_confirm_start, {
    req(rv$logged_in, rv$prv2_tpl_id_for_start, input$prv2_start_title)
    tid <- rv$prv2_tpl_id_for_start
    tpl <- appr_tpl_get(tid)
    if (is.null(tpl)) { showNotification("模板不存在", type = "error"); return() }
    fields <- tryCatch(jsonlite::fromJSON(tpl$form_fields[1], simplifyVector = FALSE),
      error = function(e) list())
    form_data <- list()
    for (f in fields) form_data[[f$key]] <- input[[paste0("prv2_f_", f$key)]]
    result <- appr_inst_create(template_id = tid, title = input$prv2_start_title,
      form_data = form_data, applicant_id = current_uid())
    removeModal()
    if (result$success) {
      refresh_all()
      showNotification(sprintf("已提交，编号: %s", result$instance_no),
        type = "message", duration = 8)
    } else showNotification(result$message, type = "error")
  })

  # 保存模板编辑
  observeEvent(input$prv2_save_edit, {
    req(rv$logged_in, rv$prv2_edit_tpl_id)
    tid <- rv$prv2_edit_tpl_id
    lines <- strsplit(input$prv2_edit_fields, "\n")[[1]]
    fields <- list()
    for (line in lines) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 2)
        fields[[length(fields) + 1]] <- list(
          key = parts[1], label = parts[2],
          type = ifelse(length(parts) >= 3, parts[3], "text"), required = TRUE)
    }
    alines <- strsplit(input$prv2_edit_approvers, "\n")[[1]]
    approvers <- list()
    for (line in alines) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 2) {
        ids <- as.integer(trimws(strsplit(parts[2], ",")[[1]]))
        con <- db_connect()
        names <- tryCatch({
          n <- dbGetQuery(con, sprintf("SELECT display_name,username FROM users WHERE id IN (%s)",
            paste(ids, collapse = ",")))
          if (nrow(n) > 0) ifelse(is.na(n$display_name) | n$display_name == "",
            n$username, n$display_name) else as.character(ids)
        }, finally = { db_disconnect(con) })
        approvers[[length(approvers) + 1]] <- list(
          step_name = parts[1], operator_type = "fixed",
          approver_ids = as.list(ids), approver_names = as.list(names))
      }
    }
    clines <- strsplit(input$prv2_edit_cc, "\n")[[1]]
    cc_list <- list(user_ids = list(), user_names = list())
    for (line in clines) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 1 && nchar(parts[1]) > 0) {
        cc_list$user_ids[[length(cc_list$user_ids) + 1]] <- as.integer(parts[1])
        cc_list$user_names[[length(cc_list$user_names) + 1]] <-
          ifelse(length(parts) >= 2, parts[2], parts[1])
      }
    }
    result <- appr_tpl_update(id = tid, name = input$prv2_edit_name,
      description = input$prv2_edit_desc %||% "", category = input$prv2_edit_cat,
      form_fields = jsonlite::toJSON(fields, auto_unbox = TRUE),
      approver_config = jsonlite::toJSON(approvers, auto_unbox = TRUE),
      cc_config = if (length(cc_list$user_ids) > 0)
        jsonlite::toJSON(list(cc_list), auto_unbox = TRUE) else "[]")
    removeModal()
    if (result$success) { refresh_all(); showNotification("保存成功", type = "message") }
    else showNotification(result$message, type = "error")
  })

  # 确认删除模板
  observeEvent(input$prv2_confirm_del_tpl, {
    req(rv$logged_in, rv$prv2_del_tpl_id)
    result <- appr_tpl_delete(rv$prv2_del_tpl_id)
    removeModal()
    if (result$success) { refresh_all(); showNotification("已删除", type = "message") }
    else showNotification(result$message, type = "error")
  })

  ##################
  # 新建模板
  ##################
  observeEvent(input$prv2_new_tpl, {
    req(rv$logged_in)
    showModal(modalDialog(title = "新建审批模板", size = "l", easyClose = TRUE,
      textInput("prv2_new_name", "模板名称", placeholder = "如：请假申请"),
      textInput("prv2_new_desc", "描述", placeholder = "选填"),
      selectInput("prv2_new_cat", "分类",
        choices = c("通用" = "general", "请假" = "leave", "报销" = "expense",
                    "加班" = "overtime", "出差" = "travel", "审批" = "approval")),
      div(class = "prv2-tpl-builder",
        h6("表单字段"),
        textAreaInput("prv2_new_fields", NULL, rows = 4,
          value = "reason|申请事由|text\ndetail|详细说明|textarea")),
      div(class = "prv2-tpl-builder",
        h6("审批人"),
        textAreaInput("prv2_new_approvers", NULL, rows = 3,
          value = "主管审批|1\n部门经理|2")),
      div(class = "prv2-tpl-builder",
        h6("抄送人（可选）"),
        textAreaInput("prv2_new_cc", NULL, rows = 2)),
      footer = tagList(modalButton("取消"),
        actionButton("prv2_save_new_tpl", "保存", class = "btn-primary"))))
  })

  observeEvent(input$prv2_save_new_tpl, {
    req(rv$logged_in, input$prv2_new_name)
    fields <- list()
    for (line in strsplit(input$prv2_new_fields, "\n")[[1]]) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 2)
        fields[[length(fields) + 1]] <- list(
          key = parts[1], label = parts[2],
          type = ifelse(length(parts) >= 3, parts[3], "text"), required = TRUE)
    }
    approvers <- list()
    for (line in strsplit(input$prv2_new_approvers, "\n")[[1]]) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 2) {
        ids <- as.integer(trimws(strsplit(parts[2], ",")[[1]]))
        con <- db_connect()
        names <- tryCatch({
          n <- dbGetQuery(con, sprintf("SELECT display_name,username FROM users WHERE id IN (%s)",
            paste(ids, collapse = ",")))
          if (nrow(n) > 0) ifelse(is.na(n$display_name) | n$display_name == "",
            n$username, n$display_name) else as.character(ids)
        }, finally = { db_disconnect(con) })
        approvers[[length(approvers) + 1]] <- list(
          step_name = parts[1], operator_type = "fixed",
          approver_ids = as.list(ids), approver_names = as.list(names))
      }
    }
    cc_list <- list(user_ids = list(), user_names = list())
    for (line in strsplit(input$prv2_new_cc, "\n")[[1]]) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      if (length(parts) >= 1 && nchar(parts[1]) > 0) {
        cc_list$user_ids[[length(cc_list$user_ids) + 1]] <- as.integer(parts[1])
        cc_list$user_names[[length(cc_list$user_names) + 1]] <-
          ifelse(length(parts) >= 2, parts[2], parts[1])
      }
    }
    result <- appr_tpl_create(name = input$prv2_new_name,
      description = input$prv2_new_desc %||% "", category = input$prv2_new_cat,
      form_fields = jsonlite::toJSON(fields, auto_unbox = TRUE),
      approver_config = jsonlite::toJSON(approvers, auto_unbox = TRUE),
      cc_config = if (length(cc_list$user_ids) > 0)
        jsonlite::toJSON(list(cc_list), auto_unbox = TRUE) else "[]",
      created_by = current_uid())
    removeModal()
    if (result$success) { refresh_all(); showNotification(result$message, type = "message") }
    else showNotification(result$message, type = "error")
  })

  ##################
  # 示例模板
  ##################
  observeEvent(input$prv2_create_demo, {
    result <- appr_create_demo_tpl(created_by = current_uid())
    if (result$success) {
      refresh_all()
      showNotification(sprintf("已创建「%s」，请发布后使用", result$message),
        type = "message", duration = 5)
    } else showNotification(result$message, type = "error")
  })

  ##################
  # 全局刷新
  ##################
  observeEvent(input$prv2_refresh_all, refresh_all())
}