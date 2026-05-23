# 流程模块服务端 v3 — 一键体验流程

process_server <- function(input, output, session, rv) {

  process_refresh_trigger <- reactiveVal(0)

  ##################
  # 统计概览
  ##################
  output$proc_stat_total <- renderText({ process_refresh_trigger(); nrow(process_def_list()) })
  output$proc_stat_running <- renderText({ process_refresh_trigger(); nrow(process_instance_list(status = "running")) })
  output$proc_stat_completed <- renderText({ process_refresh_trigger(); nrow(process_instance_list(status = "completed")) })
  output$proc_stat_todos <- renderText({
    process_refresh_trigger()
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) nrow(process_get_todos(rv$current_user$id[1])) else 0
  })

  ##################
  # 我的待办
  ##################
  output$proc_todo_table <- DT::renderDT({
    process_refresh_trigger()
    if (is.null(rv$current_user) || nrow(rv$current_user) == 0) {
      return(DT::datatable(data.frame(信息 = "请先登录"), options = list(dom = 't')))
    }
    todos <- process_get_todos(rv$current_user$id[1])
    if (nrow(todos) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无待办任务"), options = list(dom = 't')))
    }
    display <- data.frame(
      流程名称 = todos$instance_title,
      流程编号 = todos$instance_no,
      节点名称 = todos$node_name,
      到达时间 = todos$entered_at,
      操作 = sprintf('<button class="btn btn-success btn-xs process-todo-btn" data-inst="%s" data-node="%s">✓ 处理</button>',
                      todos$instance_id, todos$node_instance_id),
      stringsAsFactors = FALSE
    )
    DT::datatable(display, escape = FALSE,
      options = list(pageLength = 10, dom = 'rtip', scrollX = TRUE),
      rownames = FALSE, class = 'cell-border stripe hover')
  })

  ##################
  # 流程定义
  ##################
  output$proc_def_table <- DT::renderDT({
    process_refresh_trigger()
    defs <- process_def_list(input$proc_def_status_filter)
    if (nrow(defs) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无流程定义"), options = list(dom = 't')))
    }
    display <- data.frame(
      编号 = defs$def_no,
      名称 = defs$name,
      分类 = defs$category,
      版本 = sprintf("v%d", defs$version),
      状态 = process_status_label(defs$status),
      创建人 = defs$creator_name %||% "",
      创建时间 = defs$created_at,
      操作 = ifelse(defs$status == "draft",
        sprintf('<button class="btn btn-info btn-xs process-publish-btn" data-id="%d">发布</button>', defs$id),
        sprintf('<button class="btn btn-success btn-xs process-start-btn" data-id="%d">启动</button>', defs$id)),
      stringsAsFactors = FALSE
    )
    DT::datatable(display, escape = FALSE,
      options = list(pageLength = 15, dom = 'rtip', scrollX = TRUE,
        columnDefs = list(list(targets = 7, orderable = FALSE))),
      rownames = FALSE, class = 'cell-border stripe hover')
  })

  ##################
  # 流程实例
  ##################
  output$proc_instance_table <- DT::renderDT({
    process_refresh_trigger()
    insts <- process_instance_list(input$proc_inst_status_filter)
    if (nrow(insts) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无流程实例"), options = list(dom = 't')))
    }
    current_nodes <- sapply(insts$id, function(id) {
      nodes <- process_get_active_nodes(id)
      active <- nodes[nodes$status == "active", ]
      if (nrow(active) > 0) sprintf("%s(%s)", active$node_name[1], process_status_label(active$node_type[1]))
      else "-"
    })
    display <- data.frame(
      实例编号 = insts$instance_no,
      流程名称 = insts$def_name %||% insts$title,
      标题 = insts$title,
      状态 = process_status_label(insts$status),
      当前节点 = current_nodes,
      启动人 = insts$started_by_name %||% "",
      启动时间 = insts$started_at,
      操作 = sprintf('<button class="btn btn-default btn-xs process-inst-log-btn" data-inst="%s">日志</button>', insts$id),
      stringsAsFactors = FALSE
    )
    DT::datatable(display, escape = FALSE,
      options = list(pageLength = 15, dom = 'rtip', scrollX = TRUE,
        columnDefs = list(list(targets = 7, orderable = FALSE))),
      rownames = FALSE, class = 'cell-border stripe hover')
  })

  ##################
  # 监控日志
  ##################
  observe({
    insts <- process_instance_list()
    choices <- stats::setNames(insts$id, sprintf("%s - %s", insts$instance_no, insts$title))
    updateSelectInput(session, "proc_log_inst_select", choices = c("请选择" = "", choices))
  })

  output$proc_log_table <- DT::renderDT({
    req(input$proc_log_inst_select)
    process_refresh_trigger()
    logs <- process_get_logs(as.integer(input$proc_log_inst_select))
    if (nrow(logs) == 0) return(DT::datatable(data.frame(信息 = "暂无日志"), options = list(dom = 't')))
    display <- data.frame(
      时间 = logs$created_at, 级别 = logs$log_level, 类型 = logs$log_type,
      节点 = logs$node_id %||% "", 消息 = logs$message,
      耗时 = ifelse(is.na(logs$duration_ms), "", sprintf("%dms", logs$duration_ms)),
      stringsAsFactors = FALSE
    )
    DT::datatable(display, options = list(pageLength = 20, dom = 'rtip', scrollX = TRUE),
      rownames = FALSE, class = 'cell-border stripe hover')
  })

  ##################
  # 操作处理
  ##################

  observeEvent(input$proc_refresh_defs, { process_refresh_trigger(process_refresh_trigger() + 1) })
  observeEvent(input$proc_refresh_insts, { process_refresh_trigger(process_refresh_trigger() + 1) })
  observeEvent(input$proc_refresh_logs, { process_refresh_trigger(process_refresh_trigger() + 1) })

  # ===== 一键创建并启动示例流程 =====
  observeEvent(input$proc_create_demo, {
    user_id <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- process_create_and_start_demo(started_by = user_id)
    process_refresh_trigger(process_refresh_trigger() + 1)
    if (result$success) {
      showModal(modalDialog(
        title = "🎉 示例流程已就绪",
        size = "m",
        div(
          h4("流程信息"),
          p(sprintf("定义编号: %s", result$def_no)),
          p(sprintf("实例编号: %s", result$instance_no)),
          hr(),
          h4("下一步操作"),
          p("当前流程已自动推进到「审批确认」节点，请按以下步骤操作："),
          div(style = "background:#f0f8ff; padding:12px; border-radius:6px;",
            p("1️⃣ 点击上方标签切换到「我的待办」"),
            p("2️⃣ 找到该流程记录"),
            p("3️⃣ 点击「✓ 处理」按钮完成审批"),
            p("4️⃣ 观察「流程实例」中的状态变化")
          ),
          hr(),
          p("您也可以自行创建流程定义 → 发布 → 启动，体验完整流程。",
            style = "color:#666; font-size:13px;")
        ),
        footer = tagList(
          actionButton("proc_goto_todo", "前往我的待办", class = "btn-success",
                       icon = icon("clipboard-list"),
                       onclick = "$('.nav-tabs li:eq(0) a').tab('show');"),
          modalButton("关闭")
        ),
        easyClose = TRUE
      ))
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # ===== 新建流程定义 =====
  observeEvent(input$proc_create_def, {
    showModal(modalDialog(
      title = "新建流程定义",
      textInput("proc_new_def_name", "流程名称"),
      textInput("proc_new_def_desc", "描述"),
      selectInput("proc_new_def_category", "分类",
                  choices = c("通用" = "general", "故障" = "fault", "变更" = "change", "审批" = "approval")),
      textAreaInput("proc_new_def_json", "流程定义 JSON", rows = 10,
        value = '{\n  "nodes": [\n    { "id": "start", "type": "start", "label": "开始" },\n    { "id": "task1", "type": "task", "label": "审批确认" },\n    { "id": "end", "type": "end", "label": "结束" }\n  ],\n  "transitions": [\n    { "from": "start", "to": "task1", "condition": "" },\n    { "from": "task1", "to": "end", "condition": "" }\n  ]\n}'),
      footer = tagList(modalButton("取消"), actionButton("proc_save_new_def", "创建", class = "btn-primary"))
    ))
  })

  observeEvent(input$proc_save_new_def, {
    req(input$proc_new_def_name)
    result <- process_def_create(
      name = input$proc_new_def_name,
      description = input$proc_new_def_desc %||% "",
      category = input$proc_new_def_category,
      definition = input$proc_new_def_json,
      created_by = if (!is.null(rv$current_user)) rv$current_user$id[1] else NULL)
    removeModal()
    if (result$success) {
      process_refresh_trigger(process_refresh_trigger() + 1)
      showNotification(sprintf("%s，需要发布后才能启动", result$message), type = "message", duration = 8)
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # ===== 发布流程定义 =====
  observeEvent(input$process_publish_click, {
    result <- process_def_publish(input$process_publish_click, change_log = "手动发布")
    if (result$success) {
      process_refresh_trigger(process_refresh_trigger() + 1)
      showNotification(result$message, type = "message", duration = 5)
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # ===== 启动流程实例 =====
  observeEvent(input$process_start_click, {
    def_id <- as.integer(input$process_start_click)
    def <- process_def_get(def_id)
    if (is.null(def)) { showNotification("流程定义不存在", type = "error"); return() }
    showModal(modalDialog(
      title = sprintf("启动流程: %s", def$name[1]),
      textInput("proc_start_title", "实例标题", value = def$name[1]),
      textAreaInput("proc_start_context", "上下文数据 (JSON)", rows = 6,
        value = '{\n  "title": "示例",\n  "priority": "normal"\n}'),
      footer = tagList(modalButton("取消"), actionButton("proc_confirm_start", "启动", class = "btn-success"))
    ))
  })

  observeEvent(input$proc_confirm_start, {
    req(input$process_start_click)
    context <- tryCatch(jsonlite::fromJSON(input$proc_start_context, simplifyVector = FALSE),
                        error = function(e) list(title = input$proc_start_title, priority = "normal"))
    result <- process_instance_start(
      def_id = as.integer(input$process_start_click),
      title = input$proc_start_title,
      context_data = context,
      started_by = if (!is.null(rv$current_user)) rv$current_user$id[1] else NULL)
    removeModal()
    if (result$success) {
      process_refresh_trigger(process_refresh_trigger() + 1)
      advance <- process_advance(result$id)
      process_refresh_trigger(process_refresh_trigger() + 1)
      if (advance$success) {
        if (isTRUE(advance$completed)) {
          showNotification(sprintf("%s → %s", result$message, advance$message), type = "success", duration = 5)
        } else {
          showNotification(sprintf("%s，当前在「%s」节点", result$message, advance$next_node_label), type = "message", duration = 8)
          if (advance$next_node_type == "task") {
            showNotification("请在「我的待办」中处理该任务", type = "warning", duration = 8)
          }
        }
      } else {
        showNotification(sprintf("%s（推进异常: %s）", result$message, advance$message), type = "warning", duration = 8)
      }
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # ===== 查看日志 =====
  observeEvent(input$process_inst_log_click, {
    inst_id <- as.integer(input$process_inst_log_click)
    instance <- process_instance_get(inst_id)
    if (is.null(instance)) { showNotification("实例不存在", type = "error"); return() }
    logs <- process_get_logs(inst_id)
    events <- process_get_events(inst_id)
    nodes <- process_get_active_nodes(inst_id)
    log_text <- paste(
      sprintf("=== 流程实例: %s ===\n状态: %s | 当前节点: %s\n启动: %s\n\n",
              instance$instance_no[1], instance$status[1], instance$current_node[1] %||% "-", instance$started_at[1]),
      "--- 节点状态 ---\n",
      paste(capture.output(if (nrow(nodes) > 0) print(nodes[, c("node_id", "node_name", "node_type", "status", "entered_at", "completed_at")]) else "无"), collapse = "\n"),
      "\n\n--- 事件日志 ---\n",
      paste(capture.output(if (nrow(events) > 0) print(events[, c("event_type", "status", "message", "created_at")]) else "无"), collapse = "\n"),
      "\n\n--- 运行日志 ---\n",
      paste(capture.output(if (nrow(logs) > 0) print(logs[, c("log_level", "log_type", "message", "created_at")]) else "无"), collapse = "\n"),
      sep = "")
    showModal(modalDialog(
      title = sprintf("流程实例日志: %s", instance$instance_no[1]), size = "l",
      pre(log_text, style = "font-size:12px; max-height:500px; overflow:auto; background:#f5f5f5; padding:10px;"),
      footer = modalButton("关闭"), easyClose = TRUE))
  })

  # ===== 待办处理 =====
  observeEvent(input$process_todo_click, {
    click_data <- input$process_todo_click
    instance_id <- as.integer(click_data$instance_id)
    node_instance_id <- as.integer(click_data$node_id)
    con <- db_connect()
    tryCatch({
      dbExecute(con, sprintf(
        "UPDATE process_nodes SET status = 'completed', result = 'approved',
         completed_at = datetime('now','localtime'), remark = '已处理' WHERE id = %d", node_instance_id))
    }, finally = { db_disconnect(con) })
    process_log_write(instance_id, as.character(node_instance_id), "info", "task_complete", "用户已处理待办任务")
    process_event_record("node_complete", instance_id, as.character(node_instance_id), source = "user",
                         status = "success", message = "待办已处理")
    advance <- process_advance(instance_id)
    process_refresh_trigger(process_refresh_trigger() + 1)
    if (advance$success) {
      if (isTRUE(advance$completed)) {
        showModal(modalDialog(
          title = "✅ 流程已完成",
          p("该流程实例已全部走完，请在「流程实例」中查看最终状态。"),
          footer = modalButton("知道了"),
          easyClose = TRUE
        ))
      } else {
        showNotification(sprintf("已处理，%s", advance$message), type = "message", duration = 5)
      }
    } else {
      showNotification(sprintf("处理完成，但流转异常: %s", advance$message), type = "warning", duration = 8)
    }
  })
}
