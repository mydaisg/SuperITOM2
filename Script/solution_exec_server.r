# 方案执行模块 - 服务端 v3
# 优化：操作列(编辑/删除)、问题管理下拉选择、配置管理用系统数据源

solution_exec_server <- function(input, output, session, rv) {

  exec_trigger <- reactiveVal(0)
  exec_project_id <- reactiveVal(NULL)

  # 彩虹色
  rainbow_colors <- c("#e74c3c","#e67e22","#f1c40f","#2ecc71","#1abc9c","#3498db","#9b59b6","#e91e63",
    "#00bcd4","#ff5722","#795548","#607d8b","#8bc34a","#673ab7","#03a9f4","#cddc39")

  .safe <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)

  output$exec_has_project <- reactive({ !is.null(exec_project_id()) })
  outputOptions(output, "exec_has_project", suspendWhenHidden = FALSE)

  # ── 项目选择/创建 ──
  output$exec_project_selector <- renderUI({
    exec_trigger()
    projects <- exec_project_get_all()
    choices <- if (nrow(projects) > 0) setNames(as.character(projects$id), projects$name) else c("— 暂无项目 —" = "")
    tagList(
      div(style = "display:flex; gap:8px; align-items:center; margin-bottom:12px;",
        selectInput("exec_project_select", NULL, choices = choices, width = "300px",
          selected = exec_project_id()),
        actionButton("exec_project_new_btn", "新建执行项目", icon = icon("plus"), class = "btn-success btn-sm"),
        if (!is.null(exec_project_id())) actionButton("exec_project_del_btn", "删除", icon = icon("trash"), class = "btn-danger btn-sm")
      )
    )
  })

  observeEvent(input$exec_project_select, {
    if (input$exec_project_select != "") exec_project_id(as.integer(input$exec_project_select))
  })

  observeEvent(input$exec_project_new_btn, {
    req(rv$logged_in)
    showModal(modalDialog(title = "新建执行项目", size = "s",
      textInput("exec_new_name", "项目名称*", placeholder = "如: LVCC_协同平台_培训与试运行"),
      textAreaInput("exec_new_desc", "描述", rows = 2),
      footer = tagList(modalButton("取消"), actionButton("exec_new_save", "创建", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$exec_new_save, {
    req(rv$logged_in, input$exec_new_name)
    result <- exec_project_add(input$exec_new_name, input$exec_new_desc)
    if (result$success) { removeModal(); exec_trigger(exec_trigger() + 1) }
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  observeEvent(input$exec_project_del_btn, {
    req(rv$logged_in, exec_project_id())
    showModal(modalDialog(title = "确认删除项目",
      "删除项目将同时删除所有关联任务，不可恢复。",
      footer = tagList(modalButton("取消"), actionButton("exec_project_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$exec_project_del_confirm, {
    req(exec_project_id())
    result <- exec_project_delete(exec_project_id())
    removeModal(); exec_project_id(NULL); exec_trigger(exec_trigger() + 1)
    showNotification(result$message, type = "warning")
  })

  # ── 任务板块渲染 ──
  task_types <- list(
    train = "培训计划",
    pilot = "试运行计划",
    basic = "基础信息维护",
    issue = "问题管理"
  )
  test_types <- list(
    test_hr = "人力资源测试用例",
    test_admin = "行政管理测试用例",
    test_fin = "财务管理测试用例",
    test_it = "IT管理测试用例"
  )

  # 操作按钮 HTML 模板
  action_btns <- function(id) {
    sprintf(paste0(
      '<button class="btn btn-xs btn-warning" style="margin-right:3px;" ',
      'onclick="Shiny.setInputValue(\'exec_edit_row\',{id:%d},{priority:\'event\'});" title="编辑">✏</button>',
      '<button class="btn btn-xs btn-danger" ',
      'onclick="if(confirm(\'确认删除？\'))Shiny.setInputValue(\'exec_del_row\',{id:%d},{priority:\'event\'});" title="删除">🗑</button>'),
      id, id)
  }

  # 渲染一个任务板块的表格
  render_task_table <- function(type_key, output_name) {
    output[[output_name]] <- DT::renderDataTable({
      exec_trigger()
      pid <- exec_project_id()
      if (is.null(pid)) return(data.frame(提示 = "请先选择或创建执行项目"))
      items <- exec_task_get_by_project(pid, type_key)
      if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))
      disp <- data.frame(
        ID = items$id,
        操作 = sapply(items$id, action_btns),
        序号 = items$seq %||% "",
        模块 = items$module %||% "", 内容 = items$content %||% "",
        对象 = items$target %||% "", 时长 = items$duration %||% "",
        方式 = items$method %||% "", 计划日期 = items$plan_date %||% "",
        负责人 = items$responsible %||% "", 部门 = items$department %||% "",
        状态 = sprintf('<select class="exec-cell-status" data-id="%d" onchange="Shiny.setInputValue(\'exec_cell_update\',{id:%d,field:\'status\',val:this.value},{priority:\'event\'})"><option selected>%s</option>%s</select>',
          items$id, items$id, items$status %||% "--",
          paste0('<option>待开始</option><option>进行中</option><option>已完成</option><option>已延期</option>')),
        备注 = items$remark %||% "",
        stringsAsFactors = FALSE, check.names = FALSE
      )
      DT::datatable(disp, rownames = FALSE, escape = FALSE,
        options = list(pageLength = 50, dom = "ltip", scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))),
        class = "cell-border stripe compact")
    })
  }

  # 渲染用例表格
  render_test_table <- function(type_key, output_name) {
    output[[output_name]] <- DT::renderDataTable({
      exec_trigger()
      pid <- exec_project_id()
      if (is.null(pid)) return(data.frame(提示 = "请先选择或创建执行项目"))
      items <- exec_task_get_by_project(pid, type_key)
      if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))
      disp <- data.frame(
        ID = items$id,
        操作 = sapply(items$id, action_btns),
        编号 = items$seq %||% "",
        测试模块 = items$module %||% "", 测试场景 = items$content %||% "",
        测试步骤 = items$target %||% "", 预期结果 = items$duration %||% "",
        实际结果 = items$method %||% "",
        状态 = sprintf('<select class="exec-cell-status" data-id="%d" onchange="Shiny.setInputValue(\'exec_cell_update\',{id:%d,field:\'status\',val:this.value},{priority:\'event\'})"><option selected>%s</option>%s</select>',
          items$id, items$id, items$status %||% "--",
          paste0('<option>待测试</option><option>测试中</option><option>已通过</option><option>有问题</option>')),
        优先级 = items$priority %||% "", 测试人 = items$tester %||% "",
        测试日期 = items$test_date %||% "", 问题 = items$issue_desc %||% "",
        备注 = items$remark %||% "",
        stringsAsFactors = FALSE, check.names = FALSE
      )
      DT::datatable(disp, rownames = FALSE, escape = FALSE,
        options = list(pageLength = 50, dom = "ltip", scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))),
        class = "cell-border stripe compact")
    })
  }

  # 渲染问题管理表格（带下拉选择配置项）
  output$exec_issue_table_v2 <- DT::renderDataTable({
    exec_trigger()
    pid <- exec_project_id()
    if (is.null(pid)) return(data.frame(提示 = "请先选择或创建执行项目"))
    items <- exec_task_get_by_project(pid, "issue")
    if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))

    # 从 exec_config 读取可选项
    modules <- exec_config_get("module")
    module_opts <- if (nrow(modules) > 0) paste0('<option>', modules$value, '</option>', collapse = "") else ""
    issue_types <- exec_config_get("issue_type")
    type_opts <- if (nrow(issue_types) > 0) paste0('<option>', issue_types$value, '</option>', collapse = "") else
      '<option>功能缺陷</option><option>性能问题</option><option>界面问题</option><option>数据问题</option><option>其他</option>'
    severities <- exec_config_get("severity")
    sev_opts <- if (nrow(severities) > 0) paste0('<option>', severities$value, '</option>', collapse = "") else
      '<option>致命</option><option>严重</option><option>一般</option><option>轻微</option><option>建议</option>'

    disp <- data.frame(
      ID = items$id,
      操作 = sapply(items$id, action_btns),
      编号 = items$seq %||% "",
      问题标题 = items$content %||% "",
      所属模块 = sprintf('<select class="exec-cell-status" data-id="%d" onchange="Shiny.setInputValue(\'exec_cell_update\',{id:%d,field:\'module\',val:this.value},{priority:\'event\'})"><option selected>%s</option>%s</select>',
        items$id, items$id, items$module %||% "--", module_opts),
      问题类型 = sprintf('<select class="exec-cell-status" data-id="%d" onchange="Shiny.setInputValue(\'exec_cell_update\',{id:%d,field:\'target\',val:this.value},{priority:\'event\'})"><option selected>%s</option>%s</select>',
        items$id, items$id, items$target %||% "--", type_opts),
      严重程度 = sprintf('<select class="exec-cell-status" data-id="%d" onchange="Shiny.setInputValue(\'exec_cell_update\',{id:%d,field:\'priority\',val:this.value},{priority:\'event\'})"><option selected>%s</option>%s</select>',
        items$id, items$id, items$priority %||% "--", sev_opts),
      描述 = items$duration %||% "", 提交人 = items$tester %||% "",
      日期 = items$test_date %||% "",
      状态 = sprintf('<select class="exec-cell-status" data-id="%d" onchange="Shiny.setInputValue(\'exec_cell_update\',{id:%d,field:\'status\',val:this.value},{priority:\'event\'})"><option selected>%s</option>%s</select>',
        items$id, items$id, items$status %||% "--",
        paste0('<option>待处理</option><option>处理中</option><option>已修复</option><option>已关闭</option>')),
      责任人 = items$responsible %||% "", 建议 = items$remark %||% "",
      stringsAsFactors = FALSE, check.names = FALSE
    )
    DT::datatable(disp, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 50, dom = "ltip", scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))),
      class = "cell-border stripe compact")
  })

  # 注册所有板块输出
  for (nm in names(task_types)) {
    render_task_table(nm, paste0("exec_tab_", nm))
  }
  for (nm in names(test_types)) {
    render_test_table(nm, paste0("exec_tab_", nm))
  }

  # ── 行内编辑 ──
  observeEvent(input$exec_cell_update, {
    req(rv$logged_in)
    data <- input$exec_cell_update
    result <- exec_task_update(as.integer(data$id), setNames(list(data$val), data$field))
    exec_trigger(exec_trigger() + 1)
  })

  # ── 行编辑弹窗 ──
  observeEvent(input$exec_edit_row, {
    req(rv$logged_in)
    id <- as.integer(input$exec_edit_row$id)
    con <- db_connect()
    item <- tryCatch({
      dbGetQuery(con, sprintf("SELECT * FROM exec_tasks WHERE id = %d", id))
    }, error = function(e) NULL, finally = { db_disconnect(con) })
    if (is.null(item) || nrow(item) == 0) return()
    item <- item[1, ]

    showModal(modalDialog(title = paste("编辑任务 #", id), size = "m",
      textInput("exec_edit_seq", "序号/编号", value = item$seq %||% ""),
      textInput("exec_edit_module", "模块", value = item$module %||% ""),
      textAreaInput("exec_edit_content", "内容", value = item$content %||% "", rows = 2),
      textInput("exec_edit_target", "对象/场景", value = item$target %||% ""),
      textInput("exec_edit_duration", "时长/预期", value = item$duration %||% ""),
      textInput("exec_edit_method", "方式/实际", value = item$method %||% ""),
      textInput("exec_edit_date", "计划日期", value = item$plan_date %||% ""),
      textInput("exec_edit_responsible", "负责人", value = item$responsible %||% ""),
      textInput("exec_edit_department", "部门", value = item$department %||% ""),
      textInput("exec_edit_remark", "备注", value = item$remark %||% ""),
      footer = tagList(
        tags$span(style = "color:#999;font-size:11px;", "编辑后自动保存"),
        modalButton("关闭")
      ),
      easyClose = TRUE
    ))
    # 监听编辑弹窗中的字段变更，自动保存
    observeEvent(input$exec_edit_seq, {
      exec_task_update(id, seq = input$exec_edit_seq)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_module, {
      exec_task_update(id, module = input$exec_edit_module)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_content, {
      exec_task_update(id, content = input$exec_edit_content)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_target, {
      exec_task_update(id, target = input$exec_edit_target)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_duration, {
      exec_task_update(id, duration = input$exec_edit_duration)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_method, {
      exec_task_update(id, method = input$exec_edit_method)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_date, {
      exec_task_update(id, plan_date = input$exec_edit_date)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_responsible, {
      exec_task_update(id, responsible = input$exec_edit_responsible)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_department, {
      exec_task_update(id, department = input$exec_edit_department)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
    observeEvent(input$exec_edit_remark, {
      exec_task_update(id, remark = input$exec_edit_remark)
      exec_trigger(exec_trigger() + 1)
    }, ignoreInit = TRUE)
  })

  # ── 行删除 ──
  observeEvent(input$exec_del_row, {
    req(rv$logged_in)
    id <- as.integer(input$exec_del_row$id)
    result <- exec_task_delete(id)
    exec_trigger(exec_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 添加任务弹窗 ──
  observeEvent(input$exec_task_add_btn, {
    req(rv$logged_in, exec_project_id())
    showModal(modalDialog(title = "添加任务", size = "m",
      selectInput("exec_add_type", "任务类型",
        choices = c("培训计划"="train","试运行计划"="pilot","基础信息维护"="basic",
          "人力资源测试用例"="test_hr","行政管理测试用例"="test_admin",
          "财务管理测试用例"="test_fin","IT管理测试用例"="test_it","问题管理"="issue")),
      textInput("exec_add_seq", "序号/编号"),
      textInput("exec_add_module", "模块"),
      textAreaInput("exec_add_content", "内容", rows = 2),
      textInput("exec_add_target", "对象/场景"),
      textInput("exec_add_duration", "时长/预期"),
      textInput("exec_add_method", "方式/实际"),
      textInput("exec_add_date", "计划日期"),
      textInput("exec_add_responsible", "负责人"),
      textInput("exec_add_department", "部门"),
      selectInput("exec_add_status", "状态",
        choices = c("待开始","进行中","已完成","待测试","测试中","已通过","待处理","处理中","已修复")),
      footer = tagList(modalButton("取消"), actionButton("exec_task_save", "保存", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$exec_task_save, {
    req(rv$logged_in, exec_project_id())
    result <- exec_task_add(
      project_id = exec_project_id(), task_type = input$exec_add_type,
      seq = input$exec_add_seq, module = input$exec_add_module,
      content = input$exec_add_content, target = input$exec_add_target,
      duration = input$exec_add_duration, method = input$exec_add_method,
      plan_date = input$exec_add_date, responsible = input$exec_add_responsible,
      department = input$exec_add_department, status = input$exec_add_status
    )
    if (result$success) removeModal()
    exec_trigger(exec_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # ── 配置管理（模块/部门/人员等可选项） ──
  # 部门从系统 departments 表读取，人员从 users 表读取，不再允许手动创建
  output$exec_config_ui <- renderUI({
    exec_trigger()
    modules <- exec_config_get("module")
    issue_types <- exec_config_get("issue_type")
    severities <- exec_config_get("severity")

    # 从系统 departments 表读取
    sys_depts <- tryCatch({
      con <- db_connect()
      on.exit(db_disconnect(con))
      dbGetQuery(con, "SELECT id, name FROM departments ORDER BY sort_order, name")
    }, error = function(e) data.frame())

    # 从系统 users 表读取
    sys_persons <- tryCatch({
      con <- db_connect()
      on.exit(db_disconnect(con))
      dbGetQuery(con, "SELECT id, COALESCE(NULLIF(display_name,''), username) as display_name FROM users WHERE active = 1 ORDER BY username")
    }, error = function(e) data.frame())

    tagList(
      h5("模块管理（自定义配置项）"),
      div(style = "display:flex; gap:4px; flex-wrap:wrap; margin-bottom:8px;",
        lapply(seq_len(nrow(modules)), function(i) {
          tags$span(style = sprintf("background:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px; cursor:pointer;",
            rainbow_colors[((i-1) %% length(rainbow_colors)) + 1]),
            modules$value[i],
            tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('exec_config_del',{id:%d},{priority:'event'});return false;", modules$id[i]),
              style = "color:rgba(255,255,255,0.6); margin-left:4px; text-decoration:none;", "✕"))
        })
      ),
      textInput("exec_new_module", NULL, placeholder = "新模块名..."),
      actionButton("exec_add_module", "添加模块", class = "btn-xs btn-info"),
      hr(),
      h5("问题类型管理"),
      div(style = "display:flex; gap:4px; flex-wrap:wrap; margin-bottom:8px;",
        lapply(seq_len(nrow(issue_types)), function(i) {
          tags$span(style = sprintf("background:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px; cursor:pointer;",
            rainbow_colors[((i-1) %% length(rainbow_colors)) + 1]),
            issue_types$value[i],
            tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('exec_config_del',{id:%d},{priority:'event'});return false;", issue_types$id[i]),
              style = "color:rgba(255,255,255,0.6); margin-left:4px; text-decoration:none;", "✕"))
        })
      ),
      textInput("exec_new_issue_type", NULL, placeholder = "新问题类型..."),
      actionButton("exec_add_issue_type", "添加问题类型", class = "btn-xs btn-info"),
      hr(),
      h5("严重程度管理"),
      div(style = "display:flex; gap:4px; flex-wrap:wrap; margin-bottom:8px;",
        lapply(seq_len(nrow(severities)), function(i) {
          tags$span(style = sprintf("background:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px; cursor:pointer;",
            rainbow_colors[((i-1) %% length(rainbow_colors)) + 1]),
            severities$value[i],
            tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('exec_config_del',{id:%d},{priority:'event'});return false;", severities$id[i]),
              style = "color:rgba(255,255,255,0.6); margin-left:4px; text-decoration:none;", "✕"))
        })
      ),
      textInput("exec_new_severity", NULL, placeholder = "新严重程度..."),
      actionButton("exec_add_severity", "添加严重程度", class = "btn-xs btn-info"),
      hr(),
      h5(icon("database"), " 系统部门（只读，来自组织架构）"),
      div(style = "display:flex; gap:4px; flex-wrap:wrap; margin-bottom:8px;",
        if (nrow(sys_depts) > 0) {
          lapply(seq_len(nrow(sys_depts)), function(i) {
            tags$span(style = sprintf("background:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px;",
              rainbow_colors[((i-1) %% length(rainbow_colors)) + 1]),
              sys_depts$name[i])
          })
        } else {
          tags$span(style = "color:#999; font-size:11px;", "暂无部门，请在 管理→组织架构 中添加")
        }
      ),
      hr(),
      h5(icon("users"), " 系统人员（只读，来自用户管理）"),
      div(style = "display:flex; gap:4px; flex-wrap:wrap; margin-bottom:8px;",
        if (nrow(sys_persons) > 0) {
          lapply(seq_len(min(nrow(sys_persons), 30)), function(i) {
            tags$span(style = sprintf("background:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px;",
              rainbow_colors[((i-1) %% length(rainbow_colors)) + 1]),
              sys_persons$display_name[i])
          })
        } else {
          tags$span(style = "color:#999; font-size:11px;", "暂无人员")
        }
      )
    )
  })

  observeEvent(input$exec_add_module, {
    if (trimws(input$exec_new_module) != "") exec_config_add("module", input$exec_new_module)
    exec_trigger(exec_trigger() + 1); updateTextInput(session, "exec_new_module", value = "")
  })
  observeEvent(input$exec_add_issue_type, {
    if (trimws(input$exec_new_issue_type) != "") exec_config_add("issue_type", input$exec_new_issue_type)
    exec_trigger(exec_trigger() + 1); updateTextInput(session, "exec_new_issue_type", value = "")
  })
  observeEvent(input$exec_add_severity, {
    if (trimws(input$exec_new_severity) != "") exec_config_add("severity", input$exec_new_severity)
    exec_trigger(exec_trigger() + 1); updateTextInput(session, "exec_new_severity", value = "")
  })
  observeEvent(input$exec_config_del, {
    exec_config_delete(as.integer(input$exec_config_del$id))
    exec_trigger(exec_trigger() + 1)
  })
}
