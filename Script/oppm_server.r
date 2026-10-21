# ================================================================
# OPPM 一页纸项目管理 - 服务端
# 一页纸视图（阶段/任务 × 周进度 × RACI）+ 角色/任务管理 + 进度/RACI 点击编辑
# ================================================================

oppm_server <- function(input, output, session, rv) {

  ov <- reactiveValues(
    plan_id = NULL,
    refresh = 0
  )

  esc <- function(x) {
    if (is.null(x) || is.na(x)) return("")
    x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x); x <- gsub('"', "&quot;", x)
    x <- gsub("'", "&#39;", x); x
  }

  # 项目下拉选项（用于新建计划）
  oppm_project_choices <- reactive({
    ov$refresh
    projs <- project_get_all("all", rv$current_user)
    if (nrow(projs) > 0) {
      setNames(as.character(projs$id), projs$name)
    } else { c() }
  })

  # ================================================================
  # 计划选择下拉
  # ================================================================
  output$oppm_plan_filter_ui <- renderUI({
    ov$refresh
    plans <- oppm_plan_get_all()
    choices <- c("请选择计划" = "")
    if (nrow(plans) > 0) {
      labs <- ifelse(is.na(plans$project_name) | plans$project_name == "",
                     plans$plan_no,
                     sprintf("%s - %s (%s)", plans$project_name, plans$version, plans$plan_no))
      choices <- c(choices, setNames(as.character(plans$id), labs))
    }
    selectInput("oppm_plan_select", "选择 OPPM 计划", choices = choices,
                selected = if (!is.null(ov$plan_id)) as.character(ov$plan_id) else "")
  })

  observeEvent(input$oppm_plan_select, {
    if (!is.null(input$oppm_plan_select) && input$oppm_plan_select != "") {
      ov$plan_id <- as.integer(input$oppm_plan_select)
    } else {
      ov$plan_id <- NULL
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$oppm_refresh, { ov$refresh <- ov$refresh + 1 })

  # ================================================================
  # 进度 / RACI 点击循环
  # ================================================================
  prog_cycle <- function(mark, color) {
    if (is.null(mark) || mark == "none") return(list(mark = "filled", color = "green"))
    if (mark == "filled" && color == "green")  return(list(mark = "filled", color = "yellow"))
    if (mark == "filled" && color == "yellow") return(list(mark = "filled", color = "red"))
    if (mark == "filled" && color == "red")    return(list(mark = "hollow", color = "green"))
    if (mark == "hollow" && color == "green")  return(list(mark = "hollow", color = "yellow"))
    if (mark == "hollow" && color == "yellow") return(list(mark = "hollow", color = "red"))
    if (mark == "hollow" && color == "red")    return(list(mark = "none", color = "green"))
    list(mark = "none", color = "green")
  }
  raci_cycle <- function(code) {
    codes <- c("A", "R", "C", "I", "S")
    if (is.null(code) || !code %in% codes) return("A")
    idx <- match(code, codes)
    if (idx >= length(codes)) return("")
    codes[idx + 1]
  }

  # ================================================================
  # 单元格渲染辅助
  # ================================================================
  render_prog_cell <- function(task_id, week, mark, color) {
    if (is.null(mark) || is.na(mark)) mark <- "none"
    if (is.null(color) || is.na(color)) color <- "green"
    if (mark == "none") {
      inner <- '<span style="color:#e3e3e3; font-size:12px;">&#9675;</span>'
    } else {
      col <- switch(color, green = "#5cb85c", yellow = "#f0ad4e", red = "#d9534f", "#ccc")
      sym <- if (mark == "filled") "&#9679;" else "&#9675;"
      inner <- sprintf('<span style="color:%s; font-size:14px;">%s</span>', col, sym)
    }
    sprintf('<div class="oppm-prog-cell" data-task="%d" data-week="%d" data-mark="%s" data-color="%s" title="点击切换进度状态" style="width:30px; min-width:30px; flex-shrink:0; text-align:center; cursor:pointer; display:flex; align-items:center; justify-content:center; height:100%%;">%s</div>',
      as.integer(task_id), as.integer(week), mark, color, inner)
  }

  render_raci_cell <- function(task_id, role_id, code) {
    if (is.null(code) || is.na(code)) code <- ""
    col <- switch(code, A = "#d9534f", R = "#337ab7", C = "#f0ad4e", I = "#5bc0de", S = "#5cb85c", "#bbb")
    txt <- if (code == "") '<span style="color:#ddd;">&middot;</span>' else code
    sprintf('<div class="oppm-raci-cell" data-task="%d" data-role="%d" data-code="%s" title="点击切换RACI角色" style="width:30px; min-width:30px; flex-shrink:0; text-align:center; cursor:pointer; font-weight:bold; font-size:12px; color:%s; display:flex; align-items:center; justify-content:center; height:100%%;">%s</div>',
      as.integer(task_id), as.integer(role_id), code, col, txt)
  }

  # ================================================================
  # 一页纸视图渲染
  # ================================================================
  output$oppm_chart_ui <- renderUI({
    ov$refresh
    if (is.null(ov$plan_id)) {
      return(div(style = "padding:40px; text-align:center; color:#999;",
        icon("file-text", "fa-3x"), br(), br(),
        "请选择或新建一个 OPPM 计划，用于一页纸项目管理。"))
    }
    plan <- oppm_plan_get_by_id(ov$plan_id)
    if (nrow(plan) == 0) {
      return(div(style = "padding:20px; color:#999;", "计划不存在或已删除"))
    }
    plan <- plan[1, ]

    roles <- oppm_role_get_by_plan(ov$plan_id)
    tasks <- oppm_task_get_by_plan(ov$plan_id)
    raci  <- oppm_raci_get_by_plan(ov$plan_id)
    progs <- oppm_progress_get_by_plan(ov$plan_id)

    week_count <- if (!is.na(plan$week_count) && plan$week_count > 0) as.integer(plan$week_count) else 20
    start_date <- if (!is.null(plan$start_date) && !is.na(plan$start_date) && plan$start_date != "") {
      tryCatch(as.Date(plan$start_date), error = function(e) Sys.Date())
    } else Sys.Date()

    # 周日期序列
    week_dates <- start_date + (seq_len(week_count) - 1) * 7

    # 数据映射
    raci_map <- new.env(hash = TRUE)
    if (nrow(raci) > 0) {
      for (i in 1:nrow(raci)) {
        key <- paste(raci$task_id[i], raci$role_id[i], sep = "_")
        raci_map[[key]] <- raci$raci_code[i]
      }
    }
    prog_map <- new.env(hash = TRUE)
    if (nrow(progs) > 0) {
      for (i in 1:nrow(progs)) {
        key <- paste(progs$task_id[i], progs$week_index[i], sep = "_")
        prog_map[[key]] <- list(mark = progs$mark[i], color = progs$color[i])
      }
    }

    get_raci <- function(task_id, role_id) {
      key <- paste(task_id, role_id, sep = "_")
      if (exists(key, envir = raci_map)) get(key, envir = raci_map) else ""
    }
    get_prog <- function(task_id, week) {
      key <- paste(task_id, week, sep = "_")
      if (exists(key, envir = prog_map)) get(key, envir = prog_map) else list(mark = "none", color = "green")
    }

    # ---- 表头信息条 ----
    proj_name <- if (!is.null(plan$project_name)) plan$project_name else ""
    header_html <- sprintf(
      '<div style="display:flex; flex-wrap:wrap; gap:16px; padding:12px 16px; background:#f7f9fc; border:1px solid #e1e6ee; border-radius:6px 6px 0 0; font-size:13px; color:#333;">
        <div><b style="color:#337ab7;">项目名称：</b>%s</div>
        <div><b style="color:#337ab7;">项目负责人：</b>%s</div>
        <div><b style="color:#337ab7;">开始日期：</b>%s</div>
        <div><b style="color:#337ab7;">版本：</b>%s</div>
        <div><b style="color:#337ab7;">更新：</b>%s</div>
      </div>',
      esc(proj_name), esc(plan$project_leader),
      esc(as.character(plan$start_date)), esc(plan$version),
      if (!is.null(plan$updated_at) && !is.na(plan$updated_at)) esc(as.character(plan$updated_at)) else "")
    obj_html <- if (!is.null(plan$project_objective) && !is.na(plan$project_objective) && nchar(trimws(plan$project_objective)) > 0) {
      sprintf('<div style="padding:8px 16px; background:#f7f9fc; border:1px solid #e1e6ee; border-top:none; font-size:13px; color:#555;"><b style="color:#337ab7;">项目目标：</b>%s</div>', esc(plan$project_objective))
    } else ""

    # ---- 表头行 ----
    left_w <- 260
    head_left <- sprintf('<div style="width:%dpx; min-width:%dpx; flex-shrink:0; padding:6px 10px; font-weight:bold; color:#333; border-right:1px solid #ddd;">Sub-Objectives / Major Tasks</div>', left_w, left_w)

    head_week <- paste(sapply(seq_len(week_count), function(w) {
      sprintf('<div style="width:30px; min-width:30px; flex-shrink:0; text-align:center; border-right:1px solid #f0f0f0; font-size:11px; color:#666;">W%d<div style="font-size:9px; color:#aaa;">%s</div></div>',
        w, format(week_dates[w], "%m/%d"))
    }), collapse = "")

    head_raci <- ""
    if (nrow(roles) > 0) {
      head_raci <- paste(sapply(1:nrow(roles), function(i) {
        sprintf('<div style="width:30px; min-width:30px; flex-shrink:0; text-align:center; border-left:1px solid #f0f0f0; font-size:10px; color:#666; writing-mode:vertical-rl; text-orientation:mixed; height:100%%; display:flex; align-items:center; justify-content:center; padding:2px 0;">%s</div>',
          esc(roles$role_name[i]))
      }), collapse = "")
    }
    head_raci_label <- sprintf('<div style="flex-shrink:0; padding:6px 8px; font-weight:bold; color:#333; border-left:2px solid #ddd; white-space:nowrap;">Owner &amp; Helpers</div>')

    head_row <- sprintf(
      '<div style="display:flex; align-items:stretch; background:#f2f5f9; border-bottom:2px solid #ddd; min-height:44px;">%s%s%s%s</div>',
      head_left, head_week, head_raci_label, head_raci)

    # ---- 数据行 ----
    rows_html <- ""
    if (nrow(tasks) > 0) {
      for (i in 1:nrow(tasks)) {
        tk <- tasks[i, ]
        is_phase <- !is.na(tk$is_phase) && tk$is_phase == 1
        indent <- if (is_phase) 0 else 24
        name_style <- if (is_phase) {
          "font-weight:bold; font-size:13px; color:#337ab7;"
        } else {
          "font-size:12px; color:#444;"
        }
        seq_txt <- if (!is.null(tk$seq_no) && !is.na(tk$seq_no) && tk$seq_no != "") tk$seq_no else ""
        name_txt <- paste0(if (nchar(seq_txt) > 0) paste0(seq_txt, " ") else "", tk$task_name)

        left_cell <- sprintf(
          '<div style="width:%dpx; min-width:%dpx; flex-shrink:0; padding:5px 8px; padding-left:%dpx; %s; border-right:1px solid #eee; display:flex; align-items:center; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;" title="%s">%s</div>',
          left_w, left_w, indent + 8, name_style, esc(name_txt), esc(name_txt))

        week_cells <- paste(sapply(seq_len(week_count), function(w) {
          pg <- get_prog(tk$id, w)
          render_prog_cell(tk$id, w, pg$mark, pg$color)
        }), collapse = "")

        raci_cells <- if (nrow(roles) > 0) {
          paste(sapply(1:nrow(roles), function(ri) {
            render_raci_cell(tk$id, roles$id[ri], get_raci(tk$id, roles$id[ri]))
          }), collapse = "")
        } else ""

        row_bg <- if (is_phase) "background:#fafbfd;" else "background:#fff;"
        rows_html <- paste0(rows_html, sprintf(
          '<div style="display:flex; align-items:stretch; border-bottom:1px solid #f0f0f0; min-height:30px; %s">%s%s%s</div>',
          row_bg, left_cell, week_cells, raci_cells))
      }
    } else {
      rows_html <- sprintf('<div style="padding:16px; color:#999;">尚无任务，请在下方管理面板添加阶段与任务。</div>')
    }

    # ---- 图例 ----
    legend_html <- sprintf(
      '<div style="margin-top:10px; font-size:12px; color:#666; line-height:22px;">
        <b>进度标记：</b>
        <span style="color:#5cb85c;">&#9679;</span>完成·顺利
        <span style="color:#f0ad4e; margin-left:10px;">&#9679;</span>完成·预警
        <span style="color:#d9534f; margin-left:10px;">&#9679;</span>完成·延期
        <span style="color:#5cb85c; margin-left:10px;">&#9675;</span>进行·顺利
        <span style="color:#f0ad4e; margin-left:10px;">&#9675;</span>进行·预警
        <span style="color:#d9534f; margin-left:10px;">&#9675;</span>进行·延期
        &nbsp;&nbsp;|&nbsp;&nbsp;<b>RACI：</b>
        <span style="color:#d9534f; font-weight:bold;">A</span>负责
        <span style="color:#337ab7; font-weight:bold; margin-left:8px;">R</span>执行
        <span style="color:#f0ad4e; font-weight:bold; margin-left:8px;">C</span>咨询
        <span style="color:#5bc0de; font-weight:bold; margin-left:8px;">I</span>通知
        <span style="color:#5cb85c; font-weight:bold; margin-left:8px;">S</span>支持
        &nbsp;&nbsp;<span style="color:#999;">（点击单元格切换状态）</span>
      </div>')

    # ---- 组装 ----
    chart_html <- sprintf(
      '<div style="border:1px solid #ddd; border-radius:6px; overflow:hidden;">%s%s
        <div style="overflow-x:auto;">%s%s</div>
      </div>%s',
      header_html, obj_html, head_row, rows_html, legend_html)

    HTML(chart_html)
  })

  # ================================================================
  # 管理面板（角色 + 任务）
  # ================================================================
  output$oppm_manage_ui <- renderUI({
    ov$refresh
    if (is.null(ov$plan_id)) return(NULL)

    roles <- oppm_role_get_by_plan(ov$plan_id)
    tasks <- oppm_task_get_by_plan(ov$plan_id)

    # 角色列表
    role_chips <- ""
    if (nrow(roles) > 0) {
      role_chips <- paste(sapply(1:nrow(roles), function(i) {
        sprintf('<span style="display:inline-flex; align-items:center; background:#eef3fb; border:1px solid #d5e2f5; color:#337ab7; border-radius:14px; padding:2px 6px 2px 12px; margin:2px 6px 2px 0; font-size:12px;">%s<a class="oppm-role-del" data-id="%d" href="#" style="margin-left:6px; color:#d9534f; text-decoration:none;" title="删除角色">&times;</a></span>',
          esc(roles$role_name[i]), roles$id[i])
      }), collapse = "")
    } else {
      role_chips <- '<span style="color:#999; font-size:12px;">尚未添加角色（RACI 责任矩阵列）</span>'
    }

    # 任务列表
    task_rows <- ""
    if (nrow(tasks) > 0) {
      task_rows <- paste(sapply(1:nrow(tasks), function(i) {
        tk <- tasks[i, ]
        is_phase <- !is.na(tk$is_phase) && tk$is_phase == 1
        seq_txt <- if (!is.null(tk$seq_no) && !is.na(tk$seq_no)) tk$seq_no else ""
        indent <- if (is_phase) 0 else 20
        tag <- if (is_phase) '<span style="background:#337ab7;color:#fff;border-radius:3px;padding:1px 5px;font-size:10px;">阶段</span>' else '<span style="background:#5cb85c;color:#fff;border-radius:3px;padding:1px 5px;font-size:10px;">任务</span>'
        name_style <- if (is_phase) "font-weight:bold; color:#337ab7;" else "color:#444;"
        sprintf('<div style="display:flex; align-items:center; padding:3px 0; padding-left:%dpx; border-bottom:1px dashed #f0f0f0; font-size:12px;">
          %s <span style="margin:0 8px; %s">%s %s</span>
          <a class="oppm-task-edit" data-id="%d" data-name="%s" data-seq="%s" href="#" style="margin-left:auto; color:#337ab7; text-decoration:none;" title="编辑">&nbsp;编辑</a>
          <a class="oppm-task-del" data-id="%d" href="#" style="margin-left:10px; color:#d9534f; text-decoration:none;" title="删除">&times; 删除</a>
        </div>',
          indent, tag, name_style, esc(seq_txt), esc(tk$task_name),
          tk$id, esc(tk$task_name), esc(seq_txt), tk$id)
      }), collapse = "")
    } else {
      task_rows <- '<div style="color:#999; font-size:12px; padding:4px 0;">尚未添加阶段/任务</div>'
    }

    # 阶段下拉（用于任务添加父级）
    phase_choices <- c("（顶层/无父级）" = "0")
    if (nrow(tasks) > 0) {
      ph <- tasks[tasks$is_phase == 1, , drop = FALSE]
      if (nrow(ph) > 0) {
        phase_choices <- c(phase_choices, setNames(as.character(ph$id), ph$task_name))
      }
    }

    tagList(
      tags$div(style = "margin-top:16px; padding:14px 16px; background:#fff; border:1px solid #e1e6ee; border-radius:6px;",
        tags$h5(icon("users"), " 角色管理（RACI 责任矩阵列）", style = "margin-top:0; color:#337ab7;"),
        tags$div(HTML(role_chips), style = "margin-bottom:8px;"),
        fluidRow(
          column(5, textInput("oppm_new_role", NULL, placeholder = "输入角色名，如：项目负责人")),
          column(2, div(style = "margin-top:4px;", actionButton("oppm_role_add_btn", "添加角色", class = "btn-primary btn-sm")))
        )
      ),
      tags$div(style = "margin-top:12px; padding:14px 16px; background:#fff; border:1px solid #e1e6ee; border-radius:6px;",
        tags$h5(icon("tasks"), " 阶段 / 任务管理", style = "margin-top:0; color:#337ab7;"),
        fluidRow(
          column(2, textInput("oppm_task_seq", NULL, placeholder = "编号，如 1 / 1.1")),
          column(4, textInput("oppm_task_name", NULL, placeholder = "名称，如 启动阶段 / 整体计划")),
          column(3, selectInput("oppm_task_parent", NULL, choices = phase_choices, label = NULL)),
          column(3, div(style = "margin-top:4px;",
            actionButton("oppm_task_add_phase", "加阶段", class = "btn-info btn-sm"),
            actionButton("oppm_task_add_task", "加任务", class = "btn-primary btn-sm")))
        ),
        tags$div(HTML(task_rows), style = "margin-top:8px; max-height:300px; overflow-y:auto;")
      )
    )
  })

  # ================================================================
  # 事件处理
  # ================================================================

  # 新建计划
  observeEvent(input$oppm_new_plan, {
    req(rv$logged_in)
    pchoices <- oppm_project_choices()
    if (length(pchoices) == 0) {
      showNotification("暂无项目，请先在项目列表中创建项目", type = "warning"); return()
    }
    showModal(modalDialog(
      title = "新建 OPPM 计划",
      size = "m",
      selectInput("oppm_modal_project", "关联项目", choices = pchoices),
      textInput("oppm_modal_leader", "项目负责人", placeholder = "如：恒迪-戴诗贡"),
      textAreaInput("oppm_modal_objective", "项目目标", rows = 3, placeholder = "如：本项目实现XXX系统实施、上线、交付"),
      dateInput("oppm_modal_start", "开始日期", value = Sys.Date()),
      numericInput("oppm_modal_weeks", "周数（进度表列数）", value = 20, min = 1, max = 104),
      textInput("oppm_modal_version", "版本", value = "v1.0"),
      footer = tagList(
        actionButton("oppm_modal_save", "创建", class = "btn-primary"),
        modalButton("取消")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$oppm_modal_save, {
    req(input$oppm_modal_project)
    result <- oppm_plan_add(
      input$oppm_modal_project,
      input$oppm_modal_leader,
      input$oppm_modal_objective,
      as.character(input$oppm_modal_start),
      input$oppm_modal_weeks,
      input$oppm_modal_version,
      rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) {
      removeModal()
      ov$plan_id <- result$id
      ov$refresh <- ov$refresh + 1
    }
  })

  # 编辑计划信息
  observeEvent(input$oppm_edit_plan, {
    req(ov$plan_id)
    plan <- oppm_plan_get_by_id(ov$plan_id)
    if (nrow(plan) == 0) { showNotification("计划不存在", type = "error"); return() }
    plan <- plan[1, ]
    showModal(modalDialog(
      title = "编辑 OPPM 计划信息",
      size = "m",
      textInput("oppm_edit_leader", "项目负责人", value = if (!is.na(plan$project_leader)) plan$project_leader else ""),
      textAreaInput("oppm_edit_objective", "项目目标", rows = 3, value = if (!is.na(plan$project_objective)) plan$project_objective else ""),
      dateInput("oppm_edit_start", "开始日期", value = if (!is.null(plan$start_date) && !is.na(plan$start_date) && plan$start_date != "") tryCatch(as.Date(plan$start_date), error = function(e) Sys.Date()) else Sys.Date()),
      numericInput("oppm_edit_weeks", "周数", value = if (!is.na(plan$week_count)) plan$week_count else 20, min = 1, max = 104),
      textInput("oppm_edit_version", "版本", value = if (!is.na(plan$version)) plan$version else "v1.0"),
      footer = tagList(
        actionButton("oppm_edit_save", "保存", class = "btn-primary"),
        actionButton("oppm_edit_delete", "删除计划", class = "btn-danger"),
        modalButton("取消")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$oppm_edit_save, {
    req(ov$plan_id)
    result <- oppm_plan_update(ov$plan_id, input$oppm_edit_leader, input$oppm_edit_objective,
      as.character(input$oppm_edit_start), input$oppm_edit_weeks, input$oppm_edit_version, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { removeModal(); ov$refresh <- ov$refresh + 1 }
  })

  observeEvent(input$oppm_edit_delete, {
    req(ov$plan_id)
    result <- oppm_plan_delete(ov$plan_id, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    removeModal()
    ov$plan_id <- NULL
    ov$refresh <- ov$refresh + 1
  })

  # 进度单元格点击
  observeEvent(input$oppm_prog_click, {
    req(rv$logged_in, ov$plan_id)
    info <- input$oppm_prog_click
    task_id <- as.integer(info$task_id)
    week <- as.integer(info$week)
    next_state <- prog_cycle(info$mark, info$color)
    result <- oppm_progress_set(task_id, week, next_state$mark, next_state$color)
    if (!result$success) showNotification(result$message, type = "error")
    ov$refresh <- ov$refresh + 1
  })

  # RACI 单元格点击
  observeEvent(input$oppm_raci_click, {
    req(rv$logged_in, ov$plan_id)
    info <- input$oppm_raci_click
    task_id <- as.integer(info$task_id)
    role_id <- as.integer(info$role_id)
    next_code <- raci_cycle(info$code)
    result <- oppm_raci_set(task_id, role_id, next_code)
    if (!result$success) showNotification(result$message, type = "error")
    ov$refresh <- ov$refresh + 1
  })

  # 角色管理
  observeEvent(input$oppm_role_add_btn, {
    req(ov$plan_id)
    if (is.null(input$oppm_new_role) || trimws(input$oppm_new_role) == "") {
      showNotification("请输入角色名", type = "error"); return()
    }
    result <- oppm_role_add(ov$plan_id, trimws(input$oppm_new_role))
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { updateTextInput(session, "oppm_new_role", value = ""); ov$refresh <- ov$refresh + 1 }
  })

  observeEvent(input$oppm_role_del_click, {
    req(ov$plan_id)
    result <- oppm_role_delete(as.integer(input$oppm_role_del_click))
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) ov$refresh <- ov$refresh + 1
  })

  # 任务管理
  observeEvent(input$oppm_task_add_phase, {
    req(ov$plan_id)
    if (is.null(input$oppm_task_name) || trimws(input$oppm_task_name) == "") {
      showNotification("请输入阶段名称", type = "error"); return()
    }
    result <- oppm_task_add(ov$plan_id, 0, trimws(input$oppm_task_seq), trimws(input$oppm_task_name), 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) {
      updateTextInput(session, "oppm_task_name", value = ""); updateTextInput(session, "oppm_task_seq", value = "")
      ov$refresh <- ov$refresh + 1
    }
  })

  observeEvent(input$oppm_task_add_task, {
    req(ov$plan_id)
    if (is.null(input$oppm_task_name) || trimws(input$oppm_task_name) == "") {
      showNotification("请输入任务名称", type = "error"); return()
    }
    parent_id <- if (!is.null(input$oppm_task_parent)) as.integer(input$oppm_task_parent) else 0
    result <- oppm_task_add(ov$plan_id, parent_id, trimws(input$oppm_task_seq), trimws(input$oppm_task_name), 0)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) {
      updateTextInput(session, "oppm_task_name", value = ""); updateTextInput(session, "oppm_task_seq", value = "")
      ov$refresh <- ov$refresh + 1
    }
  })

  observeEvent(input$oppm_task_del_click, {
    req(ov$plan_id)
    result <- oppm_task_delete(as.integer(input$oppm_task_del_click))
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) ov$refresh <- ov$refresh + 1
  })

  observeEvent(input$oppm_task_edit_click, {
    req(ov$plan_id)
    info <- input$oppm_task_edit_click
    ov$edit_task_id <- as.integer(info$id)
    showModal(modalDialog(
      title = "编辑任务",
      size = "s",
      textInput("oppm_edit_seq", "编号", value = info$seq),
      textInput("oppm_edit_name", "名称", value = info$name),
      footer = tagList(
        actionButton("oppm_edit_task_save", "保存", class = "btn-primary"),
        modalButton("取消")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$oppm_edit_task_save, {
    req(ov$edit_task_id)
    result <- oppm_task_update(ov$edit_task_id, input$oppm_edit_seq, input$oppm_edit_name)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { removeModal(); ov$edit_task_id <- NULL; ov$refresh <- ov$refresh + 1 }
  })
}
