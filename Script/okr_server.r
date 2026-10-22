# OKR 模块服务端
# 周期 → 目标(O) → 关键结果(KR) → 进度检查；打通工单/项目任务/巡检/月度绩效

okr_server <- function(input, output, session, rv) {
  okr_refresh <- reactiveVal(0)
  okr_selected_obj <- reactiveVal(NULL)
  # 弹窗编辑对象暂存（event 输入在观察者运行后会被重置，需存 id）
  okr_edit_obj_id <- reactiveVal(NULL)
  okr_edit_kr_id <- reactiveVal(NULL)
  okr_checkin_kr_id <- reactiveVal(NULL)

  # 延迟注册动态输出（okr_objectives_table 等由 renderUI 创建）
  observe({
    req(rv$logged_in)
    session$onFlushed(function() {
      tryCatch({
        outputOptions(output, "okr_objectives_table", suspendWhenHidden = FALSE)
        outputOptions(output, "okr_kr_table", suspendWhenHidden = FALSE)
        outputOptions(output, "okr_updates_table", suspendWhenHidden = FALSE)
      }, error = function(e) NULL)
    }, once = TRUE)
  })

  is_admin <- reactive({
    !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
  })
  current_uid <- reactive({
    if (is.null(rv$current_user) || nrow(rv$current_user) == 0) NULL else rv$current_user$id[1]
  })

  # 当前用户对目标的编辑权限
  can_edit_obj <- function(obj) {
    if (is.null(obj)) return(FALSE)
    if (is_admin()) return(TRUE)
    !is.null(current_uid()) && !is.na(obj$owner_id) && obj$owner_id == current_uid()
  }

  # 所有活跃用户（用于负责人选择）
  okr_all_users <- function() {
    con <- db_connect()
    on.exit(db_disconnect(con))
    dbGetQuery(con, "SELECT id, display_name, username FROM users WHERE active = 1 ORDER BY display_name")
  }

  # 进度条 HTML
  okr_progress_bar <- function(p, status_code) {
    color <- if (status_code == "achieved") "progress-bar-success"
             else if (status_code == "on_track") "progress-bar-info"
             else if (status_code == "at_risk") "progress-bar-warning"
             else "progress-bar-danger"
    pv <- format(round(as.numeric(p), 1), trim = TRUE, scientific = FALSE)
    sprintf('<div class="progress" style="margin:0;height:16px;min-width:110px;"><div class="progress-bar %s" role="progressbar" style="width:%s%%;">%s%%</div></div>',
      color, pv, pv)
  }

  ##################
  # Header：标题 + 周期选择 + 统计卡片（不依赖 input$okr_cycle）
  ##################
  output$okr_header <- renderUI({
    req(rv$logged_in); okr_refresh()
    cycles <- okr_cycle_list()
    active <- okr_cycle_active()
    has_active <- !is.null(active) && nrow(active) > 0
    choices <- if (nrow(cycles) > 0) stats::setNames(as.character(cycles$id), cycles$name) else c("（无周期）" = "")
    sel <- if (has_active) as.character(active$id[1]) else (if (nrow(cycles) > 0) as.character(cycles$id[1]) else "")

    # 统计（基于默认周期，避免依赖 input$okr_cycle）
    obj_count <- 0; kr_count <- 0; avg_prog <- 0
    stat_cycle <- if (has_active) active else NULL
    if (!is.null(stat_cycle)) {
      objs <- tryCatch(okr_objective_list(stat_cycle$id[1]), error = function(e) data.frame())
      krs <- tryCatch(okr_kr_list_by_cycle(stat_cycle$id[1]), error = function(e) data.frame())
      obj_count <- nrow(objs)
      kr_count <- nrow(krs)
      if (kr_count > 0) avg_prog <- round(mean(krs$progress, na.rm = TRUE), 1)
    }

    tagList(
      div(style = "text-align:center;margin:8px 0 5px;",
        h3(icon("bullseye"), " OKR 目标与关键结果"),
        p(style = "color:#7f8c8d;font-size:12px;",
          "Objectives & Key Results ｜ 周期 → 目标(O) → 关键结果(KR) → 进度检查 ｜ 自动打通工单/项目任务/巡检/月度绩效")),
      fluidRow(
        column(3, div(class = "well well-sm", style = "text-align:center;padding:10px;",
          div(style = "font-size:13px;color:#666;", "目标数(O)"),
          div(style = "font-size:24px;font-weight:bold;color:#333;", obj_count))),
        column(3, div(class = "well well-sm", style = "text-align:center;padding:10px;",
          div(style = "font-size:13px;color:#666;", "关键结果数(KR)"),
          div(style = "font-size:24px;font-weight:bold;color:#337ab7;", kr_count))),
        column(3, div(class = "well well-sm", style = "text-align:center;padding:10px;",
          div(style = "font-size:13px;color:#666;", "平均进度"),
          div(style = "font-size:24px;font-weight:bold;color:#e67e22;", sprintf("%s%%", avg_prog)))),
        column(3, div(class = "well well-sm", style = "text-align:center;padding:10px;",
          div(style = "font-size:13px;color:#666;", "周期数"),
          div(style = "font-size:24px;font-weight:bold;color:#27ae60;", nrow(cycles))))
      ),
      hr(),
      fluidRow(
        column(3, selectInput("okr_cycle", "选择周期", choices = choices, selected = sel, width = "100%")),
        column(2, div(style = "margin-top:25px;",
          actionButton("okr_new_cycle", "新建周期", class = "btn-primary btn-sm", icon = icon("calendar-plus")))),
        column(2, div(style = "margin-top:25px;",
          actionButton("okr_new_obj", "新建目标", class = "btn-success btn-sm", icon = icon("bullseye")))),
        column(2, div(style = "margin-top:25px;",
          actionButton("okr_refresh_btn", "刷新", class = "btn-info btn-sm"))),
        column(3, div(style = "margin-top:25px;",
          actionButton("okr_sync_all", "同步全部数据", class = "btn-warning btn-sm", icon = icon("sync"),
            title = "按各KR关联的数据源自动同步当前值与进度")))
      )
    )
  })

  ##################
  # Main：目标列表 + 关键结果详情
  ##################
  output$okr_main <- renderUI({
    req(rv$logged_in, input$okr_cycle, nchar(input$okr_cycle) > 0); okr_refresh()
    tagList(
      tags$hr(),
      h4(icon("list"), " 目标 Objectives", style = "margin-bottom:10px;"),
      DT::DTOutput("okr_objectives_table"),
      tags$hr(),
      uiOutput("okr_kr_section")
    )
  })
  outputOptions(output, "okr_main", suspendWhenHidden = FALSE)

  ##################
  # 目标列表
  ##################
  output$okr_objectives_table <- DT::renderDT({
    req(rv$logged_in); okr_refresh(); req(input$okr_cycle, nchar(input$okr_cycle) > 0)
    cycle <- okr_cycle_get(as.integer(input$okr_cycle))
    if (is.null(cycle)) return(DT::datatable(data.frame(信息 = "请先新建 OKR 周期"), options = list(dom = 't')))
    objs <- okr_objective_list(cycle$id[1])
    if (!is_admin() && !is.null(current_uid()) && nrow(objs) > 0) {
      objs <- objs[objs$owner_id == current_uid(), , drop = FALSE]
    }
    if (nrow(objs) == 0) {
      hint <- if (is_admin()) "暂无目标，点击「新建目标」创建" else "暂无你的 OKR 目标"
      return(DT::datatable(data.frame(信息 = hint), options = list(dom = 't')))
    }
    ops <- sapply(seq_len(nrow(objs)), function(i) {
      b <- sprintf('<button class="btn btn-primary btn-xs okr-view-btn" data-id="%s">KR详情</button>', objs$id[i])
      if (is_admin()) {
        b <- paste0(b,
          sprintf(' <button class="btn btn-info btn-xs okr-obj-edit-btn" data-id="%s">编辑</button>', objs$id[i]),
          sprintf(' <button class="btn btn-danger btn-xs okr-obj-del-btn" data-id="%s">删除</button>', objs$id[i]))
      }
      b
    })
    display <- data.frame(
      目标 = objs$title,
      层级 = objs$level_label,
      负责人 = ifelse(is.na(objs$owner_name), "未指定", objs$owner_name),
      KR数 = objs$kr_count,
      进度 = sapply(seq_len(nrow(objs)), function(i) okr_progress_bar(objs$progress[i], okr_kr_status(objs$progress[i]))),
      状态 = objs$status_label,
      操作 = ops,
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(display, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 50, dom = 't', scrollX = TRUE,
        columnDefs = list(list(targets = "_all", className = "dt-center"))),
      class = 'cell-border stripe hover')
  })

  ##################
  # 关键结果详情区
  ##################
  output$okr_kr_section <- renderUI({
    req(rv$logged_in); okr_refresh()
    obj_id <- okr_selected_obj()
    if (is.null(obj_id)) {
      return(tags$div(style = "color:#999;text-align:center;padding:20px;",
        icon("info-circle"), " 点击上方目标的「KR详情」查看关键结果"))
    }
    obj <- okr_objective_get(obj_id)
    if (is.null(obj)) return(tags$div(style = "color:#999;", "目标不存在或已删除"))
    if (!is_admin() && !is.null(current_uid()) && !is.na(obj$owner_id) && obj$owner_id != current_uid()) {
      return(tags$div(style = "color:#999;", "无权查看该目标"))
    }
    cycle <- okr_cycle_get(obj$cycle_id)
    cycle_name <- if (!is.null(cycle) && nrow(cycle) > 0) cycle$name[1] else ""
    prog <- okr_objective_progress(obj_id)
    editable <- can_edit_obj(obj)

    tagList(
      h4(icon("bullseye"), " 目标：", obj$title),
      p(style = "color:#666;font-size:12px;margin-bottom:4px;",
        sprintf("周期：%s ｜ 层级：%s ｜ 负责人：%s ｜ 进度：%s",
          cycle_name, okr_level_label(obj$level), okr_user_name(obj$owner_id),
          sprintf("%.1f%%", prog))),
      if (!is.na(obj$description) && obj$description != "")
        p(style = "color:#555;font-size:12px;", "说明：", obj$description) else "",
      fluidRow(
        column(7, h5(icon("key"), " 关键结果 Key Results")),
        column(5, div(style = "text-align:right;",
          if (editable) actionButton("okr_new_kr", "添加KR", class = "btn-success btn-sm", icon = icon("plus")),
          actionButton("okr_sync_obj", "同步数据", class = "btn-warning btn-sm", icon = icon("sync"),
            title = "按数据源同步该目标下所有KR"))
        )
      ),
      DT::DTOutput("okr_kr_table"),
      tags$hr(),
      h5(icon("history"), " 进度更新记录 Check-ins"),
      DT::DTOutput("okr_updates_table")
    )
  })
  outputOptions(output, "okr_kr_section", suspendWhenHidden = FALSE)

  ##################
  # KR 列表
  ##################
  output$okr_kr_table <- DT::renderDT({
    req(rv$logged_in); okr_refresh()
    obj_id <- okr_selected_obj(); req(obj_id)
    obj <- okr_objective_get(obj_id); req(obj)
    krs <- okr_kr_list(obj_id)
    if (nrow(krs) == 0) return(DT::datatable(data.frame(信息 = "暂无关键结果，点击「添加KR」创建"), options = list(dom = 't')))
    editable <- can_edit_obj(obj)
    ops <- sapply(seq_len(nrow(krs)), function(i) {
      if (!editable) return("")
      paste0(
        sprintf('<button class="btn btn-info btn-xs okr-kr-edit-btn" data-id="%s">编辑</button> ', krs$id[i]),
        sprintf('<button class="btn btn-warning btn-xs okr-kr-checkin-btn" data-id="%s">进度更新</button> ', krs$id[i]),
        sprintf('<button class="btn btn-success btn-xs okr-kr-sync-btn" data-id="%s">同步</button> ', krs$id[i]),
        sprintf('<button class="btn btn-danger btn-xs okr-kr-del-btn" data-id="%s">删除</button>', krs$id[i]))
    })
    src_label <- sapply(krs$source_type, function(s) {
      if (is.na(s) || s == "" || s == "manual" || s == "手工") return("手工")
      idx <- match(s, OKR_SOURCE_TYPES)
      if (is.na(idx)) s else names(OKR_SOURCE_TYPES)[idx]
    })
    display <- data.frame(
      关键结果 = krs$title,
      衡量 = paste0(ifelse(is.na(krs$metric), "", krs$metric),
        ifelse(is.na(krs$unit) | krs$unit == "", "", sprintf(" (%s)", krs$unit))),
      基线 = krs$baseline,
      目标 = krs$target,
      当前 = krs$current_value,
      进度 = sapply(krs$progress, function(p) okr_progress_bar(p, okr_kr_status(p))),
      状态 = sapply(krs$progress, okr_confidence_label),
      数据源 = src_label,
      操作 = ops,
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(display, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 50, dom = 't', scrollX = TRUE,
        columnDefs = list(
          list(targets = c(2,3,4,6,7,8), className = "dt-center"),
          list(targets = 8, orderable = FALSE))),
      class = 'cell-border stripe hover')
  })

  ##################
  # 进度更新记录
  ##################
  output$okr_updates_table <- DT::renderDT({
    req(rv$logged_in); okr_refresh()
    obj_id <- okr_selected_obj(); req(obj_id)
    ups <- okr_updates_by_objective(obj_id)
    if (nrow(ups) == 0) return(DT::datatable(data.frame(信息 = "暂无更新记录"), options = list(dom = 't')))
    updater <- ifelse(is.na(ups$display_name) | ups$display_name == "", ups$username, ups$display_name)
    display <- data.frame(
      时间 = ups$created_at,
      关键结果 = ups$kr_title,
      旧值 = ups$old_value,
      新值 = ups$new_value,
      进度 = sprintf("%.1f%%", ups$progress),
      备注 = ups$note %||% "",
      更新人 = updater,
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(display, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 10, dom = 't', scrollX = TRUE,
        columnDefs = list(list(targets = c(2,3,4), className = "dt-center"))),
      class = 'cell-border stripe hover')
  })

  ##################
  # 事件处理
  ##################
  observeEvent(input$okr_refresh_btn, { req(rv$logged_in); okr_refresh(okr_refresh() + 1) })

  # 新建周期
  observeEvent(input$okr_new_cycle, {
    req(rv$logged_in, is_admin())
    today <- Sys.Date()
    y <- as.integer(format(today, "%Y"))
    q <- (as.integer(format(today, "%m")) - 1) %/% 3 + 1
    showModal(modalDialog(
      title = "新建 OKR 周期", size = "m",
      selectInput("okr_cy_year", "年份", choices = seq(y - 2, y + 2), selected = y),
      selectInput("okr_cy_quarter", "周期类型",
        choices = c("年度" = "0", "Q1" = "1", "Q2" = "2", "Q3" = "3", "Q4" = "4"), selected = as.character(q)),
      textInput("okr_cy_name", "名称（留空自动生成）", placeholder = "如 2026 Q4 / 2026年度"),
      dateInput("okr_cy_start", "开始日期", value = today),
      dateInput("okr_cy_end", "结束日期", value = today + 90),
      footer = tagList(modalButton("取消"), actionButton("okr_cy_confirm", "创建", class = "btn-primary")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$okr_cy_confirm, {
    req(rv$logged_in, is_admin())
    nm <- trimws(input$okr_cy_name)
    result <- okr_cycle_create(
      year = as.integer(input$okr_cy_year),
      quarter = as.integer(input$okr_cy_quarter),
      name = if (nchar(nm) > 0) nm else NULL,
      start_date = as.character(input$okr_cy_start),
      end_date = as.character(input$okr_cy_end))
    removeModal()
    okr_refresh(okr_refresh() + 1)
    if (result$success) {
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 新建目标
  observeEvent(input$okr_new_obj, {
    req(rv$logged_in, input$okr_cycle, nchar(input$okr_cycle) > 0)
    users <- okr_all_users()
    level_choices <- stats::setNames(OKR_LEVELS, names(OKR_LEVELS))
    # 上级目标（对齐）
    objs <- okr_objective_list(as.integer(input$okr_cycle))
    parent_choices <- c("（无，作为顶层目标）" = "")
    if (nrow(objs) > 0) parent_choices <- c(parent_choices, stats::setNames(as.character(objs$id), objs$title))
    owner_ui <- if (is_admin()) {
      selectInput("okr_obj_owner", "负责人", choices = stats::setNames(as.character(users$id),
        ifelse(is.na(users$display_name) | users$display_name == "", users$username, users$display_name)))
    } else {
      tags$div(class = "form-group",
        tags$label("负责人"),
        tags$div(class = "form-control", style = "background:#f5f5f5;",
          if (is.null(current_uid())) "未登录" else okr_user_name(current_uid())))
    }
    showModal(modalDialog(
      title = "新建目标 Objective", size = "m",
      textInput("okr_obj_title", "目标标题 *", placeholder = "想达成什么（定性、有方向）"),
      textAreaInput("okr_obj_desc", "目标说明", rows = 2, placeholder = "可选"),
      selectInput("okr_obj_level", "层级", choices = level_choices, selected = "team"),
      owner_ui,
      selectInput("okr_obj_parent", "对齐上级目标", choices = parent_choices),
      numericInput("okr_obj_weight", "权重", value = 1, min = 0.1, step = 0.1),
      footer = tagList(modalButton("取消"), actionButton("okr_obj_confirm", "创建", class = "btn-success")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$okr_obj_confirm, {
    req(rv$logged_in, input$okr_cycle, nchar(input$okr_cycle) > 0, input$okr_obj_title)
    owner_id <- if (is_admin()) {
      if (nchar(input$okr_obj_owner) > 0) as.integer(input$okr_obj_owner) else NULL
    } else current_uid()
    parent_id <- if (nchar(input$okr_obj_parent) > 0) as.integer(input$okr_obj_parent) else NULL
    result <- okr_objective_add(
      cycle_id = as.integer(input$okr_cycle),
      title = trimws(input$okr_obj_title),
      description = trimws(input$okr_obj_desc),
      level = input$okr_obj_level,
      owner_id = owner_id,
      parent_id = parent_id,
      weight = as.numeric(input$okr_obj_weight %||% 1))
    removeModal()
    okr_refresh(okr_refresh() + 1)
    showNotification(if (result$success) result$message else result$message, type = if (result$success) "message" else "error")
  })

  # 查看 KR（选择目标）
  observeEvent(input$okr_view_click, {
    req(rv$logged_in)
    okr_selected_obj(as.integer(input$okr_view_click))
  })

  # 编辑目标
  observeEvent(input$okr_obj_edit_click, {
    req(rv$logged_in, is_admin())
    okr_edit_obj_id(as.integer(input$okr_obj_edit_click))
    obj <- okr_objective_get(okr_edit_obj_id())
    req(obj)
    users <- okr_all_users()
    level_choices <- stats::setNames(OKR_LEVELS, names(OKR_LEVELS))
    status_choices <- stats::setNames(OKR_OBJ_STATUS, names(OKR_OBJ_STATUS))
    showModal(modalDialog(
      title = sprintf("编辑目标 #%s", obj$id), size = "m",
      textInput("okr_edit_title", "目标标题", value = obj$title),
      textAreaInput("okr_edit_desc", "目标说明", value = obj$description %||% "", rows = 2),
      selectInput("okr_edit_level", "层级", choices = level_choices, selected = obj$level),
      selectInput("okr_edit_owner", "负责人", choices = stats::setNames(as.character(users$id),
        ifelse(is.na(users$display_name) | users$display_name == "", users$username, users$display_name)),
        selected = if (!is.na(obj$owner_id)) as.character(obj$owner_id) else ""),
      selectInput("okr_edit_status", "状态", choices = status_choices, selected = obj$status),
      numericInput("okr_edit_weight", "权重", value = obj$weight, min = 0.1, step = 0.1),
      footer = tagList(modalButton("取消"), actionButton("okr_edit_confirm", "保存", class = "btn-primary")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$okr_edit_confirm, {
    req(rv$logged_in, is_admin(), okr_edit_obj_id())
    result <- okr_objective_update(
      objective_id = okr_edit_obj_id(),
      title = trimws(input$okr_edit_title),
      description = trimws(input$okr_edit_desc),
      level = input$okr_edit_level,
      owner_id = if (nchar(input$okr_edit_owner) > 0) as.integer(input$okr_edit_owner) else NULL,
      status = input$okr_edit_status,
      weight = as.numeric(input$okr_edit_weight %||% 1))
    removeModal()
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 删除目标
  observeEvent(input$okr_obj_del_click, {
    req(rv$logged_in, is_admin())
    result <- okr_objective_delete(as.integer(input$okr_obj_del_click))
    if (result$success) okr_selected_obj(NULL)
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 添加 KR
  observeEvent(input$okr_new_kr, {
    req(rv$logged_in)
    obj_id <- okr_selected_obj(); req(obj_id)
    obj <- okr_objective_get(obj_id); req(obj)
    if (!can_edit_obj(obj)) return()
    src_choices <- stats::setNames(OKR_SOURCE_TYPES, names(OKR_SOURCE_TYPES))
    sheets <- perf_sheet_list()
    sheet_choices <- if (nrow(sheets) > 0) stats::setNames(as.character(sheets$id), sheets$year_month) else c("（无绩效月表）" = "")
    showModal(modalDialog(
      title = "添加关键结果 KR", size = "m",
      textInput("okr_kr_title", "关键结果 *", placeholder = "如何衡量达成（定量、可度量）"),
      textInput("okr_kr_metric", "衡量指标", placeholder = "如：完成数量 / 满意度"),
      textInput("okr_kr_unit", "单位", placeholder = "如：个 / 项 / 分"),
      fluidRow(
        column(4, numericInput("okr_kr_baseline", "起点值", value = 0)),
        column(4, numericInput("okr_kr_target", "目标值", value = 100)),
        column(4, numericInput("okr_kr_current", "当前值", value = 0))
      ),
      numericInput("okr_kr_weight", "权重", value = 1, min = 0.1, step = 0.1),
      selectInput("okr_kr_source", "数据源（打通已有数据）", choices = src_choices, selected = "manual"),
      conditionalPanel(
        condition = "input.okr_kr_source == '绩效'",
        selectInput("okr_kr_sheet", "绩效月表", choices = sheet_choices),
        tags$small(style = "color:#888;", "当前值将取该月表下负责人的「绩效得分（10分制）」，用于自动计算进度。")
      ),
      conditionalPanel(
        condition = "input.okr_kr_source != '绩效' && input.okr_kr_source != 'manual'",
        tags$small(style = "color:#888;", "当前值将按「负责人 + 周期日期」自动统计已完成数量，用于计算进度。")
      ),
      footer = tagList(modalButton("取消"), actionButton("okr_kr_confirm", "添加", class = "btn-success")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$okr_kr_confirm, {
    req(rv$logged_in, input$okr_kr_title)
    obj_id <- okr_selected_obj(); req(obj_id)
    src <- input$okr_kr_source %||% "manual"
    source_id <- NULL
    if (src == "绩效" && nchar(input$okr_kr_sheet) > 0) source_id <- as.integer(input$okr_kr_sheet)
    result <- okr_kr_add(
      objective_id = obj_id,
      title = trimws(input$okr_kr_title),
      metric = trimws(input$okr_kr_metric),
      unit = trimws(input$okr_kr_unit),
      baseline = as.numeric(input$okr_kr_baseline %||% 0),
      target = as.numeric(input$okr_kr_target %||% 100),
      current_value = as.numeric(input$okr_kr_current %||% 0),
      weight = as.numeric(input$okr_kr_weight %||% 1),
      source_type = src,
      source_id = source_id)
    removeModal()
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 编辑 KR
  observeEvent(input$okr_kr_edit_click, {
    req(rv$logged_in)
    okr_edit_kr_id(as.integer(input$okr_kr_edit_click))
    kr <- okr_kr_get(okr_edit_kr_id()); req(kr)
    obj <- okr_objective_get(kr$objective_id); req(obj)
    if (!can_edit_obj(obj)) return()
    src_choices <- stats::setNames(OKR_SOURCE_TYPES, names(OKR_SOURCE_TYPES))
    showModal(modalDialog(
      title = sprintf("编辑关键结果 #%s", kr$id), size = "m",
      textInput("okr_ek_title", "关键结果", value = kr$title),
      textInput("okr_ek_metric", "衡量指标", value = kr$metric %||% ""),
      textInput("okr_ek_unit", "单位", value = kr$unit %||% ""),
      fluidRow(
        column(4, numericInput("okr_ek_baseline", "起点值", value = kr$baseline)),
        column(4, numericInput("okr_ek_target", "目标值", value = kr$target)),
        column(4, numericInput("okr_ek_current", "当前值", value = kr$current_value))
      ),
      numericInput("okr_ek_weight", "权重", value = kr$weight, min = 0.1, step = 0.1),
      selectInput("okr_ek_source", "数据源", choices = src_choices, selected = kr$source_type %||% "manual"),
      footer = tagList(modalButton("取消"), actionButton("okr_ek_confirm", "保存", class = "btn-primary")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$okr_ek_confirm, {
    req(rv$logged_in, okr_edit_kr_id())
    result <- okr_kr_update(
      kr_id = okr_edit_kr_id(),
      title = trimws(input$okr_ek_title),
      metric = trimws(input$okr_ek_metric),
      unit = trimws(input$okr_ek_unit),
      baseline = as.numeric(input$okr_ek_baseline),
      target = as.numeric(input$okr_ek_target),
      current_value = as.numeric(input$okr_ek_current),
      weight = as.numeric(input$okr_ek_weight),
      source_type = input$okr_ek_source)
    removeModal()
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 进度更新（Check-in）
  observeEvent(input$okr_kr_checkin_click, {
    req(rv$logged_in)
    okr_checkin_kr_id(as.integer(input$okr_kr_checkin_click))
    kr <- okr_kr_get(okr_checkin_kr_id()); req(kr)
    obj <- okr_objective_get(kr$objective_id); req(obj)
    if (!can_edit_obj(obj)) return()
    showModal(modalDialog(
      title = sprintf("进度更新：%s", substr(kr$title, 1, 30)), size = "m",
      p(style = "color:#666;font-size:12px;",
        sprintf("当前：%s ｜ 目标：%s（起点 %s）", format(kr$current_value), format(kr$target), format(kr$baseline))),
      numericInput("okr_ck_value", "新的当前值", value = kr$current_value),
      textAreaInput("okr_ck_note", "备注", rows = 2, placeholder = "进展说明、风险等"),
      footer = tagList(modalButton("取消"), actionButton("okr_ck_confirm", "更新进度", class = "btn-warning")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$okr_ck_confirm, {
    req(rv$logged_in, okr_checkin_kr_id())
    result <- okr_kr_checkin(
      kr_id = okr_checkin_kr_id(),
      new_value = as.numeric(input$okr_ck_value),
      note = trimws(input$okr_ck_note),
      updated_by = current_uid())
    removeModal()
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 同步单个 KR
  observeEvent(input$okr_kr_sync_click, {
    req(rv$logged_in)
    result <- okr_kr_sync_progress(as.integer(input$okr_kr_sync_click))
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 同步目标下全部 KR
  observeEvent(input$okr_sync_obj, {
    req(rv$logged_in)
    obj_id <- okr_selected_obj(); req(obj_id)
    krs <- okr_kr_list(obj_id)
    if (nrow(krs) == 0) { showNotification("该目标暂无关键结果", type = "warning"); return() }
    synced <- 0; failed <- 0
    for (i in seq_len(nrow(krs))) {
      r <- okr_kr_sync_progress(krs$id[i])
      if (r$success) synced <- synced + 1 else failed <- failed + 1
    }
    okr_refresh(okr_refresh() + 1)
    showNotification(sprintf("已同步 %d 个关键结果%s", synced, if (failed > 0) sprintf("（%d 个失败）", failed) else ""),
      type = if (synced > 0) "message" else "warning")
  })

  # 同步全部 KR（当前周期）
  observeEvent(input$okr_sync_all, {
    req(rv$logged_in, input$okr_cycle, nchar(input$okr_cycle) > 0)
    krs <- okr_kr_list_by_cycle(as.integer(input$okr_cycle))
    if (nrow(krs) == 0) { showNotification("当前周期暂无关键结果", type = "warning"); return() }
    synced <- 0; failed <- 0
    for (i in seq_len(nrow(krs))) {
      r <- okr_kr_sync_progress(krs$id[i])
      if (r$success) synced <- synced + 1 else failed <- failed + 1
    }
    okr_refresh(okr_refresh() + 1)
    showNotification(sprintf("已同步 %d 个关键结果%s", synced, if (failed > 0) sprintf("（%d 个失败）", failed) else ""),
      type = if (synced > 0) "message" else "warning")
  })

  # 删除 KR
  observeEvent(input$okr_kr_del_click, {
    req(rv$logged_in)
    result <- okr_kr_delete(as.integer(input$okr_kr_del_click))
    okr_refresh(okr_refresh() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })
}
