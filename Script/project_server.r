# 项目管理模块 - 服务端逻辑
# 子标签1：项目列表（钻入导航）
# 子标签2：项目详情（全部阶段一览）
# 子标签3：任务管理（全部任务一览）
# 所有优先级/状态选项从 config_options 表读取

project_server <- function(input, output, session, rv) {

  # ================================================================
  # 公共工具
  # ================================================================
  rv$proj_nav_level <- "projects"
  rv$proj_nav_project_id <- NULL
  rv$proj_nav_project_name <- ""
  rv$proj_nav_phase_id <- NULL
  rv$proj_nav_phase_name <- ""
  rv$proj_nav_wp_id <- NULL
  rv$proj_nav_wp_name <- ""
  rv$proj_data_refresh <- 0

  esc <- function(x) {
    x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x); x <- gsub('"', "&quot;", x)
    x <- gsub("'", "&#39;", x); x
  }

  render_log_content <- function(txt) {
    if (is.null(txt) || is.na(txt) || nchar(trimws(txt)) == 0) return("")
    if (grepl("<[a-zA-Z][^>]*>", txt)) return(txt)
    if (requireNamespace("commonmark", quietly = TRUE)) {
      result <- tryCatch(commonmark::markdown_html(txt), error = function(e) NULL)
      if (!is.null(result)) return(result)
    }
    gsub("\n", "<br/>", gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", txt))))
  }

  # 状态/优先级标签&颜色（从配置读取）
  lbl <- function(cat, val) config_option_label(cat, val)
  clr <- function(cat, val) config_option_color(cat, val)

  assignable_users <- reactive({
    req(rv$logged_in)
    users <- project_get_assignable_users()
    if (nrow(users) > 0) {
      labels <- ifelse(is.na(users$display_name) | users$display_name == "",
                       users$username, sprintf("%s (%s)", users$display_name, users$username))
      c("\u672a\u6307\u5b9a" = "", setNames(as.character(users$id), labels))
    } else { c("\u672a\u6307\u5b9a" = "") }
  })

  # 项目列表（用于筛选下拉）
  project_choices <- reactive({
    rv$proj_data_refresh
    projs <- project_get_all("all")
    if (nrow(projs) > 0) {
      c("\u5168\u90e8\u9879\u76ee" = "all", setNames(as.character(projs$id), projs$name))
    } else { c("\u5168\u90e8\u9879\u76ee" = "all") }
  })

  # ================================================================
  # 统计卡片（动态渲染，使用配置颜色）
  # ================================================================
  output$proj_stat_cards <- renderUI({
    req(rv$logged_in); req(input$proj_tabs)
    rv$proj_data_refresh
    stats <- project_get_stats()
    opts <- config_option_get("project_status")
    cards <- list(
      column(2, div(class = "well well-sm", style = "text-align:center; padding:12px 8px;",
        div(style = "font-size:14px; color:#666; font-weight:500;", "\u603b\u9879\u76ee"),
        div(style = "font-size:26px; font-weight:bold; color:#333;", stats$total[1])))
    )
    status_map <- list(planning="planning", active="active", completed="completed",
                       suspended="suspended", closed="closed")
    for (sv in names(status_map)) {
      opt_row <- opts[opts$option_value == sv, ]
      bg <- if (nrow(opt_row) > 0 && opt_row$color[1] != "") opt_row$color[1] else "#999"
      lab <- if (nrow(opt_row) > 0) opt_row$option_label[1] else sv
      val <- stats[[sv]][1]
      cards <- c(cards, list(
        column(2, div(class = "well well-sm",
          style = sprintf("text-align:center; padding:12px 8px; background:%s; color:white;", bg),
          div(style = "font-size:14px; font-weight:500;", lab),
          div(style = "font-size:26px; font-weight:bold;", val)))
      ))
    }
    fluidRow(column(12, div(style = "margin-bottom:15px;", do.call(fluidRow, cards))))
  })

  # ================================================================
  # 子标签1：项目列表 - 面包屑导航
  # ================================================================
  output$proj_breadcrumb <- renderUI({
    level <- rv$proj_nav_level
    link_style <- "color:#337ab7; cursor:pointer; text-decoration:underline;"
    current_style <- "font-weight:bold; color:#333; font-size:15px;"
    sep <- span(" / ", style = "color:#999; margin:0 4px;")
    crumbs <- switch(level,
      "projects" = span("\u5168\u90e8\u9879\u76ee", style = current_style),
      "phases" = tagList(actionLink("proj_nav_home", "\u5168\u90e8\u9879\u76ee", style = link_style), sep,
        span(rv$proj_nav_project_name, style = current_style)),
      "work_packages" = tagList(actionLink("proj_nav_home", "\u5168\u90e8\u9879\u76ee", style = link_style), sep,
        actionLink("proj_nav_to_phases", rv$proj_nav_project_name, style = link_style), sep,
        span(rv$proj_nav_phase_name, style = current_style)),
      "tasks" = tagList(actionLink("proj_nav_home", "\u5168\u90e8\u9879\u76ee", style = link_style), sep,
        actionLink("proj_nav_to_phases", rv$proj_nav_project_name, style = link_style), sep,
        actionLink("proj_nav_to_wps", rv$proj_nav_phase_name, style = link_style), sep,
        span(rv$proj_nav_wp_name, style = current_style)))

    if (level == "projects") {
      fluidRow(
        column(7, div(style = "padding:10px 15px; background:#f5f5f5; border-radius:4px;", crumbs)),
        column(3, selectInput("proj_status_filter", NULL,
          choices = config_option_choices("project_status", include_all = TRUE), selected = "all")),
        column(2, div(style = "margin-top:2px;", actionButton("proj_refresh", "\u5237\u65b0", class = "btn-info btn-sm"))))
    } else if (level == "phases") {
      fluidRow(
        column(7, div(style = "padding:10px 15px; background:#f5f5f5; border-radius:4px;", crumbs)),
        column(3, selectInput("phase_status_filter", NULL,
          choices = config_option_choices("phase_status", include_all = TRUE), selected = "all")),
        column(2, div(style = "margin-top:2px;", actionButton("proj_refresh", "\u5237\u65b0", class = "btn-info btn-sm"))))
    } else if (level == "work_packages") {
      fluidRow(
        column(7, div(style = "padding:10px 15px; background:#f5f5f5; border-radius:4px;", crumbs)),
        column(3, selectInput("wp_status_filter", NULL,
          choices = config_option_choices("wp_status", include_all = TRUE), selected = "all")),
        column(2, div(style = "margin-top:2px;", actionButton("proj_refresh", "\u5237\u65b0", class = "btn-info btn-sm"))))
    } else {
      fluidRow(
        column(5, div(style = "padding:10px 15px; background:#f5f5f5; border-radius:4px;", crumbs)),
        column(3, selectInput("task_status_filter", NULL,
          choices = config_option_choices("task_status", include_all = TRUE), selected = "all")),
        column(2, selectInput("task_priority_filter", NULL,
          choices = config_option_choices("task_priority", include_all = TRUE), selected = "all")),
        column(2, div(style = "margin-top:2px;", actionButton("proj_refresh", "\u5237\u65b0", class = "btn-info btn-sm"))))
    }
  })

  # 面包屑点击
  observeEvent(input$proj_nav_home, { rv$proj_nav_level <- "projects" })
  observeEvent(input$proj_nav_to_phases, { rv$proj_nav_level <- "phases" })
  observeEvent(input$proj_nav_to_wps, { rv$proj_nav_level <- "work_packages" })
  observeEvent(input$proj_refresh, { rv$proj_data_refresh <- rv$proj_data_refresh + 1 })

  # 钻入
  observeEvent(input$proj_enter_click, {
    rv$proj_nav_project_id <- input$proj_enter_click$id
    rv$proj_nav_project_name <- input$proj_enter_click$name
    rv$proj_nav_level <- "phases"
  })
  observeEvent(input$phase_enter_click, {
    rv$proj_nav_phase_id <- input$phase_enter_click$id
    rv$proj_nav_phase_name <- input$phase_enter_click$name
    rv$proj_nav_level <- "work_packages"
  })
  observeEvent(input$wp_enter_click, {
    rv$proj_nav_wp_id <- input$wp_enter_click$id
    rv$proj_nav_wp_name <- input$wp_enter_click$name
    rv$proj_nav_level <- "tasks"
  })

  # ================================================================
  # 子标签1：项目列表 - 数据表格
  # ================================================================
  output$proj_data_table <- renderDT({
    cat("[proj_data_table] renderDT 被触发, logged_in =", rv$logged_in, "\n")
    req(rv$logged_in)
    # 等待 tabsetPanel UI 渲染完成，确保 DTOutput 容器已在 DOM 中
    req(input$proj_tabs)
    rv$proj_data_refresh
    tryCatch({
    level <- rv$proj_nav_level
    cat("[proj_data_table] nav level =", level, "\n")
    if (level == "projects") {
      fv <- if (!is.null(input$proj_status_filter)) input$proj_status_filter else "all"
      data <- project_get_all(fv)
      if (nrow(data) > 0) {
        display <- data.frame(
          `操作` = sprintf('<button class="btn btn-sm btn-info proj-view-btn" data-id="%s">\u8be6\u60c5</button> <button class="btn btn-sm btn-success proj-enter-btn" data-id="%s" data-name="%s">\u8fdb\u5165</button>', data$id, data$id, esc(data$name)),
          `项目编号` = data$project_no,
          `项目名称` = sprintf('<a href="#" class="proj-enter-btn" data-id="%s" data-name="%s" style="color:#337ab7;font-weight:bold;cursor:pointer;">%s</a>', data$id, esc(data$name), data$name),
          `优先级` = sapply(data$priority, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("project_priority", v), v)),
          `状态` = sapply(data$status, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("project_status", v), lbl("project_status", v))),
          `创建人` = ifelse(is.na(data$creator_name), "\u672a\u77e5", data$creator_name),
          `开始` = ifelse(is.na(data$start_date), "-", data$start_date),
          `结束` = ifelse(is.na(data$end_date), "-", data$end_date),
          stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        display <- data.frame(`操作`=character(), `项目编号`=character(), `项目名称`=character(), `优先级`=character(), `状态`=character(), `创建人`=character(), `开始`=character(), `结束`=character(), stringsAsFactors=FALSE, check.names=FALSE)
      }
    } else if (level == "phases") {
      data <- phase_get_by_project(rv$proj_nav_project_id)
      fv <- if (!is.null(input$phase_status_filter)) input$phase_status_filter else "all"
      if (fv != "all" && nrow(data) > 0) data <- data[data$status == fv, , drop=FALSE]
      if (nrow(data) > 0) {
        display <- data.frame(
          `操作` = sprintf('<button class="btn btn-sm btn-success phase-enter-btn" data-id="%s" data-name="%s">\u8fdb\u5165</button> <button class="btn btn-sm btn-danger phase-del-btn" data-id="%s">\u5220\u9664</button>', data$id, esc(data$name), data$id),
          `阶段名称` = sprintf('<a href="#" class="phase-enter-btn" data-id="%s" data-name="%s" style="color:#337ab7;font-weight:bold;cursor:pointer;">%s</a>', data$id, esc(data$name), data$name),
          `描述` = ifelse(is.na(data$description), "", data$description),
          `状态` = sapply(data$status, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("phase_status", v), lbl("phase_status", v))),
          `排序` = data$sort_order, stringsAsFactors=FALSE, check.names=FALSE)
      } else {
        display <- data.frame(`操作`=character(), `阶段名称`=character(), `描述`=character(), `状态`=character(), `排序`=integer(), stringsAsFactors=FALSE, check.names=FALSE)
      }
    } else if (level == "work_packages") {
      data <- wp_get_by_phase(rv$proj_nav_phase_id)
      fv <- if (!is.null(input$wp_status_filter)) input$wp_status_filter else "all"
      if (fv != "all" && nrow(data) > 0) data <- data[data$status == fv, , drop=FALSE]
      if (nrow(data) > 0) {
        display <- data.frame(
          `操作` = sprintf('<button class="btn btn-sm btn-success wp-enter-btn" data-id="%s" data-name="%s">\u8fdb\u5165</button> <button class="btn btn-sm btn-danger wp-del-btn" data-id="%s">\u5220\u9664</button>', data$id, esc(data$name), data$id),
          `工作包名称` = sprintf('<a href="#" class="wp-enter-btn" data-id="%s" data-name="%s" style="color:#337ab7;font-weight:bold;cursor:pointer;">%s</a>', data$id, esc(data$name), data$name),
          `描述` = ifelse(is.na(data$description), "", data$description),
          `负责人` = ifelse(is.na(data$assignee_name), "\u672a\u6307\u5b9a", data$assignee_name),
          `状态` = sapply(data$status, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("wp_status", v), lbl("wp_status", v))),
          `排序` = data$sort_order, stringsAsFactors=FALSE, check.names=FALSE)
      } else {
        display <- data.frame(`操作`=character(), `工作包名称`=character(), `描述`=character(), `负责人`=character(), `状态`=character(), `排序`=integer(), stringsAsFactors=FALSE, check.names=FALSE)
      }
    } else {
      data <- task_get_by_wp(rv$proj_nav_wp_id)
      sf <- if (!is.null(input$task_status_filter)) input$task_status_filter else "all"
      if (sf != "all" && nrow(data) > 0) data <- data[data$status == sf, , drop=FALSE]
      pf <- if (!is.null(input$task_priority_filter)) input$task_priority_filter else "all"
      if (pf != "all" && nrow(data) > 0) data <- data[data$priority == pf, , drop=FALSE]
      if (nrow(data) > 0) {
        display <- data.frame(
          `操作` = sprintf('<button class="btn btn-sm btn-info task-view-btn" data-id="%s">\u67e5\u770b</button> <button class="btn btn-sm btn-warning task-to-wo-btn" data-id="%s">\u8f6c\u5de5\u5355</button>', data$id, data$id),
          `任务编号` = ifelse(is.na(data$task_no), "-", data$task_no),
          `任务名称` = sprintf('<a href="#" class="task-view-btn" data-id="%s" style="color:#337ab7;font-weight:bold;cursor:pointer;">%s</a>', data$id, data$name),
          `优先级` = sapply(data$priority, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("task_priority", v), v)),
          `状态` = sapply(data$status, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("task_status", v), lbl("task_status", v))),
          `负责人` = ifelse(is.na(data$assignee_name), "\u672a\u6307\u5b9a", data$assignee_name),
          `截止日期` = ifelse(is.na(data$due_date), "-", data$due_date),
          `关联工单` = ifelse(is.na(data$work_order_id), "-", as.character(data$work_order_id)),
          stringsAsFactors=FALSE, check.names=FALSE)
      } else {
        display <- data.frame(`操作`=character(), `任务编号`=character(), `任务名称`=character(), `优先级`=character(), `状态`=character(), `负责人`=character(), `截止日期`=character(), `关联工单`=character(), stringsAsFactors=FALSE, check.names=FALSE)
      }
    }
    DT::datatable(display, escape = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, dom = 'lfrtip',
        lengthMenu = list(c(10,20,50,-1), c('10','20','50','\u5168\u90e8')),
        columnDefs = list(list(targets = 0, width = '150px', className = 'dt-center', orderable = FALSE))),
      rownames = FALSE, selection = 'none', class = 'cell-border stripe hover')
    }, error = function(e) {
      cat("[proj_data_table] renderDT error:", e$message, "\n")
      DT::datatable(data.frame(`错误` = e$message, check.names = FALSE))
    })
  })

  # ================================================================
  # 子标签1：创建表单（动态，配置项驱动）
  # ================================================================
  output$proj_create_form <- renderUI({
    level <- rv$proj_nav_level
    if (level == "projects") {
      wellPanel(h4("\u521b\u5efa\u65b0\u9879\u76ee"), fluidRow(
        column(3, textInput("proj_new_name", "\u9879\u76ee\u540d\u79f0")),
        column(2, selectInput("proj_new_priority", "\u4f18\u5148\u7ea7",
          choices = config_option_choices("project_priority"),
          selected = config_option_default("project_priority"))),
        column(2, dateInput("proj_new_start", "\u5f00\u59cb\u65e5\u671f", value = Sys.Date(), language = "zh-CN")),
        column(2, dateInput("proj_new_end", "\u7ed3\u675f\u65e5\u671f", value = Sys.Date() + 30, language = "zh-CN")),
        column(1, div(style = "margin-top:20px;", actionButton("proj_add", "\u521b\u5efa", class = "btn-primary btn-sm")))),
        fluidRow(column(12, textAreaInput("proj_new_desc", "\u9879\u76ee\u63cf\u8ff0", rows = 2))))
    } else if (level == "phases") {
      wellPanel(h4("\u6dfb\u52a0\u65b0\u9636\u6bb5"), fluidRow(
        column(4, textInput("phase_new_name", "\u9636\u6bb5\u540d\u79f0")),
        column(4, textInput("phase_new_desc", "\u9636\u6bb5\u63cf\u8ff0")),
        column(2, numericInput("phase_new_order", "\u6392\u5e8f", value = 1, min = 1)),
        column(2, div(style = "margin-top:20px;", actionButton("phase_add", "\u6dfb\u52a0\u9636\u6bb5", class = "btn-primary btn-sm")))))
    } else if (level == "work_packages") {
      wellPanel(h4("\u6dfb\u52a0\u65b0\u5de5\u4f5c\u5305"), fluidRow(
        column(3, textInput("wp_new_name", "\u5de5\u4f5c\u5305\u540d\u79f0")),
        column(3, textInput("wp_new_desc", "\u5de5\u4f5c\u5305\u63cf\u8ff0")),
        column(2, selectInput("wp_new_assignee", "\u8d1f\u8d23\u4eba", choices = assignable_users())),
        column(2, numericInput("wp_new_order", "\u6392\u5e8f", value = 1, min = 1)),
        column(2, div(style = "margin-top:20px;", actionButton("wp_add", "\u6dfb\u52a0", class = "btn-primary btn-sm")))))
    } else {
      wellPanel(h4("\u521b\u5efa\u65b0\u4efb\u52a1"), fluidRow(
        column(3, textInput("task_new_name", "\u4efb\u52a1\u540d\u79f0")),
        column(2, selectInput("task_new_priority", "\u4f18\u5148\u7ea7",
          choices = config_option_choices("task_priority"),
          selected = config_option_default("task_priority"))),
        column(2, selectInput("task_new_assignee", "\u8d1f\u8d23\u4eba", choices = assignable_users())),
        column(2, dateInput("task_new_due", "\u622a\u6b62\u65e5\u671f", value = Sys.Date() + 7, language = "zh-CN")),
        column(1, div(style = "margin-top:20px;", actionButton("task_add", "\u521b\u5efa\u4efb\u52a1", class = "btn-primary btn-sm")))),
        fluidRow(column(12, textAreaInput("task_new_desc", "\u4efb\u52a1\u63cf\u8ff0", rows = 2))))
    }
  })

  # ================================================================
  # 子标签2：项目详情 — 全部阶段一览
  # ================================================================
  output$pd_project_filter_ui <- renderUI({
    selectInput("pd_project_filter", "\u9879\u76ee\u7b5b\u9009", choices = project_choices())
  })
  output$pd_status_filter_ui <- renderUI({
    selectInput("pd_phase_status_filter", "\u72b6\u6001\u7b5b\u9009",
      choices = config_option_choices("phase_status", include_all = TRUE), selected = "all")
  })
  observeEvent(input$pd_refresh, { rv$proj_data_refresh <- rv$proj_data_refresh + 1 })

  output$pd_phase_table <- renderDT({
    req(rv$logged_in); req(input$proj_tabs); rv$proj_data_refresh
    sf <- if (!is.null(input$pd_phase_status_filter)) input$pd_phase_status_filter else "all"
    data <- phase_get_all(sf)
    pf <- if (!is.null(input$pd_project_filter)) input$pd_project_filter else "all"
    if (pf != "all" && nrow(data) > 0) data <- data[data$project_id == as.integer(pf), , drop=FALSE]
    if (nrow(data) > 0) {
      display <- data.frame(
        `操作` = sprintf('<button class="btn btn-sm btn-success phase-enter-btn" data-id="%s" data-name="%s">\u8fdb\u5165</button>', data$id, esc(data$name)),
        `所属项目` = ifelse(is.na(data$project_name), "-", data$project_name),
        `阶段名称` = sprintf('<a href="#" class="phase-enter-btn" data-id="%s" data-name="%s" style="color:#337ab7;font-weight:bold;cursor:pointer;">%s</a>', data$id, esc(data$name), data$name),
        `描述` = ifelse(is.na(data$description), "", data$description),
        `状态` = sapply(data$status, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("phase_status", v), lbl("phase_status", v))),
        `排序` = data$sort_order,
        stringsAsFactors=FALSE, check.names=FALSE)
    } else {
      display <- data.frame(`操作`=character(), `所属项目`=character(), `阶段名称`=character(), `描述`=character(), `状态`=character(), `排序`=integer(), stringsAsFactors=FALSE, check.names=FALSE)
    }
    DT::datatable(display, escape = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, dom = 'lfrtip',
        lengthMenu = list(c(10,20,50,-1), c('10','20','50','\u5168\u90e8')),
        columnDefs = list(list(targets = 0, width = '80px', className = 'dt-center', orderable = FALSE))),
      rownames = FALSE, selection = 'none', class = 'cell-border stripe hover')
  })

  # 项目详情 - 创建阶段（需先选项目）
  output$pd_create_form <- renderUI({
    wellPanel(h4("\u6dfb\u52a0\u65b0\u9636\u6bb5"), fluidRow(
      column(3, selectInput("pd_new_phase_project", "\u6240\u5c5e\u9879\u76ee",
        choices = {
          projs <- project_get_all("all")
          if (nrow(projs) > 0) setNames(as.character(projs$id), projs$name) else c()
        })),
      column(3, textInput("pd_new_phase_name", "\u9636\u6bb5\u540d\u79f0")),
      column(3, textInput("pd_new_phase_desc", "\u63cf\u8ff0")),
      column(1, numericInput("pd_new_phase_order", "\u6392\u5e8f", value = 1, min = 1)),
      column(2, div(style = "margin-top:20px;", actionButton("pd_phase_add", "\u6dfb\u52a0", class = "btn-primary btn-sm")))))
  })
  observeEvent(input$pd_phase_add, {
    req(rv$logged_in)
    if (is.null(input$pd_new_phase_name) || trimws(input$pd_new_phase_name) == "") {
      showNotification("\u8bf7\u8f93\u5165\u9636\u6bb5\u540d\u79f0", type = "error"); return()
    }
    result <- phase_add(input$pd_new_phase_project, input$pd_new_phase_name,
      input$pd_new_phase_desc, input$pd_new_phase_order)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) {
      updateTextInput(session, "pd_new_phase_name", value = "")
      updateTextInput(session, "pd_new_phase_desc", value = "")
      rv$proj_data_refresh <- rv$proj_data_refresh + 1
    }
  })

  # ================================================================
  # 子标签3：任务管理 — 全部任务一览
  # ================================================================
  output$tm_project_filter_ui <- renderUI({
    selectInput("tm_project_filter", "\u9879\u76ee", choices = project_choices())
  })
  output$tm_status_filter_ui <- renderUI({
    selectInput("tm_task_status_filter", "\u72b6\u6001",
      choices = config_option_choices("task_status", include_all = TRUE), selected = "all")
  })
  output$tm_priority_filter_ui <- renderUI({
    selectInput("tm_task_priority_filter", "\u4f18\u5148\u7ea7",
      choices = config_option_choices("task_priority", include_all = TRUE), selected = "all")
  })
  output$tm_assignee_filter_ui <- renderUI({
    selectInput("tm_assignee_filter", "\u8d1f\u8d23\u4eba",
      choices = c("\u5168\u90e8" = "all", assignable_users()))
  })
  observeEvent(input$tm_refresh, { rv$proj_data_refresh <- rv$proj_data_refresh + 1 })

  output$tm_task_table <- renderDT({
    req(rv$logged_in); req(input$proj_tabs); rv$proj_data_refresh
    sf <- if (!is.null(input$tm_task_status_filter)) input$tm_task_status_filter else "all"
    pf <- if (!is.null(input$tm_task_priority_filter)) input$tm_task_priority_filter else "all"
    data <- task_get_all_global(sf, pf)
    # 项目筛选
    proj_f <- if (!is.null(input$tm_project_filter)) input$tm_project_filter else "all"
    if (proj_f != "all" && nrow(data) > 0) data <- data[data$project_id == as.integer(proj_f), , drop=FALSE]
    # 负责人筛选
    assign_f <- if (!is.null(input$tm_assignee_filter)) input$tm_assignee_filter else "all"
    if (assign_f != "all" && assign_f != "" && nrow(data) > 0) {
      data <- data[!is.na(data$assigned_to) & data$assigned_to == as.integer(assign_f), , drop=FALSE]
    }
    if (nrow(data) > 0) {
      display <- data.frame(
        `操作` = sprintf('<button class="btn btn-sm btn-info task-view-btn" data-id="%s">\u67e5\u770b</button> <button class="btn btn-sm btn-warning task-to-wo-btn" data-id="%s">\u8f6c\u5de5\u5355</button>', data$id, data$id),
        `任务编号` = ifelse(is.na(data$task_no), "-", data$task_no),
        `任务名称` = sprintf('<a href="#" class="task-view-btn" data-id="%s" style="color:#337ab7;font-weight:bold;cursor:pointer;">%s</a>', data$id, data$name),
        `项目` = ifelse(is.na(data$project_name), "-", data$project_name),
        `阶段` = ifelse(is.na(data$phase_name), "-", data$phase_name),
        `工作包` = ifelse(is.na(data$wp_name), "-", data$wp_name),
        `优先级` = sapply(data$priority, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("task_priority", v), v)),
        `状态` = sapply(data$status, function(v) sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:3px;">%s</span>', clr("task_status", v), lbl("task_status", v))),
        `负责人` = ifelse(is.na(data$assignee_name), "\u672a\u6307\u5b9a", data$assignee_name),
        `截止日期` = ifelse(is.na(data$due_date), "-", data$due_date),
        stringsAsFactors=FALSE, check.names=FALSE)
    } else {
      display <- data.frame(`操作`=character(), `任务编号`=character(), `任务名称`=character(), `项目`=character(), `阶段`=character(), `工作包`=character(), `优先级`=character(), `状态`=character(), `负责人`=character(), `截止日期`=character(), stringsAsFactors=FALSE, check.names=FALSE)
    }
    DT::datatable(display, escape = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, dom = 'lfrtip',
        lengthMenu = list(c(10,20,50,-1), c('10','20','50','\u5168\u90e8')),
        columnDefs = list(list(targets = 0, width = '150px', className = 'dt-center', orderable = FALSE))),
      rownames = FALSE, selection = 'none', class = 'cell-border stripe hover')
  })

  # ================================================================
  # CRUD 处理（子标签1）
  # ================================================================
  observeEvent(input$proj_add, {
    req(rv$logged_in)
    if (is.null(input$proj_new_name) || trimws(input$proj_new_name) == "") {
      showNotification("\u8bf7\u8f93\u5165\u9879\u76ee\u540d\u79f0", type = "error"); return()
    }
    result <- project_add(input$proj_new_name, input$proj_new_desc,
      input$proj_new_priority, as.character(input$proj_new_start),
      as.character(input$proj_new_end), rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { updateTextInput(session, "proj_new_name", value = ""); updateTextAreaInput(session, "proj_new_desc", value = ""); rv$proj_data_refresh <- rv$proj_data_refresh + 1 }
  })
  observeEvent(input$phase_add, {
    req(rv$logged_in)
    if (is.null(input$phase_new_name) || trimws(input$phase_new_name) == "") {
      showNotification("\u8bf7\u8f93\u5165\u9636\u6bb5\u540d\u79f0", type = "error"); return()
    }
    result <- phase_add(rv$proj_nav_project_id, input$phase_new_name, input$phase_new_desc, input$phase_new_order)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { updateTextInput(session, "phase_new_name", value = ""); updateTextInput(session, "phase_new_desc", value = ""); rv$proj_data_refresh <- rv$proj_data_refresh + 1 }
  })
  observeEvent(input$phase_del_click, {
    req(rv$logged_in)
    showModal(modalDialog(title = "\u786e\u8ba4\u5220\u9664", "\u786e\u5b9a\u5220\u9664\u8be5\u9636\u6bb5\uff1f\u5176\u4e0b\u5de5\u4f5c\u5305\u548c\u4efb\u52a1\u5c06\u4e00\u5e76\u5220\u9664\u3002",
      footer = tagList(actionButton("phase_del_confirm", "\u786e\u8ba4\u5220\u9664", class = "btn-danger"), modalButton("\u53d6\u6d88")), easyClose = TRUE))
    rv$phase_del_target <- input$phase_del_click
  })
  observeEvent(input$phase_del_confirm, {
    result <- phase_delete(rv$phase_del_target)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1
  })
  observeEvent(input$wp_add, {
    req(rv$logged_in)
    if (is.null(input$wp_new_name) || trimws(input$wp_new_name) == "") {
      showNotification("\u8bf7\u8f93\u5165\u5de5\u4f5c\u5305\u540d\u79f0", type = "error"); return()
    }
    result <- wp_add(rv$proj_nav_phase_id, rv$proj_nav_project_id, input$wp_new_name, input$wp_new_desc, input$wp_new_assignee, input$wp_new_order)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { updateTextInput(session, "wp_new_name", value = ""); updateTextInput(session, "wp_new_desc", value = ""); rv$proj_data_refresh <- rv$proj_data_refresh + 1 }
  })
  observeEvent(input$wp_del_click, {
    req(rv$logged_in)
    showModal(modalDialog(title = "\u786e\u8ba4\u5220\u9664", "\u786e\u5b9a\u5220\u9664\u8be5\u5de5\u4f5c\u5305\uff1f\u5176\u4e0b\u4efb\u52a1\u5c06\u4e00\u5e76\u5220\u9664\u3002",
      footer = tagList(actionButton("wp_del_confirm", "\u786e\u8ba4\u5220\u9664", class = "btn-danger"), modalButton("\u53d6\u6d88")), easyClose = TRUE))
    rv$wp_del_target <- input$wp_del_click
  })
  observeEvent(input$wp_del_confirm, {
    result <- wp_delete(rv$wp_del_target)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1
  })
  observeEvent(input$task_add, {
    req(rv$logged_in)
    if (is.null(input$task_new_name) || trimws(input$task_new_name) == "") {
      showNotification("\u8bf7\u8f93\u5165\u4efb\u52a1\u540d\u79f0", type = "error"); return()
    }
    result <- task_add(rv$proj_nav_wp_id, rv$proj_nav_phase_id, rv$proj_nav_project_id,
      input$task_new_name, input$task_new_desc, input$task_new_priority,
      input$task_new_assignee, as.character(input$task_new_due), rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { updateTextInput(session, "task_new_name", value = ""); updateTextAreaInput(session, "task_new_desc", value = ""); rv$proj_data_refresh <- rv$proj_data_refresh + 1 }
  })

  # ================================================================
  # 项目详情弹窗
  # ================================================================
  observeEvent(input$proj_view_click, {
    req(rv$logged_in)
    proj <- project_get_by_id(input$proj_view_click)
    if (nrow(proj) == 0) return()
    p <- proj[1, ]; rv$proj_modal_id <- p$id
    pc <- clr("project_priority", p$priority); sc <- clr("project_status", p$status)
    sl <- lbl("project_status", p$status)
    tasks <- task_get_by_project(p$id)
    tt <- nrow(tasks); td <- if (tt > 0) sum(tasks$status == "completed") else 0
    tp <- if (tt > 0) round(td / tt * 100) else 0
    phases <- phase_get_by_project(p$id)
    hh <- ""
    if (nrow(phases) > 0) {
      for (pi in 1:nrow(phases)) {
        ph <- phases[pi, ]; wps <- wp_get_by_phase(ph$id); wc <- nrow(wps)
        pt <- if (tt > 0) tasks[tasks$phase_id == ph$id, , drop=FALSE] else data.frame()
        ptot <- nrow(pt); pdn <- if (ptot > 0) sum(pt$status == "completed") else 0
        hh <- paste0(hh, sprintf('<div style="margin-bottom:12px;"><div style="background:#e8f4fd;padding:8px 12px;border-radius:4px;font-weight:bold;color:#337ab7;border-left:4px solid #337ab7;">\u9636\u6bb5 %d: %s <span style="font-weight:normal;color:#666;margin-left:12px;">[ %s ]</span><span style="float:right;font-size:12px;color:#666;">%d \u5de5\u4f5c\u5305 | \u4efb\u52a1 %d/%d</span></div>', pi, ph$name, lbl("phase_status", ph$status), wc, pdn, ptot))
        if (wc > 0) {
          for (wi in 1:wc) {
            wp <- wps[wi, ]; wt <- task_get_by_wp(wp$id); wtot <- nrow(wt)
            wdn <- if (wtot > 0) sum(wt$status == "completed") else 0
            asgn <- ifelse(is.na(wp$assignee_name), "\u672a\u6307\u5b9a", wp$assignee_name)
            hh <- paste0(hh, sprintf('<div style="margin-left:20px;margin-top:6px;"><div style="background:#f0f0f0;padding:6px 10px;border-radius:3px;font-size:13px;border-left:3px solid #5cb85c;"><b>%s</b> <span style="color:#888;margin-left:8px;">\u8d1f\u8d23\u4eba: %s</span><span style="float:right;font-size:12px;color:#666;">\u4efb\u52a1 %d/%d</span></div>', wp$name, asgn, wdn, wtot))
            if (wtot > 0) {
              hh <- paste0(hh, '<table style="width:100%;margin-left:10px;margin-top:4px;font-size:12px;border-collapse:collapse;"><tr style="background:#fafafa;font-weight:bold;color:#666;"><td style="padding:4px 8px;border-bottom:1px solid #eee;width:25%;">\u4efb\u52a1\u7f16\u53f7</td><td style="padding:4px 8px;border-bottom:1px solid #eee;width:30%;">\u540d\u79f0</td><td style="padding:4px 8px;border-bottom:1px solid #eee;width:15%;">\u72b6\u6001</td><td style="padding:4px 8px;border-bottom:1px solid #eee;width:10%;">\u4f18\u5148\u7ea7</td><td style="padding:4px 8px;border-bottom:1px solid #eee;width:20%;">\u8d1f\u8d23\u4eba</td></tr>')
              for (ti in 1:wtot) { tk <- wt[ti, ]; hh <- paste0(hh, sprintf('<tr><td style="padding:3px 8px;border-bottom:1px solid #f5f5f5;">%s</td><td style="padding:3px 8px;border-bottom:1px solid #f5f5f5;">%s</td><td style="padding:3px 8px;border-bottom:1px solid #f5f5f5;"><span style="background:%s;color:white;padding:1px 6px;border-radius:3px;font-size:11px;">%s</span></td><td style="padding:3px 8px;border-bottom:1px solid #f5f5f5;">%s</td><td style="padding:3px 8px;border-bottom:1px solid #f5f5f5;">%s</td></tr>', ifelse(is.na(tk$task_no), "-", tk$task_no), tk$name, clr("task_status", tk$status), lbl("task_status", tk$status), tk$priority, ifelse(is.na(tk$assignee_name), "\u672a\u6307\u5b9a", tk$assignee_name))) }
              hh <- paste0(hh, '</table>')
            } else { hh <- paste0(hh, '<div style="margin-left:10px;padding:4px 8px;color:#999;font-size:12px;">\u6682\u65e0\u4efb\u52a1</div>') }
            hh <- paste0(hh, '</div>')
          }
        } else { hh <- paste0(hh, '<div style="margin-left:20px;padding:6px 10px;color:#999;font-size:13px;">\u6682\u65e0\u5de5\u4f5c\u5305</div>') }
        hh <- paste0(hh, '</div>')
      }
    } else { hh <- '<div style="color:#999;padding:10px;">\u6682\u65e0\u9636\u6bb5</div>' }
    mc <- HTML(sprintf('<div style="padding:10px;"><div style="background:#f5f5f5;padding:14px;border-radius:6px;margin-bottom:15px;"><table style="width:100%%;font-size:14px;"><tr><td style="width:100px;font-weight:bold;color:#666;">\u9879\u76ee\u7f16\u53f7\uff1a</td><td style="color:#337ab7;font-weight:bold;">%s</td><td style="width:100px;font-weight:bold;color:#666;">\u540d\u79f0\uff1a</td><td style="font-weight:bold;">%s</td></tr><tr><td colspan="4" style="height:8px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u4f18\u5148\u7ea7\uff1a</td><td><span style="background:%s;color:white;padding:3px 10px;border-radius:4px;">%s</span></td><td style="font-weight:bold;color:#666;">\u72b6\u6001\uff1a</td><td><span style="background:%s;color:white;padding:3px 10px;border-radius:4px;">%s</span></td></tr><tr><td colspan="4" style="height:8px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u521b\u5efa\u4eba\uff1a</td><td>%s</td><td style="font-weight:bold;color:#666;">\u521b\u5efa\u65f6\u95f4\uff1a</td><td>%s</td></tr><tr><td colspan="4" style="height:8px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u5f00\u59cb\uff1a</td><td>%s</td><td style="font-weight:bold;color:#666;">\u7ed3\u675f\uff1a</td><td>%s</td></tr><tr><td colspan="4" style="height:8px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u4efb\u52a1\u8fdb\u5ea6\uff1a</td><td colspan="3"><div style="background:#eee;border-radius:10px;height:20px;"><div style="background:#5cb85c;border-radius:10px;height:20px;width:%d%%;text-align:center;color:white;font-size:12px;line-height:20px;">%d/%d (%d%%)</div></div></td></tr></table></div><div><div style="font-weight:bold;margin-bottom:8px;">\u9879\u76ee\u63cf\u8ff0</div><div style="background:#fafafa;padding:14px;border-radius:6px;border-left:4px solid #337ab7;min-height:40px;white-space:pre-wrap;">%s</div></div><div style="margin-top:15px;"><div style="font-weight:bold;margin-bottom:8px;">\u9879\u76ee\u7ed3\u6784\u660e\u7ec6</div><div style="max-height:400px;overflow-y:auto;border:1px solid #eee;border-radius:6px;padding:10px;">%s</div></div></div>',
      p$project_no, p$name, pc, p$priority, sc, sl,
      ifelse(is.na(p$creator_name), "\u672a\u77e5", p$creator_name), p$created_at,
      ifelse(is.na(p$start_date), "-", p$start_date), ifelse(is.na(p$end_date), "-", p$end_date),
      tp, td, tt, tp, ifelse(is.na(p$description), "\u65e0\u63cf\u8ff0", p$description), hh))
    showModal(modalDialog(title = paste0("\u9879\u76ee\u8be6\u60c5 - ", p$project_no), mc,
      footer = tagList(actionButton("proj_modal_edit", "\u7f16\u8f91\u9879\u76ee", class = "btn-warning"),
        actionButton("proj_modal_delete", "\u5220\u9664\u9879\u76ee", class = "btn-danger"), modalButton("\u5173\u95ed")),
      size = "l", easyClose = TRUE))
  })

  # 编辑项目
  observeEvent(input$proj_modal_edit, {
    req(rv$proj_modal_id); proj <- project_get_by_id(rv$proj_modal_id)
    if (nrow(proj) == 0) return(); p <- proj[1, ]; removeModal()
    showModal(modalDialog(title = "\u7f16\u8f91\u9879\u76ee",
      fluidRow(
        column(6, textInput("proj_edit_name", "\u9879\u76ee\u540d\u79f0", value = p$name)),
        column(3, selectInput("proj_edit_priority", "\u4f18\u5148\u7ea7",
          choices = config_option_choices("project_priority"), selected = p$priority)),
        column(3, selectInput("proj_edit_status", "\u72b6\u6001",
          choices = config_option_choices("project_status"), selected = p$status))),
      fluidRow(
        column(6, dateInput("proj_edit_start", "\u5f00\u59cb\u65e5\u671f", value = ifelse(is.na(p$start_date), Sys.Date(), p$start_date), language = "zh-CN")),
        column(6, dateInput("proj_edit_end", "\u7ed3\u675f\u65e5\u671f", value = ifelse(is.na(p$end_date), Sys.Date()+30, p$end_date), language = "zh-CN"))),
      textAreaInput("proj_edit_desc", "\u63cf\u8ff0", value = ifelse(is.na(p$description), "", p$description), rows = 3),
      footer = tagList(actionButton("proj_edit_save", "\u4fdd\u5b58", class = "btn-primary"), modalButton("\u53d6\u6d88")), size = "m", easyClose = TRUE))
  })
  observeEvent(input$proj_edit_save, {
    req(rv$proj_modal_id, input$proj_edit_name)
    result <- project_update(rv$proj_modal_id, input$proj_edit_name, input$proj_edit_desc,
      input$proj_edit_priority, input$proj_edit_status,
      as.character(input$proj_edit_start), as.character(input$proj_edit_end), rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1 }
  })
  observeEvent(input$proj_modal_delete, {
    req(rv$proj_modal_id); removeModal()
    showModal(modalDialog(title = "\u786e\u8ba4\u5220\u9664", "\u786e\u5b9a\u5220\u9664\u8be5\u9879\u76ee\uff1f\u6240\u6709\u9636\u6bb5\u3001\u5de5\u4f5c\u5305\u3001\u4efb\u52a1\u5c06\u4e00\u5e76\u5220\u9664\u3002",
      footer = tagList(actionButton("proj_confirm_delete", "\u786e\u8ba4\u5220\u9664", class = "btn-danger"), modalButton("\u53d6\u6d88")), easyClose = TRUE))
  })
  observeEvent(input$proj_confirm_delete, {
    result <- project_delete(rv$proj_modal_id, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1
  })

  # ================================================================
  # 任务详情弹窗
  # ================================================================
  observeEvent(input$task_view_click, {
    req(rv$logged_in)
    task <- task_get_by_id(input$task_view_click)
    if (nrow(task) == 0) return(); t <- task[1, ]; rv$task_modal_id <- t$id
    pc <- clr("task_priority", t$priority); sc <- clr("task_status", t$status)
    sl <- lbl("task_status", t$status)
    logs <- task_log_get_by_task(t$id); lh <- ""
    if (nrow(logs) > 0) {
      for (i in 1:nrow(logs)) {
        lg <- logs[i, ]
        badge <- switch(lg$log_type,
          execution='<span style="background:#337ab7;color:white;padding:2px 8px;border-radius:3px;font-size:11px;">\u6267\u884c</span>',
          feedback='<span style="background:#5cb85c;color:white;padding:2px 8px;border-radius:3px;font-size:11px;">\u53cd\u9988</span>',
          status_change='<span style="background:#f0ad4e;color:white;padding:2px 8px;border-radius:3px;font-size:11px;">\u72b6\u6001</span>',
          note='<span style="background:#5bc0de;color:white;padding:2px 8px;border-radius:3px;font-size:11px;">\u5907\u6ce8</span>',
          '<span style="background:#999;color:white;padding:2px 8px;border-radius:3px;font-size:11px;">\u5176\u4ed6</span>')
        lh <- paste0(lh, sprintf('<div style="background:#f9f9f9;padding:10px 14px;margin-bottom:8px;border-radius:6px;border-left:4px solid #5bc0de;"><div style="font-size:12px;color:#666;margin-bottom:4px;">%s <b style="margin-left:8px;">%s</b><span style="float:right;">%s</span></div><div style="font-size:13px;line-height:1.6;">%s</div></div>',
          badge, ifelse(is.na(lg$creator_name), "\u7cfb\u7edf", lg$creator_name), lg$created_at, render_log_content(lg$content)))
      }
    } else { lh <- '<div style="color:#999;padding:10px;">\u6682\u65e0\u8bb0\u5f55</div>' }
    mc <- HTML(sprintf('<div style="padding:10px;"><div style="background:#f5f5f5;padding:14px;border-radius:6px;margin-bottom:15px;"><table style="width:100%%;font-size:14px;"><tr><td style="width:90px;font-weight:bold;color:#666;">\u4efb\u52a1\u7f16\u53f7\uff1a</td><td style="color:#337ab7;font-weight:bold;">%s</td><td style="width:90px;font-weight:bold;color:#666;">\u540d\u79f0\uff1a</td><td style="font-weight:bold;">%s</td></tr><tr><td colspan="4" style="height:6px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u4f18\u5148\u7ea7\uff1a</td><td><span style="background:%s;color:white;padding:2px 8px;border-radius:4px;">%s</span></td><td style="font-weight:bold;color:#666;">\u72b6\u6001\uff1a</td><td><span style="background:%s;color:white;padding:2px 8px;border-radius:4px;">%s</span></td></tr><tr><td colspan="4" style="height:6px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u9879\u76ee\uff1a</td><td>%s</td><td style="font-weight:bold;color:#666;">\u9636\u6bb5\uff1a</td><td>%s</td></tr><tr><td colspan="4" style="height:6px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u5de5\u4f5c\u5305\uff1a</td><td>%s</td><td style="font-weight:bold;color:#666;">\u8d1f\u8d23\u4eba\uff1a</td><td>%s</td></tr><tr><td colspan="4" style="height:6px;"></td></tr><tr><td style="font-weight:bold;color:#666;">\u622a\u6b62\u65e5\u671f\uff1a</td><td>%s</td><td style="font-weight:bold;color:#666;">\u5173\u8054\u5de5\u5355\uff1a</td><td>%s</td></tr></table></div><div style="margin-bottom:10px;"><b>\u4efb\u52a1\u63cf\u8ff0</b><div style="background:#fafafa;padding:12px;border-radius:6px;border-left:4px solid #337ab7;min-height:30px;white-space:pre-wrap;margin-top:6px;">%s</div></div><div><b>\u6267\u884c\u4e0e\u53cd\u9988\u8bb0\u5f55 <span style="font-weight:normal;color:#888;font-size:12px;">\uff08\u5171 %d \u6761\uff09</span></b><div style="margin-top:6px;max-height:350px;overflow-y:auto;border:1px solid #eee;border-radius:6px;padding:8px;">%s</div></div></div>',
      ifelse(is.na(t$task_no), "-", t$task_no), t$name, pc, t$priority, sc, sl,
      ifelse(is.na(t$project_name), "-", t$project_name), ifelse(is.na(t$phase_name), "-", t$phase_name),
      ifelse(is.na(t$wp_name), "-", t$wp_name), ifelse(is.na(t$assignee_name), "\u672a\u6307\u5b9a", t$assignee_name),
      ifelse(is.na(t$due_date), "-", t$due_date), ifelse(is.na(t$work_order_id), "\u65e0", as.character(t$work_order_id)),
      ifelse(is.na(t$description), "\u65e0\u63cf\u8ff0", t$description), nrow(logs), lh))
    sb <- tagList()
    if (t$status == "pending") sb <- tagList(actionButton("task_modal_start", "\u5f00\u59cb\u6267\u884c", class = "btn-primary"), actionButton("task_modal_block", "\u6807\u8bb0\u963b\u585e", class = "btn-danger"))
    else if (t$status == "in_progress") sb <- tagList(actionButton("task_modal_complete", "\u5b8c\u6210\u4efb\u52a1", class = "btn-success"), actionButton("task_modal_block", "\u6807\u8bb0\u963b\u585e", class = "btn-danger"))
    else if (t$status == "blocked") sb <- tagList(actionButton("task_modal_start", "\u6062\u590d\u6267\u884c", class = "btn-primary"))
    showModal(modalDialog(title = paste0("\u4efb\u52a1\u8be6\u60c5 - ", ifelse(is.na(t$task_no), "", t$task_no)), mc,
      wellPanel(h5("\u6dfb\u52a0\u53cd\u9988"), fluidRow(
        column(3, selectInput("task_modal_log_type", NULL, choices = c("\u6267\u884c\u8bb0\u5f55"="execution","\u53cd\u9988"="feedback","\u5907\u6ce8"="note"))),
        column(9, textAreaInput("task_modal_log_content", NULL, placeholder = "\u652f\u6301\u591a\u884c\u6587\u672c\uff0c\u53ef\u7c98\u8d34 HTML \u6216 Markdown \u683c\u5f0f\u5185\u5bb9", rows = 5))),
        fluidRow(column(12, div(style = "text-align:right;margin-top:8px;", actionButton("task_modal_add_log", "\u63d0\u4ea4\u53cd\u9988", class = "btn-primary"))))),
      footer = tagList(sb, actionButton("task_modal_delete", "\u5220\u9664\u4efb\u52a1", class = "btn-danger"), modalButton("\u5173\u95ed")),
      size = "l", easyClose = TRUE))
  })
  observeEvent(input$task_modal_start, { req(rv$task_modal_id); task_update_status(rv$task_modal_id, "in_progress", rv$current_user); showNotification("\u4efb\u52a1\u5df2\u5f00\u59cb", type = "message"); removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1 })
  observeEvent(input$task_modal_complete, { req(rv$task_modal_id); task_update_status(rv$task_modal_id, "completed", rv$current_user); showNotification("\u4efb\u52a1\u5df2\u5b8c\u6210", type = "message"); removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1 })
  observeEvent(input$task_modal_block, { req(rv$task_modal_id); task_update_status(rv$task_modal_id, "blocked", rv$current_user); showNotification("\u4efb\u52a1\u5df2\u6807\u8bb0\u963b\u585e", type = "warning"); removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1 })
  observeEvent(input$task_modal_delete, { req(rv$task_modal_id); removeModal(); showModal(modalDialog(title = "\u786e\u8ba4\u5220\u9664", "\u786e\u5b9a\u5220\u9664\u8be5\u4efb\u52a1\u53ca\u5176\u6240\u6709\u53cd\u9988\u8bb0\u5f55\u5417\uff1f", footer = tagList(actionButton("task_confirm_delete", "\u786e\u8ba4\u5220\u9664", class = "btn-danger"), modalButton("\u53d6\u6d88")), easyClose = TRUE)) })
  observeEvent(input$task_confirm_delete, { result <- task_delete(rv$task_modal_id, rv$current_user); showNotification(result$message, type = ifelse(result$success, "message", "error")); removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1 })
  observeEvent(input$task_modal_add_log, {
    req(rv$task_modal_id, input$task_modal_log_content)
    if (nchar(trimws(input$task_modal_log_content)) == 0) { showNotification("\u8bf7\u8f93\u5165\u53cd\u9988\u5185\u5bb9", type = "error"); return() }
    result <- task_log_add(rv$task_modal_id, input$task_modal_log_type, input$task_modal_log_content, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { removeModal(); Sys.sleep(0.3); session$sendInputMessage("task_view_click", list(value = rv$task_modal_id)) }
  })
  observeEvent(input$task_to_wo_click, { req(rv$logged_in); rv$task_to_wo_id <- input$task_to_wo_click; showModal(modalDialog(title = "\u4efb\u52a1\u8f6c\u5de5\u5355", "\u786e\u5b9a\u5c06\u8be5\u4efb\u52a1\u8f6c\u4e3a\u5de5\u5355\uff1f\u7cfb\u7edf\u5c06\u81ea\u52a8\u521b\u5efa\u5173\u8054\u5de5\u5355\u7528\u4e8e\u5206\u6d3e\u5904\u7406\u3002", footer = tagList(actionButton("task_to_wo_confirm", "\u786e\u8ba4\u8f6c\u6362", class = "btn-primary"), modalButton("\u53d6\u6d88")), easyClose = TRUE)) })
  observeEvent(input$task_to_wo_confirm, { req(rv$task_to_wo_id); result <- task_convert_to_work_order(rv$task_to_wo_id, rv$current_user); showNotification(result$message, type = ifelse(result$success, "message", "error")); removeModal(); rv$proj_data_refresh <- rv$proj_data_refresh + 1 })

  # ================================================================
  # 选项配置管理（管理模块 server）
  # ================================================================
  rv$co_refresh <- 0
  rv$co_edit_id <- NULL

  output$co_option_table <- renderDT({
    rv$co_refresh; input$co_refresh
    cat <- if (!is.null(input$co_category)) input$co_category else "project_status"
    data <- config_option_get(cat)
    if (nrow(data) > 0) {
      display <- data.frame(
        ID = data$id,
        `选项值` = data$option_value,
        `显示名称` = data$option_label,
        `颜色` = sprintf('<span style="background:%s;color:white;padding:2px 10px;border-radius:3px;">%s</span>', ifelse(data$color == "", "#999", data$color), data$option_label),
        `排序` = data$sort_order,
        `默认` = ifelse(data$is_default == 1, "\u2713", ""),
        stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      display <- data.frame(ID=integer(), `选项值`=character(), `显示名称`=character(), `颜色`=character(), `排序`=integer(), `默认`=character(), stringsAsFactors=FALSE, check.names=FALSE)
    }
    DT::datatable(display, escape = FALSE, selection = 'single',
      options = list(pageLength = 20, dom = 'tip'), rownames = FALSE, class = 'cell-border stripe hover')
  })
  # 点击行填充编辑
  observeEvent(input$co_option_table_rows_selected, {
    sel <- input$co_option_table_rows_selected
    cat <- if (!is.null(input$co_category)) input$co_category else "project_status"
    data <- config_option_get(cat)
    if (!is.null(sel) && sel <= nrow(data)) {
      row <- data[sel, ]; rv$co_edit_id <- row$id
      updateTextInput(session, "co_value", value = row$option_value)
      updateTextInput(session, "co_label", value = row$option_label)
      updateTextInput(session, "co_color", value = row$color)
      updateNumericInput(session, "co_sort", value = row$sort_order)
      updateSelectInput(session, "co_default", selected = as.character(row$is_default))
    }
  })
  observeEvent(input$co_add, {
    if (is.null(input$co_value) || trimws(input$co_value) == "" || is.null(input$co_label) || trimws(input$co_label) == "") {
      showNotification("\u8bf7\u586b\u5199\u9009\u9879\u503c\u548c\u663e\u793a\u540d\u79f0", type = "error"); return()
    }
    result <- config_option_add(input$co_category, input$co_value, input$co_label,
      input$co_color, input$co_sort, as.integer(input$co_default))
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { updateTextInput(session, "co_value", value = ""); updateTextInput(session, "co_label", value = ""); rv$co_refresh <- rv$co_refresh + 1 }
  })
  observeEvent(input$co_save_edit, {
    if (is.null(rv$co_edit_id)) { showNotification("\u8bf7\u5148\u70b9\u51fb\u8868\u683c\u4e2d\u7684\u884c\u9009\u62e9\u8981\u7f16\u8f91\u7684\u9879", type = "warning"); return() }
    result <- config_option_update(rv$co_edit_id, input$co_value, input$co_label,
      input$co_color, input$co_sort, as.integer(input$co_default))
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) { rv$co_edit_id <- NULL; rv$co_refresh <- rv$co_refresh + 1 }
  })
}
