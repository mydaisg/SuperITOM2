# 数据中心模块 服务端
# 数据归集模块服务端逻辑

data_center_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
  ##################
  # 数据统计函数
  ##################
  
  # 获取日报统计（日报是实时生成的，统计活跃用户和当日工作）
  daily_report_get_stats <- function() {
    con <- db_connect()
    tryCatch({
      today <- format(Sys.Date(), "%Y-%m-%d")
      month_start <- format(Sys.Date(), "%Y-%m")
      
      # 统计活跃用户总数
      total_users <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM users WHERE active = 1")$cnt[1]
      
      # 统计当天的工单活动
      wo_today <- dbGetQuery(con, sprintf("
        SELECT COUNT(DISTINCT assigned_to) as cnt FROM work_orders 
        WHERE DATE(created_at) = '%s' OR DATE(updated_at) = '%s'", today, today))$cnt[1]
      
      # 统计当天的任务活动
      task_today <- dbGetQuery(con, sprintf("
        SELECT COUNT(DISTINCT assigned_to) as cnt FROM project_tasks 
        WHERE DATE(created_at) = '%s' OR DATE(updated_at) = '%s'", today, today))$cnt[1]
      
      # 本月有活动的工单数
      wo_month <- dbGetQuery(con, sprintf("
        SELECT COUNT(*) as cnt FROM work_orders 
        WHERE created_at LIKE '%s%%'", month_start))$cnt[1]
      
      return(data.frame(
        total_users = total_users,
        wo_today = wo_today,
        task_today = task_today,
        wo_month = wo_month
      ))
    }, error = function(e) {
      data.frame(total_users = 0, wo_today = 0, task_today = 0, wo_month = 0)
    }, finally = {
      db_disconnect(con)
    })
  }
  
  # 获取网络测试日志数量
  network_test_get_stats <- function() {
    tryCatch({
      log_dir <- file.path(getwd(), "Log", "network_test")
      if (!dir.exists(log_dir)) {
        return(list(count = 0, last_time = "无记录"))
      }
      files <- list.files(log_dir, pattern = "\\.log$", full.names = FALSE)
      if (length(files) == 0) {
        return(list(count = 0, last_time = "无记录"))
      }
      # 按修改时间排序
      file_info <- file.info(file.path(log_dir, files))
      file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
      last_file <- rownames(file_info)[1]
      last_time <- format(file_info$mtime[1], "%m-%d %H:%M")
      return(list(count = length(files), last_time = last_time, last_file = last_file))
    }, error = function(e) {
      list(count = 0, last_time = "无记录")
    })
  }
  
  ##################
  # 统计数据渲染
  ##################
  
  # 项目统计
  output$proj_total <- renderText({
    stats <- project_get_stats()
    as.character(stats$total[1])
  })
  
  output$proj_active <- renderText({
    stats <- project_get_stats()
    as.character(stats$active[1] + stats$planning[1])
  })
  
  output$proj_completed <- renderText({
    stats <- project_get_stats()
    as.character(stats$completed[1])
  })
  
  # 工单统计
  output$wo_total <- renderText({
    stats <- work_order_get_stats()
    as.character(stats$total[1])
  })
  
  output$wo_pending <- renderText({
    stats <- work_order_get_stats()
    as.character(stats$pending[1] + stats$assigned[1] + stats$processing[1])
  })
  
  output$wo_completed <- renderText({
    stats <- work_order_get_stats()
    as.character(stats$completed[1] + stats$closed[1])
  })
  
  # 巡检统计
  output$insp_plans <- renderText({
    stats <- inspection_get_stats()
    as.character(stats$total_plans[1])
  })
  
  output$insp_tasks <- renderText({
    stats <- inspection_get_stats()
    as.character(stats$active_plans[1])
  })
  
  output$insp_issues <- renderText({
    stats <- inspection_get_stats()
    as.character(stats$pending_issues[1])
  })
  
  # 网络测试统计
  output$nt_logs <- renderText({
    stats <- network_test_get_stats()
    as.character(stats$count)
  })
  
  output$nt_last <- renderText({
    stats <- network_test_get_stats()
    stats$last_time
  })
  
  output$nt_dir <- renderText({
    "Log/network_test"
  })
  
  # 日报统计
  output$dr_total <- renderText({
    stats <- daily_report_get_stats()
    as.character(stats$total_users[1])
  })
  
  output$dr_month <- renderText({
    stats <- daily_report_get_stats()
    as.character(stats$wo_month[1])
  })
  
  output$dr_today <- renderText({
    stats <- daily_report_get_stats()
    as.character(stats$wo_today[1] + stats$task_today[1])
  })
  
  ##################
  # 卡片点击事件
  ##################
  
  # 当前选中的模块
  current_module <- reactiveVal(NULL)
  
  # 项目卡片点击
  observeEvent(input$card_project, {
    current_module("project")
  })
  
  # 工单卡片点击
  observeEvent(input$card_workorder, {
    current_module("workorder")
  })
  
  # 巡检卡片点击
  observeEvent(input$card_inspection, {
    current_module("inspection")
  })
  
  # 测试卡片点击
  observeEvent(input$card_network, {
    current_module("network")
  })
  
  # 日报卡片点击
  observeEvent(input$card_daily, {
    current_module("daily")
  })
  
  # 渲染明细区域
  output$detail_container <- renderUI({
    module <- current_module()
    
    if (is.null(module)) {
      return(div(class = "detail-section",
        p("点击上方模块卡片查看数据明细", style = "text-align: center; color: #95a5a6; padding: 40px;")
      ))
    }
    
    switch(module,
      "project" = render_project_detail(),
      "workorder" = render_workorder_detail(),
      "inspection" = render_inspection_detail(),
      "network" = render_network_detail(),
      "daily" = render_daily_detail()
    )
  })
  
  ##################
  # 各模块明细渲染
  ##################
  
  # 项目明细
  render_project_detail <- function() {
    projects <- project_get_all()
    
    if (nrow(projects) == 0) {
      return(div(class = "detail-section",
        h4(icon("folder-open"), " 项目明细"),
        actionLink(ns("go_project"), "前往项目模块 →", class = "btn btn-primary btn-sm"),
        hr(),
        p("暂无项目数据")
      ))
    }
    
    tagList(
      div(class = "detail-section",
        h4(icon("folder-open"), " 项目明细"),
        div(
          actionLink(ns("go_project"), "前往项目模块 →", class = "btn btn-primary btn-sm"),
          span(style = "float: right; color: #7f8c8d;", sprintf("共 %d 个项目", nrow(projects)))
        ),
        hr(),
        DT::DTOutput(ns("project_detail_table"))
      )
    )
  }
  
  output$project_detail_table <- DT::renderDT({
    projects <- project_get_all()
    
    if (nrow(projects) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无项目数据")))
    }
    
    display <- projects[, c("project_no", "name", "status", "creator_name", "created_at")]
    colnames(display) <- c("项目编号", "项目名称", "状态", "创建人", "创建时间")
    
    DT::datatable(display, escape = FALSE,
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'rtip',
        columnDefs = list(
          list(targets = 0, width = '130px'),
          list(targets = 2, width = '80px', className = 'dt-center')
        )
      ),
      rownames = FALSE, class = 'cell-border stripe hover'
    )
  })
  
  # 工单明细
  render_workorder_detail <- function() {
    orders <- work_order_get_all()
    
    if (nrow(orders) == 0) {
      return(div(class = "detail-section",
        h4(icon("clipboard-list"), " 工单明细"),
        p("暂无工单数据")
      ))
    }
    
    tagList(
      div(class = "detail-section",
        h4(icon("clipboard-list"), " 工单明细"),
        div(
          span(style = "color: #7f8c8d;", sprintf("共 %d 个工单", nrow(orders))),
          span(style = "float: right;", 
            sprintf("待处理: %d | 进行中: %d | 已完成: %d",
              sum(orders$status %in% c("pending", "assigned")),
              sum(orders$status == "processing"),
              sum(orders$status %in% c("completed", "closed"))
            ))
        ),
        hr(),
        DT::DTOutput(ns("workorder_detail_table"))
      )
    )
  }
  
  output$workorder_detail_table <- DT::renderDT({
    orders <- work_order_get_all()
    
    if (nrow(orders) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无工单数据")))
    }
    
    display <- orders[, c("order_no", "title", "priority", "status", "created_by_name", "assigned_to_name", "created_at")]
    colnames(display) <- c("工单编号", "工单标题", "优先级", "状态", "创建人", "处理人", "创建时间")
    
    DT::datatable(display, escape = FALSE,
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'rtip',
        columnDefs = list(
          list(targets = 2, width = '70px', className = 'dt-center'),
          list(targets = 3, width = '80px', className = 'dt-center')
        )
      ),
      rownames = FALSE, class = 'cell-border stripe hover'
    )
  })
  
  # 巡检明细
  render_inspection_detail <- function() {
    stats <- inspection_get_stats()
    
    tagList(
      div(class = "detail-section",
        h4(icon("clipboard-check"), " 巡检明细"),
        div(
          span(style = "color: #7f8c8d;", sprintf("计划: %d | 任务: %d | 异常: %d",
            stats$total_plans[1], stats$total_tasks[1], stats$total_issues[1]))
        ),
        hr(),
        # 巡检概览统计
        fluidRow(
          column(3,
            div(class = "well well-sm", style = "text-align: center;",
              h3(stats$active_plans[1], style = "margin: 0; color: #27ae60;"),
              p("执行中计划", style = "margin: 5px 0 0 0; font-size: 12px; color: #7f8c8d;")
            )
          ),
          column(3,
            div(class = "well well-sm", style = "text-align: center;",
              h3(stats$pending_tasks[1], style = "margin: 0; color: #f39c12;"),
              p("待执行任务", style = "margin: 5px 0 0 0; font-size: 12px; color: #7f8c8d;")
            )
          ),
          column(3,
            div(class = "well well-sm", style = "text-align: center;",
              h3(stats$completed_tasks[1], style = "margin: 0; color: #3498db;"),
              p("已完成任务", style = "margin: 5px 0 0 0; font-size: 12px; color: #7f8c8d;")
            )
          ),
          column(3,
            div(class = "well well-sm", style = "text-align: center;",
              h3(stats$pending_issues[1], style = "margin: 0; color: #e74c3c;"),
              p("待处理异常", style = "margin: 5px 0 0 0; font-size: 12px; color: #7f8c8d;")
            )
          )
        ),
        hr(),
        p("如需查看详细巡检数据，请前往 ", strong("巡检"), " 模块", style = "text-align: center; color: #7f8c8d;")
      )
    )
  }
  
  # 网络测试明细
  render_network_detail <- function() {
    stats <- network_test_get_stats()
    
    log_dir <- file.path(getwd(), "Log", "network_test")
    log_files <- if (dir.exists(log_dir)) list.files(log_dir, pattern = "\\.log$", full.names = TRUE) else character(0)
    
    # 读取最近的日志文件
    recent_logs <- data.frame()
    if (length(log_files) > 0) {
      file_info <- file.info(log_files)
      file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
      recent_files <- head(rownames(file_info), 10)
      
      for (f in recent_files) {
        fname <- basename(f)
        mtime <- format(file_info[f, ]$mtime, "%Y-%m-%d %H:%M")
        recent_logs <- rbind(recent_logs, data.frame(
          文件名 = fname,
          修改时间 = mtime,
          大小 = format(file.info(f)$size / 1024, digits = 1)
        ))
      }
    }
    colnames(recent_logs) <- c("文件名", "修改时间", "大小(KB)")
    
    tagList(
      div(class = "detail-section",
        h4(icon("wifi"), " 网络测试明细"),
        div(
          span(style = "color: #7f8c8d;", sprintf("共 %d 条测试记录", stats$count)),
          span(style = "float: right;", sprintf("最近测试: %s", stats$last_time))
        ),
        hr(),
        # 目录信息
        div(class = "alert alert-info",
          icon("info-circle"), 
          sprintf("日志目录: %s", log_dir)
        ),
        if (nrow(recent_logs) > 0) {
          tagList(
            p("最近10条测试记录:", style = "font-weight: bold; margin-top: 15px;"),
            DT::DTOutput(ns("network_detail_table"))
          )
        } else {
          p("暂无测试记录", style = "text-align: center; color: #95a5a6; padding: 20px;")
        }
      )
    )
  }
  
  output$network_detail_table <- DT::renderDT({
    log_dir <- file.path(getwd(), "Log", "network_test")
    log_files <- if (dir.exists(log_dir)) list.files(log_dir, pattern = "\\.log$", full.names = TRUE) else character(0)
    
    recent_logs <- data.frame()
    if (length(log_files) > 0) {
      file_info <- file.info(log_files)
      file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
      recent_files <- head(rownames(file_info), 10)
      
      for (f in recent_files) {
        fname <- basename(f)
        mtime <- format(file_info[f, ]$mtime, "%Y-%m-%d %H:%M")
        recent_logs <- rbind(recent_logs, data.frame(
          文件名 = fname,
          修改时间 = mtime,
          大小 = sprintf("%.1f KB", file.info(f)$size / 1024)
        ))
      }
    }
    
    if (nrow(recent_logs) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无测试记录")))
    }
    
    DT::datatable(recent_logs, escape = FALSE,
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'rtip',
        columnDefs = list(
          list(targets = 1, width = '150px'),
          list(targets = 2, width = '80px', className = 'dt-center')
        )
      ),
      rownames = FALSE, class = 'cell-border stripe hover'
    )
  })
  
  # 日报明细
  render_daily_detail <- function() {
    stats <- daily_report_get_stats()
    
    con <- db_connect()
    
    # 获取当天的工单活动
    today_orders <- tryCatch({
      today <- format(Sys.Date(), "%Y-%m-%d")
      dbGetQuery(con, sprintf("
        SELECT DISTINCT u.username, u.display_name, COUNT(wo.id) as wo_count
        FROM users u
        LEFT JOIN work_orders wo ON u.id = wo.assigned_to AND (DATE(wo.created_at) = '%s' OR DATE(wo.updated_at) = '%s')
        WHERE u.active = 1
        GROUP BY u.id
        ORDER BY wo_count DESC
        LIMIT 20", today, today))
    }, error = function(e) {
      data.frame()
    })
    
    # 获取活跃用户列表
    active_users <- tryCatch({
      dbGetQuery(con, "SELECT username, display_name, role FROM users WHERE active = 1 ORDER BY username")
    }, error = function(e) {
      data.frame()
    })
    
    db_disconnect(con)
    
    tagList(
      div(class = "detail-section",
        h4(icon("calendar-alt"), " 日报明细"),
        div(
          span(style = "color: #7f8c8d;", sprintf("活跃用户: %d | 本月工单: %d | 今日活动: %d",
            stats$total_users[1], stats$wo_month[1], stats$wo_today[1] + stats$task_today[1]))
        ),
        hr(),
        # 日报说明
        div(class = "alert alert-info",
          icon("info-circle"), 
          "日报模块实时汇总工单和任务数据，自动按人生成工作日报"
        ),
        if (nrow(today_orders) > 0) {
          tagList(
            p("今日有活动的用户:", style = "font-weight: bold; margin-top: 15px;"),
            DT::DTOutput(ns("daily_activity_table"))
          )
        } else {
          p("今日暂无活动记录", style = "text-align: center; color: #95a5a6; padding: 20px;")
        },
        hr(),
        p("如需查看/编辑日报，请前往 ", strong("日报"), " 模块", style = "text-align: center; color: #7f8c8d;")
      )
    )
  }
  
  output$daily_activity_table <- DT::renderDT({
    con <- db_connect()
    today <- format(Sys.Date(), "%Y-%m-%d")
    today_orders <- tryCatch({
      dbGetQuery(con, sprintf("
        SELECT u.username, u.display_name, COUNT(wo.id) as wo_count
        FROM users u
        LEFT JOIN work_orders wo ON u.id = wo.assigned_to AND (DATE(wo.created_at) = '%s' OR DATE(wo.updated_at) = '%s')
        WHERE u.active = 1
        GROUP BY u.id
        ORDER BY wo_count DESC
        LIMIT 20", today, today))
    }, error = function(e) {
      data.frame()
    })
    db_disconnect(con)
    
    if (nrow(today_orders) == 0) {
      return(DT::datatable(data.frame(信息 = "暂无活动记录")))
    }
    
    colnames(today_orders) <- c("用户名", "显示名", "工单数")
    
    DT::datatable(today_orders, escape = FALSE,
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'rtip',
        columnDefs = list(
          list(targets = 2, width = '80px', className = 'dt-center')
        )
      ),
      rownames = FALSE, class = 'cell-border stripe hover'
    )
  })
  
  ##################
  # 穿透链接
  ##################
  
  # 前往项目模块
  observeEvent(input$go_project, {
    parent_session <- session$parent
    if (!is.null(parent_session)) {
      updateTabsetPanel(parent_session, "main_tabs", selected = "项目")
    }
  })
  
  })  # moduleServer
}
