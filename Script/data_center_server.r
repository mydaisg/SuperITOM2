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
  # 卡片点击事件（穿透导航）
  ##################
  
  # 项目卡片点击 → 直达项目模块
  observeEvent(input$card_project, {
    navigate_to_tab("项目")
  })
  
  # 工单卡片点击 → 直达工单模块
  observeEvent(input$card_workorder, {
    navigate_to_tab("工单")
  })
  
  # 巡检卡片点击 → 直达巡检模块
  observeEvent(input$card_inspection, {
    navigate_to_tab("巡检")
  })
  
  # 测试卡片点击 → 直达测试模块
  observeEvent(input$card_network, {
    navigate_to_tab("测试")
  })
  
  # 日报卡片点击 → 直达日报模块
  observeEvent(input$card_daily, {
    navigate_to_tab("日报")
  })
  
  # 渲染明细区域（卡片点击已改为直接导航，此区域仅作为快捷入口）
  output$detail_container <- renderUI({
    div(class = "detail-section",
      p("点击上方模块卡片，可直接跳转到对应模块页面", 
        style = "text-align: center; color: #95a5a6; padding: 20px;")
    )
  })
  
  ##################
  # 穿透链接：导航到各模块
  ##################
  
  # 封装导航函数：通过 JS 消息触发 switchToTab，无会话作用域问题
  navigate_to_tab <- function(tab_name) {
    session$sendCustomMessage("navigateToTab", tab_name)
  }
  
  # 前往项目模块
  observeEvent(input$go_project, {
    navigate_to_tab("项目")
  })
  
  # 前往工单模块
  observeEvent(input$go_workorder, {
    navigate_to_tab("工单")
  })
  
  # 前往巡检模块
  observeEvent(input$go_inspection, {
    navigate_to_tab("巡检")
  })
  
  # 前往测试模块
  observeEvent(input$go_network, {
    navigate_to_tab("测试")
  })
  
  # 前往日报模块
  observeEvent(input$go_daily, {
    navigate_to_tab("日报")
  })
  
  ##################
  # 新增模块统计
  ##################
  
  # 资产统计
  output$ast_total <- renderText({
    items <- tryCatch(asset_get_all(), error=function(e) data.frame())
    as.character(nrow(items))
  })
  output$ast_active <- renderText({
    items <- tryCatch(asset_get_all(), error=function(e) data.frame())
    as.character(sum(items$status == "active", na.rm=TRUE))
  })
  output$ast_maint <- renderText({
    items <- tryCatch(asset_get_all(), error=function(e) data.frame())
    as.character(sum(items$status == "maintenance", na.rm=TRUE))
  })
  
  # 记事统计
  output$note_total <- renderText({
    items <- tryCatch(note_get_all(), error=function(e) data.frame())
    as.character(nrow(items))
  })
  output$note_pending <- renderText({
    items <- tryCatch(note_get_all(), error=function(e) data.frame())
    as.character(sum(items$status == "pending", na.rm=TRUE))
  })
  output$note_progress <- renderText({
    items <- tryCatch(note_get_all(), error=function(e) data.frame())
    as.character(sum(items$status == "in_progress", na.rm=TRUE))
  })
  
  # 岗职统计
  output$duty_pos <- renderText({
    items <- tryCatch(duty_position_get_all(), error=function(e) data.frame())
    as.character(nrow(items))
  })
  output$duty_staff <- renderText({
    items <- tryCatch(duty_staff_get_all(), error=function(e) data.frame())
    as.character(nrow(items))
  })
  output$duty_items <- renderText({
    items <- tryCatch(duty_item_get_all(), error=function(e) data.frame())
    as.character(nrow(items))
  })
  
  # 绩效统计
  output$perf_sheets <- renderText({
    items <- tryCatch(perf_sheet_list(), error=function(e) data.frame())
    as.character(nrow(items))
  })
  output$perf_emps <- renderText({
    current <- format(Sys.Date(), "%Y-%m")
    emps <- tryCatch(perf_active_employees(current), error=function(e) data.frame())
    as.character(nrow(emps))
  })
  output$perf_inds <- renderText({
    as.character(length(perf_indicators()))
  })
  
  ##################
  # 新卡片导航
  ##################
  observeEvent(input$card_asset, { navigate_to_tab("资产") })
  observeEvent(input$card_note, { navigate_to_tab("记事") })
  observeEvent(input$card_duty, { navigate_to_tab("岗职") })
  observeEvent(input$card_perf, { navigate_to_tab("绩效") })
  
  })  # moduleServer
}
