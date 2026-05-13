# ============================================================
# 巡检管理模块 - 服务端逻辑
# 流程：计划 -> 任务 -> 执行 -> 记录/异常 -> 整改工单
# ============================================================

inspection_server <- function(input, output, session, rv) {
  
  # ========================================
  # 响应式值初始化
  # ========================================
  rv$inspection_refresh_trigger <- 0
  rv$inspection_selected_plan_id <- NULL
  rv$inspection_selected_task_id <- NULL
  
  # ========================================
  # Admin权限判断（用于UI条件渲染）
  # ========================================
  output$isAdminUser <- reactive({
    if (!is.null(rv$logged_in) && rv$logged_in && 
        !is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      return(rv$current_user$role[1] == "admin")
    }
    return(FALSE)
  })
  outputOptions(output, "isAdminUser", suspendWhenHidden = FALSE)
  
  # 监听用户登录状态，设置Admin权限标志供conditionalPanel使用
  observe({
    req(rv$logged_in)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    session$sendInputMessage("isAdminInspectionUser", list(value = is_admin))
  })
  
  # ========================================
  # 巡检统计卡片
  # ========================================
  output$insp_stat_plans <- renderText({
    rv$inspection_refresh_trigger
    stats <- inspection_get_stats()
    as.character(stats$total_plans[1])
  })
  
  output$insp_stat_active_plans <- renderText({
    rv$inspection_refresh_trigger
    stats <- inspection_get_stats()
    as.character(stats$active_plans[1])
  })
  
  output$insp_stat_pending_tasks <- renderText({
    rv$inspection_refresh_trigger
    stats <- inspection_get_stats()
    as.character(stats$pending_tasks[1])
  })
  
  output$insp_stat_completed_tasks <- renderText({
    rv$inspection_refresh_trigger
    stats <- inspection_get_stats()
    as.character(stats$completed_tasks[1])
  })
  
  output$insp_stat_abnormal_tasks <- renderText({
    rv$inspection_refresh_trigger
    stats <- inspection_get_stats()
    as.character(stats$abnormal_tasks[1])
  })
  
  output$insp_stat_issues <- renderText({
    rv$inspection_refresh_trigger
    stats <- inspection_get_stats()
    as.character(stats$pending_issues[1])
  })
  
  # ========================================
  # Admin专属：已删除记录Tab内容渲染（Tab外壳在main_ui.r中定义）
  # ========================================
  output$insp_admin_deleted_content <- renderUI({
    req(rv$logged_in, rv$current_user)
    
    # 检查是否为Admin
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    
    if (is_admin) {
      tagList(
        br(),
        fluidRow(
          column(12,
            div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
              strong("提示："), "此页面仅Admin可见，显示已删除的巡检计划和记录，可用于审计追溯。"
            )
          )
        ),
        fluidRow(
          column(6,
            wellPanel(
              h4("已删除的巡检计划", style = "color: #d9534f;"),
              DTOutput("insp_deleted_plans_table")
            )
          ),
          column(6,
            wellPanel(
              h4("已删除的巡检记录", style = "color: #d9534f;"),
              DTOutput("insp_deleted_records_table")
            )
          )
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # Admin专属：已删除计划表格渲染
  output$insp_deleted_plans_table <- renderDT({
    req(rv$logged_in, rv$current_user)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    req(is_admin)
    rv$inspection_refresh_trigger
    
    plans <- inspection_plan_get_deleted()
    
    if (nrow(plans) > 0) {
      display_data <- data.frame(
        计划编号 = plans$plan_no,
        计划名称 = plans$name,
        巡检项 = ifelse(is.na(plans$inspection_category), "—", plans$inspection_category),
        状态 = '<span style="background:#d9534f;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">已删除</span>',
        创建时间 = ifelse(is.na(plans$created_at), "—", substr(plans$created_at, 1, 16)),
        删除时间 = ifelse(is.na(plans$updated_at), "—", substr(plans$updated_at, 1, 16))
      )
    } else {
      display_data <- data.frame(
        计划编号 = character(), 计划名称 = character(), 巡检项 = character(),
        状态 = character(), 创建时间 = character(), 删除时间 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 10, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '120px'),
        list(targets = 1, width = '180px'),
        list(targets = 2, width = '100px'),
        list(targets = 3, width = '70px', className = 'dt-center'),
        list(targets = 4, width = '120px'),
        list(targets = 5, width = '120px')
      )
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # Admin专属：已删除记录表格渲染
  output$insp_deleted_records_table <- renderDT({
    req(rv$logged_in, rv$current_user)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    req(is_admin)
    rv$inspection_refresh_trigger
    
    records <- inspection_record_get_deleted()
    
    if (nrow(records) > 0) {
      records$result_label <- sapply(records$result_type, function(r) {
        color <- ifelse(r == "normal", "#5cb85c", "#d9534f")
        label <- ifelse(r == "normal", "正常", "异常")
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      display_data <- data.frame(
        任务编号 = ifelse(is.na(records$task_no), "—", records$task_no),
        检查项 = ifelse(is.na(records$item_name), "—", records$item_name),
        检查人 = ifelse(is.na(records$inspector_name), "—", records$inspector_name),
        结果 = records$result_label,
        创建时间 = ifelse(is.na(records$created_at), "—", substr(records$created_at, 1, 16))
      )
    } else {
      display_data <- data.frame(
        任务编号 = character(), 检查项 = character(), 检查人 = character(),
        结果 = character(), 创建时间 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 10, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '110px'),
        list(targets = 1, width = '150px'),
        list(targets = 2, width = '80px'),
        list(targets = 3, width = '70px', className = 'dt-center'),
        list(targets = 4, width = '120px')
      )
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # ========================================
  # 巡检计划列表渲染
  # ========================================
  output$inspection_plan_table <- renderDT({
    req(rv$logged_in)
    rv$inspection_refresh_trigger
    
    plans <- inspection_plan_get_all(input$insp_plan_status_filter)
    
    if (nrow(plans) > 0) {
      plans$status_label <- sapply(plans$status, function(s) {
        color <- switch(s, 
                       "draft" = "#999", "active" = "#5cb85c", "paused" = "#f0ad4e", "completed" = "#337ab7", "#999")
        label <- switch(s, "draft" = "草稿", "active" = "进行中", "paused" = "已暂停", "completed" = "已完成", s)
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      # 巡检项颜色
      plans$insp_cat_color <- sapply(plans$inspection_category, function(c) {
        color_map <- c("数据中心巡检" = "#e3f2fd", "电力机房巡检" = "#fff3e0", 
                      "会议室巡检" = "#e8f5e9", "设备间巡检" = "#f3e5f5")
        bg <- ifelse(is.na(c), "#f5f5f5", color_map[[c]] %||% "#f5f5f5")
        sprintf('<span style="background:%s;padding:2px 8px;border-radius:4px;font-size:12px;">%s</span>', bg, ifelse(is.na(c), "—", c))
      })
      
      display_data <- data.frame(
        计划编号 = sprintf('<a href="#" class="insp-plan-link" data-id="%d" style="font-weight:bold;color:#337ab7;">%s</a>', 
                          plans$id, plans$plan_no),
        计划名称 = plans$name,
        巡检项 = plans$insp_cat_color,
        巡检分类 = ifelse(is.na(plans$category), "—", plans$category),
        周期 = ifelse(is.na(plans$cycle_type), "—", plans$cycle_type),
        负责人 = ifelse(is.na(plans$responsible_name), "—", plans$responsible_name),
        状态 = plans$status_label,
        创建时间 = ifelse(is.na(plans$created_at), "—", substr(plans$created_at, 1, 16)),
        更新时间 = ifelse(is.na(plans$updated_at), "—", substr(plans$updated_at, 1, 16))
      )
    } else {
      display_data <- data.frame(
        计划编号 = character(), 计划名称 = character(), 巡检项 = character(),
        巡检分类 = character(), 周期 = character(), 负责人 = character(),
        状态 = character(), 创建时间 = character(), 更新时间 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 20, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '130px'),
        list(targets = 1, width = '150px'),
        list(targets = 2, width = '100px'),
        list(targets = 3, width = '80px'),
        list(targets = 4, width = '70px'),
        list(targets = 5, width = '80px'),
        list(targets = 6, width = '70px', className = 'dt-center'),
        list(targets = 7, width = '130px'),
        list(targets = 8, width = '130px')
      )
    ), callback = JS(
      "table.on('click', 'a.insp-plan-link', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        Shiny.setInputValue('insp_plan_view', id, {priority: 'event'});
      });"
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # ========================================
  # 巡检任务列表渲染（计划视图下的任务）
  # ========================================
  output$inspection_task_table <- renderDT({
    req(rv$logged_in)
    rv$inspection_refresh_trigger
    
    tasks <- inspection_task_get_all(input$insp_task_status_filter, rv$inspection_selected_plan_id)
    
    if (nrow(tasks) > 0) {
      tasks$status_label <- sapply(tasks$status, function(s) {
        color <- switch(s, "pending" = "#f0ad4e", "processing" = "#5bc0de", 
                       "completed" = "#5cb85c", "abnormal" = "#d9534f", "#999")
        label <- switch(s, "pending" = "待执行", "processing" = "执行中", 
                       "completed" = "已完成", "abnormal" = "异常", s)
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      display_data <- data.frame(
        任务编号 = sprintf('<a href="#" class="insp-task-link" data-id="%d" style="font-weight:bold;color:#337ab7;">%s</a>',
                          tasks$id, tasks$task_no),
        检查项 = ifelse(is.na(tasks$item_display), "—", tasks$item_display),
        计划 = ifelse(is.na(tasks$plan_name), "—", tasks$plan_name),
        检查人 = ifelse(is.na(tasks$inspector_name), "—", tasks$inspector_name),
        计划日期 = ifelse(is.na(tasks$scheduled_date), "—", tasks$scheduled_date),
        状态 = tasks$status_label,
        更新时间 = ifelse(is.na(tasks$updated_at), "—", substr(tasks$updated_at, 1, 16))
      )
    } else {
      display_data <- data.frame(
        任务编号 = character(), 检查项 = character(), 计划 = character(),
        检查人 = character(), 计划日期 = character(), 状态 = character(), 更新时间 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 20, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '130px'),
        list(targets = 1, width = '150px'),
        list(targets = 2, width = '100px'),
        list(targets = 3, width = '80px'),
        list(targets = 4, width = '90px'),
        list(targets = 5, width = '70px', className = 'dt-center'),
        list(targets = 6, width = '130px')
      )
    ), callback = JS(
      "table.on('click', 'a.insp-task-link', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        Shiny.setInputValue('insp_task_view', id, {priority: 'event'});
      });"
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # ========================================
  # 我的巡检任务列表渲染
  # ========================================
  output$insp_my_task_table <- renderDT({
    req(rv$logged_in, rv$current_user)
    rv$inspection_refresh_trigger
    
    uid <- rv$current_user$id[1]
    tasks <- inspection_task_get_mine(uid, input$insp_my_status_filter)
    
    if (nrow(tasks) > 0) {
      tasks$status_label <- sapply(tasks$status, function(s) {
        color <- switch(s, "pending" = "#f0ad4e", "processing" = "#5bc0de", 
                       "completed" = "#5cb85c", "abnormal" = "#d9534f", "#999")
        label <- switch(s, "pending" = "待执行", "processing" = "执行中", 
                       "completed" = "已完成", "abnormal" = "异常", s)
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      # 优先级标记（过期任务标红）
      today <- format(Sys.Date(), "%Y-%m-%d")
      tasks$urgent <- sapply(seq_len(nrow(tasks)), function(i) {
        d <- tasks$scheduled_date[i]
        s <- tasks$status[i]
        if (!is.na(d) && !is.na(s) && d < today && s == "pending") {
          '<span style="color:#d9534f;font-weight:bold;">!</span>'
        } else ""
      })
      
      display_data <- data.frame(
        紧急 = tasks$urgent,
        任务编号 = sprintf('<a href="#" class="insp-mytask-link" data-id="%d" style="font-weight:bold;color:#337ab7;">%s</a>',
                          tasks$id, tasks$task_no),
        检查项 = ifelse(is.na(tasks$item_display), "—", tasks$item_display),
        计划 = ifelse(is.na(tasks$plan_name), "—", tasks$plan_name),
        计划日期 = ifelse(is.na(tasks$scheduled_date), "—", tasks$scheduled_date),
        状态 = tasks$status_label
      )
    } else {
      display_data <- data.frame(
        紧急 = character(), 任务编号 = character(), 检查项 = character(),
        计划 = character(), 计划日期 = character(), 状态 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 20, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '40px', className = 'dt-center'),
        list(targets = 1, width = '130px'),
        list(targets = 2, width = '180px'),
        list(targets = 3, width = '120px'),
        list(targets = 4, width = '90px'),
        list(targets = 5, width = '70px', className = 'dt-center')
      )
    ), callback = JS(
      "table.on('click', 'a.insp-mytask-link', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        Shiny.setInputValue('insp_mytask_view', id, {priority: 'event'});
      });"
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # ========================================
  # 巡检记录表格渲染
  # ========================================
  output$insp_record_table <- renderDT({
    req(rv$logged_in)
    rv$inspection_refresh_trigger
    
    records <- inspection_record_get_all(status_filter = input$insp_record_status_filter)
    
    if (nrow(records) > 0) {
      # 结果类型标签
      records$result_label <- sapply(records$result_type, function(r) {
        color <- ifelse(r == "normal", "#5cb85c", "#d9534f")
        label <- ifelse(r == "normal", "正常", "异常")
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      # 任务状态标签
      records$task_status_label <- sapply(records$task_status, function(s) {
        color <- switch(s, "pending" = "#f0ad4e", "processing" = "#5bc0de", 
                       "completed" = "#5cb85c", "abnormal" = "#d9534f", "#999")
        label <- switch(s, "pending" = "待执行", "processing" = "执行中", 
                       "completed" = "已完成", "abnormal" = "异常", s)
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      display_data <- data.frame(
        任务编号 = ifelse(is.na(records$task_no), "—", records$task_no),
        检查项 = ifelse(is.na(records$item_names_display), "—", records$item_names_display),
        计划 = ifelse(is.na(records$plan_name), "—", records$plan_name),
        检查人 = ifelse(is.na(records$inspector_name), "—", records$inspector_name),
        结果 = records$result_label,
        任务状态 = records$task_status_label,
        评分 = ifelse(is.na(records$score), "—", as.character(records$score)),
        提交时间 = ifelse(is.na(records$created_at), "—", substr(records$created_at, 1, 16))
      )
    } else {
      display_data <- data.frame(
        任务编号 = character(), 检查项 = character(), 计划 = character(),
        检查人 = character(), 结果 = character(), 任务状态 = character(),
        评分 = character(), 提交时间 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 20, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '130px'),
        list(targets = 1, width = '200px'),
        list(targets = 2, width = '120px'),
        list(targets = 3, width = '80px'),
        list(targets = 4, width = '60px', className = 'dt-center'),
        list(targets = 5, width = '70px', className = 'dt-center'),
        list(targets = 6, width = '50px', className = 'dt-center'),
        list(targets = 7, width = '130px')
      )
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # ========================================
  # 异常转工单（按任务批量）
  # ========================================
  observeEvent(input$create_issue_work_order, {
    req(rv$logged_in)
    req(input$create_issue_work_order)
    
    task_id <- as.integer(input$create_issue_work_order)
    
    result <- inspection_task_create_work_order(task_id, rv$current_user)
    
    if (result$success) {
      rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
      # 刷新工单列表
      if (!is.null(rv$work_order_refresh_trigger)) {
        rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      }
    }
    
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  
  # ========================================
  # 巡检异常列表渲染（按任务分组）
  # ========================================
  output$insp_issue_table <- renderDT({
    req(rv$logged_in)
    rv$inspection_refresh_trigger
    
    # 使用按任务分组的异常列表
    grouped_issues <- inspection_issue_get_grouped(input$insp_issue_status_filter)
    
    if (nrow(grouped_issues) > 0) {
      # 状态标签
      grouped_issues$status_label <- sapply(grouped_issues$status, function(s) {
        color <- switch(s, "pending" = "#f0ad4e", "processing" = "#5bc0de", 
                       "resolved" = "#5cb85c", "closed" = "#999", "#999")
        label <- switch(s, "pending" = "待处理", "processing" = "处理中", 
                       "resolved" = "已解决", "closed" = "已关闭", s)
        sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%s</span>', color, label)
      })
      
      # 工单链接
      grouped_issues$wo_link <- sapply(seq_len(nrow(grouped_issues)), function(i) {
        wo_id <- grouped_issues$work_order_id[i]
        if (!is.na(wo_id) && !is.null(wo_id)) {
          wo_no <- grouped_issues$work_order_no[i]
          sprintf('<a href="#" onclick="Shiny.setInputValue(\'work_order_view_click\', %d, {priority: \'event\'});" style="color:#337ab7;">%s</a>', wo_id, wo_no)
        } else "—"
      })
      
      # 转工单按钮
      grouped_issues$action_btn <- sapply(seq_len(nrow(grouped_issues)), function(i) {
        task_id <- grouped_issues$task_id[i]
        wo_id <- grouped_issues$work_order_id[i]
        if (is.na(wo_id) || is.null(wo_id)) {
          sprintf('<button class="btn btn-xs btn-success" onclick="Shiny.setInputValue(\'create_issue_work_order\', %d, {priority: \'event\'});">转工单</button>', task_id)
        } else {
          '<span style="color:#5cb85c;">已转</span>'
        }
      })
      
      display_data <- data.frame(
        任务编号 = sprintf('<a href="#" class="insp-issue-task-link" data-id="%d" style="font-weight:bold;color:#337ab7;">%s</a>',
                          grouped_issues$task_id, grouped_issues$task_no),
        计划 = ifelse(is.na(grouped_issues$plan_name), "—", grouped_issues$plan_name),
        检查项 = ifelse(is.na(grouped_issues$item_names_display), "—", grouped_issues$item_names_display),
        异常数 = sprintf('<span style="background:#d9534f;color:white;padding:2px 8px;border-radius:10px;font-size:11px;">%d项</span>', grouped_issues$issue_count),
        严重程度 = ifelse(is.na(grouped_issues$severity_summary), "—", grouped_issues$severity_summary),
        状态 = grouped_issues$status_label,
        关联工单 = grouped_issues$wo_link,
        操作 = grouped_issues$action_btn,
        发现时间 = ifelse(is.na(grouped_issues$created_at), "—", grouped_issues$created_at)
      )
    } else {
      display_data <- data.frame(
        任务编号 = character(), 计划 = character(), 检查项 = character(),
        异常数 = character(), 严重程度 = character(), 状态 = character(),
        关联工单 = character(), 操作 = character(), 发现时间 = character()
      )
    }
    
    DT::datatable(display_data, escape = FALSE, options = list(
      pageLength = 20, paging = TRUE, searching = FALSE, ordering = TRUE,
      info = FALSE, lengthChange = FALSE, dom = 't<"float-left"p>',
      columnDefs = list(
        list(targets = 0, width = '120px'),
        list(targets = 1, width = '100px'),
        list(targets = 2, width = '150px'),
        list(targets = 3, width = '60px', className = 'dt-center'),
        list(targets = 4, width = '80px'),
        list(targets = 5, width = '80px', className = 'dt-center'),
        list(targets = 6, width = '100px'),
        list(targets = 7, width = '70px', className = 'dt-center'),
        list(targets = 8, width = '130px')
      )
    ), callback = JS(
      "table.on('click', 'a.insp-issue-task-link', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        Shiny.setInputValue('insp_task_view', id, {priority: 'event'});
      });"
    ), rownames = FALSE, selection = 'single', class = 'cell-border stripe hover')
  })
  
  # ========================================
  # 加载下拉选项
  # ========================================
  observe({
    req(rv$logged_in)
    
    # 计划状态下拉
    plan_status_choices <- c("全部" = "all", "草稿" = "draft", "进行中" = "active", 
                            "已暂停" = "paused", "已完成" = "completed")
    updateSelectInput(session, "insp_plan_status_filter", choices = plan_status_choices, selected = "all")
    
    # 任务状态下拉
    task_status_choices <- c("全部" = "all", "待执行" = "pending", "执行中" = "processing",
                             "已完成" = "completed", "异常" = "abnormal")
    updateSelectInput(session, "insp_task_status_filter", choices = task_status_choices, selected = "all")
    updateSelectInput(session, "insp_my_status_filter", choices = task_status_choices, selected = "pending")
    
    # 异常状态下拉
    issue_status_choices <- c("全部" = "all", "待处理" = "pending", "处理中" = "processing",
                              "已解决" = "resolved", "已关闭" = "closed")
    updateSelectInput(session, "insp_issue_status_filter", choices = issue_status_choices, selected = "all")
    
    # 检查人员下拉
    inspectors <- inspection_get_inspectors()
    if (nrow(inspectors) > 0) {
      choices <- setNames(inspectors$id, sprintf("%s (%s)", inspectors$username, inspectors$role))
      updateSelectInput(session, "insp_task_inspector", choices = choices)
    }
  })
  
  # ========================================
  # 创建巡检计划弹窗
  # ========================================
  observeEvent(input$insp_create_plan, {
    req(rv$logged_in)
    
    # 获取负责人选项
    responsibles <- inspection_get_responsibles()
    resp_choices <- c("请选择" = "")
    if (nrow(responsibles) > 0) {
      resp_choices <- c(resp_choices, setNames(responsibles$id, sprintf("%s (%s)", responsibles$username, responsibles$role)))
    }
    
    # 获取巡检项分类
    categories <- inspection_category_get_all()
    cat_choices <- c("请选择" = "")
    if (nrow(categories) > 0) {
      cat_choices <- c(cat_choices, setNames(categories$category, categories$category))
    }
    
    showModal(modalDialog(
      title = "创建巡检计划",
      wellPanel(
        h4("基本信息"),
        fluidRow(
          column(6, selectInput("insp_plan_inspection_category", "巡检项（一级）", 
                               choices = cat_choices, selected = "")),
          column(6, selectInput("insp_plan_cycle", "执行周期",
                               choices = c("请选择" = "", "每天" = "daily", "每周" = "weekly", 
                                          "每月" = "monthly", "每季度" = "quarterly", "一次性" = "once")))
        ),
        fluidRow(
          column(12, div(id = "insp_auto_name_display",
            p(em("计划名称将在选择检查项后自动生成"), style = "color: #666; padding: 8px; background: #f5f5f5; border-radius: 4px;")
          ))
        ),
        fluidRow(
          column(6, dateInput("insp_plan_start", "开始日期", value = Sys.Date(), format = "yyyy-mm-dd")),
          column(6, dateInput("insp_plan_end", "结束日期", value = Sys.Date() + 30, format = "yyyy-mm-dd"))
        ),
        selectInput("insp_plan_responsible", "被检查负责人", choices = resp_choices),
        hr(),
        h4("选择检查项（二级）"),
        p("请先选择上方【巡检项】，然后勾选要包含的检查项"),
        div(id = "insp_items_container",
          fluidRow(
            column(12, 
              selectInput("insp_plan_category", "巡检分类", 
                         choices = c("请选择" = "", "日常巡检" = "日常巡检", "定期巡检" = "定期巡检",
                                    "专项巡检" = "专项巡检", "节前巡检" = "节前巡检", "故障巡检" = "故障巡检"))
            )
          ),
          fluidRow(
            column(12, 
              div(id = "insp_checklist_placeholder", 
                p(em("请先选择巡检项，系统将显示对应的检查项..."), style = "color: #999; padding: 10px;")
              )
            )
          )
        )
      ),
      footer = tagList(
        actionButton("insp_confirm_create_plan", "创建计划", class = "btn-primary"),
        modalButton("取消")
      ),
      easyClose = FALSE, size = "l"
    ))
  })
  
  # 监听巡检项选择变化，动态加载检查项
  observeEvent(input$insp_plan_inspection_category, {
    req(input$insp_plan_inspection_category)
    
    category <- input$insp_plan_inspection_category
    if (is.null(category) || category == "") {
      return()
    }
    
    # 先移除旧的检查项UI（防止重复添加）
    removeUI(selector = "#insp_checklist_wrapper", multiple = TRUE)
    
    # 延迟一点再插入新内容，避免UI操作冲突
    Sys.sleep(0.1)
    
    # 获取该分类下的检查项模板
    templates <- inspection_template_get_by_category(category)
    
    if (nrow(templates) > 0) {
      # 构建复选框列表
      checkbox_items <- lapply(1:nrow(templates), function(i) {
        t <- templates[i, ]
        tagList(
          fluidRow(
            column(1, checkboxInput(paste0("insp_item_", t$id), label = "", value = TRUE)),
            column(11, 
              div(style = "margin-bottom: 8px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
                strong(t$item_name), br(),
                span(style = "color: #666; font-size: 12px;", 
                     sprintf("标准: %s", ifelse(is.na(t$check_standard), "-", t$check_standard)))
              )
            )
          )
        )
      })
      
      insertUI(
        selector = "#insp_items_container",
        where = "beforeEnd",
        ui = tagList(
          div(id = "insp_checklist_wrapper",
            wellPanel(
              p(strong(sprintf("「%s」检查项（共%d项）:", category, nrow(templates)))),
              p(em("勾选要包含的检查项（默认全选）"), style = "color: #666;"),
              checkboxGroupInput("insp_selected_items", "选择检查项",
                                choices = setNames(templates$id, templates$item_name),
                                selected = templates$id),
              div(id = "insp_items_detail",
                lapply(1:nrow(templates), function(i) {
                  t <- templates[i, ]
                  div(id = paste0("insp_item_detail_", t$id),
                    p(strong(t$item_name), style = "margin-top: 10px;"),
                    p(sprintf("检查标准: %s", ifelse(is.na(t$check_standard), "-", t$check_standard)), style = "color: #666;")
                  )
                })
              )
            )
          )
        )
      )
    } else {
      insertUI(
        selector = "#insp_items_container",
        where = "beforeEnd",
        ui = tagList(
          div(id = "insp_checklist_wrapper",
            p(em("该巡检项暂无预设检查项，请手动添加。"), style = "color: #999; padding: 10px;")
          )
        )
      )
    }
  })
  
  # 确认创建巡检计划
  observeEvent(input$insp_confirm_create_plan, {
    req(rv$logged_in)
    req(input$insp_plan_inspection_category, input$insp_plan_cycle)
    
    # 获取选中的检查项模板
    selected_ids <- input$insp_selected_items
    check_items <- list()
    item_names <- c()
    
    if (!is.null(selected_ids) && length(selected_ids) > 0) {
      # 从模板获取选中的检查项
      templates <- inspection_template_get_by_category(input$insp_plan_inspection_category)
      for (id in selected_ids) {
        idx <- which(templates$id == as.integer(id))
        if (length(idx) > 0) {
          t <- templates[idx[1], ]
          item <- list(
            category = input$insp_plan_inspection_category,
            name = t$item_name,
            description = ifelse(is.na(t$item_description), "", t$item_description),
            standard = ifelse(is.na(t$check_standard), "", t$check_standard),
            max_score = ifelse(is.na(t$max_score), 100, as.integer(t$max_score)),
            scoring_type = ifelse(is.na(t$scoring_type), "pass_fail", t$scoring_type)
          )
          check_items <- c(check_items, list(item))
          item_names <- c(item_names, t$item_name)
        }
      }
    }
    
    if (length(check_items) == 0) {
      showNotification("请至少选择一个检查项", type = "warning")
      return()
    }
    
    # 自动生成计划名称：从检查项提取关键字
    plan_name <- generate_plan_name_from_items(input$insp_plan_inspection_category, item_names)
    
    result <- inspection_plan_add(
      name = plan_name,
      description = "",  # 不再使用计划描述
      category = input$insp_plan_category %||% "日常巡检",
      inspection_category = input$insp_plan_inspection_category,
      cycle_type = input$insp_plan_cycle,
      cycle_value = "",
      start_date = format(input$insp_plan_start, "%Y-%m-%d"),
      end_date = format(input$insp_plan_end, "%Y-%m-%d"),
      responsible_user = input$insp_plan_responsible,
      check_items = check_items,
      current_user = rv$current_user
    )
    
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
    }
  })
  
  # ========================================
  # 生成巡检任务
  # ========================================
  observeEvent(input$insp_generate_tasks, {
    req(rv$logged_in)
    req(input$insp_task_inspector, input$insp_task_date)
    
    # 检查是否有选中的计划
    if (is.null(rv$inspection_selected_plan_id)) {
      showNotification("请先选择一个巡检计划", type = "warning")
      return()
    }
    
    result <- inspection_task_generate_from_plan(
      plan_id = rv$inspection_selected_plan_id,
      scheduled_date = format(input$insp_task_date, "%Y-%m-%d"),
      inspector = input$insp_task_inspector,
      current_user = rv$current_user
    )
    
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
    }
  })
  
  # ========================================
  # 查看/执行巡检任务
  # ========================================
  observeEvent(input$insp_mytask_view, {
    req(rv$logged_in, input$insp_mytask_view)
    
    task_id <- input$insp_mytask_view
    task <- inspection_task_get_by_id(task_id)
    
    if (nrow(task) > 0) {
      rv$inspection_selected_task_id <- task_id
      t <- task[1, ]
      
      # 任务状态
      status_cn <- switch(t$status, "pending" = "待执行", "processing" = "执行中",
                         "completed" = "已完成", "abnormal" = "存在异常", t$status)
      status_color <- switch(t$status, "pending" = "#f0ad4e", "processing" = "#5bc0de",
                            "completed" = "#5cb85c", "abnormal" = "#d9534f", "#999")
      
      # 获取检查项列表
      items <- t$original_items[[1]]
      if (is.null(items) || nrow(items) == 0) {
        # 兼容旧数据：使用 item_name 等字段
        items <- data.frame(
          item_name = ifelse(is.na(t$item_name), "未知检查项", t$item_name),
          check_standard = ifelse(is.na(t$check_standard), "", t$check_standard),
          stringsAsFactors = FALSE
        )
      }
      
      # 生成检查项HTML列表
      items_html <- ""
      for (i in 1:nrow(items)) {
        item <- items[i, ]
        item_name <- as.character(item$item_name)
        check_std <- as.character(item$check_standard)
        check_std <- ifelse(is.na(check_std) || check_std == "", "暂无检查标准", check_std)
        items_html <- paste0(items_html, sprintf('
          <div style="background:#f9f9f9;padding:10px;margin-bottom:8px;border-radius:4px;border-left:3px solid #337ab7;">
            <div style="font-weight:bold;color:#333;">【%d】%s</div>
            <div style="color:#666;font-size:12px;margin-top:4px;">标准：%s</div>
          </div>', i, item_name, check_std))
      }
      
      # 已完成时显示历史记录
      history_html <- ""
      if (t$status %in% c("completed", "abnormal")) {
        records <- inspection_record_get_by_task(task_id)
        if (nrow(records) > 0) {
          r <- records[1, ]
          result_cn <- switch(r$result_type, "normal" = "正常", "abnormal" = "存在异常", r$result_type)
          result_color <- switch(r$result_type, "normal" = "#5cb85c", "abnormal" = "#d9534f", "#999")
          history_html <- sprintf('
            <div style="margin-top:15px;padding:12px;background:#e8f5e9;border-radius:6px;">
              <strong>已完成记录：</strong><br>
              <span style="background:%s;color:white;padding:2px 8px;border-radius:4px;margin-right:10px;">%s</span>
              <strong>评分：</strong>%s | <strong>时间：</strong>%s<br>
              <div style="margin-top:8px;color:#333;">%s</div>
            </div>', result_color, result_cn, r$score, substr(r$created_at, 1, 19),
            ifelse(is.na(r$remark) || r$remark == "", "无备注", r$remark))
        }
      }
      
      showModal(modalDialog(
        title = sprintf("巡检任务 - %s", t$task_no),
        HTML(sprintf('
          <div style="padding:10px;max-height:70vh;overflow-y:auto;">
            <div style="background:#e3f2fd;padding:12px;border-radius:6px;margin-bottom:15px;">
              <table style="width:100%%;font-size:14px;">
                <tr><td style="width:90px;color:#666;">巡检计划：</td><td><strong>%s</strong></td></tr>
                <tr><td style="color:#666;">计划日期：</td><td>%s</td></tr>
                <tr><td style="color:#666;">巡检员：</td><td>%s</td></tr>
                <tr><td style="color:#666;">状态：</td><td><span style="background:%s;color:white;padding:2px 8px;border-radius:4px;">%s</span></td></tr>
              </table>
            </div>
            
            <h4 style="margin-bottom:10px;">检查项清单（共 %d 项）</h4>
            %s
            %s
          </div>
        ', ifelse(is.na(t$plan_name), "—", t$plan_name),
           ifelse(is.na(t$scheduled_date), "—", t$scheduled_date),
           ifelse(is.na(t$inspector_name), "—", t$inspector_name),
           status_color, status_cn,
           nrow(items), items_html, history_html)),
        footer = tagList(
          if (t$status %in% c("pending", "processing")) {
            actionButton("insp_execute_task", "执行巡检", class = "btn-primary btn-lg")
          },
          modalButton("关闭")
        ),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  
  # 执行巡检任务 - 显示执行表单（支持多检查项）
  observeEvent(input$insp_execute_task, {
    req(rv$logged_in, rv$inspection_selected_task_id)
    
    task <- inspection_task_get_by_id(rv$inspection_selected_task_id)
    if (nrow(task) == 0) return()
    t <- task[1, ]
    
    # 获取原始检查项数据
    items <- t$original_items[[1]]
    if (is.null(items) || nrow(items) == 0) {
      # 兼容旧数据：使用 item_name 等字段
      items <- data.frame(
        id = ifelse(is.na(t$item_id), 0, t$item_id),
        item_name = ifelse(is.na(t$item_name), "未知检查项", t$item_name),
        check_standard = ifelse(is.na(t$check_standard), "", t$check_standard),
        stringsAsFactors = FALSE
      )
    }
    
    # 生成动态检查项表单
    items_form <- lapply(1:nrow(items), function(i) {
      item <- items[i, ]
      item_id <- item$id
      item_name <- as.character(item$item_name)
      check_std <- as.character(ifelse(is.na(item$check_standard) || item$check_standard == "", "暂无检查标准", item$check_standard))
      
      wellPanel(
        h5(sprintf("【%d】%s", i, item_name)),
        p(strong("检查标准："), code(check_std), style = "background:#f9f9f9;padding:8px;border-radius:4px;"),
        hr(),
        fluidRow(
          column(6, selectInput(sprintf("insp_item_result_%d", item_id), "检查结果",
                     choices = c("正常" = "normal", "异常" = "abnormal", "不适用" = "na"),
                     selected = "normal")),
          column(6, numericInput(sprintf("insp_item_score_%d", item_id), "评分", 
                     value = 100, min = 0, max = 100, step = 1))
        ),
        textAreaInput(sprintf("insp_item_remark_%d", item_id), "备注", rows = 2, 
                    placeholder = "输入检查备注...")
      )
    })
    
    # 生成弹窗UI
    modal_ui <- tagList(
      wellPanel(
        div(style = "background:#e3f2fd;padding:12px;border-radius:6px;margin-bottom:15px;",
          h4(sprintf("巡检计划：%s", ifelse(is.na(t$plan_name), "—", t$plan_name)), style = "margin-top:0;"),
          p(sprintf("任务编号：%s | 计划日期：%s", t$task_no, ifelse(is.na(t$scheduled_date), "—", t$scheduled_date)))
        ),
        h4(sprintf("检查项清单（共 %d 项）", nrow(items))),
        p(em("请逐项完成检查，填写结果后提交"), style = "color:#666;")
      ),
      items_form,
      wellPanel(
        h5("总体评价"),
        fluidRow(
          column(6, numericInput("insp_total_score", "综合评分", value = 100, min = 0, max = 100, step = 1)),
          column(6, selectInput("insp_overall_result", "总体结果",
                     choices = c("全部正常" = "normal", "存在异常" = "abnormal"),
                     selected = "normal"))
        ),
        textAreaInput("insp_overall_remark", "巡检总结", rows = 3, 
                    placeholder = "简要描述本次巡检的总体情况...")
      )
    )
    
    # 存储检查项ID列表到响应式值
    rv$inspection_task_items <- items$id
    
    showModal(modalDialog(
      title = "执行巡检检查",
      modal_ui,
      footer = tagList(
        actionButton("insp_confirm_execute_all", "提交全部结果", class = "btn-success"),
        modalButton("取消")
      ),
      easyClose = FALSE, size = "l"
    ))
  })
  
  # 确认执行结果 - 批量提交所有检查项
  observeEvent(input$insp_confirm_execute_all, {
    req(rv$logged_in, rv$inspection_selected_task_id)
    req(rv$inspection_task_items)
    
    task_id <- rv$inspection_selected_task_id
    item_ids <- rv$inspection_task_items
    
    # 统计结果
    total_items <- length(item_ids)
    normal_count <- 0
    abnormal_count <- 0
    na_count <- 0
    all_results <- list()
    
    # 遍历每个检查项，收集结果
    for (item_id in item_ids) {
      result_type <- input[[sprintf("insp_item_result_%d", item_id)]]
      score <- input[[sprintf("insp_item_score_%d", item_id)]]
      remark <- input[[sprintf("insp_item_remark_%d", item_id)]]
      
      # 统计
      if (result_type == "normal") normal_count <- normal_count + 1
      else if (result_type == "abnormal") abnormal_count <- abnormal_count + 1
      else na_count <- na_count + 1
      
      all_results[[as.character(item_id)]] <- list(
        result_type = result_type,
        score = score,
        remark = remark
      )
    }
    
    # 提交总体记录（使用JSON存储所有检查项结果）
    results_json <- jsonlite::toJSON(all_results)
    overall_remark <- input$insp_overall_remark
    overall_result <- if (abnormal_count > 0) "abnormal" else "normal"
    total_score <- input$insp_total_score
    
    result <- inspection_record_add_batch(
      task_id = task_id,
      results_json = results_json,
      overall_result = overall_result,
      total_score = total_score,
      overall_remark = overall_remark,
      current_user = rv$current_user
    )
    
    if (result$success) {
      # 如果有异常，创建异常记录
      if (abnormal_count > 0) {
        abnormal_items <- names(which(sapply(all_results, function(r) r$result_type == "abnormal")))
        for (ab_item_id in abnormal_items) {
          item_remark <- all_results[[ab_item_id]]$remark
          inspection_issue_add(
            record_id = result$record_id,
            task_id = task_id,
            issue_type = "巡检异常",
            issue_description = sprintf("检查项 [%s] 异常：%s", ab_item_id, item_remark),
            severity = "medium",
            photos_json = NULL,
            current_user = rv$current_user
          )
        }
      }
      
      showNotification(sprintf("巡检完成！正常: %d, 异常: %d, 不适用: %d", 
                              normal_count, abnormal_count, na_count), 
                      type = ifelse(abnormal_count > 0, "warning", "message"))
    } else {
      showNotification(result$message, type = "error")
    }
    
    removeModal()
    rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
    rv$inspection_selected_task_id <- NULL
    rv$inspection_task_items <- NULL
  })
  
  # 上报异常
  observeEvent(input$insp_report_issue, {
    req(rv$logged_in, rv$inspection_selected_task_id)
    
    task <- inspection_task_get_by_id(rv$inspection_selected_task_id)
    if (nrow(task) == 0) return()
    t <- task[1, ]
    
    showModal(modalDialog(
      title = "上报巡检异常",
      wellPanel(
        h4(sprintf("检查项：%s", t$item_name)),
        selectInput("insp_issue_type", "问题类型",
                   choices = c("设备故障" = "设备故障", "安全隐患" = "安全隐患", 
                              "卫生问题" = "卫生问题", "其他问题" = "其他问题")),
        selectInput("insp_issue_severity", "严重程度",
                   choices = c("低" = "low", "中" = "medium", "高" = "high"),
                   selected = "medium"),
        textAreaInput("insp_issue_desc", "问题描述", rows = 3, placeholder = "详细描述发现的问题..."),
        textInput("insp_issue_photo", "问题照片URL", placeholder = "可上传或拍照后粘贴URL"),
        hr(),
        p(strong("是否同步创建整改工单？"), style = "color:#d9534f;")
      ),
      footer = tagList(
        actionButton("insp_confirm_issue", "提交异常", class = "btn-danger"),
        modalButton("取消")
      ),
      easyClose = FALSE, size = "m"
    ))
  })
  
  # 确认异常提交
  observeEvent(input$insp_confirm_issue, {
    req(rv$logged_in, rv$inspection_selected_task_id)
    req(input$insp_issue_type, input$insp_issue_desc)
    
    # 先提交异常记录
    issue_result <- inspection_issue_add(
      record_id = NULL,
      task_id = rv$inspection_selected_task_id,
      issue_type = input$insp_issue_type,
      issue_description = input$insp_issue_desc,
      severity = input$insp_issue_severity,
      photos_json = ifelse(input$insp_issue_photo == "", NA, input$insp_issue_photo),
      current_user = rv$current_user
    )
    
    if (issue_result$success) {
      issue_id <- issue_result$issue_id
      
      # 自动创建整改工单
      wo_result <- inspection_issue_create_work_order(issue_id, rv$current_user)
      
      removeModal()
      rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
      rv$inspection_selected_task_id <- NULL
      
      showNotification(wo_result$message, type = ifelse(wo_result$success, "message", "error"))
      
      # 如果工单创建成功，刷新工单列表
      if (wo_result$success) {
        rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      }
    } else {
      showNotification(issue_result$message, type = "error")
    }
  })
  
  # ========================================
  # 查看/管理巡检计划详情
  # ========================================
  observeEvent(input$insp_plan_view, {
    req(rv$logged_in, input$insp_plan_view)
    
    plan_id <- input$insp_plan_view
    plan <- inspection_plan_get_by_id(plan_id)
    
    if (nrow(plan) > 0) {
      rv$inspection_selected_plan_id <- plan_id
      p <- plan[1, ]
      
      # 检查是否为Admin
      is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
      
      # 获取检查项
      items <- inspection_item_get_by_plan(plan_id)
      items_html <- ""
      if (nrow(items) > 0) {
        items_html <- "<ul>"
        for (i in 1:nrow(items)) {
          items_html <- paste0(items_html, sprintf("<li>%s (满分%d分)</li>", 
                                                    items$item_name[i], items$max_score[i]))
        }
        items_html <- paste0(items_html, "</ul>")
      } else {
        items_html <- "<span style='color:#999;'>暂无检查项</span>"
      }
      
      status_cn <- switch(p$status, "draft" = "草稿", "active" = "进行中",
                         "paused" = "已暂停", "completed" = "已完成", p$status)
      status_color <- switch(p$status, "draft" = "#999", "active" = "#5cb85c",
                            "paused" = "#f0ad4e", "completed" = "#337ab7", "#999")
      
      # 检查是否有待执行任务
      tasks <- inspection_task_get_all(plan_filter = plan_id)
      pending_count <- sum(tasks$status == "pending", na.rm = TRUE)
      
      # 获取历史评论
      comments <- inspection_plan_get_comments(plan_id)
      
      # 构建评论 HTML（包含添加评论输入框）
      comments_html <- "<div style='margin-top: 15px;'><div style='font-weight: bold; color: #333; margin-bottom: 10px; font-size: 15px;'>💬 评论</div>"
      # 添加评论输入区域
      comments_html <- paste0(comments_html, '
        <div style="background: #fff; padding: 12px; border-radius: 6px; border: 1px solid #ddd; margin-bottom: 12px;">
          <textarea id="inspection_plan_comment_input" placeholder="输入评论内容..." style="width: 100%; min-height: 60px; padding: 8px; border: 1px solid #ddd; border-radius: 4px; font-size: 13px; resize: vertical;"></textarea>
          <button class="btn btn-primary btn-sm" style="margin-top: 8px;" onclick="Shiny.setInputValue(\'add_inspection_plan_comment\', $(this).closest(\'.modal\').find(\'textarea\').val(), {priority: \'event\'});">添加评论</button>
        </div>
      ')
      # 历史评论
      if (nrow(comments) > 0) {
        for (i in 1:nrow(comments)) {
          creator <- ifelse(is.na(comments$creator_name[i]), "未知用户", comments$creator_name[i])
          comment_text <- ifelse(is.na(comments$comment[i]), "", comments$comment[i])
          created_at <- ifelse(is.na(comments$created_at[i]), "", comments$created_at[i])
          comments_html <- paste0(comments_html, sprintf('
            <div style="background: #fff8e7; padding: 10px 12px; margin-bottom: 8px; border-radius: 6px; border-left: 4px solid #f0ad4e;">
              <div style="font-size: 12px; color: #666; margin-bottom: 6px;">
                <span style="font-weight: bold; color: #337ab7;">%s</span>
                <span style="margin-left: 10px;">%s</span>
              </div>
              <div style="font-size: 13px; line-height: 1.6; white-space: pre-wrap;">%s</div>
            </div>', creator, created_at, comment_text))
        }
      } else {
        comments_html <- paste0(comments_html, "<div style='color: #999; font-style: italic; padding: 10px;'>暂无评论</div>")
      }
      comments_html <- paste0(comments_html, "</div>")
      
      showModal(modalDialog(
        title = sprintf("巡检计划详情 - %s", p$plan_no),
        HTML(sprintf('
          <div style="padding:10px;max-height:70vh;overflow-y:auto;">
            <div style="background:#f5f5f5;padding:12px;border-radius:6px;margin-bottom:15px;">
              <table style="width:100%%;font-size:14px;">
                <tr><td style="width:100px;color:#666;">计划名称：</td><td><strong>%s</strong></td></tr>
                <tr><td style="color:#666;">计划描述：</td><td>%s</td></tr>
                <tr><td style="color:#666;">巡检分类：</td><td>%s</td></tr>
                <tr><td style="color:#666;">执行周期：</td><td>%s</td></tr>
                <tr><td style="color:#666;">有效期：</td><td>%s 至 %s</td></tr>
                <tr><td style="color:#666;">被检查负责人：</td><td>%s</td></tr>
                <tr><td style="color:#666;">创建人：</td><td>%s</td></tr>
                <tr><td style="color:#666;">状态：</td><td><span style="background:%s;color:white;padding:2px 8px;border-radius:4px;">%s</span></td></tr>
                <tr><td style="color:#666;">创建时间：</td><td>%s</td></tr>
                <tr><td style="color:#666;">更新时间：</td><td>%s</td></tr>
              </table>
            </div>
            
            <div style="margin-bottom:15px;">
              <h4>检查项列表</h4>
              %s
            </div>
            
            <div style="background:#fff3e0;padding:12px;border-radius:6px;margin-bottom:15px;">
              <strong>当前任务状态：</strong>待执行 %d 个任务
            </div>
            
            %s
          </div>
        ', p$name, ifelse(is.na(p$description), "—", p$description),
           ifelse(is.na(p$category), "—", p$category),
           ifelse(is.na(p$cycle_type), "—", p$cycle_type),
           ifelse(is.na(p$start_date), "—", p$start_date),
           ifelse(is.na(p$end_date), "—", p$end_date),
           ifelse(is.na(p$responsible_name), "—", p$responsible_name),
           ifelse(is.na(p$creator_name), "—", p$creator_name),
           status_color, status_cn,
           ifelse(is.na(p$created_at), "—", p$created_at),
           ifelse(is.na(p$updated_at), "—", p$updated_at),
           items_html, pending_count, comments_html)),
        footer = tagList(
          if (is_admin) {
            tagList(
              actionButton("insp_delete_plan_btn", "删除", class = "btn-danger", style = "margin-right:5px;"),
              actionButton("insp_edit_plan_btn", "修改计划", class = "btn-warning", style = "margin-right:5px;")
            )
          },
          actionButton("insp_generate_task_btn", "生成任务", class = "btn-primary"),
          modalButton("关闭")
        ),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  
  # 处理添加巡检计划评论
  observeEvent(input$add_inspection_plan_comment, {
    req(rv$logged_in)
    req(rv$inspection_selected_plan_id)
    comment_text <- input$add_inspection_plan_comment
    req(comment_text)
    comment_text <- trimws(comment_text)
    req(nchar(comment_text) > 0)
    
    result <- inspection_plan_add_comment(rv$inspection_selected_plan_id, comment_text, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      # 重新加载计划详情以刷新评论显示
      rv$inspection_comment_refresh <- ifelse(is.null(rv$inspection_comment_refresh), 0, rv$inspection_comment_refresh + 1)
    }
  })
  
  # 处理修改巡检计划按钮（Admin专属）
  observeEvent(input$insp_edit_plan_btn, {
    req(rv$logged_in, rv$inspection_selected_plan_id)
    
    # 检查是否为Admin
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    if (!is_admin) {
      showNotification("只有Admin才能修改巡检计划", type = "error")
      return()
    }
    
    plan_id <- rv$inspection_selected_plan_id
    plan <- inspection_plan_get_by_id(plan_id)
    if (nrow(plan) == 0) return()
    p <- plan[1, ]
    
    # 获取负责人选项
    responsibles <- inspection_get_responsibles()
    resp_choices <- c("未指定" = "")
    if (nrow(responsibles) > 0) {
      resp_choices <- c(resp_choices, setNames(responsibles$id, sprintf("%s (%s)", responsibles$username, responsibles$role)))
    }
    
    # 获取状态选项
    status_choices <- c("草稿" = "draft", "进行中" = "active", "已暂停" = "paused", "已完成" = "completed")
    
    # 获取巡检项分类
    categories <- inspection_category_get_all()
    cat_choices <- c("请选择" = "")
    if (nrow(categories) > 0) {
      cat_choices <- c(cat_choices, setNames(categories$category, categories$category))
    }
    
    # 获取分类选项
    category_choices <- c("日常巡检" = "日常巡检", "定期巡检" = "定期巡检", "专项巡检" = "专项巡检", "节前巡检" = "节前巡检", "故障巡检" = "故障巡检")
    
    # 关闭详情弹窗
    removeModal()
    
    # 显示修改弹窗
    showModal(modalDialog(
      title = sprintf("修改巡检计划 - %s", p$plan_no),
      wellPanel(
        h4("基本信息"),
        textInput("insp_edit_plan_no", "计划编号", value = ifelse(is.na(p$plan_no), "", p$plan_no)),
        textInput("insp_edit_plan_name", "计划名称", value = p$name),
        textAreaInput("insp_edit_plan_desc", "计划描述", rows = 3, value = ifelse(is.na(p$description), "", p$description)),
        fluidRow(
          column(6, selectInput("insp_edit_plan_inspection_category", "巡检项（一级）", 
                               choices = cat_choices, selected = ifelse(is.na(p$inspection_category), "", p$inspection_category))),
          column(6, selectInput("insp_edit_plan_category", "巡检分类", 
                               choices = category_choices, selected = ifelse(is.na(p$category), "日常巡检", p$category)))
        ),
        fluidRow(
          column(6, selectInput("insp_edit_plan_cycle", "执行周期",
                               choices = c("每天" = "daily", "每周" = "weekly", 
                                          "每月" = "monthly", "每季度" = "quarterly", "一次性" = "once"),
                               selected = ifelse(is.na(p$cycle_type), "once", p$cycle_type))),
          column(6, selectInput("insp_edit_plan_status", "状态",
                               choices = status_choices, selected = p$status))
        ),
        fluidRow(
          column(6, dateInput("insp_edit_plan_start", "开始日期", value = ifelse(is.na(p$start_date), Sys.Date(), as.Date(p$start_date)), format = "yyyy-mm-dd")),
          column(6, dateInput("insp_edit_plan_end", "结束日期", value = ifelse(is.na(p$end_date), Sys.Date() + 30, as.Date(p$end_date)), format = "yyyy-mm-dd"))
        ),
        selectInput("insp_edit_plan_responsible", "被检查负责人", choices = resp_choices,
                   selected = ifelse(is.na(p$responsible_user), "", p$responsible_user))
      ),
      footer = tagList(
        actionButton("insp_confirm_edit_plan", "保存修改", class = "btn-warning"),
        modalButton("取消")
      ),
      easyClose = FALSE, size = "l"
    ))
  })
  
  # 确认修改巡检计划
  observeEvent(input$insp_confirm_edit_plan, {
    req(rv$logged_in, rv$inspection_selected_plan_id)
    req(input$insp_edit_plan_name)
    
    # 获取原始计划信息
    original_plan <- inspection_plan_get_by_id(rv$inspection_selected_plan_id)
    if (nrow(original_plan) == 0) {
      showNotification("计划不存在", type = "error")
      return()
    }
    p <- original_plan[1, ]
    
    # 构建修改后的值（保留原始值如果为空）
    plan_name <- ifelse(input$insp_edit_plan_name == "", p$name, input$insp_edit_plan_name)
    plan_desc <- ifelse(input$insp_edit_plan_desc == "", p$description, input$insp_edit_plan_desc)
    inspection_category <- ifelse(is.null(input$insp_edit_plan_inspection_category) || input$insp_edit_plan_inspection_category == "", 
                                   p$inspection_category, input$insp_edit_plan_inspection_category)
    category <- ifelse(is.null(input$insp_edit_plan_category) || input$insp_edit_plan_category == "", 
                       p$category, input$insp_edit_plan_category)
    cycle_type <- ifelse(is.null(input$insp_edit_plan_cycle) || input$insp_edit_plan_cycle == "", 
                        p$cycle_type, input$insp_edit_plan_cycle)
    status <- ifelse(is.null(input$insp_edit_plan_status) || input$insp_edit_plan_status == "", 
                     p$status, input$insp_edit_plan_status)
    start_date <- ifelse(is.null(input$insp_edit_plan_start) || input$insp_edit_plan_start == "", 
                        p$start_date, format(input$insp_edit_plan_start, "%Y-%m-%d"))
    end_date <- ifelse(is.null(input$insp_edit_plan_end) || input$insp_edit_plan_end == "", 
                      p$end_date, format(input$insp_edit_plan_end, "%Y-%m-%d"))
    
    result <- inspection_plan_update(
      id = rv$inspection_selected_plan_id,
      name = plan_name,
      description = plan_desc,
      category = category,
      inspection_category = inspection_category,
      cycle_type = cycle_type,
      start_date = start_date,
      end_date = end_date,
      responsible_user = input$insp_edit_plan_responsible,
      status = status,
      current_user = rv$current_user
    )
    
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    if (result$success) {
      removeModal()
      rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
    }
  })
  
  # 处理删除巡检计划（Admin专属）
  observeEvent(input$insp_delete_plan_btn, {
    req(rv$logged_in, rv$inspection_selected_plan_id)

    # 检查是否为Admin
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    if (!is_admin) {
      showNotification("只有Admin才能删除巡检计划", type = "error")
      return()
    }

    # 显示确认对话框
    showModal(modalDialog(
      title = "确认删除",
      p(strong("警告："), "删除巡检计划将同时删除其下的所有任务和记录！"),
      p("此操作不可恢复，是否继续？"),
      footer = tagList(
        actionButton("insp_confirm_delete_plan", "确认删除", class = "btn-danger"),
        modalButton("取消")
      ),
      easyClose = TRUE
    ))
  })

  # 确认删除巡检计划
  observeEvent(input$insp_confirm_delete_plan, {
    req(rv$logged_in, rv$inspection_selected_plan_id)

    result <- inspection_plan_delete(rv$inspection_selected_plan_id, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    if (result$success) {
      removeModal()
      rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
      rv$inspection_selected_plan_id <- NULL
    }
  })
  
  # 刷新按钮
  observeEvent(input$insp_refresh, {
    rv$inspection_refresh_trigger <- rv$inspection_refresh_trigger + 1
  })
}
