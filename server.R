# 加载功能模块脚本
# 这些脚本包含了系统的核心功能实现
source("Script/auth.r")           # 身份验证模块（登录/注销）
source("Script/data_management.r")  # 数据管理模块
source("Script/model_training.r")   # 模型训练模块
source("Script/visualization.r")    # 数据可视化模块
source("Script/user_management.r")   # 用户管理模块
source("Script/system_settings.r")  # 系统设置模块
source("Script/work_order.r")       # 工单管理模块
source("Script/project_management.r") # 项目管理模块（数据层）
source("Script/project_server.r")     # 项目管理模块（服务端逻辑）
source("Script/information_collector.r")  # 信息收集器模块
source("Script/inspection_patrol.r")    # 巡检管理模块（旧版兼容）
source("Script/inspection_management.r") # 巡检管理模块（数据层）
source("Script/inspection_server.r")    # 巡检管理模块（服务端）
source("Script/login_ui.r")         # 登录界面定义
source("Script/data_center_server.r")   # 数据中心模块（数据归集）
source("Script/integration_management.r") # 集成模块数据层
source("Script/integration_server.r")     # 集成模块服务端
source("Script/tools_server.r")         # 工具模块
source("Script/ai_management.r")       # AI 模块数据层
source("Script/ai_server.r")           # AI 模块
source("Script/process_engine.r")       # 流程引擎核心（定义 %||% 等工具函数，network_test.r 依赖）
source("Script/github_autosubmit.r") # GitHub自动提交功能
source("Script/std_computer.r")        # 标准化模块
source("Script/main_ui.r")          # 主界面定义
source("Script/process_server.r")       # 流程模块服务端
source("Script/sysmon_management.r")   # 性能监控数据层
source("Script/sysmon_server.r")       # 性能监控服务端
source("Script/solution_management.r") # 方案模块数据层
source("Script/solution_server.r")     # 方案模块服务端
source("Script/solution_exec.r")        # 方案执行模块数据层
source("Script/solution_exec_server.r") # 方案执行模块服务端
source("Script/performance_management.r") # 绩效数据层
source("Script/performance_server.r")   # 绩效模块服务端
source("Script/note_management.r")   # 记事模块数据层
source("Script/note_server.r")       # 记事模块服务端
source("Script/asset_management.r")  # 资产模块数据层
source("Script/asset_server.r")      # 资产模块服务端
source("Script/attendance_device.r")  # 考勤设备模块数据层
source("Script/seat_map_management.r")  # 工位图模块数据层
source("Script/seat_map_server.r")      # 工位图模块服务端
source("Script/duty_matrix_management.r") # 岗职模块数据层
source("Script/duty_matrix_server.r")     # 岗职模块服务端
source("Script/module_inventory.r")       # 模块清单（全站映射参考）
source("Script/system_architecture.r")    # 系统架构可视化
source("Script/monthly_carryover.r")      # 月度数据结转
source("Script/dev_log_management.r")     # 开发日志数据层
source("Script/dev_log_server.r")         # 开发日志模块
source("Script/meta_task_management.r")  # 元任务数据层
source("Script/meta_task_ui.r")          # 元任务UI
source("Script/meta_task_server.r")      # 元任务模块


# 注册静态资源路径（www 目录下的文件可通过 /www/ 访问）
addResourcePath("www", "www")

# 定义server函数
# 这是Shiny应用的服务器逻辑核心
# 参数说明：
# - input: 接收来自UI的用户输入
# - output: 向UI发送输出内容
# - session: 管理用户会话
server <- function(input, output, session) {

  # ====== 大屏数据代理 API（供独立大屏页调用） ======
  # 注册 HTTP GET /bigscreen_api 端点，代理请求远程 API 返回 JSON
  session$registerDataObj("bigscreen_api", data.frame(), function(data, req) {
    if (!requireNamespace("httr", quietly = TRUE)) {
      return(list(status = 500L, body = '{"error":"httr not installed"}', headers = list("Content-Type" = "application/json")))
    }
    resp <- tryCatch({
      httr::GET("https://lvcchong.com/factoryBi/charge/0/realTimeData",
        httr::add_headers("Accept" = "application/json"),
        httr::timeout(10))
    }, error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) {
      return(list(status = 502L, body = '{"error":"upstream error"}', headers = list("Content-Type" = "application/json")))
    }
    body <- httr::content(resp, "text", encoding = "UTF-8")
    return(list(status = 200L, body = body, headers = list(
      "Content-Type" = "application/json",
      "Access-Control-Allow-Origin" = "*"
    )))
  })

  # 初始化工单配置选项（确保如果已有 config_options 表但没有工单配置时能正确初始化）
  tryCatch({
    init_work_order_config_options()
  }, error = function(e) {
    warning("初始化工单配置选项失败: ", e$message)
  })
  
  # 创建响应式值对象，用于管理应用状态
  # reactiveValues是Shiny的核心功能，用于存储和管理响应式状态
  # 当这些值发生变化时，依赖它们的UI和计算会自动更新
  rv <- reactiveValues(
    logged_in = FALSE,  # 登录状态，默认为未登录
    current_user = NULL, # 当前用户信息，默认为空
    daily_report_refresh = 0,  # 日报刷新触发器
    home_dev_refresh = 0L  # 首页开发日志刷新
  )
  
  # 通用按钮状态控制
  toggle_btn <- function(id, enabled) {
    session$sendCustomMessage("toggleBtn", list(id = id, disabled = !isTRUE(enabled)))
  }
  btn_ok <- function(val) { !is.null(val) && length(val) > 0 && nchar(trimws(paste(val, collapse=""))) > 0 }

  # 可视化渲染器：词云→ggplot, 其他→plotly
  viz_render <- function(viz_type, viz_data) {
    if (viz_type == "词云图") {
      # 先渲染 ggplot
      output$viz_ggplot <- renderPlot({
        viz_generate(viz_type, viz_data)
      }, bg = "transparent")
      renderUI({ plotOutput("viz_ggplot", height = "500px") })
    } else {
      output$viz_plotly <- renderPlotly({
        viz_generate(viz_type, viz_data)
      })
      renderUI({ plotlyOutput("viz_plotly", height = "500px") })
    }
  }

  # OLD 架构：renderUI 直接返回 login_ui() 或 main_ui()
  output$app_ui <- renderUI({
    if (!rv$logged_in) {
      login_ui()
    } else {
      # message("[RENDER] 调用 main_ui()")  # debug only
      is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
      user_modules <- rbac_get_user_modules(rv$current_user)
      main_ui(is_admin = is_admin, user_modules = user_modules, current_user = rv$current_user)
    }
  })

  # 模块数据刷新辅助函数
  refresh_all_modules <- function() {
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$proj_data_refresh <- if(is.null(rv$proj_data_refresh)) 1 else rv$proj_data_refresh + 1
    rv$inspection_refresh_trigger <- if(is.null(rv$inspection_refresh_trigger)) 1 else rv$inspection_refresh_trigger + 1
  }

  # 控制admin菜单显示/隐藏（admin角色 或 拥有任意admin_权限的非admin用户）
  observe({
    req(rv$logged_in)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    has_admin_perm <- !is_admin && !is.null(rv$current_user) && nrow(rv$current_user) > 0 &&
      any(grepl("^admin_", rbac_get_user_perms(rv$current_user$id[1])))
    session$sendCustomMessage(type = "toggleAdminMenu", message = list(show = is_admin || has_admin_perm))
  })

  # OLD 登录模式：直接设置 rv$logged_in，renderUI 自动切换到 main_ui()
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    result <- auth_login(input$login_username, input$login_password)
    if (result$success) {
      rv$logged_in <- TRUE
      rv$current_user <- result$user
      session$sendCustomMessage(type = "saveLoginState", message = list(user_id = result$user$id[1]))
      showNotification(sprintf("欢迎回来，%s！", result$user$display_name[1] %||% result$user$username[1]), type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # OLD 自动登录：设置状态，renderUI 自动渲染 main_ui()
  observeEvent(input$auto_login_user_id, {
    req(input$auto_login_user_id)
    if (rv$logged_in) return()
    result <- auth_login_by_id(input$auto_login_user_id)
    if (result$success) {
      rv$logged_in <- TRUE
      rv$current_user <- result$user
      showNotification(sprintf("欢迎回来，%s！", result$user$display_name[1] %||% result$user$username[1]), type = "message")
    } else {
      session$sendCustomMessage(type = "clearLoginState", message = list())
    }
  })

  # 处理退出登录按钮点击事件
  observeEvent(input$logout, {
    # 调用auth_logout函数处理注销逻辑
    result <- auth_logout()
    # 清除浏览器localStorage中的登录状态
    session$sendCustomMessage(type = "clearLoginState", message = list())
    # 重置登录状态和用户信息
    rv$logged_in <- FALSE
    rv$current_user <- NULL
    # 显示注销成功通知
    showNotification(result$message, type = "message")
    # 延迟后重新加载页面，确保回到登录界面
    Sys.sleep(0.5)
    session$reload()
  })
  
  # 处理数据刷新按钮点击事件
  observeEvent(input$refresh_data, {
    # 检查登录状态
    req(rv$logged_in)
    # 更新数据表格输出
    output$data_table <- renderDT({
      DT::datatable(
        data_get_all(),  # 获取所有数据
        options = list(pageLength = 10, scrollX = TRUE),  # 表格选项
        editable = TRUE,  # 允许编辑
        rownames = FALSE  # 不显示行名
      )
    })
  })
  
  # 处理添加数据按钮点击事件
  observeEvent(input$add_data, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$data_name, input$data_type, input$data_value)
    # 调用data_add函数添加数据
    result <- data_add(input$data_name, input$data_type, input$data_value)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新数据表格
    output$data_table <- renderDT({
      DT::datatable(
        data_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        editable = TRUE,
        rownames = FALSE
      )
    })
  })
  
  # 初始渲染数据表格
  # 当应用启动或页面加载时，自动渲染数据表格
  output$data_table <- renderDT({
    DT::datatable(
      data_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      editable = TRUE,
      rownames = FALSE
    )
  })
  
  # ========== 工单模块 ==========
  # 创建响应式值存储选中的工单ID和工单详情
  rv$selected_work_order_id <- NULL
  rv$selected_work_order_detail <- NULL
  # 工单数据刷新触发器
  rv$work_order_refresh_trigger <- 0
  
  # 工单统计输出（使用触发器确保自动刷新）
  output$wo_stat_total <- renderText({
    rv$logged_in; rv$work_order_refresh_trigger
    stats <- work_order_get_stats(rv$current_user)
    message("[OUT] wo_stat_total called, logged_in=", rv$logged_in, 
            " trigger=", rv$work_order_refresh_trigger, " total=", stats$total[1])
    as.character(stats$total[1])
  })
  output$wo_stat_pending <- renderText({
    rv$logged_in; rv$work_order_refresh_trigger
    stats <- work_order_get_stats(rv$current_user)
    as.character(stats$pending[1])
  })
  output$wo_stat_assigned <- renderText({
    rv$logged_in; rv$work_order_refresh_trigger
    stats <- work_order_get_stats(rv$current_user)
    as.character(stats$assigned[1])
  })
  output$wo_stat_processing <- renderText({
    rv$logged_in; rv$work_order_refresh_trigger
    stats <- work_order_get_stats(rv$current_user)
    as.character(stats$processing[1])
  })
  output$wo_stat_completed <- renderText({
    rv$logged_in; rv$work_order_refresh_trigger
    stats <- work_order_get_stats(rv$current_user)
    as.character(stats$completed[1])
  })
  output$wo_stat_closed <- renderText({
    rv$logged_in; rv$work_order_refresh_trigger
    stats <- work_order_get_stats(rv$current_user)
    as.character(stats$closed[1])
  })
  # ★ 去掉 suspendWhenHidden=FALSE，让输出在 display:none 时挂起
  # showMainArea 后才恢复 → 首次评估即 logged_in=TRUE → 数据正常
  
  # 工单状态筛选动态UI（无标签）
  output$work_order_status_filter_ui <- renderUI({
    choices <- work_order_status_choices(include_all = TRUE)
    if (length(choices) == 0) {
      choices <- c("全部工单" = "all", "待处理" = "pending", "已派发" = "assigned",
                   "处理中" = "processing", "已完成" = "completed", "已关闭" = "closed")
    }
    selectInput("work_order_status_filter", NULL, choices = choices, selected = "all")
  })
  
  # 工单优先级动态UI（从配置读取）
  output$work_order_priority_ui <- renderUI({
    choices <- config_option_choices("work_order_priority")
    if (length(choices) == 0) {
      choices <- c("低" = "低", "中" = "中", "高" = "高", "紧急" = "紧急")
    }
    selected <- config_option_default("work_order_priority")
    if (is.null(selected) || selected == "") selected <- "中"
    selectInput("work_order_priority", "优先级", choices = choices, selected = selected)
  })
  
  # 工单分类动态UI（从配置读取）
  output$work_order_category_ui <- renderUI({
    choices <- config_option_choices("work_order_category")
    if (length(choices) == 0) {
      choices <- c("一般" = "一般", "硬件故障" = "硬件故障", "软件故障" = "软件故障", 
                    "网络问题" = "网络问题", "系统维护" = "系统维护", "账号权限" = "账号权限", "其他" = "其他")
    }
    selected <- config_option_default("work_order_category")
    if (is.null(selected) || selected == "") selected <- "一般"
    selectInput("work_order_category", "分类", choices = choices, selected = selected)
  })
  
  # 编辑工单时的优先级和分类UI（从配置读取）
  output$edit_work_order_priority_ui <- renderUI({
    choices <- config_option_choices("work_order_priority")
    if (length(choices) == 0) {
      choices <- c("低" = "低", "中" = "中", "高" = "高", "紧急" = "紧急")
    }
    selectInput("edit_work_order_priority", "优先级", choices = choices)
  })
  
  output$edit_work_order_category_ui <- renderUI({
    choices <- config_option_choices("work_order_category")
    if (length(choices) == 0) {
      choices <- c("一般" = "一般", "硬件故障" = "硬件故障", "软件故障" = "软件故障", 
                    "网络问题" = "网络问题", "系统维护" = "系统维护", "账号权限" = "账号权限", "其他" = "其他")
    }
    selectInput("edit_work_order_category", "分类", choices = choices)
  })
  
  # 渲染工单表格（使用触发器确保自动刷新）
  output$work_order_table <- renderDT({
    req(rv$logged_in)
    rv$work_order_refresh_trigger
    all_orders <- work_order_get_all(input$work_order_status_filter, current_user = rv$current_user)

    # 服务器端搜索过滤
    search_term <- input$work_order_search
    if (!is.null(search_term) && nchar(trimws(search_term)) > 0) {
      search_term <- toupper(trimws(search_term))
      # 在工单号、标题、描述、分类、处理人中搜索
      all_orders <- all_orders[grepl(search_term, toupper(all_orders$order_no)) |
                               grepl(search_term, toupper(all_orders$title)) |
                               grepl(search_term, toupper(all_orders$description)) |
                               grepl(search_term, toupper(all_orders$category)) |
                               grepl(search_term, toupper(all_orders$current_handler)), ]
    }

      # Admin权限标记
      is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
      
      # 列顺序：工单号、标题、描述、分类、优先级、处理人、状态、请求用户、创建人、时间
      if (nrow(all_orders) > 0) {
        # 工单号链接
        order_nos <- ifelse(is.na(all_orders$order_no),
                           paste0("ITS", format(as.Date(all_orders$created_at), "%Y%m%d"), sprintf("%03d", all_orders$id)),
                           all_orders$order_no)
        order_nos <- sprintf('<a href="#" class="wo-view-link" data-id="%s" style="font-weight:bold;color:#337ab7;">%s</a>', all_orders$id, order_nos)

        # 选择并重命名列
        display_data <- data.frame(
          工单号 = order_nos,
          标题 = all_orders$title,
          描述 = all_orders$description,
          分类 = ifelse(is.na(all_orders$category), "未分类", all_orders$category),
          优先级 = all_orders$priority,
          处理人 = ifelse(is.na(all_orders$current_handler), "未分配", all_orders$current_handler),
          状态 = all_orders$status,
          请求用户 = ifelse(is.na(all_orders$request_user_name), "—", all_orders$request_user_name),
          创建人 = ifelse(is.na(all_orders$creator_name), "未知", all_orders$creator_name),
          时间 = all_orders$created_at,
          stringsAsFactors = FALSE
        )
        
        # ★ Admin权限：添加复选框列（第一列）
        if (is_admin) {
          display_data <- cbind(
            选择 = sprintf('<input type="checkbox" class="wo-batch-cb" value="%d" onchange="this.closest(\'tr\').classList.toggle(\'selected-row\',this.checked)">', all_orders$id),
            display_data,
            stringsAsFactors = FALSE
          )
        }
      
      # 使用配置的函数获取状态颜色和标签
      display_data$状态 <- sapply(display_data$状态, function(s) {
        color <- work_order_status_color(s)
        label <- work_order_status_label(s)
        sprintf('<span style="background-color:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px; font-weight:bold;">%s</span>',
                color, label)
      })
      
      # 使用配置的函数获取优先级颜色
      display_data$优先级 <- sapply(display_data$优先级, function(p) {
        color <- config_option_color("work_order_priority", p)
        sprintf('<span style="background-color:%s; color:white; padding:2px 8px; border-radius:10px; font-size:11px; font-weight:bold;">%s</span>',
                color, p)
      })
    } else {
      base_cols <- list(
        工单号 = character(), 标题 = character(), 描述 = character(),
        分类 = character(), 优先级 = character(), 处理人 = character(),
        状态 = character(), 请求用户 = character(), 创建人 = character(),
        时间 = character()
      )
      if (is_admin) base_cols <- c(list(选择 = character()), base_cols)
      display_data <- as.data.frame(base_cols, stringsAsFactors = FALSE)
    }
    
    col_offset <- if (is_admin) 1 else 0  # admin多一列复选框
    
    DT::datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 50,
        paging = TRUE,
        searching = FALSE,
        ordering = TRUE,
        info = FALSE,
        lengthChange = FALSE,
        dom = 't<"float-left"p>',
        columnDefs = list(
          list(targets = col_offset + 0, width = '100px', className = 'dt-center'),
          list(targets = col_offset + 1, width = '120px', className = 'dt-left'),
          list(targets = col_offset + 2, width = '150px', className = 'dt-left'),
          list(targets = col_offset + 3, width = '70px', className = 'dt-center'),
          list(targets = col_offset + 4, width = '55px', className = 'dt-center'),
          list(targets = col_offset + 5, width = '70px', className = 'dt-center'),
          list(targets = col_offset + 6, width = '70px', className = 'dt-center'),
          list(targets = col_offset + 7, width = '70px', className = 'dt-center'),
          list(targets = col_offset + 8, width = '70px', className = 'dt-center'),
          list(targets = col_offset + 9, width = '120px', className = 'dt-center')
        ),
        rowCallback = JS(
          "function(row, data) {
            $('td', row).css({'max-height':'60px','overflow':'hidden','text-overflow':'ellipsis','white-space':'nowrap'});
            $('td:eq(2)', row).css({'white-space':'normal','line-height':'1.4em','max-height':'4.2em'});
          }"
        )
      ),
      callback = JS(
        "table.on('click', 'a.wo-view-link', function(e) {
          e.preventDefault();
          Shiny.setInputValue('work_order_view_click', $(this).data('id'));
        });"
      ),
      rownames = FALSE,
      selection = list(mode = 'single', target = 'row'),
      class = 'cell-border stripe hover'
    )
  })
  
  # 处理工单表格行选择事件（用于派发和处理页面）
  observeEvent(input$work_order_table_rows_selected, {
    req(rv$logged_in)
    selected_rows <- input$work_order_table_rows_selected
    if (length(selected_rows) > 0) {
      all_orders <- work_order_get_all(input$work_order_status_filter, current_user = rv$current_user)
      if (nrow(all_orders) >= selected_rows) {
        rv$selected_work_order_id <- all_orders$id[selected_rows]
        rv$selected_work_order_detail <- all_orders[selected_rows, ]
      }
    } else {
      rv$selected_work_order_id <- NULL
      rv$selected_work_order_detail <- NULL
    }
  })
  
  # ★ Admin权限标记（控制批量操作栏显示）
  output$wo_is_admin <- reactive({
    !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
  })
  outputOptions(output, "wo_is_admin", suspendWhenHidden = FALSE)
  
  # ★ 批量删除工单
  observeEvent(input$wo_batch_delete, {
    req(rv$logged_in)
    ids_str <- input$wo_batch_ids
    if (is.null(ids_str) || nchar(trimws(ids_str)) == 0) {
      showNotification("请先勾选工单", type = "warning"); return()
    }
    ids <- as.integer(strsplit(ids_str, ",")[[1]])
    showModal(modalDialog(
      title = "确认批量删除",
      sprintf("确定删除 %d 条工单？此操作不可撤销。", length(ids)),
      footer = tagList(
        modalButton("取消"),
        actionButton("wo_batch_delete_confirm", "确认删除", class = "btn-danger")
      ), easyClose = TRUE
    ))
  })
  observeEvent(input$wo_batch_delete_confirm, {
    req(rv$logged_in)
    ids_str <- input$wo_batch_ids
    ids <- as.integer(strsplit(ids_str, ",")[[1]])
    removeModal()
    result <- work_order_batch_delete(ids, rv$current_user)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    }
  })
  
  # ★ 批量激活工单（状态→pending）
  observeEvent(input$wo_batch_reopen, {
    req(rv$logged_in)
    ids_str <- input$wo_batch_ids
    if (is.null(ids_str) || nchar(trimws(ids_str)) == 0) {
      showNotification("请先勾选工单", type = "warning"); return()
    }
    ids <- as.integer(strsplit(ids_str, ",")[[1]])
    result <- work_order_batch_reopen(ids, rv$current_user)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      updateTextInput(session, "wo_batch_ids", value = "")
    }
  })
  
  # ★ 批量关闭工单（状态→closed）
  observeEvent(input$wo_batch_close, {
    req(rv$logged_in)
    ids_str <- input$wo_batch_ids
    if (is.null(ids_str) || nchar(trimws(ids_str)) == 0) {
      showNotification("请先勾选工单", type = "warning"); return()
    }
    ids <- as.integer(strsplit(ids_str, ",")[[1]])
    result <- work_order_batch_close(ids, rv$current_user)
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      updateTextInput(session, "wo_batch_ids", value = "")
    }
  })
  
  # 处理"查看"按钮点击事件，显示完整工单信息弹窗
  observeEvent(input$work_order_view_click, {
    req(rv$logged_in)
    wo_id <- input$work_order_view_click
    req(wo_id)
    
    # 通过工单ID获取完整详情
    wo_detail <- work_order_get_by_id(wo_id)
    
    if (nrow(wo_detail) > 0) {
      wo <- wo_detail[1, ]
      
      # 获取历史评论
      comments <- work_order_get_comments(wo_id)
      
      # 构建评论 HTML（包含添加评论输入框）
      comments_html <- "<div style='margin-top: 15px;'><div style='font-weight: bold; color: #333; margin-bottom: 10px; font-size: 15px;'>💬 评论</div>"
      # 添加评论输入区域
      comments_html <- paste0(comments_html, '
        <div style="background: #fff; padding: 12px; border-radius: 6px; border: 1px solid #ddd; margin-bottom: 12px;">
          <textarea id="work_order_comment_input" placeholder="输入评论内容..." style="width: 100%; min-height: 60px; padding: 8px; border: 1px solid #ddd; border-radius: 4px; font-size: 13px; resize: vertical;"></textarea>
          <button class="btn btn-primary btn-sm" style="margin-top: 8px;" onclick="Shiny.setInputValue(\'add_work_order_comment\', $(this).closest(\'.modal\').find(\'textarea\').val(), {priority: \'event\'});">添加评论</button>
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
      
      # 状态中文映射
      status_cn <- switch(wo$status,
        "pending" = "待处理",
        "assigned" = "已派发",
        "processing" = "处理中",
        "completed" = "已完成",
        "closed" = "已关闭",
        wo$status
      )
      
      # 优先级颜色映射
      priority_color <- switch(wo$priority,
        "紧急" = "#d9534f",
        "高" = "#f0ad4e",
        "中" = "#5bc0de",
        "低" = "#5cb85c",
        "#999"
      )
      
      # 状态颜色映射
      status_color <- switch(wo$status,
        "pending" = "#f0ad4e",
        "assigned" = "#5bc0de",
        "processing" = "#337ab7",
        "completed" = "#5cb85c",
        "closed" = "#777",
        "#999"
      )
      
      # 构建弹窗内容
      modal_content <- HTML(sprintf('
        <div style="padding: 10px; max-height: 70vh; overflow-y: auto;">
          <div style="background: #f5f5f5; padding: 14px; border-radius: 6px; margin-bottom: 15px;">
            <table style="width: 100%%; font-size: 14px;">
              <tr>
                <td style="width: 110px; font-weight: bold; color: #666;">工单号：</td>
                <td style="font-weight: bold; color: #337ab7; font-size: 15px;">%s</td>
                <td style="width: 110px; font-weight: bold; color: #666;">标题：</td>
                <td style="font-weight: bold;">%s</td>
              </tr>
              <tr><td colspan="4" style="height: 8px;"></td></tr>
              <tr>
                <td style="font-weight: bold; color: #666;">优先级：</td>
                <td><span style="background: %s; color: white; padding: 3px 10px; border-radius: 4px; font-size: 13px;">%s</span></td>
                <td style="font-weight: bold; color: #666;">状态：</td>
                <td><span style="background: %s; color: white; padding: 3px 10px; border-radius: 4px; font-size: 13px;">%s</span></td>
              </tr>
              <tr><td colspan="4" style="height: 8px;"></td></tr>
              <tr>
                <td style="font-weight: bold; color: #666;">分类：</td>
                <td>%s</td>
                <td style="font-weight: bold; color: #666;">创建人：</td>
                <td>%s</td>
              </tr>
              <tr><td colspan="4" style="height: 8px;"></td></tr>
              <tr>
                <td style="font-weight: bold; color: #666;">处理人：</td>
                <td>%s</td>
                <td style="font-weight: bold; color: #666;">请求用户：</td>
                <td>%s</td>
              </tr>
              <tr><td colspan="4" style="height: 8px;"></td></tr>
              <tr>
                <td style="font-weight: bold; color: #666;">创建时间：</td>
                <td>%s</td>
                <td style="font-weight: bold; color: #666;">指派时间：</td>
                <td>%s</td>
              </tr>
              <tr><td colspan="4" style="height: 8px;"></td></tr>
              <tr>
                <td style="font-weight: bold; color: #666;">处理时间：</td>
                <td>%s</td>
                <td style="font-weight: bold; color: #666;">完成时间：</td>
                <td>%s</td>
              </tr>
            </table>
          </div>
          
          <div style="margin-bottom: 15px;">
            <div style="font-weight: bold; color: #333; margin-bottom: 8px; font-size: 15px;">📋 工单描述</div>
            <div style="background: #fafafa; padding: 14px; border-radius: 6px; border-left: 4px solid #337ab7; min-height: 50px; white-space: pre-wrap; word-wrap: break-word; font-size: 14px; line-height: 1.7;">%s</div>
          </div>
          
          <div style="margin-bottom: 15px;">
            <div style="font-weight: bold; color: #333; margin-bottom: 8px; font-size: 15px;">✅ 解决方案/关闭原因</div>
            <div style="background: #f0f9e8; padding: 14px; border-radius: 6px; border-left: 4px solid #5cb85c; min-height: 50px; white-space: pre-wrap; word-wrap: break-word; font-size: 14px; line-height: 1.7;">%s</div>
          </div>
          
          %s
        </div>
      ',
      ifelse(is.na(wo$order_no), sprintf("ITS%s%03d", format(as.Date(wo$created_at), "%Y%m%d"), wo$id), wo$order_no),
      wo$title,
      priority_color,
      wo$priority,
      status_color,
      status_cn,
      ifelse(is.na(wo$category), "未分类", wo$category),
      ifelse(is.na(wo$creator_name), "未知", wo$creator_name),
      ifelse(is.na(wo$handler_name), ifelse(is.na(wo$assignee_name), "未分配", wo$assignee_name), wo$handler_name),
      ifelse(is.na(wo$request_user_name), "—", wo$request_user_name),
      wo$created_at,
      ifelse(is.na(wo$assigned_at), "未指派", wo$assigned_at),
      ifelse(is.na(wo$handled_at), "未开始", wo$handled_at),
      ifelse(is.na(wo$completed_at), "未完成", wo$completed_at),
      ifelse(is.na(wo$description), "无描述", wo$description),
      ifelse(is.na(wo$resolution), "暂无", wo$resolution),
      comments_html
      ))
      
      # 根据状态决定显示哪些操作按钮
      can_assign <- wo$status %in% c("pending")
      can_start <- wo$status %in% c("pending", "assigned")
      can_complete <- wo$status %in% c("processing")
      can_close <- wo$status %in% c("completed")
      
      # 检查是否为Admin + RBAC权限
      is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
      can_edit_wo <- is_admin || rbac_check(rv$current_user, "wo_edit")
      can_delete_wo <- is_admin || rbac_check(rv$current_user, "wo_delete")
      
      # 构建操作按钮HTML
      action_buttons <- ""
      if (can_assign || can_start || can_complete || can_close || can_edit_wo) {
        action_buttons <- '<div style="margin-bottom: 10px; padding: 10px; background: #f0f7ff; border-radius: 6px;">'
        # 修改和删除按钮（Admin或有对应RBAC权限）
        if (can_edit_wo) {
          action_buttons <- paste0(action_buttons, sprintf('<button class="btn btn-warning btn-sm" style="margin-right: 5px;" onclick="Shiny.setInputValue(\'modal_work_order_edit\', %d, {priority: \'event\'});">修改</button>', wo_id))
        }
        if (can_delete_wo) {
          action_buttons <- paste0(action_buttons, sprintf('<button class="btn btn-danger btn-sm" style="margin-right: 5px;" onclick="Shiny.setInputValue(\'delete_work_order_btn\', %d, {priority: \'event\'});">删除</button>', wo_id))
        }
        if (can_assign) {
          action_buttons <- paste0(action_buttons, sprintf('<button class="btn btn-primary btn-sm" style="margin-right: 5px;" onclick="Shiny.setInputValue(\'modal_work_order_assign\', %d, {priority: \'event\'});">派发</button>', wo_id))
        }
        if (can_start) {
          action_buttons <- paste0(action_buttons, sprintf('<button class="btn btn-info btn-sm" style="margin-right: 5px;" onclick="Shiny.setInputValue(\'modal_work_order_start\', %d, {priority: \'event\'});">开始处理</button>', wo_id))
        }
        if (can_complete) {
          action_buttons <- paste0(action_buttons, sprintf('<button class="btn btn-success btn-sm" style="margin-right: 5px;" onclick="Shiny.setInputValue(\'modal_work_order_complete\', %d, {priority: \'event\'});">完成</button>', wo_id))
        }
        if (can_close) {
          action_buttons <- paste0(action_buttons, sprintf('<button class="btn btn-warning btn-sm" style="margin-right: 5px;" onclick="Shiny.setInputValue(\'modal_work_order_close\', %d, {priority: \'event\'});">关闭</button>', wo_id))
        }
        action_buttons <- paste0(action_buttons, '</div>')
      }
      
      showModal(modalDialog(
        title = paste0("工单详情 - ", ifelse(is.na(wo$order_no), sprintf("ITS%s%03d", format(as.Date(wo$created_at), "%Y%m%d"), wo$id), wo$order_no)),
        HTML(modal_content),
        footer = tagList(
          HTML(action_buttons),
          modalButton("关闭")
        ),
        size = "l",
        easyClose = TRUE
      ))
    }
  })
  
  # 处理弹窗内派发按钮点击事件
  observeEvent(input$modal_work_order_assign, {
    req(rv$logged_in)
    wo_id <- input$modal_work_order_assign
    req(wo_id)
    
    # 获取工单详情
    wo_detail <- work_order_get_by_id(wo_id)
    if (nrow(wo_detail) > 0) {
      rv$selected_work_order_id <- wo_id
      rv$selected_work_order_detail <- wo_detail
      
      # 获取可派发用户列表
      users <- work_order_get_assignable_users()
      if (nrow(users) > 0) {
        uname <- ifelse(!is.na(users$display_name) & users$display_name != "", users$display_name, users$username)
        choices <- setNames(users$id, sprintf("%s (%s)", uname, users$role))
        
        # 关闭详情弹窗
        removeModal()
        
        # 显示派发选择弹窗
        showModal(modalDialog(
          title = "派发工单",
          wellPanel(
            h4("选择处理人"),
            selectInput("modal_assignee_select", "处理人", choices = choices),
            actionButton("modal_confirm_assign", "确认派发", class = "btn-primary")
          ),
          footer = modalButton("取消"),
          easyClose = TRUE
        ))
      }
    }
  })
  
  # 处理弹窗内开始处理按钮点击事件
  observeEvent(input$modal_work_order_start, {
    req(rv$logged_in)
    wo_id <- input$modal_work_order_start
    req(wo_id)
    
    result <- work_order_start_handle(wo_id, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    }
  })
  
  # 处理弹窗内完成工单按钮点击事件
  observeEvent(input$modal_work_order_complete, {
    req(rv$logged_in)
    wo_id <- input$modal_work_order_complete
    req(wo_id)
    
    removeModal()
    
    showModal(modalDialog(
      title = "完成工单",
      wellPanel(
        h4("输入解决方案/处理结果"),
        textAreaInput("modal_resolution_input", "", rows = 4),
        actionButton("modal_confirm_complete", "确认完成", class = "btn-success")
      ),
      footer = modalButton("取消"),
      easyClose = TRUE
    ))
    
    # 临时保存工单ID用于完成
    rv$temp_complete_wo_id <- wo_id
  })
  
  # 处理弹窗内关闭工单按钮点击事件
  observeEvent(input$modal_work_order_close, {
    req(rv$logged_in)
    wo_id <- input$modal_work_order_close
    req(wo_id)
    
    removeModal()
    
    showModal(modalDialog(
      title = "关闭工单",
      wellPanel(
        h4("选择关闭原因"),
        selectInput("modal_close_reason_input", "", 
                   choices = c("请选择关闭原因" = "", "已处理和交付" = "已处理和交付", "无法处理关闭" = "无法处理关闭")),
        actionButton("modal_confirm_close", "确认关闭", class = "btn-warning")
      ),
      footer = modalButton("取消"),
      easyClose = TRUE
    ))
    
    rv$temp_close_wo_id <- wo_id
  })
  
  # 处理弹窗内确认派发
  observeEvent(input$modal_confirm_assign, {
    req(rv$logged_in)
    req(input$modal_assignee_select)
    req(rv$selected_work_order_id)
    
    result <- work_order_assign(rv$selected_work_order_id, input$modal_assignee_select, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      rv$selected_work_order_id <- NULL
      rv$selected_work_order_detail <- NULL
    }
  })
  
  # 处理弹窗内确认完成
  observeEvent(input$modal_confirm_complete, {
    req(rv$logged_in)
    req(input$modal_resolution_input)
    req(rv$temp_complete_wo_id)
    
    result <- work_order_complete(rv$temp_complete_wo_id, input$modal_resolution_input, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      rv$temp_complete_wo_id <- NULL
    }
  })
  
  # 处理弹窗内确认关闭
  observeEvent(input$modal_confirm_close, {
    req(rv$logged_in)
    req(input$modal_close_reason_input)
    req(input$modal_close_reason_input != "")
    req(rv$temp_close_wo_id)
    
    result <- work_order_close(rv$temp_close_wo_id, input$modal_close_reason_input, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      rv$temp_close_wo_id <- NULL
    }
  })
  
  # 是否有选中工单
  output$work_order_selected <- reactive({
    return(!is.null(rv$selected_work_order_id))
  })
  outputOptions(output, "work_order_selected", suspendWhenHidden = FALSE)
  
  # 是否可以开始处理（状态为 pending 或 assigned）
  output$work_order_can_start <- reactive({
    if (is.null(rv$selected_work_order_detail)) return(FALSE)
    return(rv$selected_work_order_detail$status[1] %in% c("pending", "assigned"))
  })
  outputOptions(output, "work_order_can_start", suspendWhenHidden = FALSE)
  
  # 是否可以完成（状态为 processing）
  output$work_order_can_complete <- reactive({
    if (is.null(rv$selected_work_order_detail)) return(FALSE)
    return(rv$selected_work_order_detail$status[1] == "processing")
  })
  outputOptions(output, "work_order_can_complete", suspendWhenHidden = FALSE)

  # 是否可以关闭（状态不为 closed）
  output$work_order_can_close <- reactive({
    if (is.null(rv$selected_work_order_detail)) return(FALSE)
    return(rv$selected_work_order_detail$status[1] != "closed")
  })
  outputOptions(output, "work_order_can_close", suspendWhenHidden = FALSE)

  # 是否可以编辑（选中工单即可编辑基本信息）
  output$work_order_can_edit <- reactive({
    return(!is.null(rv$selected_work_order_id))
  })
  outputOptions(output, "work_order_can_edit", suspendWhenHidden = FALSE)

  # 是否处于编辑模式
  rv$work_order_edit_mode <- FALSE
  output$work_order_edit_mode <- reactive({
    return(rv$work_order_edit_mode)
  })
  outputOptions(output, "work_order_edit_mode", suspendWhenHidden = FALSE)

  # 显示选中工单信息（派发页面）
  output$selected_work_order_info <- renderPrint({
    req(rv$selected_work_order_detail)
    wo <- rv$selected_work_order_detail
    cat(sprintf("工单ID: %d\n", wo$id[1]))
    cat(sprintf("标题: %s\n", wo$title[1]))
    cat(sprintf("优先级: %s\n", wo$priority[1]))
    cat(sprintf("当前状态: %s\n", wo$status[1]))
    cat(sprintf("请求用户: %s\n", ifelse(is.na(wo$request_user_name[1]) || wo$request_user_name[1] == "", "—", wo$request_user_name[1])))
    cat(sprintf("创建人: %s\n", ifelse(is.na(wo$creator_name[1]), "未知", wo$creator_name[1])))
    if (!is.na(wo$assignee_name[1])) {
      cat(sprintf("当前处理人: %s\n", wo$assignee_name[1]))
    }
  })

  # 显示选中工单信息（处理页面）
  output$selected_work_order_info2 <- renderPrint({
    req(rv$selected_work_order_detail)
    wo <- rv$selected_work_order_detail
    cat(sprintf("工单ID: %d\n", wo$id[1]))
    cat(sprintf("标题: %s\n", wo$title[1]))
    cat(sprintf("描述: %s\n", wo$description[1]))
    cat(sprintf("优先级: %s\n", wo$priority[1]))
    cat(sprintf("分类: %s\n", ifelse(is.na(wo$category[1]), "未分类", wo$category[1])))
    cat(sprintf("当前状态: %s\n", wo$status[1]))
    cat(sprintf("请求用户: %s\n", ifelse(is.na(wo$request_user_name[1]) || wo$request_user_name[1] == "", "—", wo$request_user_name[1])))
    cat(sprintf("创建人: %s\n", ifelse(is.na(wo$creator_name[1]), "未知", wo$creator_name[1])))
    if (!is.na(wo$assignee_name[1])) {
      cat(sprintf("指派给: %s\n", wo$assignee_name[1]))
    }
    if (!is.na(wo$handler_name[1])) {
      cat(sprintf("处理人: %s\n", wo$handler_name[1]))
    }
  })
  
  # 加载可派发用户列表
  observe({
    req(rv$logged_in)
    users <- work_order_get_assignable_users()
    if (nrow(users) > 0) {
      uname <- ifelse(!is.na(users$display_name) & users$display_name != "", users$display_name, users$username)
      choices <- setNames(users$id, sprintf("%s (%s)", uname, users$role))
      updateSelectInput(session, "work_order_assignee", choices = choices)
    }
  })
  
  # 处理创建工单按钮点击事件
  observe({
    toggle_btn("add_work_order", btn_ok(input$work_order_title) && btn_ok(input$work_order_description))
  })
  observeEvent(input$add_work_order, {
    req(rv$logged_in)
    req(input$work_order_title, input$work_order_description, input$work_order_priority)

    result <- work_order_add(
      input$work_order_title,
      input$work_order_description,
      input$work_order_priority,
      input$work_order_category,
      "",
      input$work_order_request_user,
      rv$current_user
    )
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    # 清空输入
    updateTextInput(session, "work_order_title", value = "")
    updateTextAreaInput(session, "work_order_description", value = "")
    updateTextInput(session, "work_order_request_user", value = "")
    updateSelectInput(session, "work_order_priority", selected = "中")
    updateSelectInput(session, "work_order_category", selected = "一般")

    # 触发刷新
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
  })

  # 处理快速工单按钮点击事件
  observe({
    toggle_btn("create_quick_work_order", btn_ok(input$quick_work_order_text))
  })
  observeEvent(input$create_quick_work_order, {
    req(rv$logged_in)
    req(input$quick_work_order_text)

    text <- trimws(input$quick_work_order_text)
    if (text == "") {
      showNotification("请输入工单内容", type = "warning")
      return()
    }

    # 解析文本
    parsed <- work_order_parse_quick_text(text)

    if (!parsed$success) {
      showNotification(parsed$message, type = "error")
      return()
    }

    # 生成工单标题：从内容中提取前20个字符作为标题
    title_content <- gsub("[\\n\\r]", " ", parsed$description)  # 替换换行符为空格
    if (nchar(title_content) > 20) {
      title_content <- substr(title_content, 1, 20)
    }
    title <- paste0(parsed$category, " - ", title_content)

    # 创建工单
    result <- work_order_add(
      title = title,
      description = parsed$description,
      priority = "中",
      category = parsed$category,
      subcategory = "",
      request_user = parsed$request_user,
      current_user = rv$current_user
    )

    if (result$success) {
      # 如果指定了分派人，查找用户ID并派发
      if (!is.null(parsed$assignee_name) && parsed$assignee_name != "") {
        assignee_id <- work_order_find_user_by_name(parsed$assignee_name)
        if (!is.null(assignee_id)) {
          # 获取刚创建的工单ID
          con <- db_connect()
          tryCatch({
            # 查找最新创建的工单（按创建时间倒序）
            latest_order <- dbGetQuery(con, "SELECT id FROM work_orders ORDER BY created_at DESC LIMIT 1")
            if (nrow(latest_order) > 0) {
              assign_result <- work_order_assign(latest_order$id[1], assignee_id, rv$current_user)
              if (assign_result$success) {
                showNotification(paste0("工单创建成功并已派发给：", parsed$assignee_name), type = "message")
              } else {
                showNotification(paste0("工单创建成功，但派发失败：", assign_result$message), type = "warning")
              }
            }
          }, error = function(e) {
            showNotification(paste0("工单创建成功，但派发失败：", e$message), type = "warning")
          }, finally = {
            db_disconnect(con)
          })
        } else {
          showNotification(paste0("工单创建成功，但未找到分派人：", parsed$assignee_name), type = "warning")
        }
      } else {
        showNotification("工单创建成功", type = "message")
      }

      # 清空输入
      updateTextAreaInput(session, "quick_work_order_text", value = "")

      # 触发刷新
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # 处理快速创建按钮点击事件 - 滚动到快速工单区域并聚焦文本框
  observeEvent(input$show_quick_work_order, {
    session$sendCustomMessage("runjs", 'scrollToQuickWO')
  })
  
  # ★ 批量补工单：存储解析结果供预览和创建共用
  rv$batch_parsed <- NULL
  
  # 解析缓存（输入变化时自动重新解析）
  observe({
    req(rv$logged_in)
    text <- trimws(input$batch_work_order_text %||% "")
    if (nchar(text) < 10) {
      rv$batch_parsed <- NULL
      return()
    }
    rv$batch_parsed <- work_order_batch_parse(text)
  })
  
  # 批量补工单：可编辑预览（请求人可修改）
  output$batch_work_order_preview <- renderUI({
    parsed <- rv$batch_parsed
    if (is.null(parsed)) return(NULL)
    if (!parsed$success) {
      return(div(style = "margin-top:10px; color:#d9534f; font-size:12px;", parsed$message))
    }
    div(style = "margin-top:10px; font-size:12px;",
      # 摘要行
      p(style = "color:#337ab7; margin:0 0 6px; font-weight:600;",
        sprintf("处理人：%s  |  日期：%s  |  共 %d 条工单  |  时间：%s",
          parsed$handler_name, parsed$batch_date, parsed$count, parsed$batch_time)),
      # 可编辑表格头
      div(style = "display:flex; font-weight:600; color:#666; padding:4px 8px; background:#eee; border-radius:4px 4px 0 0;",
        div(style = "width:30px;", "#"), 
        div(style = "width:90px;", "请求人"),
        div(style = "flex:1;", "标题")
      ),
      # 可编辑行
      div(style = "max-height:300px; overflow-y:auto; border:1px solid #ddd; border-top:none; border-radius:0 0 4px 4px;",
        lapply(seq_along(parsed$orders), function(i) {
          o <- parsed$orders[[i]]
          div(style = paste0("display:flex; align-items:center; padding:2px 8px; border-bottom:1px solid #f0f0f0;",
            if(i %% 2 == 0) "background:#fafafa;" else "background:#fff;"),
            div(style = "width:30px; color:#999;", i),
            div(style = "width:90px;",
              textInput(paste0("batch_req_user_", i), NULL, value = o$request_user, width = "85px")
            ),
            div(style = "flex:1; padding-left:4px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
              o$title)
          )
        })
      )
    )
  })
  
  # 批量补工单：创建（读取可编辑预览中的修改值）
  observe({
    toggle_btn("create_batch_work_order", !is.null(rv$batch_parsed) && rv$batch_parsed$success)
  })
  observeEvent(input$create_batch_work_order, {
    req(rv$logged_in)
    parsed <- rv$batch_parsed
    if (is.null(parsed) || !parsed$success) {
      showNotification("请先粘贴日报文本并确认预览无误", type = "warning")
      return()
    }
    
    # ★ 从可编辑输入框读取用户修改后的请求人
    for (i in seq_along(parsed$orders)) {
      edited <- input[[paste0("batch_req_user_", i)]]
      if (!is.null(edited) && nchar(trimws(edited)) > 0) {
        parsed$orders[[i]]$request_user <- trimws(edited)
      }
    }
    
    removeModal()  # just in case
    withProgress(message = "正在批量创建工单...", value = 0, {
      result <- work_order_batch_create(parsed, rv$current_user)
      incProgress(1)
    })
    
    showNotification(result$message, type = if(result$success) "message" else "error")
    
    if (result$success) {
      updateTextAreaInput(session, "batch_work_order_text", value = "")
      rv$batch_parsed <- NULL
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    }
  })
  
  # 处理刷新工单按钮点击事件
  observeEvent(input$refresh_work_orders, {
    req(rv$logged_in)
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
  })
  
  # 处理新建工单按钮点击事件 - 显示弹窗
  observeEvent(input$show_create_work_order, {
    req(rv$logged_in)
    
    # 获取优先级和分类选项
    priority_choices <- config_option_choices("work_order_priority")
    if (length(priority_choices) == 0) {
      priority_choices <- c("低" = "低", "中" = "中", "高" = "高", "紧急" = "紧急")
    }
    priority_default <- config_option_default("work_order_priority")
    if (is.null(priority_default) || priority_default == "") priority_default <- "中"
    
    category_choices <- config_option_choices("work_order_category")
    if (length(category_choices) == 0) {
      category_choices <- c("一般" = "一般", "硬件故障" = "硬件故障", "软件故障" = "软件故障", 
                            "网络问题" = "网络问题", "系统维护" = "系统维护", "账号权限" = "账号权限", "其他" = "其他")
    }
    category_default <- config_option_default("work_order_category")
    if (is.null(category_default) || category_default == "") category_default <- "一般"
    
    showModal(modalDialog(
      title = "新建工单",
      wellPanel(
        h4("工单信息"),
        textInput("modal_wo_title", "标题", placeholder = "请输入工单标题"),
        textAreaInput("modal_wo_description", "描述", rows = 4, placeholder = "请输入工单描述"),
        textInput("modal_wo_request_user", "请求用户", placeholder = "请输入请求用户（工单来源者）"),
        selectInput("modal_wo_priority", "优先级", choices = priority_choices, selected = priority_default),
        selectInput("modal_wo_category", "分类", choices = category_choices, selected = category_default),
        actionButton("modal_confirm_create_wo", "创建工单", class = "btn-primary")
      ),
      footer = modalButton("取消"),
      easyClose = TRUE
    ))
  })

  # 处理弹窗内确认创建工单
  observeEvent(input$modal_confirm_create_wo, {
    req(rv$logged_in)
    req(input$modal_wo_title, input$modal_wo_description)

    result <- work_order_add(
      input$modal_wo_title,
      input$modal_wo_description,
      input$modal_wo_priority,
      input$modal_wo_category,
      "",
      input$modal_wo_request_user,
      rv$current_user
    )
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    }
  })
  
  # 处理弹窗内修改工单按钮点击事件（Admin或有wo_edit权限）
  observeEvent(input$modal_work_order_edit, {
    req(rv$logged_in)
    wo_id <- input$modal_work_order_edit
    req(wo_id)
    
    # 检查RBAC权限（Admin或拥有wo_edit权限）
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    if (!is_admin && !rbac_check(rv$current_user, "wo_edit")) {
      showNotification("没有修改工单的权限", type = "error")
      return()
    }
    
    # 获取工单详情
    wo_detail <- work_order_get_by_id(wo_id)
    if (nrow(wo_detail) > 0) {
      rv$selected_work_order_id <- wo_id
      rv$selected_work_order_detail <- wo_detail
      
      # 获取优先级和分类选项
      priority_choices <- config_option_choices("work_order_priority")
      if (length(priority_choices) == 0) {
        priority_choices <- c("低" = "低", "中" = "中", "高" = "高", "紧急" = "紧急")
      }
      
      category_choices <- config_option_choices("work_order_category")
      if (length(category_choices) == 0) {
        category_choices <- c("一般" = "一般", "硬件故障" = "硬件故障", "软件故障" = "软件故障", 
                              "网络问题" = "网络问题", "系统维护" = "系统维护", "账号权限" = "账号权限", "其他" = "其他")
      }
      
      wo <- wo_detail[1, ]
      
      # 获取状态选项
      status_choices <- c("pending" = "待处理", "assigned" = "已派发", "processing" = "处理中", 
                          "completed" = "已完成", "closed" = "已关闭")
      
      # 获取工程师列表
      con <- db_connect()
      engineers <- tryCatch({
        dbGetQuery(con, "SELECT id, username FROM users 
                      WHERE role IN ('it_desk', 'it_engineer', 'sys_engineer', 'admin') 
                      AND active = 1 ORDER BY username")
      }, error = function(e) data.frame(), finally = db_disconnect(con))
      
      engineer_choices <- c("未分配" = "unassigned")
      if (nrow(engineers) > 0) {
        engineer_choices <- c(engineer_choices, setNames(engineers$id, engineers$username))
      }
      
      # 关闭详情弹窗
      removeModal()
      
      # 显示修改弹窗
      showModal(modalDialog(
        title = paste0("修改工单 - ", ifelse(is.na(wo$order_no), sprintf("ITS%s%03d", format(as.Date(wo$created_at), "%Y%m%d"), wo$id), wo$order_no)),
        wellPanel(
          h4("修改工单信息"),
          textInput("modal_edit_wo_order_no", "工单号", value = ifelse(is.na(wo$order_no), "", wo$order_no), placeholder = "ITSYYYYMMDDXXX"),
          textInput("modal_edit_wo_title", "标题", value = wo$title),
          textAreaInput("modal_edit_wo_description", "描述", rows = 4, value = wo$description),
          textInput("modal_edit_wo_request_user", "请求用户", value = ifelse(is.na(wo$request_user), "", wo$request_user), placeholder = "请输入请求用户"),
          selectInput("modal_edit_wo_priority", "优先级", choices = priority_choices, selected = wo$priority),
          selectInput("modal_edit_wo_category", "分类", choices = category_choices, selected = ifelse(is.na(wo$category), "一般", wo$category)),
          selectInput("modal_edit_wo_status", "状态", choices = status_choices, selected = wo$status),
          selectInput("modal_edit_wo_assigned_to", "指派给", choices = engineer_choices,
                      selected = ifelse(is.na(wo$assigned_to), "unassigned", wo$assigned_to)),
          actionButton("modal_confirm_edit_wo", "保存修改", class = "btn-warning")
        ),
        footer = modalButton("取消"),
        easyClose = TRUE
      ))
    }
  })
  
  # 处理弹窗内确认修改工单
  observeEvent(input$modal_confirm_edit_wo, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)
    req(input$modal_edit_wo_order_no, input$modal_edit_wo_title, input$modal_edit_wo_description)

    result <- work_order_edit(
      rv$selected_work_order_id,
      input$modal_edit_wo_order_no,
      input$modal_edit_wo_title,
      input$modal_edit_wo_description,
      input$modal_edit_wo_priority,
      input$modal_edit_wo_category,
      input$modal_edit_wo_status,
      input$modal_edit_wo_assigned_to,
      input$modal_edit_wo_request_user,
      rv$current_user
    )
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    if (result$success) {
      removeModal()
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      rv$selected_work_order_id <- NULL
      rv$selected_work_order_detail <- NULL
    }
  })
  
  # 处理派发工单按钮点击事件
  observeEvent(input$assign_work_order, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)
    req(input$work_order_assignee)
    
    result <- work_order_assign(rv$selected_work_order_id, input$work_order_assignee, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    # 触发刷新
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$selected_work_order_id <- NULL
    rv$selected_work_order_detail <- NULL
  })
  
  # 处理开始处理工单按钮点击事件
  observeEvent(input$start_handle_work_order, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)
    
    result <- work_order_start_handle(rv$selected_work_order_id, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    # 触发刷新并更新选中工单详情
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$selected_work_order_detail <- work_order_get_by_id(rv$selected_work_order_id)
  })
  
  # 处理完成工单按钮点击事件
  observeEvent(input$complete_work_order, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)
    req(input$work_order_resolution)
    
    result <- work_order_complete(rv$selected_work_order_id, input$work_order_resolution, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    # 清空解决方案输入
    updateTextAreaInput(session, "work_order_resolution", value = "")
    
    # 触发刷新
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$selected_work_order_id <- NULL
    rv$selected_work_order_detail <- NULL
  })

  # 处理关闭工单按钮点击事件
  observeEvent(input$close_work_order, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)

    # 验证必须选择关闭原因
    if (is.null(input$work_order_close_reason) || input$work_order_close_reason == "") {
      showNotification("请选择关闭原因", type = "error")
      return()
    }

    result <- work_order_close(rv$selected_work_order_id, input$work_order_close_reason, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    # 重置关闭原因选择
    updateSelectInput(session, "work_order_close_reason", selected = "")

    # 触发刷新
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$selected_work_order_id <- NULL
    rv$selected_work_order_detail <- NULL
  })

  # 处理编辑工单按钮点击事件
  observeEvent(input$edit_work_order_btn, {
    req(rv$logged_in)
    req(rv$selected_work_order_detail)

    wo <- rv$selected_work_order_detail
    updateTextInput(session, "edit_work_order_title", value = wo$title[1])
    updateTextAreaInput(session, "edit_work_order_description", value = wo$description[1])
    updateSelectInput(session, "edit_work_order_priority", selected = wo$priority[1])
    updateSelectInput(session, "edit_work_order_category", selected = ifelse(is.na(wo$category[1]), "一般", wo$category[1]))

    rv$work_order_edit_mode <- TRUE
  })

  # 处理保存编辑按钮点击事件
  observeEvent(input$save_edit_work_order, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)
    req(input$edit_work_order_title, input$edit_work_order_description)

    result <- work_order_edit(
      rv$selected_work_order_id,
      input$edit_work_order_title,
      input$edit_work_order_description,
      input$edit_work_order_priority,
      input$edit_work_order_category,
      rv$current_user
    )
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    if (result$success) {
      rv$work_order_edit_mode <- FALSE
      rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
      rv$selected_work_order_detail <- work_order_get_by_id(rv$selected_work_order_id)
    }
  })

  # 处理取消编辑按钮点击事件
  observeEvent(input$cancel_edit_work_order, {
    rv$work_order_edit_mode <- FALSE
  })

  # 处理删除工单按钮点击事件
  observeEvent(input$delete_work_order_btn, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)

    showModal(modalDialog(
      title = "确认删除",
      "确定要删除这个工单吗？此操作不可撤销。",
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_delete_work_order", "确认删除", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  # 处理确认删除工单
  observeEvent(input$confirm_delete_work_order, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)

    result <- work_order_delete(rv$selected_work_order_id, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    removeModal()
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$selected_work_order_id <- NULL
    rv$selected_work_order_detail <- NULL
  })

  # 处理添加评论按钮点击事件
  observeEvent(input$add_work_order_comment, {
    req(rv$logged_in)
    req(rv$selected_work_order_id)
    comment_text <- input$add_work_order_comment
    req(comment_text)
    comment_text <- trimws(comment_text)
    req(nchar(comment_text) > 0)

    result <- work_order_add_comment(rv$selected_work_order_id, comment_text, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    if (result$success) {
      # 刷新评论列表
      rv$work_order_comment_refresh <- ifelse(is.null(rv$work_order_comment_refresh), 0, rv$work_order_comment_refresh + 1)
      # 重新加载工单详情以刷新评论显示
      wo_detail <- work_order_get_by_id(rv$selected_work_order_id)
      if (nrow(wo_detail) > 0) {
        rv$selected_work_order_detail <- wo_detail
      }
    }
  })

  # 渲染工单评论列表
  output$work_order_comments_ui <- renderUI({
    req(rv$logged_in)
    req(rv$selected_work_order_id)

    # 触发刷新
    rv$work_order_comment_refresh

    comments <- work_order_get_comments(rv$selected_work_order_id)

    if (nrow(comments) == 0) {
      return(div(class = "text-muted", style = "padding: 10px;", "暂无评论"))
    }

    comment_list <- lapply(1:nrow(comments), function(i) {
      div(
        style = "background: #f9f9f9; padding: 8px 12px; margin-bottom: 6px; border-radius: 4px; border-left: 3px solid #5bc0de;",
        div(
          style = "font-size: 12px; color: #666; margin-bottom: 4px;",
          span(style = "font-weight: bold;", ifelse(is.na(comments$creator_name[i]), "未知用户", comments$creator_name[i])),
          span(style = "margin-left: 10px;", comments$created_at[i])
        ),
        div(
          style = "font-size: 13px; line-height: 1.5;",
          comments$comment[i]
        )
      )
    })

    do.call(tagList, comment_list)
  })

  # 初始渲染收集器表格
  output$collector_table <- renderDT({
    DT::datatable(
      info_collector_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 添加收集器按钮：只有填写了名称和类型才启用
  observe({
    ok <- btn_ok(input$collector_name) && btn_ok(input$collector_type)
    toggle_btn("add_collector", ok)
  })
  # 处理添加收集器按钮点击事件
  observeEvent(input$add_collector, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$collector_name, input$collector_type, input$collector_config)
    # 调用info_collector_add函数添加收集器
    result <- info_collector_add(input$collector_name, input$collector_type, input$collector_config, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新收集器表格
    output$collector_table <- renderDT({
      DT::datatable(
        info_collector_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 处理刷新收集器按钮点击事件
  observeEvent(input$refresh_collectors, {
    # 检查登录状态
    req(rv$logged_in)
    # 刷新收集器表格
    output$collector_table <- renderDT({
      DT::datatable(
        info_collector_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 处理模型刷新按钮点击事件
  observeEvent(input$refresh_models, {
    # 检查登录状态
    req(rv$logged_in)
    # 更新模型表格输出
    output$model_table <- renderDT({
      DT::datatable(
        model_get_all(),  # 获取所有模型
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 训练模型按钮：填写了名称才启用
  observe({
    toggle_btn("train_model", btn_ok(input$model_name))
  })
  # 处理模型训练按钮点击事件
  observeEvent(input$train_model, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$model_name, input$model_type)
    
    # 显示训练进度条
    withProgress(message = "正在训练模型...", value = 0, {
      # 调用model_train函数训练模型
      result <- model_train(input$model_name, input$model_type, input$model_params)
      # 更新进度条
      incProgress(1)
    })
    
    # 显示训练结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    # 将训练好的模型添加到数据库
    model_add(input$model_name, input$model_type, input$model_params, result$accuracy)
    
    # 显示训练结果详情
    output$training_result <- renderPrint({
      cat(sprintf("模型名称: %s\n", input$model_name))
      cat(sprintf("模型类型: %s\n", input$model_type))
      cat(sprintf("准确率: %.2f%%\n", result$accuracy * 100))
    })
    
    # 刷新模型表格
    output$model_table <- renderDT({
      DT::datatable(
        model_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 初始渲染模型表格
  output$model_table <- renderDT({
    DT::datatable(
      model_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 显示当前图表类型的算法代码
  output$viz_code <- renderUI({
    req(rv$logged_in)
    viz_highlight_r(viz_get_algorithm_code(input$viz_type, input$viz_data))
  })

  # 处理生成可视化按钮点击事件
  observeEvent(input$generate_viz, {
    req(rv$logged_in)
    withProgress(message = '正在生成图表', value = 0, {
      incProgress(0.3, detail = '准备数据...')
      output$viz_plot <- viz_render(input$viz_type, input$viz_data)
      incProgress(1, detail = '完成')
    })
  })

  # 初始渲染可视化图表
  output$viz_plot <- viz_render("词云图", "记事数据")

  # 可视化页 - 流程监控指标
  output$viz_mtr_complete_rate <- renderText({ "0%" })
  output$viz_mtr_timeout_rate <- renderText({ "0%" })
  output$viz_mtr_avg_duration <- renderText({ "0 分钟" })
  output$viz_mtr_running <- renderText({ "0" })
  output$viz_mtr_today <- renderText({ "暂无" })
  
  # ====================================
  # 组织架构模块 — Xmind 思维导图风格
  # ====================================
  org_trigger <- reactiveVal(0)
  org_refresh <- function() { org_trigger(org_trigger() + 1) }
  org_selected_dept <- reactiveVal(NULL)      # 当前选中的部门/人员 ID
  org_selected_type <- reactiveVal(NULL)      # "dept" 或 "user"
  org_collapsed <- reactiveVal(integer(0))   # 已折叠的部门 ID 集合

  # 部门色板 —— 从全局彩虹色配置读取
  org_dept_color <- function(dept_id, depts) {
    colors <- config_get_rainbow()
    if (length(colors) == 0) return("#2196F3")
    root_id <- dept_id
    for(.i in 1:10) {
      row <- which(depts$id == root_id)
      if (length(row)==0) break
      pid <- depts$parent_id[row[1]]
      if (is.na(pid) || pid==0) break
      root_id <- pid
    }
    colors[((abs(root_id) - 1) %% length(colors)) + 1]
  }

  # 清理 Mermaid 标签中的特殊字符
  org_clean_label <- function(s) {
    if (is.null(s) || is.na(s)) return("")
    s <- as.character(s)
    s <- gsub('"', "'", s)
    s <- gsub("[\\{\\}\\[\\]\\|\\<\\>]", "", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }

  # 递归人员计数（含子部门）
  org_count_users_recursive <- function(dept_id, depts, users_by_dept) {
    # 当前部门
    cur <- users_by_dept[[as.character(dept_id)]]
    cnt <- if (is.null(cur)) 0 else nrow(cur)
    # 所有子部门
    kids <- depts[!is.na(depts$parent_id) & depts$parent_id == dept_id, , drop = FALSE]
    if (nrow(kids) > 0) {
      for (i in seq_len(nrow(kids))) {
        cnt <- cnt + org_count_users_recursive(kids$id[i], depts, users_by_dept)
      }
    }
    cnt
  }

  # 构建 Mermaid flowchart LR 语法（Xmind 风格）
  # collapsed_ids: 已折叠的部门 ID 集合（不渲染其下级）
  # hide_zero: 是否隐藏无子部门且无人员的"0部门"（默认隐藏）
  org_build_mermaid <- function(depts, users, collapsed_ids = integer(0), search_kw = NULL, hide_zero = TRUE) {
    if (nrow(depts) == 0) return("flowchart LR\n  N_ROOT[LVCC]")

    l1 <- depts[is.na(depts$parent_id), , drop = FALSE]
    if ("sort_order" %in% names(l1)) l1 <- l1[order(l1$sort_order, l1$name), , drop = FALSE]

    # 用户表索引
    users_by_dept <- list()
    if (nrow(users) > 0 && "department_id" %in% names(users)) {
      valid <- !is.na(users$department_id)
      if (any(valid)) users_by_dept <- split(users[valid, , drop = FALSE], users$department_id[valid])
    }

    # 中文字符按 2、英文/数字按 1 计算等宽
    nchar_label <- function(s) {
      s <- as.character(s)
      n <- 0
      for (ch in strsplit(s, "")[[1]]) {
        if (grepl("[A-Za-z0-9 ]", ch)) n <- n + 1 else n <- n + 2
      }
      n
    }

    # 深度 1 的部门需要区分颜色（轮询），不与已有色重复
    color_picker <- new.env()
    assign("used", character(0), envir = color_picker)
    pick_color <- function() {
      used <- get0("used", envir = color_picker, ifnotfound = character(0))
      rainbow <- config_get_rainbow()
      for (c in rainbow) {
        if (!(c %in% used)) {
          assign("used", c(used, c), envir = color_picker)
          return(c)
        }
      }
      # 用完了，循环
      c <- rainbow[(length(used) %% length(rainbow)) + 1]
      assign("used", c(used, c), envir = color_picker)
      c
    }

    lines <- character(0)
    lines <- c(lines, "flowchart LR")
    lines <- c(lines, '  N_ROOT["LVCC"]')
    lines <- c(lines, '  style N_ROOT fill:#337ab7,color:#fff,stroke:#1a5276,stroke-width:2px')

    # 第一遍：先按 sort_order 排序收集所有非零部门（每个深度分别排）
    # 过滤空叶子部门：仅当既无子部门又无人员时隐藏
    is_empty_leaf <- function(dept_id) {
      kids <- depts[!is.na(depts$parent_id) & depts$parent_id == dept_id, , drop = FALSE]
      u <- users_by_dept[[as.character(dept_id)]]
      nrow(kids) == 0 && (is.null(u) || nrow(u) == 0)
    }
    if (FALSE && hide_zero) {  # 暂时禁用：会误杀有意义的顶级部门
      l1 <- l1[!sapply(l1$id, is_empty_leaf), , drop = FALSE]
    }

    # 收集各层节点名用于宽度对齐
    layer_labels <- list()
    collect_layer <- function(parent_id, depth) {
      if (depth == 1) {
        kids <- l1
      } else {
        kids <- depts[!is.na(depts$parent_id) & depts$parent_id == parent_id, , drop = FALSE]
        if ("sort_order" %in% names(kids)) kids <- kids[order(kids$sort_order, kids$name), , drop = FALSE]
        if (hide_zero) kids <- kids[!sapply(kids$id, is_empty_leaf), , drop = FALSE]
      }
      if (nrow(kids) == 0) return()
      if (is.null(layer_labels[[as.character(depth)]])) layer_labels[[as.character(depth)]] <<- character(0)
      for (i in seq_len(nrow(kids))) {
        kd <- kids[i, ]
        nm <- org_clean_label(kd$name)
        if (nchar(nm) == 0) nm <- sprintf("Dept-%d", kd$id)
        # ★ 人数不显示，节点只显示名称
        layer_labels[[as.character(depth)]] <<- c(layer_labels[[as.character(depth)]], nm)
        if (parent_id %in% collapsed_ids) next
        collect_layer(kd$id, depth + 1)
      }
    }
    collect_layer(NA, 1)

    # 每层最宽宽度（用于加 padding 空格统一宽度）
    layer_max_width <- list()
    for (d in names(layer_labels)) {
      ws <- sapply(layer_labels[[d]], nchar_label)
      layer_max_width[[d]] <- max(ws, na.rm = TRUE)
    }
    pad_label <- function(s, depth) {
      maxw <- layer_max_width[[as.character(depth)]]
      if (is.null(maxw)) return(s)
      curw <- nchar_label(s)
      pad_n <- max(0, maxw - curw)
      if (pad_n > 0) {
        paste0(s, paste0(rep(" ", pad_n), collapse=""))
      } else {
        s
      }
    }

    # linkStyle 计数器
    link_env <- new.env()
    assign("idx", 0, envir = link_env)
    link_index <- function() {
      v <- get0("idx", envir = link_env, ifnotfound = 0)
      v
    }
    link_inc <- function() {
      v <- get0("idx", envir = link_env, ifnotfound = 0)
      assign("idx", v + 1, envir = link_env)
    }

    # 递归构建节点
    build_branch <- function(dept_row, parent_id, depth) {
      nid  <- sprintf("D%d", dept_row$id)
      name <- org_clean_label(dept_row$name)
      if (nchar(name) == 0) name <- sprintf("Dept-%d", dept_row$id)

      # ★ 人员计数（暂不显示在节点内，可用于后续显示）
      user_count <- org_count_users_recursive(dept_row$id, depts, users_by_dept)

      # 同层 pad 宽度
      label <- pad_label(name, depth)
      label <- gsub("[\\[\\]\\(\\)\\{\\}\\|\\<\\>]", "", label)

      # 节点颜色：L1 用同深度不重复色板（pick_color 已保证不重复）
      # L2/L3 沿用 L1 颜色但用白底
      if (depth == 1) {
        color <- pick_color()
      } else {
        # 继承父节点色，但用白底（颜色由 build_branch 闭包外传入，这里从父继承）
        color <- attr(dept_row, "color") %||% "#999"
      }

      # 节点样式：L1=实心彩色，L2/L3=白底
      if (depth == 1) {
        lines <<- c(lines, sprintf('  %s["%s"]', nid, label))
        lines <<- c(lines, sprintf('  style %s fill:%s,color:#fff,stroke:%s,stroke-width:2px', nid, color, color))
      } else {
        lines <<- c(lines, sprintf('  %s["%s"]', nid, label))
        lines <<- c(lines, sprintf('  style %s fill:#ffffff,color:#333,stroke:%s,stroke-width:1.5px', nid, color))
      }

      # ★ 连接线：从父节点最右边出去
      # 用 `A --- B`（无箭头）做 L1 连接，L2 起用 `A -->|...| B`（有箭头）
      # 用 Mermaid 10 语法：A -- text --- B
      if (depth == 1) {
        # N_ROOT --- D1（无箭头）
        lines <<- c(lines, sprintf('  %s --- %s', parent_id, nid))
      } else {
        # 父节点 --> 当前（带箭头从最右边出去）
        lines <<- c(lines, sprintf('  %s --> %s', parent_id, nid))
      }
      lines <<- c(lines, sprintf('  linkStyle %d stroke:%s,stroke-width:1.5px',
        link_index(), color))
      link_inc()

      # click 指令
      lines <<- c(lines, sprintf('  click %s orgNodeClick', nid))

      # 折叠态 → 不渲染下级
      if (dept_row$id %in% collapsed_ids) {
        return()
      }

      # 子部门
      kids <- depts[!is.na(depts$parent_id) & depts$parent_id == dept_row$id, , drop = FALSE]
      if (nrow(kids) > 0 && depth < 3) {
        if ("sort_order" %in% names(kids)) kids <- kids[order(kids$sort_order, kids$name), , drop = FALSE]
        if (hide_zero) kids <- kids[!sapply(kids$id, is_empty_leaf), , drop = FALSE]
        for (ki in seq_len(nrow(kids))) {
          attr(kids[ki, ], "color") <- color  # 继承父色
          build_branch(kids[ki, ], nid, depth + 1)
        }
      }
    }

    for (i in seq_len(nrow(l1))) {
      d <- l1[i, ]
      tryCatch(
        build_branch(d, "N_ROOT", 1),
        error = function(e) {}
      )
    }

    paste(lines, collapse = "\n")
  }

  # 搜索词 reactive
  org_search_kw <- reactiveVal("")

  # 思维导图渲染
  output$org_mindmap <- renderUI({
    org_trigger(); req(rv$logged_in)
    depts <- dept_get_all()
    if (nrow(depts) == 0) return(tags$div(style="text-align:center;padding:40px;color:#999;", "暂无部门数据，请先添加部门"))

    users <- user_get_all()
    kw <- org_search_kw()
    collapsed <- org_collapsed()
    mermaid_code <- org_build_mermaid(depts, users, collapsed, kw, hide_zero = FALSE)

    tagList(
      tags$pre(class = "mermaid", id = "org_mermaid", style = "background:transparent; border:none;", mermaid_code),
      tags$script(HTML(sprintf("
        setTimeout(function() {
          if (typeof mermaid !== 'undefined') {
            mermaid.initialize({ startOnLoad: false, securityLevel: 'loose', theme: 'default' });
            mermaid.run({ nodes: document.querySelectorAll('#org_mermaid') }).then(function() {
              // 渲染完成后，绑定每个节点的折叠/展开事件
              var svg = document.querySelector('#org_mindmap_container svg');
              if (svg) {
                // 给 D 前缀的部门节点绑定点击折叠（click callback 已经触发选中）
                // 这里再附加 dblclick 编辑
                svg.querySelectorAll('g.node').forEach(function(g) {
                  if (g.id && g.id.indexOf('flowchart-D') >= 0) {
                    g.style.cursor = 'pointer';
                    g.addEventListener('dblclick', function(e) {
                      var m = g.id.match(/flowchart-D(\\d+)/);
                      if (m) {
                        Shiny.setInputValue('org_dblclick_dept', parseInt(m[1]), {priority:'event'});
                      }
                      e.stopPropagation();
                    });
                  }
                });
              }
            });
          }
        }, 100);
      ")))
    )
  })

  # 思维导图节点点击 → 选中（部门同时切换折叠/展开）
  observeEvent(input$org_mindmap_click, {
    req(rv$logged_in)
    node_id <- input$org_mindmap_click
    if (is.null(node_id) || nchar(node_id) == 0) return()
    prefix <- substr(node_id, 1, 1)
    id_str <- substr(node_id, 2, nchar(node_id))
    id <- suppressWarnings(as.integer(id_str))
    if (is.na(id)) return()
    if (prefix == "U") {
      # 用户节点：只选中，不折叠
      org_selected_type("user")
      org_selected_dept(id)
    } else {
      # 部门节点：选中 + 切换折叠
      org_selected_type("dept")
      org_selected_dept(id)
      cur <- org_collapsed()
      if (is.null(cur)) cur <- integer(0)
      if (id %in% cur) {
        org_collapsed(setdiff(cur, id))
      } else {
        org_collapsed(c(cur, id))
      }
    }
  })

  # 双击部门 → 弹出编辑
  observeEvent(input$org_dblclick_dept, {
    req(rv$logged_in)
    did <- as.integer(input$org_dblclick_dept)
    if (is.na(did)) return()
    org_selected_dept(did)
    org_selected_type("dept")
    # 调用已有的编辑弹窗公共函数
    if (exists("org_show_edit_dept_modal")) {
      org_show_edit_dept_modal(did)
    }
  })

  # 搜索：回车 或 点放大镜
  observeEvent(input$org_search_trigger, {
    kw <- trimws(input$org_search_input)
    org_search_kw(kw)
    if (nchar(kw) > 0) {
      showNotification(sprintf("搜索：%s", kw), type = "message", duration = 2)
    }
  })

  # 清除搜索
  observeEvent(input$org_search_clear_btn, {
    org_search_kw("")
    # 通过 JS 清空输入框
    session$sendCustomMessage("orgClearSearch", list())
  })

  # 选中信息
  output$org_selected_info <- renderUI({
    did <- org_selected_dept()
    if (is.null(did)) return(tags$span(style="color:#999;", "点击思维导图节点查看详情 · 单击展开/折叠 · 双击编辑"))

    stype <- org_selected_type()
    if (!is.null(stype) && stype == "user") {
      users <- user_get_all()
      u <- users[users$id == did, , drop = FALSE]
      if (nrow(u) == 0) return("")
      dl <- u$display_name[1]
      uname <- if (!is.null(dl) && !is.na(dl) && dl != "") dl else u$username[1]
      return(tags$span(
        icon("user"), tags$b(uname),
        sprintf(" · %s", u$role[1]),
        actionButton("org_deselect", "×", class="btn-xs btn-default", style="margin-left:4px; padding:0 6px;")
      ))
    }

    depts <- dept_get_all()
    d <- depts[depts$id == did, , drop = FALSE]
    if (nrow(d) == 0) return("")
    # 递归人员计数（含子部门）
    users <- user_get_all()
    users_by_dept <- list()
    if (nrow(users) > 0 && "department_id" %in% names(users)) {
      valid <- !is.na(users$department_id)
      if (any(valid)) users_by_dept <- split(users[valid, , drop = FALSE], users$department_id[valid])
    }
    ucount <- org_count_users_recursive(did, depts, users_by_dept)
    dept_path <- dept_get_tree()
    dp <- dept_path[dept_path$id == did, , drop = FALSE]
    path_str <- if (nrow(dp) > 0) dp$path[1] else d$name[1]
    collapsed <- !is.null(org_collapsed()) && did %in% org_collapsed()
    tags$span(
      icon("building"), tags$b(path_str),
      sprintf(" · %d人", ucount),
      tags$span(style=sprintf("margin-left:6px; font-size:11px; color:%s;", if(collapsed) "#d9534f" else "#5cb85c"),
        if(collapsed) "[已折叠]" else "[已展开]"),
      actionButton("org_deselect", "×", class="btn-xs btn-default", style="margin-left:4px; padding:0 6px;")
    )
  })

  observeEvent(input$org_deselect, { org_selected_dept(NULL); org_selected_type(NULL) })

  # 全部展开
  observeEvent(input$org_expand_all, {
    org_collapsed(integer(0))
  })

  # 全部折叠：把所有有子部门的部门 ID 加入折叠集合
  observeEvent(input$org_collapse_all, {
    depts <- dept_get_all()
    if (nrow(depts) == 0) return()
    parent_ids <- unique(depts$parent_id[!is.na(depts$parent_id)])
    org_collapsed(as.integer(parent_ids))
  })

  # 人员列表（选中部门则筛选）
  org_users <- reactive({
    org_trigger(); req(rv$logged_in)
    did <- org_selected_dept()
    stype <- org_selected_type()
    if (!is.null(did) && (is.null(stype) || stype == "dept")) return(dept_users(did))
    if (!is.null(did) && stype == "user") {
      users <- user_get_all()
      return(users[users$id == did, , drop = FALSE])
    }
    user_get_all()
  })

  # 添加部门
  observeEvent(input$org_add_dept, {
    req(rv$logged_in)
    depts <- dept_get_all(); dept_choices <- c("(顶级)"="", setNames(as.character(depts$id), depts$name))
    # 默认为选中部门的上级（即选中的部门）
    default_parent <- ""
    sel <- org_selected_dept(); stype <- org_selected_type()
    if (!is.null(sel) && (is.null(stype) || stype == "dept")) default_parent <- as.character(sel)
    showModal(modalDialog(title="添加部门", size="s", easyClose=TRUE,
      textInput("org_new_dept_name", "部门名称 *", placeholder="例如：研发中心"),
      selectizeInput("org_new_dept_parent", "上级部门", choices=dept_choices, selected=default_parent, width="100%"),
      numericInput("org_new_dept_sort", "显示序号", value=0, min=0, max=999, step=1),
      textInput("org_new_dept_desc", "描述"),
      footer=tagList(modalButton("取消"), actionButton("org_add_dept_confirm", "添加", class="btn-success"))))
  })
  observeEvent(input$org_add_dept_confirm, {
    req(rv$logged_in, input$org_new_dept_name)
    pid <- input$org_new_dept_parent; if (is.null(pid)||pid=="") pid <- NA
    result <- dept_add(input$org_new_dept_name, pid, sort_order=input$org_new_dept_sort, description=input$org_new_dept_desc %||% "")
    if (result$success) { removeModal(); org_refresh() }
    showNotification(result$message, type=if(result$success) "message" else "error")
  })

  # 编辑部门
  # 编辑部门弹窗（公共函数，工具栏按钮和 L2 双击共用）
  org_show_edit_dept_modal <- function(did) {
    depts <- dept_get_all(); d <- depts[depts$id == did, ]
    if (nrow(d) == 0) return()
    dept_choices <- c("(顶级)"="", setNames(as.character(depts$id[depts$id!=did]), depts$name[depts$id!=did]))
    showModal(modalDialog(title=paste("编辑部门", d$name[1]), size="s", easyClose=TRUE,
      textInput("org_edit_dept_name", "名称", value=d$name[1]),
      selectizeInput("org_edit_dept_parent", "上级部门", choices=dept_choices,
        selected=as.character(d$parent_id[1] %||% ""), width="100%"),
      numericInput("org_edit_dept_sort", "显示序号", value=d$sort_order[1] %||% 0, min=0, max=999, step=1),
      textInput("org_edit_dept_desc", "描述", value=d$description[1] %||% ""),
      footer=tagList(modalButton("取消"), actionButton("org_edit_dept_confirm", "保存", class="btn-primary"))))
  }

  observeEvent(input$org_edit_dept, {
    req(rv$logged_in, org_selected_dept())
    org_show_edit_dept_modal(org_selected_dept())
  })

  # L2 双击 → 选中并弹出编辑
  observeEvent(input$org_dblclick_l2, {
    req(rv$logged_in)
    did <- as.integer(input$org_dblclick_l2)
    org_selected_dept(did)
    org_show_edit_dept_modal(did)
  })
  observeEvent(input$org_edit_dept_confirm, {
    req(rv$logged_in, org_selected_dept())
    pid <- input$org_edit_dept_parent; if (is.null(pid)||pid=="") pid <- NA
    result <- dept_update(org_selected_dept(), name=input$org_edit_dept_name, parent_id=pid,
      sort_order=input$org_edit_dept_sort, description=input$org_edit_dept_desc)
    if (result$success) { removeModal(); org_refresh() }
    showNotification(result$message, type=if(result$success) "message" else "error")
  })

  # 删除部门
  observeEvent(input$org_del_dept, {
    req(rv$logged_in, org_selected_dept())
    did <- org_selected_dept(); depts <- dept_get_all(); d <- depts[depts$id==did, ]
    if (nrow(d)==0) return()
    showModal(modalDialog(title="确认删除部门",
      tags$div(style="font-size:13px;",
        tags$p(tags$b(sprintf("确定删除部门 [%s] 吗？", d$name[1]))),
        tags$p(style="color:#d9534f; font-size:12px;", "有子部门或人员时，无法删除。")),
      footer=tagList(modalButton("取消"), actionButton("org_del_dept_confirm", "确认删除", class="btn-danger")),
      size="s", easyClose=TRUE))
  })
  observeEvent(input$org_del_dept_confirm, {
    req(rv$logged_in, org_selected_dept())
    result <- dept_delete(org_selected_dept())
    if (result$success) { removeModal(); org_selected_dept(NULL); org_refresh() }
    showNotification(result$message, type=if(result$success) "message" else "error")
  })

  # 添加人员弹窗
  observeEvent(input$org_add_user, {
    req(rv$logged_in)
    did <- org_selected_dept(); depts <- dept_get_all()
    dept_choices <- c("(无)"="", setNames(as.character(depts$id), depts$path))
    showModal(modalDialog(title="添加人员", size="s", easyClose=TRUE,
      textInput("org_new_user_name", "用户名 *"),
      textInput("org_new_user_dn", "显示名称"),
      passwordInput("org_new_user_pw", "密码 *"),
      radioButtons("org_new_user_gender", "性别", choices=c("男"="M","女"="F"), selected="M", inline=TRUE),
      selectInput("org_new_user_role", "角色", choices=c("user","admin","it_desk","it_engineer","sys_engineer")),
      selectizeInput("org_new_user_dept", "所属部门", choices=dept_choices,
        selected=as.character(did %||% ""), width="100%"),
      footer=tagList(modalButton("取消"), actionButton("org_add_user_confirm", "添加", class="btn-primary"))))
  })
  observeEvent(input$org_add_user_confirm, {
    req(rv$logged_in, input$org_new_user_name, input$org_new_user_pw)
    did <- input$org_new_user_dept; if (is.null(did)||did=="") did <- NA
    gender <- input$org_new_user_gender; if (is.null(gender) || gender=="") gender <- "M"
    result <- user_add(input$org_new_user_name, input$org_new_user_pw, input$org_new_user_role,
      input$org_new_user_dn, did, gender, rv$current_user)
    if (result$success) { removeModal(); org_refresh() }
    showNotification(result$message, type=if(result$success) "message" else "error")
  })

  # 移入人员（弹窗选择）
  observeEvent(input$org_move_user, {
    req(rv$logged_in, org_selected_dept())
    all_users <- user_get_all()
    if (nrow(all_users)==0) { showNotification("暂无人员",type="warning"); return() }
    uc <- setNames(as.character(all_users$id), sprintf("%s (%s)", all_users$display_name %||% all_users$username, all_users$username))
    showModal(modalDialog(title="选择要移入的人员", size="m", easyClose=TRUE,
      selectizeInput("org_move_user_sel","人员", choices=uc, multiple=TRUE, width="100%",
        options=list(placeholder="搜索人员...")),
      footer=tagList(modalButton("取消"),
        actionButton("org_move_user_confirm","移入",class="btn-primary"))))
  })
  observeEvent(input$org_move_user_confirm, {
    req(rv$logged_in, org_selected_dept(), input$org_move_user_sel)
    for (uid in input$org_move_user_sel) user_set_department(as.integer(uid), org_selected_dept())
    removeModal(); org_refresh()
    showNotification(sprintf("已移入 %d 人", length(input$org_move_user_sel)), type="message")
  })

  # 移出人员（弹窗选择当前部门人员）
  observeEvent(input$org_remove_user, {
    req(rv$logged_in, org_selected_dept())
    users <- dept_users(org_selected_dept())
    if (nrow(users)==0) { showNotification("该部门暂无人员",type="warning"); return() }
    uc <- setNames(as.character(users$id), sprintf("%s (%s)", users$display_label, users$username))
    showModal(modalDialog(title="选择要移出的人员", size="m", easyClose=TRUE,
      selectizeInput("org_remove_user_sel","人员", choices=uc, multiple=TRUE, width="100%",
        options=list(placeholder="选择人员...")),
      footer=tagList(modalButton("取消"),
        actionButton("org_remove_user_confirm","移出",class="btn-warning"))))
  })
  observeEvent(input$org_remove_user_confirm, {
    req(rv$logged_in, input$org_remove_user_sel)
    for (uid in input$org_remove_user_sel) user_set_department(as.integer(uid), NA)
    removeModal(); org_refresh()
    showNotification(sprintf("已移出 %d 人", length(input$org_remove_user_sel)), type="message")
  })

  # 编辑人员：选中用户时直接编辑，否则弹窗选择
  observeEvent(input$org_edit_user, {
    req(rv$logged_in)
    did <- org_selected_dept(); stype <- org_selected_type()
    if (!is.null(did) && !is.null(stype) && stype == "user") {
      # 直接编辑选中的用户
      rv$org_edit_uid <- did
    } else {
      all_users <- user_get_all()
      if (nrow(all_users)==0) { showNotification("暂无人员",type="warning"); return() }
      dl <- all_users$display_name
      dl[is.na(dl) | dl==""] <- all_users$username
      uc <- setNames(as.character(all_users$id), sprintf("%s [%s] — %s", dl, all_users$username, all_users$department_name %||% "无部门"))
      showModal(modalDialog(title="选择要编辑的人员", size="l", easyClose=TRUE,
        selectizeInput("org_edit_user_sel","人员", choices=uc, width="100%",
          options=list(placeholder="搜索人员...")),
        footer=tagList(modalButton("取消"),
          actionButton("org_edit_user_next","下一步",class="btn-primary"))))
    }
  })
  observeEvent(input$org_edit_user_next, {
    req(input$org_edit_user_sel)
    uid <- as.integer(input$org_edit_user_sel)
    # 关闭选择弹窗，打开编辑弹窗
    removeModal()
    rv$org_edit_uid <- uid
    session$sendCustomMessage("orgTriggerEdit", uid)
  })

  # 编辑人员弹窗（从 org_edit_user 或直接触发）
  org_show_edit_modal <- function(uid) {
    all_users <- user_get_all(); u <- all_users[all_users$id == uid, ]
    if (nrow(u)==0) return()
    depts <- dept_get_all()
    dept_choices <- c("(无)"="", setNames(as.character(depts$id), depts$path))
    has_did <- !is.null(u$department_id) && !is.na(u$department_id)
    cur_gender <- if (!is.null(u$gender) && !is.na(u$gender[1]) && u$gender[1] %in% c("M","F")) u$gender[1] else "M"
    showModal(modalDialog(title="编辑人员", size="s", easyClose=TRUE,
      textInput("org_edit_user_name", "用户名 *", value=u$username[1]),
      textInput("org_edit_user_dn", "显示名称", value=u$display_name[1] %||% ""),
      passwordInput("org_edit_user_pw", "新密码（留空不修改）"),
      radioButtons("org_edit_user_gender", "性别", choices=c("男"="M","女"="F"), selected=cur_gender, inline=TRUE),
      selectInput("org_edit_user_role", "角色",
        choices=c("user","admin","it_desk","it_engineer","sys_engineer"),
        selected=u$role[1]),
      selectizeInput("org_edit_user_dept", "所属部门", choices=dept_choices,
        selected=if(has_did) as.character(u$department_id[1]) else "", width="100%"),
      footer=tagList(modalButton("取消"),
        actionButton("org_edit_user_confirm", "保存", class="btn-primary"))))
  }

  observeEvent(rv$org_edit_uid, {
    if (!is.null(rv$org_edit_uid)) org_show_edit_modal(rv$org_edit_uid)
  })

  observeEvent(input$org_edit_user_confirm, {
    req(rv$logged_in, input$org_edit_user_name)
    uid <- rv$org_edit_uid; rv$org_edit_uid <- NULL
    did <- input$org_edit_user_dept; if (is.null(did)||did=="") did <- NA
    pw <- input$org_edit_user_pw; if (is.null(pw)||pw=="") pw <- NULL
    gender <- input$org_edit_user_gender; if (is.null(gender) || gender=="") gender <- "M"
    result <- user_update(uid, input$org_edit_user_name, input$org_edit_user_role,
      password=pw, display_name=input$org_edit_user_dn, department_id=did, gender=gender, current_user=rv$current_user)
    if (result$success) { removeModal(); org_refresh() }
    showNotification(result$message, type=if(result$success) "message" else "error")
  })

  # 刷新
  observeEvent(input$org_refresh, { org_refresh() })
  
  # 处理配置刷新按钮点击事件
  observeEvent(input$refresh_config, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 更新配置表格输出
    output$config_table <- renderDT({
      DT::datatable(
        config_get_all(),  # 获取所有配置
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })

  # 初始化字体大小输入框的值
  observe({
    req(rv$logged_in)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    if (is_admin) {
      tfs <- config_get_value("table_font_size", "13")
      ifs <- config_get_value("input_font_size", "13")
      updateNumericInput(session, "cfg_table_font_size", value = as.integer(tfs))
      updateNumericInput(session, "cfg_input_font_size", value = as.integer(ifs))
    }
  })

  # 保存字体大小配置
  observeEvent(input$save_font_config, {
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    req(input$cfg_table_font_size, input$cfg_input_font_size)

    con <- db_connect()
    tryCatch({
      # 更新或插入 table_font_size
      existing <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key = 'table_font_size'")
      if (nrow(existing) > 0) {
        dbExecute(con, sprintf("UPDATE system_config SET config_value = '%s', updated_at = CURRENT_TIMESTAMP WHERE config_key = 'table_font_size'",
          as.character(input$cfg_table_font_size)))
      } else {
        dbExecute(con, sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('table_font_size', '%s', '列表表格字体大小(px)')",
          as.character(input$cfg_table_font_size)))
      }
      # 更新或插入 input_font_size
      existing2 <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key = 'input_font_size'")
      if (nrow(existing2) > 0) {
        dbExecute(con, sprintf("UPDATE system_config SET config_value = '%s', updated_at = CURRENT_TIMESTAMP WHERE config_key = 'input_font_size'",
          as.character(input$cfg_input_font_size)))
      } else {
        dbExecute(con, sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('input_font_size', '%s', '输入框和选择框字体大小(px)')",
          as.character(input$cfg_input_font_size)))
      }
      showNotification("字体大小设置已保存，刷新页面后生效", type = "message")
    }, error = function(e) {
      showNotification(paste("保存失败:", e$message), type = "error")
    }, finally = { db_disconnect(con) })
  })

  # ========== 彩虹色配置管理 ==========
  # 色块渲染
  output$cfg_rainbow_swatches <- renderUI({
    colors <- config_get_rainbow()
    lapply(seq_along(colors), function(i) {
      c <- colors[i]
      tags$div(
        title = sprintf("#%d: %s (点击编辑)", i, c),
        style = sprintf(
          "width:32px;height:32px;border-radius:6px;background:%s;cursor:pointer;border:2px solid %s;position:relative;transition:transform 0.15s;",
          c, c
        ),
        onclick = sprintf("Shiny.setInputValue('cfg_rainbow_click_idx', %d, {priority:'event'})", i),
        tags$span(style = paste0(
          "position:absolute;bottom:-14px;left:50%;transform:translateX(-50%);font-size:9px;color:#666;white-space:nowrap;"
        ), i)
      )
    })
  })

  # 单击色块 → 弹出颜色选择器
  observeEvent(input$cfg_rainbow_click_idx, {
    req(rv$logged_in)
    idx <- as.integer(input$cfg_rainbow_click_idx)
    colors <- config_get_rainbow()
    if (idx < 1 || idx > length(colors)) return()
    cur <- colors[idx]
    showModal(modalDialog(
      title = sprintf("编辑彩虹色 #%d", idx),
      size = "s", easyClose = TRUE,
      tags$div(style = "display:flex; align-items:center; gap:12px;",
        tags$input(id = "cfg_rainbow_picker", type = "color", value = cur,
          style = "width:60px; height:40px; border:none; cursor:pointer;"),
        tags$div(style = sprintf("width:80px; height:40px; border-radius:6px; background:%s;", cur),
          textInput("cfg_rainbow_hex", NULL, value = gsub("#","",cur),
            placeholder = "HEX", width = "80px"))
      ),
      tags$p(style = "font-size:11px; color:#888; margin-top:8px;",
        "选色后点保存。也可删除此颜色（需保留至少1个）。"),
      footer = tagList(
        actionButton("cfg_rainbow_del", "删除", class = "btn-danger btn-sm"),
        modalButton("取消"),
        actionButton("cfg_rainbow_save", "保存", class = "btn-primary btn-sm")
      )
    ))
    # 存当前编辑的 idx
    rv$cfg_rainbow_edit_idx <- idx
  })

  # 颜色选择器变化 → 同步 hex 文本框
  observeEvent(input$cfg_rainbow_picker, {
    updateTextInput(session, "cfg_rainbow_hex", value = gsub("#", "", input$cfg_rainbow_picker))
  })

  # 保存编辑的颜色
  observeEvent(input$cfg_rainbow_save, {
    req(rv$logged_in, rv$cfg_rainbow_edit_idx)
    idx <- rv$cfg_rainbow_edit_idx
    colors <- config_get_rainbow()
    if (idx < 1 || idx > length(colors)) { removeModal(); return() }
    hex <- trimws(toupper(input$cfg_rainbow_hex))
    if (!grepl("^[0-9A-F]{6}$", hex)) {
      showNotification("无效颜色值，需要 6 位 HEX", type = "error"); return()
    }
    colors[idx] <- paste0("#", hex)
    r <- config_set_rainbow(colors)
    removeModal()
    if (r$success) {
      showNotification(sprintf("#%d 颜色已更新 → %s", idx, colors[idx]), type = "message")
    } else {
      showNotification(r$message, type = "error")
    }
  })

  # 删除颜色
  observeEvent(input$cfg_rainbow_del, {
    req(rv$logged_in, rv$cfg_rainbow_edit_idx)
    idx <- rv$cfg_rainbow_edit_idx
    colors <- config_get_rainbow()
    if (length(colors) <= 1) {
      showNotification("至少保留 1 个颜色", type = "warning"); return()
    }
    colors <- colors[-idx]
    r <- config_set_rainbow(colors)
    removeModal()
    if (r$success) {
      showNotification(sprintf("已删除 #%d，剩余 %d 色", idx, length(colors)), type = "message")
    } else {
      showNotification(r$message, type = "error")
    }
  })

  # 追加新颜色
  observeEvent(input$cfg_rainbow_add, {
    req(rv$logged_in)
    colors <- config_get_rainbow()
    showModal(modalDialog(
      title = "追加彩虹色",
      size = "s", easyClose = TRUE,
      tags$div(style = "display:flex; align-items:center; gap:12px;",
        tags$input(id = "cfg_rainbow_picker", type = "color", value = "#2196F3",
          style = "width:60px; height:40px; border:none; cursor:pointer;"),
        textInput("cfg_rainbow_hex", NULL, value = "2196F3", placeholder = "HEX", width = "80px")
      ),
      footer = tagList(
        modalButton("取消"),
        actionButton("cfg_rainbow_add_confirm", "追加", class = "btn-success btn-sm")
      )
    ))
  })
  observeEvent(input$cfg_rainbow_add_confirm, {
    req(rv$logged_in)
    hex <- trimws(toupper(input$cfg_rainbow_hex))
    if (!grepl("^[0-9A-F]{6}$", hex)) {
      showNotification("无效颜色值", type = "error"); return()
    }
    colors <- config_get_rainbow()
    colors <- c(colors, paste0("#", hex))
    r <- config_set_rainbow(colors)
    removeModal()
    if (r$success) {
      showNotification(sprintf("已追加 → 共 %d 色", length(colors)), type = "message")
    } else {
      showNotification(r$message, type = "error")
    }
  })

  # 恢复默认
  observeEvent(input$cfg_rainbow_reset, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "恢复默认彩虹色", size = "s",
      "将重置为 20 个默认颜色，覆盖当前自定义。",
      footer = tagList(
        modalButton("取消"),
        actionButton("cfg_rainbow_reset_confirm", "确认重置", class = "btn-warning btn-sm")
      )
    ))
  })
  observeEvent(input$cfg_rainbow_reset_confirm, {
    config_set_rainbow(.RAINBOW_DEFAULT)
    removeModal()
    showNotification("彩虹色已恢复默认 20 色", type = "message")
  })

  # 处理添加配置按钮点击事件
  observe({ toggle_btn("add_config", btn_ok(input$config_key) && btn_ok(input$config_value)) })
  observe({ toggle_btn("save_font_config", btn_ok(input$cfg_table_font_size) && btn_ok(input$cfg_input_font_size)) })
  observeEvent(input$add_config, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 确保必要输入存在
    req(input$config_key, input$config_value)
    # 调用config_add函数添加配置
    result <- config_add(input$config_key, input$config_value, input$config_desc)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新配置表格
    output$config_table <- renderDT({
      DT::datatable(
        config_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 初始渲染配置表格
  output$config_table <- renderDT({
    DT::datatable(
      config_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 处理GitHub自动提交按钮点击事件
  observeEvent(input$github_autosubmit, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 确保提交信息存在
    req(input$commit_message)
    # 调用github_autosubmit函数执行提交操作
    output$github_output <- renderPrint({
      github_autosubmit(input$commit_message)
    })
    # 显示操作结果通知
    showNotification("代码已提交到 GitHub", type = "message")
  })
  
  # 处理查看Git状态按钮点击事件
  observeEvent(input$github_check_status, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 调用github_check_status函数查看Git状态
    output$github_output <- renderPrint({
      github_check_status()
    })
  })
  
  # 处理拉取GitHub代码按钮点击事件
  observeEvent(input$github_pull, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 调用github_pull函数拉取代码
    output$github_output <- renderPrint({
      github_pull()
    })
    # 显示操作结果通知
    showNotification("代码已从 GitHub 拉取", type = "message")
  })
  
  # ========== RBAC 授权管理（admin专用） ==========
  source("Script/rbac_management.r")
  rbac_refresh <- reactiveVal(0)
  rbac_u_trigger <- reactiveVal(0)
  rbac_u_dept_filter <- reactiveVal(NULL)

  ##################
  # 用户管理 Tab
  ##################
  # 递归获取部门及所有子部门的人员总数
  .rbac_u_count_recursive <- function(dept_id) {
    ids <- dept_get_descendant_ids(dept_id)
    if (length(ids) == 0) return(0)
    con <- db_connect()
    tryCatch({
      dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM users WHERE department_id IN (%s)",
        paste(as.integer(ids), collapse=",")))$n[1]
    }, error=function(e)0, finally={db_disconnect(con)})
  }

  # 部门筛选（selectInput 层级下拉，替代旧版自定义 HTML 树）
  output$rbac_u_dept_tree <- renderUI({
    rbac_u_trigger(); req(rv$logged_in)
    depts <- dept_get_all()
    if (nrow(depts) == 0) return(tags$div(style="color:#999;font-size:13px;padding:8px;", "暂无部门"))

    l1 <- depts[is.na(depts$parent_id), , drop = FALSE]
    if ("sort_order" %in% names(l1)) l1 <- l1[order(l1$sort_order, l1$name), , drop = FALSE]

    all_count <- nrow(user_get_all())
    sel <- isolate(rbac_u_dept_filter())
    selected <- if (is.null(sel)) "-1" else as.character(sel)

    # 构建层级 optgroup 选项
    choices <- list()
    choices[["— 全部部门"]] <- "-1"

    for (i in seq_len(nrow(l1))) {
      d <- l1[i, ]
      kids <- depts[!is.na(depts$parent_id) & depts$parent_id == d$id, , drop = FALSE]
      if ("sort_order" %in% names(kids)) kids <- kids[order(kids$sort_order, kids$name), , drop = FALSE]

      rec_count <- if (nrow(kids) > 0) .rbac_u_count_recursive(d$id) else nrow(tryCatch(dept_users(d$id), error=function(e)data.frame()))
      group_name <- sprintf("%s（%d人）", d$name, rec_count)

      if (nrow(kids) > 0) {
        sub <- list()
        sub[[sprintf("  [全部] %s", d$name)]] <- as.character(d$id)
        for (j in seq_len(nrow(kids))) {
          k <- kids[j, ]
          kcount <- nrow(tryCatch(dept_users(k$id), error=function(e)data.frame()))
          sub[[sprintf("  ├ %s（%d人）", k$name, kcount)]] <- as.character(k$id)
        }
        choices[[group_name]] <- sub
      } else {
        choices[[group_name]] <- as.character(d$id)
      }
    }

    selectInput("rbac_u_dept_sel", NULL, choices = choices, selected = selected, width = "100%")
  })

  # 部门选择变更 → 筛选用户列表
  observeEvent(input$rbac_u_dept_sel, {
    val <- input$rbac_u_dept_sel
    if (is.null(val) || val == "" || val == "-1") {
      rbac_u_dept_filter(NULL)
    } else {
      rbac_u_dept_filter(as.integer(val))
    }
  }, ignoreInit = TRUE)

  # 用户数据（含递归部门筛选）
  rbac_u_data <- reactive({
    rbac_u_trigger(); rbac_refresh(); req(rv$logged_in)
    users <- user_get_all()
    # 部门筛选（一级包含子部门）
    did <- rbac_u_dept_filter()
    if (!is.null(did)) {
      ids <- dept_get_descendant_ids(did)
      users <- users[!is.na(users$department_id) & users$department_id %in% ids, , drop = FALSE]
    }
    # 角色
    rf <- input$rbac_u_filter_role
    if (length(rf) > 0 && rf != "") users <- users[users$role == rf, , drop = FALSE]
    # 搜索
    kw <- trimws(input$rbac_u_search)
    if (length(kw) > 0 && kw != "") {
      users <- users[grepl(tolower(kw), tolower(paste(users$username, users$display_name))), , drop = FALSE]
    }
    users
  })

  # 进入用户管理 Tab 时强制清空筛选
  observeEvent(input$rbac_u_refresh, {
    rbac_u_dept_filter(NULL)
    updateSelectizeInput(session, "rbac_u_filter_role", selected = "")
    updateTextInput(session, "rbac_u_search", value = "")
    rbac_u_trigger(rbac_u_trigger() + 1)
  })

  output$rbac_u_table <- DT::renderDT({
    users <- rbac_u_data()
    if (nrow(users) == 0) return(DT::datatable(data.frame(提示="无匹配用户"), options=list(dom="t")))
    disp <- data.frame(
      ID = users$id,
      用户名 = users$username,
      显示名 = users$display_name %||% "",
      角色 = users$role,
      部门 = users$department_name %||% "—",
      状态 = sapply(users$active, function(a) if(a==1) '<span style="color:#5cb85c;">启用</span>' else '<span style="color:#d9534f;">禁用</span>'),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    DT::datatable(disp, escape = FALSE, rownames = FALSE, selection = "single",
      options = list(pageLength = 25, dom = "ltip"))
  })

  # 添加用户
  observeEvent(input$rbac_u_add, {
    req(rv$logged_in)
    depts <- dept_get_all()
    dc <- c("(无)"="", setNames(as.character(depts$id), depts$path))
    showModal(modalDialog(title="添加用户", size="s", easyClose=TRUE,
      textInput("rbac_u_new_uname", "用户名 *"),
      textInput("rbac_u_new_dname", "显示名称"),
      passwordInput("rbac_u_new_pw", "密码 *"),
      radioButtons("rbac_u_new_gender", "性别", choices=c("男"="M","女"="F"), selected="M", inline=TRUE),
      selectInput("rbac_u_new_role", "角色", choices=c("user","admin","it_desk","it_engineer","sys_engineer")),
      selectizeInput("rbac_u_new_dept", "所属部门", choices=dc, width="100%"),
      footer=tagList(modalButton("取消"), actionButton("rbac_u_add_confirm","添加",class="btn-primary"))))
  })
  observeEvent(input$rbac_u_add_confirm, {
    req(rv$logged_in, input$rbac_u_new_uname, input$rbac_u_new_pw)
    did <- input$rbac_u_new_dept; if(is.null(did)||did=="") did <- NA
    gender <- input$rbac_u_new_gender; if(is.null(gender)||gender=="") gender <- "M"
    result <- user_add(input$rbac_u_new_uname, input$rbac_u_new_pw, input$rbac_u_new_role, input$rbac_u_new_dname, did, gender, rv$current_user)
    if(result$success) { removeModal(); rbac_u_trigger(rbac_u_trigger()+1) }
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 编辑用户
  observeEvent(input$rbac_u_edit, {
    req(rv$logged_in)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) { showNotification("请先选择用户", type="warning"); return() }
    users <- rbac_u_data(); u <- users[sel, ]
    depts <- dept_get_all()
    dc <- c("(无)"="", setNames(as.character(depts$id), depts$path))
    has_did <- !is.null(u$department_id) && !is.na(u$department_id)
    showModal(modalDialog(title="编辑用户", size="s", easyClose=TRUE,
      textInput("rbac_u_edit_uname", "用户名 *", value=u$username[1]),
      textInput("rbac_u_edit_dname", "显示名称", value=u$display_name[1] %||% ""),
      passwordInput("rbac_u_edit_pw", "新密码（留空不修改）"),
      radioButtons("rbac_u_edit_gender", "性别", choices=c("男"="M","女"="F"),
        selected=if(!is.null(u$gender)&&!is.na(u$gender[1])&&u$gender[1]%in%c("M","F")) u$gender[1] else "M", inline=TRUE),
      selectInput("rbac_u_edit_role", "角色", choices=c("user","admin","it_desk","it_engineer","sys_engineer"), selected=u$role[1]),
      selectizeInput("rbac_u_edit_dept", "所属部门", choices=dc,
        selected=if(has_did) as.character(u$department_id[1]) else "", width="100%"),
      footer=tagList(modalButton("取消"), actionButton("rbac_u_edit_confirm","保存",class="btn-primary"))))
  })
  observeEvent(input$rbac_u_edit_confirm, {
    req(rv$logged_in, input$rbac_u_edit_uname)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) return()
    users <- rbac_u_data(); uid <- users$id[sel]
    did <- input$rbac_u_edit_dept; if(is.null(did)||did=="") did <- NA
    pw <- input$rbac_u_edit_pw; if(is.null(pw)||pw=="") pw <- NULL
    gender <- input$rbac_u_edit_gender; if(is.null(gender)||gender=="") gender <- "M"
    result <- user_update(uid, input$rbac_u_edit_uname, input$rbac_u_edit_role, password=pw, display_name=input$rbac_u_edit_dname, department_id=did, gender=gender, current_user=rv$current_user)
    if(result$success) { removeModal(); rbac_u_trigger(rbac_u_trigger()+1) }
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 删除用户
  observeEvent(input$rbac_u_del, {
    req(rv$logged_in)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) { showNotification("请先选择用户", type="warning"); return() }
    users <- rbac_u_data(); u <- users[sel, ]
    showModal(modalDialog(title="确认删除用户",
      tags$div(style="font-size:13px;",
        tags$p(tags$b(sprintf("确定删除用户 [%s] 吗？", u$username[1]))),
        tags$p(style="color:#d9534f;font-size:12px;","此操作不可恢复。")),
      footer=tagList(modalButton("取消"), actionButton("rbac_u_del_confirm","确认删除",class="btn-danger")),
      size="s", easyClose=TRUE))
  })
  observeEvent(input$rbac_u_del_confirm, {
    req(rv$logged_in)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) return()
    users <- rbac_u_data(); uid <- users$id[sel]
    result <- user_delete(uid, rv$current_user)
    removeModal(); rbac_u_trigger(rbac_u_trigger()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 初始化密码
  observeEvent(input$rbac_u_rpw, {
    req(rv$logged_in)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) { showNotification("请先选择用户", type="warning"); return() }
    users <- rbac_u_data(); u <- users[sel, ]
    showModal(modalDialog(title="初始化密码",
      tags$p(sprintf("将 [%s] 的密码重置为默认密码 '123456'？", u$username[1])),
      tags$p(style="color:#d9534f;font-size:12px;","建议用户登录后立即修改密码。"),
      footer=tagList(modalButton("取消"), actionButton("rbac_u_rpw_confirm","确认重置",class="btn-warning")),
      size="s", easyClose=TRUE))
  })
  observeEvent(input$rbac_u_rpw_confirm, {
    req(rv$logged_in)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) return()
    users <- rbac_u_data(); uid <- users$id[sel]
    result <- user_update(uid, users$username[sel], users$role[sel], password="123456", current_user=rv$current_user)
    removeModal(); rbac_u_trigger(rbac_u_trigger()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 禁用/启用
  observeEvent(input$rbac_u_act, {
    req(rv$logged_in)
    sel <- input$rbac_u_table_rows_selected
    if(length(sel)==0) { showNotification("请先选择用户", type="warning"); return() }
    users <- rbac_u_data(); u <- users[sel, ]
    result <- user_toggle_active(u$id[1], rv$current_user)
    rbac_u_trigger(rbac_u_trigger()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })


  # 权限清单：三级可折叠模块→部件→操作
  output$rbac_perm_table <- renderUI({
    rbac_refresh(); req(rv$logged_in)
    perms <- rbac_permission_get_all()
    # 处理空 component（未归类的权限归入"基础"）
    perms$component <- ifelse(is.na(perms$component) | perms$component == "", "(通用)", perms$component)
    # 按实际导航栏顺序排列模块
    nav_order <- c("首页","项目","巡检","工单","资产","记事","标准化","测试",
                   "性能","日报","收集器","集成","数据","岗职","绩效","模型","可视化","管理")
    modules <- intersect(nav_order, unique(perms$module))
    tagList(
      tags$style(HTML("
        .rbac-mod-hdr { background:#e8f0fe; padding:6px 12px; margin:6px 0 2px; cursor:pointer; border-radius:4px; font-weight:700; font-size:14px; user-select:none; border:1px solid #c8daf5; }
        .rbac-mod-hdr:hover { background:#d4e4fc; }
        .rbac-comp-hdr { background:#f5f5f5; padding:4px 10px; margin:2px 0; cursor:pointer; border-radius:3px; font-weight:600; font-size:14px; user-select:none; border:1px solid #e0e0e0; }
        .rbac-comp-hdr:hover { background:#e8e8e8; }
        .rbac-ops { display:flex; flex-wrap:wrap; gap:4px; padding:2px 0 2px 24px; }
        .rbac-op { background:#fff; border:1px solid #ddd; border-radius:3px; padding:3px 10px; font-size:13px; font-family:monospace; }
      ")),
      lapply(seq_along(modules), function(mi) {
        mod <- modules[mi]
        mod_perms <- perms[perms$module == mod, ]
        components <- unique(mod_perms$component)
        tags$div(
          tags$div(class = "rbac-mod-hdr", onclick = sprintf(
            "var el=document.getElementById('rbac-mod-m%d'); el.style.display=el.style.display==='none'?'block':'none';", mi
          ), paste0("📁 ", mod, " (", nrow(mod_perms), "项)")),
          tags$div(id = paste0("rbac-mod-m", mi), style = "display:block; padding-left:12px;",
            lapply(seq_along(components), function(ci) {
              comp <- components[ci]
              comp_perms <- mod_perms[mod_perms$component == comp, ]
              comp_id <- paste0("m", mi, "c", ci)
              tags$div(
                tags$div(class = "rbac-comp-hdr", onclick = sprintf(
                  "var el=document.getElementById('rbac-comp-%s'); el.style.display=el.style.display==='none'?'flex':'none';", comp_id
                ), paste0("▸ ", comp, " (", nrow(comp_perms), ")")),
                tags$div(id = paste0("rbac-comp-", comp_id), class = "rbac-ops", style = "display:flex;",
                  lapply(seq_len(nrow(comp_perms)), function(i) {
                    tags$span(class = "rbac-op",
                      paste0(comp_perms$name[i], " [", comp_perms$code[i], "]"))
                  })
                )
              )
            })
          )
        )
      })
    )
  })

  # 角色列表（带刷新，多选支持批量删除）
  output$rbac_role_table <- DT::renderDT({
    rbac_refresh(); req(rv$logged_in)
    roles <- rbac_role_get_all()
    DT::datatable(roles, rownames = FALSE, selection = "multiple",
      colnames = c("ID", "角色名称", "描述"),
      options = list(pageLength = 10, dom = 't'))
  })

  selected_role_id <- reactiveVal(NULL)
  selected_role_ids <- reactiveVal(integer(0))  # 多选角色 ID 列表
  rbac_role_perms_initial <- reactiveVal(character(0))

  observeEvent(input$rbac_role_table_rows_selected, {
    roles <- rbac_role_get_all()
    sel_rows <- input$rbac_role_table_rows_selected
    if (length(sel_rows) > 0) {
      sids <- roles$id[sel_rows]
      selected_role_ids(as.integer(sids))
      # 单选时更新权限面板
      if (length(sel_rows) == 1) {
        selected_role_id(sids[1])
        rbac_role_perms_initial(as.character(rbac_role_perms_get(sids[1])))
      } else {
        selected_role_id(NULL)
      }
    } else {
      selected_role_ids(integer(0))
      selected_role_id(NULL)
    }
  })

  # 角色权限编辑：三级模块→部件→操作可折叠勾选框
  output$rbac_role_perms_ui <- renderUI({
    req(selected_role_id())
    roles <- rbac_role_get_all()
    role <- roles[roles$id == selected_role_id(), ]
    current_perms <- rbac_role_perms_get(selected_role_id())
    all_perms <- rbac_permission_get_all()
    all_perms$component <- ifelse(is.na(all_perms$component) | all_perms$component == "", "(通用)", all_perms$component)
    # 按实际导航栏顺序排列模块
    navbar_order <- c("首页","项目","巡检","工单","资产","记事","标准化","测试",
                      "性能","日报","收集器","集成","数据","岗职","绩效","模型","可视化","管理")
    modules <- intersect(navbar_order, unique(all_perms$module))

    tagList(
      h5(paste("为角色 [", role$name[1], "] 配置权限")),
      # ★ 成员列表
      {
        role_users <- rbac_role_get_users(selected_role_id())
        if (nrow(role_users) > 0) {
          tags$div(style = "background:#f0faf5; border:1px solid #c3e6cb; border-radius:6px; padding:8px 12px; margin-bottom:10px;",
            tags$div(style = "font-size:12px; color:#155724; font-weight:600; margin-bottom:4px;",
              paste0("👥 成员 (", nrow(role_users), "人)")),
            tags$div(style = "display:flex; flex-wrap:wrap; gap:4px;",
              lapply(seq_len(nrow(role_users)), function(i) {
                u <- role_users[i, ]
                tags$span(style = "font-size:11px; background:#d4edda; color:#155724; padding:2px 8px; border-radius:10px; white-space:nowrap;",
                  u$display_name[1] %||% u$username[1])
              })
            )
          )
        } else {
          tags$div(style = "background:#fdfdff; border:1px dashed #ccc; border-radius:6px; padding:8px 12px; margin-bottom:10px;",
            tags$span(style = "font-size:12px; color:#999;", "👤 暂无成员"))
        }
      },
      # 快速模式：selectInput 搜索
      wellPanel(
        tags$b("快速搜索添加："),
        selectInput("rbac_role_perms_select", NULL,
          choices = stats::setNames(all_perms$code, sprintf("[%s/%s] %s", all_perms$module, all_perms$component, all_perms$name)),
          selected = current_perms, multiple = TRUE, width = "100%", selectize = TRUE),
        tags$p(style = "color:#999;font-size:10px;", "输入关键字搜索权限，支持多选（勾选=有此权限）")
      ),
      # 树形勾选模式 + 保存按钮同行
      div(style = "display:flex; align-items:center; gap:10px; margin-bottom:6px;",
        h5("按模块/部件勾选：", style="margin:0;"),
        tags$button(id="rbac_save_perms", type="button", class="btn btn-success action-button", disabled=NA, list(icon("save"), "保存权限"))
      ),
      # JS同步checkbox到selectInput（使用命名空间避免冲突）
      tags$script(HTML('
        $(document).off("change.rbac").on("change.rbac", "input.rp-tree-cb", function() {
          var vals = [];
          $("input.rp-tree-cb:checked").each(function(){ vals.push($(this).val()); });
          var sel = $("#rbac_role_perms_select")[0].selectize;
          if (sel) { sel.setValue(vals); }
          // 备份：显式通知Shiny确保observer触发
          Shiny.setInputValue("rbac_role_perms_select", vals, {priority: "event"});
        });
      ')),
      tags$style(HTML("
        .rp-mod-hdr { background:#e8f0fe; padding:6px 12px; margin:4px 0 0; cursor:pointer; border-radius:4px; font-weight:700; font-size:14px; user-select:none; border:1px solid #c8daf5; display:flex; align-items:center; gap:8px; }
        .rp-mod-hdr:hover { background:#d4e4fc; }
        .rp-op-row { display:flex; flex-wrap:wrap; gap:4px; padding:4px 4px 4px 24px; }
      ")),
      lapply(seq_along(modules), function(mi) {
        mod <- modules[mi]
        mod_perms <- all_perms[all_perms$module == mod, ]
        mod_idx <- mi
        mod_all_checked <- all(mod_perms$code %in% current_perms)
        tags$div(style = "margin-bottom:4px; border:1px solid #d0d0d0; border-radius:4px; overflow:hidden;",
          tags$div(class = "rp-mod-hdr",
            tags$input(type = "checkbox",
              class = paste0("rp-mod-cb-", mod_idx),
              onclick = sprintf("
                var self=this, cbs=document.querySelectorAll('.rp-cb-m%d');
                for(var i=0;i<cbs.length;i++){cbs[i].checked=self.checked;cbs[i].dispatchEvent(new Event('change',{bubbles:true}));}
              ", mod_idx),
              checked = if(mod_all_checked) NA else NULL),
            tags$span(onclick = sprintf(
              "var el=document.getElementById('rp-mod-body-m%d'); el.style.display=el.style.display==='none'?'block':'none';", mod_idx
            ), style = "flex:1;", paste0("📁 ", mod, " (", nrow(mod_perms), "项)"))
          ),
          tags$div(id = paste0("rp-mod-body-m", mod_idx), class = "rp-op-row", style = "display:flex;",
            lapply(seq_len(nrow(mod_perms)), function(i) {
              p <- mod_perms[i, ]
              tags$label(style = "font-size:13px; white-space:nowrap;",
                tags$input(type = "checkbox",
                  class = paste0("rp-cb-m", mod_idx, " rp-tree-cb"),
                  name = "rbac_perm_codes", value = p$code,
                  checked = if(p$code %in% current_perms) NA else NULL),
                p$name)
            })
          )
        )
      })
    )
  })

  # 保存角色权限（同时处理select和多选框）
  observeEvent(input$rbac_save_perms, {
    req(rv$logged_in, selected_role_id())
    perms <- input$rbac_role_perms_select
    if (is.null(perms)) perms <- character(0)
    result <- rbac_role_perms_set(selected_role_id(), perms)
    if (result$success) {
      rbac_role_perms_initial(as.character(perms))  # 更新初始状态
      rbac_refresh(rbac_refresh() + 1)
    }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # ── 角色按钮启用控制 ──
  observe({ toggle_btn("rbac_add_role", btn_ok(input$rbac_new_role_name)) })
  observe({ toggle_btn("rbac_edit_role", length(selected_role_ids()) == 1) })
  observe({ toggle_btn("rbac_delete_roles", length(selected_role_ids()) > 0) })
  # 保存权限：初始灰色，权限变更后才启用
  observe({
    cur <- input$rbac_role_perms_select
    if (is.null(cur)) cur <- character(0)
    init <- rbac_role_perms_initial()
    changed <- !setequal(as.character(cur), as.character(init))
    toggle_btn("rbac_save_perms", changed)
  })

  # ── 添加角色 ──
  observeEvent(input$rbac_add_role, {
    req(rv$logged_in, input$rbac_new_role_name)
    desc <- trimws(input$rbac_new_role_desc %||% "")
    result <- rbac_role_add(input$rbac_new_role_name, desc)
    if (result$success) {
      updateTextInput(session, "rbac_new_role_name", value = "")
      updateTextInput(session, "rbac_new_role_desc", value = "")
      rbac_refresh(rbac_refresh() + 1)
    }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # ── 编辑角色 ──
  observeEvent(input$rbac_edit_role, {
    req(rv$logged_in, length(selected_role_ids()) == 1)
    rid <- selected_role_ids()[1]
    roles <- rbac_role_get_all()
    role <- roles[roles$id == rid, ]
    if (nrow(role) == 0) return()
    showModal(modalDialog(
      title = paste("编辑角色 —", role$name[1]),
      textInput("rbac_edit_role_name", "角色名称", value = role$name[1]),
      textInput("rbac_edit_role_desc", "描述", value = role$description[1] %||% ""),
      footer = tagList(
        modalButton("取消"),
        actionButton("rbac_edit_role_save", "保存", class = "btn-primary")
      ), size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$rbac_edit_role_save, {
    req(rv$logged_in, input$rbac_edit_role_name, length(selected_role_ids()) == 1)
    rid <- selected_role_ids()[1]
    desc <- trimws(input$rbac_edit_role_desc %||% "")
    result <- rbac_role_update(rid, input$rbac_edit_role_name, desc)
    if (result$success) {
      removeModal()
      rbac_refresh(rbac_refresh() + 1)
    }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # ── 批量删除角色（含确认弹窗） ──
  observeEvent(input$rbac_delete_roles, {
    req(rv$logged_in, length(selected_role_ids()) > 0)
    sids <- selected_role_ids()
    details <- lapply(sids, function(rid) rbac_role_get_detail(rid))
    has_blocked <- FALSE
    rows_html <- ""
    for (d in details) {
      if (is.null(d)) next
      r <- d$role
      blocked <- d$user_count > 0
      if (blocked) has_blocked <- TRUE
      # 有用户→橙红警告 / 无用户→绿色可删
      if (blocked) {
        row_style <- "background:#fff0f0; border-left:4px solid #d9534f;"
        user_style <- "color:#d9534f;font-weight:bold;font-size:13px;"
        badge_html  <- '<span style="background:#d9534f;color:#fff;padding:1px 8px;border-radius:10px;font-size:10px;font-weight:bold;">不可删</span>'
      } else {
        row_style <- "background:#f0faf0; border-left:4px solid #5cb85c;"
        user_style <- ""
        badge_html  <- '<span style="background:#5cb85c;color:#fff;padding:1px 8px;border-radius:10px;font-size:10px;font-weight:bold;">可删除</span>'
      }
      rows_html <- paste0(rows_html, sprintf(
        '<tr style="%s">
          <td>%d</td><td><b>%s</b>  %s</td><td>%s</td>
          <td style="text-align:center;">%d</td>
          <td style="text-align:center;%s">%d</td>
        </tr>',
        row_style, r$id[1], r$name[1], badge_html, r$description[1] %||% "—",
        d$perm_count, user_style, d$user_count
      ))
    }
    table_html <- sprintf(
      '<table class="table table-bordered table-sm" style="font-size:12px;">
        <thead><tr><th>ID</th><th>名称</th><th>描述</th><th style="text-align:center;">权限数</th><th style="text-align:center;">用户数</th></tr></thead>
        <tbody>%s</tbody></table>', rows_html)
    footer_btns <- tagList(
      modalButton("取消"),
      if (!has_blocked) actionButton("rbac_delete_roles_confirm", "确认删除", class = "btn-danger")
    )
    showModal(modalDialog(
      title = sprintf("确认批量删除 %d 个角色", length(sids)),
      HTML(paste0(
        if (has_blocked) '<div class="alert alert-danger" style="font-size:12px;margin-bottom:12px;padding:8px 12px;">⛔ 红色标记角色下仍有用户，<b>无法删除</b>。请先在用户授权中移除关联后重试。</div>' else '<div class="alert alert-success" style="font-size:12px;margin-bottom:12px;padding:8px 12px;">✅ 所选角色均无用户关联，可安全删除。</div>',
        '<p style="font-size:13px;margin-bottom:4px;color:#666;">图例：<span style="background:#f0faf0;border:1px solid #5cb85c;padding:2px 8px;border-radius:4px;">🟢 可删除</span> <span style="background:#fff0f0;border:1px solid #d9534f;padding:2px 8px;border-radius:4px;margin-left:6px;">🔴 不可删</span></p>',
        table_html
      )),
      footer = footer_btns,
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input$rbac_delete_roles_confirm, {
    req(rv$logged_in, length(selected_role_ids()) > 0)
    result <- rbac_role_batch_delete(selected_role_ids())
    removeModal()
    if (result$success) {
      selected_role_ids(integer(0))
      selected_role_id(NULL)
      rbac_refresh(rbac_refresh() + 1)
    }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 用户列表
  output$rbac_user_table <- DT::renderDT({
    req(rv$logged_in)
    users <- rbac_user_get_all()
    DT::datatable(users[, c("id","username","display_name","role")], rownames = FALSE, selection = "single",
      colnames = c("ID","用户名","显示名","原角色"),
      options = list(pageLength = 25, dom = 't'))
  })

  selected_user_id <- reactiveVal(NULL)
  rbac_user_roles_initial <- reactiveVal(character(0))  # 记录初始角色勾选
  
  observeEvent(input$rbac_user_table_rows_selected, {
    users <- rbac_user_get_all()
    if (length(input$rbac_user_table_rows_selected) > 0) {
      uid <- users$id[input$rbac_user_table_rows_selected]
      selected_user_id(uid)
      # 记录初始角色状态
      rbac_user_roles_initial(as.character(rbac_user_roles_get(uid)))
    }
  })

  # 用户角色分配
  output$rbac_user_roles_ui <- renderUI({
    req(selected_user_id())
    all_roles <- rbac_role_get_all()
    current_roles <- rbac_user_roles_get(selected_user_id())
    tagList(
      checkboxGroupInput("rbac_user_roles_cb", "分配角色",
        choices = stats::setNames(as.character(all_roles$id), all_roles$name),
        selected = as.character(current_roles)),
      tags$p(style = "color:#999;font-size:11px;", "用户可拥有多个角色，权限叠加")
    )
  })
  
  # 保存角色按钮：初始灰色，勾选项变更后才启用
  observe({
    cur <- input$rbac_user_roles_cb
    if (is.null(cur)) cur <- character(0)
    init <- rbac_user_roles_initial()
    changed <- !setequal(as.character(cur), as.character(init))
    toggle_btn("rbac_save_user_roles", changed)
  })

  observeEvent(input$rbac_save_user_roles, {
    req(rv$logged_in, selected_user_id())
    role_ids <- input$rbac_user_roles_cb
    if (is.null(role_ids)) role_ids <- character(0)
    result <- rbac_user_roles_set(selected_user_id(), role_ids)
    if (result$success) {
      # 更新初始状态，按钮变回灰色
      rbac_user_roles_initial(as.character(role_ids))
    }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # ========== 个人信息模块（所有用户可见） ==========
  # 渲染个人信息
  output$self_info_username <- renderText({
    req(rv$logged_in, rv$current_user)
    rv$current_user$username[1]
  })
  output$self_info_display_name <- renderText({
    req(rv$logged_in, rv$current_user)
    if (!is.null(rv$current_user$display_name) && !is.na(rv$current_user$display_name[1]) && rv$current_user$display_name[1] != "")
      rv$current_user$display_name[1] else "-"
  })
  output$self_info_role <- renderText({
    req(rv$logged_in, rv$current_user)
    switch(rv$current_user$role[1],
      admin = "管理员",
      it_desk = "IT服务台",
      it_engineer = "IT工程师",
      sys_engineer = "系统工程师",
      user = "普通用户",
      rv$current_user$role[1])
  })

  # 修改密码
  observe({
    ok <- btn_ok(input$self_old_password) && btn_ok(input$self_new_password) && btn_ok(input$self_new_password_confirm)
    toggle_btn("self_save_password", ok)
  })
  observeEvent(input$self_save_password, {
    req(rv$logged_in, rv$current_user)
    old_pw <- trimws(input$self_old_password %||% "")
    new_pw <- trimws(input$self_new_password %||% "")
    new_pw_confirm <- trimws(input$self_new_password_confirm %||% "")

    output$self_password_msg <- renderText({ "" })

    # 校验
    if (old_pw == "" || new_pw == "" || new_pw_confirm == "") {
      output$self_password_msg <- renderText({ "所有密码字段不能为空" })
      return()
    }
    if (nchar(new_pw) < 3) {
      output$self_password_msg <- renderText({ "新密码长度至少3位" })
      return()
    }
    if (new_pw != new_pw_confirm) {
      output$self_password_msg <- renderText({ "两次输入的新密码不一致" })
      return()
    }
    # 验证旧密码是否正确
    if (old_pw != rv$current_user$password[1]) {
      output$self_password_msg <- renderText({ "旧密码不正确" })
      return()
    }
    # 调用 user_update_password（该函数也允许用户修改自己的密码）
    result <- user_update_password(rv$current_user$id[1], new_pw, rv$current_user)
    if (result$success) {
      # 更新 current_user 中的密码字段
      rv$current_user$password[1] <- new_pw
      output$self_password_msg <- renderText({ "密码修改成功" })
      updateTextInput(session, "self_old_password", value = "")
      updateTextInput(session, "self_new_password", value = "")
      updateTextInput(session, "self_new_password_confirm", value = "")
    } else {
      output$self_password_msg <- renderText({ result$message })
    }
  })

  # ========== 首页模块 ==========
  # 我的项目（排除已完成/已关闭）
  output$home_my_projects <- renderUI({
    req(rv$logged_in, rv$current_user)
    uid <- rv$current_user$id[1]
    con <- db_connect()
    projects <- tryCatch({
      dbGetQuery(con, sprintf(
        "SELECT id, project_no, name, status, priority, start_date, end_date
         FROM projects
         WHERE created_by = %d AND status NOT IN ('completed', 'closed')
         ORDER BY updated_at DESC LIMIT 5", uid))
    }, error = function(e) data.frame(), finally = db_disconnect(con))

    if (nrow(projects) == 0) {
      return(div(style = "color:#999; padding:10px;", "暂无进行中的项目"))
    }

    items <- lapply(1:nrow(projects), function(i) {
      p <- projects[i, ]
      status_cn <- switch(as.character(p$status),
        "planning" = "规划中", "active" = "进行中", "suspended" = "已暂停", p$status)
      status_color <- switch(as.character(p$status),
        "planning" = "#5bc0de", "active" = "#337ab7", "suspended" = "#f0ad4e", "#999")
      # 转义单引号避免JS错误
      escaped_name <- gsub("'", "\\\\'", as.character(p$name))
      div(style = "padding:8px 12px; margin-bottom:6px; background:#f9f9f9; border-radius:4px; font-size:13px;",
        span(style = sprintf("display:inline-block;padding:1px 6px;border-radius:3px;font-size:11px;color:white;background:%s;margin-right:8px;", status_color), status_cn),
        tags$a(href = "#", class = "proj-enter-btn", `data-id` = p$id, `data-name` = p$name, style = "color:#337ab7;text-decoration:none;cursor:pointer;", 
               tags$b(p$name)),
        span(style = "color:#999;margin-left:10px;font-size:12px;", sprintf("[%s]", p$project_no))
      )
    })
    do.call(tagList, items)
  })

  # 我的工单（排除已完成/已关闭）
  output$home_my_work_orders <- renderUI({
    req(rv$logged_in, rv$current_user)
    uid <- rv$current_user$id[1]
    con <- db_connect()
    orders <- tryCatch({
      dbGetQuery(con, sprintf(
        "SELECT wo.id, wo.order_no, wo.title, wo.status, wo.priority, wo.category
         FROM work_orders wo
         WHERE (wo.assigned_to = %d OR wo.handled_by = %d OR wo.created_by = %d)
           AND wo.status NOT IN ('completed', 'closed')
         ORDER BY wo.updated_at DESC LIMIT 5", uid, uid, uid))
    }, error = function(e) data.frame(), finally = db_disconnect(con))

    if (nrow(orders) == 0) {
      return(div(style = "color:#999; padding:10px;", "暂无待处理的工单"))
    }

    items <- lapply(1:nrow(orders), function(i) {
      w <- orders[i, ]
      status_cn <- switch(as.character(w$status),
        "pending" = "待处理", "assigned" = "已派发", "processing" = "处理中", w$status)
      status_color <- switch(as.character(w$status),
        "pending" = "#f0ad4e", "assigned" = "#5bc0de", "processing" = "#337ab7", "#999")
      # 处理工单号显示
      order_no_val <- ifelse(is.na(w$order_no) || is.null(w$order_no) || nchar(trimws(as.character(w$order_no))) == 0,
                             sprintf("ITS%s%03d", format(as.Date(w$created_at), "%Y%m%d"), w$id),
                             w$order_no)
      div(style = "padding:8px 12px; margin-bottom:6px; background:#f9f9f9; border-radius:4px; font-size:13px;",
        span(style = sprintf("display:inline-block;padding:1px 6px;border-radius:3px;font-size:11px;color:white;background:%s;margin-right:8px;", status_color), status_cn),
        tags$a(href = "#", style = "color:#5bc0de;text-decoration:none;cursor:pointer;", 
               onclick = sprintf("Shiny.setInputValue('work_order_view_click', %d, {priority: 'event'});",
                                 w$id),
               tags$b(order_no_val)),
        span(style = "margin-left:8px;", w$title),
        span(style = "color:#999;margin-left:8px;font-size:12px;", sprintf("[%s]", ifelse(is.na(w$category), "", w$category)))
      )
    })
    do.call(tagList, items)
  })

  # 我的任务（排除已完成）
  output$home_my_tasks <- renderUI({
    req(rv$logged_in, rv$current_user)
    uid <- rv$current_user$id[1]
    con <- db_connect()
    tasks <- tryCatch({
      dbGetQuery(con, sprintf(
        "SELECT t.id, t.task_no, t.name, t.status, t.priority, t.importance,
                p.name as project_name
         FROM project_tasks t
         LEFT JOIN projects p ON t.project_id = p.id
         WHERE t.assigned_to = %d AND t.status NOT IN ('completed')
         ORDER BY COALESCE(t.importance, 0) DESC, t.updated_at DESC LIMIT 15", uid))
    }, error = function(e) data.frame(), finally = db_disconnect(con))

    if (nrow(tasks) == 0) {
      return(div(style = "color:#999; padding:10px;", "暂无待处理的任务"))
    }

    items <- lapply(1:nrow(tasks), function(i) {
      tk <- tasks[i, ]
      status_cn <- switch(as.character(tk$status),
        "pending" = "待处理", "in_progress" = "进行中", "blocked" = "已阻塞", tk$status)
      status_color <- switch(as.character(tk$status),
        "pending" = "#f0ad4e", "in_progress" = "#337ab7", "blocked" = "#d9534f", "#999")
      importance_flags <- if (!is.na(tk$importance) && tk$importance > 0) {
        paste(rep("\U0001F6A9", tk$importance), collapse = "")
      } else ""
      task_no <- ifelse(is.na(tk$task_no), "-", tk$task_no)
      div(style = "padding:8px 12px; margin-bottom:6px; background:#f9f9f9; border-radius:4px; font-size:13px;",
        span(style = sprintf("display:inline-block;padding:1px 6px;border-radius:3px;font-size:11px;color:white;background:%s;margin-right:8px;", status_color), status_cn),
        if (importance_flags != "") span(style = "margin-right:6px;", importance_flags),
        tags$a(href = "#", style = "color:#5cb85c;text-decoration:none;cursor:pointer;", 
               onclick = sprintf("Shiny.setInputValue('task_view_click', %d, {priority: 'event'});",
                                 tk$id),
               tags$b(task_no)),
        span(style = "margin-left:8px;", tk$name),
        span(style = "color:#999;margin-left:8px;font-size:12px;", sprintf("[%s]", ifelse(is.na(tk$project_name), "", tk$project_name)))
      )
    })
    do.call(tagList, items)
  })
  
  # ★ suspendWhenHidden 使用默认 TRUE：display:none 时挂起，显示时恢复并重新评估

  # 项目管理模块逻辑
  project_server(input, output, session, rv)

  # 首页项目点击监听（直接监听，保证首页项目点击能正常工作）
  observeEvent(input$proj_enter_click, {
    req(rv$logged_in)
    req(input$proj_enter_click$id)
    rv$proj_nav_project_id <- input$proj_enter_click$id
    rv$proj_nav_project_name <- input$proj_enter_click$name
    rv$proj_nav_level <- "phases"
    # 切换到项目管理tab
    updateTabsetPanel(session, "main_tabs", selected = "项目")
  })

  # 首页 "more" → 跳转
  observeEvent(input$home_goto_proj, {
    updateTabsetPanel(session, "main_tabs", selected = "项目")
  })
  observeEvent(input$home_goto_wo, {
    updateTabsetPanel(session, "main_tabs", selected = "工单")
  })

  # ========== 首页快速开发 ==========
  # 提交到元任务 NTE20260606002
  observeEvent(input$quick_dev_submit, {
    req(rv$logged_in, rv$current_user)
    content <- trimws(input$quick_dev_input %||% "")
    if (nchar(content) == 0) {
      showNotification("请输入开发内容", type = "warning")
      return()
    }
    # 获取元任务 note_id
    con <- db_connect()
    note_id <- tryCatch({
      dbGetQuery(con, "SELECT id FROM notes WHERE note_no = 'NTE20260606002'")$id[1]
    }, error = function(e) NA_integer_, finally = { db_disconnect(con) })
    if (is.na(note_id)) {
      showNotification("元任务 NTE20260606002 不存在", type = "error")
      return()
    }
    result <- note_comment_add(note_id, content, rv$current_user$id[1])
    if (isTRUE(result$success)) {
      updateTextAreaInput(session, "quick_dev_input", value = "")
      rv$home_dev_refresh <- isolate(rv$home_dev_refresh) + 1L
      showNotification("已提交到元任务", type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # 跳转开发日志
  observeEvent(input$quick_dev_goto_log, {
    req(rv$logged_in)
    session$sendCustomMessage("runjs", "switchToDevLogTab")
  })
  # View More 跳转
  observeEvent(input$quick_note_viewmore, {
    updateTabsetPanel(session, "main_tabs", selected = "记事")
  })
  observeEvent(input$quick_wo_viewmore, {
    updateTabsetPanel(session, "main_tabs", selected = "工单")
  })

  # 最近 N 条元任务评论（NTE20260606002）
  output$home_latest_dev_logs <- renderUI({
    req(rv$logged_in)
    rv$home_dev_refresh
    con <- db_connect()
    comments <- tryCatch({
      dbGetQuery(con, "SELECT nc.id, nc.content, nc.status, nc.created_at, nc.created_by, u.display_name
        FROM note_comments nc
        LEFT JOIN users u ON nc.created_by = u.id
        WHERE nc.note_id = (SELECT id FROM notes WHERE note_no = 'NTE20260606002')
        ORDER BY nc.created_at DESC LIMIT 8")
    }, error = function(e) data.frame(), finally = { db_disconnect(con) })
    if (nrow(comments) == 0) return(tags$p(style="color:#999;font-size:12px;text-align:center;margin:0;","暂无"))
    do.call(tagList, lapply(seq_len(nrow(comments)), function(i) {
      r <- comments[i,]
      is_done <- !is.na(r$status) && r$status == "completed"
      status_badge <- if (is_done)
        tags$span(style="display:inline-block;background:#d4edda;color:#155724;border-radius:10px;padding:0 6px;font-size:10px;margin-right:4px;", "✅ 完成")
      else
        tags$span(style="display:inline-block;background:#fff3cd;color:#856404;border-radius:10px;padding:0 6px;font-size:10px;margin-right:4px;", "⏳ 待开")
      preview <- if (nchar(r$content) > 60) paste0(substr(r$content, 1, 60), "…") else r$content
      tags$div(style="font-size:12px; padding:3px 0; border-bottom:1px dotted #eee;",
        tags$span(style="color:#888;font-family:Consolas,monospace;margin-right:4px;", sprintf("#%d", r$id)),
        status_badge,
        tags$span(style="color:#333;", preview),
        tags$span(style="color:#bbb;float:right;", substr(r$created_at, 12, 16))
      )
    }))
  })

  # 最近8条记事
  output$home_recent_notes <- renderUI({
    req(rv$logged_in)
    rv$home_dev_refresh
    con <- db_connect()
    notes <- tryCatch(dbGetQuery(con,
      "SELECT id, note_no, title, status, updated_at FROM notes WHERE status != 'completed' ORDER BY updated_at DESC LIMIT 8"),
      error=function(e) data.frame(), finally={db_disconnect(con)})
    if (nrow(notes)==0) return(tags$p(style="color:#999;font-size:12px;text-align:center;margin:0;","暂无"))
    st_col <- c(pending="#f0ad4e", in_progress="#337ab7", completed="#5cb85c")
    do.call(tagList, lapply(seq_len(nrow(notes)), function(i){
      r <- notes[i,]
      clr <- st_col[as.character(r$status)]
      if(is.na(clr)) clr <- "#999"
      tags$div(style="font-size:12px;padding:3px 0;border-bottom:1px dotted #eee;",
        tags$span(style=sprintf("display:inline-block;width:7px;height:7px;border-radius:50%%;background:%s;margin-right:4px;",clr)),
        tags$span(style="color:#888;margin-right:4px;", r$note_no),
        tags$a(style="color:#333;text-decoration:none;cursor:pointer;",
          href="#", onclick=sprintf("Shiny.setInputValue('note_edit_click',%d,{priority:'event'});return false;", r$id),
          r$title),
        tags$span(style="color:#bbb;float:right;", substr(r$updated_at,12,16))
      )
    }))
  })

  # 最近8条工单
  output$home_recent_wos <- renderUI({
    req(rv$logged_in)
    rv$home_dev_refresh
    con <- db_connect()
    wos <- tryCatch(dbGetQuery(con,
      "SELECT id, order_no, title, status, updated_at FROM work_orders WHERE status NOT IN ('completed','closed') ORDER BY updated_at DESC LIMIT 8"),
      error=function(e) data.frame(), finally={db_disconnect(con)})
    if (nrow(wos)==0) return(tags$p(style="color:#999;font-size:12px;text-align:center;margin:0;","暂无"))
    st_col <- c(pending="#f0ad4e",assigned="#5bc0de",processing="#337ab7")
    do.call(tagList, lapply(seq_len(nrow(wos)), function(i){
      r <- wos[i,]
      clr <- st_col[as.character(r$status)]
      if(is.na(clr)) clr <- "#999"
      tags$div(style="font-size:12px;padding:3px 0;border-bottom:1px dotted #eee;",
        tags$span(style=sprintf("display:inline-block;width:7px;height:7px;border-radius:50%%;background:%s;margin-right:4px;",clr)),
        tags$span(style="color:#888;margin-right:4px;", r$order_no),
        tags$a(style="color:#333;text-decoration:none;cursor:pointer;",
          href="#", onclick=sprintf("Shiny.setInputValue('work_order_view_click',%d,{priority:'event'});return false;", r$id),
          r$title),
        tags$span(style="color:#bbb;float:right;", substr(r$updated_at,12,16))
      )
    }))
  })

  # 高亮关键字辅助函数
  hl <- function(txt, kw) {
    if (is.null(kw) || nchar(kw)==0) return(txt)
    gsub(paste0("(",kw,")"), "<mark style='background:#fef08a;padding:0 1px;border-radius:2px;'>\\1</mark>", txt, ignore.case=TRUE)
  }

  # 快速记事搜索（标题+评论，高亮，X清除）
  observeEvent(input$home_note_search_btn, {
    rv$home_note_search_kw <- trimws(input$home_note_search %||% "")
  })
  observeEvent(input$home_note_search_x, {
    updateTextInput(session, "home_note_search", value = "")
    rv$home_note_search_kw <- NULL
  })
  output$home_note_search_result <- renderUI({
    req(rv$logged_in)
    kw <- rv$home_note_search_kw
    if (is.null(kw) || nchar(kw) == 0) return(NULL)
    con <- db_connect()
    notes <- tryCatch(dbGetQuery(con, sprintf(
      "SELECT DISTINCT n.id, n.note_no, n.title, n.status FROM notes n
       LEFT JOIN note_comments nc ON nc.note_id=n.id
       WHERE n.title LIKE '%%%s%%' OR nc.content LIKE '%%%s%%'
       ORDER BY n.updated_at DESC LIMIT 10",
      gsub("'","''",kw), gsub("'","''",kw))),
      error=function(e) data.frame(), finally={db_disconnect(con)})
    if (nrow(notes)==0) return(tags$p(style="color:#999;font-size:12px;text-align:center;margin:0;",sprintf("未找到「%s」",kw)))
    st_col <- c(pending="#f0ad4e", in_progress="#337ab7")
    do.call(tagList, lapply(seq_len(nrow(notes)), function(i){
      r <- notes[i,]
      clr <- st_col[as.character(r$status)]
      if(is.na(clr)) clr <- "#999"
      tags$div(style="font-size:12px;padding:3px 0;border-bottom:1px dotted #eee;",
        tags$span(style=sprintf("display:inline-block;width:7px;height:7px;border-radius:50%%;background:%s;margin-right:4px;",clr)),
        tags$span(style="color:#888;margin-right:4px;", r$note_no),
        tags$a(style="color:#333;text-decoration:none;cursor:pointer;",
          href="#", onclick=sprintf("Shiny.setInputValue('note_edit_click',%d,{priority:'event'});return false;", r$id),
          HTML(hl(r$title, kw)))
      )
    }))
  })

  # 快速开发搜索（元任务评论）
  observeEvent(input$home_dl_search_btn, {
    rv$home_dl_search_kw <- trimws(input$home_dl_search %||% "")
  })
  observeEvent(input$home_dl_search_x, {
    updateTextInput(session, "home_dl_search", value = "")
    rv$home_dl_search_kw <- NULL
  })
  output$home_dl_search_result <- renderUI({
    req(rv$logged_in)
    kw <- rv$home_dl_search_kw
    if (is.null(kw) || nchar(kw) == 0) return(NULL)
    con <- db_connect()
    comments <- tryCatch({
      dbGetQuery(con, sprintf(
        "SELECT nc.id, nc.content, nc.status, nc.created_at, u.display_name
         FROM note_comments nc LEFT JOIN users u ON nc.created_by = u.id
         WHERE nc.note_id = (SELECT id FROM notes WHERE note_no = 'NTE20260606002')
         AND nc.content LIKE '%%%s%%' ORDER BY nc.created_at DESC LIMIT 10",
        gsub("'", "''", kw)))
    }, error=function(e) data.frame(), finally={db_disconnect(con)})
    if (nrow(comments)==0) return(tags$p(style="color:#999;font-size:12px;text-align:center;margin:0;",sprintf("未找到「%s」",kw)))
    do.call(tagList, lapply(seq_len(nrow(comments)), function(i){
      r <- comments[i,]
      is_done <- !is.na(r$status) && r$status == "completed"
      status_badge <- if (is_done)
        tags$span(style="display:inline-block;background:#d4edda;color:#155724;border-radius:10px;padding:0 6px;font-size:10px;margin-right:4px;", "✅")
      else
        tags$span(style="display:inline-block;background:#fff3cd;color:#856404;border-radius:10px;padding:0 6px;font-size:10px;margin-right:4px;", "⏳")
      preview <- if (nchar(r$content) > 60) paste0(substr(r$content, 1, 60), "…") else r$content
      tags$div(style="font-size:12px;padding:3px 0;border-bottom:1px dotted #eee;",
        tags$span(style="color:#888;font-family:Consolas,monospace;margin-right:4px;", sprintf("#%d", r$id)),
        status_badge,
        tags$span(style="color:#333;", preview),
        tags$span(style="color:#bbb;float:right;", substr(r$created_at,12,16))
      )
    }))
  })
  observeEvent(input$home_dl_goto, {
    session$sendCustomMessage("runjs", "switchToDevLogTab")
  })

  # 快速工单搜索
  observeEvent(input$home_wo_search_btn, {
    rv$home_wo_search_kw <- trimws(input$home_wo_search %||% "")
  })
  observeEvent(input$home_wo_search_x, {
    updateTextInput(session, "home_wo_search", value = "")
    rv$home_wo_search_kw <- NULL
  })
  output$home_wo_search_result <- renderUI({
    req(rv$logged_in)
    kw <- rv$home_wo_search_kw
    if (is.null(kw) || nchar(kw) == 0) return(NULL)
    con <- db_connect()
    wos <- tryCatch(dbGetQuery(con, sprintf(
      "SELECT id, order_no, title, status, updated_at FROM work_orders WHERE title LIKE '%%%s%%' OR order_no LIKE '%%%s%%' ORDER BY updated_at DESC LIMIT 10",
      gsub("'","''",kw), gsub("'","''",kw))),
      error=function(e) data.frame(), finally={db_disconnect(con)})
    if (nrow(wos)==0) return(tags$p(style="color:#999;font-size:12px;text-align:center;margin:0;",sprintf("未找到「%s」",kw)))
    st_col <- c(pending="#f0ad4e",assigned="#5bc0de",processing="#337ab7")
    do.call(tagList, lapply(seq_len(nrow(wos)), function(i){
      r <- wos[i,]
      clr <- st_col[as.character(r$status)]
      if(is.na(clr)) clr <- "#999"
      tags$div(style="font-size:12px;padding:3px 0;border-bottom:1px dotted #eee;",
        tags$span(style=sprintf("display:inline-block;width:7px;height:7px;border-radius:50%%;background:%s;margin-right:4px;",clr)),
        tags$span(style="color:#888;margin-right:4px;", r$order_no),
        tags$a(style="color:#333;text-decoration:none;cursor:pointer;",
          href="#", onclick=sprintf("Shiny.setInputValue('work_order_view_click',%d,{priority:'event'});return false;", r$id),
          HTML(hl(r$title, kw))),
        tags$span(style="color:#bbb;float:right;", substr(r$updated_at,12,16))
      )
    }))
  })

  # 快速记事
  observeEvent(input$quick_note_submit, {
    req(rv$logged_in, rv$current_user)
    content <- trimws(input$quick_note_input %||% "")
    if (nchar(content) == 0) {
      showNotification("请输入记事内容", type = "warning")
      return()
    }
    result <- note_add(content, rv$current_user$id[1])
    if (isTRUE(result$success)) {
      updateTextAreaInput(session, "quick_note_input", value = "")
      showNotification(result$message, type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # 快速工单
  observeEvent(input$quick_wo_submit, {
    req(rv$logged_in, rv$current_user)
    content <- trimws(input$quick_wo_input %||% "")
    if (nchar(content) == 0) {
      showNotification("请输入工单内容", type = "warning")
      return()
    }
    parsed <- tryCatch(work_order_parse_quick_text(content), error = function(e) NULL)
    if (!is.null(parsed) && nchar(parsed$title) > 0) {
      result <- work_order_add(parsed$title, parsed$description %||% "",
        parsed$priority %||% "中", parsed$category %||% "一般",
        parsed$subcategory %||% "", parsed$request_user, rv$current_user)
      if (isTRUE(result$success)) {
        updateTextAreaInput(session, "quick_wo_input", value = "")
        # 指派处理人
        if (!is.null(parsed$assigned_to) && nchar(parsed$assigned_to) > 0) {
          wo_id <- result$id
          if (!is.null(wo_id)) {
            con <- db_connect()
            tryCatch({
              assignee <- dbGetQuery(con, sprintf(
                "SELECT id FROM users WHERE username = '%s' OR display_name = '%s'",
                parsed$assigned_to, parsed$assigned_to))
              if (nrow(assignee) > 0) {
                work_order_assign(wo_id, assignee$id[1], rv$current_user)
              }
            }, error = function(e) NULL, finally = { db_disconnect(con) })
          }
        }
        showNotification(result$message, type = "message")
      } else {
        showNotification(result$message, type = "error")
      }
    } else {
      showNotification("无法解析工单格式。请用标准格式：IT服务请求 日期 时间：\\n用户：姓名-部门\\n内容：…", type = "error")
    }
  })

  # 日报模块逻辑
  daily_report_server(input, output, session, rv)

  # 标准化模块逻辑
  std_server(input, output, session)

  # 测试模块逻辑（网络巡检）
  network_test_server(input, output, session, rv)

  # 数据中心模块逻辑（数据归集）
  data_center_server("data_center", rv)
  
  # 集成模块逻辑
  integration_server(input, output, session, rv)

  # 工具模块逻辑
  tools_server(input, output, session, rv)

  # AI 模块逻辑
  ai_server(input, output, session, rv)

  # 巡检模块逻辑
  inspection_server(input, output, session, rv)

  # 性能监控模块逻辑
  sysmon_server(input, output, session, rv)


  # 方案模块逻辑
  solution_server(input, output, session, rv)

  # 方案执行模块逻辑
  solution_exec_server(input, output, session, rv)

  # 流程模块逻辑
  process_server(input, output, session, rv)

  # 绩效模块逻辑
  performance_server(input, output, session, rv)

  # 记事模块逻辑
  note_server(input, output, session, rv)
  
  # 资产模块逻辑
  asset_server(input, output, session, rv)

  # 工位图模块逻辑
  seat_map_server(input, output, session, rv)

  # 岗职模块逻辑
  duty_matrix_server(input, output, session, rv)

  # 开发日志模块
  dev_log_server(input, output, session, rv)

  # 元任务模块
  meta_task_server(input, output, session, rv)

  # 流程超时检测已移除（新审批模块为同步流转）

  # ========== 数据结转模块 ==========
  carryover_trigger <- reactiveVal(0)

  # 加载上月末完成清单
  observeEvent(input$carryover_load_prev, {
    req(rv$logged_in)
    rv$carryover_prev_notes <- carryover_prev_month_pending()
    carryover_trigger(carryover_trigger() + 1)
  })
  observeEvent(input$carryover_select_all, {
    proxy <- DT::dataTableProxy("carryover_prev_table")
    if (!is.null(rv$carryover_prev_notes) && nrow(rv$carryover_prev_notes) > 0) {
      DT::selectRows(proxy, seq_len(nrow(rv$carryover_prev_notes)))
    }
  })
  observeEvent(input$carryover_deselect_all, {
    proxy <- DT::dataTableProxy("carryover_prev_table")
    DT::selectRows(proxy, NULL)
  })

  output$carryover_prev_month_label <- renderUI({
    req(rv$carryover_prev_notes)
    n <- nrow(rv$carryover_prev_notes)
    if (n == 0) {
      ym <- carryover_get_prev_ym()
      if (is.null(ym)) return(tags$span(style="color:#5cb85c;", "没有待处理的记事"))
      return(tags$span(style="color:#5cb85c;", sprintf("%s 月 所有记事已完成", ym)))
    }
    ym <- rv$carryover_prev_notes$ym[1]
    tags$span(sprintf("%s 月 共 %d 条未完成", ym, n))
  })

  output$carryover_prev_table <- DT::renderDT({
    carryover_trigger()
    notes <- rv$carryover_prev_notes
    if (is.null(notes) || nrow(notes) == 0) {
      return(DT::datatable(data.frame(提示="点击 [加载上月清单] 按钮查看", stringsAsFactors=FALSE), options=list(dom="t")))
    }
    disp <- data.frame(
      id = notes$id,
      note_no = ifelse(is.na(notes$note_no), "", notes$note_no),
      title = notes$title,
      status = notes$status,
      created_at = substr(notes$created_at, 1, 10),
      ym = notes$ym,
      stringsAsFactors = FALSE, check.names = FALSE
    )
    colnames(disp) <- c("ID", "记事号", "标题", "状态", "创建时间", "月份")
    DT::datatable(disp, escape = FALSE, rownames = FALSE, selection = "multiple",
      options = list(pageLength = 25, dom = "ltip", columnDefs = list(list(targets = 0, visible = FALSE)))
    )
  })

  # 确认结账
  observeEvent(input$carryover_close_btn, {
    req(rv$logged_in, rv$current_user)
    sel <- input$carryover_prev_table_rows_selected
    notes <- rv$carryover_prev_notes
    if (is.null(sel) || length(sel) == 0 || is.null(notes)) {
      showNotification("请先勾选要结账的记事", type = "warning"); return()
    }
    ids <- notes$id[sel]
    titles <- notes$title[sel]
    showModal(modalDialog(
      title = "确认结账",
      size = "m",
      tags$p(sprintf("将以下 %d 条记事标记为“已完成”：", length(ids))),
      tags$ul(lapply(titles, tags$li)),
      tags$p(style="color:#d9534f; font-size:12px;", "此操作不可撤销。"),
      footer = tagList(
        modalButton("取消"),
        actionButton("carryover_close_confirm", "确认结转", class = "btn-warning")
      )
    ))
  })
  observeEvent(input$carryover_close_confirm, {
    req(rv$logged_in, rv$current_user)
    sel <- input$carryover_prev_table_rows_selected
    notes <- rv$carryover_prev_notes
    ids <- notes$id[sel]
    result <- carryover_close_notes(ids, rv$current_user)
    removeModal()
    showNotification(result$message, type = if(result$success) "message" else "error")
    if (result$success) {
      rv$carryover_prev_notes <- carryover_prev_month_pending()
      carryover_trigger(carryover_trigger() + 1)
    }
  })

  # 加载本月模板
  observeEvent(input$carryover_load_curr, {
    req(rv$logged_in)
    rv$carryover_templates <- carryover_current_month_templates()
    carryover_trigger(carryover_trigger() + 1)
  })
  observeEvent(input$carryover_gen_sel_all, {
    proxy <- DT::dataTableProxy("carryover_template_table")
    if (!is.null(rv$carryover_templates) && nrow(rv$carryover_templates) > 0) {
      DT::selectRows(proxy, seq_len(nrow(rv$carryover_templates)))
    }
  })
  observeEvent(input$carryover_gen_desel_all, {
    proxy <- DT::dataTableProxy("carryover_template_table")
    DT::selectRows(proxy, NULL)
  })

  output$carryover_next_month_label <- renderUI({
    notes <- rv$carryover_templates
    if (is.null(notes) || nrow(notes) == 0) {
      return(tags$span("请先加载模板"))
    }
    from_ym <- notes$ym[1]
    dates <- carryover_next_month_dates(from_ym)
    tags$span(sprintf("将生成: %d年%02d月 · 首日 1号8:00 · 提醒 25号8:01 · 到期 末天17:00",
      dates$year, dates$month))
  })

  output$carryover_template_table <- DT::renderDT({
    carryover_trigger()
    notes <- rv$carryover_templates
    if (is.null(notes) || nrow(notes) == 0) {
      return(DT::datatable(data.frame(提示="点击 [加载本月模板] 按钮查看", stringsAsFactors=FALSE), options=list(dom="t")))
    }
    disp <- data.frame(
      id = notes$id,
      note_no = ifelse(is.na(notes$note_no), "", notes$note_no),
      title = notes$title,
      status = notes$status,
      created_at = substr(notes$created_at, 1, 10),
      ym = notes$ym,
      stringsAsFactors = FALSE, check.names = FALSE
    )
    colnames(disp) <- c("ID", "记事号", "标题", "状态", "创建时间", "月份")
    DT::datatable(disp, escape = FALSE, rownames = FALSE, selection = "multiple",
      options = list(pageLength = 25, dom = "ltip", columnDefs = list(list(targets = 0, visible = FALSE)))
    )
  })

  # 生成下月
  observeEvent(input$carryover_gen_btn, {
    req(rv$logged_in, rv$current_user)
    sel <- input$carryover_template_table_rows_selected
    notes <- rv$carryover_templates
    if (is.null(sel) || length(sel) == 0 || is.null(notes)) {
      showNotification("请先勾选模板记事", type = "warning"); return()
    }
    ids <- notes$id[sel]
    from_ym <- notes$ym[sel][1]
    rv$carryover_gen_from_ym <- from_ym
    dates <- carryover_next_month_dates(from_ym)
    showModal(modalDialog(
      title = "确认生成下月记事",
      size = "m",
      tags$p(sprintf("将从 %d 条模板生成 %d年%02d月 的副本：", length(ids), dates$year, dates$month)),
      tags$ul(lapply(notes$title[sel], function(t) {
        new_t <- carryover_replace_ym(t, sprintf("%04d-%02d", dates$year, dates$month))
        tags$li(tags$span(style="color:#999;", t), " → ", tags$b(new_t))
      })),
      tags$p(style="color:#999; font-size:12px;",
        "创建日期: 1号8:00 · 提醒: 25号8:01 · 到期: 末天17:00"),
      footer = tagList(
        modalButton("取消"),
        actionButton("carryover_gen_confirm", "确认生成", class = "btn-success")
      )
    ))
  })
  observeEvent(input$carryover_gen_confirm, {
    req(rv$logged_in, rv$current_user)
    sel <- input$carryover_template_table_rows_selected
    notes <- rv$carryover_templates
    ids <- notes$id[sel]
    from_ym <- notes$ym[sel][1]
    result <- carryover_generate_next_month(ids, rv$current_user, from_ym)
    removeModal()
    showNotification(result$message, type = if(result$success) "message" else "error", duration = 5)
  })

  # ========== 模块清单（刷新按钮触发重新渲染） ==========
  rv$mi_refresh <- 0
  observeEvent(input$mi_refresh_btn, {
    req(rv$logged_in)
    rv$mi_refresh <- rv$mi_refresh + 1
    showNotification("模块清单已刷新（文件时间戳已更新）", type = "message")
  })
  output$module_inventory_ui <- renderUI({
    rv$mi_refresh
    module_inventory_ui()
  })

  # 系统架构图已改用预渲染 SVG（见 Script/system_architecture.r），无需服务端渲染



}

# 总结：
# 1. ui.R三行代码能运行的原理：
#    - 采用了"动态UI"设计模式
#    - ui.R只提供基础框架（fluidPage + uiOutput）
#    - 实际UI内容由server端的renderUI根据状态动态生成
#    - 这种设计使得UI结构更灵活，可根据应用状态实时调整
#
# 2. runApp.R如何识别server.R和ui.R：
#    - 这是Shiny框架的"约定优于配置"设计原则
#    - runApp()函数会在当前工作目录自动查找：
#      - ui.R文件：应包含一个名为ui的对象
#      - server.R文件：应包含一个名为server的函数
#    - 找到后自动将它们组合成完整的应用
#    - 这种约定简化了应用启动流程，无需手动加载和组合文件
