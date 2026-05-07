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
source("Script/inspection_patrol.r")    # 巡检管理模块
source("Script/login_ui.r")         # 登录界面定义
source("Script/main_ui.r")          # 主界面定义
source("Script/github_autosubmit.r") # GitHub自动提交功能
source("Script/std_computer.r")        # 标准化模块

# 定义server函数
# 这是Shiny应用的服务器逻辑核心
# 参数说明：
# - input: 接收来自UI的用户输入
# - output: 向UI发送输出内容
# - session: 管理用户会话
server <- function(input, output, session) {
  
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
    current_user = NULL # 当前用户信息，默认为空
  )
  
  # 动态生成UI内容
  # renderUI函数用于根据应用状态动态生成UI组件
  # 这是ui.R中三行代码就能运行的核心原理：
  # 1. ui.R只提供了基础框架（fluidPage包含uiOutput）
  # 2. 实际UI内容由server端的renderUI动态生成
  # 3. 根据登录状态切换不同的界面
  output$app_ui <- renderUI({
    if (!rv$logged_in) {
      # 未登录时显示登录界面
      login_ui()
    } else {
      # 登录后显示主应用界面
      main_ui()
    }
  })
  
  # 控制admin菜单显示/隐藏
  observe({
    req(rv$logged_in)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    session$sendCustomMessage(type = "toggleAdminMenu", message = list(show = is_admin))
  })

  # 处理登录按钮点击事件
  # observeEvent函数用于响应用户操作
  observeEvent(input$login_btn, {
    # req函数确保输入值存在，防止空值导致错误
    req(input$login_username, input$login_password)
    
    # 调用auth_login函数进行身份验证
    result <- auth_login(input$login_username, input$login_password)
    
    if (result$success) {
      # 登录成功，更新登录状态和用户信息
      rv$logged_in <- TRUE
      rv$current_user <- result$user
      # 将用户ID保存到浏览器localStorage，刷新后可自动恢复
      session$sendCustomMessage(type = "saveLoginState", message = list(user_id = result$user$id[1]))
      # 显示欢迎通知
      showNotification(sprintf("欢迎回来，%s！", result$user$username[1]), type = "message")
    } else {
      # 登录失败，显示错误通知
      showNotification(result$message, type = "error")
    }
  })

  # 页面刷新后自动恢复登录状态
  observeEvent(input$auto_login_user_id, {
    req(input$auto_login_user_id)
    # 避免重复恢复（如果已经登录则跳过）
    if (rv$logged_in) return()
    result <- auth_login_by_id(input$auto_login_user_id)
    if (result$success) {
      rv$logged_in <- TRUE
      rv$current_user <- result$user
      showNotification(sprintf("欢迎回来，%s！", result$user$username[1]), type = "message")
    } else {
      # 自动登录失败（用户可能被删除或禁用），清除localStorage
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
    rv$work_order_refresh_trigger
    stats <- work_order_get_stats()
    as.character(stats$total[1])
  })
  output$wo_stat_pending <- renderText({
    rv$work_order_refresh_trigger
    stats <- work_order_get_stats()
    as.character(stats$pending[1])
  })
  output$wo_stat_assigned <- renderText({
    rv$work_order_refresh_trigger
    stats <- work_order_get_stats()
    as.character(stats$assigned[1])
  })
  output$wo_stat_processing <- renderText({
    rv$work_order_refresh_trigger
    stats <- work_order_get_stats()
    as.character(stats$processing[1])
  })
  output$wo_stat_completed <- renderText({
    rv$work_order_refresh_trigger
    stats <- work_order_get_stats()
    as.character(stats$completed[1])
  })
  output$wo_stat_closed <- renderText({
    rv$work_order_refresh_trigger
    stats <- work_order_get_stats()
    as.character(stats$closed[1])
  })
  
  # 工单状态筛选动态UI（从配置读取）
  output$work_order_status_filter_ui <- renderUI({
    choices <- work_order_status_choices(include_all = TRUE)
    if (length(choices) == 0) {
      choices <- c("全部工单" = "all", "待处理" = "pending", "已派发" = "assigned",
                   "处理中" = "processing", "已完成" = "completed", "已关闭" = "closed")
    }
    selectInput("work_order_status_filter", "状态筛选", choices = choices, selected = "all")
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
    all_orders <- work_order_get_all(input$work_order_status_filter)
    
    # 列顺序：工单号、操作、标题、描述、分类、优先级、处理人、状态、创建人、时间
    if (nrow(all_orders) > 0) {
      # 选择并重命名列
      display_data <- data.frame(
        工单号 = ifelse(is.na(all_orders$order_no), paste0("ITS", format(as.Date(all_orders$created_at), "%Y%m%d"), sprintf("%03d", all_orders$id)), all_orders$order_no),
        操作 = sprintf('<button class="btn btn-xs btn-info wo-view-btn" data-id="%s">查看</button>', all_orders$id),
        标题 = all_orders$title,
        描述 = all_orders$description,
        分类 = ifelse(is.na(all_orders$category), "未分类", all_orders$category),
        优先级 = all_orders$priority,
        处理人 = ifelse(is.na(all_orders$current_handler), "未分配", all_orders$current_handler),
        状态 = all_orders$status,
        创建人 = ifelse(is.na(all_orders$creator_name), "未知", all_orders$creator_name),
        时间 = all_orders$created_at,
        stringsAsFactors = FALSE
      )
      
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
      display_data <- data.frame(
        操作 = character(),
        工单号 = character(),
        标题 = character(),
        描述 = character(),
        分类 = character(),
        优先级 = character(),
        处理人 = character(),
        状态 = character(),
        创建人 = character(),
        时间 = character(),
        stringsAsFactors = FALSE
      )
    }
    
    DT::datatable(
      display_data,
      escape = FALSE,  # 允许HTML按钮
      options = list(
        pageLength = 50,  # 默认显示50行
        paging = TRUE,
        searching = TRUE,
        ordering = TRUE,
        info = FALSE,  # 隐藏底部信息
        lengthChange = FALSE,  # 隐藏每页显示数选择器
        dom = '<"row"<"col-sm-6"f><"col-sm-6"p>>t',  # 搜索框和分页在同一行
        columnDefs = list(
          # 工单号列
          list(targets = 0, width = '120px', className = 'dt-center'),
          # 操作列（按钮）
          list(targets = 1, width = '60px', className = 'dt-center', orderable = FALSE),
          # 标题列
          list(targets = 2, width = '150px', className = 'dt-left'),
          # 描述列：限制宽度
          list(targets = 3, width = '200px', className = 'dt-left'),
          # 分类列
          list(targets = 4, width = '80px', className = 'dt-center'),
          # 优先级列
          list(targets = 5, width = '60px', className = 'dt-center'),
          # 处理人列
          list(targets = 6, width = '80px', className = 'dt-center'),
          # 状态列
          list(targets = 7, width = '80px', className = 'dt-center'),
          # 创建人列
          list(targets = 8, width = '80px', className = 'dt-center'),
          # 时间列
          list(targets = 9, width = '130px', className = 'dt-center')
        ),
        rowCallback = JS(
          "function(row, data, index) {
            // 设置行高，确保一行最多跨三行
            $('td', row).css({
              'max-height': '60px',
              'overflow': 'hidden',
              'text-overflow': 'ellipsis',
              'white-space': 'nowrap'
            });
            // 描述列允许换行但限制行数
            $('td:eq(3)', row).css({
              'white-space': 'normal',
              'line-height': '1.4em',
              'max-height': '4.2em'
            });
          }"
        )
      ),
      callback = JS(
        "table.on('click', 'button.wo-view-btn', function() {
          var woId = $(this).data('id');
          Shiny.setInputValue('work_order_view_click', woId);
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
      all_orders <- work_order_get_all(input$work_order_status_filter)
      if (nrow(all_orders) >= selected_rows) {
        rv$selected_work_order_id <- all_orders$id[selected_rows]
        rv$selected_work_order_detail <- all_orders[selected_rows, ]
      }
    } else {
      rv$selected_work_order_id <- NULL
      rv$selected_work_order_detail <- NULL
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
      
      # 构建评论 HTML
      comments_html <- ""
      if (nrow(comments) > 0) {
        comments_html <- "<div style='margin-top: 15px;'><div style='font-weight: bold; color: #333; margin-bottom: 10px; font-size: 15px;'>💬 历史评论</div>"
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
        comments_html <- paste0(comments_html, "</div>")
      } else {
        comments_html <- "<div style='margin-top: 15px;'><div style='font-weight: bold; color: #333; margin-bottom: 8px; font-size: 15px;'>💬 历史评论</div><div style='color: #999; font-style: italic;'>暂无评论</div></div>"
      }
      
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
                <td style="font-weight: bold; color: #666;">指派给：</td>
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
      ifelse(is.na(wo$assignee_name), "未指派", wo$assignee_name),
      wo$created_at,
      ifelse(is.na(wo$assigned_at), "未指派", wo$assigned_at),
      ifelse(is.na(wo$handled_at), "未开始", wo$handled_at),
      ifelse(is.na(wo$completed_at), "未完成", wo$completed_at),
      ifelse(is.na(wo$description), "无描述", wo$description),
      ifelse(is.na(wo$resolution), "暂无", wo$resolution),
      comments_html
      ))
      
      showModal(modalDialog(
        title = paste0("工单详情 - ", ifelse(is.na(wo$order_no), sprintf("ITS%s%03d", format(as.Date(wo$created_at), "%Y%m%d"), wo$id), wo$order_no)),
        modal_content,
        footer = modalButton("关闭"),
        size = "l",
        easyClose = TRUE
      ))
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
      choices <- setNames(users$id, sprintf("%s (%s)", users$username, users$role))
      updateSelectInput(session, "work_order_assignee", choices = choices)
    }
  })
  
  # 处理创建工单按钮点击事件
  observeEvent(input$add_work_order, {
    req(rv$logged_in)
    req(input$work_order_title, input$work_order_description, input$work_order_priority)
    
    result <- work_order_add(
      input$work_order_title, 
      input$work_order_description, 
      input$work_order_priority,
      input$work_order_category,
      "",
      rv$current_user
    )
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    # 清空输入
    updateTextInput(session, "work_order_title", value = "")
    updateTextAreaInput(session, "work_order_description", value = "")
    updateSelectInput(session, "work_order_priority", selected = "中")
    updateSelectInput(session, "work_order_category", selected = "一般")
    
    # 触发刷新
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
  })
  
  # 处理刷新工单按钮点击事件
  observeEvent(input$refresh_work_orders, {
    req(rv$logged_in)
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
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
    req(input$work_order_comment)

    result <- work_order_add_comment(rv$selected_work_order_id, input$work_order_comment, rv$current_user)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))

    if (result$success) {
      updateTextAreaInput(session, "work_order_comment", value = "")
      rv$work_order_comment_refresh <- ifelse(is.null(rv$work_order_comment_refresh), 0, rv$work_order_comment_refresh + 1)
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
  
  # 初始渲染巡检表格
  output$inspection_table <- renderDT({
    DT::datatable(
      inspection_patrol_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 处理创建巡检按钮点击事件
  observeEvent(input$add_inspection, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$inspection_name, input$inspection_type, input$inspection_schedule)
    # 调用inspection_patrol_add函数创建巡检
    result <- inspection_patrol_add(input$inspection_name, input$inspection_type, input$inspection_schedule, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新巡检表格
    output$inspection_table <- renderDT({
      DT::datatable(
        inspection_patrol_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 处理刷新巡检按钮点击事件
  observeEvent(input$refresh_inspections, {
    # 检查登录状态
    req(rv$logged_in)
    # 刷新巡检表格
    output$inspection_table <- renderDT({
      DT::datatable(
        inspection_patrol_get_all(),
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
  
  # 处理生成可视化按钮点击事件
  observeEvent(input$generate_viz, {
    # 检查登录状态
    req(rv$logged_in)
    # 更新可视化图表输出
    output$viz_plot <- renderPlotly({
      # 调用viz_generate函数生成可视化图表
      viz_generate(input$viz_type, input$viz_data)
    })
  })
  
  # 初始渲染可视化图表
  output$viz_plot <- renderPlotly({
    viz_generate(input$viz_type, input$viz_data)
  })
  
  # 处理用户刷新按钮点击事件
  observeEvent(input$refresh_users, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 更新用户表格输出
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),  # 获取所有用户
        colnames = c("ID", "用户名", "显示名称", "角色", "状态", "创建时间", "更新时间"),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
  })
  
  # 处理用户表格点击事件
  observeEvent(input$user_table_rows_selected, {
    # 检查登录状态
    req(rv$logged_in)
    # 获取选中的用户信息
    selected_rows <- input$user_table_rows_selected
    if (length(selected_rows) > 0) {
      # 获取用户数据
      users <- user_get_all()
      selected_user <- users[selected_rows, ]
      # 将用户信息填充到输入框中
      updateTextInput(session, "selected_user_id", value = selected_user$id)
      updateTextInput(session, "username", value = selected_user$username)
      updateTextInput(session, "display_name", value = ifelse(is.na(selected_user$display_name), "", selected_user$display_name))
      updateTextInput(session, "password", value = "")
      updateSelectInput(session, "role", selected = selected_user$role)
    }
  })
  
  # 处理添加用户按钮点击事件
  observeEvent(input$add_user, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 确保必要输入存在
    req(input$username, input$password, input$role)
    # 调用user_add函数添加用户，传递当前用户信息
    result <- user_add(input$username, input$password, input$role, input$display_name, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新用户表格
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        colnames = c("ID", "用户名", "显示名称", "角色", "状态", "创建时间", "更新时间"),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
    # 清空输入框
    updateTextInput(session, "selected_user_id", value = "")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "display_name", value = "")
    updateTextInput(session, "password", value = "")
    updateSelectInput(session, "role", selected = "user")
  })
  
  # 处理修改账号按钮点击事件
  observeEvent(input$update_user, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 确保必要输入存在
    req(input$selected_user_id, input$username, input$role)
    # 调用user_update函数更新用户信息，传递当前用户信息和密码
    result <- user_update(input$selected_user_id, input$username, input$role, input$password, input$display_name, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新用户表格
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        colnames = c("ID", "用户名", "显示名称", "角色", "状态", "创建时间", "更新时间"),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
    # 清空输入框
    updateTextInput(session, "selected_user_id", value = "")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "display_name", value = "")
    updateTextInput(session, "password", value = "")
    updateSelectInput(session, "role", selected = "user")
  })
  
  # 处理禁用/启用用户按钮点击事件
  observeEvent(input$toggle_active_user, {
    # 检查登录状态和admin权限
    req(rv$logged_in)
    req(!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin")
    # 确保必要输入存在
    req(input$selected_user_id)
    # 调用user_toggle_active函数切换用户状态，传递当前用户信息
    result <- user_toggle_active(input$selected_user_id, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新用户表格
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        colnames = c("ID", "用户名", "显示名称", "角色", "状态", "创建时间", "更新时间"),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
    # 清空输入框
    updateTextInput(session, "selected_user_id", value = "")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "display_name", value = "")
    updateTextInput(session, "password", value = "")
    updateSelectInput(session, "role", selected = "user")
  })
  
  # 初始渲染用户表格
  output$user_table <- renderDT({
    DT::datatable(
      user_get_all(),
      colnames = c("ID", "用户名", "显示名称", "角色", "状态", "创建时间", "更新时间"),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      selection = list(mode = 'single', target = 'row', selected = integer(0))
    )
  })
  
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
  
  # 处理添加配置按钮点击事件
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
         ORDER BY updated_at DESC LIMIT 10", uid))
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
      div(style = "padding:8px 12px; margin-bottom:6px; background:#f9f9f9; border-radius:4px; font-size:13px;",
        span(style = sprintf("display:inline-block;padding:1px 6px;border-radius:3px;font-size:11px;color:white;background:%s;margin-right:8px;", status_color), status_cn),
        tags$b(p$name),
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
         ORDER BY wo.updated_at DESC LIMIT 10", uid, uid, uid))
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
      order_no <- ifelse(is.na(w$order_no), "-", w$order_no)
      div(style = "padding:8px 12px; margin-bottom:6px; background:#f9f9f9; border-radius:4px; font-size:13px;",
        span(style = sprintf("display:inline-block;padding:1px 6px;border-radius:3px;font-size:11px;color:white;background:%s;margin-right:8px;", status_color), status_cn),
        tags$b(order_no),
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
      div(style = "padding:8px 12px; margin-bottom:6px; background:#f9f9f9; border-radius:4px; font-size:13px;",
        span(style = sprintf("display:inline-block;padding:1px 6px;border-radius:3px;font-size:11px;color:white;background:%s;margin-right:8px;", status_color), status_cn),
        if (importance_flags != "") span(style = "margin-right:6px;", importance_flags),
        tags$b(ifelse(is.na(tk$task_no), "-", tk$task_no)),
        span(style = "margin-left:8px;", tk$name),
        span(style = "color:#999;margin-left:8px;font-size:12px;", sprintf("[%s]", ifelse(is.na(tk$project_name), "", tk$project_name)))
      )
    })
    do.call(tagList, items)
  })

  # 项目管理模块逻辑
  project_server(input, output, session, rv)

  # 日报模块逻辑
  daily_report_server(input, output, session, rv)

  # 标准化模块逻辑
  std_server(input, output, session)

  # 测试模块逻辑（网络巡检）
  network_test_server(input, output, session)
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
