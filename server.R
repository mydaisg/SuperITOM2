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
source("Script/process_engine.r")       # 流程引擎核心（定义 %||% 等工具函数，network_test.r 依赖）
source("Script/github_autosubmit.r") # GitHub自动提交功能
source("Script/std_computer.r")        # 标准化模块
source("Script/main_ui.r")          # 主界面定义
source("Script/process_server.r")       # 流程模块服务端
source("Script/sysmon_management.r")   # 性能监控数据层
source("Script/sysmon_server.r")       # 性能监控服务端
source("Script/performance_management.r") # 绩效数据层
source("Script/performance_server.r")   # 绩效模块服务端
source("Script/note_management.r")   # 记事模块数据层
source("Script/note_server.r")       # 记事模块服务端
source("Script/asset_management.r")  # 资产模块数据层
source("Script/asset_server.r")      # 资产模块服务端
source("Script/duty_matrix_management.r") # 岗职模块数据层
source("Script/duty_matrix_server.r")     # 岗职模块服务端
source("Script/module_inventory.r")       # 模块清单（全站映射参考）

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
    current_user = NULL, # 当前用户信息，默认为空
    daily_report_refresh = 0  # 日报刷新触发器
  )
  
  # 通用按钮状态控制
  toggle_btn <- function(id, enabled) {
    session$sendCustomMessage("toggleBtn", list(id = id, disabled = !isTRUE(enabled)))
  }
  btn_ok <- function(val) { !is.null(val) && length(val) > 0 && nchar(trimws(paste(val, collapse=""))) > 0 }
  
  # OLD 架构：renderUI 直接返回 login_ui() 或 main_ui()
  output$app_ui <- renderUI({
    if (!rv$logged_in) {
      login_ui()
    } else {
      message("[RENDER] 调用 main_ui()")
      is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
      user_modules <- rbac_get_user_modules(rv$current_user)
      main_ui(is_admin = is_admin, user_modules = user_modules)
    }
  })

  # 模块数据刷新辅助函数
  refresh_all_modules <- function() {
    rv$work_order_refresh_trigger <- rv$work_order_refresh_trigger + 1
    rv$proj_data_refresh <- if(is.null(rv$proj_data_refresh)) 1 else rv$proj_data_refresh + 1
    rv$inspection_refresh_trigger <- if(is.null(rv$inspection_refresh_trigger)) 1 else rv$inspection_refresh_trigger + 1
  }

  # 控制admin菜单显示/隐藏
  observe({
    req(rv$logged_in)
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    session$sendCustomMessage(type = "toggleAdminMenu", message = list(show = is_admin))
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
  
  # 处理生成可视化按钮点击事件
  observeEvent(input$generate_viz, {
    # 检查登录状态
    req(rv$logged_in)
    # 更新可视化图表输出
    output$viz_plot <- renderPlotly({
      viz_generate(input$viz_type, input$viz_data)
    })
  })

  # 初始渲染可视化图表
  output$viz_plot <- renderPlotly({
    viz_generate(input$viz_type, input$viz_data)
  })

  # 可视化页 - 流程监控指标
  output$viz_mtr_complete_rate <- renderText({ "0%" })
  output$viz_mtr_timeout_rate <- renderText({ "0%" })
  output$viz_mtr_avg_duration <- renderText({ "0 分钟" })
  output$viz_mtr_running <- renderText({ "0" })
  output$viz_mtr_today <- renderText({ "暂无" })
  
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
  observe({
    ok <- btn_ok(input$username) && btn_ok(input$password) && btn_ok(input$role)
    toggle_btn("add_user", ok)
  })
  observe({
    ok <- btn_ok(input$selected_user_id) && btn_ok(input$username) && btn_ok(input$role)
    toggle_btn("update_user", ok)
  })
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

  # 角色列表（带刷新）
  output$rbac_role_table <- DT::renderDT({
    rbac_refresh(); req(rv$logged_in)
    roles <- rbac_role_get_all()
    DT::datatable(roles, rownames = FALSE, selection = "single",
      colnames = c("ID", "角色名称", "描述"),
      options = list(pageLength = 10, dom = 't'))
  })

  selected_role_id <- reactiveVal(NULL)
  rbac_role_perms_initial <- reactiveVal(character(0))  # 记录权限初始状态
  
  observeEvent(input$rbac_role_table_rows_selected, {
    roles <- rbac_role_get_all()
    if (length(input$rbac_role_table_rows_selected) > 0) {
      rid <- roles$id[input$rbac_role_table_rows_selected]
      selected_role_id(rid)
      rbac_role_perms_initial(as.character(rbac_role_perms_get(rid)))
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

  # 添加角色（刷新列表）
  observe({ toggle_btn("rbac_add_role", btn_ok(input$rbac_new_role_name)) })
  # 保存权限：初始灰色，权限变更后才启用
  observe({
    cur <- input$rbac_role_perms_select
    if (is.null(cur)) cur <- character(0)
    init <- rbac_role_perms_initial()
    changed <- !setequal(as.character(cur), as.character(init))
    toggle_btn("rbac_save_perms", changed)
  })
  observeEvent(input$rbac_add_role, {
    req(rv$logged_in, input$rbac_new_role_name)
    result <- rbac_role_add(input$rbac_new_role_name)
    if (result$success) {
      updateTextInput(session, "rbac_new_role_name", value = "")
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
      options = list(pageLength = 15, dom = 't'))
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

  # 日报模块逻辑
  daily_report_server(input, output, session, rv)

  # 标准化模块逻辑
  std_server(input, output, session)

  # 测试模块逻辑（网络巡检）
  network_test_server(input, output, session)

  # 数据中心模块逻辑（数据归集）
  data_center_server("data_center", rv)
  
  # 集成模块逻辑
  integration_server(input, output, session, rv)
  
  # 巡检模块逻辑
  inspection_server(input, output, session, rv)

  # 性能监控模块逻辑
  sysmon_server(input, output, session, rv)

  # 流程模块逻辑（暂停排查）
  # process_server(input, output, session, rv)

  # 绩效模块逻辑
  performance_server(input, output, session, rv)

  # 记事模块逻辑
  note_server(input, output, session, rv)
  
  # 资产模块逻辑
  asset_server(input, output, session, rv)

  # 岗职模块逻辑
  duty_matrix_server(input, output, session, rv)

  # 流程超时检测已移除（新审批模块为同步流转）

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
