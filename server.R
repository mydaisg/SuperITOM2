# 加载功能模块脚本
# 这些脚本包含了系统的核心功能实现
source("Script/auth.r")           # 身份验证模块（登录/注销）
source("Script/data_management.r")  # 数据管理模块
source("Script/model_training.r")   # 模型训练模块
source("Script/visualization.r")    # 数据可视化模块
source("Script/user_management.r")   # 用户管理模块
source("Script/system_settings.r")  # 系统设置模块
source("Script/work_order.r")       # 工单管理模块
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
      # 显示欢迎通知
      showNotification(sprintf("欢迎回来，%s！", result$user$username[1]), type = "message")
    } else {
      # 登录失败，显示错误通知
      showNotification(result$message, type = "error")
    }
  })
  
  # 处理退出登录按钮点击事件
  observeEvent(input$logout, {
    # 调用auth_logout函数处理注销逻辑
    result <- auth_logout()
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
  
  # 初始渲染工单表格
  output$work_order_table <- renderDT({
    DT::datatable(
      work_order_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 处理创建工单按钮点击事件
  observeEvent(input$add_work_order, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$work_order_title, input$work_order_description, input$work_order_priority)
    # 调用work_order_add函数创建工单
    result <- work_order_add(input$work_order_title, input$work_order_description, input$work_order_priority, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新工单表格
    output$work_order_table <- renderDT({
      DT::datatable(
        work_order_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 处理刷新工单按钮点击事件
  observeEvent(input$refresh_work_orders, {
    # 检查登录状态
    req(rv$logged_in)
    # 刷新工单表格
    output$work_order_table <- renderDT({
      DT::datatable(
        work_order_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
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
    # 检查登录状态
    req(rv$logged_in)
    # 更新用户表格输出
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),  # 获取所有用户
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
      updateTextInput(session, "password", value = "")
      updateSelectInput(session, "role", selected = selected_user$role)
    }
  })
  
  # 处理添加用户按钮点击事件
  observeEvent(input$add_user, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$username, input$password, input$role)
    # 调用user_add函数添加用户，传递当前用户信息
    result <- user_add(input$username, input$password, input$role, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新用户表格
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
    # 清空输入框
    updateTextInput(session, "selected_user_id", value = "")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    updateSelectInput(session, "role", selected = "user")
  })
  
  # 处理修改账号按钮点击事件
  observeEvent(input$update_user, {
    # 检查登录状态
    req(rv$logged_in)
    # 确保必要输入存在
    req(input$selected_user_id, input$username, input$role)
    # 调用user_update函数更新用户信息，传递当前用户信息和密码
    result <- user_update(input$selected_user_id, input$username, input$role, input$password, rv$current_user)
    # 显示操作结果通知
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 刷新用户表格
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
    # 清空输入框
    updateTextInput(session, "selected_user_id", value = "")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    updateSelectInput(session, "role", selected = "user")
  })
  
  # 处理禁用/启用用户按钮点击事件
  observeEvent(input$toggle_active_user, {
    # 检查登录状态
    req(rv$logged_in)
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
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        selection = list(mode = 'single', target = 'row', selected = integer(0))
      )
    })
    # 清空输入框
    updateTextInput(session, "selected_user_id", value = "")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    updateSelectInput(session, "role", selected = "user")
  })
  
  # 初始渲染用户表格
  output$user_table <- renderDT({
    DT::datatable(
      user_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      selection = list(mode = 'single', target = 'row', selected = integer(0))
    )
  })
  
  # 处理配置刷新按钮点击事件
  observeEvent(input$refresh_config, {
    # 检查登录状态
    req(rv$logged_in)
    # 更新配置表格输出
    output$config_table <- renderDT({
      DT::datatable(
        config_get_all(),  # 获取所有配置
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  # 处理添加配置按钮点击事件
  observeEvent(input$add_config, {
    # 检查登录状态
    req(rv$logged_in)
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
    # 调用github_check_status函数查看Git状态
    output$github_output <- renderPrint({
      github_check_status()
    })
  })
  
  # 处理拉取GitHub代码按钮点击事件
  observeEvent(input$github_pull, {
    # 调用github_pull函数拉取代码
    output$github_output <- renderPrint({
      github_pull()
    })
    # 显示操作结果通知
    showNotification("代码已从 GitHub 拉取", type = "message")
  })
  
  # 标准化模块逻辑
  std_server(input, output, session)
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
