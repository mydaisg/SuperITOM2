# 标准化模块功能

# 加载语法高亮模块
source("Script/high_light.r")

# UI部分
std_ui <- function() {
  fluidPage(
    # 添加语法高亮资源
    high_light_ui(),
    
    titlePanel("计算机标准化"),
    fluidRow(
      column(4,
        h4("计算机信息"),
        textInput("std_ip", "IP地址"),
        textInput("std_os", "操作系统"),
        textInput("std_user", "用户名"),
        textInput("std_password", "密码"),
        textInput("std_new_name", "新计算机名"),
        br(),
        actionButton("std_add", "添加计算机", class = "btn-success"),
        actionButton("std_delete", "删除计算机", class = "btn-danger"),
        br(), br(),
        actionButton("std_ping", "测试连通性", class = "btn-info"),
        br(), br(),
        h4("标准化脚本"),
        uiOutput("std_script_ui"),
        actionButton("std_show_script", "显示脚本", class = "btn-secondary"),
        actionButton("std_execute", "执行脚本", class = "btn-primary")
      ),
      column(8,
        h4("计算机列表"),
        DT::dataTableOutput("std_hosts_table"),
        br(),
        h4("操作结果"),
        verbatimTextOutput("std_output"),
        br(),
        h4("脚本内容"),
        htmlOutput("std_script_content")
      )
    )
  )
}

# Server部分
std_server <- function(input, output, session) {
  # 创建响应式值来存储主机数据
  std_hosts_data <- reactiveVal(NULL)
  # 创建响应式值来存储脚本显示状态
  std_script_visible <- reactiveVal(FALSE)
  
  # 加载脚本列表
  output$std_script_ui <- renderUI({
    # 获取STD目录下的脚本文件
    std_dir <- file.path(getwd(), "STD")
    
    if (dir.exists(std_dir)) {
      # 列出脚本文件
      script_files <- list.files(std_dir, pattern = "\\.(ps1|sh|r|bat)$", full.names = FALSE)
      # 生成脚本选择框
      selectInput("std_script", "选择脚本", choices = script_files)
    } else {
      selectInput("std_script", "选择脚本", choices = c("无可用脚本"))
    }
  })
  
  # 加载主机列表
  observe({
    # 读取hosts_new.csv文件
    hosts_file <- file.path(getwd(), "STD", "hosts_new.csv")
    if (file.exists(hosts_file)) {
      hosts_data <- read.csv(hosts_file, stringsAsFactors = FALSE)
      # 添加ID列
      hosts_data$ID <- 1:nrow(hosts_data)
      # 存储到响应式值
      std_hosts_data(hosts_data)
    }
  })
  
  # 渲染主机数据表格
  output$std_hosts_table <- DT::renderDataTable({
    hosts_data <- std_hosts_data()
    if (!is.null(hosts_data)) {
      DT::datatable(
        hosts_data[, c("ID", "IPAddress", "OS", "User", "Password")],
        selection = 'single',
        editable = TRUE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = TRUE,
          info = FALSE
        ),
        rownames = FALSE,
        colnames = c("ID", "IP地址", "操作系统", "用户名", "密码")
      )
    }
  })
  
  # 选择主机后填充信息
  observeEvent(input$std_hosts_table_rows_selected, {
    selected_row <- input$std_hosts_table_rows_selected
    hosts_data <- std_hosts_data()
    
    if (length(selected_row) > 0 && !is.null(hosts_data) && nrow(hosts_data) >= selected_row) {
      host <- hosts_data[selected_row, ]
      
      # 更新输入框
      updateTextInput(session, "std_ip", value = host$IPAddress)
      updateTextInput(session, "std_os", value = host$OS)
      updateTextInput(session, "std_user", value = host$User)
      updateTextInput(session, "std_password", value = host$Password)
      
      # 显示通知
      showNotification(sprintf("已选择主机: %s", host$IPAddress), type = "message")
    }
  })
  
  # 添加计算机
  observeEvent(input$std_add, {
    ip <- input$std_ip
    os <- input$std_os
    user <- input$std_user
    password <- input$std_password
    
    # 验证输入
    if (is.null(ip) || ip == "") {
      showNotification("请输入IP地址", type = "error")
      return()
    }
    
    if (is.null(user) || user == "") {
      showNotification("请输入用户名", type = "error")
      return()
    }
    
    if (is.null(password) || password == "") {
      showNotification("请输入密码", type = "error")
      return()
    }
    
    tryCatch({
      # 获取当前数据
      hosts_data <- std_hosts_data()
      if (is.null(hosts_data)) {
        hosts_data <- data.frame(
          ID = 1,
          IPAddress = ip,
          OS = os,
          User = user,
          Password = password,
          stringsAsFactors = FALSE
        )
      } else {
        # 检查IP是否已存在
        if (ip %in% hosts_data$IPAddress) {
          showNotification("该IP地址已存在", type = "error")
          return()
        }
        
        # 添加新行
        new_row <- data.frame(
          ID = max(hosts_data$ID) + 1,
          IPAddress = ip,
          OS = os,
          User = user,
          Password = password,
          stringsAsFactors = FALSE
        )
        hosts_data <- rbind(hosts_data, new_row)
      }
      
      # 存储到响应式值
      std_hosts_data(hosts_data)
      
      # 保存到文件
      write.csv(hosts_data[, -1], file.path(getwd(), "STD", "hosts_new.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      
      # 显示成功通知
      showNotification("计算机添加成功", type = "message")
    }, error = function(e) {
      showNotification(paste("添加计算机失败:", e$message), type = "error")
    })
  })
  
  # 删除计算机
  observeEvent(input$std_delete, {
    selected_row <- input$std_hosts_table_rows_selected
    hosts_data <- std_hosts_data()
    
    if (length(selected_row) > 0 && !is.null(hosts_data) && nrow(hosts_data) >= selected_row) {
      tryCatch({
        # 删除选中行
        hosts_data <- hosts_data[-selected_row, ]
        # 更新ID列
        hosts_data$ID <- 1:nrow(hosts_data)
        # 存储到响应式值
        std_hosts_data(hosts_data)
        # 保存到文件
        write.csv(hosts_data[, -1], file.path(getwd(), "STD", "hosts_new.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        # 显示成功通知
        showNotification("计算机删除成功", type = "message")
      }, error = function(e) {
        showNotification(paste("删除计算机失败:", e$message), type = "error")
      })
    } else {
      showNotification("请先选择要删除的计算机", type = "error")
    }
  })
  
  # 测试连通性
  observeEvent(input$std_ping, {
    ip <- input$std_ip
    
    if (is.null(ip) || ip == "") {
      showNotification("请输入IP地址", type = "error")
      return()
    }
    
    # 执行ping命令
    ping_command <- sprintf("ping %s", ip)
    ping_result <- system(ping_command, intern = TRUE, ignore.stderr = TRUE)
    
    # 显示ping结果
    output$std_output <- renderPrint({
      cat("测试连通性结果:\n")
      cat(ping_result, sep = "\n")
    })
  })
  
  # 显示/隐藏脚本内容
  observeEvent(input$std_show_script, {
    # 切换脚本显示状态
    current_state <- std_script_visible()
    new_state <- !current_state
    std_script_visible(new_state)
    
    # 更新按钮文本
    if (new_state) {
      updateActionButton(session, "std_show_script", label = "关闭显示")
      
      # 显示脚本内容
      script_name <- input$std_script
      
      if (!is.null(script_name) && script_name != "") {
        # 构建脚本路径
        script_path <- file.path(getwd(), "STD", script_name)
        
        # 读取脚本内容
        if (file.exists(script_path)) {
          script_content <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
          
          # 显示脚本内容
          output$std_script_content <- renderUI({
            # 使用语法高亮模块生成带高亮的代码
            generate_highlighted_code(paste(script_content, collapse = '\n'), "powershell")
          })
        } else {
          # 脚本不存在
          output$std_script_content <- renderUI({
            tags$div(
              tags$pre(
                tags$code(
                  paste("错误: 脚本文件不存在", "脚本路径:", script_path, sep = "\n")
                )
              )
            )
          })
        }
      } else {
        # 未选择脚本
        output$std_script_content <- renderUI({
          tags$div(
            tags$pre(
              tags$code("请选择一个脚本以查看其内容")
            )
          )
        })
      }
    } else {
      # 隐藏脚本内容
      updateActionButton(session, "std_show_script", label = "显示脚本")
      output$std_script_content <- renderUI({})
    }
  })
  
  # 执行脚本
  observeEvent(input$std_execute, {
    target_ip <- input$std_ip
    script_name <- input$std_script
    
    if (is.null(target_ip) || target_ip == "") {
      showNotification("请输入IP地址", type = "error")
      return()
    }
    
    if (is.null(script_name) || script_name == "") {
      showNotification("请选择脚本", type = "error")
      return()
    }
    
    # 构建脚本路径
    script_path <- file.path(getwd(), "STD", script_name)
    
    # 执行脚本
    if (file.exists(script_path)) {
      # 根据脚本类型执行不同的命令
      if (endsWith(script_path, ".ps1")) {
        # 执行PowerShell脚本
        script_command <- sprintf("powershell -File %s %s", script_path, target_ip)
      } else if (endsWith(script_path, ".sh")) {
        # 执行bash脚本
        script_command <- sprintf("bash %s %s", script_path, target_ip)
      } else if (endsWith(script_path, ".r")) {
        # 执行R脚本
        script_command <- sprintf("Rscript %s %s", script_path, target_ip)
      } else if (endsWith(script_path, ".bat")) {
        # 执行批处理脚本
        script_command <- sprintf("%s %s", script_path, target_ip)
      }
      
      # 执行命令并获取结果
      script_result <- system(script_command, intern = TRUE, ignore.stderr = TRUE)
      
      # 显示执行结果
      output$std_output <- renderPrint({
        cat("脚本执行结果:\n")
        cat(script_result, sep = "\n")
      })
    } else {
      # 脚本不存在
      output$std_output <- renderPrint({
        cat("错误: 脚本文件不存在\n")
        cat("脚本路径:", script_path, "\n")
      })
    }
  })
}
