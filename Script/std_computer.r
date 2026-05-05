# 标准化模块功能

# 加载语法高亮模块
source("Script/high_light.r")

# 从配置获取 STD 目录路径（若配置未加载则回退到默认）
.std_dir <- function() {
  if (exists("get_std_dir")) get_std_dir() else file.path(getwd(), "STD")
}

# 从配置获取平台命令
.ping_cmd <- function() {
  if (exists("get_ping_cmd")) get_ping_cmd() else "ping"
}

.powershell_cmd <- function() {
  if (exists("get_powershell_cmd")) get_powershell_cmd() else "powershell"
}

.bash_cmd <- function() {
  if (exists("get_bash_cmd")) get_bash_cmd() else "bash"
}

.rscript_cmd <- function() {
  if (exists("get_rscript_cmd")) get_rscript_cmd() else "Rscript"
}

.os <- function() {
  if (exists("get_os")) get_os() else "windows"
}

# UI部分
std_ui <- function() {
  fluidPage(
    # 添加语法高亮资源
    high_light_ui(),

    br(),
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
  # 刷新触发器
  std_refresh <- reactiveVal(0)
  # 创建响应式值来存储脚本显示状态
  std_script_visible <- reactiveVal(FALSE)
  
  # 从数据库加载主机列表的辅助函数
  .load_hosts <- function() {
    con <- db_connect()
    on.exit(db_disconnect(con))
    dbGetQuery(con, "SELECT id, ip_address, os, username, password, computer_name FROM std_hosts ORDER BY id")
  }
  
  # 加载脚本列表
  output$std_script_ui <- renderUI({
    # 获取STD目录下的脚本文件
    std_dir <- .std_dir()
    
    if (dir.exists(std_dir)) {
      # 列出脚本文件
      script_files <- list.files(std_dir, pattern = "\\.(ps1|sh|r|bat)$", full.names = FALSE)
      # 生成脚本选择框
      selectInput("std_script", "选择脚本", choices = script_files)
    } else {
      selectInput("std_script", "选择脚本", choices = c("无可用脚本"))
    }
  })
  
  # 加载主机列表（从数据库）
  observe({
    std_refresh()
    hosts_data <- .load_hosts()
    std_hosts_data(hosts_data)
  })
  
  # 渲染主机数据表格
  output$std_hosts_table <- DT::renderDataTable({
    hosts_data <- std_hosts_data()
    if (!is.null(hosts_data) && nrow(hosts_data) > 0) {
      display_data <- hosts_data[, c("id", "ip_address", "os", "username", "password")]
      DT::datatable(
        display_data,
        selection = 'single',
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
      updateTextInput(session, "std_ip", value = host$ip_address)
      updateTextInput(session, "std_os", value = host$os)
      updateTextInput(session, "std_user", value = host$username)
      updateTextInput(session, "std_password", value = host$password)
      updateTextInput(session, "std_new_name", value = if (!is.null(host$computer_name)) host$computer_name else "")
      
      # 显示通知
      showNotification(sprintf("已选择主机: %s", host$ip_address), type = "message")
    }
  })
  
  # 添加计算机
  observeEvent(input$std_add, {
    ip <- trimws(input$std_ip)
    os <- trimws(input$std_os)
    user <- trimws(input$std_user)
    password <- trimws(input$std_password)
    computer_name <- trimws(input$std_new_name)
    
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
      con <- db_connect()
      on.exit(db_disconnect(con))
      
      # 检查IP是否已存在
      existing <- dbGetQuery(con, "SELECT id FROM std_hosts WHERE ip_address = ?", list(ip))
      if (nrow(existing) > 0) {
        showNotification("该IP地址已存在", type = "error")
        return()
      }
      
      # 插入新记录
      dbExecute(con, "INSERT INTO std_hosts (ip_address, os, username, password, computer_name) VALUES (?, ?, ?, ?, ?)",
                list(ip, os, user, password, computer_name))
      
      # 刷新列表
      std_refresh(std_refresh() + 1)
      
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
        host_id <- hosts_data[selected_row, "id"]
        con <- db_connect()
        on.exit(db_disconnect(con))
        dbExecute(con, "DELETE FROM std_hosts WHERE id = ?", list(host_id))
        
        # 刷新列表
        std_refresh(std_refresh() + 1)
        
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
    ping_cmd <- .ping_cmd()
    ping_command <- sprintf("%s %s 4 %s", ping_cmd, ifelse(.os() == "windows", "-n", "-c"), ip)
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
        script_path <- file.path(.std_dir(), script_name)
        
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
    script_path <- file.path(.std_dir(), script_name)
    
    # 执行脚本
    if (file.exists(script_path)) {
      # 根据脚本类型执行不同的命令
      if (endsWith(script_path, ".ps1")) {
        # 执行PowerShell脚本
        script_command <- sprintf("%s -File %s %s", .powershell_cmd(), script_path, target_ip)
      } else if (endsWith(script_path, ".sh")) {
        # 执行bash脚本
        script_command <- sprintf("%s %s %s", .bash_cmd(), script_path, target_ip)
      } else if (endsWith(script_path, ".r")) {
        # 执行R脚本
        script_command <- sprintf("%s %s %s", .rscript_cmd(), script_path, target_ip)
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
