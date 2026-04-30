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

    tabsetPanel(id = "std_tabs",

      # ============ 子标签1：计算机标准化（原有功能） ============
      tabPanel("计算机标准化", icon = icon("desktop"),
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
      ),

      # ============ 子标签2：网络巡检 ============
      tabPanel("网络巡检", icon = icon("network-wired"),
        br(),
        fluidRow(
          column(4,
            wellPanel(
              h4("巡检配置"),
              textInput("ni_target", "测试目标（域名/IP）", value = "qq.com",
                placeholder = "例如 qq.com / 8.8.8.8"),
              textInput("ni_domain", "AD域名（可选）", value = "lvcc.org",
                placeholder = "例如 lvcc.org，用于 nltest 域控检测"),
              numericInput("ni_ping_count", "Ping 次数", value = 4, min = 1, max = 20),
              hr(),
              h5("巡检项目"),
              checkboxGroupInput("ni_checks", NULL,
                choices = c(
                  "网卡信息 (ipconfig /all)" = "ipconfig",
                  "连通性测试 (ping)" = "ping",
                  "DNS解析 (nslookup)" = "nslookup",
                  "路由追踪 (tracert)" = "tracert",
                  "域控检测 (nltest)" = "nltest"
                ),
                selected = c("ipconfig", "ping", "nslookup", "tracert", "nltest")),
              hr(),
              div(style = "text-align:center;",
                actionButton("ni_run", "开始巡检", class = "btn-primary btn-lg",
                  icon = icon("play-circle"), style = "width:100%;"),
                br(), br(),
                actionButton("ni_clear", "清空结果", class = "btn-default btn-sm"),
                actionButton("ni_save_log", "保存日志", class = "btn-info btn-sm"))
            ),
            wellPanel(
              h5("历史日志"),
              uiOutput("ni_log_list")
            )
          ),
          column(8,
            div(style = "background:#1e1e1e; color:#d4d4d4; font-family:'Consolas','Courier New',monospace; font-size:13px; padding:15px; border-radius:6px; min-height:500px; max-height:700px; overflow-y:auto;",
              uiOutput("ni_status_bar"),
              htmlOutput("ni_result_output")
            )
          )
        )
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
  
  # 加载主机列表
  observe({
    # 读取hosts_new.csv文件
    hosts_file <- file.path(.std_dir(), "hosts_new.csv")
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
      write.csv(hosts_data[, -1], file.path(.std_dir(), "hosts_new.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      
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
        write.csv(hosts_data[, -1], file.path(.std_dir(), "hosts_new.csv"), row.names = FALSE, fileEncoding = "UTF-8")
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

  # ================================================================
  # 网络巡检模块 Server
  # ================================================================

  # 巡检日志目录
  ni_log_dir <- file.path(getwd(), "Log", "network_inspection")

  # 响应式：存储当前巡检结果（HTML片段列表）
  ni_result_html <- reactiveVal("")
  ni_running <- reactiveVal(FALSE)

  # ---- 辅助：执行单个命令并返回 HTML 格式输出 ----
  ni_exec_cmd <- function(label, cmd) {
    header <- sprintf(
      '<div style="margin-top:12px;margin-bottom:4px;color:#569cd6;font-weight:bold;font-size:14px;">== %s ==</div><div style="color:#808080;font-size:11px;margin-bottom:4px;">$ %s</div>',
      label, gsub("<", "&lt;", gsub(">", "&gt;", cmd)))
    result <- tryCatch({
      # 转义内部双引号，防止与 cmd.exe /C "..." 外层引号冲突
      cmd_safe <- gsub('"', '^"', cmd)
      full_cmd <- sprintf('cmd.exe /C "chcp 65001 >nul & %s"', cmd_safe)
      # suppressWarnings: system(intern=TRUE) 在命令非零退出时会发警告，
      # 但输出仍然有效（如 findstr 未匹配返回1），不能让 warning 吞掉输出
      out <- suppressWarnings(system(full_cmd, intern = TRUE, ignore.stderr = FALSE))
      # 处理编码
      out <- iconv(out, from = "", to = "UTF-8", sub = "byte")
      if (length(out) == 0) out <- "(无输出)"
      out
    }, error = function(e) {
      c(paste("执行失败:", e$message))
    })
    # 转义 HTML 特殊字符
    safe_lines <- gsub("&", "&amp;", result)
    safe_lines <- gsub("<", "&lt;", safe_lines)
    safe_lines <- gsub(">", "&gt;", safe_lines)
    body <- paste0('<pre style="margin:0;padding:8px;background:#252526;border-radius:4px;white-space:pre-wrap;word-break:break-all;color:#d4d4d4;font-size:12px;line-height:1.5;">',
      paste(safe_lines, collapse = "\n"), '</pre>')
    paste0(header, body)
  }

  # ---- 状态栏 ----
  output$ni_status_bar <- renderUI({
    if (ni_running()) {
      div(style = "background:#264f78;color:#fff;padding:8px 12px;border-radius:4px;margin-bottom:10px;font-size:13px;",
        icon("spinner", class = "fa-spin"), " 巡检执行中，请稍候...")
    } else if (nchar(ni_result_html()) > 0) {
      div(style = "background:#2d4a2d;color:#6aff6a;padding:8px 12px;border-radius:4px;margin-bottom:10px;font-size:13px;",
        icon("check-circle"), " 巡检完成")
    } else {
      div(style = "background:#333;color:#808080;padding:8px 12px;border-radius:4px;margin-bottom:10px;font-size:13px;",
        icon("info-circle"), " 请配置巡检项目后点击「开始巡检」")
    }
  })

  # ---- 结果输出 ----
  output$ni_result_output <- renderUI({
    HTML(ni_result_html())
  })

  # ---- 开始巡检 ----
  observeEvent(input$ni_run, {
    target <- trimws(input$ni_target)
    domain <- trimws(input$ni_domain)
    checks <- input$ni_checks
    ping_n <- input$ni_ping_count

    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    if (is.null(checks) || length(checks) == 0) {
      showNotification("请至少选择一个巡检项目", type = "error"); return()
    }

    ni_running(TRUE)
    ni_result_html("")

    total_checks <- length(checks)
    html_parts <- ""

    withProgress(message = "巡检执行中...", value = 0, {

      # 时间戳头
      start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      html_parts <<- sprintf(
        '<div style="color:#dcdcaa;font-size:14px;font-weight:bold;margin-bottom:8px;">网络巡检报告</div><div style="color:#808080;font-size:12px;margin-bottom:4px;">开始时间: %s</div><div style="color:#808080;font-size:12px;margin-bottom:10px;">测试目标: %s</div>',
        start_time, gsub("<", "&lt;", target))

      step <- 0

      # 依次执行选中的巡检项
      if ("ipconfig" %in% checks) {
        step <- step + 1
        incProgress(step / total_checks, detail = "网卡信息")
        html_parts <<- paste0(html_parts, ni_exec_cmd(
          "网卡信息",
          'ipconfig /all | findstr /i "v4 Host Servers"'))
      }
      if ("ping" %in% checks) {
        step <- step + 1
        incProgress(step / total_checks, detail = sprintf("ping %s", target))
        html_parts <<- paste0(html_parts, ni_exec_cmd(
          sprintf("连通性测试 - ping %s", target),
          sprintf("ping %s -n %d", target, ping_n)))
      }
      if ("nslookup" %in% checks) {
        step <- step + 1
        incProgress(step / total_checks, detail = sprintf("nslookup %s", target))
        html_parts <<- paste0(html_parts, ni_exec_cmd(
          sprintf("DNS解析 - nslookup %s", target),
          sprintf("nslookup %s", target)))
      }
      if ("tracert" %in% checks) {
        step <- step + 1
        incProgress(step / total_checks, detail = sprintf("tracert %s (较慢)", target))
        html_parts <<- paste0(html_parts, ni_exec_cmd(
          sprintf("路由追踪 - tracert %s", target),
          sprintf("tracert -d -h 15 %s", target)))
      }
      if ("nltest" %in% checks) {
        dc <- if (!is.null(domain) && domain != "") domain else "lvcc.org"
        step <- step + 1
        incProgress(step / total_checks, detail = sprintf("nltest %s", dc))
        html_parts <<- paste0(html_parts, ni_exec_cmd(
          sprintf("域控检测 - nltest /dsgetdc:%s", dc),
          sprintf("nltest /dsgetdc:%s", dc)))
      }

      # 结束时间戳
      end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      html_parts <<- paste0(html_parts, sprintf(
        '<div style="color:#808080;font-size:12px;margin-top:12px;border-top:1px solid #444;padding-top:8px;">结束时间: %s</div>', end_time))
    })

    ni_result_html(html_parts)
    ni_running(FALSE)
    showNotification("网络巡检完成", type = "message")
  })

  # ---- 清空结果 ----
  observeEvent(input$ni_clear, {
    ni_result_html("")
  })

  # ---- 保存日志 ----
  observeEvent(input$ni_save_log, {
    html_content <- ni_result_html()
    if (is.null(html_content) || nchar(html_content) == 0) {
      showNotification("没有巡检结果可保存", type = "warning"); return()
    }
    tryCatch({
      if (!dir.exists(ni_log_dir)) dir.create(ni_log_dir, recursive = TRUE)
      # 纯文本版日志（去掉HTML标签）
      plain <- gsub("<[^>]+>", "", html_content)
      plain <- gsub("&amp;", "&", plain)
      plain <- gsub("&lt;", "<", plain)
      plain <- gsub("&gt;", ">", plain)
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      target_safe <- gsub("[^a-zA-Z0-9._-]", "_", input$ni_target)
      filename <- sprintf("ni_%s_%s.log", ts, target_safe)
      filepath <- file.path(ni_log_dir, filename)
      writeLines(plain, filepath, useBytes = TRUE)
      # 同时写 HTML 版
      html_filename <- sprintf("ni_%s_%s.html", ts, target_safe)
      html_filepath <- file.path(ni_log_dir, html_filename)
      html_full <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>网络巡检 ', ts,
        '</title><style>body{background:#1e1e1e;color:#d4d4d4;font-family:Consolas,monospace;padding:20px;}</style></head><body>',
        html_content, '</body></html>')
      writeLines(html_full, html_filepath, useBytes = TRUE)

      showNotification(sprintf("日志已保存: %s", filename), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("保存失败:", e$message), type = "error")
    })
  })

  # ---- 历史日志列表 ----
  output$ni_log_list <- renderUI({
    # 响应刷新
    input$ni_save_log; input$ni_run
    if (!dir.exists(ni_log_dir)) {
      return(div(style = "color:#999;font-size:12px;", "暂无历史日志"))
    }
    logs <- list.files(ni_log_dir, pattern = "\\.log$", full.names = FALSE)
    if (length(logs) == 0) {
      return(div(style = "color:#999;font-size:12px;", "暂无历史日志"))
    }
    logs <- sort(logs, decreasing = TRUE)
    # 最多显示最近20条
    logs <- head(logs, 20)
    log_items <- lapply(logs, function(f) {
      # 从文件名提取日期
      ts_part <- sub("^ni_", "", sub("_[^_]+\\.log$", "", f))
      ts_display <- tryCatch(
        format(as.POSIXct(ts_part, format = "%Y%m%d_%H%M%S"), "%m-%d %H:%M"),
        error = function(e) ts_part)
      target_part <- sub("^ni_[0-9_]+_", "", sub("\\.log$", "", f))
      div(style = "padding:4px 0;border-bottom:1px solid #eee;font-size:12px;",
        tags$a(href = "#", class = "ni-log-link", `data-file` = f,
          style = "color:#337ab7;cursor:pointer;",
          sprintf("%s [%s]", ts_display, target_part)))
    })
    tagList(
      tags$script(HTML("
        $(document).on('click', '.ni-log-link', function(e) {
          e.preventDefault();
          Shiny.setInputValue('ni_load_log', $(this).data('file'), {priority:'event'});
        });
      ")),
      do.call(tagList, log_items)
    )
  })

  # ---- 加载历史日志 ----
  observeEvent(input$ni_load_log, {
    filename <- input$ni_load_log
    # 优先加载 HTML 版
    html_name <- sub("\\.log$", ".html", filename)
    html_path <- file.path(ni_log_dir, html_name)
    log_path <- file.path(ni_log_dir, filename)
    if (file.exists(html_path)) {
      content <- readLines(html_path, warn = FALSE, encoding = "UTF-8")
      # 提取 body 内容
      full <- paste(content, collapse = "\n")
      body_match <- regmatches(full, regexpr("<body>.*</body>", full))
      if (length(body_match) > 0) {
        body <- sub("^<body>", "", sub("</body>$", "", body_match[1]))
        ni_result_html(body)
      } else {
        ni_result_html(full)
      }
    } else if (file.exists(log_path)) {
      lines <- readLines(log_path, warn = FALSE, encoding = "UTF-8")
      safe <- gsub("&", "&amp;", lines)
      safe <- gsub("<", "&lt;", safe)
      safe <- gsub(">", "&gt;", safe)
      ni_result_html(paste0(
        '<pre style="margin:0;padding:8px;color:#d4d4d4;font-size:12px;white-space:pre-wrap;">',
        paste(safe, collapse = "\n"), '</pre>'))
    } else {
      showNotification("日志文件不存在", type = "error")
    }
  })
}
