# 测试模块（网络巡检）
# 独立一级模块，逐条命令实时显示结果
# 带彩虹风格颜色显示

# UI部分
network_test_ui <- function() {
  fluidPage(
    # 自定义颜色样式
    tags$head(
      tags$style(HTML("
        .nt-result {
          font-family: 'Consolas', 'Courier New', monospace;
          font-size: 13px;
          line-height: 1.6;
          background-color: #1e1e1e;
          color: #d4d4d4;
          padding: 15px;
          border-radius: 5px;
          white-space: pre-wrap;
          word-wrap: break-word;
          min-height: 200px;
        }
        .nt-header {
          color: #569cd6;
          font-weight: bold;
        }
        .nt-cmd {
          color: #9cdcfe;
        }
        .nt-success {
          color: #4ec9b0;
          font-weight: bold;
        }
        .nt-error {
          color: #f48771;
          font-weight: bold;
        }
        .nt-warning {
          color: #cca700;
          font-weight: bold;
        }
        .nt-info {
          color: #4fc1ff;
        }
        .nt-succeeded {
          color: #6a9955;
          font-weight: bold;
        }
        .nt-failed {
          color: #f14c4c;
          font-weight: bold;
        }
        .nt-ttl {
          color: #b5cea8;
        }
        .nt-time {
          color: #ce9178;
        }
        .nt-separator {
          color: #808080;
        }
        .nt-separator-bold {
          color: #c586c0;
          font-weight: bold;
        }
        .nt-green-bold {
          color: rgb(0, 255, 0);
          font-weight: bold;
        }
        .nt-red-bold {
          color: rgb(255, 0, 0);
          font-weight: bold;
        }
      "))
    ),
    titlePanel("网络测试"),
    br(),
    fluidRow(
      column(4,
        wellPanel(
          h4("测试项目"),
          actionButton("nt_run_all", "全部测试", class = "btn-primary btn-block",
            icon = icon("play-circle")),
          br(),
          actionButton("nt_run_ipconfig", "网卡信息 (ipconfig /all)", class = "btn-default btn-block",
            icon = icon("network-wired")),
          actionButton("nt_run_ping", "连通性测试 (ping)", class = "btn-default btn-block",
            icon = icon("satellite-dish")),
          actionButton("nt_run_nslookup", "DNS解析 (nslookup)", class = "btn-default btn-block",
            icon = icon("search")),
          actionButton("nt_run_nltest", "域控检测 (nltest)", class = "btn-default btn-block",
            icon = icon("server")),
          actionButton("nt_run_tracert", "路由追踪 (tracert)", class = "btn-default btn-block",
            icon = icon("route")),
          actionButton("nt_run_curl", "HTTP测试 (curl)", class = "btn-default btn-block",
            icon = icon("globe")),
          hr(),
          h4("文件服务器"),
          actionButton("nt_run_fileserver1", "文件服务器 #1 (10.10.50.50)", class = "btn-default btn-block",
            icon = icon("folder")),
          actionButton("nt_run_fileserver2", "文件服务器 #2 (10.10.50.150)", class = "btn-default btn-block",
            icon = icon("folder")),
          hr(),
          h4("测试配置"),
          textInput("nt_target", "测试目标（域名/IP）", value = "qq.com"),
          textInput("nt_domain", "AD域名（可选）", value = "lvcc.org"),
          textInput("nt_http_target", "HTTP测试目标", value = "www.baidu.com"),
          numericInput("nt_ping_count", "Ping 次数", value = 4, min = 1, max = 20)
        )
      ),
      column(8,
        div(style = "margin-bottom:10px;",
          actionButton("nt_save_log", "保存日志", class = "btn-info btn-sm", icon = icon("save")),
          actionButton("nt_clear", "清空结果", class = "btn-default btn-sm", icon = icon("trash"))
        ),
        h4("测试结果"),
        div(class = "nt-result", htmlOutput("nt_output"))
      )
    )
  )
}

# 辅助函数：转义HTML特殊字符
.html_escape <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text
}

# 辅助函数：格式化一行文本
.format_line <- function(line) {
  if (is.na(line) || is.null(line)) return("")
  line <- .html_escape(trimws(as.character(line)))
  if (line == "") return("")
  
  # 增强版成功关键字检测（纯绿色加粗）
  if (grepl("successfully|TRUE|0% loss|^OK$|complete[d]?", line, ignore.case = TRUE, perl = TRUE)) {
    # 替换成功关键字为绿色加粗
    line <- gsub("(?i)(successfully|TRUE|0% loss|^OK$|complete|completed)", '<span class="nt-green-bold">\\1</span>', line, perl = TRUE)
    return(line)
  }
  
  # 测试成功标志
  if (grepl("TTL=|ttl=|Reply from|来自 .* 的回复|TcpTestSucceeded.*TRUE|命令成功|PingSucceeded.*True|成功", line, ignore.case = TRUE)) {
    return(paste0('<span class="nt-success">', line, '</span>'))
  }
  
  # 测试失败标志
  if (grepl("失败|错误|Error|Failed|Timed out|超时|无法连接|Access is denied|Request timed|PingSucceeded.*FALSE|TcpTestSucceeded.*FALSE|UnKnown", line, ignore.case = TRUE)) {
    return(paste0('<span class="nt-red-bold">', line, '</span>'))
  }
  
  # TTL相关
  if (grepl("TTL=\\d+|ttl=\\d+|时间[=<].*ms|time[=<].*ms", line, ignore.case = TRUE)) {
    return(paste0('<span class="nt-ttl">', line, '</span>'))
  }
  
  # IP地址
  if (grepl("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", line)) {
    line <- gsub("(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})", '<span class="nt-info">\\1</span>', line)
    return(line)
  }
  
  # 数字
  if (grepl("\\d+", line)) {
    line <- gsub("(\\d+)", '<span class="nt-time">\\1</span>', line)
    return(line)
  }
  
  return(line)
}

# 辅助函数：格式化测试结果为HTML
.format_result_html <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- lines[!is.na(lines)]
  html_lines <- sapply(lines, function(line) {
    if (is.na(line) || is.null(line)) return("")
    # 分隔线
    if (grepl("^=+$", line)) {
      return(paste0('<span class="nt-separator-bold">', .html_escape(line), '</span>'))
    }
    # 命令行 $ 开头
    if (grepl("^\\$ ", line)) {
      cmd_part <- substr(line, 3, nchar(line))
      return(paste0('<span class="nt-cmd">$ ', .html_escape(cmd_part), '</span>'))
    }
    # 标签行 == 开头
    if (grepl("^== .* ==$", line)) {
      return(paste0('<span class="nt-header">', .html_escape(line), '</span>'))
    }
    # 普通行
    .format_line(line)
  })
  paste(html_lines, collapse = "\n")
}

# Server部分
network_test_server <- function(input, output, session) {

  # 日志保存目录
  nt_log_dir <- file.path(getwd(), "Log", "network_test")

  # 累计显示的文本（原始文本，用于保存）
  nt_result <- reactiveVal("")

  # 待执行的命令队列
  nt_queue <- reactiveVal(list())

  # 是否正在执行中
  nt_running <- reactiveVal(FALSE)

  # 执行 Windows 系统命令
  nt_exec_system <- function(label, cmd) {
    header <- sprintf("\n== %s ==\n$ %s\n\n", label, cmd)
    out <- tryCatch({
      result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
      if (length(result) == 0) {
        "(无输出)"
      } else {
        # 编码转换：GBK -> UTF-8
        result <- iconv(result, from = "GBK", to = "UTF-8", sub = "")
        paste(result, collapse = "\n")
      }
    }, error = function(e) {
      paste("执行失败:", e$message)
    })
    paste0(header, out, "\n")
  }

  # 使用 R socketConnection 测试 TCP 端口
  nt_exec_port_test <- function(ip, port) {
    label <- sprintf("文件服务器 %s - 端口 %s", ip, port)
    header <- sprintf("\n== %s ==\n\n", label)
    out <- tryCatch({
      con <- socketConnection(host = ip, port = port, open = "r+b", blocking = TRUE, timeout = 3)
      close(con)
      paste0("ComputerName     : ", ip, "\n",
             "RemoteAddress    : ", ip, "\n",
             "RemotePort       : ", port, "\n",
             "TcpTestSucceeded : TRUE")
    }, error = function(e) {
      err_msg <- iconv(e$message, from = "GBK", to = "UTF-8", sub = "")
      paste0("ComputerName     : ", ip, "\n",
             "RemoteAddress    : ", ip, "\n",
             "RemotePort       : ", port, "\n",
             "TcpTestSucceeded : FALSE\n",
             "Error            : ", err_msg)
    })
    paste0(header, out, "\n")
  }

  # 使用 R 内置 ping
  nt_exec_ping <- function(ip, count = 4) {
    label <- sprintf("文件服务器 %s - Ping", ip)
    header <- sprintf("\n== %s ==\n\n", label)
    out <- tryCatch({
      result <- system(sprintf("ping -n %d %s", count, ip), intern = TRUE, ignore.stderr = TRUE)
      if (length(result) == 0) {
        "(无输出)"
      } else {
        result <- iconv(result, from = "GBK", to = "UTF-8", sub = "")
        paste(result, collapse = "\n")
      }
    }, error = function(e) {
      paste("执行失败:", e$message)
    })
    paste0(header, out, "\n")
  }

  # 逐条执行队列中的任务
  observe({
    if (!nt_running()) return()
    queue <- nt_queue()
    if (length(queue) == 0) {
      end_text <- paste0(
        "========================================\n",
        sprintf("结束时间: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "========================================\n"
      )
      nt_result(paste0(nt_result(), end_text))
      nt_running(FALSE)
      return()
    }

    task <- queue[[1]]
    remaining <- queue[-1]
    nt_queue(remaining)

    result_text <- switch(task$type,
      "system" = nt_exec_system(task$label, task$cmd),
      "port" = nt_exec_port_test(task$ip, task$port),
      "ping" = nt_exec_ping(task$ip, task$count),
      paste("未知任务类型:", task$type)
    )
    nt_result(paste0(nt_result(), result_text))

    invalidateLater(50, session)
  })

  # 显示累计结果（带颜色）
  output$nt_output <- renderUI({
    HTML(.format_result_html(nt_result()))
  })

  # 构建命令的辅助函数
  .build_cmd_ipconfig <- function() {
    list(label = "网卡信息", cmd = 'ipconfig /all', type = "system")
  }
  .build_cmd_ping <- function(target, ping_n) {
    list(label = sprintf("连通性测试 - ping %s", target), cmd = sprintf("ping %s -n %d", target, ping_n), type = "system")
  }
  .build_cmd_nslookup <- function(target) {
    list(label = sprintf("DNS解析 - nslookup %s", target), cmd = sprintf("nslookup %s", target), type = "system")
  }
  .build_cmd_nltest <- function(domain) {
    dc <- if (!is.null(domain) && domain != "") domain else "lvcc.org"
    list(label = sprintf("域控检测 - nltest /dsgetdc:%s", dc), cmd = sprintf("nltest /dsgetdc:%s", dc), type = "system")
  }
  .build_cmd_tracert <- function(target) {
    list(label = sprintf("路由追踪 - tracert %s", target), cmd = sprintf("tracert %s", target), type = "system")
  }
  .build_cmd_curl <- function(http_target) {
    target <- if (!is.null(http_target) && http_target != "") http_target else "www.baidu.com"
    list(label = sprintf("HTTP测试 - curl -I %s", target), cmd = sprintf("curl -I %s", target), type = "system")
  }
  .build_cmd_fileserver <- function(ip) {
    list(label = sprintf("文件服务器 %s - SMB连接测试", ip), cmd = sprintf("net use \\\\%s", ip), type = "system")
  }

  # 启动队列执行的通用函数
  .start_test <- function(cmd_list, custom_target = NULL) {
    if (nt_running()) {
      showNotification("测试正在执行中，请等待完成", type = "warning")
      return()
    }
    target <- trimws(input$nt_target)
    domain <- trimws(input$nt_domain)
    http_target <- trimws(input$nt_http_target)
    
    # 根据测试类型显示对应的目标
    if (!is.null(custom_target) && custom_target != "") {
      target_display <- custom_target
    } else if (grepl("网卡|ipconfig", cmd_list[[1]]$label, ignore.case = TRUE)) {
      target_display <- "(本机网络配置)"
    } else if (grepl("DNS|nslookup", cmd_list[[1]]$label, ignore.case = TRUE)) {
      target_display <- target
    } else if (grepl("域控|nltest", cmd_list[[1]]$label, ignore.case = TRUE)) {
      target_display <- if (domain != "") domain else "(未设置)"
    } else if (grepl("HTTP|curl", cmd_list[[1]]$label, ignore.case = TRUE)) {
      target_display <- http_target
    } else if (grepl("文件服务器|port|ping", cmd_list[[1]]$label, ignore.case = TRUE)) {
      # 提取IP或主机名
      ip_match <- regmatches(cmd_list[[1]]$label, regexpr("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", cmd_list[[1]]$label))
      target_display <- if (length(ip_match) > 0) ip_match[1] else target
    } else {
      target_display <- target
    }
    
    header_text <- paste0(
      "========================================\n",
      "       网络测试报告 (V3)\n",
      "========================================\n",
      sprintf("开始时间: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("测试目标: %s\n", target_display),
      "========================================\n"
    )
    nt_result(header_text)
    nt_queue(cmd_list)
    nt_running(TRUE)
  }

  # 全部测试
  observeEvent(input$nt_run_all, {
    target <- trimws(input$nt_target)
    domain <- trimws(input$nt_domain)
    http_target <- trimws(input$nt_http_target)
    ping_n <- input$nt_ping_count

    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }

    cmd_list <- list(
      .build_cmd_ipconfig(),
      .build_cmd_ping(target, ping_n),
      .build_cmd_nslookup(target),
      .build_cmd_nltest(domain),
      .build_cmd_tracert(target),
      .build_cmd_curl(http_target)
    )
    .start_test(cmd_list, "综合测试")
  })

  # 单项测试：网卡信息
  observeEvent(input$nt_run_ipconfig, {
    .start_test(list(.build_cmd_ipconfig()))
  })

  # 单项测试：ping
  observeEvent(input$nt_run_ping, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_ping(target, input$nt_ping_count)), target)
  })

  # 单项测试：nslookup
  observeEvent(input$nt_run_nslookup, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_nslookup(target)), target)
  })

  # 单项测试：nltest
  observeEvent(input$nt_run_nltest, {
    domain <- trimws(input$nt_domain)
    .start_test(list(.build_cmd_nltest(domain)), domain)
  })

  # 单项测试：tracert
  observeEvent(input$nt_run_tracert, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_tracert(target)), target)
  })

  # 单项测试：curl
  observeEvent(input$nt_run_curl, {
    http_target <- trimws(input$nt_http_target)
    .start_test(list(.build_cmd_curl(http_target)), http_target)
  })

  # 文件服务器 #1 测试
  observeEvent(input$nt_run_fileserver1, {
    ip <- "10.10.50.50"
    cmd_list <- list(
      .build_cmd_fileserver(ip),
      list(label = sprintf("文件服务器 %s - 端口 445", ip), ip = ip, port = 445, type = "port"),
      list(label = sprintf("文件服务器 %s - 端口 139", ip), ip = ip, port = 139, type = "port"),
      list(label = sprintf("文件服务器 %s - 端口 5000", ip), ip = ip, port = 5000, type = "port"),
      list(label = sprintf("文件服务器 %s - Ping", ip), ip = ip, count = 4, type = "ping")
    )
    .start_test(cmd_list, ip)
  })

  # 文件服务器 #2 测试
  observeEvent(input$nt_run_fileserver2, {
    ip <- "10.10.50.150"
    cmd_list <- list(
      .build_cmd_fileserver(ip),
      list(label = sprintf("文件服务器 %s - 端口 445", ip), ip = ip, port = 445, type = "port"),
      list(label = sprintf("文件服务器 %s - 端口 139", ip), ip = ip, port = 139, type = "port"),
      list(label = sprintf("文件服务器 %s - 端口 5000", ip), ip = ip, port = 5000, type = "port"),
      list(label = sprintf("文件服务器 %s - Ping", ip), ip = ip, count = 4, type = "ping")
    )
    .start_test(cmd_list, ip)
  })

  # 清空
  observeEvent(input$nt_clear, {
    if (nt_running()) {
      nt_queue(list())
      nt_running(FALSE)
    }
    nt_result("")
  })

  # 保存日志（保存原始文本）
  observeEvent(input$nt_save_log, {
    current_text <- nt_result()
    if (is.null(current_text) || current_text == "") {
      showNotification("没有可保存的测试结果", type = "warning"); return()
    }

    tryCatch({
      if (!dir.exists(nt_log_dir)) dir.create(nt_log_dir, recursive = TRUE)

      target <- trimws(input$nt_target)
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      target_safe <- gsub("[^a-zA-Z0-9._-]", "_", target)
      filename <- sprintf("nt_%s_%s.log", ts, target_safe)
      filepath <- file.path(nt_log_dir, filename)
      writeLines(current_text, filepath, useBytes = TRUE)
      showNotification(sprintf("日志已保存: %s", filename), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("保存失败:", e$message), type = "error")
    })
  })
}
