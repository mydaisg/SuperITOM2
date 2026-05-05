# 测试模块（网络巡检）
# 独立一级模块，逐条命令实时显示结果
# 参照客户端网络测试命令V3

# UI部分
network_test_ui <- function() {
  fluidPage(
    titlePanel("网络测试"),
    br(),
    fluidRow(
      column(4,
        wellPanel(
          h4("测试配置"),
          textInput("nt_target", "测试目标（域名/IP）", value = "qq.com"),
          textInput("nt_domain", "AD域名（可选）", value = "lvcc.org"),
          textInput("nt_http_target", "HTTP测试目标", value = "www.baidu.com"),
          numericInput("nt_ping_count", "Ping 次数", value = 4, min = 1, max = 20),
          hr(),
          h5("测试项目"),
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
          actionButton("nt_save_log", "保存日志", class = "btn-info btn-sm"),
          actionButton("nt_clear", "清空结果", class = "btn-default btn-sm")
        )
      ),
      column(8,
        h4("测试结果"),
        verbatimTextOutput("nt_output")
      )
    )
  )
}

# Server部分
network_test_server <- function(input, output, session) {

  # 日志保存目录
  nt_log_dir <- file.path(getwd(), "Log", "network_test")

  # 累计显示的文本
  nt_result <- reactiveVal("")

  # 待执行的命令队列 list of list(label, cmd)
  nt_queue <- reactiveVal(list())

  # 是否正在执行中
  nt_running <- reactiveVal(FALSE)

  # 执行单条命令，返回格式化文本
  nt_exec_cmd <- function(label, cmd) {
    header <- sprintf("\n== %s ==\n$ %s\n\n", label, cmd)
    cmd_safe <- gsub('"', '^"', cmd)
    full_cmd <- sprintf('cmd.exe /C "chcp 65001 >nul & %s"', cmd_safe)
    out <- tryCatch(
      suppressWarnings(system(full_cmd, intern = TRUE, ignore.stderr = FALSE)),
      error = function(e) paste("执行失败:", e$message)
    )
    out <- iconv(out, from = "", to = "UTF-8", sub = "byte")
    if (length(out) == 0) out <- "(无输出)"
    paste0(header, paste(out, collapse = "\n"), "\n")
  }

  # 逐条执行队列中的命令
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

    result_text <- nt_exec_cmd(task$label, task$cmd)
    nt_result(paste0(nt_result(), result_text))

    invalidateLater(50, session)
  })

  # 显示累计结果
  output$nt_output <- renderText({
    nt_result()
  })

  # 构建命令的辅助函数（顺序参照V3脚本）
  .build_cmd_ipconfig <- function() {
    list(label = "网卡信息", cmd = 'ipconfig /all | findstr /i "v4 Host Servers"')
  }
  .build_cmd_ping <- function(target, ping_n) {
    list(label = sprintf("连通性测试 - ping %s", target), cmd = sprintf("ping %s -n %d", target, ping_n))
  }
  .build_cmd_nslookup <- function(target) {
    list(label = sprintf("DNS解析 - nslookup %s", target), cmd = sprintf("nslookup %s", target))
  }
  .build_cmd_nltest <- function(domain) {
    dc <- if (!is.null(domain) && domain != "") domain else "lvcc.org"
    list(label = sprintf("域控检测 - nltest /dsgetdc:%s", dc), cmd = sprintf("nltest /dsgetdc:%s", dc))
  }
  .build_cmd_tracert <- function(target) {
    list(label = sprintf("路由追踪 - tracert %s", target), cmd = sprintf("tracert %s", target))
  }
  .build_cmd_curl <- function(http_target) {
    target <- if (!is.null(http_target) && http_target != "") http_target else "www.baidu.com"
    list(label = sprintf("HTTP测试 - curl -I %s", target), cmd = sprintf("curl -I %s", target))
  }

  # 启动队列执行的通用函数
  .start_test <- function(cmd_list) {
    if (nt_running()) {
      showNotification("测试正在执行中，请等待完成", type = "warning")
      return()
    }
    target <- trimws(input$nt_target)
    header_text <- paste0(
      "========================================\n",
      "       网络测试报告 (V3)\n",
      "========================================\n",
      sprintf("开始时间: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("测试目标: %s\n", target),
      "========================================\n"
    )
    nt_result(header_text)
    nt_queue(cmd_list)
    nt_running(TRUE)
  }

  # 全部测试（V3顺序：ipconfig → ping → nslookup → nltest → tracert → curl）
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
    .start_test(cmd_list)
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
    .start_test(list(.build_cmd_ping(target, input$nt_ping_count)))
  })

  # 单项测试：nslookup
  observeEvent(input$nt_run_nslookup, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_nslookup(target)))
  })

  # 单项测试：nltest
  observeEvent(input$nt_run_nltest, {
    domain <- trimws(input$nt_domain)
    .start_test(list(.build_cmd_nltest(domain)))
  })

  # 单项测试：tracert
  observeEvent(input$nt_run_tracert, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_tracert(target)))
  })

  # 单项测试：curl
  observeEvent(input$nt_run_curl, {
    http_target <- trimws(input$nt_http_target)
    .start_test(list(.build_cmd_curl(http_target)))
  })

  # 清空
  observeEvent(input$nt_clear, {
    if (nt_running()) {
      nt_queue(list())
      nt_running(FALSE)
    }
    nt_result("")
  })

  # 保存日志
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
