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
      column(3,
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
          h4("邮箱测试"),
          actionButton("nt_run_email_all", "邮箱诊断", class = "btn-warning btn-block",
            icon = icon("envelope")),
          br(),
          actionButton("nt_run_email_mx", "MX记录查询", class = "btn-default btn-block",
            icon = icon("server")),
          actionButton("nt_run_email_a", "A记录(邮件子域)", class = "btn-default btn-block",
            icon = icon("globe")),
          actionButton("nt_run_email_smtp", "SMTP端口(25/465/587)", class = "btn-default btn-block",
            icon = icon("plug")),
          actionButton("nt_run_email_pop3", "POP3端口(110/995)", class = "btn-default btn-block",
            icon = icon("inbox")),
          actionButton("nt_run_email_imap", "IMAP端口(143/993)", class = "btn-default btn-block",
            icon = icon("cloud-download-alt")),
          actionButton("nt_run_email_spf", "SPF记录检查", class = "btn-default btn-block",
            icon = icon("shield-alt")),
          actionButton("nt_run_email_dkim", "DKIM记录检查", class = "btn-default btn-block",
            icon = icon("key")),
          actionButton("nt_run_email_dmarc", "DMARC记录检查", class = "btn-default btn-block",
            icon = icon("flag")),
          actionButton("nt_run_email_ptr", "PTR反向解析", class = "btn-default btn-block",
            icon = icon("exchange-alt")),
          hr(),
          h4("应用系统测试"),
          div(style = "margin-bottom:4px;",
            textInput("nt_app_name", "服务器描述", value = "LVCC协同平台-前端服务器", placeholder = "例如：协同平台前端"),
            textInput("nt_app_domain", "域名", value = "ecs.Lvcchong.com", placeholder = "ecs.Lvcchong.com"),
            textInput("nt_app_ip", "IP地址", value = "117.162.0.171", placeholder = "117.162.0.171"),
            textInput("nt_app_url", "URL", value = "http://ecs.Lvcchong.com:20600", placeholder = "http://ecs.Lvcchong.com:20600"),
            numericInput("nt_app_port", "端口", value = 20600, min = 1, max = 65535)
          ),
          # 预设按钮
          actionButton("nt_run_app_ecs", "协同平台-前端", class = "btn-success btn-block",
            icon = icon("server")),
          actionButton("nt_run_app_custom", "自定义测试 (使用上方参数)", class = "btn-default btn-block",
            icon = icon("play")),
          hr(),
          h4("测试配置"),
          textInput("nt_target", "测试目标（域名/IP）", value = "qq.com"),
          textInput("nt_domain", "AD域名（可选）", value = "lvcc.org"),
          textInput("nt_http_target", "HTTP测试目标", value = "www.baidu.com"),
          textInput("nt_email_domain", "邮箱域名", value = "CNLVCC.Com"),
          numericInput("nt_ping_count", "Ping 次数", value = 4, min = 1, max = 20)
        )
      ),
      column(9,
        div(style = "margin-bottom:10px;",
          actionButton("nt_save_log", "保存日志", class = "btn-info btn-sm", icon = icon("save")),
          actionButton("nt_load_log", "历史日志", class = "btn-default btn-sm", icon = icon("folder-open")),
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
  text <- enc2utf8(text)  # 确保 UTF-8
  lines <- suppressWarnings(strsplit(text, "\n", fixed = TRUE)[[1]])
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
network_test_server <- function(input, output, session, rv = NULL) {

  # 日志保存目录
  nt_log_dir <- file.path(getwd(), "Log", "network_test")

  # 累计显示的文本（原始文本，用于保存）
  nt_result <- reactiveVal("")

  # 当前测试名称（用于自动保存日志文件名）
  nt_test_name <- reactiveVal("")

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
        # iconv removed
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
      con <- suppressWarnings(socketConnection(host = ip, port = port, open = "r+b", blocking = TRUE, timeout = 3))
      close(con)
      paste0("ComputerName     : ", ip, "\n",
             "RemoteAddress    : ", ip, "\n",
             "RemotePort       : ", port, "\n",
             "TcpTestSucceeded : TRUE")
    }, error = function(e) {
      err_msg <- e$message  # iconv removed
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
        # iconv removed
        paste(result, collapse = "\n")
      }
    }, error = function(e) {
      paste("执行失败:", e$message)
    })
    paste0(header, out, "\n")
  }

  # ========== 邮箱诊断函数 ==========

  # MX 记录查询
  nt_exec_mx <- function(domain) {
    label <- sprintf("MX记录查询 - %s", domain)
    header <- sprintf("\n== %s ==\n\n", label)
    result <- system(sprintf("nslookup -type=mx %s", domain), intern = TRUE, ignore.stderr = TRUE)
    # iconv removed
    paste0(header, paste(result, collapse = "\n"), "\n")
  }

  # A 记录查询（邮件子域名: mail, smtp, pop, imap）
  nt_exec_a_record <- function(domain) {
    subdomains <- c("mail", "smtp", "pop", "imap", paste0("mail.",domain))
    results <- ""
    # 先查主域名 MX 指向的域名
    mx_result <- system(sprintf("nslookup -type=mx %s", domain), intern = TRUE, ignore.stderr = TRUE)
    # iconv removed
    mx_lines <- mx_result[grepl("mail exchanger", mx_result, ignore.case=TRUE)]
    mx_targets <- gsub(".*= ", "", mx_lines)
    mx_targets <- trimws(gsub("\\s+", " ", mx_targets))
    if (length(mx_targets) > 0) {
      results <- paste0(results, sprintf("MX目标服务器:\n%s\n\n", paste(mx_targets, collapse="\n")))
      for (mt in mx_targets) {
        parts <- trimws(strsplit(mt, " ")[[1]])
        mx_host <- parts[length(parts)]
        label <- sprintf("A记录 - MX目标: %s", mx_host)
        header <- sprintf("== %s ==\n$ nslookup %s\n\n", label, mx_host)
        a_result <- system(sprintf("nslookup %s", mx_host), intern = TRUE, ignore.stderr = TRUE)
        # iconv removed
        results <- paste0(results, header, paste(a_result, collapse = "\n"), "\n\n")
      }
    }
    # 查常见邮件子域名
    for (sd in subdomains) {
      if (sd == paste0("mail.",domain)) next
      label <- sprintf("A记录 - 子域: %s.%s", sd, domain)
      header <- sprintf("== %s ==\n$ nslookup %s.%s\n\n", label, sd, domain)
      a_result <- system(sprintf("nslookup %s.%s", sd, domain), intern = TRUE, ignore.stderr = TRUE)
      # iconv removed
      results <- paste0(results, header, paste(a_result, collapse = "\n"), "\n\n")
    }
    results
  }

  # DNS TXT 记录查询（SPF/DKIM/DMARC）
  nt_exec_txt <- function(query, label_prefix) {
    label <- sprintf("%s - %s", label_prefix, query)
    header <- sprintf("\n== %s ==\n\n", label)
    result <- system(sprintf("nslookup -type=txt %s", query), intern = TRUE, ignore.stderr = TRUE)
    # iconv removed
    out <- paste(result, collapse = "\n")
    # 提取 TXT 记录值
    txt_match <- regmatches(out, gregexpr('"([^"]+)"', out))[[1]]
    txt_vals <- gsub('"', '', txt_match)
    if (length(txt_vals) > 0) {
      out <- paste0(out, "\n--- 解析结果 ---\n", paste(txt_vals, collapse = "\n"))
    }
    paste0(header, out, "\n")
  }

  # PTR 反向解析
  nt_exec_ptr <- function(domain) {
    label <- sprintf("PTR反向解析 - %s", domain)
    header <- sprintf("\n== %s ==\n\n", label)
    mx_result <- system(sprintf("nslookup -type=mx %s", domain), intern = TRUE, ignore.stderr = TRUE)
    # iconv removed
    mx_lines <- mx_result[grepl("mail exchanger", mx_result, ignore.case=TRUE)]
    mx_targets <- gsub(".*= ", "", mx_lines)
    mx_targets <- trimws(gsub("\\s+", " ", mx_targets))
    if (length(mx_targets) == 0) {
      return(paste0(header, "--- PTR反向解析 ---\n未能获取到 ", domain, " 的MX记录，无法反向解析\n"))
    }
    results <- ""
    for (mt in mx_targets) {
      parts <- trimws(strsplit(mt, " ")[[1]])
      mx_host <- parts[length(parts)]
      ip_result <- system(sprintf("nslookup %s", mx_host), intern = TRUE, ignore.stderr = TRUE)
      # iconv removed
      ip_lines <- ip_result[grepl("Address", ip_result, ignore.case=TRUE) & !grepl("#", ip_result)]
      ips <- unique(unlist(regmatches(ip_lines, gregexpr("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", ip_lines))))
      results <- paste0(results, sprintf("MX: %s\n", mx_host))
      for (ip in ips) {
        ptr <- system(sprintf("nslookup %s", ip), intern = TRUE, ignore.stderr = TRUE)
        # iconv removed
        ptr_name_lines <- ptr[grepl("name\\s*=", ptr, ignore.case=TRUE) | grepl("名称", ptr)]
        if (length(ptr_name_lines) > 0) {
          pname <- trimws(gsub(".*name\\s*=\\s*|.*名称\\s*:\\s*", "", ptr_name_lines[1], ignore.case=TRUE))
          match <- if (grepl(gsub("\\.$","",domain), pname, ignore.case=TRUE)) " ✓ 匹配域名" else " ✗ 不匹配域名"
          results <- paste0(results, sprintf("  %-15s → %s%s\n", ip, pname, match))
        } else {
          results <- paste0(results, sprintf("  %-15s → (无PTR记录)\n", ip))
        }
      }
    }
    paste0(header, results, "--- PTR解析完成 ---\n")
  }

  # 邮件端口测试（通用：SMTP/POP3/IMAP）
  nt_exec_mail_ports <- function(domain, protocol = "smtp") {
    port_map <- list(
      smtp = c(25, 465, 587),
      pop3  = c(110, 995),
      imap  = c(143, 993)
    )
    ports <- port_map[[protocol]] %||% c(25, 465, 587)
    proto_name <- toupper(protocol)
    results <- ""
    for (p in ports) {
      label <- sprintf("%s端口测试 - %s:%d", proto_name, domain, p)
      header <- sprintf("\n== %s ==\n\n", label)
      tls_label <- if (p %in% c(465, 995, 993)) " (TLS加密)" else ""
      protocol_label <- switch(as.character(p),
        "25"="SMTP", "465"="SMTPS", "587"="SMTP Submission",
        "110"="POP3", "995"="POP3S",
        "143"="IMAP", "993"="IMAPS",
        as.character(p))
      res <- tryCatch({
        con <- suppressWarnings(socketConnection(host = domain, port = p, open = "r+b", blocking = TRUE, timeout = 5))
        close(con)
        sprintf("Port %-4d : OPEN    [%s]%s", p, protocol_label, tls_label)
      }, error = function(e) {
        err <- e$message  # iconv removed
        err_short <- substr(err, 1, 40)
        sprintf("Port %-4d : CLOSED  [%s] (%s)", p, protocol_label, err_short)
      })
      results <- paste0(results, header, res, "\n")
    }
    results
  }

  # 后台进程句柄（callr::r_bg 异步执行，不阻塞UI）
  nt_bg_job <- reactiveVal(NULL)

  # 在后台R进程中执行的完整任务函数
  .run_task <- function(task) {
    Sys.setlocale("LC_CTYPE", "Chinese")
    switch(task$type,
      "system" = {
        out <- tryCatch({
          result <- system(task$cmd, intern = TRUE, ignore.stderr = TRUE)
          if (length(result) == 0) "(无输出)" else {
            result <- iconv(result, from="", to="UTF-8", sub="")
            paste(result, collapse = "\n")
          }
        }, error = function(e) paste("执行失败:", e$message))
        paste0(sprintf("\n== %s ==\n$ %s\n\n", task$label, task$cmd), out, "\n")
      },
      "port" = {
        header <- sprintf("\n== 文件服务器 %s - 端口 %s ==\n\n", task$ip, task$port)
        out <- tryCatch({
          con <- suppressWarnings(socketConnection(host = task$ip, port = as.integer(task$port), open = "r+b", blocking = TRUE, timeout = 3))
          close(con)
          paste0("ComputerName     : ", task$ip, "\nRemoteAddress    : ", task$ip, "\nRemotePort       : ", task$port, "\nTcpTestSucceeded : TRUE")
        }, error = function(e) {
          err_msg <- e$message  # iconv removed
          paste0("ComputerName     : ", task$ip, "\nRemoteAddress    : ", task$ip, "\nRemotePort       : ", task$port, "\nTcpTestSucceeded : FALSE\nError            : ", err_msg)
        })
        paste0(header, out, "\n")
      },
      "ping" = {
        header <- sprintf("\n== 文件服务器 %s - Ping ==\n\n", task$ip)
        out <- tryCatch({
          result <- system(sprintf("ping -n %d %s", as.integer(task$count), task$ip), intern = TRUE, ignore.stderr = TRUE)
          if (length(result) == 0) "(无输出)" else {
            result <- iconv(result, from="", to="UTF-8", sub="")
            paste(result, collapse = "\n")
          }
        }, error = function(e) paste("执行失败:", e$message))
        paste0(header, out, "\n")
      },
      "email_mx" = {
        header <- sprintf("\n== MX记录查询 - %s ==\n\n", task$domain)
        out <- system(sprintf("nslookup -type=mx %s", task$domain), intern = TRUE, ignore.stderr = TRUE)
        result <- iconv(result, from="", to="UTF-8", sub="")
        paste0(header, paste(out, collapse = "\n"), "\n")
      },
      "email_a" = {
        domain <- task$domain
        subdomains <- c("mail", "smtp", "pop", "imap", paste0("mail.", domain))
        results <- ""
        mx_result <- system(sprintf("nslookup -type=mx %s", domain), intern = TRUE, ignore.stderr = TRUE)
        result <- iconv(result, from="", to="UTF-8", sub="")
        mx_lines <- mx_result[grepl("mail exchanger", mx_result, ignore.case = TRUE)]
        mx_targets <- trimws(gsub(".*= ", "", mx_lines))
        if (length(mx_targets) > 0) {
          results <- paste0(results, sprintf("MX目标服务器:\n%s\n\n", paste(mx_targets, collapse = "\n")))
          for (mt in mx_targets) {
            parts <- trimws(strsplit(mt, " ")[[1]])
            mx_host <- parts[length(parts)]
            label <- sprintf("A记录 - MX目标: %s", mx_host)
            a_result <- system(sprintf("nslookup %s", mx_host), intern = TRUE, ignore.stderr = TRUE)
            result <- iconv(result, from="", to="UTF-8", sub="")
            results <- paste0(results, sprintf("== %s ==\n$ nslookup %s\n\n", label, mx_host), paste(a_result, collapse = "\n"), "\n\n")
          }
        }
        for (sd in subdomains) {
          if (sd == paste0("mail.", domain)) next
          label <- sprintf("A记录 - 子域: %s.%s", sd, domain)
          a_result <- system(sprintf("nslookup %s.%s", sd, domain), intern = TRUE, ignore.stderr = TRUE)
          result <- iconv(result, from="", to="UTF-8", sub="")
          results <- paste0(results, sprintf("== %s ==\n$ nslookup %s.%s\n\n", label, sd, domain), paste(a_result, collapse = "\n"), "\n\n")
        }
        results
      },
      "email_ptr" = {
        domain <- task$domain
        header <- sprintf("\n== PTR反向解析 - %s ==\n\n", domain)
        mx_result <- system(sprintf("nslookup -type=mx %s", domain), intern = TRUE, ignore.stderr = TRUE)
        result <- iconv(result, from="", to="UTF-8", sub="")
        mx_lines <- mx_result[grepl("mail exchanger", mx_result, ignore.case = TRUE)]
        mx_targets <- trimws(gsub(".*= ", "", mx_lines))
        if (length(mx_targets) == 0) return(paste0(header, "未能获取MX记录\n"))
        results <- ""
        for (mt in mx_targets) {
          parts <- trimws(strsplit(mt, " ")[[1]]); mx_host <- parts[length(parts)]
          ip_result <- system(sprintf("nslookup %s", mx_host), intern = TRUE, ignore.stderr = TRUE)
          result <- iconv(result, from="", to="UTF-8", sub="")
          ip_lines <- ip_result[grepl("Address", ip_result, ignore.case = TRUE) & !grepl("#", ip_result)]
          ips <- unique(unlist(regmatches(ip_lines, gregexpr("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", ip_lines))))
          results <- paste0(results, sprintf("MX: %s\n", mx_host))
          for (ip in ips) {
            ptr <- system(sprintf("nslookup %s", ip), intern = TRUE, ignore.stderr = TRUE)
            result <- iconv(result, from="", to="UTF-8", sub="")
            ptr_name_lines <- ptr[grepl("name\\s*=", ptr, ignore.case = TRUE) | grepl("名称", ptr)]
            if (length(ptr_name_lines) > 0) {
              pname <- trimws(gsub(".*name\\s*=\\s*|.*名称\\s*:\\s*", "", ptr_name_lines[1], ignore.case = TRUE))
              match <- if (grepl(gsub("\\.$", "", domain), pname, ignore.case = TRUE)) " \u2713 匹配" else " \u2717 不匹配"
              results <- paste0(results, sprintf("  %-15s \u2192 %s%s\n", ip, pname, match))
            } else {
              results <- paste0(results, sprintf("  %-15s \u2192 (无PTR记录)\n", ip))
            }
          }
        }
        paste0(header, results, "--- PTR解析完成 ---\n")
      },
      "email_txt" = {
        header <- sprintf("\n== %s - %s ==\n\n", task$label_prefix, task$query)
        out <- system(sprintf("nslookup -type=txt %s", task$query), intern = TRUE, ignore.stderr = TRUE)
        result <- iconv(result, from="", to="UTF-8", sub="")
        out <- paste(out, collapse = "\n")
        txt_match <- regmatches(out, gregexpr('"([^"]+)"', out))[[1]]
        txt_vals <- gsub('"', '', txt_match)
        if (length(txt_vals) > 0) out <- paste0(out, "\n--- 解析结果 ---\n", paste(txt_vals, collapse = "\n"))
        paste0(header, out, "\n")
      },
      "email_mailport" = {
        port_map <- list(smtp = c(25, 465, 587), pop3 = c(110, 995), imap = c(143, 993))
        ports <- port_map[[task$protocol]] %||% c(25, 465, 587)
        proto_name <- toupper(task$protocol)
        results <- ""
        for (p in ports) {
          header <- sprintf("\n== %s端口测试 - %s:%d ==\n\n", proto_name, task$domain, p)
          res <- tryCatch({
            con <- suppressWarnings(socketConnection(host = task$domain, port = p, open = "r+b", blocking = TRUE, timeout = 5))
            close(con)
            sprintf("Port %-4d : OPEN", p)
          }, error = function(e) sprintf("Port %-4d : CLOSED (%s)", p, substr(e$message, 1, 40)))
          results <- paste0(results, header, res, "\n")
        }
        results
      },
      "app_system" = {
        # 仅用于报告头（已在主进程 enc2utf8），不处理系统命令
        task$label
      },
      paste("未知任务类型:", task$type)
    )
  }

  # 逐条执行队列中的任务（异步：每任务独立R进程，UI不阻塞）
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
      # 全部测试完成后自动保存日志
      nt_auto_save()
      return()
    }

    bg <- nt_bg_job()
    if (is.null(bg)) {
      # 启动后台进程执行第一个任务
      task <- queue[[1]]
      bg <- callr::r_bg(.run_task, args = list(task = task))
      nt_bg_job(bg)
      invalidateLater(200)
    } else if (bg$is_alive()) {
      # 后台还在跑，等200ms再检查
      invalidateLater(200)
    } else {
      # 后台完成，收割结果
      result_text <- tryCatch(suppressWarnings(bg$get_result()), error = function(e) paste("后台执行失败:", e$message))
      result_text <- enc2utf8(result_text)  # 确保 UTF-8
      nt_result(paste0(nt_result(), result_text))
      nt_bg_job(NULL)
      nt_queue(queue[-1])  # 移出已完成的任务
      invalidateLater(50)
    }
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
  .start_test <- function(cmd_list, custom_target = NULL, tag = NULL) {
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
    nt_test_name(if (!is.null(tag) && tag != "") tag else target_display)
    # 确保所有中文 label 在 callr 序列化前已是 UTF-8
    for (i in seq_along(cmd_list)) {
      if (!is.null(cmd_list[[i]]$label)) cmd_list[[i]]$label <- enc2utf8(cmd_list[[i]]$label)
      if (!is.null(cmd_list[[i]]$label_prefix)) cmd_list[[i]]$label_prefix <- enc2utf8(cmd_list[[i]]$label_prefix)
      if (!is.null(cmd_list[[i]]$desc)) cmd_list[[i]]$desc <- enc2utf8(cmd_list[[i]]$desc)
      if (!is.null(cmd_list[[i]]$url)) cmd_list[[i]]$url <- enc2utf8(cmd_list[[i]]$url)
    }
    nt_queue(cmd_list)
    nt_running(TRUE)
  }

  # 全部测试（包含文件服务器）
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
      .build_cmd_curl(http_target),
      # 文件服务器 #1 测试
      .build_cmd_fileserver("10.10.50.50"),
      list(label = "文件服务器 10.10.50.50 - 端口 445", ip = "10.10.50.50", port = 445, type = "port"),
      list(label = "文件服务器 10.10.50.50 - 端口 139", ip = "10.10.50.50", port = 139, type = "port"),
      list(label = "文件服务器 10.10.50.50 - Ping", ip = "10.10.50.50", count = 4, type = "ping"),
      # 文件服务器 #2 测试
      .build_cmd_fileserver("10.10.50.150"),
      list(label = "文件服务器 10.10.50.150 - 端口 445", ip = "10.10.50.150", port = 445, type = "port"),
      list(label = "文件服务器 10.10.50.150 - 端口 139", ip = "10.10.50.150", port = 139, type = "port"),
      list(label = "文件服务器 10.10.50.150 - Ping", ip = "10.10.50.150", count = 4, type = "ping")
    )
    .start_test(cmd_list, "综合测试（包含文件服务器）", tag = "all_tests")
  })

  # 单项测试：网卡信息
  observeEvent(input$nt_run_ipconfig, {
    .start_test(list(.build_cmd_ipconfig()), tag = "ipconfig")
  })

  # 单项测试：ping
  observeEvent(input$nt_run_ping, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_ping(target, input$nt_ping_count)), target, tag = "ping")
  })

  # 单项测试：nslookup
  observeEvent(input$nt_run_nslookup, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_nslookup(target)), target, tag = "nslookup")
  })

  # 单项测试：nltest
  observeEvent(input$nt_run_nltest, {
    domain <- trimws(input$nt_domain)
    .start_test(list(.build_cmd_nltest(domain)), domain, tag = "nltest")
  })

  # 单项测试：tracert
  observeEvent(input$nt_run_tracert, {
    target <- trimws(input$nt_target)
    if (is.null(target) || target == "") {
      showNotification("请输入测试目标", type = "error"); return()
    }
    .start_test(list(.build_cmd_tracert(target)), target, tag = "tracert")
  })

  # 单项测试：curl
  observeEvent(input$nt_run_curl, {
    http_target <- trimws(input$nt_http_target)
    .start_test(list(.build_cmd_curl(http_target)), http_target, tag = "curl")
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
    .start_test(cmd_list, ip, tag = "fileserver1")
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
    .start_test(cmd_list, ip, tag = "fileserver2")
  })

  # ========== 邮箱诊断 ==========

  # 获取邮箱域名
  .get_email_domain <- function() {
    d <- trimws(input$nt_email_domain)
    if (is.null(d) || d == "") d <- trimws(input$nt_target)
    d
  }

  # 邮箱全诊断（MX + A记录 + 所有端口 + SPF + DKIM + DMARC）
  observeEvent(input$nt_run_email_all, {
    domain <- .get_email_domain()
    cmd_list <- list(
      list(type="email_mx", domain=domain),
      list(type="email_a", domain=domain),
      list(type="email_ptr", domain=domain),
      list(type="email_mailport", domain=domain, protocol="smtp"),
      list(type="email_mailport", domain=domain, protocol="pop3"),
      list(type="email_mailport", domain=domain, protocol="imap"),
      list(type="email_txt", query=domain, label_prefix="SPF记录检查"),
      list(type="email_txt", query=paste0("default._domainkey.", domain), label_prefix="DKIM记录检查 (default selector)"),
      list(type="email_txt", query=paste0("_dmarc.", domain), label_prefix="DMARC记录检查")
    )
    .start_test(cmd_list, sprintf("邮箱诊断: %s", domain), tag = "email_all")
  })

  # MX记录
  observeEvent(input$nt_run_email_mx, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_mx", domain=domain)), sprintf("MX查询: %s", domain), tag = "email_mx")
  })

  # A记录
  observeEvent(input$nt_run_email_a, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_a", domain=domain)), sprintf("A记录: %s", domain), tag = "email_a")
  })

  # SMTP端口
  observeEvent(input$nt_run_email_smtp, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_mailport", domain=domain, protocol="smtp")), sprintf("SMTP端口: %s", domain), tag = "email_smtp")
  })

  # POP3端口
  observeEvent(input$nt_run_email_pop3, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_mailport", domain=domain, protocol="pop3")), sprintf("POP3端口: %s", domain), tag = "email_pop3")
  })

  # IMAP端口
  observeEvent(input$nt_run_email_imap, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_mailport", domain=domain, protocol="imap")), sprintf("IMAP端口: %s", domain), tag = "email_imap")
  })

  # SPF
  observeEvent(input$nt_run_email_spf, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_txt", query=domain, label_prefix="SPF记录检查")), sprintf("SPF: %s", domain), tag = "email_spf")
  })

  # DKIM
  observeEvent(input$nt_run_email_dkim, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_txt", query=paste0("default._domainkey.", domain), label_prefix="DKIM记录检查 (default selector)")), sprintf("DKIM: %s", domain), tag = "email_dkim")
  })

  # DMARC
  observeEvent(input$nt_run_email_dmarc, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_txt", query=paste0("_dmarc.", domain), label_prefix="DMARC记录检查")), sprintf("DMARC: %s", domain), tag = "email_dmarc")
  })

  # PTR
  observeEvent(input$nt_run_email_ptr, {
    domain <- .get_email_domain()
    .start_test(list(list(type="email_ptr", domain=domain)), sprintf("PTR: %s", domain), tag = "email_ptr")
  })

  # ========== 应用系统测试 ==========

  # 协同平台-前端 (预设)
  observeEvent(input$nt_run_app_ecs, {
    desc <- "LVCC协同平台-前端服务器"
    domain <- "ecs.Lvcchong.com"
    ip <- "117.162.0.171"
    port <- 20600L
    url <- "http://ecs.Lvcchong.com:20600"
    header_label <- enc2utf8(paste0(
      sprintf("========================================\n"),
      sprintf("  应用系统连通性测试\n"),
      sprintf("========================================\n"),
      sprintf("服务器描述  : %s\n", desc),
      sprintf("URL         : %s\n", url),
      sprintf("域名        : %s\n", domain),
      sprintf("IP地址      : %s\n", ip),
      sprintf("测试端口    : %d\n", port),
      sprintf("测试时间    : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("========================================\n\n")
    ))
    cmd_list <- list(
      list(type = "app_system", label = header_label),
      .build_cmd_nslookup(domain),
      list(label = sprintf("Ping测试 - %s", ip), cmd = sprintf("ping %s -n 4", ip), type = "system"),
      list(label = sprintf("TCP端口测试 - %s:%d", domain, port), ip = domain, port = port, type = "port")
    )
    .start_test(cmd_list, desc, tag = "app_ecs_frontend")
  })

  # 自定义应用测试 (使用上方参数)
  observeEvent(input$nt_run_app_custom, {
    desc <- trimws(input$nt_app_name)
    domain <- trimws(input$nt_app_domain)
    ip <- trimws(input$nt_app_ip)
    port <- as.integer(input$nt_app_port)
    url <- trimws(input$nt_app_url)
    if (is.null(domain) || domain == "") {
      showNotification("请输入域名", type = "error"); return()
    }
    if (is.na(port) || port < 1) {
      showNotification("请输入有效端口号", type = "error"); return()
    }
    if (is.null(desc) || desc == "") desc <- domain
    if (is.null(url) || url == "") url <- sprintf("http://%s:%d", domain, port)
    header_label <- enc2utf8(paste0(
      sprintf("========================================\n"),
      sprintf("  应用系统连通性测试\n"),
      sprintf("========================================\n"),
      sprintf("服务器描述  : %s\n", desc),
      sprintf("URL         : %s\n", url),
      sprintf("域名        : %s\n", domain),
      sprintf("IP地址      : %s\n", ip),
      sprintf("测试端口    : %d\n", port),
      sprintf("测试时间    : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("========================================\n\n")
    ))
    cmd_list <- list(
      list(type = "app_system", label = header_label),
      .build_cmd_nslookup(domain),
      list(label = sprintf("Ping测试 - %s", ip), cmd = sprintf("ping %s -n 4", ip), type = "system"),
      list(label = sprintf("TCP端口测试 - %s:%d", domain, port), ip = domain, port = port, type = "port")
    )
    .start_test(cmd_list, desc, tag = "app_custom")
  })

  # 清空
  observeEvent(input$nt_clear, {
    if (nt_running()) {
      nt_queue(list())
      nt_running(FALSE)
    }
    nt_result("")
  })

  # 保存日志（内部通用函数）
  nt_save_log_file <- function(current_text, test_name_hint = NULL) {
    if (is.null(current_text) || current_text == "") return(FALSE)
    if (!dir.exists(nt_log_dir)) dir.create(nt_log_dir, recursive = TRUE)
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    # 用户名
    user_part <- ""
    if (!is.null(rv) && !is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      user_part <- paste0("_", gsub("[^a-zA-Z0-9._-]", "_", rv$current_user$username[1]))
    }
    # 文件名后缀：tag 优先（英文简称），否则用 nt_target
    if (!is.null(test_name_hint) && test_name_hint != "") {
      name_safe <- gsub("[^a-zA-Z0-9._-]", "_", test_name_hint)
      name_safe <- substr(name_safe, 1, 30)
    } else {
      target <- trimws(input$nt_target)
      name_safe <- gsub("[^a-zA-Z0-9._-]", "_", target)
    }
    filename <- sprintf("nt_%s%s_%s.log", ts, user_part, name_safe)
    filepath <- file.path(nt_log_dir, filename)
    # 用 UTF-8 编码写入，避免乱码
    current_text <- enc2utf8(current_text)
    con <- file(filepath, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    suppressWarnings(writeLines(current_text, con))
    showNotification(sprintf("日志已保存: %s", filename), type = "message", duration = 5)
    TRUE
  }

  # 自动保存（测试全部完成时调用）
  nt_auto_save <- function() {
    current_text <- nt_result()
    test_name <- nt_test_name()
    if (!is.null(test_name) && test_name != "") {
      nt_save_log_file(current_text, test_name)
    }
  }

  # 手动保存日志
  observeEvent(input$nt_save_log, {
    current_text <- nt_result()
    if (is.null(current_text) || current_text == "") {
      showNotification("没有可保存的测试结果", type = "warning"); return()
    }
    tryCatch({
      nt_save_log_file(current_text, nt_test_name())
    }, error = function(e) {
      showNotification(paste("保存失败:", e$message), type = "error")
    })
  })

  # 加载历史日志
  observeEvent(input$nt_load_log, {
    if (nt_running()) {
      showNotification("测试正在执行中，请稍后", type = "warning"); return()
    }

    tryCatch({
      if (!dir.exists(nt_log_dir)) {
        showNotification("暂无历史日志", type = "message"); return()
      }

      log_files <- list.files(nt_log_dir, pattern = "\\.log$", full.names = FALSE)
      if (length(log_files) == 0) {
        showNotification("暂无历史日志", type = "message"); return()
      }

      # 按时间倒序排列
      log_files <- rev(sort(log_files))

      # 显示选择对话框
      showModal(modalDialog(
        title = "选择历史日志",
        selectInput("nt_select_log", "日志文件：",
          choices = setNames(file.path(nt_log_dir, log_files), log_files),
          selected = file.path(nt_log_dir, log_files[1])),
        footer = tagList(
          modalButton("取消"),
          actionButton("nt_load_selected_log", "加载", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("读取日志失败:", e$message), type = "error")
    })
  })

  # 加载选中的日志
  observeEvent(input$nt_load_selected_log, {
    removeModal()
    log_path <- input$nt_select_log
    if (is.null(log_path) || log_path == "") return()

    tryCatch({
      content <- paste(readLines(log_path, warn = FALSE), collapse = "\n")
      nt_result(content)
      showNotification("日志已加载", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("加载失败:", e$message), type = "error")
    })
  })
}
