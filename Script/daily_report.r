# 日报模块
# 自动从工单和项目任务中按人提取当天相关记录，编制成个人工作日报
# 全员日报列表形式展示

# ================================================================
# 数据层：获取日报数据
# ================================================================

# 获取指定日期每人的工单汇总
daily_report_get_work_orders <- function(report_date) {
  con <- db_connect()
  tryCatch({
    date_str <- as.character(report_date)
    # 获取当天创建、处理中、或已完成的工单
    query <- sprintf("
      SELECT wo.id, wo.order_no, wo.title, wo.priority, wo.status, wo.category,
             wo.assigned_to, wo.handled_by, wo.created_by,
             wo.created_at, wo.handled_at, wo.completed_at,
             u_assign.username as assignee_name,
             u_handler.username as handler_name,
             u_creator.username as creator_name
      FROM work_orders wo
      LEFT JOIN users u_assign ON wo.assigned_to = u_assign.id
      LEFT JOIN users u_handler ON wo.handled_by = u_handler.id
      LEFT JOIN users u_creator ON wo.created_by = u_creator.id
      WHERE DATE(wo.created_at) = '%s'
         OR DATE(wo.handled_at) = '%s'
         OR DATE(wo.completed_at) = '%s'
         OR (wo.status IN ('processing', 'assigned') AND DATE(wo.updated_at) = '%s')
      ORDER BY wo.updated_at DESC
    ", date_str, date_str, date_str, date_str)
    dbGetQuery(con, query)
  }, error = function(e) {
    warning(paste("获取日报工单数据失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 获取指定日期每人的任务汇总
daily_report_get_tasks <- function(report_date) {
  con <- db_connect()
  tryCatch({
    date_str <- as.character(report_date)
    # 获取当天有操作（状态变更、创建、反馈）的任务
    query <- sprintf("
      SELECT DISTINCT t.id, t.task_no, t.name as task_name, t.status, t.priority,
             t.assigned_to, t.created_by,
             p.name as project_name,
             ph.name as phase_name,
             u_assign.username as assignee_name,
             u_creator.username as creator_name,
             t.updated_at
      FROM project_tasks t
      LEFT JOIN projects p ON t.project_id = p.id
      LEFT JOIN project_phases ph ON t.phase_id = ph.id
      LEFT JOIN users u_assign ON t.assigned_to = u_assign.id
      LEFT JOIN users u_creator ON t.created_by = u_creator.id
      WHERE DATE(t.created_at) = '%s'
         OR DATE(t.updated_at) = '%s'
         OR t.id IN (SELECT task_id FROM project_task_logs WHERE DATE(created_at) = '%s')
      ORDER BY t.updated_at DESC
    ", date_str, date_str, date_str)
    dbGetQuery(con, query)
  }, error = function(e) {
    warning(paste("获取日报任务数据失败:", e$message))
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 获取指定日期的任务日志（反馈记录）
daily_report_get_task_logs <- function(report_date) {
  con <- db_connect()
  tryCatch({
    date_str <- as.character(report_date)
    query <- sprintf("
      SELECT l.task_id, l.log_type, l.content, l.created_at,
             t.task_no, t.name as task_name,
             u.username as creator_name
      FROM project_task_logs l
      LEFT JOIN project_tasks t ON l.task_id = t.id
      LEFT JOIN users u ON l.created_by = u.id
      WHERE DATE(l.created_at) = '%s'
      ORDER BY l.created_at DESC
    ", date_str)
    dbGetQuery(con, query)
  }, error = function(e) {
    data.frame()
  }, finally = { db_disconnect(con) })
}

# 获取所有活跃用户列表
daily_report_get_users <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT id, username, display_name, role FROM users WHERE active = 1 ORDER BY username")
  }, error = function(e) { data.frame() }, finally = { db_disconnect(con) })
}

# ================================================================
# UI 定义
# ================================================================

daily_report_ui <- function() {
  fluidPage(
    tags$style(HTML("
      .dr-card {
        background: #fff; border: 1px solid #e0e0e0; border-radius: 8px;
        padding: 16px; margin-bottom: 16px; box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      }
      .dr-card-header {
        display: flex; align-items: center; margin-bottom: 12px;
        padding-bottom: 10px; border-bottom: 2px solid #337ab7;
      }
      .dr-card-header .dr-avatar {
        width: 40px; height: 40px; border-radius: 50%; background: #337ab7;
        color: white; display: flex; align-items: center; justify-content: center;
        font-size: 18px; font-weight: bold; margin-right: 12px;
      }
      .dr-card-header .dr-name {
        font-size: 16px; font-weight: bold; color: #333;
      }
      .dr-card-header .dr-stats {
        margin-left: auto; font-size: 12px; color: #666;
      }
      .dr-section { margin-bottom: 12px; }
      .dr-section-title {
        font-size: 13px; font-weight: bold; color: #555;
        margin-bottom: 6px; padding-left: 8px;
        border-left: 3px solid #337ab7;
      }
      .dr-section-title.wo { border-left-color: #5bc0de; }
      .dr-section-title.task { border-left-color: #5cb85c; }
      .dr-item {
        padding: 6px 12px; margin-bottom: 4px; background: #f9f9f9;
        border-radius: 4px; font-size: 13px; line-height: 1.6;
      }
      .dr-item .dr-badge {
        display: inline-block; padding: 1px 6px; border-radius: 3px;
        font-size: 11px; color: white; margin-right: 6px;
      }
      .dr-empty { color: #999; font-size: 13px; padding: 8px 12px; }
    ")),
    fluidRow(
      column(3, dateInput("dr_date", "选择日期", value = Sys.Date(), language = "zh-CN")),
      column(2, div(style = "margin-top:25px;",
        actionButton("dr_today", "今天", class = "btn-default btn-sm"),
        actionButton("dr_yesterday", "昨天", class = "btn-default btn-sm"))),
      column(2, selectInput("dr_user_filter", "筛选人员",
        choices = c("全部人员" = "all"))),
      column(2, div(style = "margin-top:25px;",
        actionButton("dr_refresh", "刷新日报", class = "btn-primary btn-sm", icon = icon("sync")))),
      column(3, div(style = "margin-top:25px; text-align:right;",
        actionButton("dr_copy_text", "复制文本日报", class = "btn-default btn-sm", icon = icon("copy"))))
    ),
    hr(),
    uiOutput("dr_report_content"),
    # 隐藏textarea用于存放纯文本日报
    tags$textarea(id = "dr_text_content", style = "display:none;"),
    tags$script(HTML("
      $(document).on('click', '#dr_copy_text', function() {
        var text = $('#dr_text_content').val();
        if (text) {
          navigator.clipboard.writeText(text).then(function() {
            Shiny.setInputValue('dr_copy_done', Math.random(), {priority:'event'});
          });
        }
      });
      Shiny.addCustomMessageHandler('dr_update_text', function(message) {
        $('#dr_text_content').val(message);
      });
    "))
  )
}

# ================================================================
# Server 逻辑
# ================================================================

daily_report_server <- function(input, output, session, rv) {

  # 快捷日期按钮
  observeEvent(input$dr_today, {
    updateDateInput(session, "dr_date", value = Sys.Date())
  })
  observeEvent(input$dr_yesterday, {
    updateDateInput(session, "dr_date", value = Sys.Date() - 1)
  })

  # 初始化用户筛选下拉
  observe({
    req(rv$logged_in)
    users <- daily_report_get_users()
    if (nrow(users) > 0) {
      labels <- ifelse(is.na(users$display_name) | users$display_name == "",
                       users$username, sprintf("%s (%s)", users$display_name, users$username))
      choices <- c("全部人员" = "all", setNames(as.character(users$id), labels))
    } else {
      choices <- c("全部人员" = "all")
    }
    updateSelectInput(session, "dr_user_filter", choices = choices)
  })

  # 日报数据
  dr_data <- reactiveVal(NULL)

  # 生成日报
  observeEvent(list(input$dr_refresh, input$dr_date, input$dr_user_filter), {
    req(rv$logged_in, input$dr_date)

    report_date <- input$dr_date
    work_orders <- daily_report_get_work_orders(report_date)
    tasks <- daily_report_get_tasks(report_date)
    task_logs <- daily_report_get_task_logs(report_date)
    users <- daily_report_get_users()

    dr_data(list(
      date = report_date,
      work_orders = work_orders,
      tasks = tasks,
      task_logs = task_logs,
      users = users
    ))
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # 渲染日报内容
  output$dr_report_content <- renderUI({
    req(rv$logged_in)
    data <- dr_data()
    if (is.null(data)) return(div(class = "dr-empty", "请选择日期并点击刷新"))

    users <- data$users
    work_orders <- data$work_orders
    tasks <- data$tasks
    task_logs <- data$task_logs
    user_filter <- input$dr_user_filter

    if (nrow(users) == 0) return(div(class = "dr-empty", "暂无用户数据"))

    # 筛选用户
    if (!is.null(user_filter) && user_filter != "all") {
      users <- users[users$id == as.integer(user_filter), , drop = FALSE]
    }

    # 为每个用户生成日报卡片
    cards_html <- ""
    text_report <- sprintf("=== 工作日报 %s ===\n\n", as.character(data$date))

    for (ui in 1:nrow(users)) {
      u <- users[ui, ]
      uid <- u$id
      uname <- ifelse(is.na(u$display_name) || u$display_name == "", u$username, u$display_name)
      initial <- substr(uname, 1, 1)

      # 该用户相关的工单
      user_wo <- data.frame()
      if (nrow(work_orders) > 0) {
        user_wo <- work_orders[
          (!is.na(work_orders$assigned_to) & work_orders$assigned_to == uid) |
          (!is.na(work_orders$handled_by) & work_orders$handled_by == uid) |
          (!is.na(work_orders$created_by) & work_orders$created_by == uid), , drop = FALSE]
      }

      # 该用户相关的任务
      user_tasks <- data.frame()
      if (nrow(tasks) > 0) {
        user_tasks <- tasks[
          (!is.na(tasks$assigned_to) & tasks$assigned_to == uid) |
          (!is.na(tasks$created_by) & tasks$created_by == uid), , drop = FALSE]
      }

      # 该用户的反馈日志
      user_logs <- data.frame()
      if (nrow(task_logs) > 0) {
        user_logs <- task_logs[!is.na(task_logs$creator_name) & task_logs$creator_name == u$username, , drop = FALSE]
      }

      # 无数据则跳过
      if (nrow(user_wo) == 0 && nrow(user_tasks) == 0 && nrow(user_logs) == 0) next

      # 统计
      wo_count <- nrow(user_wo)
      task_count <- nrow(user_tasks)
      log_count <- nrow(user_logs)

      # 构建卡片HTML
      wo_html <- ""
      if (wo_count > 0) {
        wo_items <- ""
        for (wi in 1:nrow(user_wo)) {
          w <- user_wo[wi, ]
          status_cn <- switch(as.character(w$status),
            "pending" = "待处理", "assigned" = "已派发", "processing" = "处理中",
            "completed" = "已完成", "closed" = "已关闭", w$status)
          status_color <- switch(as.character(w$status),
            "pending" = "#f0ad4e", "assigned" = "#5bc0de", "processing" = "#337ab7",
            "completed" = "#5cb85c", "closed" = "#777", "#999")
          order_no <- ifelse(is.na(w$order_no), "-", w$order_no)
          wo_items <- paste0(wo_items, sprintf(
            '<div class="dr-item"><span class="dr-badge" style="background:%s;">%s</span> <b>%s</b> %s <span style="color:#999;margin-left:8px;">[%s]</span></div>',
            status_color, status_cn, order_no, w$title, ifelse(is.na(w$category), "", w$category)))
        }
        wo_html <- sprintf('<div class="dr-section"><div class="dr-section-title wo">工单 (%d)</div>%s</div>', wo_count, wo_items)
      }

      task_html <- ""
      if (task_count > 0) {
        task_items <- ""
        for (ti in 1:nrow(user_tasks)) {
          tk <- user_tasks[ti, ]
          status_cn <- switch(as.character(tk$status),
            "pending" = "待处理", "in_progress" = "进行中",
            "completed" = "已完成", "blocked" = "已阻塞", tk$status)
          status_color <- switch(as.character(tk$status),
            "pending" = "#f0ad4e", "in_progress" = "#337ab7",
            "completed" = "#5cb85c", "blocked" = "#d9534f", "#999")
          proj_name <- ifelse(is.na(tk$project_name), "", tk$project_name)
          task_items <- paste0(task_items, sprintf(
            '<div class="dr-item"><span class="dr-badge" style="background:%s;">%s</span> <b>%s</b> %s <span style="color:#999;margin-left:8px;">[%s]</span></div>',
            status_color, status_cn, ifelse(is.na(tk$task_no), "-", tk$task_no), tk$task_name, proj_name))
        }
        task_html <- sprintf('<div class="dr-section"><div class="dr-section-title task">项目任务 (%d)</div>%s</div>', task_count, task_items)
      }

      log_html <- ""
      if (log_count > 0) {
        log_items <- ""
        for (li in 1:min(5, nrow(user_logs))) {
          lg <- user_logs[li, ]
          log_type_cn <- switch(as.character(lg$log_type),
            "execution" = "执行", "feedback" = "反馈",
            "status_change" = "状态", "note" = "备注", "其他")
          content_short <- if (nchar(lg$content) > 60) paste0(substr(lg$content, 1, 60), "...") else lg$content
          log_items <- paste0(log_items, sprintf(
            '<div class="dr-item"><span class="dr-badge" style="background:#5bc0de;">%s</span> [%s] %s</div>',
            log_type_cn, ifelse(is.na(lg$task_name), "-", lg$task_name), content_short))
        }
        if (log_count > 5) {
          log_items <- paste0(log_items, sprintf('<div class="dr-item" style="color:#999;">... 还有 %d 条记录</div>', log_count - 5))
        }
        log_html <- sprintf('<div class="dr-section"><div class="dr-section-title">反馈记录 (%d)</div>%s</div>', log_count, log_items)
      }

      cards_html <- paste0(cards_html, sprintf(
        '<div class="dr-card">
          <div class="dr-card-header">
            <div class="dr-avatar">%s</div>
            <div class="dr-name">%s</div>
            <div class="dr-stats">工单 %d | 任务 %d | 反馈 %d</div>
          </div>
          %s%s%s
        </div>', initial, uname, wo_count, task_count, log_count, wo_html, task_html, log_html))

      # 纯文本日报
      text_report <- paste0(text_report, sprintf("【%s】\n", uname))
      if (wo_count > 0) {
        text_report <- paste0(text_report, "  [工单]\n")
        for (wi in 1:nrow(user_wo)) {
          w <- user_wo[wi, ]
          status_cn <- switch(as.character(w$status),
            "pending" = "待处理", "assigned" = "已派发", "processing" = "处理中",
            "completed" = "已完成", "closed" = "已关闭", w$status)
          text_report <- paste0(text_report, sprintf("    - [%s] %s %s\n",
            status_cn, ifelse(is.na(w$order_no), "", w$order_no), w$title))
        }
      }
      if (task_count > 0) {
        text_report <- paste0(text_report, "  [任务]\n")
        for (ti in 1:nrow(user_tasks)) {
          tk <- user_tasks[ti, ]
          status_cn <- switch(as.character(tk$status),
            "pending" = "待处理", "in_progress" = "进行中",
            "completed" = "已完成", "blocked" = "已阻塞", tk$status)
          text_report <- paste0(text_report, sprintf("    - [%s] %s %s (%s)\n",
            status_cn, ifelse(is.na(tk$task_no), "", tk$task_no), tk$task_name,
            ifelse(is.na(tk$project_name), "", tk$project_name)))
        }
      }
      text_report <- paste0(text_report, "\n")
    }

    if (cards_html == "") {
      cards_html <- '<div class="dr-empty" style="text-align:center;padding:40px;font-size:15px;color:#999;">该日期暂无工作记录</div>'
      text_report <- paste0(text_report, "该日期暂无工作记录\n")
    }

    # 更新隐藏textarea的内容
    session$sendCustomMessage("dr_update_text", text_report)

    HTML(cards_html)
  })

  # 复制完成通知
  observeEvent(input$dr_copy_done, {
    showNotification("日报文本已复制到剪贴板", type = "message")
  })
}
