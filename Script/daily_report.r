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
             COALESCE(NULLIF(u_assign.display_name,''), u_assign.username) as assignee_name,
             COALESCE(NULLIF(u_handler.display_name,''), u_handler.username) as handler_name,
             COALESCE(NULLIF(u_creator.display_name,''), u_creator.username) as creator_name
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
             COALESCE(NULLIF(u.display_name,''), u.username) as creator_name
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

# 获取指定日期的记事评论（含跨天连续性：当天评论的所有祖先+后代）
daily_report_get_note_comments <- function(report_date) {
  con <- db_connect()
  tryCatch({
    date_str <- as.character(report_date)
    # 第一步：当天的评论
    query <- sprintf("
      SELECT nc.id, nc.content, nc.created_at, nc.created_by,
             nc.parent_id,
             n.note_no, n.title as note_title,
             u.username, u.display_name
      FROM note_comments nc
      LEFT JOIN notes n ON nc.note_id = n.id
      LEFT JOIN users u ON nc.created_by = u.id
      WHERE DATE(nc.created_at) = '%s'
    ", date_str)
    today <- dbGetQuery(con, query)

    if (nrow(today) == 0) return(data.frame())

    # 第二步：当天评论的所有父级（祖先链）
    parent_ids <- unique(today$parent_id[!is.na(today$parent_id) & today$parent_id > 0])
    ancestors <- data.frame()
    while (length(parent_ids) > 0) {
      ids_str <- paste(parent_ids, collapse=",")
      a <- dbGetQuery(con, sprintf("
        SELECT nc.id, nc.content, nc.created_at, nc.created_by, nc.parent_id,
               n.note_no, n.title as note_title,
               u.username, u.display_name
        FROM note_comments nc
        LEFT JOIN notes n ON nc.note_id = n.id
        LEFT JOIN users u ON nc.created_by = u.id
        WHERE nc.id IN (%s)
      ", ids_str))
      if (nrow(a) > 0) {
        ancestors <- rbind(ancestors, a)
        grand_ids <- unique(a$parent_id[!is.na(a$parent_id) & a$parent_id > 0])
        parent_ids <- setdiff(grand_ids, c(ancestors$id, today$id))
      } else break
    }

    # 第三步：当天评论的所有子级（后代），递归直到无更多层级
    today_ids <- today$id
    all_ids <- today$id
    children <- data.frame()
    while (length(today_ids) > 0) {
      ids_str <- paste(today_ids, collapse=",")
      ch <- dbGetQuery(con, sprintf("
        SELECT nc.id, nc.content, nc.created_at, nc.created_by, nc.parent_id,
               n.note_no, n.title as note_title,
               u.username, u.display_name
        FROM note_comments nc
        LEFT JOIN notes n ON nc.note_id = n.id
        LEFT JOIN users u ON nc.created_by = u.id
        WHERE nc.parent_id IN (%s)
      ", ids_str))
      if (nrow(ch) > 0) {
        children <- rbind(children, ch)
        new_ids <- setdiff(ch$id, all_ids)
        all_ids <- c(all_ids, new_ids)
        today_ids <- new_ids
      } else break
    }

    # 合并去重，按时间排序
    result <- today
    if (nrow(ancestors) > 0) result <- rbind(result, ancestors)
    if (nrow(children) > 0) result <- rbind(result, children)
    result <- result[!duplicated(result$id), ]
    result <- result[order(result$created_at), ]
    result
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取所有活跃用户列表
daily_report_get_users <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT id, username, display_name, role FROM users WHERE active = 1 ORDER BY username")
  }, error = function(e) { data.frame() }, finally = { db_disconnect(con) })
}

# 中文大写数字转换（1→一, 10→十, 11→十一, 99→九十九）
dr_cn_number <- function(n) {
  if (n <= 0) return(as.character(n))
  digits <- c("","一","二","三","四","五","六","七","八","九")
  units  <- c("","十","百","千")
  if (n < 10) return(digits[n + 1])
  if (n == 10) return("十")
  s <- as.character(n)
  nc <- nchar(s)
  result <- ""
  for (i in seq_len(nc)) {
    d <- as.integer(substr(s, i, i))
    u <- nc - i + 1  # 1=个位, 2=十位, 3=百位
    if (d == 0) next
    if (u == 2 && d == 1) {
      result <- paste0(result, "十")
    } else {
      result <- paste0(result, digits[d + 1], units[u])
    }
  }
  result
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
      .dr-section-title.note { border-left-color: #6c3bbf; }
      .dr-item {
        padding: 6px 12px; margin-bottom: 4px; background: #f9f9f9;
        border-radius: 4px; font-size: 13px; line-height: 1.6;
      }
      .dr-item .dr-badge {
        display: inline-block; padding: 1px 6px; border-radius: 3px;
        font-size: 11px; color: white; margin-right: 6px;
      }
      .dr-empty { color: #999; font-size: 13px; padding: 8px 12px; }
      /* ── 时间流水：整点区间 + 左右交替时间轴 ── */
      .dr-timeline { max-width: 900px; margin: 0 auto; padding: 20px 0 40px; position: relative; }
      .dr-timeline::before {
        content: ''; position: absolute; left: 50%; top: 0; bottom: 0;
        width: 4px; background: linear-gradient(180deg, #e0e7ff, #c7d2fe 30%, #a5b4fc 70%, #e0e7ff);
        transform: translateX(-50%); border-radius: 2px;
      }
      .tl-header { text-align: center; font-size: 15px; font-weight: bold; color: #4f46e5; margin-bottom: 24px; }
      .tl-hour-label {
        position: relative; left: 50%; transform: translateX(-50%);
        width: 52px; text-align: center; background: #f8f9ff; border: 1px solid #c7d2fe;
        color: #6366f1; font-size: 11px; font-weight: 600; border-radius: 12px;
        padding: 2px 0; margin: 8px 0; z-index: 2;
      }
      .tl-item {
        position: relative; width: 50%; padding: 8px 40px 8px 0; box-sizing: border-box;
        min-height: 60px;
      }
      .tl-item:nth-child(even) {
        margin-left: 50%; padding: 8px 0 8px 40px;
      }
      .tl-item::before {
        content: ''; position: absolute; right: -8px; top: 22px;
        width: 16px; height: 16px; border-radius: 50%; background: #fff;
        border: 4px solid #6366f1; box-shadow: 0 0 0 2px #e0e7ff; z-index: 3;
      }
      .tl-item:nth-child(even)::before { left: -8px; right: auto; }
      .tl-item::after {
        content: ''; position: absolute; right: 0; top: 28px; width: 32px; height: 2px; background: #c7d2fe; z-index: 1;
      }
      .tl-item:nth-child(even)::after { left: 0; right: auto; }
      .tl-card {
        background: #fff; border: 1px solid #e0e7ff; border-radius: 10px;
        padding: 12px 14px; box-shadow: 0 2px 6px rgba(99,102,241,0.08);
        position: relative; transition: all .15s;
      }
      .tl-card:hover { transform: translateY(-2px); box-shadow: 0 6px 16px rgba(99,102,241,0.15); border-color: #a5b4fc; }
      .tl-card::before {
        content: ''; position: absolute; top: 20px; width: 10px; height: 10px; background: #fff; border: 1px solid #e0e7ff;
        transform: rotate(45deg); border-right-color: #fff; border-top-color: #fff;
      }
      .tl-item:nth-child(odd) .tl-card::before { right: -6px; }
      .tl-item:nth-child(even) .tl-card::before { left: -6px; transform: rotate(45deg); border-left-color: #fff; border-bottom-color: #fff; border-right-color: #e0e7ff; border-top-color: #e0e7ff; }
      .tl-card-hd { display: flex; align-items: center; justify-content: space-between; margin-bottom: 6px; }
      .tl-card-hd-left { display: flex; align-items: center; gap: 8px; }
      .tl-card-hd b { font-size: 13px; color: #4338ca; }
      .tl-time-badge { font-size: 10px; background: #ede9fe; color: #5b21b6; border-radius: 6px; padding: 1px 8px; font-family: Consolas, monospace; }
      .tl-tag { font-size: 10px; background: #e0f2fe; color: #0369a1; border-radius: 10px; padding: 1px 8px; white-space: nowrap; max-width: 180px; overflow: hidden; text-overflow: ellipsis; display: inline-block; }
      .tl-card-bd { font-size: 12px; color: #475569; line-height: 1.6; white-space: pre-wrap; }
    ")),
    fluidRow(
      column(2, dateInput("dr_date", "选择日期", value = Sys.Date(), language = "zh-CN")),
      column(4, div(style = "margin-top:25px;",
        actionButton("dr_today", "今天", class = "btn-default btn-sm"),
        actionButton("dr_yesterday", "昨天", class = "btn-default btn-sm"),
        actionButton("dr_this_week", "本周", class = "btn-default btn-sm"),
        actionButton("dr_last_week", "上周", class = "btn-default btn-sm"),
        actionButton("dr_this_month", "本月", class = "btn-info btn-sm"),
        actionButton("dr_last_month", "上月", class = "btn-info btn-sm"))),
      column(2, selectInput("dr_user_filter", "筛选人员",
        choices = c("全部人员" = "all"))),
      column(2, div(style = "margin-top:25px;",
        actionButton("dr_refresh", "刷新日报", class = "btn-primary btn-sm", icon = icon("sync")))),
      column(3, div(style = "margin-top:25px; text-align:right;",
        actionButton("dr_copy_text", "复制文本日报", class = "btn-default btn-sm", icon = icon("copy"))))
    ),
    hr(),
    tabsetPanel(
      tabPanel("人员日报", uiOutput("dr_report_content")),
      tabPanel("⏱ 时间流水", uiOutput("dr_timeline_content"))
    ),
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

  # 月报模式（NULL=日报，list(start,end,label)或字符标签）
  dr_month_mode <- reactiveVal(NULL)

  # 快捷日期按钮
  observeEvent(input$dr_today, {
    dr_month_mode(NULL)
    updateDateInput(session, "dr_date", value = Sys.Date())
  })
  observeEvent(input$dr_yesterday, {
    dr_month_mode(NULL)
    updateDateInput(session, "dr_date", value = Sys.Date() - 1)
  })
  observeEvent(input$dr_this_week, {
    d <- Sys.Date()
    monday <- d - as.integer(format(d, "%u")) + 1
    sunday <- min(monday + 6, d)
    dr_month_mode(list(start = monday, end = sunday,
                       label = sprintf("本周 (%s~%s)", format(monday, "%m/%d"), format(sunday, "%m/%d"))))
    updateDateInput(session, "dr_date", value = monday)
  })
  observeEvent(input$dr_last_week, {
    d <- Sys.Date() - 7
    monday <- d - as.integer(format(d, "%u")) + 1
    sunday <- monday + 6
    dr_month_mode(list(start = monday, end = sunday,
                       label = sprintf("上周 (%s~%s)", format(monday, "%m/%d"), format(sunday, "%m/%d"))))
    updateDateInput(session, "dr_date", value = monday)
  })
  observeEvent(input$dr_this_month, {
    d <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    dr_month_mode(list(start = d, end = seq(d, by = "month", length.out = 2)[2] - 1,
                       label = format(d, "%Y年%m月")))
    updateDateInput(session, "dr_date", value = d)
  })
  observeEvent(input$dr_last_month, {
    d <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
    d <- as.Date(format(d, "%Y-%m-01"))
    dr_month_mode(list(start = d, end = seq(d, by = "month", length.out = 2)[2] - 1,
                       label = format(d, "%Y年%m月")))
    updateDateInput(session, "dr_date", value = d)
  })

  # 初始化用户筛选下拉
  observe({
    req(rv$logged_in)
    users <- daily_report_get_users()
    # 非admin用户只能看自己的日报
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    if (!is_admin && !is.null(rv$current_user)) {
      users <- users[users$id == rv$current_user$id[1], , drop = FALSE]
    }
    if (nrow(users) > 0) {
      labels <- ifelse(is.na(users$display_name) | users$display_name == "",
                       users$username, sprintf("%s (%s)", users$display_name, users$username))
      choices <- if (is_admin) c("全部人员" = "all", setNames(as.character(users$id), labels))
                 else setNames(as.character(users$id), labels)
    } else {
      choices <- if (is_admin) c("全部人员" = "all") else character(0)
    }
    updateSelectInput(session, "dr_user_filter", choices = choices)
  })

  # 日报数据
  dr_data <- reactiveVal(NULL)

  # 生成日报（支持日/月模式）
  observeEvent(list(input$dr_refresh, input$dr_date, input$dr_user_filter, rv$daily_report_refresh), {
    req(rv$logged_in, input$dr_date)

    mm <- dr_month_mode()
    report_date <- input$dr_date
    if (!is.null(mm)) {
      # 月报模式：逐日查询合并
      dates <- seq(mm$start, mm$end, by = "day")
      work_orders <- do.call(rbind, lapply(dates, daily_report_get_work_orders))
      tasks <- do.call(rbind, lapply(dates, daily_report_get_tasks))
      task_logs <- do.call(rbind, lapply(dates, daily_report_get_task_logs))
      note_comments <- do.call(rbind, lapply(dates, daily_report_get_note_comments))
      # 去重（跨天可能有重复）
      if (!is.null(work_orders) && nrow(work_orders) > 0) work_orders <- work_orders[!duplicated(work_orders$id), ]
      if (!is.null(tasks) && nrow(tasks) > 0) tasks <- tasks[!duplicated(tasks$id), ]
      if (!is.null(note_comments) && nrow(note_comments) > 0) note_comments <- note_comments[!duplicated(note_comments$id), ]
      dr_data(list(
        date = report_date, month_label = mm$label, month_dates = dates, month_mode = TRUE,
        work_orders = work_orders %||% data.frame(),
        tasks = tasks %||% data.frame(),
        task_logs = task_logs %||% data.frame(),
        note_comments = note_comments %||% data.frame(),
        users = daily_report_get_users()
      ))
    } else {
      # 日模式
      work_orders <- daily_report_get_work_orders(report_date)
      tasks <- daily_report_get_tasks(report_date)
      task_logs <- daily_report_get_task_logs(report_date)
      note_comments <- daily_report_get_note_comments(report_date)
      dr_data(list(
        date = report_date, month_mode = FALSE,
        work_orders = work_orders, tasks = tasks,
        task_logs = task_logs, note_comments = note_comments,
        users = daily_report_get_users()
      ))
    }
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
    note_comments <- data$note_comments
    user_filter <- input$dr_user_filter

    if (nrow(users) == 0) return(div(class = "dr-empty", "暂无用户数据"))

    # 筛选用户
    if (!is.null(user_filter) && user_filter != "all") {
      users <- users[users$id == as.integer(user_filter), , drop = FALSE]
    }

    # 为每个用户生成日报卡片
    cards_html <- ""
    text_report <- ""

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
        user_logs <- task_logs[!is.na(task_logs$creator_name) & (task_logs$creator_name == u$display_name | task_logs$creator_name == u$username), , drop = FALSE]
      }

      # 该用户的记事评论（排除元任务 NTE20260606002）
      user_notes <- data.frame()
      if (nrow(note_comments) > 0) {
        user_notes <- note_comments[!is.na(note_comments$created_by) & note_comments$created_by == uid &
          note_comments$note_no != "NTE20260606002", , drop = FALSE]
      }

      # 无数据则跳过
      if (nrow(user_wo) == 0 && nrow(user_tasks) == 0 && nrow(user_logs) == 0 && nrow(user_notes) == 0) next

      # 统计
      wo_count <- nrow(user_wo)
      task_count <- nrow(user_tasks)
      log_count <- nrow(user_logs)
      note_count <- nrow(user_notes)

      # 工作日志标题：月报或日期
    report_label <- if (isTRUE(data$month_mode) && !is.null(data$month_label)) {
      data$month_label
    } else {
      substr(as.character(data$date), 1, 10)
    }

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

      # 工作日志（按记事分组，层级缩进：4空格/级）
      note_html <- ""
      if (note_count > 0) {
        note_by_no <- split(user_notes, user_notes$note_no)
        note_items <- ""
        gi <- 0
        for (gn in names(note_by_no)) {
          gi <- gi + 1; grp <- note_by_no[[gn]]
          gn_title <- grp$note_title[1] %||% gn
          tops <- grp[is.na(grp$parent_id) | grp$parent_id == 0, , drop = FALSE]
          reps <- grp[!(is.na(grp$parent_id) | grp$parent_id == 0), , drop = FALSE]
          # 一级标题（字体加大）
          note_items <- paste0(note_items, sprintf(
            '<div style="font-size:14px;font-weight:600;color:#6c3bbf;margin:6px 0 4px;">%s、 📋 %s %s · %d条</div>',
            dr_cn_number(gi), gn, gn_title, nrow(grp)))
          # 清理空白行辅助函数
          .clean_lines <- function(txt) { trimws(gsub("\n\\s*\n", "\n", txt)) }
          ni <- 0
          # 递归子回复（每级缩进 4字符 ≈ 2em）
          render_replies <- function(pid, prefix, indent_em) {
            sub <- reps[reps$parent_id == pid, , drop = FALSE]
            if (nrow(sub) == 0) return("")
            html <- ""
            for (si in seq_len(nrow(sub))) {
              sc <- sub[si, ]
              sct <- .clean_lines(sc$content)
              num_lab <- sprintf("%s.%d", prefix, si)          # 纯数字编号: "1.1"
              dot_cnt <- nchar(gsub("[^.]", "", num_lab))       # 1=二级, 2=三级
              show_lab <- paste0(paste(rep("+", dot_cnt), collapse=""), num_lab)  # "+1.1" / "++1.1.1"
              html <- paste0(html, sprintf(
                '<div class="dr-item" style="padding-left:%dem; white-space:pre-wrap;"><span class="dr-badge" style="background:#a78bfa;">沟通%s</span>\n<div style="padding-left:2em;">%s</div></div>',
                indent_em, show_lab, sct))
              html <- paste0(html, render_replies(sc$id, num_lab, indent_em + 2))
            }
            html
          }
          for (ti in seq_len(nrow(tops))) {
            ni <- ni + 1; tc <- tops[ti, ]
            ct <- .clean_lines(tc$content)
            note_items <- paste0(note_items, sprintf(
              '<div class="dr-item" style="white-space:pre-wrap;"><span class="dr-badge" style="background:#6c3bbf;">工作%d</span>\n<div style="padding-left:2em;">%s</div></div>', ni, ct))
            if (nrow(reps) > 0) {
              note_items <- paste0(note_items, render_replies(tc$id, as.character(ni), 4))
            }
          }
        }
        note_html <- sprintf('<div class="dr-section"><div class="dr-section-title note">工作日志 %s (%d条)</div>%s</div>', report_label, note_count, note_items)
      }

      cards_html <- paste0(cards_html, sprintf(
        '<div class="dr-card">
          <div class="dr-card-header">
            <div class="dr-avatar">%s</div>
            <div class="dr-name">%s</div>
            <div class="dr-stats">工单 %d | 任务 %d | 记事 %d</div>
          </div>
          %s%s%s%s
        </div>', initial, uname, wo_count, task_count, note_count, wo_html, task_html, log_html, note_html))

      # 纯文本日报 — 新格式
      total_items <- wo_count + task_count + log_count + note_count
      tomorrow <- if (isTRUE(data$month_mode) && !is.null(data$month_dates)) {
        format(max(data$month_dates) + 1, "%Y-%m-%d")
      } else {
        format(as.Date(data$date) + 1, "%Y-%m-%d")
      }
      text_report <- paste0(text_report,
        sprintf("工作日志 %s (%d条) %s\n", report_label, total_items, uname))
      
      if (wo_count > 0) {
        text_report <- paste0(text_report, "\n工单\n")
        for (wi in 1:nrow(user_wo)) {
          w <- user_wo[wi, ]
          status_cn <- switch(as.character(w$status),
            "pending" = "待处理", "assigned" = "已派发", "processing" = "处理中",
            "completed" = "已完成", "closed" = "已关闭", w$status)
          order_no <- ifelse(is.na(w$order_no), "-", w$order_no)
          text_report <- paste0(text_report, sprintf("%s、 %s %s [%s]\n", dr_cn_number(wi), order_no, w$title, status_cn))
        }
      }
      if (task_count > 0) {
        text_report <- paste0(text_report, "\n任务\n")
        for (ti in 1:nrow(user_tasks)) {
          tk <- user_tasks[ti, ]
          status_cn <- switch(as.character(tk$status),
            "pending" = "待处理", "in_progress" = "进行中",
            "completed" = "已完成", "blocked" = "已阻塞", tk$status)
          task_no <- ifelse(is.na(tk$task_no), "-", tk$task_no)
          text_report <- paste0(text_report, sprintf("%s、 %s %s [%s]\n", dr_cn_number(ti), task_no, tk$task_name, status_cn))
        }
      }
      if (log_count > 0) {
        text_report <- paste0(text_report, "\n反馈日志\n")
        for (li in 1:nrow(user_logs)) {
          lg <- user_logs[li, ]
          text_report <- paste0(text_report, sprintf("[%s] %s\n", lg$task_no %||% "-", lg$content %||% ""))
        }
      }
      if (note_count > 0) {
        text_report <- paste0(text_report, "\n")
        note_by_no <- split(user_notes, user_notes$note_no)
        gi <- 0
        for (gn in names(note_by_no)) {
          gi <- gi + 1
          grp <- note_by_no[[gn]]
          tops <- grp[is.na(grp$parent_id) | grp$parent_id == 0, , drop = FALSE]
          reps <- grp[!(is.na(grp$parent_id) | grp$parent_id == 0), , drop = FALSE]
          text_report <- paste0(text_report, sprintf("%s、 %s\n", dr_cn_number(gi), grp$note_title[1] %||% ""))
          # 递归渲染子回复。返回 list(txt=文本, has_children=是否有下级)
          # parent_num: 父级序号，用于生成 1.1 / 2.3 格式
          txt_render_replies <- function(pid, depth, parent_num = NULL) {
            sub <- reps[reps$parent_id == pid, , drop = FALSE]
            if (nrow(sub) == 0) return(list(txt = "", has_children = FALSE))
            indent <- paste(rep("  ", depth), collapse = "")
            txt <- ""
            for (si in seq_len(nrow(sub))) {
              sc <- sub[si, ]
              # 子评论内容每行加4空格缩进
              sc_content <- gsub("\n", "\n    ", sc$content %||% "")
              if (depth >= 2) {
                lead <- paste(rep("+", depth), collapse = "")
                txt <- paste0(txt, sprintf("%s%s- %s\n", indent, lead, sc_content))
              } else {
                # depth==1: 二级序号 1.1 / 1.2 ...
                num <- if (!is.null(parent_num)) sprintf("%d.%d", parent_num, si) else sprintf("%d", si)
                txt <- paste0(txt, sprintf("%s%s %s\n", indent, num, sc_content))
              }
              child_result <- txt_render_replies(sc$id, depth + 1)
              txt <- paste0(txt, child_result$txt)
            }
            has_any <- nrow(sub) > 0
            list(txt = txt, has_children = has_any)
          }
          for (ti in seq_len(nrow(tops))) {
            tc <- tops[ti, ]
            text_report <- paste0(text_report, sprintf("%d、 %s\n", ti, tc$content))
            # 直接检查此评论是否有子回复
            has_replies <- nrow(reps[reps$parent_id == tc$id, , drop = FALSE]) > 0
            if (has_replies) {
              child <- txt_render_replies(tc$id, 1, parent_num = ti)
              if (nchar(child$txt) > 0) {
                text_report <- paste0(text_report, child$txt)
              }
              # 有子回复，结尾加空行
              text_report <- paste0(text_report, "\n")
            }
          }
          # 笔记间固定一个空行分隔
          text_report <- paste0(text_report, "\n")
        }
      }
      # 去除连续多余空行（保留最多一个空行 = \n\n）
      text_report <- gsub("\n{3,}", "\n\n", text_report)
      # 去掉日期括号：From吴时超（7月13日）→ From吴时超
      text_report <- gsub("（\\d{1,2}月\\d{1,2}日）", "", text_report)
      # 明日计划
      text_report <- paste0(text_report, sprintf("\n明日计划 %s\n重点跟进：\n\n", tomorrow))
    }

    # Admin 派发汇总（底部展示所有被派发记事的评论）
    dispatch_html <- ""
    is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
    if (is_admin) {
      con <- db_connect()
      disps <- tryCatch({
        if (isTRUE(data$month_mode) && !is.null(data$month_dates)) {
          date_clause <- sprintf("date(nc.created_at) BETWEEN '%s' AND '%s'",
            format(min(data$month_dates), "%Y-%m-%d"),
            format(max(data$month_dates), "%Y-%m-%d"))
        } else {
          date_clause <- sprintf("date(nc.created_at) = '%s'", format(as.Date(data$date), "%Y-%m-%d"))
        }
        dbGetQuery(con, sprintf(
          "SELECT DISTINCT n.id, n.note_no, n.title,
                  nc.content, nc.status, nc.completed_at,
                  u.username, COALESCE(NULLIF(u.display_name,''), u.username) as display_name,
                  nc.created_at as comment_time
           FROM notes n
           JOIN note_dispatches nd ON nd.note_id = n.id
           LEFT JOIN note_comments nc ON nc.note_id = n.id
           LEFT JOIN users u ON nc.created_by = u.id
           WHERE %s
           ORDER BY n.title, nc.created_at", date_clause))
      }, error = function(e) data.frame(), finally = { db_disconnect(con) })
      if (nrow(disps) > 0) {
        by_title <- split(disps, disps$title)
        dispatch_html <- '<div class="dr-section"><div class="dr-section-title" style="background:#ede9fe;color:#5b21b6;">📨 派发记事汇总</div>'
        for (tn in names(by_title)) {
          grp <- by_title[[tn]]
          dispatch_html <- paste0(dispatch_html, sprintf(
            '<div style="font-weight:600;color:#6c3bbf;margin:6px 0 3px;">📋 %s</div>', tn))
          for (ri in seq_len(nrow(grp))) {
            r <- grp[ri, ]
            done_badge <- if (!is.na(r$status) && r$status == "completed")
              '<span style="display:inline-block;background:#d4edda;color:#155724;border-radius:10px;padding:0 8px;font-size:10px;margin-left:6px;">✅ 已完成</span>' else ""
            dispatch_html <- paste0(dispatch_html, sprintf(
              '<div style="font-size:12px;padding:2px 0 2px 16px;color:#555;">%s <span style="color:#999;">(%s)</span>%s</div>',
              r$content, r$display_name, done_badge))
          }
        }
        dispatch_html <- paste0(dispatch_html, '</div>')
      }
    }

    if (cards_html == "" && dispatch_html == "") {
      cards_html <- '<div class="dr-empty" style="text-align:center;padding:40px;font-size:15px;color:#999;">该日期暂无工作记录</div>'
      text_report <- paste0(text_report, "该日期暂无工作记录\n")
    }
    cards_html <- paste0(cards_html, dispatch_html)

    # 更新隐藏textarea的内容
    session$sendCustomMessage("dr_update_text", text_report)

    HTML(cards_html)
  })

  # 时间流水
  output$dr_timeline_content <- renderUI({
    req(rv$logged_in)
    data <- dr_data()
    if (is.null(data)) return(div(class = "dr-empty", "请选择日期并点击刷新"))
    note_comments <- data$note_comments
    if (is.null(note_comments) || nrow(note_comments) == 0)
      return(div(class = "dr-empty", style = "text-align:center;padding:40px;", "暂无评论记录"))

    # 多日模式：取日期范围；单日模式：取当天
    if (isTRUE(data$month_mode) && !is.null(data$month_dates)) {
      dates <- sort(data$month_dates)
      period_label <- data$month_label %||% paste(format(min(dates), "%m/%d"), "~", format(max(dates), "%m/%d"))
    } else {
      dates <- as.Date(data$date)
      period_label <- format(dates, "%Y-%m-%d")
    }

    total_count <- 0
    timeline_html <- ""

    # 用索引遍历保持 Date 类（for-in 会剥除 S3 class）
    for (di in seq_along(dates)) {
      d <- dates[di]
      day_str <- format(d, "%Y-%m-%d")
      day_label <- format(d, "%m月%d日 %a")
      day_comments <- note_comments[substr(note_comments$created_at, 1, 10) == day_str &
        note_comments$note_no != "NTE20260606002", , drop = FALSE]
      if (nrow(day_comments) == 0) next
      day_comments <- day_comments[order(day_comments$created_at), ]
      total_count <- total_count + nrow(day_comments)

      # 日期标题行
      timeline_html <- paste0(timeline_html, sprintf(
        '<div class="tl-hour-label" style="width:auto;font-size:13px;color:#4338ca;border-color:#a5b4fc;background:#eef2ff;margin:16px 0 8px;">📅 %s · %d条</div>',
        day_label, nrow(day_comments)))

      # 整点区间标签（06:00 ~ 23:00）
      hours <- 6:23
      for (h in hours) {
        hr_label <- sprintf("%02d:00", h)
        hr_str <- sprintf("%02d:", h)
        hr_items <- day_comments[substr(day_comments$created_at, 12, 14) == hr_str, , drop = FALSE]
        if (nrow(hr_items) == 0) next
        timeline_html <- paste0(timeline_html, sprintf('<div class="tl-hour-label">%s</div>', hr_label))
        for (ti in seq_len(nrow(hr_items))) {
          tc <- hr_items[ti, ]
          tm <- substr(tc$created_at, 12, 16)
          who <- if (!is.na(tc$display_name) && nchar(tc$display_name) > 0) tc$display_name else tc$username
          content <- trimws(tc$content %||% "")
          if (nchar(content) > 200) content <- paste0(substr(content, 1, 200), "…")
          note_tag <- if (!is.na(tc$note_title) && nchar(tc$note_title) > 0)
            sprintf('<span class="tl-tag">%s</span>', tc$note_title) else ""
          timeline_html <- paste0(timeline_html, sprintf(
            '<div class="tl-item">
              <div class="tl-card">
                <div class="tl-card-hd">
                  <div class="tl-card-hd-left"><b>%s</b>%s</div>
                  <span class="tl-time-badge">%s</span>
                </div>
                <div class="tl-card-bd">%s</div>
              </div>
            </div>',
            who, note_tag, tm, content))
        }
      }
    }

    if (total_count == 0)
      return(div(class = "dr-empty", style = "text-align:center;padding:40px;", "所选时间范围内暂无评论记录"))

    HTML(sprintf(
      '<div class="dr-timeline">
        <div class="tl-header">⏱ %s · 共 %d 条评论</div>
        %s
      </div>', period_label, total_count, timeline_html))
  })

  # 复制完成通知
  observeEvent(input$dr_copy_done, {
    showNotification("日报文本已复制到剪贴板", type = "message")
  })
}
