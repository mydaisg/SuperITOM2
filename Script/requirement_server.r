# 需求模块 Server
requirement_server <- function(input, output, session, rv) {
  req_trigger <- reactiveVal(0)
  req_selected <- reactiveVal(NULL)          # 当前选中需求 ID（用于进度Tab）
  req_prog_trigger <- reactiveVal(0)         # 进度刷新触发器
  req_gantt_trigger <- reactiveVal(0)

  # 需求列表（Tab1）
  output$req_list <- renderUI({
    req(rv$logged_in); req_trigger()
    kw <- trimws(input$req_search %||% "")
    st <- trimws(input$req_filter_status %||% "")
    items <- requirement_get_all(status = if (st == "") NULL else st, keyword = if (kw == "") NULL else kw)
    if (is.null(items) || nrow(items) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "暂无需求，点击「新建需求」开始"))
    }
    sel_id <- req_selected()
    parts <- c()
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      active <- !is.null(sel_id) && sel_id == r$id
      status_col <- requirement_status_color(r$status %||% "进行中")
      prio_col <- switch(r$priority %||% "中", "高" = "#d9534f", "中" = "#f0ad4e", "低" = "#5bc0de", "#999")
      date_html <- ""
      if (!is.null(r$start_date) && !is.na(r$start_date) && r$start_date != "" &&
          !is.null(r$due_date) && !is.na(r$due_date) && r$due_date != "") {
        date_html <- sprintf('<span style="font-size:11px;color:#999;margin-left:8px;">%s ~ %s</span>',
          r$start_date, r$due_date)
      }
      desc_html <- if (!is.null(r$description) && !is.na(r$description) && nchar(r$description) > 0)
        sprintf('<div style="font-size:12px;color:#555;margin-top:6px;">%s</div>', substr(r$description, 1, 120)) else ""
      parts <- c(parts, sprintf(
        '<div class="req-card%s" onclick="Shiny.setInputValue(\'req_select_click\',%d,{priority:\'event\'});">
          <div style="font-size:11px;color:#888;font-family:Consolas,monospace;">%s</div>
          <div style="margin-top:2px;">
            <b style="font-size:15px;color:#1a237e;">%s</b>
            <span class="req-badge" style="background:%s;">%s</span>
            <span class="req-badge" style="background:%s;">%s</span>
            %s
          </div>
          %s
          <div style="font-size:11px;color:#999;margin-top:6px;">负责人：%s · 更新：%s
            <span style="float:right;">
              <button class="btn btn-xs btn-info" onclick="event.stopPropagation();Shiny.setInputValue(\'req_edit_click\',%d,{priority:\'event\'});">✏</button>
              <button class="btn btn-xs btn-dark" onclick="event.stopPropagation();if(confirm(\'确认删除此需求？\'))Shiny.setInputValue(\'req_delete_click\',%d,{priority:\'event\'});">🗑</button>
            </span>
          </div>
        </div>',
        if (active) " active" else "", r$id,
        r$req_no, r$title, status_col, r$status %||% "进行中", prio_col, r$priority %||% "中", date_html,
        desc_html, r$owner %||% "", substr(r$updated_at %||% r$created_at %||% "", 1, 16),
        r$id, r$id))
    }
    HTML(paste(parts, collapse = ""))
  })

  # 搜索按钮
  observeEvent(input$req_search_btn, { req_trigger(req_trigger() + 1) })

  # 点击需求卡片 → 选中并切到进度Tab
  observeEvent(input$req_select_click, {
    req(rv$logged_in, input$req_select_click)
    id <- as.integer(input$req_select_click)
    req_selected(id)
    req_prog_trigger(req_prog_trigger() + 1)
  })

  # 新建需求弹窗
  observeEvent(input$req_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新建需求",
      textInput("req_edit_title", "需求标题", width = "100%"),
      textAreaInput("req_edit_desc", "需求描述", rows = 4, width = "100%"),
      fluidRow(
        column(6, selectInput("req_edit_status", "状态", choices = requirement_status_choices(), selected = "进行中")),
        column(6, selectInput("req_edit_priority", "优先级", choices = requirement_priority_choices(), selected = "中"))
      ),
      fluidRow(
        column(6, textInput("req_edit_owner", "负责人", width = "100%")),
        column(6, dateInput("req_edit_start", "开始日期", value = Sys.Date()))
      ),
      dateInput("req_edit_due", "截止日期", value = Sys.Date() + 30),
      footer = tagList(
        actionButton("req_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$req_save_btn, {
    req(rv$logged_in, input$req_edit_title)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- requirement_add(
      input$req_edit_title, input$req_edit_desc, input$req_edit_status,
      input$req_edit_priority, input$req_edit_owner,
      as.character(input$req_edit_start), as.character(input$req_edit_due), uid)
    if (result$success) {
      removeModal()
      req_trigger(req_trigger() + 1)
      req_selected(result$id)
      req_prog_trigger(req_prog_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 编辑需求弹窗
  observeEvent(input$req_edit_click, {
    req(rv$logged_in)
    item <- requirement_get_by_id(as.integer(input$req_edit_click))
    if (is.null(item) || nrow(item) == 0) return()
    item <- item[1, ]
    showModal(modalDialog(
      title = paste("编辑需求", item$req_no),
      textInput("req_edit_title_m", "需求标题", value = item$title, width = "100%"),
      textAreaInput("req_edit_desc_m", "需求描述", value = item$description %||% "", rows = 4, width = "100%"),
      fluidRow(
        column(6, selectInput("req_edit_status_m", "状态", choices = requirement_status_choices(), selected = item$status %||% "进行中")),
        column(6, selectInput("req_edit_priority_m", "优先级", choices = requirement_priority_choices(), selected = item$priority %||% "中"))
      ),
      fluidRow(
        column(6, textInput("req_edit_owner_m", "负责人", value = item$owner %||% "", width = "100%")),
        column(6, dateInput("req_edit_start_m", "开始日期", value = if (!is.null(item$start_date) && !is.na(item$start_date) && item$start_date != "") as.Date(item$start_date) else Sys.Date()))
      ),
      dateInput("req_edit_due_m", "截止日期", value = if (!is.null(item$due_date) && !is.na(item$due_date) && item$due_date != "") as.Date(item$due_date) else Sys.Date() + 30),
      footer = tagList(
        actionButton("req_update_btn", "更新", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$req_update_btn, {
    req(rv$logged_in, input$req_edit_title_m)
    id <- as.integer(input$req_edit_click)
    result <- requirement_update(id,
      input$req_edit_title_m, input$req_edit_desc_m, input$req_edit_status_m,
      input$req_edit_priority_m, input$req_edit_owner_m,
      as.character(input$req_edit_start_m), as.character(input$req_edit_due_m))
    if (result$success) {
      removeModal()
      req_trigger(req_trigger() + 1)
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification("已更新", type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 删除需求
  observeEvent(input$req_delete_click, {
    req(rv$logged_in)
    id <- as.integer(input$req_delete_click)
    result <- requirement_delete(id)
    if (result$success) {
      if (!is.null(req_selected()) && req_selected() == id) req_selected(NULL)
      req_trigger(req_trigger() + 1)
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification("已删除", type = "message")
    }
  })

  # ================================================================
  # Tab2：进度（参照记事评论：层级 + 彩虹色 + 编号）
  # ================================================================
  output$req_progress_ui <- renderUI({
    req(rv$logged_in); req_prog_trigger()
    all_req <- requirement_get_all()
    if (is.null(all_req) || nrow(all_req) == 0) {
      return(div(style = "text-align:center;padding:40px;color:#999;",
        "请先在「需求」Tab 新建需求"))
    }
    sel_id <- req_selected()
    if (is.null(sel_id)) sel_id <- all_req$id[1]
    if (!(sel_id %in% all_req$id)) sel_id <- all_req$id[1]

    req_info <- all_req[all_req$id == sel_id, ][1, ]
    prog <- requirement_progress_get_by_req(sel_id)

    # 顶部：需求选择器 + 添加条目按钮
    top <- tagList(
      fluidRow(
        column(5, selectInput("req_prog_selector", "选择需求",
          choices = setNames(as.character(all_req$id), all_req$title),
          selected = as.character(sel_id), width = "100%")),
        column(3, actionButton("req_prog_add_btn", "添加进度条目", icon = icon("plus"),
          class = "btn-success btn-sm", style = "width:100%;")),
        column(4, div(style = "text-align:right;font-size:12px;color:#888;padding-top:6px;",
          sprintf("<b>%s</b>（%s）· 共 %d 条进度", req_info$req_no, req_info$status %||% "", nrow(prog))))
      ),
      hr()
    )

    if (nrow(prog) == 0) {
      body <- div(style = "text-align:center;padding:40px;color:#999;", "暂无进度条目，点击「添加进度条目」开始")
      return(tagList(top, body))
    }

    # 分离顶层（parent_id==0）与子条目（parent_id>0）
    prog$pid <- ifelse(is.na(prog$parent_id) | is.null(prog$parent_id), 0, prog$parent_id)
    tops <- prog[prog$pid == 0, , drop = FALSE]
    children <- prog[prog$pid != 0, , drop = FALSE]

    # 目标日期：当天工作记录；今天没有则标记最后一天（最大日期）
    valid_dates <- prog$progress_date[!is.na(prog$progress_date) & prog$progress_date != ""]
    target_date <- ""
    if (length(valid_dates) > 0) {
      today_str <- format(Sys.Date(), "%Y-%m-%d")
      if (today_str %in% valid_dates) {
        target_date <- today_str
      } else {
        target_date <- max(valid_dates)
      }
    }

    rainbow <- requirement_rainbow_colors()

    # 渲染单条记录（含其子记录递归）
    # 记录格式严格顺序：序号、人员、记事、时间、状态
    render_item <- function(p, num_label, color, level) {
      person <- if (!is.null(p$person) && !is.na(p$person) && p$person != "") p$person else ""
      date_str <- if (!is.null(p$progress_date) && !is.na(p$progress_date) && p$progress_date != "") p$progress_date else ""
      content <- if (!is.null(p$content) && !is.na(p$content) && p$content != "") p$content else ""
      st <- if (!is.null(p$status) && !is.na(p$status)) p$status else ""
      st_col <- requirement_progress_status_color(st)
      indent <- if (level > 0) sprintf("margin-left:%dpx;", level * 28) else ""
      # 当天（或最后一天）记录：浅绿色底色高亮
      is_today <- (target_date != "" && date_str == target_date)
      bg_color <- if (is_today) "#e8f5e9" else "#fafafa"

      # 子记录（递归）
      subs_html <- ""
      if (nrow(children) > 0) {
        subs <- children[children$pid == p$id, , drop = FALSE]
        if (nrow(subs) > 0) {
          sub_parts <- c()
          for (si in seq_len(nrow(subs))) {
            sub_label <- sprintf("%s.%d", num_label, si)
            sub_parts <- c(sub_parts, render_item(subs[si, ], sub_label, color, level + 1))
          }
          subs_html <- sprintf('<div style="margin-top:2px;">%s</div>', paste(sub_parts, collapse = ""))
        }
      }

      # 绿勾状态按钮（参照记事评论）：已完成→绿色实心✓，未完成→灰色空心✓
      is_done <- (st == "已完成")
      done_btn <- if (is_done) {
        sprintf('<button class="btn btn-xs btn-success" title="已完成，点击取消" onclick="Shiny.setInputValue(\'req_prog_done_click\',%d,{priority:\'event\'});">✓</button>', p$id)
      } else {
        sprintf('<button class="btn btn-xs btn-default" title="标记完成" onclick="Shiny.setInputValue(\'req_prog_done_click\',%d,{priority:\'event\'});">✓</button>', p$id)
      }

      # 严格顺序：序号、人员、记事、时间、状态（单行排列）
      seq_html <- sprintf('<span style="font-weight:bold;color:%s;font-family:Consolas,monospace;font-size:12px;">%s</span>', color, num_label)
      person_html <- if (person != "") sprintf('<span style="font-weight:bold;color:#333;font-size:13px;">%s</span>', person) else ""
      content_html <- if (content != "") sprintf('<span style="font-size:12px;color:#555;">%s</span>', content) else ""
      # 时间：当天（或最后一天）加绿色"今日"标记
      date_html <- if (date_str != "") {
        mark <- if (is_today) '<span style="color:#2e7d32;font-size:10px;margin-right:2px;">●今日</span>' else ""
        sprintf('<span style="color:#999;font-size:11px;white-space:nowrap;">%s%s</span>', mark, date_str)
      } else ""
      status_html <- if (st != "") {
        sprintf('<span style="display:inline-block;background:%s;color:#fff;font-size:10px;padding:1px 8px;border-radius:10px;white-space:nowrap;">%s</span>', st_col, st)
      } else ""

      sprintf(
        '<div style="background:%s;padding:6px 10px;margin-bottom:4px;border-radius:6px;border-left:4px solid %s;%s">
          <div style="display:flex;justify-content:space-between;align-items:center;gap:8px;">
            <div style="flex:1;min-width:0;display:flex;align-items:baseline;gap:6px;flex-wrap:wrap;">
              %s%s%s%s%s
            </div>
            <div style="white-space:nowrap;flex-shrink:0;">
              %s
              <button class="btn btn-xs btn-default" title="增加下级" onclick="Shiny.setInputValue(\'req_prog_addsub_click\',%d,{priority:\'event\'});">➕</button>
              <button class="btn btn-xs btn-info" title="编辑" onclick="Shiny.setInputValue(\'req_prog_edit_click\',%d,{priority:\'event\'});">✏</button>
              <button class="btn btn-xs btn-danger" title="删除" onclick="if(confirm(\'删除此进度条目及其下级？\'))Shiny.setInputValue(\'req_prog_delete_click\',%d,{priority:\'event\'});">🗑</button>
            </div>
          </div>
          %s
        </div>',
        bg_color, color, indent,
        seq_html, person_html, content_html, date_html, status_html,
        done_btn, p$id, p$id, p$id, subs_html)
    }

    # 按部门分组渲染（一级类编号：一、二、三...）
    depts <- unique(tops$dept)
    depts <- depts[!is.na(depts) & depts != ""]
    if (length(depts) == 0) depts <- "(未分组)"
    parts <- c()
    dept_idx <- 0L
    for (d in depts) {
      dept_idx <- dept_idx + 1L
      if (d == "(未分组)") {
        sub <- tops[is.na(tops$dept) | tops$dept == "", , drop = FALSE]
      } else {
        sub <- tops[!is.na(tops$dept) & tops$dept == d, , drop = FALSE]
      }
      if (nrow(sub) == 0) next
      rows <- c()
      for (j in seq_len(nrow(sub))) {
        p <- sub[j, ]
        col <- rainbow[((j - 1) %% length(rainbow)) + 1]
        rows <- c(rows, render_item(p, as.character(j), col, 0))
      }
      dept_cn <- requirement_num_to_cn(dept_idx)
      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:12px;">
          <div style="font-weight:700;font-size:14px;color:#0f2b5c;margin-bottom:8px;">%s、%s <span style="font-size:11px;color:#999;font-weight:400;">(%d条)</span></div>
          %s
        </div>', dept_cn, d, nrow(sub), paste(rows, collapse = "")))
    }
    tagList(top, HTML(paste(parts, collapse = "")))
  })

  # 进度需求选择器切换
  observeEvent(input$req_prog_selector, {
    req(rv$logged_in)
    req_selected(as.integer(input$req_prog_selector))
    req_prog_trigger(req_prog_trigger() + 1)
  })

  # 添加进度条目弹窗（顶层）
  observeEvent(input$req_prog_add_btn, {
    req(rv$logged_in)
    sel_id <- req_selected()
    if (is.null(sel_id)) {
      showNotification("请先选择需求", type = "warning"); return()
    }
    rv$req_prog_parent_id <- 0L
    showModal(modalDialog(
      title = "添加进度条目",
      textInput("req_prog_edit_dept", "部门", width = "100%", placeholder = "如：采购部"),
      textInput("req_prog_edit_person", "人员", width = "100%", placeholder = "如：李红"),
      textInput("req_prog_edit_status", "状态", value = "已完成", width = "100%",
        placeholder = "如：已完成 / 未回复 / 进行中"),
      dateInput("req_prog_edit_date", "日期", value = Sys.Date()),
      textAreaInput("req_prog_edit_content", "内容", rows = 3, width = "100%"),
      footer = tagList(
        actionButton("req_prog_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  # 增加下级弹窗
  observeEvent(input$req_prog_addsub_click, {
    req(rv$logged_in, input$req_prog_addsub_click)
    pid <- as.integer(input$req_prog_addsub_click)
    rv$req_prog_parent_id <- pid
    # 读取父条目，用于继承部门
    con <- db_connect(); on.exit(db_disconnect(con))
    parent <- dbGetQuery(con, sprintf("SELECT dept, person FROM requirement_progress WHERE id=%d", pid))
    parent_dept <- if (nrow(parent) > 0) parent$dept[1] %||% "" else ""
    showModal(modalDialog(
      title = sprintf("增加下级（父 #%d）", pid),
      textInput("req_prog_edit_dept", "部门", value = parent_dept, width = "100%"),
      textInput("req_prog_edit_person", "人员", width = "100%", placeholder = "如：李红"),
      textInput("req_prog_edit_status", "状态", value = "进行中", width = "100%",
        placeholder = "如：已完成 / 未回复 / 进行中"),
      dateInput("req_prog_edit_date", "日期", value = Sys.Date()),
      textAreaInput("req_prog_edit_content", "内容", rows = 3, width = "100%"),
      footer = tagList(
        actionButton("req_prog_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$req_prog_save_btn, {
    req(rv$logged_in)
    sel_id <- req_selected()
    if (is.null(sel_id)) return()
    pid <- rv$req_prog_parent_id %||% 0L
    result <- requirement_progress_add(sel_id,
      input$req_prog_edit_dept, input$req_prog_edit_person,
      input$req_prog_edit_status, as.character(input$req_prog_edit_date),
      input$req_prog_edit_content, parent_id = as.integer(pid))
    if (result$success) {
      removeModal()
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 编辑进度条目
  observeEvent(input$req_prog_edit_click, {
    req(rv$logged_in)
    con <- db_connect()
    on.exit(db_disconnect(con))
    p <- dbGetQuery(con, sprintf("SELECT * FROM requirement_progress WHERE id=%d", as.integer(input$req_prog_edit_click)))
    if (nrow(p) == 0) return()
    p <- p[1, ]
    showModal(modalDialog(
      title = "编辑进度条目",
      textInput("req_prog_edit_dept_m", "部门", value = p$dept %||% "", width = "100%"),
      textInput("req_prog_edit_person_m", "人员", value = p$person %||% "", width = "100%"),
      textInput("req_prog_edit_status_m", "状态", value = p$status %||% "", width = "100%"),
      dateInput("req_prog_edit_date_m", "日期", value = if (!is.null(p$progress_date) && !is.na(p$progress_date) && p$progress_date != "") as.Date(p$progress_date) else Sys.Date()),
      textAreaInput("req_prog_edit_content_m", "内容", value = p$content %||% "", rows = 3, width = "100%"),
      footer = tagList(
        actionButton("req_prog_update_btn", "更新", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$req_prog_update_btn, {
    req(rv$logged_in)
    id <- as.integer(input$req_prog_edit_click)
    result <- requirement_progress_update(id,
      input$req_prog_edit_dept_m, input$req_prog_edit_person_m,
      input$req_prog_edit_status_m, as.character(input$req_prog_edit_date_m),
      input$req_prog_edit_content_m)
    if (result$success) {
      removeModal()
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification("已更新", type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 删除进度条目（含下级级联）
  observeEvent(input$req_prog_delete_click, {
    req(rv$logged_in)
    id <- as.integer(input$req_prog_delete_click)
    # 级联删除所有下级
    con <- db_connect()
    tryCatch({
      # 递归收集子 id
      all_ids <- id
      queue <- id
      while (length(queue) > 0) {
        cur <- queue[1]; queue <- queue[-1]
        kids <- dbGetQuery(con, sprintf("SELECT id FROM requirement_progress WHERE parent_id=%d", cur))$id
        if (length(kids) > 0) { all_ids <- c(all_ids, kids); queue <- c(queue, kids) }
      }
      dbExecute(con, sprintf("DELETE FROM requirement_progress WHERE id IN (%s)", paste(all_ids, collapse=",")))
      dbDisconnect(con)
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification(sprintf("已删除 %d 条", length(all_ids)), type = "message")
    }, error = function(e) {
      dbDisconnect(con)
      showNotification(e$message, type = "error")
    })
  })

  # 绿勾状态更新（已完成 ↔ 进行中）
  observeEvent(input$req_prog_done_click, {
    req(rv$logged_in)
    result <- requirement_progress_toggle_done(as.integer(input$req_prog_done_click))
    if (result$success) {
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification(result$message, type = "message", duration = 1.5)
    } else showNotification(result$message, type = "error")
  })

  # ================================================================
  # Tab3：甘特图（简单版·时长数据条）
  # ================================================================
  output$req_gantt_filter_ui <- renderUI({
    req(rv$logged_in)
    all_req <- requirement_get_all()
    if (is.null(all_req) || nrow(all_req) == 0) return(NULL)
    selectInput("req_gantt_filter", "选择需求",
      choices = setNames(as.character(all_req$id), all_req$title),
      width = "100%")
  })

  observeEvent(input$req_gantt_refresh, { req_gantt_trigger(req_gantt_trigger() + 1) })

  output$req_gantt_chart_ui <- renderUI({
    req(rv$logged_in); req_gantt_trigger()
    all_req <- requirement_get_all()
    if (is.null(all_req) || nrow(all_req) == 0) {
      return(div(style = "padding:40px;color:#999;text-align:center;", "暂无需求数据"))
    }
    sel_id <- if (!is.null(input$req_gantt_filter)) as.integer(input$req_gantt_filter) else all_req$id[1]
    if (!(sel_id %in% all_req$id)) sel_id <- all_req$id[1]
    req_info <- all_req[all_req$id == sel_id, ][1, ]

    # 取该需求的进度条目，按部门分组，部门 = 阶段（仅顶层，排除子记录）
    prog <- requirement_progress_get_by_req(sel_id)
    if (nrow(prog) > 0) {
      prog$pid <- ifelse(is.na(prog$parent_id) | is.null(prog$parent_id), 0, prog$parent_id)
      prog <- prog[prog$pid == 0, , drop = FALSE]
    }

    # 如果没有进度条目，退化为用需求本身的 start_date/due_date 单条
    if (nrow(prog) == 0) {
      start_d <- req_info$start_date %||% ""
      due_d <- req_info$due_date %||% ""
      items <- data.frame(
        name = req_info$title, dept = "", status = req_info$status %||% "进行中",
        date = if (start_d != "") start_d else "",
        stringsAsFactors = FALSE
      )
    } else {
      items <- prog
    }

    # 解析日期，计算全局时间范围
    date_vals <- c()
    for (i in seq_len(nrow(items))) {
      d <- items$progress_date[i]
      if (is.null(d) || is.na(d) || d == "") next
      dv <- as.Date(d)
      if (!is.na(dv)) date_vals <- c(date_vals, as.numeric(dv))
    }
    if (length(date_vals) == 0) {
      return(div(style = "padding:40px;color:#999;text-align:center;",
        "暂无日期数据，无法绘制甘特图。请在「进度」Tab 添加带日期的进度条目。"))
    }
    global_start <- as.Date(min(date_vals), origin = "1970-01-01")
    global_end   <- as.Date(max(date_vals), origin = "1970-01-01")
    total_days <- as.numeric(global_end - global_start) + 1
    if (total_days < 1) total_days <- 1

    # 图例：按状态颜色
    statuses <- unique(items$status)
    statuses <- statuses[!is.na(statuses) & statuses != ""]
    legend_parts <- c()
    for (s in statuses) {
      col <- requirement_progress_status_color(s)
      legend_parts <- c(legend_parts, sprintf(
        '<span class="li"><span class="dot" style="background:%s;"></span>%s</span>', col, s))
    }
    legend_html <- paste(legend_parts, collapse = "")

    # 按部门分组构建行
    depts <- unique(items$dept)
    depts <- depts[!is.na(depts) & depts != ""]
    rows_html <- ""
    for (d in depts) {
      sub <- items[!is.na(items$dept) & items$dept == d, , drop = FALSE]
      if (nrow(sub) == 0) next
      # 阶段行：部门 = 阶段
      dept_dates <- as.numeric(as.Date(sub$progress_date))
      dept_dates <- dept_dates[!is.na(dept_dates)]
      d_start <- as.Date(min(dept_dates), origin = "1970-01-01")
      d_end   <- as.Date(max(dept_dates), origin = "1970-01-01")
      d_days  <- as.numeric(d_end - d_start) + 1
      d_offset <- as.numeric(d_start - global_start) / total_days * 100
      d_width  <- d_days / total_days * 100
      ph_col <- "#64748b"
      rows_html <- paste0(rows_html, sprintf(
        '<div class="sb-row sb-phase"><div class="sb-name"><span class="ph-name-block" style="background:%s;">%s</span><span class="sb-range-tag">%s~%s</span><span class="sb-day-block" style="background:%s;">约 %d 天</span></div><div class="sb-range"></div><div class="sb-barwrap"><div class="sb-bar sb-phasebar" style="margin-left:%.1f%%;width:%.1f%%;background:%s;" title="约 %d 天"></div></div></div>',
        ph_col, d, format(d_start, "%m月%d日"), format(d_end, "%m月%d日"), ph_col, d_days,
        d_offset, d_width, ph_col, d_days))

      # 条目行
      for (j in seq_len(nrow(sub))) {
        p <- sub[j, ]
        p_date <- as.Date(p$progress_date)
        if (is.na(p_date)) next
        p_offset <- as.numeric(p_date - global_start) / total_days * 100
        p_width <- max(2.0, 1 / total_days * 100)  # 单点最小宽度 2%
        p_days <- 1
        col <- requirement_progress_status_color(p$status)
        name <- if (!is.null(p$person) && !is.na(p$person) && p$person != "") p$person else p$content %||% ""
        if (nchar(name) > 20) name <- substr(name, 1, 20)
        date_str <- format(p_date, "%m月%d日")
        rows_html <- paste0(rows_html, sprintf(
          '<div class="sb-row"><div class="sb-name">%s</div><div class="sb-range">%s</div><div class="sb-barwrap"><div class="sb-bar" style="margin-left:%.1f%%;width:%.1f%%;background:%s;" title="%d天"></div><span class="sb-days">%d天</span></div></div>',
          name, date_str, p_offset, p_width, col, p_days, p_days))
      }
    }
    # 未分组条目
    ungrouped <- items[is.na(items$dept) | items$dept == "", , drop = FALSE]
    if (nrow(ungrouped) > 0) {
      for (j in seq_len(nrow(ungrouped))) {
        p <- ungrouped[j, ]
        p_date <- as.Date(p$progress_date)
        if (is.na(p_date)) next
        p_offset <- as.numeric(p_date - global_start) / total_days * 100
        p_width <- max(2.0, 1 / total_days * 100)
        col <- requirement_progress_status_color(p$status)
        name <- if (!is.null(p$person) && !is.na(p$person) && p$person != "") p$person else p$content %||% ""
        if (nchar(name) > 20) name <- substr(name, 1, 20)
        date_str <- format(p_date, "%m月%d日")
        rows_html <- paste0(rows_html, sprintf(
          '<div class="sb-row"><div class="sb-name">%s</div><div class="sb-range">%s</div><div class="sb-barwrap"><div class="sb-bar" style="margin-left:%.1f%%;width:%.1f%%;background:%s;" title="1天"></div><span class="sb-days">1天</span></div></div>',
          name, date_str, p_offset, p_width, col))
      }
    }
    if (nchar(rows_html) == 0) {
      return(div(style = "padding:40px;color:#999;text-align:center;", "无有效日期数据"))
    }

    gantt_html <- sprintf(
      '<div class="gt-wrap">
        <div class="gt-title"><h2>%s</h2></div>
        <div class="gt-legend">%s</div>
        <div class="sb-table">%s</div>
      </div>',
      req_info$title, legend_html, rows_html)
    HTML(gantt_html)
  })

  outputOptions(output, "req_list", suspendWhenHidden = FALSE)
  outputOptions(output, "req_progress_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "req_gantt_chart_ui", suspendWhenHidden = FALSE)
}
