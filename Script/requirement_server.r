# 需求模块 Server

# 从记事选择（未完成）的下拉选项
req_prog_note_choices <- function(current_user = NULL) {
  notes <- requirement_get_uncompleted_notes(current_user)
  if (is.null(notes) || nrow(notes) == 0) {
    return(c("（无未完成记事）" = ""))
  }
  st_label <- ifelse(is.na(notes$status), "",
    ifelse(notes$status == "in_progress", "进行中",
      ifelse(notes$status == "pending", "待处理", notes$status)))
  no_prefix <- ifelse(!is.na(notes$note_no) & notes$note_no != "",
    sprintf("[%s] ", notes$note_no), sprintf("[#%d] ", notes$id))
  labels <- sprintf("%s%s · %s", no_prefix, notes$title, st_label)
  c("（不选择记事）" = "", setNames(as.character(notes$id), labels))
}

# 绑定弹窗的下拉选项：未完成且尚未绑定到该进度的记事（多选用，无空选项）
req_prog_note_bind_choices <- function(progress_id, current_user = NULL) {
  notes <- requirement_get_uncompleted_notes(current_user)
  if (is.null(notes) || nrow(notes) == 0) {
    return(character(0))
  }
  bound <- requirement_progress_notes_get(progress_id)
  if (!is.null(bound) && nrow(bound) > 0) {
    notes <- notes[!(notes$id %in% bound$note_id), , drop = FALSE]
  }
  if (nrow(notes) == 0) return(character(0))
  no_prefix <- ifelse(!is.na(notes$note_no) & notes$note_no != "",
    sprintf("[%s] ", notes$note_no), sprintf("[#%d] ", notes$id))
  labels <- sprintf("%s%s", no_prefix, notes$title)
  setNames(as.character(notes$id), labels)
}

# 需求进度 → 时间轴 HTML（样式参照总结模块"时间流水"）
req_render_timeline <- function(prog) {
  if (is.null(prog) || nrow(prog) == 0) {
    return('<div style="text-align:center;padding:40px;color:#999;">暂无进度记录</div>')
  }
  prog$pd <- ifelse(is.na(prog$progress_date), "", prog$progress_date)
  prog <- prog[prog$pd != "", , drop = FALSE]
  if (nrow(prog) == 0) {
    return('<div style="text-align:center;padding:40px;color:#999;">暂无带日期的进度记录</div>')
  }
  prog <- prog[order(prog$pd, prog$id), ]
  total <- nrow(prog)
  html <- ""
  days <- unique(prog$pd)
  for (d in days) {
    day_items <- prog[prog$pd == d, , drop = FALSE]
    dd <- tryCatch(as.Date(d), error = function(e) NA)
    day_label <- if (!is.na(dd)) format(dd, "%m月%d日 %a") else d
    html <- paste0(html, sprintf(
      '<div class="reqtl-day-label"><span>📅 %s · %d条</span></div>', day_label, nrow(day_items)))
    for (i in seq_len(nrow(day_items))) {
      p <- day_items[i, ]
      person <- if (!is.null(p$person) && !is.na(p$person) && p$person != "") p$person else "未署名"
      dept <- if (!is.null(p$dept) && !is.na(p$dept) && p$dept != "") p$dept else ""
      st <- if (!is.null(p$status) && !is.na(p$status) && p$status != "") p$status else ""
      st_col <- requirement_progress_status_color(st)
      content <- trimws(p$content %||% "")
      if (nchar(content) > 200) content <- paste0(substr(content, 1, 200), "…")
      dept_tag <- if (dept != "") sprintf('<span class="reqtl-tag">%s</span>', dept) else ""
      st_tag <- if (st != "") sprintf('<span class="reqtl-status" style="background:%s;">%s</span>', st_col, st) else ""
      html <- paste0(html, sprintf(
        '<div class="reqtl-item">
          <div class="reqtl-card">
            <div class="reqtl-card-hd">
              <div class="reqtl-card-hd-left"><b>%s</b>%s%s</div>
              <span class="reqtl-time-badge">%s</span>
            </div>
            <div class="reqtl-card-bd">%s</div>
          </div>
        </div>',
        person, dept_tag, st_tag, d, content))
    }
  }
  sprintf('<div class="reqtl-timeline"><div class="reqtl-header">⏱ 共 %d 条进度</div>%s</div>', total, html)
}

# 需求进度 → 纯文本（用于复制）
req_render_text_item <- function(p, num_label, level, children) {
  indent <- paste0(rep("  ", level), collapse = "")
  person <- if (!is.null(p$person) && !is.na(p$person) && p$person != "") p$person else ""
  date <- if (!is.null(p$progress_date) && !is.na(p$progress_date) && p$progress_date != "") p$progress_date else ""
  st <- if (!is.null(p$status) && !is.na(p$status) && p$status != "") p$status else ""
  content <- if (!is.null(p$content) && !is.na(p$content) && p$content != "") p$content else ""
  lines <- sprintf("%s%s. %s | %s | %s", indent, num_label, person, date, st)
  if (content != "") {
    for (cl in strsplit(content, "\n")[[1]]) {
      lines <- c(lines, sprintf("%s   %s", indent, cl))
    }
  }
  subs <- children[children$pid == p$id, , drop = FALSE]
  if (nrow(subs) > 0) {
    for (si in seq_len(nrow(subs))) {
      lines <- c(lines, req_render_text_item(subs[si, ], sprintf("%s.%d", num_label, si), level + 1, children))
    }
  }
  lines
}

req_render_text <- function(req_info, prog) {
  lines <- c(sprintf("【需求进度】%s %s（%s）",
    req_info$req_no %||% "", req_info$title %||% "", req_info$status %||% ""))
  lines <- c(lines, "──────────────────────────")
  if (is.null(prog) || nrow(prog) == 0) {
    lines <- c(lines, "（暂无进度记录）")
    return(paste(lines, collapse = "\n"))
  }
  prog$pid <- ifelse(is.na(prog$parent_id) | is.null(prog$parent_id), 0, prog$parent_id)
  tops <- prog[prog$pid == 0, , drop = FALSE]
  children <- prog[prog$pid != 0, , drop = FALSE]
  depts <- unique(tops$dept)
  depts <- depts[!is.na(depts) & depts != ""]
  if (length(depts) == 0) depts <- "(未分组)"
  dept_idx <- 0L
  for (d in depts) {
    dept_idx <- dept_idx + 1L
    if (d == "(未分组)") {
      sub <- tops[is.na(tops$dept) | tops$dept == "", , drop = FALSE]
    } else {
      sub <- tops[!is.na(tops$dept) & tops$dept == d, , drop = FALSE]
    }
    if (nrow(sub) == 0) next
    lines <- c(lines, sprintf("%s、%s", requirement_num_to_cn(dept_idx), d))
    for (j in seq_len(nrow(sub))) {
      lines <- c(lines, req_render_text_item(sub[j, ], as.character(j), 1, children))
    }
  }
  paste(lines, collapse = "\n")
}

requirement_server <- function(input, output, session, rv) {
  req_trigger <- reactiveVal(0)
  req_selected <- reactiveVal(NULL)          # 当前选中需求 ID（用于进度Tab）
  req_prog_trigger <- reactiveVal(0)         # 进度刷新触发器
  req_gantt_trigger <- reactiveVal(0)
  req_prog_view <- reactiveVal("list")       # 进度视图：list / timeline
  req_prog_range <- reactiveVal(NULL)        # 时间筛选：NULL=全部；c(from,to) Date
  req_prog_note_trigger <- reactiveVal(0)    # 记事绑定弹窗刷新触发器
  req_prog_note_mgr_id <- reactiveVal(NULL)  # 当前管理绑定的进度条目 id

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
    req(rv$logged_in); req_prog_trigger(); req_prog_view(); req_prog_range()
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
    total_all <- nrow(prog)

    # 时间筛选（按 progress_date 范围）
    rng <- req_prog_range()
    rng_from <- if (!is.null(rng)) rng[1] else NULL
    rng_to <- if (!is.null(rng)) rng[2] else NULL
    if (!is.null(rng) && nrow(prog) > 0) {
      from <- as.character(rng[1]); to <- as.character(rng[2])
      keep <- !is.na(prog$progress_date) & prog$progress_date != "" &
              prog$progress_date >= from & prog$progress_date <= to
      prog <- prog[keep, , drop = FALSE]
    }

    # 视图切换按钮样式
    list_btn_cls <- if (req_prog_view() == "list") "btn-primary" else "btn-default"
    tl_btn_cls <- if (req_prog_view() == "timeline") "btn-primary" else "btn-default"

    # 顶部：需求选择器 + 添加条目按钮 + 时间筛选 + 复制 + 视图切换
    top <- tagList(
      fluidRow(
        column(5, selectInput("req_prog_selector", "选择需求",
          choices = setNames(as.character(all_req$id), all_req$title),
          selected = as.character(sel_id), width = "100%")),
        column(3, actionButton("req_prog_add_btn", "添加进度条目", icon = icon("plus"),
          class = "btn-success btn-sm", style = "width:100%;")),
        column(4, div(style = "text-align:right;font-size:12px;color:#888;padding-top:6px;",
          sprintf("<b>%s</b>（%s）· 共 %d 条进度", req_info$req_no, req_info$status %||% "", total_all)))
      ),
      fluidRow(
        column(2, dateInput("req_prog_date_from", "开始", value = rng_from, width = "100%")),
        column(2, dateInput("req_prog_date_to", "结束", value = rng_to, width = "100%")),
        column(4, div(style = "padding-top:24px;white-space:nowrap;",
          actionButton("req_prog_range_all", "全部", class = "btn-sm btn-default"),
          actionButton("req_prog_range_today", "今天", class = "btn-sm btn-default"),
          actionButton("req_prog_range_week", "本周", class = "btn-sm btn-default"),
          actionButton("req_prog_range_month", "本月", class = "btn-sm btn-default"))),
        column(2, div(style = "padding-top:24px;",
          actionButton("req_prog_copy_btn", "复制", icon = icon("copy"), class = "btn-sm btn-default"))),
        column(2, div(style = "padding-top:24px;text-align:right;white-space:nowrap;",
          actionButton("req_prog_view_list", "列表", class = paste("btn-sm", list_btn_cls)),
          actionButton("req_prog_view_timeline", "时间轴", class = paste("btn-sm", tl_btn_cls))))
      ),
      hr()
    )

    # 复制文本（写入隐藏 textarea，由 JS 复制）
    text_report <- req_render_text(req_info, prog)
    session$sendCustomMessage("req_update_text", text_report)

    if (nrow(prog) == 0) {
      body <- div(style = "text-align:center;padding:40px;color:#999;", "当前筛选范围内暂无进度条目")
      return(tagList(top, body))
    }

    # 时间轴视图
    if (req_prog_view() == "timeline") {
      return(tagList(top, HTML(req_render_timeline(prog))))
    }

    # 记事绑定关系（progress_id → note_id 列表，供按钮 badge 显示）
    bound_rows <- requirement_progress_notes_get_by_req(sel_id)
    bound_map <- list()
    if (!is.null(bound_rows) && nrow(bound_rows) > 0) {
      bound_map <- split(bound_rows$note_id, bound_rows$progress_id)
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
    # counter：全局彩虹色计数器（参照记事评论：跨部门/跨层级连续，每条起始色不同，子级顺延而非继承父级同色）
    render_item <- function(p, num_label, counter, level) {
      person <- if (!is.null(p$person) && !is.na(p$person) && p$person != "") p$person else ""
      date_str <- if (!is.null(p$progress_date) && !is.na(p$progress_date) && p$progress_date != "") p$progress_date else ""
      content <- if (!is.null(p$content) && !is.na(p$content) && p$content != "") p$content else ""
      st <- if (!is.null(p$status) && !is.na(p$status)) p$status else ""
      st_col <- requirement_progress_status_color(st)
      color <- rainbow[((counter - 1) %% length(rainbow)) + 1]
      indent <- if (level > 0) sprintf("margin-left:%dpx;", level * 28) else ""
      # 当天（或最后一天）记录：浅绿色底色高亮
      is_today <- (target_date != "" && date_str == target_date)
      bg_color <- if (is_today) "#e8f5e9" else "#fafafa"

      # 子记录（递归）：子级颜色计数器顺延
      subs_html <- ""
      if (nrow(children) > 0) {
        subs <- children[children$pid == p$id, , drop = FALSE]
        if (nrow(subs) > 0) {
          sub_parts <- c()
          for (si in seq_len(nrow(subs))) {
            sub_label <- sprintf("%s.%d", num_label, si)
            sub_parts <- c(sub_parts, render_item(subs[si, ], sub_label, counter + si, level + 1))
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

      # 记事绑定按钮（🔗，点击打开绑定管理弹窗）
      bound_cnt <- length(bound_map[[as.character(p$id)]])
      note_label <- if (bound_cnt > 0) sprintf("🔗记事(%d)", bound_cnt) else "🔗记事"
      note_btn <- sprintf('<button class="btn btn-xs btn-warning" title="绑定/解绑记事" onclick="Shiny.setInputValue(\'req_prog_note_mgr_click\',%d,{priority:\'event\'});">%s</button>', p$id, note_label)

      # 严格顺序：序号、人员、时间、状态（单行排列）；记事放到明细区（可折叠）
      today_mark <- if (is_today) '<span style="color:#2e7d32;font-size:10px;font-weight:bold;margin-right:2px;">●今日</span>' else ""
      seq_html <- sprintf('<span style="font-weight:bold;color:%s;font-family:Consolas,monospace;font-size:12px;">%s</span>', color, num_label)
      person_html <- if (person != "") sprintf('<span style="font-weight:bold;color:#333;font-size:13px;font-family:\'Microsoft YaHei\',\'PingFang SC\',sans-serif;">%s</span>', person) else ""
      # 记事：支持输入换行（pre-wrap），字体放大一号
      content_html <- if (content != "") sprintf('<div style="font-size:13px;color:#555;white-space:pre-wrap;font-family:\'Microsoft YaHei\',\'PingFang SC\',sans-serif;padding-left:2px;">%s</div>', content) else ""
      # 时间
      date_html <- if (date_str != "") {
        sprintf('<span style="color:#999;font-size:11px;white-space:nowrap;font-family:\'Microsoft YaHei\',\'PingFang SC\',sans-serif;">%s</span>', date_str)
      } else ""
      status_html <- if (st != "") {
        sprintf('<span style="display:inline-block;background:%s;color:#fff;font-size:11px;padding:1px 8px;border-radius:10px;white-space:nowrap;font-family:\'Microsoft YaHei\',\'PingFang SC\',sans-serif;">%s</span>', st_col, st)
      } else ""

      # 一级/二级明细收缩/展开（参照记事评论）；三级及以下不折叠
      has_body <- (content != "" || subs_html != "")
      can_collapse <- (level %in% c(0L, 1L)) && has_body
      collapse_cls <- if (can_collapse) " reqprog-collapsible" else ""
      collapse_btn <- if (can_collapse) {
        sprintf('<button class="btn btn-xs btn-default reqprog-collapse-btn" data-id="%d" title="收缩/展开">▾ 展开</button>', p$id)
      } else ""
      body_display <- if (can_collapse) "display:none;" else ""

      sprintf(
        '<div class="reqprog-item%s" id="reqprog-%d" style="background:%s;padding:6px 10px;margin-bottom:4px;border-radius:6px;border-left:4px solid %s;%s">
          <div style="display:flex;justify-content:space-between;align-items:center;gap:8px;">
            <div style="flex:1;min-width:0;display:flex;align-items:baseline;gap:6px;flex-wrap:wrap;">
              %s%s%s%s%s
            </div>
            <div style="white-space:nowrap;flex-shrink:0;">
              %s%s%s
              <button class="btn btn-xs btn-default" title="增加下级" onclick="Shiny.setInputValue(\'req_prog_addsub_click\',%d,{priority:\'event\'});">➕</button>
              <button class="btn btn-xs btn-info" title="编辑" onclick="Shiny.setInputValue(\'req_prog_edit_click\',%d,{priority:\'event\'});">✏</button>
              <button class="btn btn-xs btn-danger" title="删除" onclick="if(confirm(\'删除此进度条目及其下级？\'))Shiny.setInputValue(\'req_prog_delete_click\',%d,{priority:\'event\'});">🗑</button>
            </div>
          </div>
          <div class="reqprog-body" id="reqprog-body-%d" style="%smargin-top:4px;">
            %s%s
          </div>
        </div>',
        collapse_cls, p$id, bg_color, color, indent,
        today_mark, seq_html, person_html, date_html, status_html,
        collapse_btn, done_btn, note_btn, p$id, p$id, p$id,
        p$id, body_display, content_html, subs_html)
    }

    # 按部门分组渲染（一级类编号：一、二、三...）
    depts <- unique(tops$dept)
    depts <- depts[!is.na(depts) & depts != ""]
    if (length(depts) == 0) depts <- "(未分组)"
    parts <- c()
    dept_idx <- 0L
    gidx <- 0L  # 全局彩虹色计数器：跨部门连续（不按部门重置），与记事评论每条起始色不同一致
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
        gidx <- gidx + 1L
        p <- sub[j, ]
        rows <- c(rows, render_item(p, as.character(j), gidx, 0))
      }
      dept_cn <- requirement_num_to_cn(dept_idx)
      # 一级行（部门标题）+ 右侧功能按键
      dept_header <- sprintf(
        '<div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;">
          <div style="font-weight:700;font-size:14px;color:#0f2b5c;font-family:\'Microsoft YaHei\',\'PingFang SC\',sans-serif;">%s、%s <span style="font-size:11px;color:#999;font-weight:400;">(%d条)</span></div>
          <div style="white-space:nowrap;flex-shrink:0;">
            <button class="btn btn-xs btn-default" title="在本部门下添加记录" onclick="Shiny.setInputValue(\'req_prog_dept_add_click\',\'%s\',{priority:\'event\'});">➕</button>
            <button class="btn btn-xs btn-info" title="编辑部门名" onclick="Shiny.setInputValue(\'req_prog_dept_edit_click\',\'%s\',{priority:\'event\'});">✏</button>
            <button class="btn btn-xs btn-danger" title="删除本部门及所有记录" onclick="if(confirm(\'删除部门「%s」及其所有记录？\'))Shiny.setInputValue(\'req_prog_dept_del_click\',\'%s\',{priority:\'event\'});">🗑</button>
          </div>
        </div>',
        dept_cn, d, nrow(sub), d, d, d, d)
      # 二级记录：整体缩进 2 个字符宽度（约 2em），体现一级层级
      parts <- c(parts, sprintf(
        '<div style="background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:14px;margin-bottom:12px;">
          %s
          <div style="margin-left:2em;">%s</div>
        </div>', dept_header, paste(rows, collapse = "")))
    }
    tagList(
      top,
      HTML('<div style="margin:0 0 10px;"><button class="btn btn-xs btn-default" onclick="reqprogToggleAll(this)">全部展开</button></div>'),
      HTML(paste(parts, collapse = ""))
    )
  })

  # ================================================================
  # 进度：时间筛选 / 视图切换 / 复制
  # ================================================================
  # 时间范围快捷按钮（直接设置范围，dateInput 跟随显示）
  observeEvent(input$req_prog_range_all, {
    req(rv$logged_in)
    req_prog_range(NULL)
  })
  observeEvent(input$req_prog_range_today, {
    req(rv$logged_in)
    req_prog_range(c(Sys.Date(), Sys.Date()))
  })
  observeEvent(input$req_prog_range_week, {
    req(rv$logged_in)
    d <- Sys.Date()
    monday <- d - as.integer(format(d, "%u")) + 1
    sunday <- monday + 6
    req_prog_range(c(monday, sunday))
  })
  observeEvent(input$req_prog_range_month, {
    req(rv$logged_in)
    d <- Sys.Date()
    first <- as.Date(format(d, "%Y-%m-01"))
    last <- seq(first, by = "month", length.out = 2)[2] - 1
    req_prog_range(c(first, last))
  })

  # dateInput 手动变化 → 更新范围状态
  observeEvent(list(input$req_prog_date_from, input$req_prog_date_to), {
    req(rv$logged_in)
    from <- input$req_prog_date_from
    to <- input$req_prog_date_to
    if (is.null(from) || is.null(to) || length(from) == 0 || length(to) == 0 ||
        is.na(from) || is.na(to)) {
      req_prog_range(NULL)
    } else {
      req_prog_range(c(as.Date(from), as.Date(to)))
    }
  })

  # 视图切换
  observeEvent(input$req_prog_view_list, { req_prog_view("list") })
  observeEvent(input$req_prog_view_timeline, { req_prog_view("timeline") })

  # 复制完成通知
  observeEvent(input$req_copy_done, {
    showNotification("进度文本已复制到剪贴板", type = "message")
  })

  # ================================================================
  # 进度条目 ↔ 记事 绑定/解绑管理
  # ================================================================
  # 打开绑定管理弹窗
  observeEvent(input$req_prog_note_mgr_click, {
    req(rv$logged_in, input$req_prog_note_mgr_click)
    pid <- as.integer(input$req_prog_note_mgr_click)
    req_prog_note_mgr_id(pid)
    req_prog_note_trigger(req_prog_note_trigger() + 1)
    showModal(modalDialog(
      title = sprintf("记事绑定管理（进度 #%d）", pid),
      tags$div(style = "font-weight:bold;margin-bottom:6px;", "已绑定记事："),
      uiOutput("req_prog_note_bound_ui"),
      hr(),
      tags$div(style = "font-weight:bold;margin-bottom:6px;", "新增绑定（选择未完成记事，可多选）："),
      selectInput("req_prog_note_bind_select", NULL,
        choices = req_prog_note_bind_choices(pid, rv$current_user),
        multiple = TRUE, width = "100%"),
      footer = tagList(
        actionButton("req_prog_note_bind_btn", "绑定并同步", class = "btn-primary"),
        modalButton("关闭")
      ), size = "l", easyClose = TRUE
    ))
  })

  # 渲染已绑定记事列表
  output$req_prog_note_bound_ui <- renderUI({
    req_prog_note_trigger()
    pid <- req_prog_note_mgr_id()
    if (is.null(pid)) return(NULL)
    bound <- requirement_progress_notes_get(pid)
    if (is.null(bound) || nrow(bound) == 0) {
      return(tags$div(style = "color:#999;padding:4px 0;", "暂无绑定记事"))
    }
    rows <- lapply(seq_len(nrow(bound)), function(i) {
      b <- bound[i, ]
      label <- sprintf("[%s] %s",
        if (!is.null(b$note_no) && !is.na(b$note_no) && b$note_no != "") b$note_no else paste0("#", b$note_id),
        b$title %||% "")
      tags$div(style = "display:flex;justify-content:space-between;align-items:center;padding:5px 8px;margin-bottom:4px;background:#f8f9fb;border:1px solid #e8ecf1;border-radius:6px;",
        tags$span(style = "font-size:13px;", label),
        tags$button(class = "btn btn-xs btn-danger", type = "button",
          onclick = sprintf("Shiny.setInputValue('req_prog_note_unbind_click', {pid:%d, nid:%d}, {priority:'event'});", pid, b$note_id),
          "解绑"))
    })
    tagList(rows)
  })

  # 绑定并同步评论
  observeEvent(input$req_prog_note_bind_btn, {
    req(rv$logged_in)
    pid <- req_prog_note_mgr_id()
    sel <- input$req_prog_note_bind_select
    if (is.null(pid) || is.null(sel) || length(sel) == 0) {
      showNotification("请选择要绑定的记事", type = "warning"); return()
    }
    prog <- requirement_progress_get_by_id(pid)
    if (is.null(prog) || nrow(prog) == 0) {
      showNotification("进度条目不存在", type = "error"); return()
    }
    req_info <- requirement_get_by_id(prog$requirement_id[1])
    req_label <- if (!is.null(req_info) && nrow(req_info) > 0) req_info$req_no[1] else ""
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    comment_text <- sprintf("【需求进度 %s】%s", req_label, prog$content[1] %||% "")
    ok <- 0; synced <- 0
    for (nid in sel) {
      nid <- as.integer(nid)
      res <- requirement_progress_note_bind(pid, nid)
      if (isTRUE(res$success)) {
        ok <- ok + 1
        if (!isTRUE(res$existed)) {
          cr <- note_comment_add(nid, comment_text, created_by = uid, current_user = rv$current_user)
          if (isTRUE(cr$success)) synced <- synced + 1
        }
      }
    }
    req_prog_note_trigger(req_prog_note_trigger() + 1)
    req_prog_trigger(req_prog_trigger() + 1)  # 刷新列表 badge
    updateSelectInput(session, "req_prog_note_bind_select",
      choices = req_prog_note_bind_choices(pid, rv$current_user))
    showNotification(sprintf("已绑定 %d 条记事，同步评论 %d 条", ok, synced), type = "message")
  })

  # 解绑
  observeEvent(input$req_prog_note_unbind_click, {
    req(rv$logged_in)
    val <- input$req_prog_note_unbind_click
    pid <- val$pid; nid <- val$nid
    if (is.null(pid) || is.null(nid)) return()
    res <- requirement_progress_note_unbind(as.integer(pid), as.integer(nid))
    req_prog_note_trigger(req_prog_note_trigger() + 1)
    req_prog_trigger(req_prog_trigger() + 1)  # 刷新列表 badge
    updateSelectInput(session, "req_prog_note_bind_select",
      choices = req_prog_note_bind_choices(pid, rv$current_user))
    showNotification(if (isTRUE(res$success)) "已解绑记事" else res$message,
      type = if (isTRUE(res$success)) "message" else "error")
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
    rv$req_prog_note_id <- NULL
    showModal(modalDialog(
      title = "添加进度条目",
      textInput("req_prog_edit_dept", "部门", width = "100%", placeholder = "如：采购部"),
      textInput("req_prog_edit_person", "人员", width = "100%", placeholder = "如：李红"),
      textInput("req_prog_edit_status", "状态", value = "已完成", width = "100%",
        placeholder = "如：已完成 / 未回复 / 进行中"),
      dateInput("req_prog_edit_date", "日期", value = Sys.Date()),
      selectInput("req_prog_note_select", "从记事选择（未完成）",
        choices = req_prog_note_choices(rv$current_user), width = "100%"),
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
    rv$req_prog_note_id <- NULL
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
      selectInput("req_prog_note_select", "从记事选择（未完成）",
        choices = req_prog_note_choices(rv$current_user), width = "100%"),
      textAreaInput("req_prog_edit_content", "内容", rows = 3, width = "100%"),
      footer = tagList(
        actionButton("req_prog_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  # 从记事选择：回填内容并记录 note_id（保存时同步写入该记事评论）
  observeEvent(input$req_prog_note_select, {
    req(rv$logged_in)
    val <- input$req_prog_note_select
    if (is.null(val) || val == "") {
      rv$req_prog_note_id <- NULL
      return()
    }
    note_id <- as.integer(val)
    rv$req_prog_note_id <- note_id
    note <- requirement_get_note_by_id(note_id)
    if (!is.null(note) && nrow(note) > 0) {
      n <- note[1, ]
      txt <- n$title[1] %||% ""
      body <- n$content[1] %||% ""
      if (!is.na(body) && nzchar(body)) txt <- paste0(txt, "\n", body)
      updateTextAreaInput(session, "req_prog_edit_content", value = txt)
    }
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
      # 若从记事选择，则持久化绑定并同步写入该记事评论
      note_id <- rv$req_prog_note_id
      note_msg <- ""
      if (!is.null(note_id) && !is.na(note_id) && as.integer(note_id) > 0) {
        bind_res <- requirement_progress_note_bind(result$id, as.integer(note_id))
        if (isTRUE(bind_res$success) && !isTRUE(bind_res$existed)) {
          uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
          req_info <- requirement_get_by_id(sel_id)
          req_label <- if (!is.null(req_info) && nrow(req_info) > 0) req_info$req_no[1] else ""
          comment_text <- sprintf("【需求进度 %s】%s", req_label, input$req_prog_edit_content %||% "")
          cr <- note_comment_add(as.integer(note_id), comment_text,
            created_by = uid, current_user = rv$current_user)
          note_msg <- if (isTRUE(cr$success)) "，已绑定并同步到记事评论" else paste0("，已绑定但同步评论失败：", cr$message)
        } else {
          note_msg <- "，记事已绑定"
        }
      }
      removeModal()
      rv$req_prog_note_id <- NULL
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification(paste0(result$message, note_msg), type = "message")
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

  # 一级行（部门）功能：本部门下添加记录
  observeEvent(input$req_prog_dept_add_click, {
    req(rv$logged_in, input$req_prog_dept_add_click)
    dept <- input$req_prog_dept_add_click
    rv$req_prog_parent_id <- 0L
    rv$req_prog_prefill_dept <- dept
    rv$req_prog_note_id <- NULL
    showModal(modalDialog(
      title = sprintf("在「%s」下添加记录", dept),
      textInput("req_prog_edit_dept", "部门", value = dept, width = "100%"),
      textInput("req_prog_edit_person", "人员", width = "100%", placeholder = "如：李红"),
      textInput("req_prog_edit_status", "状态", value = "进行中", width = "100%",
        placeholder = "如：已完成 / 未回复 / 进行中"),
      dateInput("req_prog_edit_date", "日期", value = Sys.Date()),
      selectInput("req_prog_note_select", "从记事选择（未完成）",
        choices = req_prog_note_choices(rv$current_user), width = "100%"),
      textAreaInput("req_prog_edit_content", "内容", rows = 3, width = "100%"),
      footer = tagList(
        actionButton("req_prog_save_btn", "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "l", easyClose = TRUE
    ))
  })

  # 一级行（部门）功能：编辑部门名
  observeEvent(input$req_prog_dept_edit_click, {
    req(rv$logged_in, input$req_prog_dept_edit_click)
    dept <- input$req_prog_dept_edit_click
    rv$req_prog_edit_dept_old <- dept
    showModal(modalDialog(
      title = sprintf("重命名部门「%s」", dept),
      textInput("req_prog_dept_new_name", "新部门名", value = dept, width = "100%"),
      footer = tagList(
        actionButton("req_prog_dept_rename_btn", "重命名", class = "btn-primary"),
        modalButton("取消")
      ), size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$req_prog_dept_rename_btn, {
    req(rv$logged_in)
    sel_id <- req_selected()
    old <- rv$req_prog_edit_dept_old %||% ""
    if (is.null(sel_id) || old == "") return()
    result <- requirement_progress_rename_dept(sel_id, old, input$req_prog_dept_new_name)
    if (result$success) {
      removeModal()
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 一级行（部门）功能：删除部门
  observeEvent(input$req_prog_dept_del_click, {
    req(rv$logged_in, input$req_prog_dept_del_click)
    sel_id <- req_selected()
    if (is.null(sel_id)) return()
    result <- requirement_progress_delete_dept(sel_id, input$req_prog_dept_del_click)
    if (result$success) {
      req_prog_trigger(req_prog_trigger() + 1)
      req_gantt_trigger(req_gantt_trigger() + 1)
      showNotification(result$message, type = "message")
    } else showNotification(result$message, type = "error")
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
