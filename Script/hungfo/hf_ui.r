##################
# HungFo 子项目 — 聚合 UI + Server
# 由 hf_core.r 的注册表自动生成：总览页 + 每个环节一页（Tab）
# 新增环节只需：新增 hf_mod_*.r + 在 server.R 加一行 source()
##################

# 注册 HungFo 静态资源路径（www 下文件通过 /hungfo/ 前缀访问，自包含、不依赖主应用 www）
addResourcePath("hungfo", file.path(hf_home(), "www"))

# ── 单个环节页面 UI ──
hf_module_page_ui <- function(m) {
  key <- m$key
  p <- function(x) paste0("hf_", key, "_", x)
  # 自动任务环节：特殊页面（Agent 任务来源编辑器）
  if (identical(key, "autotask")) return(hf_autotask_page_ui(m))
  # 元任务环节：特殊页面（任务记录队列，CodeBuddy 定时任务来源）
  if (identical(key, "metatask")) return(hf_metatask_page_ui(m))
  # 开发日志环节：特殊页面（查看界面，参照父项目开发日志样式）
  if (identical(key, "devlog")) return(hf_devlog_page_ui(m))
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;",
          paste0("HF-", toupper(key))),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;",
          hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;", m$desc),
        tags$div(style = "font-size:11px;color:#b0bec5;margin-top:4px;", icon("plug"), " 接口：", m$api_desc)
      ),
      # 记事式快速添加：多行输入，首行自动作为标题，全文作为内容
      div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:11px;font-weight:700;color:#6c3bbf;margin-bottom:4px;", "✦ 快速添加记录"),
        textAreaInput(p("new_text"), NULL, rows = 4, width = "100%",
          placeholder = "输入内容，第一行自动作为标题…"),
        div(style = "display:flex;gap:8px;justify-content:flex-end;margin-top:4px;",
          actionButton(p("add_btn"), "新增记录", icon = icon("plus"), class = "btn-success btn-sm"),
          actionButton(p("run_btn"), "运行接口", icon = icon("play"), class = "btn-primary btn-sm"),
          actionButton(p("refresh_btn"), "刷新", icon = icon("sync"), class = "btn-info btn-sm")
        )
      ),
      uiOutput(p("status")),
      DTOutput(p("table"))
    )
  )
}

# ── 自动任务环节专属页面：Agent 任务来源编辑器 ──
hf_autotask_page_ui <- function(m) {
  p <- function(x) paste0("hf_autotask_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-AUTOTASK"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
          "这里是 Agent 的任务来源：示例任务 / 正式任务统一在此编辑，下达给 Agent 们执行。监控页据此展示并切换「示例 / 正式」显示。")
      ),

      # 任务编辑区
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:8px;", "✦ 新增 / 编辑任务"),
        fluidRow(
          column(3, textInput(p("task_name"), "任务名称", placeholder = "如：竞品行情爬取")),
          column(2, selectInput(p("task_type"), "任务类型",
            choices = c("示例任务" = "sample", "正式任务" = "formal"), selected = "sample")),
          column(3, textInput(p("task_nodes"), "路径节点(逗号分隔)",
            placeholder = "ai_bot,chief_staff,orchestr,pm,web_res")),
          column(2, div(style = "margin-top:24px;",
            actionButton(p("task_add"), "新增任务", icon = icon("plus"), class = "btn-success btn-sm")))
        ),
        tags$div(style = "font-size:11px;color:#999;margin-top:4px;",
          "提示：路径节点 id 需与监控页 Agent 一致，常用起点 ai_bot → chief_staff → orchestr → pm → …。留空则使用默认路径。")
      ),

      # 任务列表（示例 / 正式 分栏切换）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 任务清单"),
          div(style = "margin-left:auto;display:flex;gap:6px;",
            actionButton(p("filter_sample"), "示例任务", class = "btn-default btn-xs"),
            actionButton(p("filter_formal"), "正式任务", class = "btn-default btn-xs"),
            actionButton(p("filter_all"), "全部", class = "btn-default btn-xs")
          ),
          actionButton(p("refresh_tasks"), "刷新", icon = icon("sync"), class = "btn-info btn-xs")
        ),
        DTOutput(p("task_table"))
      ),

      uiOutput(p("status")),
      DTOutput(p("table"))
    )
  )
}

# ── 元任务环节专属页面：任务记录队列（CodeBuddy 定时任务来源）──
hf_metatask_page_ui <- function(m) {
  p <- function(x) paste0("hf_metatask_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-METATASK"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
          "直接提交一条需求说明，标记为「待开发」。CodeBuddy 自动任务识别后执行，并把需求/分析/过程/结果写入开发日志，执行完成自动标记为「已完成」。")
      ),

      # 新增 / 编辑需求说明区（单框，无标题无 cron；提交需求按钮在左侧）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:8px;",
          uiOutput(p("mt_form_title"))),
        textAreaInput(p("mt_req"), NULL, rows = 5, width = "100%",
          placeholder = "在这里写一条需求说明，第一行自动作为标题…\n例如：给开发日志模块加一个查看界面，参照父项目样式。"),
        div(style = "display:flex;justify-content:flex-start;gap:8px;margin-top:6px;",
          uiOutput(p("mt_form_buttons"))
        )
      ),

      # 任务队列（待开发 / 开发异常可编辑，编辑按钮在左）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$style("#hf_metatask_mt_table table.dataTable, #hf_metatask_mt_table table.dataTable td, #hf_metatask_mt_table table.dataTable th { font-size:12px; }"),
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 需求队列"),
          div(style = "margin-left:auto;display:flex;gap:6px;",
            actionButton(p("mt_edit"), "编辑选中", icon = icon("pen"), class = "btn-primary btn-xs"),
            actionButton(p("mt_refresh"), "刷新", icon = icon("sync"), class = "btn-info btn-xs")
          )
        ),
        DTOutput(p("mt_table"))
      ),

      uiOutput(p("status"))
    )
  )
}

# ── 开发日志环节专属页面：查看界面（参照父项目开发日志样式）──
hf_devlog_page_ui <- function(m) {
  p <- function(x) paste0("hf_devlog_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-DEVLOG"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
          "记录 HungFo 子项目开发过程（需求 / 分析 / 方案 / 结果）。元任务执行过程会自动写入这里。")
      ),

      # 头部：统计徽章 + 搜索 + 刷新（同一行，参照父项目开发日志样式）
      div(style = "display:flex;align-items:center;gap:10px;margin-bottom:6px;flex-wrap:wrap;",
        tags$div(style = "display:flex;align-items:center;gap:4px;background:#f8f9fa;border:1px solid #e0e0e0;border-radius:12px;padding:3px 10px;font-size:13px;",
          tags$span(style = "color:#2563eb;", icon("file-alt")),
          tags$span(style = "font-weight:bold;color:#333;", uiOutput(p("dl_total"), inline = TRUE))
        ),
        tags$div(style = "display:flex;align-items:center;gap:4px;background:#f0fdf4;border:1px solid #dcfce7;border-radius:12px;padding:3px 10px;font-size:13px;",
          tags$span(style = "color:#0d7d3a;", icon("plus")),
          tags$span(style = "font-weight:bold;color:#333;", uiOutput(p("dl_today"), inline = TRUE))
        ),
        tags$span(style = "color:#ddd;font-size:18px;", "|"),
        tags$style(sprintf("#%s { height:26px; font-size:12px; padding:2px 6px; width:180px; }", p("dl_search"))),
        textInput(p("dl_search"), NULL, width = "180px", placeholder = "搜索日志…"),
        actionButton(p("dl_refresh"), NULL, icon = icon("sync"), class = "btn-xs btn-primary", style = "height:26px;")
      ),

      uiOutput(p("dl_list"))
    )
  )
}
hf_module_page_server <- function(input, output, session, rv, m, trigger) {
  key <- m$key
  p <- function(x) paste0("hf_", key, "_", x)
  # 自动任务环节：专属 Server（Agent 任务来源编辑）
  if (identical(key, "autotask")) return(hf_autotask_page_server(input, output, session, rv, m, trigger))
  # 元任务环节：专属 Server（任务记录队列）
  if (identical(key, "metatask")) return(hf_metatask_page_server(input, output, session, rv, m, trigger))
  # 开发日志环节：专属 Server（查看界面）
  if (identical(key, "devlog")) return(hf_devlog_page_server(input, output, session, rv, m, trigger))
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)

  output[[p("status")]] <- renderUI({
    trigger(); local_trigger()
    n <- hf_records_count(key)
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("状态："), tags$span(style = "color:#2e7d32;", hf_or(m$status, "scaffold")),
      " · 版本 ", hf_or(m$version, "v0.1"),
      " · ", tags$span(style = "color:#1565c0;", paste0("记录 ", n, " 条")))
  })

  output[[p("table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_records(key)
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(提示 = "暂无记录，点击 [新增记录] 或 [运行接口] 创建", stringsAsFactors = FALSE),
        options = list(dom = "t"), rownames = FALSE))
    }
    colnames(df) <- c("ID", "编号", "标题", "内容", "状态", "创建时间")
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "ltip"))
  })

  observeEvent(input[[p("add_btn")]], {
    req(rv$logged_in)
    t <- trimws(hf_or(input[[p("new_text")]], ""))
    if (!nzchar(t)) { showNotification("请输入内容", type = "warning"); return() }
    lines <- strsplit(t, "\n")[[1]]
    title <- trimws(lines[1])
    if (!nzchar(title)) title <- "未命名记录"
    r <- hf_record_add(key, title, payload = t, created_by = hf_current_uid(rv))
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    updateTextAreaInput(session, p("new_text"), value = "")
    local_refresh()
  })

  observeEvent(input[[p("run_btn")]], {
    req(rv$logged_in)
    t <- trimws(hf_or(input[[p("new_text")]], ""))
    title <- if (nzchar(t)) trimws(strsplit(t, "\n")[[1]][1]) else ""
    r <- tryCatch(
      hf_call(key, "run", payload = list(title = title)),
      error = function(e) list(success = FALSE, message = e$message))
    showNotification(hf_or(r$message, "接口已执行"), type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })

  observeEvent(input[[p("refresh_btn")]], local_refresh())
}

# ── 自动任务环节专属 Server ──
hf_autotask_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_autotask_", x)
  task_filter <- reactiveVal(NULL)      # NULL=全部 / sample / formal
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)

  output[[p("status")]] <- renderUI({
    trigger(); local_trigger()
    n <- hf_records_count("autotask")
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("状态："), tags$span(style = "color:#2e7d32;", hf_or(m$status, "scaffold")),
      " · 版本 ", hf_or(m$version, "v0.1"),
      " · ", tags$span(style = "color:#1565c0;", paste0("记录 ", n, " 条")))
  })

  # 任务清单表（按 filter 渲染，含「示例/正式切换」操作）
  output[[p("task_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_task_list(task_filter())
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(提示 = "暂无任务，点击上方 [新增任务] 创建", stringsAsFactors = FALSE),
        options = list(dom = "t"), rownames = FALSE))
    }
    df$类型 <- ifelse(df$task_type == "sample", "示例", "正式")
    df$启用 <- ifelse(df$enabled == 1, "是", "否")
    out <- data.frame(
      ID = df$id, 名称 = df$name, 类型 = df$类型, 启用 = df$启用,
      节点数 = vapply(df$nodes, function(x) length(tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))), integer(1)),
      stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, selection = "single",
      options = list(pageLength = 15, dom = "ltip"))
  })

  # 当前选中行 id
  selected_id <- reactive({
    idx <- input[[p("task_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    df <- hf_task_list(task_filter())
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$id[idx[1]]
  })

  # 筛选按钮
  observeEvent(input[[p("filter_sample")]], { task_filter("sample"); local_refresh() })
  observeEvent(input[[p("filter_formal")]], { task_filter("formal"); local_refresh() })
  observeEvent(input[[p("filter_all")]], { task_filter(NULL); local_refresh() })
  observeEvent(input[[p("refresh_tasks")]], local_refresh())

  # 新增任务
  observeEvent(input[[p("task_add")]], {
    req(rv$logged_in)
    name <- trimws(hf_or(input[[p("task_name")]], ""))
    if (!nzchar(name)) { showNotification("请输入任务名称", type = "warning"); return() }
    nodes_raw <- trimws(hf_or(input[[p("task_nodes")]], ""))
    nodes <- if (nzchar(nodes_raw)) {
      strsplit(gsub("\\s+", "", nodes_raw), ",")[[1]]
    } else HF_DEFAULT_TASK_NODES$research
    r <- hf_task_save(id = NULL, name = name, task_type = input[[p("task_type")]], nodes = nodes)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) {
      hf_write_tasks_js()   # 导出到监控页前端
      updateTextInput(session, p("task_name"), value = "")
      updateTextInput(session, p("task_nodes"), value = "")
    }
    local_refresh()
  })

  # 双击/选中后编辑：切换类型、删除（通过 DT 行操作按钮简化——这里提供快捷操作）
  observeEvent(input[[p("task_table_rows_selected")]], ignoreNULL = TRUE, {
    id <- selected_id()
    if (is.null(id)) return()
    df <- hf_task_list(task_filter())
    row <- df[df$id == id, ]
    if (nrow(row) == 0) return()
    # 回填编辑框
    updateTextInput(session, p("task_name"), value = row$name[1])
    updateSelectInput(session, p("task_type"), selected = row$task_type[1])
    nodes <- tryCatch(jsonlite::fromJSON(row$nodes[1]), error = function(e) character(0))
    updateTextInput(session, p("task_nodes"), value = paste(nodes, collapse = ","))
  })

  # 通用记录表（保留环节通用记事）
  output[[p("table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_records("autotask")
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(提示 = "暂无记录", stringsAsFactors = FALSE),
        options = list(dom = "t"), rownames = FALSE))
    }
    colnames(df) <- c("ID", "编号", "标题", "内容", "状态", "创建时间")
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "ltip"))
  })
}

# ── 元任务队列展示辅助：HTML 转义 / 状态颜色块 / payload 摘要 / Agent 徽章 ──
hf_html_escape <- function(x) {
  x <- hf_or(x, "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# 状态 → 颜色块（待开发黄 #f0ad4e / 执行中蓝 / 已完成绿 #5cb85c / 开发异常红 #d9534f）
hf_metatask_status_badge <- function(status) {
  st <- hf_or(status, "pending")
  map <- list(
    pending = list(label = "待开发", bg = "#f0ad4e", fg = "#fff"),
    running = list(label = "执行中", bg = "#5bc0de", fg = "#fff"),
    done    = list(label = "已完成", bg = "#5cb85c", fg = "#fff"),
    error   = list(label = "开发异常", bg = "#d9534f", fg = "#fff"),
    failed  = list(label = "开发异常", bg = "#d9534f", fg = "#fff")
  )
  m <- map[[st]]
  if (is.null(m)) m <- list(label = st, bg = "#9e9e9e", fg = "#fff")
  sprintf('<span style="display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;background:%s;color:%s;white-space:nowrap;">%s</span>',
          m$bg, m$fg, m$label)
}

# payload 内容单元格：单行摘要 + 悬浮 tooltip 显示全文（换行保留）
hf_metatask_payload_cell <- function(payload) {
  txt <- hf_or(payload, "")
  if (!nzchar(trimws(txt))) return('<span style="color:#bbb;">—</span>')
  one_line <- gsub("\\s+", " ", txt)
  brief <- if (nchar(one_line) > 60) paste0(substr(one_line, 1, 60), "…") else one_line
  tooltip <- gsub("\n", "&#10;", hf_html_escape(txt), fixed = TRUE)
  sprintf('<span style="color:#444;cursor:default;border-bottom:1px dashed #bbb;" title="%s">%s</span>',
          tooltip, hf_html_escape(brief))
}

# Agent 徽章：WorkBuddy 紫 / CodeBuddy 橙，未知显示 —
hf_metatask_agent_badge <- function(agent) {
  agt <- trimws(hf_or(agent, ""))
  if (!nzchar(agt)) return('<span style="color:#bbb;">—</span>')
  if (grepl("WorkBuddy", agt, ignore.case = TRUE)) {
    return('<span style="display:inline-block;padding:1px 7px;border-radius:10px;font-size:11px;background:#ede9fe;color:#5b21b6;white-space:nowrap;">WorkBuddy</span>')
  }
  if (grepl("CodeBuddy", agt, ignore.case = TRUE)) {
    return('<span style="display:inline-block;padding:1px 7px;border-radius:10px;font-size:11px;background:#fff3e0;color:#e65100;white-space:nowrap;">CodeBuddy</span>')
  }
  sprintf('<span style="display:inline-block;padding:1px 7px;border-radius:10px;font-size:11px;background:#f5f5f5;color:#555;white-space:nowrap;">%s</span>', hf_html_escape(agt))
}

# ── 元任务环节专属 Server ──
hf_metatask_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_metatask_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)
  editing_id <- reactiveVal(NULL)   # NULL=新增模式；非空=编辑该 id 需求

  # 表单标题（新增 / 编辑态切换）
  output[[p("mt_form_title")]] <- renderUI({
    id <- editing_id()
    if (is.null(id)) return(tags$span("✦ 新增需求说明"))
    tags$span("✦ 编辑需求 #", id, " ",
      tags$span(style = "color:#d9534f;font-weight:700;", "（保存后重置为待开发，等待重新执行）"))
  })

  # 表单按钮（新增态=提交需求；编辑态=保存修改 / 取消编辑）
  output[[p("mt_form_buttons")]] <- renderUI({
    id <- editing_id()
    if (is.null(id)) {
      return(actionButton(p("mt_add"), "提交需求", icon = icon("paper-plane"), class = "btn-success btn-sm"))
    }
    tagList(
      actionButton(p("mt_save"), "保存修改", icon = icon("check"), class = "btn-success btn-sm"),
      actionButton(p("mt_cancel"), "取消编辑", icon = icon("times"), class = "btn-default btn-sm")
    )
  })

  output[[p("status")]] <- renderUI({
    trigger(); local_trigger()
    q <- hf_metatask_list()
    n <- if (is.null(q)) 0 else nrow(q)
    pend <- if (is.null(q) || nrow(q) == 0) 0 else sum(q$status == "pending")
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("需求："), tags$span(style = "color:#1565c0;", paste0(n, " 条")),
      " · ", tags$span(style = "color:#e65100;", paste0("待开发 ", pend, " 条")),
      " · ", tags$span(style = "color:#888;", "执行完成自动写入开发日志"))
  })

  # 任务队列表（含 payload 内容、状态颜色块、完成 LLM / Agent）
  output[[p("mt_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    q <- hf_metatask_list()
    if (is.null(q) || nrow(q) == 0) {
      return(DT::datatable(
        data.frame(提示 = "暂无需求，在上方提交一条需求说明", stringsAsFactors = FALSE),
        options = list(dom = "t"), rownames = FALSE))
    }
    meta <- hf_metatask_session_meta()   # task_id, llm_name, agent（每个任务最近一次执行会话）
    has_meta <- !is.null(meta) && nrow(meta) > 0
    # 队列冗余字段（mark_done 写回）作为会话表之外的回退
    q_llm  <- if (!is.null(q$llm_name) && length(q$llm_name) >= nrow(q)) q$llm_name else rep("", nrow(q))
    q_agt  <- if (!is.null(q$agent)    && length(q$agent)    >= nrow(q)) q$agent    else rep("", nrow(q))
    n <- nrow(q)
    out <- data.frame(
      ID = q$id,
      需求 = vapply(seq_len(n), function(i) hf_html_escape(hf_or(q$title[i], "")), character(1)),
      内容 = vapply(seq_len(n), function(i) hf_metatask_payload_cell(hf_or(q$payload[i], "")), character(1)),
      状态 = vapply(seq_len(n), function(i) hf_metatask_status_badge(hf_or(q$status[i], "pending")), character(1)),
      完成LLM = vapply(seq_len(n), function(i) {
        llm <- ""
        if (has_meta) {
          hit <- meta[meta$task_id == q$id[i] & !is.na(meta$task_id), , drop = FALSE]
          if (nrow(hit) > 0) llm <- hf_or(hit$llm_name[1], "")
        }
        if (!nzchar(trimws(llm))) llm <- hf_or(q_llm[i], "")
        if (!nzchar(trimws(llm))) return('<span style="color:#bbb;">—</span>')
        sprintf('<span style="color:#333;">%s</span>', hf_html_escape(llm))
      }, character(1)),
      Agent = vapply(seq_len(n), function(i) {
        agt <- ""
        if (has_meta) {
          hit <- meta[meta$task_id == q$id[i] & !is.na(meta$task_id), , drop = FALSE]
          if (nrow(hit) > 0) agt <- hf_or(hit$agent[1], "")
        }
        if (!nzchar(trimws(agt))) agt <- hf_or(q_agt[i], "")
        hf_metatask_agent_badge(agt)
      }, character(1)),
      提交时间 = ifelse(is.na(q$created_at) | q$created_at == "", "—", q$created_at),
      完成时间 = ifelse(is.na(q$last_run_at) | q$last_run_at == "", "—", q$last_run_at),
      stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, selection = "single", escape = FALSE,
      options = list(pageLength = 15, dom = "ltip"))
  })

  selected_id <- reactive({
    idx <- input[[p("mt_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    q <- hf_metatask_list()
    if (is.null(q) || nrow(q) == 0) return(NULL)
    q$id[idx[1]]
  })

  # 新增需求（单框：需求说明，首行自动作标题，cron 留空=只跑一次）
  observeEvent(input[[p("mt_add")]], {
    req(rv$logged_in)
    req_text <- trimws(hf_or(input[[p("mt_req")]], ""))
    if (!nzchar(req_text)) { showNotification("请输入需求说明", type = "warning"); return() }
    lines <- strsplit(req_text, "\n")[[1]]
    title <- trimws(lines[1])
    if (!nzchar(title)) title <- "未命名需求"
    r <- hf_metatask_add(title, req_text, cron = "")
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) {
      editing_id(NULL)
      updateTextAreaInput(session, p("mt_req"), value = "")
    }
    local_refresh()
  })

  # 编辑选中（仅「待开发 / 开发异常」状态可编辑，回填到上方表单）
  observeEvent(input[[p("mt_edit")]], {
    id <- selected_id()
    if (is.null(id)) { showNotification("请先在列表选中一条需求", type = "warning"); return() }
    q <- hf_metatask_list()
    row <- q[q$id == id, , drop = FALSE]
    if (nrow(row) == 0) { showNotification("需求不存在，请刷新后重试", type = "warning"); return() }
    st <- hf_or(row$status[1], "pending")
    if (!(st %in% c("pending", "error", "failed"))) {
      showNotification("仅「待开发 / 开发异常」状态的需求可编辑", type = "warning"); return()
    }
    editing_id(id)
    updateTextAreaInput(session, p("mt_req"),
      value = hf_or(row$payload[1], hf_or(row$title[1], "")))
  })

  # 保存修改（编辑态提交，重置为待开发等待重新执行）
  observeEvent(input[[p("mt_save")]], {
    req(rv$logged_in)
    id <- editing_id()
    if (is.null(id)) return()
    req_text <- trimws(hf_or(input[[p("mt_req")]], ""))
    if (!nzchar(req_text)) { showNotification("请输入需求说明", type = "warning"); return() }
    lines <- strsplit(req_text, "\n")[[1]]
    title <- trimws(lines[1])
    if (!nzchar(title)) title <- "未命名需求"
    r <- hf_metatask_update(id, title, req_text)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) {
      editing_id(NULL)
      updateTextAreaInput(session, p("mt_req"), value = "")
    }
    local_refresh()
  })

  # 取消编辑（回到新增态并清空表单）
  observeEvent(input[[p("mt_cancel")]], {
    editing_id(NULL)
    updateTextAreaInput(session, p("mt_req"), value = "")
  })

  # 刷新：重读数据库并重渲染队列（local_trigger 为 mt_table / status 的响应式依赖）
  observeEvent(input[[p("mt_refresh")]], local_refresh())
}

# ── 开发日志环节专属 Server（查看界面，参照父项目开发日志样式）──
hf_devlog_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_devlog_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)
  search_term <- reactiveVal("")

  observeEvent(input[[p("dl_search")]], {
    search_term(trimws(input[[p("dl_search")]] %||% ""))
  })
  observeEvent(input[[p("dl_refresh")]], {
    search_term(trimws(input[[p("dl_search")]] %||% ""))
    local_refresh()
  })

  # 统计徽章（总数 / 今日）
  output[[p("dl_total")]] <- renderText({
    trigger(); local_trigger()
    as.character(hf_records_stats("devlog")$total)
  })
  output[[p("dl_today")]] <- renderText({
    trigger(); local_trigger()
    as.character(hf_records_stats("devlog")$today)
  })

  # 搜索词高亮（参照父项目 dev_log_server 逻辑）
  .hl <- function(text, words) {
    if (length(words) == 0 || is.null(text) || is.na(text) || !nzchar(text)) return(text)
    for (w in words) {
      if (!nzchar(w)) next
      pos <- gregexpr(w, text, ignore.case = TRUE)[[1]]
      if (pos[1] > 0) {
        parts <- c(); last <- 1
        for (i in seq_along(pos)) {
          pp <- pos[i]; ll <- attr(pos, "match.length")[i]
          if (pp > last) parts <- c(parts, substr(text, last, pp - 1))
          parts <- c(parts, '<mark style="background:#fde68a;color:#92400e;border-radius:3px;padding:0 2px;">',
            substr(text, pp, pp + ll - 1), '</mark>')
          last <- pp + ll
        }
        if (last <= nchar(text)) parts <- c(parts, substr(text, last, nchar(text)))
        text <- paste(parts, collapse = "")
      }
    }
    text
  }

  # payload 分段解析：【需求】/【分析】/【方案】/【结果】… → 有序 list(label, body)
  hf_devlog_sections <- function(txt) {
    txt <- hf_or(txt, "")
    if (!nzchar(trimws(txt))) return(list())
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    out <- list(); cur <- NULL
    for (ln in lines) {
      t <- trimws(ln, which = "left")
      if (nzchar(t) && (substr(t, 1, 1) == "\u3010" || substr(t, 1, 1) == "[")) {
        close <- regexpr("\u3011|]", t)
        if (close[1] > 1) {
          label <- trimws(substr(t, 2, close[1] - 1))
          body <- trimws(substr(t, close[1] + 1, nchar(t)))
          if (nzchar(label)) {
            out[[length(out) + 1]] <- list(label = label, body = body)
            cur <- length(out)
            next
          }
        }
      }
      if (!is.null(cur)) out[[cur]]$body <- paste(out[[cur]]$body, ln, sep = "\n")
    }
    out
  }

  # 分段样式映射（需求/分析/方案/结果，参照父项目配色）
  hf_devlog_section_style <- function(label) {
    switch(label,
      "需求" = list(icon = "clipboard", bg = "#fff8e1", border = "#f59e0b", fg = "#b45309"),
      "分析" = list(icon = "search", bg = "#f3e8ff", border = "#8e44ad", fg = "#6b21a8"),
      "方案" = list(icon = "lightbulb", bg = "#e8f5e9", border = "#4caf50", fg = "#2e7d32"),
      "结果" = list(icon = "check-circle", bg = "#e3f2fd", border = "#2196f3", fg = "#1565c0"),
      "总结" = list(icon = "flag-checkered", bg = "#e0f7fa", border = "#00acc1", fg = "#006064"),
      list(icon = "circle", bg = "#fafafa", border = "#9e9e9e", fg = "#555")
    )
  }

  # 日志列表（按日期分组、双列卡片）
  output[[p("dl_list")]] <- renderUI({
    trigger(); local_trigger()
    kw <- trimws(search_term())
    df <- hf_records_full("devlog")   # id,rec_no,title,payload,status,created_at
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "text-align:center;color:#999;padding:40px;font-size:14px;",
        if (nzchar(kw)) sprintf("未找到「%s」的相关日志", kw) else "暂无开发日志"))
    }
    search_words <- if (nzchar(kw)) strsplit(kw, "\\s+")[[1]] else character(0)
    search_words <- search_words[search_words != ""]

    if (nzchar(kw)) {
      hit <- grepl(kw, df$title, ignore.case = TRUE) | grepl(kw, df$payload, ignore.case = TRUE)
      df <- df[hit, , drop = FALSE]
    }
    if (nrow(df) == 0) {
      return(tags$div(style = "text-align:center;color:#999;padding:40px;font-size:14px;",
        sprintf("未找到「%s」的相关日志", kw)))
    }

    df$date <- substr(df$created_at, 1, 10)
    dates <- sort(unique(df$date), decreasing = TRUE)

    items_list <- list()
    for (d in dates) {
      grp <- df[df$date == d, , drop = FALSE]
      cards <- lapply(seq_len(nrow(grp)), function(i) {
        r <- grp[i, , drop = FALSE]
        time_str <- if (!is.na(r$created_at)) substr(r$created_at, 12, 16) else ""
        rec_no <- hf_or(r$rec_no, "")
        title <- hf_or(r$title, "未命名")
        hl_title <- if (length(search_words) > 0) HTML(.hl(title, search_words)) else title

        secs <- hf_devlog_sections(hf_or(r$payload, ""))
        sec_blocks <- if (length(secs) > 0) {
          lapply(secs, function(s) {
            st <- hf_devlog_section_style(s$label)
            body <- hf_or(s$body, "")
            if (!nzchar(trimws(body))) return(NULL)
            hl_body <- if (length(search_words) > 0) HTML(.hl(body, search_words)) else body
            tags$div(style = sprintf("margin-top:6px;padding:8px 10px;background:%s;border-left:3px solid %s;border-radius:0 4px 4px 0;font-size:13px;", st$bg, st$border),
              tags$div(style = sprintf("font-size:11px;color:%s;font-weight:bold;margin-bottom:3px;", st$fg), icon(st$icon), " ", s$label),
              tags$div(style = "white-space:pre-wrap;line-height:1.5;color:#333;", hl_body))
          })
        } else NULL

        # 无分段时整段 pre-wrap 展示
        raw_block <- if (length(secs) == 0 && nzchar(trimws(hf_or(r$payload, "")))) {
          hl_raw <- if (length(search_words) > 0) HTML(.hl(hf_or(r$payload, ""), search_words)) else hf_or(r$payload, "")
          tags$div(style = "white-space:pre-wrap;line-height:1.6;font-size:13px;color:#444;", hl_raw)
        } else NULL

        tags$div(style = "background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:12px;height:100%;box-sizing:border-box;",
          tags$div(style = "display:flex;align-items:flex-start;gap:6px;margin-bottom:6px;flex-wrap:wrap;",
            if (nzchar(rec_no)) tags$span(style = "font-size:10px;color:#888;font-family:Consolas,monospace;white-space:nowrap;margin-top:3px;flex-shrink:0;", rec_no),
            tags$span(style = "display:inline-block;padding:1px 7px;border-radius:10px;font-size:11px;background:#ede9fe;color:#5b21b6;white-space:nowrap;flex-shrink:0;", "HungFo"),
            tags$div(style = "flex:1;min-width:0;",
              tags$div(style = "font-size:15px;font-weight:bold;color:#222;line-height:1.3;",
                hl_title,
                tags$span(style = "font-size:11px;color:#bbb;font-weight:normal;margin-left:6px;white-space:nowrap;", time_str)))
          ),
          sec_blocks,
          raw_block
        )
      })
      items_list <- c(items_list, list(
        tags$div(style = "font-size:14px;font-weight:bold;color:#fff;margin:16px 0 8px;grid-column:1/-1;background:linear-gradient(90deg,#1a237e,#1565c0);padding:6px 14px;border-radius:6px;",
          icon("calendar"), " ", d, tags$span(style = "font-weight:normal;opacity:0.8;margin-left:8px;", sprintf("· %d 条", nrow(grp)))),
        cards
      ))
    }
    tags$div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;align-items:start;", items_list)
  })
}

# ── Agent Workflow Monitor 面板 Tab ──
# 独立页面（新标签页打开 hungfo_agent_monitor.html），原打开按键放右上角；
# 页面下方展示 Monitor 的原始记录及来源（任务来源 / 会话追踪 / CodeBuddy 自动任务）。
# 放置在「权限体系」（12 环节最后一个）之后。

# 单个来源块：标签 + 来源（数据表 → 导出文件 → 前端变量）+ 原始记录表
hf_monitor_source_block <- function(p, id, label, table, jsfile, jsvar) {
  div(style = "border:1px solid #e8ecf1;border-radius:8px;padding:10px;margin-bottom:10px;",
    div(style = "display:flex;align-items:center;gap:8px;margin-bottom:6px;flex-wrap:wrap;",
      tags$b(label),
      tags$span(style = "font-size:11px;background:#ede9fe;color:#5b21b6;padding:2px 8px;border-radius:10px;",
        "来源：", table),
      tags$span(style = "font-size:11px;color:#888;font-family:Consolas,monospace;",
        paste0("→ ", jsfile, " → ", jsvar))
    ),
    DTOutput(p(id))
  )
}

hf_agent_monitor_tab <- function() {
  p <- function(x) paste0("hf_agent_monitor_", x)
  tabPanel(
    "Agent Workflow Monitor", value = "agent_monitor", icon = icon("network-wired"),
    div(style = "padding:4px 2px;",
      # 顶部：标题 + 说明（左），原打开按键（右上角）
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;display:flex;align-items:center;gap:12px;",
        div(style = "flex:1;min-width:0;",
          tags$span(style = "font-size:16px;font-weight:bold;", icon("network-wired"), " Agent Workflow Monitor"),
          tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-MONITOR"),
          tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
            "多智能体工作流可视化监控面板（深色科技风）。点击下方按钮在新标签页打开独立页面。")
        ),
        tags$a(
          id = "hf_agent_monitor_open",
          href = "hungfo/hungfo_agent_monitor.html",
          target = "_blank",
          class = "btn btn-primary",
          style = "color:#fff;flex-shrink:0;",
          icon("external-link-alt"), " 打开监控面板（新标签页）"
        )
      ),
      # 页面下方：Monitor 原始记录及来源（三类监控采集原始数据，各自标注来源）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:4px;", "✦ Monitor 原始记录及来源"),
        tags$div(style = "font-size:11px;color:#999;margin-bottom:10px;",
          "监控页展示数据均来自以下原始记录，每条记录标注来源（数据表 → 导出文件 → 前端变量）。"),
        hf_monitor_source_block(p, "raw_tasks",    "任务来源",       "hf_agent_tasks",   "hungfo_tasks.js",    "window.HF_TASKS"),
        hf_monitor_source_block(p, "raw_sessions", "会话追踪",       "hf_task_sessions", "hungfo_sessions.js", "window.HF_SESSIONS"),
        hf_monitor_source_block(p, "raw_usage",    "CodeBuddy 自动任务", "hf_cb_usage",   "hungfo_usage.js",    "window.HF_CB_USAGE"),
        hf_monitor_source_block(p, "raw_cb_sessions", "CodeBuddy 会话清单", "hf_cb_sessions", "hungfo_cb_sessions.js", "window.HF_CB_SESSIONS")
      )
    )
  )
}

# ── Agent Workflow Monitor 面板 Server（原始记录及来源） ──
hf_agent_monitor_page_server <- function(input, output, session, rv, trigger) {
  p <- function(x) paste0("hf_agent_monitor_", x)
  empty_dt <- function(txt) {
    DT::datatable(data.frame(提示 = txt, stringsAsFactors = FALSE),
      options = list(dom = "t"), rownames = FALSE)
  }

  # 来源一：任务来源（hf_agent_tasks → hungfo_tasks.js → window.HF_TASKS）
  output[[p("raw_tasks")]] <- DT::renderDT({
    trigger()
    df <- hf_task_list()
    if (is.null(df) || nrow(df) == 0) return(empty_dt("暂无任务原始记录"))
    out <- data.frame(
      ID = df$id,
      名称 = df$name,
      类型 = ifelse(df$task_type == "sample", "示例", "正式"),
      节点数 = vapply(df$nodes, function(x) length(tryCatch(jsonlite::fromJSON(x), error = function(e) character(0))), integer(1)),
      启用 = ifelse(df$enabled == 1, "是", "否"),
      创建时间 = df$created_at,
      stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 5, dom = "ltip"))
  })

  # 来源二：会话追踪（hf_task_sessions → hungfo_sessions.js → window.HF_SESSIONS）
  output[[p("raw_sessions")]] <- DT::renderDT({
    trigger()
    df <- hf_sessions_list(50)
    if (is.null(df) || nrow(df) == 0) return(empty_dt("暂无会话原始记录"))
    out <- data.frame(
      ID = df$id,
      任务ID = ifelse(is.na(df$task_id), "—", as.character(df$task_id)),
      标题 = ifelse(is.na(df$title) | df$title == "", "—", df$title),
      LLM = ifelse(is.na(df$llm_name) | df$llm_name == "", "—", df$llm_name),
      Token入 = ifelse(is.na(df$token_in), 0, df$token_in),
      Token出 = ifelse(is.na(df$token_out), 0, df$token_out),
      耗时ms = ifelse(is.na(df$duration_ms), 0, df$duration_ms),
      状态 = df$status,
      开始时间 = ifelse(is.na(df$started_at), "—", as.character(df$started_at)),
      stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 5, dom = "ltip"))
  })

  # 来源三：CodeBuddy 自动任务（hf_cb_usage → hungfo_usage.js → window.HF_CB_USAGE）
  output[[p("raw_usage")]] <- DT::renderDT({
    trigger()
    df <- hf_cb_usage_list(50)
    if (is.null(df) || nrow(df) == 0) return(empty_dt("暂无 CodeBuddy 自动任务运行统计"))
    out <- data.frame(
      ID = df$id,
      任务ID = ifelse(is.na(df$automation_id) | df$automation_id == "", "—", df$automation_id),
      名称 = ifelse(is.na(df$name) | df$name == "", "—", df$name),
      类型 = ifelse(is.na(df$schedule_type) | df$schedule_type == "", "—", df$schedule_type),
      状态 = ifelse(is.na(df$status) | df$status == "", "—", df$status),
      运行次数 = ifelse(is.na(df$runs_total), 0, df$runs_total),
      成功次数 = ifelse(is.na(df$runs_success), 0, df$runs_success),
      失败次数 = ifelse(is.na(df$runs_failed), 0, df$runs_failed),
      成功率 = ifelse(is.na(df$success_rate), "0%", paste0(df$success_rate, "%")),
      平均耗时ms = ifelse(is.na(df$avg_duration_ms), 0, df$avg_duration_ms),
      最近运行 = ifelse(is.na(df$last_run_at) | df$last_run_at == "", "—", df$last_run_at),
      stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 5, dom = "ltip"))
  })

  # 来源四：CodeBuddy 会话清单（hf_cb_sessions → hungfo_cb_sessions.js → window.HF_CB_SESSIONS）
  output[[p("raw_cb_sessions")]] <- DT::renderDT({
    trigger()
    df <- hf_cb_sessions_list(50)
    if (is.null(df) || nrow(df) == 0) return(empty_dt("暂无 CodeBuddy 会话清单"))
    out <- data.frame(
      ID = df$id,
      会话ID = ifelse(is.na(df$conversation_id) | df$conversation_id == "", "—", df$conversation_id),
      标题 = ifelse(is.na(df$title) | df$title == "", "—", df$title),
      状态 = ifelse(is.na(df$status) | df$status == "", "—", df$status),
      Token总计 = ifelse(is.na(df$token_total), 0, df$token_total),
      输入 = ifelse(is.na(df$token_input), 0, df$token_input),
      输出 = ifelse(is.na(df$token_output), 0, df$token_output),
      缓存命中 = ifelse(is.na(df$token_cache_hit), 0, df$token_cache_hit),
      思考 = ifelse(is.na(df$token_thinking), 0, df$token_thinking),
      命中率 = ifelse(is.na(df$token_cache_hit_rate), "—", paste0(df$token_cache_hit_rate, "%")),
      更新时间 = ifelse(is.na(df$updated_at) | df$updated_at == "", "—", df$updated_at),
      stringsAsFactors = FALSE)
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 5, dom = "ltip"))
  })
}

# ── HungFo 主 UI（总览 + 各环节 Tab）──
hungfo_ui <- function() {
  mods <- hf_modules()
  tagList(
    fluidRow(
      column(12,
        div(style = "background:linear-gradient(135deg,#0d47a1,#1976d2);color:#fff;border-radius:10px;padding:16px 20px;margin-bottom:12px;",
          h3(style = "margin:0;color:#fff;", icon("cubes"), " HungFo 子项目"),
          p(style = "margin:6px 0 0;opacity:.9;font-size:13px;",
            "独立子项目（R + Shiny + SQLite）· 12 个环节各自独立、互相提供接口 · 骨架版 v0.1")
        )
      )
    ),
    do.call(tabsetPanel, c(
      list(id = "hf_main_tabs", type = "pills"),
      list(tabPanel("总览", icon = icon("home"), br(), uiOutput("hf_overview"))),
      unname(lapply(mods, hf_module_page_ui)),
      list(hf_agent_monitor_tab())
    ))
  )
}

# ── HungFo 独立界面（独立 navbarPage，不挂 ITOM 主导航栏，含「回 ITOM」导航）──
# 进入方式：server.R 检测 rv$app_view == "hungfo" 时渲染本函数
# show_back = FALSE 时去掉「回 ITOM」页签，用于完全独立运行（脱离父项目后无 ITOM 可回）。
hungfo_standalone_ui <- function(show_back = TRUE) {
  mods <- hf_modules()
  tabs <- c(
    # 总览页（默认选中）
    list(
      tabPanel("总览", value = "overview", icon = icon("home"),
        br(), uiOutput("hf_overview")
      )
    ),
    # 12 个环节页
    unname(lapply(mods, hf_module_page_ui)),
    # Agent 监控面板
    list(hf_agent_monitor_tab())
  )
  if (isTRUE(show_back)) {
    # 回 ITOM 导航（放在最右，作为「返回链接」）
    tabs <- c(tabs, list(
      tabPanel("回 ITOM", value = "back", icon = icon("arrow-left"),
        div(style = "padding:60px;text-align:center;color:#888;",
          icon("spinner", class = "fa-spin"), " 正在返回 ITOM ...")
      )
    ))
  }
  do.call(navbarPage, c(
    list(
      id = "hf_navbar",
      title = "HungFo",
      theme = shinytheme("cosmo"),
      collapsible = TRUE,
      # 顶部说明条（保留与嵌入式版本一致的品牌感）
      header = div(
        style = "background:linear-gradient(135deg,#0d47a1,#1976d2);color:#fff;border-radius:8px;padding:12px 18px;margin:8px 12px 4px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon("cubes"), " HungFo 子项目"),
        tags$span(style = "font-size:12px;opacity:.85;margin-left:10px;",
          "独立子项目（R + Shiny + SQLite）· 12 个环节各自独立、互相提供接口")
      )
    ),
    tabs
  ))
}

# ── HungFo 主 Server ──
hungfo_server <- function(input, output, session, rv) {
  hf_trigger <- reactiveVal(0)

  # 返回 ITOM：独立导航栏点击「回 ITOM」时切回主应用
  observeEvent(input$hf_navbar, {
    if (identical(input$hf_navbar, "back")) {
      rv$app_view <- "itom"
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # 初始化：建表 + 同步注册表 + 播种示例任务 + 导出任务 JS + 导出元任务镜像（登录后执行一次）
  observe({
    req(rv$logged_in)
    hf_db_init()
    hf_sync_registry()
    hf_task_seed_sample()       # 首次播种示例任务
    hf_write_tasks_js()         # 导出任务到监控页前端
    hf_metatask_export()        # 导出元任务队列镜像 MetaTasks.json（供 CodeBuddy 定时任务读取）
    hf_trigger(isolate(hf_trigger()) + 1)  # isolate 防止 observer 订阅自身写入的 hf_trigger 造成响应式死循环
  })

  output$hf_overview <- renderUI({
    hf_trigger()
    mods <- hf_modules()
    rows <- lapply(mods, function(m) {
      n <- hf_records_count(m$key)
      div(style = "border:1px solid #e0e0e0;border-radius:8px;padding:10px 12px;margin-bottom:8px;background:#fff;",
        div(style = "display:flex;align-items:center;gap:8px;",
          icon(m$icon), tags$b(m$name),
          tags$span(style = "font-size:11px;color:#888;", paste0("HF-", toupper(m$key))),
          tags$span(style = "margin-left:auto;font-size:11px;background:#e8f5e9;color:#2e7d32;padding:2px 8px;border-radius:10px;",
            paste0("记录 ", n))
        ),
        div(style = "font-size:12px;color:#666;margin-top:4px;", m$desc),
        div(style = "font-size:11px;color:#90a4ae;margin-top:2px;", icon("plug"), " ", m$api_desc)
      )
    })
    tagList(
      div(style = "font-size:12px;color:#555;margin-bottom:8px;",
        paste0("共 ", length(mods), " 个环节；各环节彼此独立，通过 hf_call(key, fn, ...) 互相提供接口。")),
      rows
    )
  })

  for (m in hf_modules()) {
    hf_module_page_server(input, output, session, rv, m, hf_trigger)
  }

  # Agent Workflow Monitor 面板（原始记录及来源）
  hf_agent_monitor_page_server(input, output, session, rv, hf_trigger)
}
