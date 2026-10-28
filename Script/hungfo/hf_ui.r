##################
# HungFo 子项目 — 聚合 UI + Server
# 由 hf_core.r 的注册表自动生成：总览页 + 每个环节一页（Tab）
# 新增环节只需：新增 hf_mod_*.r + 在 server.R 加一行 source()
##################

# 注册 HungFo 静态资源路径（www 下文件通过 /hungfo/ 前缀访问，自包含、不依赖主应用 www）
addResourcePath("hungfo", "Script/hungfo/www")

# ── 单个环节页面 UI ──
hf_module_page_ui <- function(m) {
  key <- m$key
  p <- function(x) paste0("hf_", key, "_", x)
  # 自动任务环节：特殊页面（Agent 任务来源编辑器）
  if (identical(key, "autotask")) return(hf_autotask_page_ui(m))
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

# ── 单个环节 Server ──
hf_module_page_server <- function(input, output, session, rv, m, trigger) {
  key <- m$key
  p <- function(x) paste0("hf_", key, "_", x)
  # 自动任务环节：专属 Server（Agent 任务来源编辑）
  if (identical(key, "autotask")) return(hf_autotask_page_server(input, output, session, rv, m, trigger))
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

# ── Agent Workflow Monitor 面板 Tab ──
# 独立页面（新标签页打开 hungfo_agent_monitor.html），不需要返回；
# 放置在「权限体系」（12 环节最后一个）之后。
hf_agent_monitor_tab <- function() {
  tabPanel(
    "Agent Workflow Monitor", value = "agent_monitor", icon = icon("network-wired"),
    div(style = "padding:48px 24px;text-align:center;",
      div(style = "font-size:15px;font-weight:bold;color:#455a64;margin-bottom:8px;",
        icon("network-wired"), " Agent Workflow Monitor"),
      p(style = "color:#888;font-size:13px;",
        "多智能体工作流可视化监控面板（深色科技风）。点击下方按钮在新标签页打开独立页面。"),
      tags$a(
        id = "hf_agent_monitor_open",
        href = "hungfo/hungfo_agent_monitor.html",
        target = "_blank",
        class = "btn btn-primary",
        style = "color:#fff;",
        icon("external-link-alt"), " 打开监控面板（新标签页）"
      )
    )
  )
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
hungfo_standalone_ui <- function() {
  mods <- hf_modules()
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
    # 总览页（默认选中）
    list(
      tabPanel("总览", value = "overview", icon = icon("home"),
        br(), uiOutput("hf_overview")
      )
    ),
    # 12 个环节页
    unname(lapply(mods, hf_module_page_ui)),
    # Agent 监控面板
    list(hf_agent_monitor_tab()),
    # 回 ITOM 导航（放在最右，作为「返回链接」）
    list(
      tabPanel("回 ITOM", value = "back", icon = icon("arrow-left"),
        div(style = "padding:60px;text-align:center;color:#888;",
          icon("spinner", class = "fa-spin"), " 正在返回 ITOM ...")
      )
    )
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

  # 初始化：建表 + 同步注册表 + 播种示例任务 + 导出任务 JS（登录后执行一次）
  observe({
    req(rv$logged_in)
    hf_db_init()
    hf_sync_registry()
    hf_task_seed_sample()       # 首次播种示例任务
    hf_write_tasks_js()         # 导出任务到监控页前端
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
}
