##################
# HungFo 子项目 — 聚合 UI + Server
# 由 hf_core.r 的注册表自动生成：总览页 + 每个环节一页（Tab）
# 新增环节只需：新增 hf_mod_*.r + 在 server.R 加一行 source()
##################

# ── 单个环节页面 UI ──
hf_module_page_ui <- function(m) {
  key <- m$key
  p <- function(x) paste0("hf_", key, "_", x)
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

# ── 单个环节 Server ──
hf_module_page_server <- function(input, output, session, rv, m, trigger) {
  key <- m$key
  p <- function(x) paste0("hf_", key, "_", x)
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
      unname(lapply(mods, hf_module_page_ui))
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

  # 初始化：建表 + 同步注册表（登录后执行一次）
  observe({
    req(rv$logged_in)
    hf_db_init()
    hf_sync_registry()
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
