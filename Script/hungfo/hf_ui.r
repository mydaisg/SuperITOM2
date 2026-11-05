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
  # 规则环节：特殊页面（分类 + 查看/编辑，参照父项目合规模块）
  if (identical(key, "rules")) return(hf_rules_page_ui(m))
  # 自动架构环节：特殊页面（数据驱动，随代码演进自动更新）
  if (identical(key, "arch")) return(hf_arch_page_ui(m))
  # 项目环节：特殊页面（集合需求/记事/总结/项目模板，内部 Tab 快速切换）
  if (identical(key, "project")) return(hf_project_page_ui(m))
  # 权限体系环节：特殊页面（用户 / 安全组 / 角色 / 权限管理）
  if (identical(key, "permission")) return(hf_permission_page_ui(m))
  # 配置环节：特殊页面（导航栏菜单配置中心）
  if (identical(key, "config")) return(hf_config_page_ui(m))
  # 网页工作台环节：特殊页面（提交任务给 CodeBuddy CLI，实时回传过程/结果）
  if (identical(key, "workbench")) return(hf_workbench_page_ui(m))
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
          actionButton(p("refresh_btn"), "刷新", icon = icon("sync"), class = "btn-info btn-sm")
        )
      ),
      uiOutput(p("status")),
      DTOutput(p("table"))
    )
  )
}

# ── 权限写操作守卫：非管理员（admins 组）直接拒绝并提示 ──
# 各环节的新增 / 编辑 / 删除 / 保存等写操作在 req(rv$logged_in) 后调用本函数。
hf_perm_guard_admin <- function(rv) {
  if (isTRUE(rv$logged_in) && hf_perm_is_admin(rv$current_user)) return(TRUE)
  showNotification("当前账号无管理权限（需 admins 组），仅可查看", type = "error")
  FALSE
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

      # 任务队列（待开发 / 开发异常可编辑；编辑选中 / 刷新按钮置左侧）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$style("#hf_metatask_mt_table table.dataTable, #hf_metatask_mt_table table.dataTable td, #hf_metatask_mt_table table.dataTable th { font-size:12px; }"),
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;flex-wrap:wrap;",
          # 「编辑选中」「刷新」置于最左侧（去掉原 margin-left:auto 的右推容器）
          div(style = "display:flex;gap:6px;",
            actionButton(p("mt_edit"), "编辑选中", icon = icon("pen"), class = "btn-primary btn-xs"),
            actionButton(p("mt_refresh"), "刷新", icon = icon("sync"), class = "btn-info btn-xs")
          ),
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 需求队列"),
          tags$span(style = "margin-left:auto;font-size:11px;color:#999;",
            "双击行查看全部内容并编辑 · 最新在最上 · 每 20 秒自动刷新")
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

# ── 数据表格规范（统一交互契约）──────────────────────────────
# 排序：三态 asc → desc → none（DT 表头点击默认行为，无需额外配置）
# 分页：每页 20 / 50 / 100 及「全部」；page_size = -1/"all" 时隐藏分页器
# 虚拟化：> virtualize_threshold 行启用固定高度滚动（scrollY）
# 空态：区分「无数据」(no_data) 与「筛选无结果」(no_match)
# 颜色：不硬编码，空态提示走 CSS 类 .hf-dt-empty（www/hungfo.css）
hf_datatable <- function(df, page_size = 20, virtualize_threshold = 50, scroll_y = 420,
                         empty = c("no_data", "no_match"), ...) {
  empty <- match.arg(empty)
  if (is.null(df) || nrow(df) == 0) {
    msg <- if (identical(empty, "no_match")) "没有符合筛选条件的结果" else "暂无数据"
    return(DT::datatable(
      data.frame(setNames(list(msg), "提示"), check.names = FALSE, stringsAsFactors = FALSE),
      rownames = FALSE, class = "hf-dt-empty",
      options = list(dom = "t", ordering = FALSE, paging = FALSE, searching = FALSE)))
  }
  pl <- if (identical(page_size, "all") || identical(page_size, -1)) -1 else as.integer(page_size)
  opts <- list(
    pageLength = pl,
    lengthMenu = list(c(20, 50, 100, -1), c("20", "50", "100", "全部")),
    autoWidth = FALSE,
    order = list(),
    dom = if (pl == -1) "t" else "ltip",
    language = list(lengthMenu = "每页 _MENU_ 条", search = "搜索：",
                    info = "第 _START_-_END_ 条 / 共 _TOTAL_ 条",
                    paginate = list(previous = "上一页", `next` = "下一页"))
  )
  if (nrow(df) > virtualize_threshold) opts$scrollY <- scroll_y
  # 选择「全部」时隐藏分页器
  opts$drawCallback <- DT::JS(
    "function(settings){",
    "  var api=this.api();",
    "  if(api.page.len()===-1){$(api.table().container()).find('.dataTables_paginate').hide();}",
    "  else{$(api.table().container()).find('.dataTables_paginate').show();}",
    "}")
  DT::datatable(df, rownames = FALSE, options = opts, ...)
}

# ── 规则环节专属页面：分类 + 查看/编辑（参照父项目合规模块）──
hf_rules_page_ui <- function(m) {
  p <- function(x) paste0("hf_rules_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-RULES"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;", m$desc),
        tags$div(style = "font-size:11px;color:#b0bec5;margin-top:4px;", icon("plug"), " 接口：", m$api_desc)
      ),
      # 工具栏：分类筛选 + 搜索 + 操作按钮
      div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
        fluidRow(
          column(3, selectInput(p("filter_cat"), "分类",
            choices = c("全部分类" = "", hf_rules_categories()), width = "100%")),
          column(3, textInput(p("search"), NULL, width = "100%", placeholder = "搜索规则…")),
          column(6, div(style = "display:flex;gap:6px;justify-content:flex-end;margin-top:24px;",
            actionButton(p("add_btn"), "新增规则", icon = icon("plus"), class = "btn-success btn-sm"),
            actionButton(p("view_btn"), "查看", icon = icon("eye"), class = "btn-info btn-sm"),
            actionButton(p("edit_btn"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
            actionButton(p("del_btn"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
            actionButton(p("refresh_btn"), "刷新", icon = icon("sync"), class = "btn-default btn-sm")
          ))
        )
      ),
      # 规则分类管理（分类可独立新增 / 编辑，与规则关联）
      div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 规则分类管理"),
          div(style = "margin-left:auto;display:flex;gap:6px;",
            actionButton(p("cat_add_btn"), "新增分类", icon = icon("plus"), class = "btn-success btn-xs"),
            actionButton(p("cat_edit_btn"), "编辑分类", icon = icon("pen"), class = "btn-primary btn-xs"),
            actionButton(p("cat_refresh_btn"), "刷新", icon = icon("sync"), class = "btn-info btn-xs")
          )
        ),
        DTOutput(p("cat_table"))
      ),
      uiOutput(p("status")),
      DTOutput(p("table"))
    )
  )
}

# ── 规则环节专属 Server ──
hf_rules_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_rules_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)
  filter_cat <- reactiveVal(NULL)
  search_term <- reactiveVal("")
  editing_id <- reactiveVal(NULL)   # NULL=新增；非空=编辑该 id
  cat_trigger <- reactiveVal(0)
  cat_refresh <- function() cat_trigger(cat_trigger() + 1)
  cat_editing_id <- reactiveVal(NULL)   # NULL=新增分类；非空=编辑该分类 id

  observeEvent(input[[p("filter_cat")]], {
    v <- trimws(hf_or(input[[p("filter_cat")]], ""))
    filter_cat(if (nzchar(v)) v else NULL)
  })
  observeEvent(input[[p("search")]], search_term(trimws(hf_or(input[[p("search")]], ""))))
  observeEvent(input[[p("refresh_btn")]], local_refresh())

  # 分类下拉动态同步（分类新增/重命名后，筛选下拉自动更新）
  observe({
    trigger(); local_trigger(); cat_trigger()
    cats <- hf_rules_categories()
    cur <- isolate(filter_cat())
    if (!is.null(cur) && !(cur %in% cats)) cur <- ""
    updateSelectInput(session, p("filter_cat"),
      choices = c("全部分类" = "", cats),
      selected = if (is.null(cur)) "" else cur)
  })

  output[[p("status")]] <- renderUI({
    trigger(); local_trigger()
    n <- hf_rules_count()
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("规则："), tags$span(style = "color:#1565c0;", paste0(n, " 条")),
      " · ", tags$span(style = "color:#2e7d32;", hf_or(m$status, "scaffold")),
      " · 版本 ", hf_or(m$version, "v0.1"))
  })

  # 过滤后的规则数据
  rules_df <- reactive({
    trigger(); local_trigger()
    df <- hf_rules_list(filter_cat())
    if (is.null(df) || nrow(df) == 0) return(df)
    kw <- search_term()
    if (nzchar(kw)) {
      payload_safe <- ifelse(is.na(df$payload), "", df$payload)
      hit <- grepl(kw, df$title, ignore.case = TRUE) |
             grepl(kw, df$category, ignore.case = TRUE) |
             grepl(kw, payload_safe, ignore.case = TRUE)
      df <- df[hit, , drop = FALSE]
    }
    df
  })

  output[[p("table")]] <- DT::renderDT({
    df <- rules_df()
    # 空态区分：整表无数据 vs 筛选无结果
    empty <- if (hf_rules_count(filter_cat()) == 0) "no_data" else "no_match"
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = empty))
    out <- data.frame(
      ID = df$id,
      编号 = hf_or(df$rule_no, ""),
      名称 = hf_or(df$title, ""),
      分类 = ifelse(is.na(df$category) | df$category == "", "—", df$category),
      状态 = hf_or(df$status, "draft"),
      创建时间 = ifelse(is.na(df$created_at) | df$created_at == "", "—", df$created_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })

  selected_id <- reactive({
    idx <- input[[p("table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    df <- rules_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$id[idx[1]]
  })

  # ── 查看：只读弹窗 ──
  observeEvent(input[[p("view_btn")]], {
    id <- selected_id()
    if (is.null(id)) { showNotification("请先在列表选中一条规则", type = "warning"); return() }
    r <- hf_rules_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("规则不存在，请刷新后重试", type = "warning"); return() }
    showModal(modalDialog(
      title = paste0("查看规则 #", id),
      tags$div(
        tags$p(tags$b("编号："), hf_or(r$rule_no[1], "—")),
        tags$p(tags$b("名称："), hf_or(r$title[1], "—")),
        tags$p(tags$b("分类："), hf_or(r$category[1], "—")),
        tags$p(tags$b("状态："), hf_or(r$status[1], "draft")),
        tags$p(tags$b("创建时间："), hf_or(r$created_at[1], "—")),
        tags$hr(),
        tags$b("规则内容："),
        tags$div(style = "white-space:pre-wrap;line-height:1.6;color:#333;",
          hf_or(r$payload[1], "（无）"))
      ),
      footer = modalButton("关闭"), size = "m", easyClose = TRUE
    ))
  })

  # ── 新增 / 编辑：共用弹窗 ──
  observeEvent(input[[p("add_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    editing_id(NULL)
    showModal(modalDialog(
      title = "新增规则",
      textInput(p("edit_title"), "规则名称", width = "100%"),
      selectInput(p("edit_cat"), "分类", choices = hf_rules_categories(), width = "100%"),
      textAreaInput(p("edit_payload"), "规则内容", rows = 6, width = "100%"),
      footer = tagList(
        actionButton(p("save_btn"), "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input[[p("edit_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    id <- selected_id()
    if (is.null(id)) { showNotification("请先在列表选中一条规则", type = "warning"); return() }
    r <- hf_rules_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("规则不存在，请刷新后重试", type = "warning"); return() }
    editing_id(id)
    showModal(modalDialog(
      title = paste0("编辑规则 #", id),
      textInput(p("edit_title"), "规则名称", value = hf_or(r$title[1], ""), width = "100%"),
      selectInput(p("edit_cat"), "分类", choices = hf_rules_categories(),
                  selected = hf_or(r$category[1], hf_rules_categories()[1]), width = "100%"),
      textAreaInput(p("edit_payload"), "规则内容", value = hf_or(r$payload[1], ""), rows = 6, width = "100%"),
      footer = tagList(
        actionButton(p("save_btn"), "保存修改", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input[[p("save_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    title <- trimws(hf_or(input[[p("edit_title")]], ""))
    if (!nzchar(title)) { showNotification("请输入规则名称", type = "warning"); return() }
    id <- editing_id()
    if (is.null(id)) {
      r <- hf_rules_add(title, input[[p("edit_cat")]], hf_or(input[[p("edit_payload")]], ""),
                        created_by = hf_current_uid(rv))
    } else {
      r <- hf_rules_update(id, title, input[[p("edit_cat")]], hf_or(input[[p("edit_payload")]], ""))
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_id(NULL) }
    local_refresh()
  })

  # ── 删除 ──
  observeEvent(input[[p("del_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    id <- selected_id()
    if (is.null(id)) { showNotification("请先在列表选中一条规则", type = "warning"); return() }
    r <- hf_rules_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })

  # ── 规则分类管理（独立新增 / 编辑，与规则关联）──
  cat_selected_id <- reactive({
    idx <- input[[p("cat_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    df <- hf_rules_categories_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$id[idx[1]]
  })

  output[[p("cat_table")]] <- DT::renderDT({
    trigger(); local_trigger(); cat_trigger()
    df <- hf_rules_categories_df()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    out <- data.frame(
      ID = df$id,
      分类名称 = hf_or(df$name, ""),
      关联规则数 = ifelse(is.na(df$rule_count), 0, df$rule_count),
      排序 = ifelse(is.na(df$sort_order), 0, df$sort_order),
      创建时间 = ifelse(is.na(df$created_at) | df$created_at == "", "—", df$created_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })

  observeEvent(input[[p("cat_refresh_btn")]], cat_refresh())

  # 新增分类
  observeEvent(input[[p("cat_add_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    cat_editing_id(NULL)
    showModal(modalDialog(
      title = "新增规则分类",
      textInput(p("cat_name"), "分类名称", width = "100%", placeholder = "如：告警策略"),
      footer = tagList(
        actionButton(p("cat_save_btn"), "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "s", easyClose = TRUE
    ))
  })

  # 编辑（重命名）分类
  observeEvent(input[[p("cat_edit_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    id <- cat_selected_id()
    if (is.null(id)) { showNotification("请先在分类列表选中一条分类", type = "warning"); return() }
    df <- hf_rules_categories_df()
    row <- df[df$id == id, , drop = FALSE]
    if (nrow(row) == 0) { showNotification("分类不存在，请刷新后重试", type = "warning"); return() }
    cat_editing_id(id)
    showModal(modalDialog(
      title = paste0("编辑分类 #", id),
      textInput(p("cat_name"), "分类名称", value = hf_or(row$name[1], ""), width = "100%"),
      footer = tagList(
        actionButton(p("cat_save_btn"), "保存修改", class = "btn-primary"),
        modalButton("取消")
      ), size = "s", easyClose = TRUE
    ))
  })

  # 保存分类（新增 / 重命名）
  observeEvent(input[[p("cat_save_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    name <- trimws(hf_or(input[[p("cat_name")]], ""))
    if (!nzchar(name)) { showNotification("请输入分类名称", type = "warning"); return() }
    id <- cat_editing_id()
    if (is.null(id)) {
      r <- hf_rules_category_add(name)
    } else {
      r <- hf_rules_category_update(id, name)
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); cat_editing_id(NULL) }
    cat_refresh()
    local_refresh()
  })
}

# ── 自动架构环节专属页面（数据驱动，随代码演进自动更新）──
# 参照父项目 system_architecture.r 的「色块 + 箭头 + 表格」纯 HTML 风格，
# 但节点 / 接口 / 表均来自 hf_arch_scan() 实时采集，刷新即自动反映架构变动。
hf_arch_page_ui <- function(m) {
  p <- function(x) paste0("hf_arch_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-ARCH"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
          "架构图由运行时状态自动生成：环节注册表、模块文件、数据表、对外接口。任何开发涉及架构变动，刷新即自动更新，无需手工维护。")
      ),
      # 工具栏：重建 + 刷新
      div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
        div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
          actionButton(p("rebuild"), "重建架构", icon = icon("sync"), class = "btn-primary btn-sm"),
          actionButton(p("refresh"), "刷新", icon = icon("rotate"), class = "btn-info btn-sm"),
          tags$span(style = "font-size:11px;color:#888;margin-left:auto;", "扫描时间："),
          tags$span(style = "font-size:11px;color:#1565c0;font-family:Consolas,monospace;",
            textOutput(p("scanned_at"), inline = TRUE))
        )
      ),
      uiOutput(p("body"))
    )
  )
}

# ── 自动架构环节专属 Server ──
hf_arch_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_arch_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)

  # 当前架构快照（触发重扫）
  snapshot <- reactive({
    trigger(); local_trigger()
    hf_arch_scan()
  })

  observeEvent(input[[p("rebuild")]], local_refresh())
  observeEvent(input[[p("refresh")]], local_refresh())

  output[[p("scanned_at")]] <- renderText({
    snap <- snapshot()
    hf_or(snap$scanned_at, "—")
  })

  output[[p("body")]] <- renderUI({
    snap <- snapshot()
    if (!is.null(snap$error)) {
      return(tags$div(style = "color:#d9534f;padding:20px;", "架构扫描失败：", snap$error))
    }
    mods <- snap$modules
    n <- length(mods)
    cols <- HF_ARCH_COLORS

    # 色块 / 箭头样式（与父项目 system_architecture.r 一致的纯 HTML 风格）
    style_css <- tags$style(HTML("
      .hf-arch-block { border-radius:8px; padding:12px 16px; background:#fff; border:1px solid #e0e0e0; margin-bottom:12px; }
      .hf-arch-node { display:inline-block; padding:5px 12px; border-radius:6px; color:#fff; font-size:12px; margin:3px; white-space:nowrap; }
      .hf-arch-arrow { display:inline-block; margin:0 4px; font-size:16px; color:#999; }
      .hf-arch-table { width:100%; border-collapse:collapse; font-size:12px; }
      .hf-arch-table th { background:#f5f5f5; border:1px solid #e0e0e0; padding:6px 8px; text-align:left; color:#555; }
      .hf-arch-table td { border:1px solid #e0e0e0; padding:6px 8px; color:#333; }
      .hf-arch-title { font-size:14px; font-weight:bold; color:#333; margin:4px 0 8px; }
      .hf-arch-sub { font-size:12px; color:#888; margin-bottom:8px; }
    "))

    # 区块一：环节拓扑（色块 + 箭头）
    nodes_html <- lapply(seq_along(mods), function(i) {
      md <- mods[[i]]
      col <- cols[((i - 1) %% length(cols)) + 1]
      tagList(
        tags$span(class = "hf-arch-node", style = sprintf("background:%s;", col),
          icon(hf_or(md$icon, "circle")), " ", md$name,
          tags$span(style = "opacity:.8;font-size:10px;margin-left:4px;", hf_or(md$version, ""))),
        if (i < n) tags$span(class = "hf-arch-arrow", "→") else ""
      )
    })

    # 区块二：环节明细表（id / key / 状态 / 接口说明）
    detail_rows <- lapply(mods, function(md) {
      tags$tr(
        tags$td(tags$code(hf_or(md$id, ""))),
        tags$td(tags$code(hf_or(md$key, ""))),
        tags$td(tags$b(hf_or(md$name, ""))),
        tags$td(hf_or(md$status, "scaffold")),
        tags$td(hf_or(md$api_desc, ""))
      )
    })

    # 区块三：模块文件
    mod_files <- snap$mod_files
    core_files <- snap$core_files
    files_block <- tags$div(
      class = "hf-arch-block",
      tags$div(class = "hf-arch-title", icon("file-code"), " 模块文件（", length(mod_files), " 个环节模块）"),
      tags$div(class = "hf-arch-sub", "核心文件：", tags$code(paste(core_files, collapse = "、"))),
      if (length(mod_files) > 0) {
        tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;",
          lapply(mod_files, function(f) {
            tags$span(style = "font-family:Consolas,monospace;font-size:11px;background:#f5f5f5;border:1px solid #e0e0e0;border-radius:4px;padding:2px 6px;", f)
          }))
      } else tags$div(class = "hf-arch-sub", "未发现 hf_mod_*.r 模块文件")
    )

    # 区块四：数据库表
    tables <- snap$tables
    tables_block <- tags$div(
      class = "hf-arch-block",
      tags$div(class = "hf-arch-title", icon("database"), " 数据表（", length(tables), " 张）"),
      if (length(tables) > 0) {
        tags$div(style = "display:flex;flex-wrap:wrap;gap:4px;",
          lapply(tables, function(t) {
            tags$span(style = "font-family:Consolas,monospace;font-size:11px;background:#e3f2fd;border:1px solid #bbdefb;border-radius:4px;padding:2px 6px;", t)
          }))
      } else tags$div(class = "hf-arch-sub", "暂无数据表")
    )

    # 区块五：对外接口映射
    api_map <- snap$api_map
    api_block <- tags$div(
      class = "hf-arch-block",
      tags$div(class = "hf-arch-title", icon("plug"), " 对外接口（", length(api_map), " 个环节）"),
      if (length(api_map) > 0) {
        tags$table(class = "hf-arch-table",
          tags$tr(tags$th("环节 key"), tags$th("接口函数")),
          lapply(api_map, function(a) {
            tags$tr(
              tags$td(tags$code(hf_or(a$key, ""))),
              tags$td(paste(a$fns, collapse = ", "))
            )
          })
        )
      } else tags$div(class = "hf-arch-sub", "暂无注册接口")
    )

    tagList(
      style_css,
      tags$div(class = "hf-arch-block",
        tags$div(class = "hf-arch-title", icon("sitemap"), " 环节拓扑（", n, " 个环节）"),
        tags$div(style = "line-height:2;", nodes_html)
      ),
      tags$div(class = "hf-arch-block",
        tags$div(class = "hf-arch-title", icon("list"), " 环节明细"),
        tags$table(class = "hf-arch-table",
          tags$tr(tags$th("ID"), tags$th("key"), tags$th("名称"), tags$th("状态"), tags$th("接口说明")),
          detail_rows
        )
      ),
      files_block,
      tables_block,
      api_block,
      tags$div(style = "font-size:11px;color:#bbb;text-align:center;padding:8px;",
        "HungFo 自动架构 · 数据驱动 · 随代码演进自动更新")
    )
  })
}

# ── 项目环节专属页面：集合需求/记事/总结/项目模板，内部 Tab 快速切换 ──
# 项目状态 / 优先级 / 记事状态 颜色映射（供渲染与看板使用）
hf_project_status_badge <- function(status) {
  st <- hf_or(status, "planning")
  map <- list(
    planning  = list(label = "规划中", bg = "#f0ad4e", fg = "#fff"),
    active    = list(label = "进行中", bg = "#5bc0de", fg = "#fff"),
    completed = list(label = "已完成", bg = "#5cb85c", fg = "#fff"),
    suspended = list(label = "已暂停", bg = "#9e9e9e", fg = "#fff")
  )
  m <- map[[st]]
  if (is.null(m)) m <- list(label = st, bg = "#9e9e9e", fg = "#fff")
  sprintf('<span style="display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;background:%s;color:%s;white-space:nowrap;">%s</span>',
          m$bg, m$fg, m$label)
}

hf_project_prio_badge <- function(p) {
  pr <- hf_or(p, "中")
  bg <- switch(pr, "高" = "#d9534f", "中" = "#f0ad4e", "低" = "#5bc0de", "#999")
  sprintf('<span style="display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;background:%s;color:#fff;white-space:nowrap;">%s</span>', bg, pr)
}

hf_project_req_status_badge <- function(status) {
  st <- hf_or(status, "进行中")
  bg <- if (identical(st, "已完成")) "#5cb85c" else "#f0ad4e"
  sprintf('<span style="display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;background:%s;color:#fff;white-space:nowrap;">%s</span>', bg, st)
}

hf_project_note_status_label <- function(status) {
  switch(hf_or(status, "pending"), pending = "待处理", in_progress = "进行中", done = "已完成", status)
}

# 方案 HTML 净化：提取 <style> 为 CSS 并 scope 到 .hf-sol-detail，去 script/文档级标签，返回 list(styles, body)
# 参照父项目 sol_sanitize_html，做精简版，避免方案内样式污染全局。
hf_project_solution_sanitize <- function(raw) {
  if (is.null(raw) || nchar(raw) == 0) return(list(styles = "", body = ""))
  txt <- raw
  css <- ""
  m <- gregexpr("(?s)<style[^>]*>(.*?)</style>", txt, ignore.case = TRUE, perl = TRUE)
  if (m[[1]][1] > 0) {
    blocks <- regmatches(txt, m)[[1]]
    for (s in blocks) css <- paste0(css, gsub("</?style[^>]*>", "", s, ignore.case = TRUE), "\n")
    txt <- gsub("(?s)<style[^>]*>.*?</style>", "", txt, ignore.case = TRUE, perl = TRUE)
  }
  txt <- gsub("(?s)<script[^>]*>.*?</script>", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("(?s)<head>.*?</head>", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("<body[^>]*>|</body>|<html[^>]*>|</html>", "", txt, ignore.case = TRUE)
  txt <- gsub("<!DOCTYPE[^>]*>|<meta[^>]*>|<title>.*?</title>|<link[^>]*>", "", txt, ignore.case = TRUE)
  # scope CSS：每条规则选择器前加 .hf-sol-detail
  if (nzchar(trimws(css))) {
    rules <- strsplit(css, "}", fixed = FALSE)[[1]]
    scoped <- c()
    for (rule in rules) {
      rule <- trimws(rule)
      if (!nzchar(rule)) next
      bp <- regexpr("\\{", rule)
      if (bp < 0) { scoped <- c(scoped, rule); next }
      sel <- trimws(substr(rule, 1, bp - 1))
      body <- substr(rule, bp, nchar(rule))
      if (grepl("^@|^/\\*|^$", sel)) { scoped <- c(scoped, paste0(sel, " ", body, "}")); next }
      sels <- strsplit(sel, ",", fixed = FALSE)[[1]]
      sels <- sapply(sels, function(s) {
        s <- trimws(s)
        if (!nzchar(s) || grepl("^\\.hf-sol-detail", s)) return(s)
        paste(".hf-sol-detail", s)
      }, USE.NAMES = FALSE)
      scoped <- c(scoped, paste0(paste(sels, collapse = ", "), " ", body, "}"))
    }
    css <- paste(scoped, collapse = "\n")
  }
  list(styles = css, body = trimws(txt))
}

hf_project_page_ui <- function(m) {
  p <- function(x) paste0("hf_project_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      # 记事看板卡片操作按钮 JS（左移/右移/删除）
      tags$script(HTML("
        $(document).on('click', '.hf-pnote-btn', function(e){
          e.stopPropagation();
          var id = $(this).data('id');
          var act = $(this).data('act');
          Shiny.setInputValue('hf_project_note_act', {id: id, act: act}, {priority:'event'});
        });
      ")),
      # 顶部横幅
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-PROJECT"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
          "集合父项目「需求 / 方案 / 记事 / 总结 / 项目模板」为一体：选择项目后，各 Tab 数据联动、增删改自动刷新。"),
        tags$div(style = "font-size:11px;color:#b0bec5;margin-top:4px;", icon("plug"), " 接口：", m$api_desc)
      ),
      # 项目上下文选择器（联动需求/记事/总结三个 Tab）
      div(style = "background:#fff;border-radius:8px;padding:10px 12px;margin:8px 0;border:1px solid #e8ecf1;",
        div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;white-space:nowrap;", "✦ 当前项目"),
          uiOutput(p("ctx_selector"), inline = TRUE),
          tags$span(style = "font-size:11px;color:#999;", "切换项目后，下方需求 / 方案 / 记事 / 总结自动按项目过滤刷新。")
        )
      ),
      # 内部快速 Tab 切换（项目 / 需求 / 记事 / 总结）
      tabsetPanel(
        id = p("subtabs"), type = "pills",

        # ── Tab 1：项目（项目模板）──
        tabPanel("项目", icon = icon("folder"),
          br(),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;justify-content:flex-end;flex-wrap:wrap;",
              actionButton(p("proj_add"), "新增项目", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("proj_edit"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
              actionButton(p("proj_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("proj_refresh"), "刷新", icon = icon("sync"), class = "btn-default btn-sm")
            )
          ),
          DTOutput(p("proj_table"))
        ),

        # ── Tab 2：需求（需求模块）──
        tabPanel("需求", icon = icon("clipboard-list"),
          br(),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;justify-content:flex-end;flex-wrap:wrap;",
              actionButton(p("req_add"), "新增需求", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("req_edit"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
              actionButton(p("req_done"), "切换完成", icon = icon("check"), class = "btn-info btn-sm"),
              actionButton(p("req_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("req_refresh"), "刷新", icon = icon("sync"), class = "btn-default btn-sm")
            )
          ),
          DTOutput(p("req_table"))
        ),

        # ── Tab 3：方案（方案模块，参照父项目 solutions，HTML 文档）──
        tabPanel("方案", icon = icon("lightbulb"),
          br(),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;justify-content:flex-end;flex-wrap:wrap;",
              actionButton(p("sol_add"), "新增方案", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("sol_view"), "查看", icon = icon("eye"), class = "btn-primary btn-sm"),
              actionButton(p("sol_edit"), "编辑", icon = icon("pen"), class = "btn-info btn-sm"),
              actionButton(p("sol_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("sol_refresh"), "刷新", icon = icon("sync"), class = "btn-default btn-sm")
            )
          ),
          DTOutput(p("sol_table"))
        ),

        # ── Tab 4：记事（记事模块，Trello 看板三列）──
        tabPanel("记事", icon = icon("sticky-note"),
          br(),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;justify-content:flex-end;flex-wrap:wrap;",
              actionButton(p("note_add"), "新增记事", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("note_refresh"), "刷新", icon = icon("sync"), class = "btn-default btn-sm")
            )
          ),
          uiOutput(p("note_board"))
        ),

        # ── Tab 4：总结（总结模块，一键聚合）──
        tabPanel("总结", icon = icon("calendar-day"),
          br(),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;justify-content:flex-end;flex-wrap:wrap;",
              actionButton(p("sum_build"), "一键生成", icon = icon("magic"), class = "btn-warning btn-sm"),
              actionButton(p("sum_add"), "新增总结", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("sum_edit"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
              actionButton(p("sum_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("sum_refresh"), "刷新", icon = icon("sync"), class = "btn-default btn-sm")
            )
          ),
          DTOutput(p("sum_table"))
        )
      ),
      uiOutput(p("status"))
    )
  )
}

# ── 项目环节专属 Server ──
hf_project_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_project_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)
  project_ctx <- reactiveVal(NULL)      # 当前项目 id；NULL=全部
  editing_id <- reactiveVal(NULL)       # 弹窗编辑目标 id（NULL=新增）

  # 项目上下文选择器（依赖 local_trigger 动态刷新，selected 绑定 project_ctx 保持选中）
  output[[p("ctx_selector")]] <- renderUI({
    trigger(); local_trigger()
    projs <- hf_project_list()
    choices <- c("全部项目" = "")
    if (!is.null(projs) && nrow(projs) > 0) {
      choices <- c(choices, setNames(as.character(projs$id), projs$name))
    }
    selectInput(p("ctx"), NULL, choices = choices,
                selected = if (is.null(project_ctx())) "" else as.character(project_ctx()),
                width = "240px")
  })

  observeEvent(input[[p("ctx")]], {
    v <- trimws(hf_or(input[[p("ctx")]], ""))
    project_ctx(if (nzchar(v)) as.integer(v) else NULL)
  }, ignoreNULL = FALSE)

  # 顶部状态栏
  output[[p("status")]] <- renderUI({
    trigger(); local_trigger()
    np <- hf_project_count()
    reqs <- hf_project_req_list(project_ctx())
    notes <- hf_project_note_list(project_ctx())
    sums <- hf_project_summary_list(project_ctx())
    sols <- hf_project_solution_list(project_ctx())
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("项目："), tags$span(style = "color:#1565c0;", paste0(np, " 个")),
      " · ", tags$span(style = "color:#2e7d32;", paste0("需求 ", if (is.null(reqs)) 0 else nrow(reqs), " 条")),
      " · ", tags$span(style = "color:#f59e0b;", paste0("方案 ", if (is.null(sols)) 0 else nrow(sols), " 条")),
      " · ", tags$span(style = "color:#e65100;", paste0("记事 ", if (is.null(notes)) 0 else nrow(notes), " 条")),
      " · ", tags$span(style = "color:#6a1b9a;", paste0("总结 ", if (is.null(sums)) 0 else nrow(sums), " 条")),
      " · 版本 ", hf_or(m$version, "v0.2"))
  })

  # ── Tab 1：项目表 ──
  output[[p("proj_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_project_list()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    out <- data.frame(
      ID = df$id,
      编号 = hf_or(df$project_no, ""),
      名称 = hf_or(df$name, ""),
      优先级 = vapply(seq_len(nrow(df)), function(i) hf_project_prio_badge(df$priority[i]), character(1)),
      状态 = vapply(seq_len(nrow(df)), function(i) hf_project_status_badge(df$status[i]), character(1)),
      开始 = ifelse(is.na(df$start_date) | df$start_date == "", "—", df$start_date),
      结束 = ifelse(is.na(df$end_date) | df$end_date == "", "—", df$end_date),
      创建时间 = ifelse(is.na(df$created_at) | df$created_at == "", "—", df$created_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20, escape = FALSE)
  })

  # ── Tab 2：需求表 ──
  output[[p("req_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_project_req_list(project_ctx())
    if (is.null(df) || nrow(df) == 0) {
      empty <- if (is.null(hf_project_req_list()) || nrow(hf_project_req_list()) == 0) "no_data" else "no_match"
      return(hf_datatable(df, empty = empty))
    }
    out <- data.frame(
      ID = df$id,
      编号 = hf_or(df$req_no, ""),
      标题 = hf_or(df$title, ""),
      状态 = vapply(seq_len(nrow(df)), function(i) hf_project_req_status_badge(df$status[i]), character(1)),
      优先级 = vapply(seq_len(nrow(df)), function(i) hf_project_prio_badge(df$priority[i]), character(1)),
      负责人 = ifelse(is.na(df$owner) | df$owner == "", "—", df$owner),
      截止 = ifelse(is.na(df$due_date) | df$due_date == "", "—", df$due_date),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20, escape = FALSE)
  })

  # ── Tab 3：记事看板（三列）──
  output[[p("note_board")]] <- renderUI({
    trigger(); local_trigger()
    df <- hf_project_note_list(project_ctx())
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "text-align:center;color:#999;padding:30px;font-size:13px;",
        "暂无记事，点击上方 [新增记事] 创建"))
    }
    render_col <- function(st) {
      sub <- df[df$status == st, , drop = FALSE]
      if (nrow(sub) == 0) {
        return(tags$div(style = "color:#bbb;font-size:12px;padding:8px;text-align:center;", "（空）"))
      }
      cards <- lapply(seq_len(nrow(sub)), function(i) {
        r <- sub[i, , drop = FALSE]
        left_btn <- if (st == "in_progress" || st == "done")
          sprintf('<button class="btn btn-xs btn-default hf-pnote-btn" data-id="%s" data-act="left" title="左移">←</button>', r$id) else ""
        right_btn <- if (st == "pending" || st == "in_progress")
          sprintf('<button class="btn btn-xs btn-default hf-pnote-btn" data-id="%s" data-act="right" title="右移">→</button>', r$id) else ""
        del_btn <- sprintf('<button class="btn btn-xs btn-danger hf-pnote-btn" data-id="%s" data-act="del" title="删除">🗑</button>', r$id)
        cmt_btn <- sprintf('<button class="btn btn-xs btn-info hf-pnote-btn" data-id="%s" data-act="comments" title="查看/添加评论">💬</button>', r$id)
        tags$div(style = "background:#fff;border:1px solid #e8ecf1;border-radius:8px;padding:10px 12px;margin-bottom:8px;",
          tags$div(style = "font-size:13px;font-weight:700;color:#1a2236;margin-bottom:4px;", hf_or(r$title, "")),
          tags$div(style = "font-size:12px;color:#596780;white-space:pre-wrap;max-height:120px;overflow-y:auto;",
            hf_or(r$content, "")),
          tags$div(style = "font-size:10px;color:#a0aec0;margin-top:6px;",
            hf_or(r$note_no, ""), " · ", hf_project_prio_badge(r$priority)),
          tags$div(style = "margin-top:6px;display:flex;gap:4px;justify-content:flex-end;",
            HTML(paste0(left_btn, right_btn, cmt_btn, del_btn)))
        )
      })
      tagList(cards)
    }
    col_style <- function(bg) sprintf("background:%s;border-radius:10px;padding:10px;min-height:120px;", bg)
    tagList(
      tags$style("
        .hf-pnote-col h4 { margin:0 0 8px; font-size:13px; font-weight:700; padding:6px 10px; border-radius:6px; }
      "),
      fluidRow(
        column(4, div(class = "hf-pnote-col", style = col_style("#f8f4ff"),
          tags$h4(style = "background:#ede2ff;color:#6c3bbf;", icon("inbox"), " 待处理"),
          render_col("pending"))),
        column(4, div(class = "hf-pnote-col", style = col_style("#f0f7ff"),
          tags$h4(style = "background:#d6ebff;color:#2563eb;", icon("spinner"), " 进行中"),
          render_col("in_progress"))),
        column(4, div(class = "hf-pnote-col", style = col_style("#f0faf5"),
          tags$h4(style = "background:#c7f0d8;color:#0d7d3a;", icon("check-circle"), " 已完成"),
          render_col("done")))
      )
    )
  })

  # ── Tab 4：总结表 ──
  output[[p("sum_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_project_summary_list(project_ctx())
    if (is.null(df) || nrow(df) == 0) {
      empty <- if (is.null(hf_project_summary_list()) || nrow(hf_project_summary_list()) == 0) "no_data" else "no_match"
      return(hf_datatable(df, empty = empty))
    }
    out <- data.frame(
      ID = df$id,
      编号 = hf_or(df$summary_no, ""),
      标题 = hf_or(df$title, ""),
      内容 = vapply(seq_len(nrow(df)), function(i) {
        one <- gsub("\\s+", " ", hf_or(df$content[i], ""))
        if (nchar(one) > 50) paste0(substr(one, 1, 50), "…") else one
      }, character(1)),
      日期 = ifelse(is.na(df$summary_date) | df$summary_date == "", "—", df$summary_date),
      创建时间 = ifelse(is.na(df$created_at) | df$created_at == "", "—", df$created_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })

  # ── 选中行 id 辅助 ──
  selected_id_of <- function(table_id, data_fn) {
    idx <- input[[table_id]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    df <- data_fn()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$id[idx[1]]
  }

  # ═══════ 项目 Tab 操作 ═══════
  observeEvent(input[[p("proj_refresh")]], local_refresh())
  observeEvent(input[[p("proj_add")]], {
    req(rv$logged_in)
    editing_id(NULL)
    showModal(modalDialog(
      title = "新增项目",
      textInput(p("e_proj_name"), "项目名称", width = "100%"),
      selectInput(p("e_proj_prio"), "优先级", choices = c("高", "中", "低"), selected = "中", width = "100%"),
      selectInput(p("e_proj_status"), "状态",
        choices = c("规划中" = "planning", "进行中" = "active", "已完成" = "completed", "已暂停" = "suspended"),
        selected = "planning", width = "100%"),
      fluidRow(
        column(6, dateInput(p("e_proj_start"), "开始日期", value = Sys.Date(), language = "zh-CN")),
        column(6, dateInput(p("e_proj_end"), "结束日期", value = Sys.Date() + 30, language = "zh-CN"))
      ),
      textAreaInput(p("e_proj_desc"), "项目描述", rows = 3, width = "100%"),
      footer = tagList(actionButton(p("proj_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("proj_edit")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("proj_table_rows_selected"), hf_project_list)
    if (is.null(id)) { showNotification("请先在项目表选中一行", type = "warning"); return() }
    r <- hf_project_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("项目不存在，请刷新", type = "warning"); return() }
    r <- r[1, ]
    editing_id(id)
    showModal(modalDialog(
      title = paste0("编辑项目 #", id),
      textInput(p("e_proj_name"), "项目名称", value = hf_or(r$name, ""), width = "100%"),
      selectInput(p("e_proj_prio"), "优先级", choices = c("高", "中", "低"), selected = hf_or(r$priority, "中"), width = "100%"),
      selectInput(p("e_proj_status"), "状态",
        choices = c("规划中" = "planning", "进行中" = "active", "已完成" = "completed", "已暂停" = "suspended"),
        selected = hf_or(r$status, "planning"), width = "100%"),
      fluidRow(
        column(6, dateInput(p("e_proj_start"), "开始日期",
          value = if (is.na(r$start_date) || r$start_date == "") Sys.Date() else as.Date(r$start_date), language = "zh-CN")),
        column(6, dateInput(p("e_proj_end"), "结束日期",
          value = if (is.na(r$end_date) || r$end_date == "") Sys.Date() + 30 else as.Date(r$end_date), language = "zh-CN"))
      ),
      textAreaInput(p("e_proj_desc"), "项目描述", value = hf_or(r$description, ""), rows = 3, width = "100%"),
      footer = tagList(actionButton(p("proj_save"), "保存修改", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("proj_save")]], {
    req(rv$logged_in)
    name <- trimws(hf_or(input[[p("e_proj_name")]], ""))
    if (!nzchar(name)) { showNotification("请输入项目名称", type = "warning"); return() }
    id <- editing_id()
    if (is.null(id)) {
      r <- hf_project_add(name, input[[p("e_proj_desc")]], input[[p("e_proj_prio")]],
        input[[p("e_proj_status")]], as.character(input[[p("e_proj_start")]]),
        as.character(input[[p("e_proj_end")]]), created_by = hf_current_uid(rv))
    } else {
      r <- hf_project_update(id, name, input[[p("e_proj_desc")]], input[[p("e_proj_prio")]],
        input[[p("e_proj_status")]], as.character(input[[p("e_proj_start")]]),
        as.character(input[[p("e_proj_end")]]))
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_id(NULL) }
    local_refresh()
  })
  observeEvent(input[[p("proj_del")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("proj_table_rows_selected"), hf_project_list)
    if (is.null(id)) { showNotification("请先在项目表选中一行", type = "warning"); return() }
    r <- hf_project_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (!is.null(project_ctx()) && project_ctx() == id) project_ctx(NULL)
    local_refresh()
  })

  # ═══════ 需求 Tab 操作 ═══════
  observeEvent(input[[p("req_refresh")]], local_refresh())
  req_df <- reactive({ trigger(); local_trigger(); hf_project_req_list(project_ctx()) })
  observeEvent(input[[p("req_add")]], {
    req(rv$logged_in)
    editing_id(NULL)
    showModal(modalDialog(
      title = "新增需求",
      textInput(p("e_req_title"), "需求标题", width = "100%"),
      selectInput(p("e_req_status"), "状态", choices = c("进行中", "已完成"), selected = "进行中", width = "100%"),
      selectInput(p("e_req_prio"), "优先级", choices = c("高", "中", "低"), selected = "中", width = "100%"),
      fluidRow(
        column(6, textInput(p("e_req_owner"), "负责人", width = "100%")),
        column(6, dateInput(p("e_req_due"), "截止日期", value = Sys.Date() + 7, language = "zh-CN"))
      ),
      textAreaInput(p("e_req_desc"), "需求描述", rows = 3, width = "100%"),
      footer = tagList(actionButton(p("req_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("req_edit")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("req_table_rows_selected"), function() req_df())
    if (is.null(id)) { showNotification("请先在需求表选中一行", type = "warning"); return() }
    r <- hf_project_req_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("需求不存在，请刷新", type = "warning"); return() }
    r <- r[1, ]
    editing_id(id)
    showModal(modalDialog(
      title = paste0("编辑需求 #", id),
      textInput(p("e_req_title"), "需求标题", value = hf_or(r$title, ""), width = "100%"),
      selectInput(p("e_req_status"), "状态", choices = c("进行中", "已完成"), selected = hf_or(r$status, "进行中"), width = "100%"),
      selectInput(p("e_req_prio"), "优先级", choices = c("高", "中", "低"), selected = hf_or(r$priority, "中"), width = "100%"),
      fluidRow(
        column(6, textInput(p("e_req_owner"), "负责人", value = hf_or(r$owner, ""), width = "100%")),
        column(6, dateInput(p("e_req_due"), "截止日期",
          value = if (is.na(r$due_date) || r$due_date == "") Sys.Date() + 7 else as.Date(r$due_date), language = "zh-CN"))
      ),
      textAreaInput(p("e_req_desc"), "需求描述", value = hf_or(r$description, ""), rows = 3, width = "100%"),
      footer = tagList(actionButton(p("req_save"), "保存修改", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("req_save")]], {
    req(rv$logged_in)
    title <- trimws(hf_or(input[[p("e_req_title")]], ""))
    if (!nzchar(title)) { showNotification("请输入需求标题", type = "warning"); return() }
    id <- editing_id()
    if (is.null(id)) {
      r <- hf_project_req_add(project_ctx(), title, input[[p("e_req_desc")]], input[[p("e_req_status")]],
        input[[p("e_req_prio")]], input[[p("e_req_owner")]], as.character(input[[p("e_req_due")]]),
        created_by = hf_current_uid(rv))
    } else {
      r <- hf_project_req_update(id, title, input[[p("e_req_desc")]], input[[p("e_req_status")]],
        input[[p("e_req_prio")]], input[[p("e_req_owner")]], as.character(input[[p("e_req_due")]]))
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_id(NULL) }
    local_refresh()
  })
  observeEvent(input[[p("req_done")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("req_table_rows_selected"), function() req_df())
    if (is.null(id)) { showNotification("请先在需求表选中一行", type = "warning"); return() }
    r <- hf_project_req_toggle_done(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error", duration = 1.5)
    local_refresh()
  })
  observeEvent(input[[p("req_del")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("req_table_rows_selected"), function() req_df())
    if (is.null(id)) { showNotification("请先在需求表选中一行", type = "warning"); return() }
    r <- hf_project_req_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })

  # ── Tab 3：方案表 ──
  output[[p("sol_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_project_solution_list(project_ctx())
    if (is.null(df) || nrow(df) == 0) {
      empty <- if (is.null(hf_project_solution_list()) || nrow(hf_project_solution_list()) == 0) "no_data" else "no_match"
      return(hf_datatable(df, empty = empty))
    }
    out <- data.frame(
      ID = df$id,
      编号 = hf_or(df$sol_no, ""),
      标题 = hf_or(df$title, ""),
      分类 = hf_or(df$category, ""),
      关联项目 = ifelse(is.na(df$related_project) | df$related_project == "", "—", df$related_project),
      创建时间 = ifelse(is.na(df$created_at) | df$created_at == "", "—", df$created_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })

  # ═══════ 方案 Tab 操作 ═══════
  observeEvent(input[[p("sol_refresh")]], local_refresh())
  sol_df <- reactive({ trigger(); local_trigger(); hf_project_solution_list(project_ctx()) })
  observeEvent(input[[p("sol_add")]], {
    req(rv$logged_in)
    editing_id(NULL)
    showModal(modalDialog(
      title = "新增方案",
      textInput(p("e_sol_title"), "方案标题", width = "100%"),
      selectInput(p("e_sol_cat"), "分类",
        choices = c("技术方案", "企业方案", "产品", "工具", "子项目", "其它"),
        selected = "技术方案", width = "100%"),
      textInput(p("e_sol_rel"), "关联项目", width = "100%"),
      textAreaInput(p("e_sol_content"), "方案内容（HTML，可含 <style> 样式块）", rows = 14, width = "100%"),
      footer = tagList(actionButton(p("sol_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("sol_view")]], {
    id <- selected_id_of(p("sol_table_rows_selected"), function() sol_df())
    if (is.null(id)) { showNotification("请先在方案表选中一行", type = "warning"); return() }
    r <- hf_project_solution_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("方案不存在，请刷新", type = "warning"); return() }
    r <- r[1, ]
    san <- hf_project_solution_sanitize(hf_or(r$content, ""))
    showModal(modalDialog(
      title = hf_or(r$title, ""),
      div(style = "font-size:12px;color:#999;margin-bottom:8px;",
        tags$span(hf_or(r$sol_no, "")), " · ", tags$span(hf_or(r$category, "")),
        if (nzchar(hf_or(r$related_project, ""))) paste0(" · 关联：", r$related_project) else ""),
      tags$div(class = "hf-sol-detail", style = "max-height:62vh;overflow-y:auto;padding:4px;",
        if (nzchar(san$styles)) tags$style(HTML(san$styles)) else NULL,
        HTML(san$body)),
      footer = modalButton("关闭"),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("sol_edit")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("sol_table_rows_selected"), function() sol_df())
    if (is.null(id)) { showNotification("请先在方案表选中一行", type = "warning"); return() }
    r <- hf_project_solution_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("方案不存在，请刷新", type = "warning"); return() }
    r <- r[1, ]
    editing_id(id)
    showModal(modalDialog(
      title = paste0("编辑方案 #", id),
      textInput(p("e_sol_title"), "方案标题", value = hf_or(r$title, ""), width = "100%"),
      selectInput(p("e_sol_cat"), "分类",
        choices = c("技术方案", "企业方案", "产品", "工具", "子项目", "其它"),
        selected = hf_or(r$category, "技术方案"), width = "100%"),
      textInput(p("e_sol_rel"), "关联项目", value = hf_or(r$related_project, ""), width = "100%"),
      textAreaInput(p("e_sol_content"), "方案内容（HTML，可含 <style> 样式块）", value = hf_or(r$content, ""), rows = 14, width = "100%"),
      footer = tagList(actionButton(p("sol_save"), "保存修改", class = "btn-primary"), modalButton("取消")),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("sol_save")]], {
    req(rv$logged_in)
    title <- trimws(hf_or(input[[p("e_sol_title")]], ""))
    if (!nzchar(title)) { showNotification("请输入方案标题", type = "warning"); return() }
    id <- editing_id()
    if (is.null(id)) {
      r <- hf_project_solution_add(project_ctx(), title, input[[p("e_sol_content")]],
        input[[p("e_sol_cat")]], input[[p("e_sol_rel")]], created_by = hf_current_uid(rv))
    } else {
      r <- hf_project_solution_update(id, title, input[[p("e_sol_content")]],
        input[[p("e_sol_cat")]], input[[p("e_sol_rel")]])
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_id(NULL) }
    local_refresh()
  })
  observeEvent(input[[p("sol_del")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("sol_table_rows_selected"), function() sol_df())
    if (is.null(id)) { showNotification("请先在方案表选中一行", type = "warning"); return() }
    r <- hf_project_solution_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })

  # ═══════ 记事 Tab 操作（看板卡片 act: left/right/del）══
  observeEvent(input[[p("note_refresh")]], local_refresh())
  observeEvent(input[[p("note_add")]], {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "新增记事",
      textInput(p("e_note_title"), "记事标题", width = "100%"),
      selectInput(p("e_note_status"), "状态",
        choices = c("待处理" = "pending", "进行中" = "in_progress", "已完成" = "done"),
        selected = "pending", width = "100%"),
      selectInput(p("e_note_prio"), "优先级", choices = c("高", "中", "低"), selected = "中", width = "100%"),
      textAreaInput(p("e_note_content"), "记事内容", rows = 4, width = "100%"),
      footer = tagList(actionButton(p("note_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("note_save")]], {
    req(rv$logged_in)
    title <- trimws(hf_or(input[[p("e_note_title")]], ""))
    if (!nzchar(title)) { showNotification("请输入记事标题", type = "warning"); return() }
    r <- hf_project_note_add(project_ctx(), title, input[[p("e_note_content")]],
      input[[p("e_note_status")]], input[[p("e_note_prio")]], created_by = hf_current_uid(rv))
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) removeModal()
    local_refresh()
  })
  observeEvent(input[[p("note_act")]], {
    req(rv$logged_in)
    val <- input[[p("note_act")]]
    id <- val$id; act <- val$act
    if (is.null(id) || is.null(act)) return()
    if (identical(act, "comments")) {
      cmt_note_id(as.integer(id))
      showModal(modalDialog(
        title = paste0("记事评论 #", id),
        uiOutput(p("note_cmt_list")),
        textAreaInput(p("e_cmt_content"), "添加评论", rows = 2, width = "100%"),
        footer = tagList(
          actionButton(p("cmt_send"), "发送", class = "btn-primary"),
          modalButton("关闭")
        ),
        size = "l", easyClose = TRUE
      ))
      return()
    }
    if (identical(act, "del")) {
      r <- hf_project_note_delete(as.integer(id))
    } else {
      cur <- hf_project_note_get(as.integer(id))
      if (is.null(cur) || nrow(cur) == 0) { local_refresh(); return() }
      st <- hf_or(cur$status[1], "pending")
      next_st <- switch(st,
        pending = if (act == "right") "in_progress" else st,
        in_progress = if (act == "left") "pending" else if (act == "right") "done" else st,
        done = if (act == "left") "in_progress" else st,
        st)
      if (identical(next_st, st)) { local_refresh(); return() }
      r <- hf_project_note_move(as.integer(id), next_st)
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error", duration = 1.5)
    local_refresh()
  })

  # ═══════ 记事评论（查看/添加，parent_id 自关联按缩进显示层级）══
  cmt_note_id <- reactiveVal(NULL)

  output[[p("note_cmt_list")]] <- renderUI({
    req(cmt_note_id())
    df <- hf_project_note_comment_list(cmt_note_id())
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "color:#999;padding:16px;text-align:center;font-size:13px;", "暂无评论"))
    }
    ids <- as.character(df$id)
    pars <- as.character(df$parent_id)
    pmap <- setNames(pars, ids)
    depth_of <- function(cid, seen = 0L) {
      if (seen > 50L) return(0L)
      par <- pmap[[cid]]
      if (is.null(par) || length(par) == 0 || is.na(par) || identical(par, cid)) return(0L)
      1L + depth_of(par, seen + 1L)
    }
    items <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, , drop = FALSE]
      d <- depth_of(ids[i])
      author <- hf_or(r$created_by_name[1], "")
      if (!nzchar(author)) {
        author <- if (is.na(r$created_by[1]) || is.null(r$created_by[1])) "匿名" else paste0("用户#", r$created_by[1])
      }
      done_badge <- if (!is.na(r$status[1]) && identical(as.character(r$status[1]), "completed"))
        tags$span(style = "font-size:10px;color:#0d7d3a;margin-left:6px;", "✓ 已完成") else NULL
      reply_hint <- if (!is.na(r$parent_id[1]) && !is.null(r$parent_id[1]))
        tags$span(style = "font-size:10px;color:#8b5cf6;margin-left:6px;", "↳ 回复") else NULL
      tags$div(style = sprintf("margin-left:%dpx;padding:6px 10px;border-left:2px solid #e8ecf1;margin-bottom:6px;", min(d, 5) * 22),
        tags$div(style = "font-size:11px;color:#90a4ae;",
          author, " · ", hf_or(r$created_at[1], ""), done_badge, reply_hint),
        tags$div(style = "font-size:13px;color:#2c3e50;white-space:pre-wrap;word-break:break-word;", hf_or(r$content[1], ""))
      )
    })
    tagList(items)
  })

  observeEvent(input[[p("cmt_send")]], {
    req(rv$logged_in)
    content <- trimws(hf_or(input[[p("e_cmt_content")]], ""))
    if (!nzchar(content)) { showNotification("请输入评论内容", type = "warning"); return() }
    r <- hf_project_note_comment_add(cmt_note_id(), content, created_by = hf_current_uid(rv))
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) updateTextAreaInput(session, p("e_cmt_content"), value = "")
    local_refresh()
  })

  # ═══════ 总结 Tab 操作 ═══════
  observeEvent(input[[p("sum_refresh")]], local_refresh())
  sum_df <- reactive({ trigger(); local_trigger(); hf_project_summary_list(project_ctx()) })
  observeEvent(input[[p("sum_build")]], {
    req(rv$logged_in)
    pid <- project_ctx()
    if (is.null(pid)) { showNotification("请先在顶部选择项目，再一键生成总结", type = "warning"); return() }
    b <- hf_project_summary_build(pid)
    if (!isTRUE(b$success)) { showNotification(b$message, type = "warning"); return() }
    editing_id(NULL)
    showModal(modalDialog(
      title = "一键生成总结（已聚合当前项目）",
      textInput(p("e_sum_title"), "总结标题", value = b$title, width = "100%"),
      textAreaInput(p("e_sum_content"), "总结内容", value = b$content, rows = 12, width = "100%"),
      footer = tagList(actionButton(p("sum_save"), "保存总结", class = "btn-primary"), modalButton("取消")),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("sum_add")]], {
    req(rv$logged_in)
    editing_id(NULL)
    showModal(modalDialog(
      title = "新增总结",
      textInput(p("e_sum_title"), "总结标题", width = "100%"),
      textAreaInput(p("e_sum_content"), "总结内容（可用【需求】【记事】【结果】分段）", rows = 10, width = "100%"),
      footer = tagList(actionButton(p("sum_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("sum_edit")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("sum_table_rows_selected"), function() sum_df())
    if (is.null(id)) { showNotification("请先在总结表选中一行", type = "warning"); return() }
    r <- hf_project_summary_get(id)
    if (is.null(r) || nrow(r) == 0) { showNotification("总结不存在，请刷新", type = "warning"); return() }
    r <- r[1, ]
    editing_id(id)
    showModal(modalDialog(
      title = paste0("编辑总结 #", id),
      textInput(p("e_sum_title"), "总结标题", value = hf_or(r$title, ""), width = "100%"),
      textAreaInput(p("e_sum_content"), "总结内容", value = hf_or(r$content, ""), rows = 10, width = "100%"),
      footer = tagList(actionButton(p("sum_save"), "保存修改", class = "btn-primary"), modalButton("取消")),
      size = "l", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("sum_save")]], {
    req(rv$logged_in)
    title <- trimws(hf_or(input[[p("e_sum_title")]], ""))
    if (!nzchar(title)) { showNotification("请输入总结标题", type = "warning"); return() }
    id <- editing_id()
    if (is.null(id)) {
      r <- hf_project_summary_add(project_ctx(), title, input[[p("e_sum_content")]],
        created_by = hf_current_uid(rv))
    } else {
      r <- hf_project_summary_update(id, title, input[[p("e_sum_content")]])
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_id(NULL) }
    local_refresh()
  })
  observeEvent(input[[p("sum_del")]], {
    req(rv$logged_in)
    id <- selected_id_of(p("sum_table_rows_selected"), function() sum_df())
    if (is.null(id)) { showNotification("请先在总结表选中一行", type = "warning"); return() }
    r <- hf_project_summary_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })
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
  # 规则环节：专属 Server（分类 + 查看/编辑）
  if (identical(key, "rules")) return(hf_rules_page_server(input, output, session, rv, m, trigger))
  # 自动架构环节：专属 Server（数据驱动，自动扫描）
  if (identical(key, "arch")) return(hf_arch_page_server(input, output, session, rv, m, trigger))
  # 项目环节：专属 Server（需求/记事/总结/项目模板，Tab 切换 + 数据联动刷新）
  if (identical(key, "project")) return(hf_project_page_server(input, output, session, rv, m, trigger))
  # 权限体系环节：专属 Server（用户 / 安全组 / 角色 / 权限管理）
  if (identical(key, "permission")) return(hf_permission_page_server(input, output, session, rv, m, trigger))
  # 配置环节：专属 Server（导航栏菜单配置）
  if (identical(key, "config")) return(hf_config_page_server(input, output, session, rv, m, trigger))
  # 网页工作台环节：专属 Server（派发 CodeBuddy CLI + 实时日志轮询）
  if (identical(key, "workbench")) return(hf_workbench_page_server(input, output, session, rv, m, trigger))
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
    if (!hf_perm_guard_admin(rv)) return()
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
    if (!hf_perm_guard_admin(rv)) return()
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

# ── 元任务列表：编辑内容归一化 / 公共写库 / 全字段详情（表单与双击卡片共用）──
# 标题缺省时取 payload 首行；内容内部换行原样保留
hf_metatask_edit_values <- function(title, payload) {
  p0 <- as.character(hf_or(payload, "")); if (length(p0) == 0) p0 <- ""
  t0 <- as.character(hf_or(title, ""));   if (length(t0) == 0) t0 <- ""
  payload <- trimws(paste(p0, collapse = "\n"))
  title   <- trimws(paste(t0, collapse = "\n"))
  if (!nzchar(title)) {
    lines <- strsplit(payload, "\n", fixed = TRUE)[[1]]
    title <- if (length(lines) > 0) trimws(lines[1]) else ""
  }
  if (!nzchar(title)) title <- "未命名需求"
  if (!nzchar(trimws(payload))) return(list(ok = FALSE, message = "请输入需求说明"))
  list(ok = TRUE, title = title, payload = payload)
}

# 编辑写库唯一入口：「编辑选中」表单与「双击卡片」共用，避免两套保存逻辑
# 数据层 hf_metatask_update(id, title, payload) 内部已约束仅 pending / error / failed 可改
hf_metatask_save_edit <- function(id, title, payload) {
  if (is.null(id) || (length(id) == 1 && is.na(id))) {
    return(list(success = FALSE, message = "任务 id 无效"))
  }
  v <- hf_metatask_edit_values(title, payload)
  if (!isTRUE(v$ok)) return(list(success = FALSE, message = v$message))
  hf_metatask_update(id, v$title, v$payload)
}

# 双击卡片：把一行记录的全部字段拼成可换行阅读的纯文本（payload 换行原样保留）
hf_metatask_row_text <- function(row) {
  if (is.null(row) || nrow(row) == 0) return("（无记录）")
  labels <- list(
    id = "ID", title = "标题", payload = "需求内容 payload（全文）",
    cron = "cron 循环表达式（空=只跑一次）", status = "状态",
    last_run_at = "上次执行 last_run_at", next_run_at = "下次执行 next_run_at",
    result = "执行结果 result", created_by = "创建人 created_by",
    created_at = "创建时间 created_at", updated_at = "更新时间 updated_at",
    session_id = "执行会话 session_id", llm_name = "完成 LLM llm_name",
    agent = "执行 Agent", token_in = "token_in", token_out = "token_out",
    duration_ms = "耗时 duration_ms", started_at = "开始 started_at",
    finished_at = "结束 finished_at")
  parts <- character(0)
  for (nm in names(row)) {
    v <- row[[nm]][1]
    if (is.null(v) || length(v) == 0 || is.na(v)) v <- "—" else v <- as.character(v)
    if (!nzchar(trimws(v))) v <- "—"
    lab <- labels[[nm]]
    if (is.null(lab)) lab <- nm
    parts <- c(parts, sprintf("%s：%s", lab, v))
  }
  paste(parts, collapse = "\n")
}

# 双击卡片：全字段只读视图（tags$pre 文本节点由 Shiny 自动 HTML 转义，安全）
hf_metatask_detail_pre <- function(row) {
  tags$pre(
    style = paste0("white-space:pre-wrap;word-break:break-word;background:#f8f9fa;",
                   "border:1px solid #e8ecf1;border-radius:6px;padding:10px;",
                   "font-size:12px;line-height:1.6;color:#333;margin:0;",
                   "max-height:300px;overflow:auto;"),
    hf_metatask_row_text(row))
}

# ── 元任务环节专属 Server ──
hf_metatask_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_metatask_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)
  editing_id <- reactiveVal(NULL)    # NULL=新增模式；非空=编辑该 id 需求
  modal_task_id <- reactiveVal(NULL) # 双击卡片当前打开的任务 id（NULL=未打开，用于冻结自动刷新）

  # 队列数据（单一数据源）：倒序（最新在最上）
  # 表格 / 选中 / 编辑选中 / 双击卡片全部复用本 reactive，保证行序与行号严格一致
  mt_rows <- reactive({
    trigger(); local_trigger()
    q <- hf_metatask_list()
    if (!is.null(q) && nrow(q) > 0) {
      q <- q[order(-as.integer(q$id)), , drop = FALSE]   # id 自增 == 创建顺序，倒序即最新在最上
      rownames(q) <- NULL
    }
    q
  })

  # 20 秒自动刷新列表：invalidateLater → local_trigger → mt_rows → mt_table / status 重绘
  # 与手动「刷新」按钮共用 local_trigger（都是计数 +1），故二者不冲突，手动仍立即生效
  # 编辑卡片打开期间跳过刷新，避免表格重绘 / 选中态重置打断用户正在进行的编辑
  observe({
    invalidateLater(20000, session)
    if (is.null(modal_task_id())) local_refresh()
  })

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
    q <- mt_rows()
    n <- if (is.null(q)) 0 else nrow(q)
    pend <- if (is.null(q) || nrow(q) == 0) 0 else sum(q$status == "pending")
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("需求："), tags$span(style = "color:#1565c0;", paste0(n, " 条")),
      " · ", tags$span(style = "color:#e65100;", paste0("待开发 ", pend, " 条")),
      " · ", tags$span(style = "color:#888;", "执行完成自动写入开发日志 · 每 20 秒自动刷新"))
  })

  # 双击任务行 → 把行 id 回传给 Shiny（本页非 module，直接用固定 input id）
  # 取 d[0]（ID 列）而非行号，故不受分页 / 倒序影响；priority:'event' 保证连点同一行也触发
  dblclick_callback <- DT::JS(
    "table.on('dblclick', 'tbody tr', function() {",
    "  var d = table.row(this).data();",
    "  if (!d || d.length === 0) return;",
    "  var v = parseInt(d[0], 10);",
    "  if (isNaN(v)) return;",
    "  Shiny.setInputValue('hf_metatask_dblclick_id', v, {priority: 'event'});",
    "});")

  # 任务队列表（含 payload 内容、状态颜色块、完成 LLM / Agent）
  # ordering=FALSE + order=list() 固定服务端倒序行序，避免 DT 默认排序覆盖
  output[[p("mt_table")]] <- DT::renderDT({
    q <- mt_rows()
    if (is.null(q) || nrow(q) == 0) {
      return(DT::datatable(
        data.frame(提示 = "暂无需求，在上方提交一条需求说明", stringsAsFactors = FALSE),
        options = list(dom = "t", ordering = FALSE), rownames = FALSE))
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
      callback = dblclick_callback,
      options = list(pageLength = 15, dom = "ltip", ordering = FALSE, order = list()))
  })

  # 选中行 → 任务 id（与表格共用 mt_rows，倒序后行号与 id 严格对应）
  selected_id <- reactive({
    idx <- input[[p("mt_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    q <- mt_rows()
    if (is.null(q) || nrow(q) == 0) return(NULL)
    if (idx[1] < 1 || idx[1] > nrow(q)) return(NULL)
    q$id[idx[1]]
  })

  # 新增需求（单框：需求说明，首行自动作标题，cron 留空=只跑一次）
  observeEvent(input[[p("mt_add")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
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
    q <- mt_rows()
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
    if (!hf_perm_guard_admin(rv)) return()
    id <- editing_id()
    if (is.null(id)) return()
    r <- hf_metatask_save_edit(id, "", input[[p("mt_req")]])
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

  # 刷新：手动立即重读（与 20 秒自动刷新共用 local_trigger，互不冲突）
  observeEvent(input[[p("mt_refresh")]], local_refresh())

  # ── 双击任务行 → 弹出卡片：显示全部字段 + 卡片内可编辑 ──
  observeEvent(input[["hf_metatask_dblclick_id"]], {
    id <- suppressWarnings(as.integer(input[["hf_metatask_dblclick_id"]]))
    if (length(id) == 0 || is.na(id)) return()
    q <- mt_rows()
    row <- if (is.null(q) || nrow(q) == 0) NULL else q[q$id == id, , drop = FALSE]
    if (is.null(row) || nrow(row) == 0) {
      showNotification("需求不存在，请刷新后重试", type = "warning"); return()
    }
    st <- hf_or(row$status[1], "pending")
    editable <- st %in% c("pending", "error", "failed")
    modal_task_id(id)   # 标记卡片已打开 → 冻结 20 秒自动刷新

    footer <- if (editable) {
      tagList(
        tags$span(style = "margin-right:auto;font-size:12px;color:#888;",
          "保存后重置为「待开发」，等待 CodeBuddy 重新执行"),
        actionButton(p("dlg_save"), "保存修改", icon = icon("check"), class = "btn-success btn-sm"),
        actionButton(p("dlg_close"), "关闭", class = "btn-default btn-sm")
      )
    } else {
      tagList(
        tags$span(style = "margin-right:auto;font-size:12px;color:#d9534f;",
          "当前状态（执行中 / 已完成）不可编辑，仅「待开发 / 开发异常」可修改"),
        tags$button(type = "button", class = "btn btn-default btn-sm disabled",
          disabled = "disabled", "保存修改"),
        actionButton(p("dlg_close"), "关闭", class = "btn-default btn-sm")
      )
    }

    showModal(modalDialog(
      title = tagList("元任务详情 #", id, " ", HTML(hf_metatask_status_badge(st))),
      size = "l", easyClose = TRUE, footer = footer,
      # ESC / 点击遮罩关闭时也要解除自动刷新冻结
      tags$script(HTML(
        "$(document).off('hidden.bs.modal.hfmt').on('hidden.bs.modal.hfmt', '#shiny-modal', function(){",
        "  Shiny.setInputValue('hf_metatask_modal_closed', Date.now(), {priority:'event'});",
        "});")),
      tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:6px;",
        "全部字段（只读，payload 换行原样保留）"),
      hf_metatask_detail_pre(row),
      tags$hr(style = "margin:12px 0;"),
      tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:6px;",
        "编辑区（标题 / 需求内容）"),
      textInput(p("dlg_title"), "标题", value = hf_or(row$title[1], ""), width = "100%"),
      textAreaInput(p("dlg_payload"), "需求内容 payload（换行保留）",
        value = hf_or(row$payload[1], ""), rows = 10, width = "100%"),
      if (!editable) tags$div(style = "font-size:12px;color:#d9534f;margin-top:4px;",
        "当前状态不可编辑，保存按钮已禁用；可复制上方内容查看。") else NULL
    ))
  }, ignoreInit = TRUE)

  # 卡片「保存修改」：复用公共写库函数，成功后关闭弹窗并刷新列表
  observeEvent(input[[p("dlg_save")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    id <- modal_task_id()
    if (is.null(id)) return()
    r <- hf_metatask_save_edit(id, input[[p("dlg_title")]], input[[p("dlg_payload")]])
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) {
      modal_task_id(NULL)
      removeModal()
    }
    local_refresh()
  })

  # 关闭卡片 / ESC / 遮罩关闭：解除自动刷新冻结
  observeEvent(input[[p("dlg_close")]], {
    modal_task_id(NULL)
    removeModal()
  })
  observeEvent(input[["hf_metatask_modal_closed"]], {
    modal_task_id(NULL)
  }, ignoreInit = TRUE)
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
    # 按段标签（【xx】或 [xx]）边界切分：兼容「每个段独立一行」与「同行多标签」两种写法，
    # 从根本上避免日志内容挤在同一行时只识别出第一个【需求】段、其余内容全堆进 body 的错乱。
    m <- gregexpr("(\u3010[^\u3011]*\u3011|\\[[^\\]]*\\])", txt)[[1]]
    starts <- as.integer(m)
    lens <- attr(m, "match.length")
    if (starts[1] < 0) {
      return(list(list(label = "", body = trimws(txt))))
    }
    out <- list()
    for (i in seq_along(starts)) {
      s <- starts[i]; l <- lens[i]
      label <- trimws(substr(txt, s + 1, s + l - 2))   # 去掉首尾括号后的标签
      e <- s + l - 1
      next_s <- if (i < length(starts)) starts[i + 1] - 1 else nchar(txt)
      body <- if (next_s >= e + 1) trimws(substr(txt, e + 1, next_s)) else ""
      out[[length(out) + 1]] <- list(label = label, body = body)
    }
    # 首个标签之前的引导文本（若有）作为无标签首段保留
    if (starts[1] > 1) {
      head <- trimws(substr(txt, 1, starts[1] - 1))
      if (nzchar(head)) out <- c(list(list(label = "", body = head)), out)
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

hf_agent_monitor_tab <- function(name = "Agent Workflow Monitor", ic = "network-wired") {
  p <- function(x) paste0("hf_agent_monitor_", x)
  tabPanel(
    name, value = "agent_monitor", icon = icon(ic),
    div(style = "padding:4px 2px;",
      # 顶部：标题 + 说明（左），原打开按键（右上角）
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;display:flex;align-items:center;gap:12px;",
        div(style = "flex:1;min-width:0;",
          tags$span(style = "font-size:16px;font-weight:bold;", icon(ic), " ", name),
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

# ── 权限体系环节专属页面：用户 / 安全组 / 角色 / 权限管理 ──
hf_permission_page_ui <- function(m) {
  p <- function(x) paste0("hf_perm_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-PERMISSION"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "active")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;",
          "子项目自带的 RBAC 授权管理：用户 → 安全组 → 角色 → 权限。仅管理员（admins 组）可增删改。"),
        tags$div(style = "font-size:11px;color:#b0bec5;margin-top:4px;", icon("plug"), " 接口：", m$api_desc)
      ),
      do.call(tabsetPanel, c(
        list(id = p("tabs"), type = "pills"),
        # ── 用户管理 ──
        list(tabPanel("用户管理", value = "users", icon = icon("users"),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
              actionButton(p("user_add"), "新增用户", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("user_edit"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
              actionButton(p("user_toggle"), "启用/禁用", icon = icon("power-off"), class = "btn-warning btn-sm"),
              actionButton(p("user_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("user_refresh"), "刷新", icon = icon("sync"), class = "btn-info btn-sm")
            )
          ),
          uiOutput(p("user_status")),
          DTOutput(p("user_table"))
        )),
        # ── 安全组 ──
        list(tabPanel("安全组", value = "groups", icon = icon("users-cog"),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
              actionButton(p("group_add"), "新增组", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("group_edit"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
              actionButton(p("group_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("group_refresh"), "刷新", icon = icon("sync"), class = "btn-info btn-sm")
            )
          ),
          uiOutput(p("group_status")),
          DTOutput(p("group_table"))
        )),
        # ── 角色 ──
        list(tabPanel("角色", value = "roles", icon = icon("id-badge"),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
              actionButton(p("role_add"), "新增角色", icon = icon("plus"), class = "btn-success btn-sm"),
              actionButton(p("role_edit"), "编辑", icon = icon("pen"), class = "btn-primary btn-sm"),
              actionButton(p("role_del"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
              actionButton(p("role_refresh"), "刷新", icon = icon("sync"), class = "btn-info btn-sm")
            )
          ),
          uiOutput(p("role_status")),
          DTOutput(p("role_table"))
        )),
        # ── 权限明细 ──
        list(tabPanel("权限明细", value = "perms", icon = icon("key"),
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            tags$span(style = "font-size:12px;color:#666;", "各环节的查看 / 管理权限码，由角色授权后经「组 → 角色」链路作用于用户。")
          ),
          DTOutput(p("perm_table"))
        ))
      ))
    )
  )
}

# ── 权限体系环节专属 Server ──
hf_permission_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_perm_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)

  # 是否管理员（无管理员权限则只读展示，写操作弹提示）
  is_admin <- reactive({
    req(rv$logged_in)
    hf_perm_is_admin(rv$current_user)
  })

  # 写操作守卫：非管理员直接提示并中断
  guard_admin <- function() {
    if (!isTRUE(is_admin())) {
      showNotification("需要管理员（admins 组）权限才能执行此操作", type = "error")
      return(FALSE)
    }
    TRUE
  }

  # ── 用户 ──
  users_df <- reactive({ trigger(); local_trigger(); hf_perm_users_list() })
  output[[p("user_status")]] <- renderUI({
    n <- nrow(users_df())
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("用户："), tags$span(style = "color:#1565c0;", paste0(n, " 个")),
      " · 初始账号 DaiSG(admins) / GodHat(user)，密码均为 falwpa")
  })
  output[[p("user_table")]] <- DT::renderDT({
    df <- users_df()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    out <- data.frame(
      ID = df$id, 用户名 = df$username, 显示名 = ifelse(is.na(df$display_name) | df$display_name == "", "—", df$display_name),
      所属组 = ifelse(is.na(df$group_names) | df$group_names == "", "—", df$group_names),
      状态 = ifelse(df$active == 1, "启用", "禁用"),
      创建时间 = ifelse(is.na(df$created_at), "—", df$created_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })
  sel_user_id <- reactive({
    idx <- input[[p("user_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    users_df()$id[idx[1]]
  })

  observeEvent(input[[p("user_add")]], {
    if (!guard_admin()) return()
    groups <- hf_perm_groups_list()
    choices <- setNames(groups$id, groups$name)
    showModal(modalDialog(
      title = "新增用户",
      textInput(p("user_f_uname"), "用户名", width = "100%"),
      passwordInput(p("user_f_pw"), "密码", width = "100%"),
      textInput(p("user_f_dn"), "显示名", width = "100%"),
      selectInput(p("user_f_grp"), "所属组", choices = choices, multiple = TRUE, width = "100%"),
      footer = tagList(actionButton(p("user_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input[[p("user_edit")]], {
    if (!guard_admin()) return()
    id <- sel_user_id()
    if (is.null(id)) { showNotification("请先选中一个用户", type = "warning"); return() }
    u <- users_df()[users_df()$id == id, , drop = FALSE]
    if (nrow(u) == 0) return()
    groups <- hf_perm_groups_list()
    choices <- setNames(groups$id, groups$name)
    sel_grp <- if (is.na(u$group_ids[1]) || u$group_ids[1] == "") NULL else as.integer(strsplit(u$group_ids[1], ",")[[1]])
    showModal(modalDialog(
      title = paste0("编辑用户 #", id),
      textInput(p("user_f_uname"), "用户名", value = u$username[1], width = "100%"),
      passwordInput(p("user_f_pw"), "密码（留空不修改）", width = "100%"),
      textInput(p("user_f_dn"), "显示名", value = ifelse(is.na(u$display_name[1]), "", u$display_name[1]), width = "100%"),
      selectInput(p("user_f_grp"), "所属组", choices = choices, multiple = TRUE, selected = sel_grp, width = "100%"),
      footer = tagList(actionButton(p("user_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })

  editing_user_id <- reactiveVal(NULL)
  observeEvent(input[[p("user_add")]], editing_user_id(NULL))
  observeEvent(input[[p("user_edit")]], editing_user_id(sel_user_id()))

  observeEvent(input[[p("user_save")]], {
    if (!guard_admin()) return()
    uname <- trimws(hf_or(input[[p("user_f_uname")]], ""))
    pw <- input[[p("user_f_pw")]]
    dn <- hf_or(input[[p("user_f_dn")]], "")
    grp <- input[[p("user_f_grp")]]
    if (!nzchar(uname)) { showNotification("请输入用户名", type = "warning"); return() }
    id <- editing_user_id()
    if (is.null(id)) {
      r <- hf_perm_user_add(uname, if (is.null(pw)) "" else pw, dn, grp)
    } else {
      r <- hf_perm_user_update(id, uname, dn, password = if (is.null(pw) || !nzchar(pw)) NULL else pw, active = 1, group_ids = grp)
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_user_id(NULL) }
    local_refresh()
  })

  observeEvent(input[[p("user_toggle")]], {
    if (!guard_admin()) return()
    id <- sel_user_id()
    if (is.null(id)) { showNotification("请先选中一个用户", type = "warning"); return() }
    r <- hf_perm_user_toggle(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })

  observeEvent(input[[p("user_del")]], {
    if (!guard_admin()) return()
    id <- sel_user_id()
    if (is.null(id)) { showNotification("请先选中一个用户", type = "warning"); return() }
    r <- hf_perm_user_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })
  observeEvent(input[[p("user_refresh")]], local_refresh())

  # ── 安全组 ──
  groups_df <- reactive({ trigger(); local_trigger(); hf_perm_groups_list() })
  output[[p("group_status")]] <- renderUI({
    n <- nrow(groups_df())
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("安全组："), tags$span(style = "color:#1565c0;", paste0(n, " 个")),
      " · admins=管理员组 / user=普通用户组")
  })
  output[[p("group_table")]] <- DT::renderDT({
    df <- groups_df()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    out <- data.frame(
      ID = df$id, 组名 = df$name, 说明 = ifelse(is.na(df$description) | df$description == "", "—", df$description),
      角色 = ifelse(is.na(df$role_names) | df$role_names == "", "—", df$role_names),
      成员数 = df$member_count,
      状态 = ifelse(df$active == 1, "启用", "禁用"),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })
  sel_group_id <- reactive({
    idx <- input[[p("group_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    groups_df()$id[idx[1]]
  })

  observeEvent(input[[p("group_add")]], {
    if (!guard_admin()) return()
    roles <- hf_perm_roles_list()
    choices <- setNames(roles$id, roles$name)
    showModal(modalDialog(
      title = "新增安全组",
      textInput(p("group_f_name"), "组名", width = "100%"),
      textInput(p("group_f_desc"), "说明", width = "100%"),
      selectInput(p("group_f_role"), "角色", choices = choices, multiple = TRUE, width = "100%"),
      footer = tagList(actionButton(p("group_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("group_edit")]], {
    if (!guard_admin()) return()
    id <- sel_group_id()
    if (is.null(id)) { showNotification("请先选中一个安全组", type = "warning"); return() }
    g <- groups_df()[groups_df()$id == id, , drop = FALSE]
    if (nrow(g) == 0) return()
    roles <- hf_perm_roles_list()
    choices <- setNames(roles$id, roles$name)
    sel_role <- if (is.na(g$role_ids[1]) || g$role_ids[1] == "") NULL else as.integer(strsplit(g$role_ids[1], ",")[[1]])
    showModal(modalDialog(
      title = paste0("编辑安全组 #", id),
      textInput(p("group_f_name"), "组名", value = g$name[1], width = "100%"),
      textInput(p("group_f_desc"), "说明", value = ifelse(is.na(g$description[1]), "", g$description[1]), width = "100%"),
      selectInput(p("group_f_role"), "角色", choices = choices, multiple = TRUE, selected = sel_role, width = "100%"),
      footer = tagList(actionButton(p("group_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  editing_group_id <- reactiveVal(NULL)
  observeEvent(input[[p("group_add")]], editing_group_id(NULL))
  observeEvent(input[[p("group_edit")]], editing_group_id(sel_group_id()))

  observeEvent(input[[p("group_save")]], {
    if (!guard_admin()) return()
    name <- trimws(hf_or(input[[p("group_f_name")]], ""))
    if (!nzchar(name)) { showNotification("请输入组名", type = "warning"); return() }
    id <- editing_group_id()
    if (is.null(id)) {
      r <- hf_perm_group_add(name, hf_or(input[[p("group_f_desc")]], ""), input[[p("group_f_role")]])
    } else {
      r <- hf_perm_group_update(id, name, hf_or(input[[p("group_f_desc")]], ""), active = 1, role_ids = input[[p("group_f_role")]])
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_group_id(NULL) }
    local_refresh()
  })

  observeEvent(input[[p("group_del")]], {
    if (!guard_admin()) return()
    id <- sel_group_id()
    if (is.null(id)) { showNotification("请先选中一个安全组", type = "warning"); return() }
    r <- hf_perm_group_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })
  observeEvent(input[[p("group_refresh")]], local_refresh())

  # ── 角色 ──
  roles_df <- reactive({ trigger(); local_trigger(); hf_perm_roles_list() })
  output[[p("role_status")]] <- renderUI({
    n <- nrow(roles_df())
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("角色："), tags$span(style = "color:#1565c0;", paste0(n, " 个")),
      " · admin=全部权限 / user=只读权限")
  })
  output[[p("role_table")]] <- DT::renderDT({
    df <- roles_df()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    out <- data.frame(
      ID = df$id, 角色名 = df$name, 说明 = ifelse(is.na(df$description) | df$description == "", "—", df$description),
      权限数 = df$perm_count,
      权限码 = ifelse(is.na(df$perm_codes) | df$perm_codes == "", "—", df$perm_codes),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })
  sel_role_id <- reactive({
    idx <- input[[p("role_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    roles_df()$id[idx[1]]
  })

  observeEvent(input[[p("role_add")]], {
    if (!guard_admin()) return()
    perms <- hf_perm_permissions_list()
    choices <- setNames(perms$id, sprintf("%s（%s）", perms$name, perms$code))
    showModal(modalDialog(
      title = "新增角色",
      textInput(p("role_f_name"), "角色名", width = "100%"),
      textInput(p("role_f_desc"), "说明", width = "100%"),
      selectInput(p("role_f_perm"), "权限", choices = choices, multiple = TRUE, width = "100%"),
      footer = tagList(actionButton(p("role_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("role_edit")]], {
    if (!guard_admin()) return()
    id <- sel_role_id()
    if (is.null(id)) { showNotification("请先选中一个角色", type = "warning"); return() }
    r <- roles_df()[roles_df()$id == id, , drop = FALSE]
    if (nrow(r) == 0) return()
    perms <- hf_perm_permissions_list()
    choices <- setNames(perms$id, sprintf("%s（%s）", perms$name, perms$code))
    sel_perm <- if (is.na(r$perm_ids[1]) || r$perm_ids[1] == "") NULL else as.integer(strsplit(r$perm_ids[1], ",")[[1]])
    showModal(modalDialog(
      title = paste0("编辑角色 #", id),
      textInput(p("role_f_name"), "角色名", value = r$name[1], width = "100%"),
      textInput(p("role_f_desc"), "说明", value = ifelse(is.na(r$description[1]), "", r$description[1]), width = "100%"),
      selectInput(p("role_f_perm"), "权限", choices = choices, multiple = TRUE, selected = sel_perm, width = "100%"),
      footer = tagList(actionButton(p("role_save"), "保存", class = "btn-primary"), modalButton("取消")),
      size = "m", easyClose = TRUE
    ))
  })
  editing_role_id <- reactiveVal(NULL)
  observeEvent(input[[p("role_add")]], editing_role_id(NULL))
  observeEvent(input[[p("role_edit")]], editing_role_id(sel_role_id()))

  observeEvent(input[[p("role_save")]], {
    if (!guard_admin()) return()
    name <- trimws(hf_or(input[[p("role_f_name")]], ""))
    if (!nzchar(name)) { showNotification("请输入角色名", type = "warning"); return() }
    id <- editing_role_id()
    if (is.null(id)) {
      r <- hf_perm_role_add(name, hf_or(input[[p("role_f_desc")]], ""), input[[p("role_f_perm")]])
    } else {
      r <- hf_perm_role_update(id, name, hf_or(input[[p("role_f_desc")]], ""), input[[p("role_f_perm")]])
    }
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); editing_role_id(NULL) }
    local_refresh()
  })

  observeEvent(input[[p("role_del")]], {
    if (!guard_admin()) return()
    id <- sel_role_id()
    if (is.null(id)) { showNotification("请先选中一个角色", type = "warning"); return() }
    r <- hf_perm_role_delete(id)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    local_refresh()
  })
  observeEvent(input[[p("role_refresh")]], local_refresh())

  # ── 权限明细（只读） ──
  output[[p("perm_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_perm_permissions_list()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    out <- data.frame(
      ID = df$id, 环节 = df$module_key, 权限码 = df$code, 名称 = df$name,
      说明 = ifelse(is.na(df$description) | df$description == "", "—", df$description),
      stringsAsFactors = FALSE)
    hf_datatable(out, page_size = 20)
  })
}

# ── 导航栏由「配置」模块驱动（名称/图标/位置/显示隐藏可自定义，保存后刷新即生效）──

# 读取导航配置（默认 + 用户覆盖），按 position 排序，过滤隐藏项
hf_nav_entries <- function() {
  df <- hf_nav_config_list()
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- df[df$visible == 1, , drop = FALSE]
  df[order(as.integer(df$position), df$nav_key), , drop = FALSE]
}

# 单个导航项 → tabPanel（overview / 环节 / Agent 监控）
# 用自定义属性携带「名称全称」，前端脚本同步到导航 <a> 做悬停提示。
# 注意：绝不能覆盖 tabPanel 自带的 title 属性——它是导航标签文本，覆盖会破坏标签构建。
hf_nav_tab <- function(entry) {
  key  <- entry$nav_key[1]
  name <- hf_or(entry$name[1], key)
  ic   <- hf_or(entry$icon[1], "circle")
  full <- hf_or(entry$full_name[1], name)
  if (is.null(full) || !nzchar(trimws(as.character(full)))) full <- name
  tp <- if (identical(key, "overview")) {
    # 项目说明条只在「总览」模块保留，其它模块不再重复这行
    tabPanel(name, value = "overview", icon = icon(ic),
      div(style = "background:linear-gradient(135deg,#0d47a1,#1976d2);color:#fff;border-radius:8px;padding:10px 16px;margin:8px 0 10px;",
        tags$span(style = "font-size:15px;font-weight:bold;", icon("cubes"), " HungFo 子项目"),
        tags$span(style = "font-size:12px;opacity:.85;margin-left:10px;",
          "独立子项目（R + Shiny + SQLite）· 12 个环节各自独立、互相提供接口")),
      uiOutput("hf_overview"))
  } else if (identical(key, "agent_monitor")) {
    hf_agent_monitor_tab(name = name, ic = ic)
  } else {
    m <- hf_module(key)
    if (is.null(m)) return(NULL)
    m$name <- name   # 用配置的名称/图标覆盖模块默认值
    m$icon <- ic
    hf_module_page_ui(m)
  }
  # 名称全称走自定义 data 属性（不碰 tabPanel 的 title），前端据此设置 <a> 的 title
  htmltools::tagAppendAttributes(tp,
    `data-hf-nav-full` = as.character(full),
    `data-hf-nav-name` = as.character(name))
}

# 全部导航项 → tabPanel 列表（过滤未知 key 产生的 NULL）
hf_nav_tabs <- function() {
  entries <- hf_nav_entries()
  if (is.null(entries) || nrow(entries) == 0) return(list())
  tabs <- lapply(seq_len(nrow(entries)), function(i) hf_nav_tab(entries[i, , drop = FALSE]))
  Filter(Negate(is.null), tabs)
}

# ── HungFo 主 UI（总览 + 各环节 Tab，由导航配置驱动）──
hungfo_ui <- function() {
  tagList(
    # 引入数据表格规范样式（空态提示颜色统一在此，禁止在 R 里硬编码）
    tags$head(tags$link(rel = "stylesheet", href = "/hungfo/hungfo.css")),
    # 导航菜单悬停显示「名称全称」：把 data-hf-nav-full 同步到导航 <a> 标签
    tags$script(src = "/hungfo/hungfo_nav_tooltip.js"),
    # 项目说明条已下沉到「总览」Tab，此处不再在每个模块顶部重复
    do.call(tabsetPanel, c(
      list(id = "hf_main_tabs", type = "pills"),
      hf_nav_tabs()
    ))
  )
}

# ── HungFo 登录遮罩（独立运行时显示，登录前覆盖整页）──
hf_login_overlay <- function() {
  div(
    id = "hf_login_overlay",
    style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 10000;
             display: flex; justify-content: center; align-items: center;
             background: rgba(13,71,161,0.85);",
    div(
      style = "background: white; padding: 40px; border-radius: 10px; box-shadow: 0 10px 25px rgba(0,0,0,0.3); width: 400px;",
      h3("HungFo 登录", style = "text-align: center; color: #0d47a1; margin-bottom: 24px;"),
      textInput("hf_login_username", "用户名", placeholder = "请输入用户名"),
      passwordInput("hf_login_password", "密码", placeholder = "请输入密码"),
      br(),
      actionButton("hf_login_btn", "登录", class = "btn-primary btn-block", style = "width: 100%;"),
      br(),
      p(style = "text-align:center;color:#999;font-size:12px;margin-top:8px;",
        "初始账号：DaiSG（admins） / GodHat（user）")
    )
  )
}

# ── 导航栏构建（由配置驱动；每次调用都重新从 DB 读取配置，刷新即生效）──
# show_back = TRUE 时追加「回 ITOM」页签（父项目内嵌使用）；独立运行时为 FALSE。
hf_navbar_ui <- function(show_back = TRUE) {
  # 总览 + 各环节 + Agent 监控 + 配置，全部由导航配置驱动（含「回 ITOM」除外）
  tabs <- hf_nav_tabs()
  if (isTRUE(show_back)) {
    # 回 ITOM 导航（作为「返回链接」，不参与导航配置）
    tabs <- c(tabs, list(
      tabPanel("回 ITOM", value = "back", icon = icon("arrow-left"),
        div(style = "padding:60px;text-align:center;color:#888;",
          icon("spinner", class = "fa-spin"), " 正在返回 ITOM ...")
      )
    ))
  }
  # 右侧：仅退出登录图标按钮（当前用户已移至「配置」模块页面显示）
  tabs <- c(tabs, list(
    bslib::nav_spacer(),
    bslib::nav_item(
      actionButton("hf_logout_btn", label = NULL, icon = icon("sign-out-alt"),
        class = "btn-sm btn-default",
        # 去掉文字只留图标；font-size 与导航栏菜单图标一致（btn-sm 默认 12px 偏小）
        style = "font-size:14px;line-height:1.25;padding:5px 9px;margin:7px 4px 0 6px;",
        title = "退出登录")
    )
  ))
  do.call(navbarPage, c(
    list(
      id = "hf_navbar",
      title = "HungFo",
      theme = shinytheme("cosmo"),
      collapsible = TRUE
    ),
    tabs
  ))
}

# ── HungFo 独立界面（登录遮罩 / 主导航由 Server 端 renderUI 驱动）──
# 进入方式：server.R 检测 rv$app_view == "hungfo" 时渲染本函数。
# 早期版本用 conditionalPanel + 隐式 output.hungfo_login_state 桥接登录态，
# 该桥接依赖客户端隐藏 output 绑定，重启 / 会话重建后容易失效，导致「点击登录没反应」。
# 现改为 uiOutput("hf_auth_view")，由 hungfo_server 的 renderUI 直接依赖 rv$logged_in，
# 状态切换可靠；且每个新会话都会重新执行 hf_navbar_ui() 读取导航配置，刷新即生效。
hungfo_standalone_ui <- function(show_back = TRUE) {
  tagList(
    # 工作台样式（绝对资源路径，避免相对路径 / 浏览器缓存导致样式丢失）
    tags$head(tags$link(rel = "stylesheet", href = "/hungfo/hungfo.css")),
    # 导航菜单悬停显示「名称全称」：把 data-hf-nav-full 同步到导航 <a> 标签
    tags$script(src = "/hungfo/hungfo_nav_tooltip.js"),
    # 登录遮罩 / 主导航统一由 Server 端按登录态渲染
    uiOutput("hf_auth_view")
  )
}

# ── HungFo 主 Server ──
hungfo_server <- function(input, output, session, rv) {
  hf_trigger <- reactiveVal(0)

  # 登录视图：直接依赖 rv$logged_in 渲染登录遮罩 / 主导航。
  # 替代原 conditionalPanel + 隐式 output.hungfo_login_state 桥接（该桥接在重启 / 会话
  # 重建时会失效，表现为「点击登录没反应且无报错」）；renderUI 的响应式依赖明确可靠。
  # 每个新会话都会重新执行 → hf_navbar_ui() 重新读 DB 导航配置，刷新即生效。
  output$hf_auth_view <- renderUI({
    if (!isTRUE(rv$logged_in)) return(hf_login_overlay())
    # 独立运行时不显示「回 ITOM」；父项目内嵌时显示（rv 无 hungfo_standalone 字段）
    hf_navbar_ui(show_back = !isTRUE(rv$hungfo_standalone))
  })

  # 当前用户显示已下沉到「配置」模块页面（hf_mod_config.r），导航栏不再展示

  # 登录（独立运行：先确保建表播种，再校验凭据）
  # 不使用 req() 静默阻断：空输入 / 初始化异常都要给出可见反馈，避免「点击没反应也没报错」
  observeEvent(input$hf_login_btn, {
    user <- trimws(hf_or(input$hf_login_username, ""))
    pwd  <- hf_or(input$hf_login_password, "")
    if (!nzchar(user) || !nzchar(pwd)) {
      showNotification("请输入用户名和密码", type = "warning")
      return()
    }
    res <- tryCatch({
      hf_db_init()
      hf_auth_login(user, pwd)
    }, error = function(e) {
      list(success = FALSE, message = paste0("登录初始化失败：", conditionMessage(e)))
    })
    if (isTRUE(res$success)) {
      rv$logged_in <- TRUE
      rv$current_user <- res$user
      dn <- if (!is.null(res$user$display_name) && length(res$user$display_name) > 0 &&
                !is.na(res$user$display_name[1]) && nzchar(res$user$display_name[1])) res$user$display_name[1] else res$user$username[1]
      showNotification(sprintf("欢迎回来，%s！", dn), type = "message")
    } else {
      showNotification(hf_or(res$message, "用户名或密码错误"), type = "error")
    }
  })

  # 退出登录（HungFo 自身登出，仅清空 HungFo 登录态）
  # renderUI 会自动响应 rv$logged_in 变化重新渲染登录遮罩，无需 session$reload()
  observeEvent(input$hf_logout_btn, {
    rv$logged_in <- FALSE
    rv$current_user <- NULL
    showNotification("已退出登录", type = "message")
  }, ignoreInit = TRUE)

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
    hf_project_init()          # 项目模块建表（需求/记事/总结/项目模板）
    hf_workbench_init()        # 网页工作台建表（运行记录）
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

# ── 网页工作台环节专属页面（提交任务给 CodeBuddy CLI，实时回传过程/结果）──
hf_workbench_page_ui <- function(m) {
  p <- function(x) paste0("hf_workbench_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      # 顶部横幅
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-WORKBENCH"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;", m$desc),
        tags$div(style = "font-size:11px;color:#b0bec5;margin-top:4px;", icon("plug"), " 接口：", m$api_desc)
      ),
      # 会话 Token 消耗概览（工作台 CLI 会话 + 本机 CodeBuddy 会话清单）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:8px;", "✦ 会话 Token 消耗"),
        uiOutput(p("token_kpi"))
      ),
      # 任务输入区（按钮文案改为「提交给CodeBuddy」；刷新按钮迁至真正起作用的「任务清单 / 运行记录」栏）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:6px;", "✦ 提交任务给CodeBuddy立即执行"),
        textAreaInput(p("task_in"), NULL, rows = 3, width = "100%",
          placeholder = "例如：修复 xx 模块的 bug / 帮我写一个 xx 功能…"),
        div(style = "display:flex;gap:8px;justify-content:flex-end;margin-top:4px;",
          actionButton(p("run_btn"), "提交给CodeBuddy", icon = icon("play"), class = "btn-success btn-sm")
        )
      ),
      # 执行过程区：紧凑日志窗（固定 50 行高度、自动下滚、末尾显示结果/状态、可收缩）
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        # 样式内联在本页：动态 UI / 浏览器缓存都不会让执行过程样式失效
        tags$style(HTML("
          .hf-wb-stream{height:420px;max-height:56vh;overflow-y:auto;padding:6px 8px;background:#fbfcfe;border:1px solid #e8ecf1;border-radius:6px;font-size:11px;line-height:1.5;}
          .hf-wb-stat{display:flex;flex-wrap:wrap;gap:8px;align-items:center;font-size:10px;color:#78909c;margin-bottom:6px;}
          .hf-wb-stat strong{color:#37474f;}
          .hf-wb-line{display:flex;gap:6px;align-items:flex-start;padding:2px 2px;border-bottom:1px solid #f0f2f6;}
          .hf-wb-line:last-of-type{border-bottom:none;}
          .hf-wb-no{display:inline-flex;align-items:center;justify-content:center;min-width:22px;height:18px;padding:0 4px;border-radius:5px;background:#eceff1;color:#455a64;font-size:10px;font-weight:700;font-family:Consolas,monospace;flex-shrink:0;}
          .hf-wb-badge{font-size:10px;color:#fff;padding:0 6px;border-radius:8px;white-space:nowrap;flex-shrink:0;line-height:18px;}
          .hf-wb-one{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;color:#2b2b29;font-size:11px;line-height:18px;}
          .hf-wb-details{margin:2px 0 4px 28px;font-size:10px;}
          .hf-wb-details>summary{cursor:pointer;color:#1565c0;}
          .hf-wb-code{background:#1e1e1e;color:#d1e3ff;border-radius:5px;padding:6px 8px;margin:3px 0 0;font-size:10px;line-height:1.45;max-height:200px;overflow:auto;white-space:pre-wrap;word-break:break-word;}
          .hf-wb-footer{margin-top:6px;padding:7px 9px;border-radius:6px;background:#f7faf8;font-size:11px;line-height:1.5;color:#37474f;word-break:break-word;}
        ")),
        div(style = "display:flex;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:6px;",
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 执行过程（最近 50 步 · 自动下滚）"),
          uiOutput(p("log_status"), inline = TRUE),
          div(style = "margin-left:auto;",
            tags$button(id = p("stream_toggle"), type = "button",
              class = "btn btn-default btn-xs", onclick = "hfWbToggleStream()",
              "▾ 收缩过程"))
        ),
        div(
          div(id = p("stream_hint"),
            style = "display:none;padding:14px;text-align:center;color:#90a4ae;font-size:12px;background:#f8f9fc;border:1px dashed #cfd8dc;border-radius:8px;",
            "执行过程已收缩 · 点击右上「▸ 展开过程」，或单击任务清单中的任一任务展开全部过程"),
          div(id = p("stream_body"), class = "hf-wb-body-wrap", uiOutput(p("log_out")))
        )
      ),
      # 历史运行记录（含每次会话 Token 消耗）
      # 刷新按钮移到此栏右侧：local_refresh() 实际刷新运行记录表 / Token KPI / 会话清单表 + 状态同步，
      # 执行过程由 reactivePoll 1.5 秒轮询自动刷新，与该按钮无关
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        div(style = "display:flex;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:6px;",
          tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;",
            "✦ 任务清单 / 运行记录（单击展开全部过程 · 再次单击同一行收缩 · 双击查看原任务描述）"),
          div(style = "margin-left:auto;",
            actionButton(p("refresh_btn"), "刷新", icon = icon("sync"),
              class = "btn-default btn-sm",
              title = "刷新运行记录、Token 消耗与状态（执行过程由 1.5 秒轮询自动刷新）"))
        ),
        DTOutput(p("runs_table"))
      ),
      # 本机 CodeBuddy 会话清单 Token 明细
      div(style = "background:#fff;border-radius:8px;padding:12px;margin:8px 0;border:1px solid #e8ecf1;",
        tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin-bottom:6px;", "✦ CodeBuddy 会话清单 Token 明细"),
        tags$div(style = "font-size:11px;color:#999;margin-bottom:6px;", "来源：本机 codebuddy-sessions 采集（hf_cb_sessions）"),
        DTOutput(p("cb_sessions_table"))
      ),
      # 收缩/展开交互：直接改 DOM 的 display（不依赖外部 CSS），日志轮询重渲染也不会丢失
      # 行为约定：默认全部展开；单击行 = 展开该任务全部过程；再次单击同一行 = 收缩；
      #           「▾ 收缩过程」按钮手动切换；双击行 = 弹卡片看原任务描述（抑制两次单击的切换）。
      # 事件一律 $(document) 委托 + __hfWbBound 防重复绑定；不注册 hidden.bs.modal，
      # 避免与元任务卡片的 hidden.bs.modal.hfmt 命名空间绑定冲突。
      tags$script(HTML("
(function(){
  var H='hf_workbench_stream_hint', B='hf_workbench_stream_body',
      T='hf_workbench_stream_toggle';
  // 从行内隐藏 span 取 run_id（见 runs_table 的「状态」列）；空表 / 无数据行返回 ''
  window.hfWbRowRunId = function(tr){
    var s = $(tr).find('.hf-wb-rid').first();
    if(s.length){ var r = s.text().trim(); if(r) return r; }
    return '';
  };
  window.hfWbSetCollapsed = function(v){
    var h=document.getElementById(H), b=document.getElementById(B), t=document.getElementById(T);
    v = !!v;
    if(b) b.style.display = v ? 'none' : '';
    if(h) h.style.display = v ? 'block' : 'none';
    if(t) t.textContent = v ? '▸ 展开过程' : '▾ 收缩过程';
    window.HF_WB_COLLAPSED = v;
    // 折叠状态回传 Server（预留联动；折叠本身是纯 DOM 操作，日志重渲染不会重置 display）
    try { if(window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue('hf_workbench_stream_collapsed', v); } catch(e){}
  };
  window.hfWbToggleStream = function(){
    window.hfWbSetCollapsed(!window.HF_WB_COLLAPSED);
  };
  if(window.__hfWbBound) return;
  window.__hfWbBound = true;
  window.HF_WB_LAST_ROW = null;
  window.HF_WB_COLLAPSED = false;
  var clickTimer = null;
  // 页面就绪即强制「全部展开」，防止 Shiny 重渲染后样式回到中间态
  $(document).ready(function(){ window.hfWbSetCollapsed(false); });
  Shiny.addCustomMessageHandler('hfWbSetCollapsed', function(msg){
    window.hfWbSetCollapsed(!!msg.collapsed);
    if(!msg.collapsed) window.HF_WB_LAST_ROW = null;   // 提交新任务成功后视为全新开始
  });
  // 单击：延迟 260ms 执行，给 dblclick 留出取消窗口（浏览器事件顺序 click → click → dblclick）
  //       不同行 → 回传 run_id 选中该 run + 展开全部过程；再次单击同一行 → 切换为收缩
  $(document).on('click', '#hf_workbench_runs_table tbody tr', function(){
    var tr = this;
    if(clickTimer) clearTimeout(clickTimer);
    clickTimer = setTimeout(function(){
      clickTimer = null;
      var key = window.hfWbRowRunId(tr);
      if(!key) return;
      if(window.HF_WB_LAST_ROW === key){
        window.hfWbToggleStream();
      } else {
        Shiny.setInputValue('hf_workbench_row_run', key, {priority:'event'});
        window.hfWbSetCollapsed(false);
      }
      window.HF_WB_LAST_ROW = key;
    }, 260);
  });
  // 双击：取消挂起的单击（两次 click 全部作废，不会误触发展开/收缩），回传 run_id 弹卡片；
  //       刻意不改动 HF_WB_LAST_ROW，保证双击前后「再次单击同一行 = 收缩」的语义连续
  $(document).on('dblclick', '#hf_workbench_runs_table tbody tr', function(){
    if(clickTimer){ clearTimeout(clickTimer); clickTimer = null; }
    var key = window.hfWbRowRunId(this);
    if(!key) return;
    Shiny.setInputValue('hf_workbench_dbl_run', key, {priority:'event'});
  });
})();
"))
    )
  )
}

# ── CodeBuddy 执行过程 → 紧凑单行日志（类似 CodeBuddy 对话框）──
# 只保留最近 50 步，按时间从上往下排列；固定 50 行窗口自动下滚，末尾显示执行结果和状态。
# 长文本用原生 <details> 按需展开，不把窗体整体撑开。
# ★ 所有日志内容一律走 tags$* 文本节点，由 Shiny 自动 HTML 转义，禁止拼接 HTML(日志)。
hf_workbench_event_ui <- function(rid) {
  ev <- hf_workbench_events(rid, max_events = 50, text_limit = 240)
  st <- hf_workbench_status(rid)
  st_lbl <- switch(hf_or(st, ""), running = "运行中", done = "已完成", error = "出错", "—")
  st_col <- switch(hf_or(st, ""), running = "#2563eb", done = "#0d7d3a", error = "#d9534f", "#999")
  if (length(ev$events) == 0) {
    return(tags$div(
      tags$div(style = "color:#bbb;font-size:11px;text-align:center;padding:20px;",
        "等待 CodeBuddy 输出…"),
      tags$div(class = "hf-wb-footer",
        tags$strong(style = paste0("color:", st_col, ";"), "● ", st_lbl),
        "  ·  执行结果：等待任务输出…")))
  }

  badge <- function(label, bg) tags$span(class = "hf-wb-badge", style = paste0("background:", bg, ";"), label)
  one <- function(s) gsub("\\s+", " ", hf_or(s, ""))
  brief <- function(s) substr(one(s), 1, 120)
  # 一行一步：序号 + 类型徽标 + 单行摘要；超长内容才出现「展开全文」
  line <- function(color, label, no, summary, full = NULL) {
    s <- one(summary)
    f <- if (is.null(full)) "" else one(full)
    tags$div(class = "hf-wb-line",
      tags$span(class = "hf-wb-no", sprintf("%02d", no)),
      badge(label, color),
      tags$div(style = "flex:1;min-width:0;",
        tags$div(class = "hf-wb-one", title = if (nzchar(f)) f else s, s),
        if (nzchar(f) && nchar(f) > nchar(s))
          tags$details(class = "hf-wb-details",
            tags$summary("展开全文"), tags$pre(class = "hf-wb-code", full)) else NULL))
  }

  render_event <- function(e, i) {
    no <- ev$dropped + i
    if (identical(e$kind, "meta")) {
      line("#607d8b", "会话", no,
        paste0("模型 ", hf_or(e$model, "—"), " · 工作目录 ", hf_or(e$cwd, "—"),
               " · 权限 ", hf_or(e$permission, "—")))
    } else if (identical(e$kind, "thinking")) {
      line("#6c3bbf", "思考", no, brief(e$text$text), e$text$text)
    } else if (e$kind %in% c("text", "user_text")) {
      line("#2563eb", if (identical(e$kind, "text")) "回复" else "输入",
           no, brief(e$text$text), e$text$text)
    } else if (identical(e$kind, "tool")) {
      res <- e$result
      head <- paste0(hf_or(e$name, "tool"),
        if (!is.null(res)) if (isTRUE(res$is_error)) " · 失败" else " · 成功" else "",
        if (nzchar(hf_or(e$brief, ""))) paste0(" · ", one(e$brief)) else "")
      full <- paste0("调用参数：\n", e$input$text,
        if (!is.null(res)) paste0("\n\n执行结果：\n", res$text$text) else "")
      line("#00695c", "工具", no, head, full)
    } else if (identical(e$kind, "orphan_result")) {
      line("#f59e0b", "工具结果", no, brief(e$text$text), e$text$text)
    } else if (identical(e$kind, "result")) {
      col <- if (isTRUE(e$ok)) "#0d7d3a" else "#d9534f"
      line(col, if (isTRUE(e$ok)) "完成" else "出错", no,
        sprintf("%s · 耗时 %.1fs · %d 轮 · 费用 %s", brief(e$text$text),
                e$duration_ms / 1000, as.integer(e$num_turns),
                if (nzchar(hf_or(e$cost, ""))) paste0("$", e$cost) else "—"),
        e$text$text)
    } else NULL
  }

  kinds <- vapply(ev$events, function(x) x$kind, character(1))
  stat <- tags$div(class = "hf-wb-stat",
    tags$strong(sprintf("共 %d 步", length(ev$events) + ev$dropped)),
    tags$span(paste0("思考 ", sum(kinds == "thinking"))),
    tags$span(paste0("回复 ", sum(kinds == "text" | kinds == "user_text"))),
    tags$span(paste0("工具 ", sum(kinds == "tool"))),
    if (ev$dropped > 0) tags$span(style = "color:#d9534f;",
      sprintf("仅显示最近 %d 步（省略 %d 步）", length(ev$events), ev$dropped)) else NULL,
    tags$a(href = ev$log_url, target = "_blank", rel = "noopener",
      style = "margin-left:auto;color:#1565c0;", "完整日志 ↗"))

  # 末尾固定显示：执行结果 + 状态
  res_ev <- NULL
  for (e in ev$events) if (identical(e$kind, "result")) res_ev <- e
  footer <- tags$div(class = "hf-wb-footer",
    tags$strong(style = paste0("color:", st_col, ";"), "● ", st_lbl),
    "  ·  执行结果：",
    if (!is.null(res_ev)) one(res_ev$text$text) else "等待任务完成…",
    if (!is.null(res_ev)) sprintf("  ·  耗时 %.1fs · %d 轮",
      res_ev$duration_ms / 1000, as.integer(res_ev$num_turns)) else NULL)

  tags$div(
    tags$div(id = "hf_workbench_stream", class = "hf-wb-stream",
      stat,
      do.call(tagList, lapply(seq_along(ev$events), function(i) render_event(ev$events[[i]], i))),
      # 每次重渲染都滚到最底：日志从上往下，末尾的结果/状态始终可见
      tags$script(HTML("(function(){var e=document.getElementById('hf_workbench_stream');if(e)e.scrollTop=e.scrollHeight;})();"))),
    footer)
}

hf_workbench_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_workbench_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)
  current_run <- reactiveVal(NULL)

  # run_state() 提供实时失效；同步日志中的 Token 消耗后再读运行记录
  runs_df <- reactive({
    trigger(); local_trigger(); run_state()
    hf_workbench_refresh_status()
    hf_workbench_list()
  })

  # 运行记录表
  output[[p("runs_table")]] <- DT::renderDT({
    df <- runs_df()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    st_badge <- function(s) {
      s <- hf_or(s, "running")
      switch(s,
        running = '<span style="color:#2563eb;">● 运行中</span>',
        done = '<span style="color:#0d7d3a;">● 已完成</span>',
        error = '<span style="color:#d9534f;">● 出错</span>',
        sprintf('<span>%s</span>', s))
    }
    tk <- function(x) ifelse(is.na(x), "0", format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE))
    col <- function(nm) if (is.null(df[[nm]])) rep(0, nrow(df)) else df[[nm]]
    out <- data.frame(
      ID = df$id,
      任务 = vapply(seq_len(nrow(df)), function(i) {
        one <- gsub("\\s+", " ", hf_or(df$task[i], ""))
        if (nchar(one) > 40) paste0(substr(one, 1, 40), "…") else one
      }, character(1)),
      状态 = vapply(seq_len(nrow(df)), function(i) {
        # 复用本已含 HTML 徽标的列承载 run_id（本表 escape = FALSE 已开启）：
        # 前端单击行据此选中 run、双击行据此查看原任务描述；run_id 为纯字母数字，无需额外转义
        paste0('<span class="hf-wb-rid" style="display:none">',
               hf_or(df$run_id[i], ""), '</span>',
               st_badge(df$status[i]))
      }, character(1)),
      Token合计 = tk(col("token_total")),
      输入 = tk(col("token_in")),
      输出 = tk(col("token_out")),
      缓存命中 = tk(col("token_cache_read")),
      创建时间 = ifelse(is.na(df$created_at) | df$created_at == "", "—", df$created_at),
      结束时间 = ifelse(is.na(df$finished_at) | df$finished_at == "", "—", df$finished_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 10, escape = FALSE)
  })

  # 会话 Token 消耗 KPI（今日 / 本周 / 本月 / 历史全部 + 工作台分项 + 本机 CodeBuddy 会话清单）
  output[[p("token_kpi")]] <- renderUI({
    trigger(); local_trigger(); runs_df()   # 依赖 runs_df 确保 Token 已同步
    sm <- hf_workbench_summary()
    cb <- hf_cb_sessions_summary()
    fmt <- function(x) format(as.numeric(hf_or(x, 0)), big.mark = ",", scientific = FALSE, trim = TRUE)
    tile <- function(label, value, color) {
      tags$div(style = "flex:1;min-width:92px;background:#f8f9fc;border:1px solid #e8ecf1;border-radius:8px;padding:8px 10px;",
        tags$div(style = "font-size:11px;color:#888;", label),
        tags$div(style = paste0("font-size:18px;font-weight:700;color:", color, ";"), value))
    }
    # 四组统计：每组「Token 合计 + 会话数」，沿用原有 tile 风格
    grp <- function(label, s, color) {
      tags$div(style = "flex:1;min-width:210px;background:#fff;border:1px solid #e8ecf1;border-radius:8px;padding:8px;",
        tags$div(style = paste0("font-size:11px;font-weight:700;color:", color, ";margin-bottom:6px;"), label),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:6px;",
          tile("Token 合计", fmt(s$token_total), color),
          tile("会话数", fmt(s$sessions), "#37474f")))
    }
    z <- list(token_in = 0, token_out = 0, token_cache_read = 0,
              token_cache_write = 0, token_total = 0, sessions = 0)
    tdy <- if (is.list(sm$today)) sm$today else z
    wk  <- if (is.list(sm$week))  sm$week  else z
    mo  <- if (is.list(sm$month)) sm$month else z
    all <- if (is.list(sm$total)) sm$total else z
    tags$div(
      tags$div(style = "display:flex;flex-wrap:wrap;gap:8px;",
        grp("今日", tdy, "#d9534f"),
        grp("本周（近 7 天）", wk, "#f59e0b"),
        grp("本月", mo, "#0d47a1"),
        grp("历史全部", all, "#6c3bbf")
      ),
      tags$div(style = "font-size:11px;color:#999;margin:8px 0 6px;",
        "口径：今日=本地当天；本周=近 7 天含今日；本月=当前自然月；历史全部=全部运行记录（数据源 hf_workbench_runs.created_at）。下方为工作台 Token 分项与本机 CodeBuddy 会话清单概览。"),
      tags$div(style = "display:flex;flex-wrap:wrap;gap:8px;",
        tile("工作台输入 Token", fmt(all$token_in), "#0d47a1"),
        tile("工作台输出 Token", fmt(all$token_out), "#00695c"),
        tile("工作台缓存命中", fmt(all$token_cache_read), "#f59e0b"),
        tile("CodeBuddy 会话", fmt(cb$total_sessions), "#37474f"),
        tile("清单 Token 总计", fmt(cb$total_tokens), "#6c3bbf")
      )
    )
  })

  # 本机 CodeBuddy 会话清单 Token 明细
  output[[p("cb_sessions_table")]] <- DT::renderDT({
    trigger(); local_trigger()
    df <- hf_cb_sessions_list(50)
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(df, empty = "no_data"))
    tk <- function(x) ifelse(is.na(x), "0", format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE))
    out <- data.frame(
      会话 = vapply(seq_len(nrow(df)), function(i) {
        t <- gsub("\\s+", " ", hf_or(df$title[i], ""))
        if (!nzchar(t)) t <- hf_or(df$conversation_id[i], "")
        if (nchar(t) > 36) paste0(substr(t, 1, 36), "…") else t
      }, character(1)),
      Token总计 = tk(df$token_total),
      输入 = tk(df$token_input),
      输出 = tk(df$token_output),
      缓存命中 = tk(df$token_cache_hit),
      思考 = tk(df$token_thinking),
      命中率 = ifelse(is.na(df$token_cache_hit_rate), "—", paste0(df$token_cache_hit_rate, "%")),
      更新时间 = ifelse(is.na(df$updated_at) | df$updated_at == "", "—", df$updated_at),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "none", page_size = 10, escape = FALSE)
  })

  # 点击运行记录 → 查看日志
  observeEvent(input[[p("runs_table_rows_selected")]], {
    idx <- input[[p("runs_table_rows_selected")]]
    df <- runs_df()
    if (is.null(idx) || length(idx) == 0 || is.null(df) || nrow(df) == 0) return()
    current_run(df$run_id[idx[1]])
    local_refresh()
  })

  # 前端单击行回传 run_id：选中该 run（与 DT rows_selected 兜底并存，值相同则不再重复刷新）
  # 注意：此处刻意不发送 hfWbSetCollapsed 自定义消息，否则 handler 会清空 HF_WB_LAST_ROW，
  #       破坏「再次单击同一行 = 收缩」；展开动作由前端 hfWbSetCollapsed(false) 完成
  observeEvent(input[["hf_workbench_row_run"]], {
    rid <- hf_or(input[["hf_workbench_row_run"]], "")
    if (!nzchar(rid)) return()
    df <- runs_df()
    if (is.null(df) || nrow(df) == 0 || !(rid %in% df$run_id)) return()
    if (identical(current_run(), rid)) return()
    current_run(rid)
    local_refresh()
  })

  # 前端折叠状态回传（预留联动；折叠本身由前端纯 DOM 控制，日志重渲染不会重置 display）
  stream_collapsed <- reactiveVal(FALSE)
  observeEvent(input[["hf_workbench_stream_collapsed"]], {
    stream_collapsed(isTRUE(input[["hf_workbench_stream_collapsed"]]))
  }, ignoreNULL = FALSE)

  # 双击清单行 → 弹出卡片查看「原任务描述」（run_id 由前端隐藏 span 回传，固定 input id）
  observeEvent(input[["hf_workbench_dbl_run"]], {
    rid <- hf_or(input[["hf_workbench_dbl_run"]], "")
    if (!nzchar(rid)) return()
    r <- hf_workbench_get(rid)
    if (is.null(r) || nrow(r) == 0) {
      showNotification("未找到该任务的原始记录", type = "warning"); return()
    }
    r <- r[1, ]
    st_lbl <- switch(hf_or(r$status, ""),
      running = "运行中", done = "已完成", error = "出错", "—")
    fmtk <- function(x) format(as.numeric(hf_or(x, 0)), big.mark = ",", scientific = FALSE, trim = TRUE)
    meta <- function(label, value) tags$span(tags$b(paste0(label, "：")), value)
    showModal(modalDialog(
      title = tagList(icon("eye"), " 原任务描述 · ", hf_or(r$run_id, "")),
      size = "l", easyClose = TRUE, footer = modalButton("关闭"),
      tags$div(style = "display:flex;flex-wrap:wrap;gap:6px 16px;font-size:11px;color:#607d8b;margin-bottom:8px;",
        meta("run_id", hf_or(r$run_id, "—")),
        meta("状态", st_lbl),
        meta("创建时间", hf_or(r$created_at, "—")),
        meta("结束时间", hf_or(r$finished_at, "—")),
        meta("Token 合计", fmtk(r$token_total)),
        meta("输入 / 输出", paste0(fmtk(r$token_in), " / ", fmtk(r$token_out))),
        meta("缓存命中", fmtk(r$token_cache_read))),
      tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;margin:6px 0;",
        "任务原文（换行原样保留）"),
      # tags$pre 走文本节点由 Shiny 自动转义，white-space:pre-wrap 保留原文换行
      tags$pre(style = "white-space:pre-wrap;word-break:break-word;background:#fbfcfe;border:1px solid #e8ecf1;border-radius:6px;padding:10px;font-size:12px;line-height:1.6;max-height:55vh;overflow:auto;margin:0;",
        hf_or(r$task, ""))
    ))
  })

  # 运行按钮
  observeEvent(input[[p("run_btn")]], {
    req(rv$logged_in)
    task <- trimws(hf_or(input[[p("task_in")]], ""))
    if (!nzchar(task)) { showNotification("请输入任务内容", type = "warning"); return() }
    r <- hf_workbench_run(task, created_by = hf_current_uid(rv))
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) {
      current_run(r$run_id)
      session$sendCustomMessage("hfWbSetCollapsed", list(collapsed = FALSE))
      updateTextAreaInput(session, p("task_in"), value = "")
    }
    local_refresh()
  })

  observeEvent(input[[p("refresh_btn")]], local_refresh())

  # 实时状态 + 日志（reactivePoll 每 1.5 秒轮询）
  run_state <- reactivePoll(1500, session,
    checkFunc = function() {
      rid <- current_run()
      if (is.null(rid)) return("")
      st <- hf_workbench_status(rid)
      lp <- hf_workbench_logfile(rid)
      sig <- paste0(st, "|")
      if (!is.null(lp) && file.exists(lp)) sig <- paste0(sig, as.numeric(file.info(lp)$mtime), "|", file.info(lp)$size)
      sig
    },
    valueFunc = function() {
      rid <- current_run()
      if (is.null(rid)) return(list(status = ""))
      hf_workbench_refresh_status()
      # 日志不再整段回传：由 hf_workbench_event_ui() 按需解析结构化事件（带缓存）
      list(status = hf_workbench_status(rid))
    }
  )

  output[[p("log_status")]] <- renderUI({
    rid <- current_run()
    st <- run_state()$status
    if (is.null(rid)) return(tags$span(style = "color:#999;font-size:12px;", "尚未运行任务。"))
    lbl <- switch(hf_or(st, ""), running = "运行中", done = "已完成", error = "出错", "—")
    col <- switch(hf_or(st, ""), running = "#2563eb", done = "#0d7d3a", error = "#d9534f", "#999")
    tags$span(style = paste0("font-size:12px;font-weight:700;color:", col, ";"),
      "● ", lbl, " · ", rid)
  })

  output[[p("log_out")]] <- renderUI({
    run_state()   # 依赖 reactivePoll：日志文件增长时重新渲染结构化时间线
    rid <- current_run()
    if (is.null(rid)) {
      return(tags$div(style = "color:#bbb;font-size:12px;text-align:center;padding:20px;",
        "提交任务后，这里将实时显示 CodeBuddy 的执行过程与结果"))
    }
    hf_workbench_event_ui(rid)
  })
}
