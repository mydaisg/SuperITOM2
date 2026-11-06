##################
# HungFo 环节 13 — 配置 (HF-CONFIG)
# 职责：子项目系统配置中心。三个可配置子 Tab：
#   1. 导航菜单    —— 名称 / 名称全称 / 图标 / 位置 / 显示隐藏（含当前登录用户）
#   2. 规则分类    —— 从规则主界面下沉；分类名称 + 适用任务类型 + 默认模型/effort/轮数 + 启停
#   3. 模型路由    —— 任务类型 → 分类 / 模型 / effort / 最大轮数 / 降级模型
# 配置存 SQLite（hf_nav_config / hf_rules_categories / hf_config_kv），
# 导航保存后实时刷新（session$reload + UI 函数化）；分类保存后同步规则页下拉。
##################

HF_MOD_CONFIG <- list(
  id = "hf20", key = "config", name = "配置", icon = "cog",
  desc = "配置中心：导航菜单、规则分类（含 AI 读取路由）与任务类型→模型路由，均可自定义。",
  api_desc = "nav() 取导航配置 · save()/reset() 导航 · categories()/routes() 取 AI 路由配置",
  status = "scaffold", version = "v0.3"
)

# 导航配置数据层透传接口（供跨环节 hf_call("config", ...) 调用）
hf_config_api_nav <- function() hf_nav_config_list()
hf_config_api_save <- function(nav_key, name = NULL, icon = NULL, position = NULL,
                               visible = NULL, full_name = NULL) {
  hf_nav_config_save(nav_key, name, icon, position, visible, full_name)
}
hf_config_api_reset <- function() hf_nav_config_reset()
hf_config_api_categories <- function() hf_rules_categories_df()
hf_config_api_routes <- function() hf_model_routes()

hf_register(HF_MOD_CONFIG, hf_make_api("config",
  nav = hf_config_api_nav,
  save = hf_config_api_save,
  reset = hf_config_api_reset,
  categories = hf_config_api_categories,
  routes = hf_config_api_routes
))

# ── 常用 FontAwesome 图标（导航菜单可选）──
HF_NAV_ICONS <- c(
  "cog", "sliders-h", "home", "cubes", "gavel", "spider", "database", "brain",
  "chart-line", "clock", "history", "code-branch", "file-alt", "sitemap", "plug",
  "user-shield", "network-wired", "dashboard", "table", "server", "tasks", "book",
  "search", "gear", "wrench", "eye", "bell", "star", "th-large", "list", "folder",
  "folder-open", "chart-bar", "chart-pie", "line-chart", "area-chart", "signal",
  "exchange", "link", "share-alt", "download", "upload", "paper-plane", "inbox",
  "calendar", "tag", "tags", "filter", "magic", "microchip", "robot", "project-diagram"
)

# ── 配置环节专属页面 UI（模块内子 Tab，不新增导航项）──
hf_config_page_ui <- function(m) {
  p <- function(x) paste0("hf_config_", x)
  tabPanel(
    m$name, icon = icon(m$icon),
    div(style = "padding:4px 2px;",
      div(style = "background:linear-gradient(135deg,#263238,#37474f);color:#fff;border-radius:8px;padding:14px 18px;margin-bottom:12px;",
        tags$span(style = "font-size:16px;font-weight:bold;", icon(m$icon), " ", m$name),
        tags$span(style = "font-size:11px;background:#455a64;padding:2px 8px;border-radius:10px;margin-left:8px;", "HF-CONFIG"),
        tags$span(style = "font-size:11px;background:#00695c;padding:2px 8px;border-radius:10px;margin-left:6px;", hf_or(m$status, "scaffold")),
        tags$div(style = "font-size:12px;opacity:.88;margin-top:8px;", m$desc),
        tags$div(style = "font-size:11px;color:#b0bec5;margin-top:4px;", icon("plug"), " 接口：", m$api_desc),
        # 当前用户（原顶部导航栏 hf_nav_user，下沉到配置页显示）
        tags$div(style = "font-size:12px;opacity:.95;margin-top:6px;",
          icon("user-circle"), " 当前登录用户：", uiOutput(p("current_user"), inline = TRUE))
      ),
      tabsetPanel(id = p("tabs"),
        # ── 子 Tab 1：导航菜单（原有功能，input/output id 全部保持不变）──
        tabPanel("导航菜单", value = "nav",
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
              tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 导航栏菜单配置"),
              div(style = "margin-left:auto;display:flex;gap:6px;",
                actionButton(p("edit_btn"), "编辑选中", icon = icon("pen"), class = "btn-primary btn-sm"),
                actionButton(p("refresh_btn"), "刷新", icon = icon("sync"), class = "btn-info btn-sm"),
                actionButton(p("reset_btn"), "重置默认", icon = icon("undo"), class = "btn-default btn-sm")
              )
            ),
            tags$div(style = "font-size:11px;color:#999;margin-top:6px;",
              "可配置每个导航菜单的名称、名称全称（鼠标悬停显示）、图标、位置（数字越小越靠前）与显示/隐藏。保存后刷新即生效。")
          ),
          uiOutput(p("status")),
          DTOutput(p("table"))
        ),
        # ── 子 Tab 2：规则分类（从规则主界面下沉；同时是 AI 自动分类读取的路由依据）──
        tabPanel("规则分类", value = "rulecat",
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
              tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 规则分类与 AI 读取路由"),
              div(style = "margin-left:auto;display:flex;gap:6px;",
                actionButton(p("cat_add_btn"), "新增分类", icon = icon("plus"), class = "btn-success btn-sm"),
                actionButton(p("cat_edit_btn"), "编辑选中", icon = icon("pen"), class = "btn-primary btn-sm"),
                actionButton(p("cat_refresh_btn"), "刷新", icon = icon("sync"), class = "btn-info btn-sm"),
                actionButton(p("cat_export_btn"), "导出规则/记忆", icon = icon("download"), class = "btn-default btn-sm")
              )
            ),
            tags$div(style = "font-size:11px;color:#999;margin-top:6px;",
              "适用任务类型决定 AI 执行哪类任务时自动读取本分类规则；常驻注入=所有任务都读。分类改名会同步已有规则。")
          ),
          uiOutput(p("cat_status")),
          DTOutput(p("cat_table"))
        ),
        # ── 子 Tab 3：模型路由（便宜任务走便宜模型，避免一律用贵模型）──
        tabPanel("模型路由", value = "routes",
          div(style = "background:#fff;border-radius:8px;padding:10px;margin:8px 0;border:1px solid #e8ecf1;",
            div(style = "display:flex;gap:6px;align-items:center;flex-wrap:wrap;",
              tags$div(style = "font-size:12px;font-weight:700;color:#6c3bbf;", "✦ 任务类型 → 模型 / 分类 路由表"),
              div(style = "margin-left:auto;display:flex;gap:6px;",
                actionButton(p("route_edit_btn"), "编辑选中", icon = icon("pen"), class = "btn-primary btn-sm"),
                actionButton(p("route_refresh_btn"), "刷新", icon = icon("sync"), class = "btn-info btn-sm"),
                actionButton(p("route_reset_btn"), "恢复默认", icon = icon("undo"), class = "btn-default btn-sm")
              )
            ),
            tags$div(style = "font-size:11px;color:#999;margin-top:6px;",
              "模型留空=CLI 账号默认（通常较贵）；简单任务建议指定 fast-model / custom-local 本地模型。降级模型在主模型过载时生效。")
          ),
          uiOutput(p("route_status")),
          DTOutput(p("route_table"))
        )
      )
    )
  )
}

# ── 配置环节专属 Server ──
hf_config_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_config_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(isolate(local_trigger()) + 1)
  cat_trigger <- reactiveVal(0)
  cat_refresh <- function() cat_trigger(isolate(cat_trigger()) + 1)
  route_trigger <- reactiveVal(0)
  route_refresh <- function() route_trigger(isolate(route_trigger()) + 1)
  cat_editing_id <- reactiveVal(NULL)
  route_editing_key <- reactiveVal(NULL)

  observeEvent(input[[p("refresh_btn")]], local_refresh())
  observeEvent(input[[p("cat_refresh_btn")]], cat_refresh())
  observeEvent(input[[p("route_refresh_btn")]], route_refresh())

  # 当前登录用户（原顶部导航栏显示，下沉到配置页；defensive 判空）
  output[[p("current_user")]] <- renderUI({
    u <- rv$current_user
    if (is.null(u) || is.null(nrow(u)) || nrow(u) == 0) return(tags$span("未登录"))
    dn <- if (!is.null(u$display_name) && length(u$display_name) > 0 &&
              !is.na(u$display_name[1]) && nzchar(u$display_name[1]))
            u$display_name[1] else hf_or(u$username[1], "未知用户")
    grp <- if (!is.null(u$group_names) && length(u$group_names) > 0 &&
               !is.na(u$group_names[1]) && nzchar(u$group_names[1]))
             paste0("（", u$group_names[1], "）") else ""
    tags$span(paste0(dn, grp))
  })

  # ══════════════ 子 Tab 1：导航菜单（原有逻辑，保持不变）══════════════
  observeEvent(input[[p("reset_btn")]], {
    showModal(modalDialog(
      title = "重置导航配置",
      "确认将导航栏所有菜单恢复为默认名称、名称全称、图标、位置与显示状态？",
      footer = tagList(
        actionButton(p("confirm_reset"), "确认重置", class = "btn-danger"),
        modalButton("取消")
      ), size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input[[p("confirm_reset")]], {
    r <- hf_nav_config_reset()
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); session$reload() }
  })

  nav_df <- reactive({
    trigger(); local_trigger()
    hf_nav_config_list()
  })

  output[[p("status")]] <- renderUI({
    df <- nav_df()
    n <- if (is.null(df)) 0 else nrow(df)
    vis <- if (is.null(df) || nrow(df) == 0) 0 else sum(df$visible == 1)
    div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("导航菜单："), tags$span(style = "color:#1565c0;", paste0(n, " 项")),
      " · ", tags$span(style = "color:#2e7d32;", paste0("显示 ", vis, " 项")),
      " · ", tags$span(style = "color:#888;", "保存后导航栏刷新即生效"))
  })

  output[[p("table")]] <- DT::renderDT({
    df <- nav_df()
    if (is.null(df) || nrow(df) == 0) return(hf_datatable(NULL, empty = "no_data"))
    kind_label <- function(k) c(overview = "总览", module = "环节", agent_monitor = "监控面板")[k]
    full <- as.character(hf_or(df$full_name, rep("", nrow(df))))
    blank <- is.na(full) | !nzchar(trimws(full))
    full[blank] <- as.character(df$name)[blank]
    out <- data.frame(
      key = df$nav_key,
      类型 = vapply(df$kind, kind_label, character(1)),
      名称 = df$name,
      名称全称 = full,
      图标 = df$icon,
      位置 = df$position,
      显示 = ifelse(df$visible == 1, "显示", "隐藏"),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 30)
  })

  selected_key <- reactive({
    idx <- input[[p("table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    df <- nav_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$nav_key[idx[1]]
  })

  observeEvent(input[[p("edit_btn")]], {
    req(rv$logged_in)
    key <- selected_key()
    if (is.null(key)) { showNotification("请先在列表选中一项导航菜单", type = "warning"); return() }
    df <- nav_df()
    row <- df[df$nav_key == key, , drop = FALSE]
    if (nrow(row) == 0) { showNotification("导航项不存在，请刷新后重试", type = "warning"); return() }
    is_config <- identical(key, "config")
    icon_choices <- unique(c(hf_or(row$icon[1], "cog"), HF_NAV_ICONS))
    init_full <- hf_or(row$full_name[1], "")
    if (!nzchar(trimws(as.character(init_full)))) init_full <- hf_or(row$name[1], "")
    showModal(modalDialog(
      title = paste0("编辑导航菜单：", hf_or(row$name[1], key)),
      textInput(p("edit_name"), "菜单名称", value = hf_or(row$name[1], ""), width = "100%"),
      textInput(p("edit_full_name"), "名称全称（鼠标悬停显示）", value = init_full, width = "100%"),
      selectInput(p("edit_icon"), "图标", choices = icon_choices,
                  selected = hf_or(row$icon[1], "cog"), width = "100%"),
      numericInput(p("edit_pos"), "位置（数字越小越靠前）", value = hf_or(row$position[1], 1L),
                   min = 1, step = 1, width = "100%"),
      selectInput(p("edit_visible"), "显示", choices = c("显示" = "1", "隐藏" = "0"),
                  selected = if (row$visible[1] == 1) "1" else "0", width = "100%"),
      if (is_config) tags$div(style = "font-size:11px;color:#d9534f;margin-top:6px;",
        "「配置」自身不可隐藏，否则将无法再进入本配置页。"),
      footer = tagList(
        actionButton(p("save_btn"), "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input[[p("save_btn")]], {
    req(rv$logged_in)
    key <- selected_key()
    if (is.null(key)) return()
    name <- trimws(hf_or(input[[p("edit_name")]], ""))
    if (!nzchar(name)) { showNotification("菜单名称不能为空", type = "warning"); return() }
    full_name <- trimws(hf_or(input[[p("edit_full_name")]], ""))
    if (!nzchar(full_name)) full_name <- name
    visible <- as.integer(input[[p("edit_visible")]])
    if (identical(key, "config") && visible == 0) {
      showNotification("「配置」菜单不可隐藏", type = "warning"); return()
    }
    r <- hf_nav_config_save(key, name = name, icon = input[[p("edit_icon")]],
                            position = input[[p("edit_pos")]], visible = visible,
                            full_name = full_name)
    showNotification(r$message, type = if (isTRUE(r$success)) "message" else "error")
    if (isTRUE(r$success)) { removeModal(); session$reload() }
  })

  # ══════════════ 子 Tab 2：规则分类 ══════════════
  cat_df <- reactive({
    trigger(); cat_trigger()
    hf_rules_categories_df()
  })

  output[[p("cat_status")]] <- renderUI({
    d <- cat_df()
    n <- if (is.null(d)) 0 else nrow(d)
    tags$div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("规则分类："), tags$span(style = "color:#1565c0;", paste0(n, " 项")),
      " · 常驻注入 ",
      tags$span(style = "color:#2e7d32;",
        as.character(if (is.null(d) || is.null(d$always_apply)) 0 else sum(d$always_apply == 1, na.rm = TRUE))),
      " 项 · 修改后规则页下拉会自动同步，并重新导出给 AI 读取")
  })

  output[[p("cat_table")]] <- DT::renderDT({
    d <- cat_df()
    if (is.null(d) || nrow(d) == 0) return(hf_datatable(NULL, empty = "no_data"))
    # 向量安全的取值：hf_or 只适用于标量，这里逐元素处理 NA
    g <- function(nm, def = "") {
      if (is.null(d[[nm]])) return(rep(def, nrow(d)))
      v <- as.character(d[[nm]])
      v[is.na(v)] <- def
      v
    }
    nnum <- function(nm) {
      if (is.null(d[[nm]])) return(rep(0L, nrow(d)))
      v <- suppressWarnings(as.integer(d[[nm]])); v[is.na(v)] <- 0L; v
    }
    out <- data.frame(
      ID = d$id,
      分类名称 = d$name,
      适用任务类型 = ifelse(g("applies_to") == "", "—", g("applies_to")),
      默认模型 = ifelse(g("model") == "", "CLI 默认", g("model")),
      Effort = ifelse(g("effort") == "", "—", g("effort")),
      最大轮数 = nnum("max_turns"),
      启用 = if (is.null(d$enabled)) rep("启用", nrow(d)) else ifelse(is.na(d$enabled) | d$enabled == 1, "启用", "停用"),
      常驻注入 = if (is.null(d$always_apply)) rep("否", nrow(d)) else ifelse(!is.na(d$always_apply) & d$always_apply == 1, "是", "否"),
      关联规则数 = ifelse(is.na(d$rule_count), 0, d$rule_count),
      排序 = ifelse(is.na(d$sort_order), 0, d$sort_order),
      stringsAsFactors = FALSE)
    hf_datatable(out, selection = "single", page_size = 20)
  })

  cat_selected_id <- reactive({
    idx <- input[[p("cat_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    d <- cat_df()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d$id[idx[1]]
  })

  # 分类编辑弹窗（新增 / 编辑共用）
  open_cat_modal <- function(id = NULL) {
    d <- cat_df()
    row <- if (is.null(id)) NULL else d[d$id == id, , drop = FALSE]
    val <- function(nm, def = "") {
      if (is.null(row) || nrow(row) == 0 || is.null(row[[nm]])) return(def)
      as.character(hf_or(row[[nm]][1], def))
    }
    numv <- function(nm, def = 0) {
      if (is.null(row) || nrow(row) == 0 || is.null(row[[nm]])) return(def)
      suppressWarnings(as.integer(hf_or(row[[nm]][1], def)))
    }
    model_choices <- tryCatch(hf_workbench_model_choices(), error = function(e) c("CLI 默认" = ""))
    showModal(modalDialog(
      title = if (is.null(id)) "新增规则分类" else paste0("编辑规则分类 #", id),
      textInput(p("cat_name"), "分类名称", value = val("name"), width = "100%",
                placeholder = "如：UI规则 / 架构规则"),
      textInput(p("cat_applies"), "适用任务类型（逗号分隔，或留空由路由表指定）",
                value = val("applies_to"), width = "100%",
                placeholder = "如：new_module,ui"),
      selectInput(p("cat_model"), "默认模型", choices = model_choices,
                  selected = if (nzchar(val("model"))) val("model") else "", width = "100%"),
      selectInput(p("cat_effort"), "推理强度",
                  choices = c("不限定" = "", "minimal" = "minimal", "low" = "low",
                              "medium" = "medium", "high" = "high", "xhigh" = "xhigh", "max" = "max"),
                  selected = val("effort"), width = "100%"),
      numericInput(p("cat_max_turns"), "最大轮数（0=不限定）", value = numv("max_turns"),
                   min = 0, width = "100%"),
      fluidRow(
        column(6, selectInput(p("cat_enabled"), "启用", choices = c("启用" = "1", "停用" = "0"),
                              selected = if (identical(val("enabled", "1"), "0")) "0" else "1", width = "100%")),
        column(6, selectInput(p("cat_always"), "常驻注入（所有任务都读）",
                              choices = c("否" = "0", "是" = "1"),
                              selected = if (identical(val("always_apply", "0"), "1")) "1" else "0", width = "100%"))
      ),
      textAreaInput(p("cat_desc"), "说明", value = val("description"), rows = 2, width = "100%"),
      footer = tagList(
        actionButton(p("cat_save_btn"), "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  }

  observeEvent(input[[p("cat_add_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    cat_editing_id(NULL)
    open_cat_modal(NULL)
  })

  observeEvent(input[[p("cat_edit_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    id <- cat_selected_id()
    if (is.null(id)) { showNotification("请先在分类列表选中一项", type = "warning"); return() }
    cat_editing_id(id)
    open_cat_modal(id)
  })

  observeEvent(input[[p("cat_save_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    name <- trimws(hf_or(input[[p("cat_name")]], ""))
    if (!nzchar(name)) { showNotification("请输入分类名称", type = "warning"); return() }
    id <- cat_editing_id()
    r <- if (is.null(id)) hf_rules_category_add(name) else hf_rules_category_update(id, name)
    if (!isTRUE(r$success)) { showNotification(r$message, type = "error"); return() }
    if (is.null(id)) {
      d <- hf_rules_categories_df()
      id <- if (!is.null(d) && nrow(d) > 0) d$id[d$name == name][1] else NULL
    }
    if (!is.null(id)) {
      mv <- hf_or(input[[p("cat_model")]], "")
      if (identical(mv, "__none__")) mv <- ""
      hf_rules_category_set_meta(id,
        applies_to = hf_or(input[[p("cat_applies")]], ""),
        model = mv,
        effort = hf_or(input[[p("cat_effort")]], ""),
        max_turns = suppressWarnings(as.integer(hf_or(input[[p("cat_max_turns")]], 0))),
        enabled = as.integer(hf_or(input[[p("cat_enabled")]], 1)),
        always_apply = as.integer(hf_or(input[[p("cat_always")]], 0)),
        description = hf_or(input[[p("cat_desc")]], ""))
    }
    # 导出给 CLI 的静态规则快照随分类变化重导
    try(hf_rules_export_md(), silent = TRUE)
    # 同步规则页的分类筛选 / 编辑下拉（共享 session，无需刷新整页）
    cats <- hf_rules_categories()
    tryCatch(updateSelectInput(session, "hf_rules_filter_cat",
      choices = c("全部分类" = "", cats)), error = function(e) NULL)
    tryCatch(updateSelectInput(session, "hf_rules_edit_cat", choices = cats),
      error = function(e) NULL)
    showNotification(r$message, type = "message")
    removeModal(); cat_editing_id(NULL)
    cat_refresh()
  })

  observeEvent(input[[p("cat_export_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    r <- tryCatch(hf_ai_export_all(), error = function(e) NULL)
    if (is.null(r)) { showNotification("导出失败，请检查数据表", type = "error"); return() }
    showNotification(sprintf("已导出规则 %d 个分类、记忆 %d 条；MEMORY.md / RULES.md 可用于移植",
      as.integer(hf_or(r$rules$categories, 0)), as.integer(hf_or(r$memory$items, 0))),
      type = "message")
    cat_refresh()
  })

  # ══════════════ 子 Tab 3：模型路由 ══════════════
  route_df <- reactive({
    trigger(); route_trigger()
    rs <- hf_model_routes()
    do.call(rbind, lapply(rs, function(x) data.frame(
      任务类型 = as.character(hf_or(x$task_type, "")),
      名称 = as.character(hf_or(x$label, "")),
      分类 = as.character(hf_or(x$categories, "")),
      模型 = as.character(hf_or(x$model, "")),
      Effort = as.character(hf_or(x$effort, "")),
      最大轮数 = suppressWarnings(as.integer(hf_or(x$max_turns, 0))),
      降级模型 = as.character(hf_or(x$fallback_model, "")),
      stringsAsFactors = FALSE)))
  })

  output[[p("route_status")]] <- renderUI({
    d <- route_df()
    tags$div(style = "font-size:12px;color:#555;margin:2px 0 8px;",
      tags$b("路由："), tags$span(style = "color:#1565c0;", paste0(nrow(d), " 条")),
      " · 存于 hf_config_kv(ai_model_routes) · 表单显式选择优先于本表")
  })

  output[[p("route_table")]] <- DT::renderDT({
    d <- route_df()
    if (is.null(d) || nrow(d) == 0) return(hf_datatable(NULL, empty = "no_data"))
    d$模型[is.na(d$模型) | d$模型 == ""] <- "CLI 默认"
    d$分类[is.na(d$分类) | d$分类 == ""] <- "—"
    d$Effort[is.na(d$Effort) | d$Effort == ""] <- "—"
    d$降级模型[is.na(d$降级模型) | d$降级模型 == ""] <- "—"
    hf_datatable(d, selection = "single", page_size = 20)
  })

  route_key <- reactive({
    idx <- input[[p("route_table_rows_selected")]]
    if (is.null(idx) || length(idx) == 0) return(NULL)
    d <- route_df()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    as.character(d$任务类型[idx[1]])
  })

  observeEvent(input[[p("route_edit_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    k <- route_key()
    if (is.null(k)) { showNotification("请先选中一条路由", type = "warning"); return() }
    route_editing_key(k)
    rt <- hf_model_route_get(k)
    model_choices <- tryCatch(hf_workbench_model_choices(), error = function(e) c("CLI 默认" = ""))
    showModal(modalDialog(
      title = paste0("编辑路由：", if (k %in% names(HF_TASK_TYPES)) as.character(HF_TASK_TYPES[[k]]) else k),
      selectInput(p("route_model"), "模型（空=CLI 默认）", choices = model_choices,
                  selected = if (nzchar(hf_or(rt$model, ""))) rt$model else "", width = "100%"),
      textInput(p("route_categories"), "读取分类（逗号分隔）",
                value = hf_or(rt$categories, ""), width = "100%"),
      selectInput(p("route_effort"), "推理强度",
                  choices = c("不限定" = "", "minimal" = "minimal", "low" = "low",
                              "medium" = "medium", "high" = "high", "xhigh" = "xhigh", "max" = "max"),
                  selected = hf_or(rt$effort, ""), width = "100%"),
      numericInput(p("route_max_turns"), "最大轮数", value = suppressWarnings(as.integer(hf_or(rt$max_turns, 0))),
                   min = 0, width = "100%"),
      selectInput(p("route_fallback"), "降级模型", choices = model_choices,
                  selected = hf_or(rt$fallback_model, ""), width = "100%"),
      footer = tagList(
        actionButton(p("route_save_btn"), "保存", class = "btn-primary"),
        modalButton("取消")
      ), size = "m", easyClose = TRUE
    ))
  })

  observeEvent(input[[p("route_save_btn")]], {
    req(rv$logged_in)
    k <- route_editing_key()
    if (is.null(k)) return()
    rs <- hf_model_routes()
    clean <- function(v) { v <- hf_or(v, ""); if (identical(v, "__none__")) "" else as.character(v) }
    for (i in seq_along(rs)) {
      if (!identical(as.character(hf_or(rs[[i]]$task_type, "")), k)) next
      rs[[i]]$model <- clean(input[[p("route_model")]])
      rs[[i]]$categories <- clean(input[[p("route_categories")]])
      rs[[i]]$effort <- clean(input[[p("route_effort")]])
      rs[[i]]$max_turns <- suppressWarnings(as.integer(hf_or(input[[p("route_max_turns")]], 0)))
      rs[[i]]$fallback_model <- clean(input[[p("route_fallback")]])
    }
    r <- hf_model_routes_save(rs)
    showNotification(if (isTRUE(r$success)) "路由已保存" else r$message,
                     type = if (isTRUE(r$success)) "message" else "error")
    removeModal(); route_editing_key(NULL); route_refresh()
  })

  observeEvent(input[[p("route_reset_btn")]], {
    req(rv$logged_in)
    if (!hf_perm_guard_admin(rv)) return()
    r <- hf_model_routes_save(HF_DEFAULT_ROUTES)
    showNotification(if (isTRUE(r$success)) "已恢复默认路由" else r$message,
                     type = if (isTRUE(r$success)) "message" else "error")
    route_refresh()
  })
}
