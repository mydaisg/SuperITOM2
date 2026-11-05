##################
# HungFo 环节 13 — 配置 (HF-CONFIG)
# 职责：子项目系统配置中心。可配置项：导航栏菜单的名称 / 名称全称 / 图标 / 位置 / 显示隐藏，
# 并在本页显示当前登录用户（原顶部导航栏显示已下沉至此）。
# 配置存 SQLite 表 hf_nav_config（默认值来自运行时注册表，覆盖值写库），
# 保存后导航栏由配置驱动、刷新即生效（session$reload + UI 函数化）。
##################

HF_MOD_CONFIG <- list(
  id = "hf20", key = "config", name = "配置", icon = "cog",
  desc = "子项目配置中心：可自定义导航栏菜单的名称、名称全称（悬停提示）、图标、位置与显示/隐藏，并显示当前登录用户。",
  api_desc = "nav() 取导航配置 · save() 保存单项 · reset() 重置默认",
  status = "scaffold", version = "v0.1"
)

# 导航配置数据层透传接口（供跨环节 hf_call("config", ...) 调用）
hf_config_api_nav <- function() hf_nav_config_list()
hf_config_api_save <- function(nav_key, name = NULL, icon = NULL, position = NULL,
                               visible = NULL, full_name = NULL) {
  hf_nav_config_save(nav_key, name, icon, position, visible, full_name)
}
hf_config_api_reset <- function() hf_nav_config_reset()

hf_register(HF_MOD_CONFIG, hf_make_api("config",
  nav = hf_config_api_nav,
  save = hf_config_api_save,
  reset = hf_config_api_reset
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

# ── 配置环节专属页面 UI ──
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
      # 工具栏：编辑选中 / 刷新 / 重置默认
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
    )
  )
}

# ── 配置环节专属 Server ──
hf_config_page_server <- function(input, output, session, rv, m, trigger) {
  p <- function(x) paste0("hf_config_", x)
  local_trigger <- reactiveVal(0)
  local_refresh <- function() local_trigger(local_trigger() + 1)

  observeEvent(input[[p("refresh_btn")]], local_refresh())

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

  # 重置默认：先弹确认框，避免误清空全部自定义配置
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
    if (is.null(df) || nrow(df) == 0) {
      return(hf_datatable(NULL, empty = "no_data"))
    }
    kind_label <- function(k) {
      c(overview = "总览", module = "环节", agent_monitor = "监控面板")[k]
    }
    # 名称全称兜底：库里为空时退回短名称
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

  # 编辑选中：弹窗回填当前值
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

  # 保存：写库后刷新导航栏（导航栏由配置驱动，session$reload 即刷新生效）
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
    if (isTRUE(r$success)) {
      removeModal()
      session$reload()
    }
  })
}
