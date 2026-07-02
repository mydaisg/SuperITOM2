##################
# 开发日志模块 — 服务端
##################
dev_log_server <- function(input, output, session, rv) {

  dl_trigger <- reactiveVal(0)

  # 搜索词（防抖）
  dl_search_term <- reactiveVal("")
  observeEvent(input$dl_search_input, {
    req(rv$logged_in)
    invalidateLater(400, session)
    isolate({ dl_search_term(input$dl_search_input %||% "") })
  })

  # 头部：统计 + 筛选 + 搜索（同一行）
  output$dl_head <- renderUI({
    req(rv$logged_in)
    dl_trigger()
    s <- tryCatch(dev_log_get_stats(), error = function(e) list(total = 0L, today = 0L))
    tags$div(style = "display:flex; align-items:center; gap:10px; margin-bottom:6px; flex-wrap:wrap;",
      # 统计徽章
      tags$div(style = "display:flex; align-items:center; gap:4px; background:#f8f9fa; border:1px solid #e0e0e0; border-radius:12px; padding:3px 10px; font-size:13px;",
        tags$span(style = "color:#2563eb;", icon("file-alt")),
        tags$span(style = "font-weight:bold; color:#333;", s$total)
      ),
      tags$div(style = "display:flex; align-items:center; gap:4px; background:#f0fdf4; border:1px solid #dcfce7; border-radius:12px; padding:3px 10px; font-size:13px;",
        tags$span(style = "color:#0d7d3a;", icon("plus")),
        tags$span(style = "font-weight:bold; color:#333;", s$today)
      ),
      # 分隔符
      tags$span(style = "color:#ddd; font-size:18px;", "|"),
      # 模块筛选
      tags$style("#dl_filter_module { height:26px; font-size:12px; padding:2px 6px; width:130px; }"),
      selectInput("dl_filter_module", NULL, choices = c("全部模块" = ""), width = "130px"),
      # 搜索框
      tags$style("#dl_search_input { height:26px; font-size:12px; padding:2px 6px; width:180px; }"),
      textInput("dl_search_input", NULL, width = "180px", placeholder = "搜索日志…"),
      # 刷新按钮
      actionButton("dl_refresh", NULL, icon = icon("sync"), class = "btn-xs btn-primary", style = "height:26px;")
    )
  })

  # 列表（两列，带 log_no）
  output$dl_list <- renderUI({
    req(rv$logged_in)
    dl_trigger()
    mf <- if (is.null(input$dl_filter_module) || input$dl_filter_module == "") NULL else input$dl_filter_module
    kw <- dl_search_term()
    sk <- if (is.null(kw) || nchar(trimws(kw)) == 0) NULL else trimws(kw)
    logs <- tryCatch(dev_log_get_all(mf, NULL, NULL, sk), error = function(e) data.frame())

    if (nrow(logs) == 0) {
      return(tags$div(style = "text-align:center; color:#999; padding:40px; font-size:14px;",
        if (is.null(sk)) "暂无开发日志" else sprintf("未找到「%s」的相关日志", sk)))
    }

    logs$date <- substr(logs$created_at, 1, 10)
    dates <- sort(unique(logs$date), decreasing = TRUE)

    items_list <- list()
    for (d in dates) {
      grp <- logs[logs$date == d, , drop = FALSE]
      cards <- lapply(seq_len(nrow(grp)), function(i) {
        r <- grp[i, , drop = FALSE]
        time_str <- substr(r$created_at, 12, 16)
        log_no_str <- if (!is.null(r$log_no) && !is.na(r$log_no) && nchar(r$log_no) > 0) r$log_no else ""

        parts <- list(
          # 标题行：编号 + 模块标签 + 标题 + 时间（同行）
          tags$div(style = "display:flex; align-items:flex-start; gap:6px; margin-bottom:6px;",
            if (nchar(log_no_str) > 0) tags$span(style = "font-size:10px; color:#888; font-family:Consolas,monospace; white-space:nowrap; margin-top:3px; flex-shrink:0;", log_no_str),
            tags$span(style = "display:inline-block; padding:1px 7px; border-radius:10px; font-size:11px; background:#ede9fe; color:#5b21b6; white-space:nowrap; flex-shrink:0;", r$module %||% "全局"),
            tags$div(style = "flex:1; min-width:0;",
              tags$div(style = "font-size:15px; font-weight:bold; color:#222; line-height:1.3;",
                r$title %||% "",
                tags$span(style = "font-size:11px; color:#bbb; font-weight:normal; margin-left:6px; white-space:nowrap;", time_str)
              )
            )
          )
        )
        if (!is.na(r$requirement) && nchar(r$requirement) > 0) {
          parts <- c(parts, list(
            tags$div(style = "margin-top:6px; padding:8px 10px; background:#fff8e1; border-left:3px solid #f59e0b; border-radius:0 4px 4px 0; font-size:13px;",
              tags$div(style = "font-size:11px; color:#b45309; font-weight:bold; margin-bottom:3px;", icon("clipboard"), " 需求"),
              tags$div(style = "white-space:pre-wrap; line-height:1.5;", r$requirement))
          ))
        }
        if (!is.na(r$solution) && nchar(r$solution) > 0) {
          parts <- c(parts, list(
            tags$div(style = "margin-top:6px; padding:8px 10px; background:#e8f5e9; border-left:3px solid #4caf50; border-radius:0 4px 4px 0; font-size:13px;",
              tags$div(style = "font-size:11px; color:#2e7d32; font-weight:bold; margin-bottom:3px;", icon("lightbulb"), " 方案"),
              tags$div(style = "white-space:pre-wrap; line-height:1.5;", r$solution))
          ))
        }
        if (!is.na(r$result) && nchar(r$result) > 0) {
          en_tag <- if (!is.null(r$result_en) && !is.na(r$result_en) && nchar(r$result_en) > 0) {
            tags$span(style = "display:inline-block; background:#0d47a1; color:#fff; padding:0 6px; border-radius:3px; font-size:11px; font-family:Consolas,monospace; margin-right:6px;", r$result_en)
          } else ""
          parts <- c(parts, list(
            tags$div(style = "margin-top:6px; padding:8px 10px; background:#e3f2fd; border-left:3px solid #2196f3; border-radius:0 4px 4px 0; font-size:13px;",
              tags$div(style = "font-size:11px; color:#1565c0; font-weight:bold; margin-bottom:3px;", icon("check-circle"), " 结果"),
              tags$div(style = "white-space:pre-wrap; line-height:1.5;", en_tag, r$result))
          ))
        }
        if (!is.na(r$files_changed) && nchar(r$files_changed) > 0) {
          parts <- c(parts, list(tags$div(style = "font-size:12px; color:#666; margin-top:6px;", icon("folder"), " ", r$files_changed)))
        }
        tags$div(style = "background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:12px; height:100%; box-sizing:border-box;", parts)
      })
      items_list <- c(items_list, list(
        tags$div(style = "font-size:14px; font-weight:bold; color:#555; margin:12px 0 8px; grid-column:1/-1;", d, sprintf(" · %d 条", nrow(grp))),
        cards
      ))
    }
    tags$div(style = "display:grid; grid-template-columns:1fr 1fr; gap:12px; align-items:start;", items_list)
  })

  # 模块筛选下拉
  observe({
    req(rv$logged_in)
    mods <- tryCatch(dev_log_get_modules(), error = function(e) character(0))
    if (length(mods) == 0) mods <- "全部"
    tryCatch(
      updateSelectInput(session, "dl_filter_module", choices = c("全部模块" = "", setNames(mods, mods))),
      error = function(e) NULL
    )
  })

  # 搜索回车触发
  observeEvent(input$dl_search_key, {
    req(rv$logged_in)
    inp <- input$dl_search_key
    if (!is.null(inp) && nchar(inp) > 0) {
      dl_search_term(inp)
      dl_trigger(dl_trigger() + 1)
    }
  })

  # 筛选变更自动刷新
  observeEvent(input$dl_filter_module, {
    dl_trigger(dl_trigger() + 1)
  }, ignoreInit = TRUE)

  # 刷新按钮 / 搜索框回车触发
  observeEvent(input$dl_refresh, {
    kw <- input$dl_search_input %||% ""
    dl_search_term(kw)
    dl_trigger(dl_trigger() + 1)
  })

  # 首次加载
  observe({
    req(rv$logged_in)
    if (isolate(dl_trigger()) == 0) dl_trigger(1)
  })
}
