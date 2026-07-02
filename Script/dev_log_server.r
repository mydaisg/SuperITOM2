##################
# 开发日志模块 — 服务端
##################
dev_log_server <- function(input, output, session, rv) {
  
  # 触发器
  dl_trigger <- reactiveVal(0)
  
  # 统计概览
  output$dl_stats <- renderUI({
    dl_trigger()
    req(rv$logged_in)
    s <- dev_log_get_stats()
    tags$div(style = "display:flex; gap:10px; margin-bottom:12px;",
      tags$div(class = "note-stat-box", style = "flex:1;",
        tags$div(class = "stat-num", style = "color:#2563eb;", s$total),
        tags$div(class = "stat-lbl", "总记录")
      ),
      tags$div(class = "note-stat-box", style = "flex:1;",
        tags$div(class = "stat-num", style = "color:#0d7d3a;", s$today),
        tags$div(class = "stat-lbl", "今日新增")
      )
    )
  })
  
  # 初始化模块筛选
  observe({
    req(rv$logged_in)
    modules <- dev_log_get_modules()
    if (length(modules) == 0) modules <- "全部"
    updateSelectInput(session, "dl_filter_module",
      choices = c("全部模块" = "", setNames(modules, modules)))
  })
  
  # 列表
  output$dl_list <- renderUI({
    dl_trigger()
    req(rv$logged_in)
    
    module_filter <- if (is.null(input$dl_filter_module) || input$dl_filter_module == "") NULL else input$dl_filter_module
    logs <- dev_log_get_all(module_filter, NULL, NULL)
    
    if (nrow(logs) == 0) {
      return(tags$div(style = "text-align:center; color:#999; padding:40px;", "暂无开发日志"))
    }
    
    # 按日期分组
    logs$date <- substr(logs$created_at, 1, 10)
    dates <- unique(logs$date)
    
    date_groups <- lapply(dates, function(d) {
      grp <- logs[logs$date == d, ]
      items <- lapply(1:nrow(grp), function(i) {
        r <- grp[i, ]
        tags$div(style = "background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:14px; margin-bottom:8px;",
          tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
            tags$div(
              tags$span(style = sprintf("display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; background:#ede9fe; color:#5b21b6; margin-right:8px;"), r$module %||% "全局"),
              tags$b(style = "font-size:14px; color:#1a1a2e;", r$title %||% "")
            ),
            tags$span(style = "font-size:11px; color:#999;", substr(r$created_at, 1, 16))
          ),
          # 需求
          if (!is.na(r$requirement) && nchar(r$requirement) > 0) {
            tags$div(style = "margin-bottom:6px; padding:8px 10px; background:#fff8e1; border-left:2px solid #f59e0b; border-radius:0 4px 4px 0;",
              tags$div(style = "font-size:10px; color:#b45309; font-weight:bold; margin-bottom:3px;", "📝 需求"),
              tags$div(style = "font-size:12px; color:#333; white-space:pre-wrap;", HTML(gsub("\n", "<br>", r$requirement)))
            )
          },
          # 方案
          if (!is.na(r$solution) && nchar(r$solution) > 0) {
            tags$div(style = "margin-bottom:6px; padding:8px 10px; background:#e8f5e9; border-left:2px solid #4caf50; border-radius:0 4px 4px 0;",
              tags$div(style = "font-size:10px; color:#2e7d32; font-weight:bold; margin-bottom:3px;", "💡 方案"),
              tags$div(style = "font-size:12px; color:#333; white-space:pre-wrap;", HTML(gsub("\n", "<br>", r$solution)))
            )
          },
          # 结果
          if (!is.na(r$result) && nchar(r$result) > 0) {
            tags$div(style = "margin-bottom:6px; padding:8px 10px; background:#e3f2fd; border-left:2px solid #2196f3; border-radius:0 4px 4px 0;",
              tags$div(style = "font-size:10px; color:#1565c0; font-weight:bold; margin-bottom:3px;", "✅ 结果"),
              tags$div(style = "font-size:12px; color:#333; white-space:pre-wrap;", HTML(gsub("\n", "<br>", r$result)))
            )
          },
          # 代码
          if (!is.na(r$code_snippet) && nchar(r$code_snippet) > 0) {
            tags$div(style = "margin-bottom:6px; padding:8px 10px; background:#1e1e1e; border-radius:4px;",
              tags$div(style = "font-size:10px; color:#888; font-weight:bold; margin-bottom:3px;", "💻 关键代码"),
              tags$pre(style = "font-size:11px; color:#d4d4d4; margin:0; overflow-x:auto; font-family:'Consolas','Monaco',monospace;",
                tags$code(r$code_snippet))
            )
          },
          # 变更文件
          if (!is.na(r$files_changed) && nchar(r$files_changed) > 0) {
            tags$div(style = "font-size:10px; color:#999; margin-top:4px;",
              "📁 ", gsub(",", ", ", r$files_changed))
          },
          # 操作人
          tags$div(style = "font-size:10px; color:#bbb; margin-top:6px;",
            "记录人: ", r$created_by %||% "系统")
        )
      })
      tagList(
        tags$div(style = "font-size:13px; font-weight:bold; color:#555; margin-bottom:6px; margin-top:16px;",
          d, " · ", sprintf("%d 条", nrow(grp))),
        do.call(tagList, items)
      )
    })
    
    do.call(tagList, date_groups)
  })
  
  # 刷新
  observeEvent(input$dl_refresh, {
    dl_trigger(dl_trigger() + 1)
  })
  
  # 初始加载
  observe({
    req(rv$logged_in)
    dl_trigger(dl_trigger() + 1)
  })
}
