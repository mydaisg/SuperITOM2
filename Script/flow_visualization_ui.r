# 流程数据可视化模块 — UI
# 作为「工具」模块下的子标签页（位于「记算」之后）

flow_viz_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click', '.fvz-export-btn', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        Shiny.setInputValue('fvz_export', {id: String(id), ts: Date.now()}, {priority: 'event'});
      });
    ")),
    tags$style(HTML("
      .fvz-upload-box { border:2px dashed #b0bec5; border-radius:8px; padding:20px; text-align:center; background:#fafbfc; }
      .fvz-upload-box:hover { border-color:#337ab7; background:#f0f7ff; }
      .fvz-path-box { background:#f5f5f5; border:1px solid #e0e0e0; border-radius:6px; padding:10px 14px; word-break:break-all; font-family:monospace; font-size:12px; color:#555; }
      .fvz-link { display:inline-block; padding:6px 14px; background:#337ab7; color:#fff; border-radius:4px; text-decoration:none; font-size:13px; }
      .fvz-link:hover { background:#286090; color:#fff; text-decoration:none; }
      .fvz-hist-row { border-bottom:1px solid #f0f0f0; padding:8px 4px; display:flex; align-items:center; gap:10px; flex-wrap:wrap; }
    ")),
    fluidRow(
      column(12,
        h4(icon("file-excel"), " 上传流程数据 Excel，自动生成可视化看板"),
        p(style = "color:#666; font-size:12px;",
          "要求 Excel 包含列：流程名称、当前节点、发起人、发起时间。完成标志以「当前节点」含“结束”判定。")
      )
    ),
    fluidRow(
      column(6,
        wellPanel(
          h5("1. 上传数据文件"),
          fileInput("fvz_file", NULL, width = "100%",
                    accept = c(".xlsx", ".xls", ".csv"),
                    buttonLabel = "选择文件", placeholder = "未选择文件"),
          div(style = "margin-top:4px;",
            actionButton("fvz_generate", "生成看板", icon = icon("play-circle"),
                         class = "btn-primary", style = "width:100%;")),
          br(),
          uiOutput("fvz_result")
        )
      ),
      column(6,
        wellPanel(
          h5("2. 最近转换记录"),
          div(style = "display:flex; gap:6px; margin-bottom:8px;",
            actionButton("fvz_refresh", "刷新", icon = icon("sync"), class = "btn-xs btn-default")),
          uiOutput("fvz_history")
        )
      )
    )
  )
}
