# 流程监控数据模块 — UI（作为「流程」标签页下的子标签）

flow_monitor_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click', '.fmo-gen-btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('fmo_generate_click', {id: $(this).data('id'), ts: Date.now()}, {priority: 'event'});
      });
      $(document).on('click', '.fmo-view-btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('fmo_view_click', {id: $(this).data('id'), ts: Date.now()}, {priority: 'event'});
      });
    ")),
    tags$style(HTML("
      .fmo-batch-row { border:1px solid #e8e8e8; border-radius:6px; padding:10px 14px; margin-bottom:8px; background:#fff; }
      .fmo-batch-row:hover { border-color:#1890ff; }
    ")),
    fluidRow(
      column(12,
        h4(icon("database"), " 流程监控数据"),
        p(style = "color:#666; font-size:12px;",
          "将流程监控 Excel 明细数据导入 SQLite，可随时从数据库重新生成可视化看板。")
      )
    ),
    fluidRow(
      column(5,
        wellPanel(
          h5("导入 Excel 明细"),
          fileInput("fmo_file", NULL, width = "100%",
                    accept = c(".xlsx", ".xls", ".csv"),
                    buttonLabel = "选择文件", placeholder = "未选择文件"),
          actionButton("fmo_import", "导入到数据库", icon = icon("upload"),
                       class = "btn-primary", style = "width:100%;"),
          br(), br(),
          uiOutput("fmo_import_result")
        )
      ),
      column(7,
        div(style = "display:flex; gap:6px; align-items:center; margin-bottom:8px;",
          h5("数据批次", style = "margin:0;"),
          actionButton("fmo_refresh", "刷新", icon = icon("sync"), class = "btn-xs btn-default")),
        uiOutput("fmo_batch_list")
      )
    ),
    hr(),
    # 明细查看区（点击某批次后显示）
    uiOutput("fmo_detail")
  )
}
