# 流程数据可视化模块 — UI
# 作为「工具」模块下的子标签页（位于「记算」之后）
# 布局：左侧功能区（折叠）+ 右侧列表区（独立滚动，历史列表可收缩/展开）

flow_viz_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click', '.fvz-export-btn', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        Shiny.setInputValue('fvz_export', {id: String(id), ts: Date.now()}, {priority: 'event'});
      });
      // 历史列表收缩/展开
      $(document).on('click', '#fvz_toggle_hist', function() {
        var box = $('#fvz_hist_box');
        if (box.is(':visible')) {
          box.hide();
          $(this).html('<i class=\"fa fa-chevron-down\"></i> 展开历史');
        } else {
          box.show();
          $(this).html('<i class=\"fa fa-chevron-up\"></i> 收缩历史');
        }
      });
    ")),
    tags$style(HTML("
      .fvz-upload-box { border:2px dashed #b0bec5; border-radius:8px; padding:20px; text-align:center; background:#fafbfc; }
      .fvz-upload-box:hover { border-color:#337ab7; background:#f0f7ff; }
      .fvz-path-box { background:#f5f5f5; border:1px solid #e0e0e0; border-radius:6px; padding:10px 14px; word-break:break-all; font-family:monospace; font-size:12px; color:#555; }
      .fvz-link { display:inline-block; padding:6px 14px; background:#337ab7; color:#fff; border-radius:4px; text-decoration:none; font-size:13px; }
      .fvz-link:hover { background:#286090; color:#fff; text-decoration:none; }
      .fvz-hist-row { border-bottom:1px solid #f0f0f0; padding:8px 4px; display:flex; align-items:center; gap:10px; flex-wrap:wrap; }
      /* ---------- 左右独立滚动（参照测试模块） ---------- */
      .fvz-left-panel  { height: 78vh; overflow-y: auto; padding-right: 4px; }
      .fvz-right-panel { height: 78vh; overflow-y: auto; }
      /* ---------- 折叠区样式 ---------- */
      .fvz-details summary { cursor:pointer; padding:6px 8px; font-weight:600; font-size:13px; background:#f8f9fa; border-radius:4px; margin-bottom:4px; outline:none; }
      .fvz-details[open] summary { background:#e9ecef; }
      .fvz-details { border:1px solid #dee2e6; border-radius:6px; padding:6px 10px 10px; margin-bottom:8px; }
      .fvz-panel-btn .btn { font-size:12px; }
    ")),
    titlePanel("流程数据可视化"),
    br(),
    fluidRow(
      # ========== 左侧：功能区（独立滚动，折叠分区） ==========
      column(3,
        div(class = "fvz-left-panel",
          # ── 功能1：流程数据看板 ──
          tags$details(class = "fvz-details", open = NA,
            tags$summary("📊 流程数据看板"),
            tags$p(style = "font-size:12px; color:#666; margin-bottom:6px;",
              "上传流程数据 Excel（含 所属工作流/流程名称/当前节点/发起人/发起时间），生成可视化看板。"),
            fileInput("fvz_file", NULL, width = "100%",
                      accept = c(".xlsx", ".xls", ".csv"),
                      buttonLabel = "选择文件", placeholder = "未选择文件"),
            actionButton("fvz_generate", "生成看板", icon = icon("play-circle"),
                         class = "btn-primary", style = "width:100%;")
          ),
          # ── 功能2：流程日志效率图 ──
          tags$details(class = "fvz-details",
            tags$summary("⏱ 流程日志效率图"),
            tags$p(style = "font-size:12px; color:#666; margin-bottom:6px;",
              "上传单个流程实例的状态日志 Excel，解析各节点处理时长，生成流程效率图看板。"),
            fileInput("fvz_log_file", NULL, width = "100%",
                      accept = c(".xlsx", ".xls"),
                      buttonLabel = "选择文件", placeholder = "未选择文件"),
            actionButton("fvz_log_generate", "生成效率图", icon = icon("tachometer-alt"),
                         class = "btn-primary", style = "width:100%;"),
            tags$p(style = "font-size:11px; color:#999; margin-top:6px;",
              "关键操作（批准/提交/转办）耗时用于节点效率分析，抄送/查看不计入。")
          )
        )
      ),
      # ========== 右侧：列表区（独立滚动，含结果 + 历史） ==========
      column(9,
        div(class = "fvz-right-panel",
          # 结果展示区（两个功能的生成结果）
          div(style = "margin-bottom:12px;",
            h4(icon("file-alt"), " 生成结果", style = "margin-top:0;"),
            uiOutput("fvz_result"),
            uiOutput("fvz_log_result")
          ),
          tags$hr(),
          # 历史记录列表（可收缩/展开）
          div(style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
            h4(icon("history"), " 最近转换记录", style = "margin:0; flex:1;"),
            actionButton("fvz_refresh", "刷新", icon = icon("sync"), class = "btn-xs btn-default"),
            actionButton("fvz_toggle_hist", "收缩历史", icon = icon("chevron-up"),
                         class = "btn-xs btn-info")
          ),
          div(id = "fvz_hist_box",
            uiOutput("fvz_history")
          )
        )
      )
    )
  )
}
