# 需求模块 UI — 3 个子标签：需求列表 / 进度 / 甘特图
requirement_ui <- function() {
  tagList(
    tags$style(HTML("
      .req-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .req-cap h3 { margin:0; color:#fff; }
      .req-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
      .req-card { background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:14px; margin-bottom:10px; cursor:pointer; transition:all .15s; }
      .req-card:hover { border-color:#337ab7; box-shadow:0 2px 8px rgba(0,0,0,.08); }
      .req-card.active { border-color:#337ab7; background:#f5f9ff; }
      .req-badge { display:inline-block; padding:1px 10px; border-radius:10px; font-size:11px; color:#fff; margin-left:8px; }
      .prog-dept { font-weight:700; font-size:14px; color:#0f2b5c; }
      .prog-person { color:#555; font-size:13px; }
      .prog-status { display:inline-block; padding:1px 10px; border-radius:10px; font-size:11px; color:#fff; margin-left:6px; }
      .prog-date { font-size:11px; color:#999; margin-left:6px; }
      .prog-content { font-size:13px; color:#444; margin-top:4px; padding-left:16px; }
      /* 甘特图（简单版·时长数据条） */
      .gt-wrap{background:#fff;padding:6px;}
      .gt-title{text-align:center;margin-bottom:2px;}
      .gt-title h2{font-size:17px;font-weight:700;color:#0f2b5c;margin:0;}
      .gt-legend{text-align:center;font-size:10.5px;color:#556;margin-bottom:6px;}
      .gt-legend .li{display:inline-block;margin:0 6px;}
      .gt-legend .dot{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:3px;vertical-align:-1px;}
      .sb-table{border:1px solid #e2e8f0;border-radius:6px;overflow:hidden;background:#fff;}
      .sb-row{display:flex;align-items:center;border-bottom:1px solid #f0f4f8;min-height:20px;}
      .sb-name{width:300px;min-width:300px;padding:2px 10px;font-size:11px;color:#334155;box-sizing:border-box;white-space:nowrap;overflow:hidden;}
      .sb-range{width:96px;min-width:96px;font-family:Consolas,monospace;font-size:10px;color:#7b8794;box-sizing:border-box;padding:2px 6px;text-align:center;}
      .sb-barwrap{flex:1;display:flex;align-items:center;gap:6px;padding-right:10px;min-width:0;}
      .sb-bar{height:9px;border-radius:2px;opacity:.88;min-width:2px;}
      .sb-phasebar{height:12px;opacity:.95;border-radius:3px;}
      .sb-days{font-size:10px;color:#7b8794;white-space:nowrap;}
      .sb-phase{font-size:11.5px;}
      .sb-phase .sb-name{font-weight:700;}
      .ph-name-block{display:inline-block;padding:1px 8px;border-radius:6px;font-size:11.5px;font-weight:700;color:#fff;white-space:nowrap;}
      .sb-range-tag{display:inline-block;margin-left:6px;font-size:10px;font-weight:400;color:#9aa3af;}
      .sb-day-block{display:inline-block;margin-left:8px;padding:1px 8px;border-radius:8px;font-size:10px;font-weight:600;color:#fff;white-space:nowrap;}
    ")),
    fluidRow(
      column(12,
        div(class = "req-cap", style = "background:linear-gradient(135deg,#0f2b5c,#2563eb);",
          h3(icon("clipboard-list"), " 需求管理"),
          p("需求收集 · 进度跟踪 · 甘特图（时长数据条）")
        )
      )
    ),
    tabsetPanel(
      id = "req_tabs", type = "pills",

      # ── Tab1：需求列表 ──
      tabPanel("需求", icon = icon("list"),
        br(),
        fluidRow(
          column(3, selectInput("req_filter_status", "状态",
            choices = c("全部状态" = "", requirement_status_choices()), width = "100%")),
          column(4, textInput("req_search", NULL, width = "100%", placeholder = "搜索标题/描述...")),
          column(2, actionButton("req_search_btn", "搜索", icon = icon("search"), class = "btn-primary btn-sm")),
          column(3, actionButton("req_add_btn", "新建需求", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;"))
        ),
        br(),
        uiOutput("req_list")
      ),

      # ── Tab2：进度 ──
      tabPanel("进度", icon = icon("tasks"),
        br(),
        uiOutput("req_progress_ui")
      ),

      # ── Tab3：甘特图 ──
      tabPanel("甘特图", icon = icon("chart-gantt"),
        br(),
        fluidRow(
          column(4, uiOutput("req_gantt_filter_ui")),
          column(2, actionButton("req_gantt_refresh", "刷新", class = "btn-info btn-sm", style = "margin-top:2px;"))
        ),
        br(),
        uiOutput("req_gantt_chart_ui")
      )
    )
  )
}
