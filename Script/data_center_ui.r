# 数据中心模块 UI — 稳定版（HTML可视化）
data_center_ui <- function() {
  ns <- NS("data_center")
  tagList(
    tags$head(tags$style(HTML("
      .data-module-card {
        background: white; border-radius: 12px; padding: 14px 14px 10px; margin-bottom: 12px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06); border: 1px solid #eef0f4;
        transition: all 0.25s ease; cursor: pointer; position: relative; overflow: hidden;
      }
      .data-module-card::before {
        content: ''; position: absolute; top: 0; left: 0; right: 0; height: 4px;
      }
      .module-project::before { background: #2563eb; }
      .module-workorder::before { background: #e53e3e; }
      .module-inspection::before { background: #0d7d3a; }
      .module-network::before { background: #7c3aed; }
      .module-daily::before { background: #d97706; }
      .module-asset::before { background: #0891b2; }
      .module-note::before { background: #6c3bbf; }
      .module-duty::before { background: #ea580c; }
      .module-perf::before { background: #dc2626; }
      .data-module-card:hover { transform: translateY(-2px); box-shadow: 0 6px 16px rgba(0,0,0,0.12); }
      .data-module-card .module-icon { font-size: 24px; float: right; opacity: 0.12; margin-top: -2px; }
      .module-project .module-icon { color: #2563eb; }
      .module-workorder .module-icon { color: #e53e3e; }
      .module-inspection .module-icon { color: #0d7d3a; }
      .module-network .module-icon { color: #7c3aed; }
      .module-daily .module-icon { color: #d97706; }
      .module-asset .module-icon { color: #0891b2; }
      .module-note .module-icon { color: #6c3bbf; }
      .module-duty .module-icon { color: #ea580c; }
      .module-perf .module-icon { color: #dc2626; }
      .data-module-card .module-title { font-size: 13px; font-weight: 700; margin-bottom: 4px; color: #1a2236; }
      .dc-big-num { font-size: 32px; font-weight: 800; line-height: 1.1; }
      .dc-bar-track { height: 5px; background: #eef0f4; border-radius: 3px; margin: 5px 0; overflow: hidden; }
      .dc-bar-fill { height: 100%; border-radius: 3px; }
      .dc-dot { display: inline-block; width: 7px; height: 7px; border-radius: 50%; margin-right: 3px; vertical-align: middle; }
      .dc-tag { display: inline-block; padding: 1px 7px; border-radius: 8px; font-size: 10px; font-weight: 600; margin: 1px; }
      .dc-grid-3 { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 5px; text-align: center; }
      .dc-grid-3 .dc-gn { font-size: 18px; font-weight: 700; }
      .dc-grid-3 .dc-gl { font-size: 10px; color: #8899aa; }
    "))),
    tags$script(HTML("
      $(document).on('click', '.data-module-card', function(e) {
        var id = $(this).attr('id');
        if (id) { Shiny.setInputValue(id, {click: Date.now()}, {priority: 'event'}); }
      });
    ")),
    fluidPage(
      div(style="text-align:center;margin:16px 0;",
        h2(icon("database")," 数据中心"),
        p("模块数据概览", style="color:#7f8c8d;font-size:12px;")),
      fluidRow(
        column(4, div(class="data-module-card module-project", id=ns("card_project"),
          div(class="module-icon", icon("folder-open")),
          div(class="module-title","项目管理"), htmlOutput(ns("proj_viz")))),
        column(4, div(class="data-module-card module-workorder", id=ns("card_workorder"),
          div(class="module-icon", icon("clipboard-list")),
          div(class="module-title","工单管理"), htmlOutput(ns("wo_viz")))),
        column(4, div(class="data-module-card module-inspection", id=ns("card_inspection"),
          div(class="module-icon", icon("clipboard-check")),
          div(class="module-title","巡检管理"), htmlOutput(ns("insp_viz"))))
      ),
      fluidRow(
        column(4, div(class="data-module-card module-network", id=ns("card_network"),
          div(class="module-icon", icon("wifi")),
          div(class="module-title","网络测试"), htmlOutput(ns("nt_viz")))),
        column(4, div(class="data-module-card module-daily", id=ns("card_daily"),
          div(class="module-icon", icon("calendar-alt")),
          div(class="module-title","工作日报"), htmlOutput(ns("dr_viz")))),
        column(4, div(class="data-module-card module-asset", id=ns("card_asset"),
          div(class="module-icon", icon("laptop")),
          div(class="module-title","资产管理"), htmlOutput(ns("ast_viz"))))
      ),
      fluidRow(
        column(4, div(class="data-module-card module-note", id=ns("card_note"),
          div(class="module-icon", icon("sticky-note")),
          div(class="module-title","记事管理"), htmlOutput(ns("note_viz")))),
        column(4, div(class="data-module-card module-duty", id=ns("card_duty"),
          div(class="module-icon", icon("sitemap")),
          div(class="module-title","岗职矩阵"), htmlOutput(ns("duty_viz")))),
        column(4, div(class="data-module-card module-perf", id=ns("card_perf"),
          div(class="module-icon", icon("chart-bar")),
          div(class="module-title","绩效管理"), htmlOutput(ns("perf_viz"))))
      )
    )
  )
}
