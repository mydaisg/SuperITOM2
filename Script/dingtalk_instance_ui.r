# 钉钉流程历史实例模块 — UI（作为「流程」标签页下的子标签）
# 功能：按钉钉分类（seq_no）+ 流程（flow_no）选择，查看可视化 HTML 和原始记录

dingtalk_instance_ui <- function() {
  tagList(
    tags$style(HTML("
      .dir-toolbar { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; margin-bottom: 12px; padding: 12px; background: rgba(255,255,255,0.05); border-radius: 10px; border: 1px solid rgba(255,255,255,0.1); }
      .dir-toolbar select, .dir-toolbar input { background: #1e2a44; color: #fff; border: 1px solid #2d3748; border-radius: 6px; padding: 4px 8px; font-size: 12px; }
      .dir-toolbar .badge { background: rgba(0,212,255,0.15); color: #00d4ff; padding: 2px 10px; border-radius: 10px; font-size: 12px; font-weight: 600; }
      .dir-summary { color: #8892b0; font-size: 12px; margin: 6px 0; }
    ")),
    fluidRow(
      column(12,
        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:12px;",
          h4(icon("archive"), " 钉钉旧流程数据", style = "margin:0;"),
          actionButton("dir_refresh", "刷新", icon = icon("sync"), class = "btn-xs btn-default")
        ),
        p(style = "color:#666; font-size:12px;",
          "钉钉历史流程原始记录存档与可视化（去钉钉化背景）。目录命名：全局序号-分类内序号-流程名。")
      )
    ),
    div(class = "dir-toolbar",
      tags$span("分类："),
      selectInput("dir_category", NULL,
        choices = c("请选择..." = ""),
        width = "200px"),
      tags$span("流程："),
      selectInput("dir_flow", NULL,
        choices = c("全部流程" = "all"),
        width = "200px"),
      tags$span("状态："),
      selectInput("dir_status_filter", NULL,
        choices = c("全部" = "all", "完成", "进行中"),
        width = "100px"),
      tags$span("结果："),
      selectInput("dir_result_filter", NULL,
        choices = c("全部" = "all", "同意", "拒绝"),
        width = "100px"),
      tags$span(class = "badge", uiOutput("dir_badge"))
    ),
    div(class = "dir-summary", uiOutput("dir_summary")),
    # 可视化 HTML 嵌入
    uiOutput("dir_html_block"),
    # 原始记录列表
    DT::DTOutput("dir_records_table")
  )
}