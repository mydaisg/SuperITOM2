# 方案模块 UI v2（方案 + 执行 双标签）

solution_ui <- function() {
  tagList(
    tabsetPanel(
      id = "sol_tabs", type = "pills",
      # ── 标签1：方案 ──
      tabPanel("方案",
        fluidPage(
          titlePanel(""),
          fluidRow(
            column(3,
              div(style = "display:flex; gap:6px; align-items:center;",
                textInput("sol_search", NULL, width = "100%", placeholder = "搜索方案..."),
                actionButton("sol_search_btn", NULL, icon = icon("search"), class = "btn-sm btn-primary")
              )
            ),
            column(2, selectInput("sol_filter_cat", NULL, choices = c("全部分类"=""), width = "100%")),
            column(2,
              actionButton("sol_create_btn", "新建方案", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;")
            ),
            column(5, div(style = "text-align:right;", uiOutput("sol_stats")))
          ),
          hr(),
          uiOutput("sol_list")
        )
      ),
      # ── 标签2：执行 ──
      tabPanel("执行",
        tags$style(HTML("
          .exec-tabs { margin-top:4px; }
          .exec-stat { display:flex; gap:12px; margin-bottom:10px; flex-wrap:wrap; }
          .exec-stat-item { padding:8px 14px; border-radius:6px; text-align:center; min-width:100px; }
          .exec-stat-item .n { font-size:20px; font-weight:bold; }
          .exec-stat-item .l { font-size:11px; color:#666; }
        ")),
        tabsetPanel(id = "exec_subtabs",
          tabPanel("培训计划",
            uiOutput("exec_train_stats"),
            DTOutput("exec_train_table")
          ),
          tabPanel("试运行计划",
            uiOutput("exec_pilot_stats"),
            DTOutput("exec_pilot_table")
          ),
          tabPanel("基础资料",
            uiOutput("exec_basic_stats"),
            DTOutput("exec_basic_table")
          ),
          tabPanel("HR测试用例",
            uiOutput("exec_hr_stats"),
            DTOutput("exec_hr_table")
          ),
          tabPanel("行政测试用例",
            uiOutput("exec_admin_stats"),
            DTOutput("exec_admin_table")
          ),
          tabPanel("财务测试用例",
            uiOutput("exec_fin_stats"),
            DTOutput("exec_fin_table")
          ),
          tabPanel("IT测试用例",
            uiOutput("exec_it_stats"),
            DTOutput("exec_it_table")
          ),
          tabPanel("问题反馈",
            uiOutput("exec_issue_stats"),
            DTOutput("exec_issue_table")
          )
        )
      )
    )
  )
}
