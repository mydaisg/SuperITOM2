# 方案模块 UI v3（方案 + 执行 双标签，执行支持项目选择/配置）

solution_ui <- function() {
  tagList(
    tabsetPanel(
      id = "sol_tabs", type = "pills", selected = "方案",
      # ── 标签1：方案 ──
      tabPanel("方案",
        div(
          fluidRow(
            column(3, div(style = "display:flex; gap:6px; align-items:center;",
              textInput("sol_search", NULL, width = "100%", placeholder = "搜索方案..."),
              actionButton("sol_search_btn", NULL, icon = icon("search"), class = "btn-sm btn-primary"))),
            column(2, selectInput("sol_filter_cat", NULL, choices = c("全部分类"=""), width = "100%")),
            column(2, actionButton("sol_create_btn", "新建方案", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;")),
            column(5, div(style = "text-align:right;", htmlOutput("sol_stats")))
          ),
          hr(),
          htmlOutput("sol_list")
        )
      ),
      # ── 标签2：执行 ──
      tabPanel("执行",
        uiOutput("exec_project_selector"),
        conditionalPanel("output.exec_has_project == '1'",
          tabsetPanel(id = "exec_task_tabs",
            tabPanel("培训计划", DTOutput("exec_tab_train")),
            tabPanel("试运行计划", DTOutput("exec_tab_pilot")),
            tabPanel("基础信息维护", DTOutput("exec_tab_basic")),
            tabPanel("用例",
              tabsetPanel(
                tabPanel("人力资源", DTOutput("exec_tab_test_hr")),
                tabPanel("行政管理", DTOutput("exec_tab_test_admin")),
                tabPanel("财务管理", DTOutput("exec_tab_test_fin")),
                tabPanel("IT管理", DTOutput("exec_tab_test_it"))
              )
            ),
            tabPanel("问题管理", DTOutput("exec_issue_table_v2")),
            tabPanel("配置管理", uiOutput("exec_config_ui"))
          ),
          div(style = "margin-top:8px;",
            actionButton("exec_task_add_btn", "添加任务", icon = icon("plus"), class = "btn-success btn-sm"))
        ),
        conditionalPanel("output.exec_has_project != '1'",
          div(style = "text-align:center; padding:60px; color:#999;", icon("folder-open", "fa-3x"), br(), br(), "选择或新建一个执行项目开始"))
      )
    )
  )
}
