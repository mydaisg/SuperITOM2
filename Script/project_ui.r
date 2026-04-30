# 项目管理模块 - UI定义
# 三个子标签：项目列表（钻入）、项目详情（全部阶段）、任务管理（全部任务）

project_ui <- function() {
  cat("[project_ui] 项目模块UI正在构建...\n")
  fluidPage(
    # 全局事件代理：表格中按钮/链接点击 → Shiny input
    tags$script(HTML("
      $(document).on('click', '.proj-view-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('proj_view_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.proj-enter-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('proj_enter_click',
          {id: String($(this).data('id')), name: $(this).data('name')}, {priority:'event'});
      });
      $(document).on('click', '.phase-enter-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('phase_enter_click',
          {id: String($(this).data('id')), name: $(this).data('name')}, {priority:'event'});
      });
      $(document).on('click', '.phase-del-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('phase_del_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.wp-enter-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('wp_enter_click',
          {id: String($(this).data('id')), name: $(this).data('name')}, {priority:'event'});
      });
      $(document).on('click', '.wp-del-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('wp_del_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.task-view-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('task_view_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.task-to-wo-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('task_to_wo_click', $(this).data('id'), {priority:'event'});
      });
    ")),

    # 项目统计卡片行
    uiOutput("proj_stat_cards"),

    # 三个子标签页
    tabsetPanel(id = "proj_tabs",

      # ============ 子标签1：项目列表（钻入导航） ============
      tabPanel("项目列表", icon = icon("list"),
        br(),
        uiOutput("proj_breadcrumb"),
        DTOutput("proj_data_table"),
        hr(),
        uiOutput("proj_create_form")
      ),

      # ============ 子标签2：项目详情（全部阶段） ============
      tabPanel("项目详情", icon = icon("sitemap"),
        br(),
        fluidRow(
          column(4, uiOutput("pd_project_filter_ui")),
          column(3, uiOutput("pd_status_filter_ui")),
          column(2, div(style = "margin-top:2px;",
            actionButton("pd_refresh", "刷新", class = "btn-info btn-sm")))
        ),
        DTOutput("pd_phase_table"),
        hr(),
        uiOutput("pd_create_form")
      ),

      # ============ 子标签3：任务管理（全部任务） ============
      tabPanel("任务管理", icon = icon("tasks"),
        br(),
        fluidRow(
          column(3, uiOutput("tm_project_filter_ui")),
          column(2, uiOutput("tm_status_filter_ui")),
          column(2, uiOutput("tm_priority_filter_ui")),
          column(2, uiOutput("tm_assignee_filter_ui")),
          column(1, div(style = "margin-top:2px;",
            actionButton("tm_refresh", "刷新", class = "btn-info btn-sm")))
        ),
        DTOutput("tm_task_table")
      )
    )
  )
}
