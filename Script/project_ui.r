# 项目管理模块 - UI定义
# 三个子标签：项目列表（钻入）、项目详情（全部阶段）、任务管理（全部任务）

project_ui <- function() {
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
      $(document).on('click', '.pd-phase-enter-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('pd_phase_enter_click',
          {phase_id: String($(this).data('phase-id')), phase_name: $(this).data('phase-name'),
           project_id: String($(this).data('project-id')), project_name: $(this).data('project-name')}, {priority:'event'});
      });
      $(document).on('click', '.task-view-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('task_view_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.task-to-wo-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('task_to_wo_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.task-fav-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        var el = $(this);
        var taskId = el.data('id');
        // 立即切换视觉状态
        if (el.html().charCodeAt(0) === 9733) {
          el.html('&#9734;').css('color', '#ccc');
        } else {
          el.html('&#9733;').css('color', '#f0ad4e');
        }
        Shiny.setInputValue('task_fav_click', taskId, {priority:'event'});
      });
      $(document).on('click', '.task-importance-btn', function(e) {
        e.stopPropagation(); e.preventDefault();
        var el = $(this);
        var taskId = el.data('id');
        var level = el.data('level');
        // 找到同一任务的所有旗子（同一行内）
        var row = el.closest('td');
        var flags = row.find('.task-importance-btn');
        // 判断是否点击的是当前已激活的最高级（再点清零）
        var currentLevel = 0;
        flags.each(function() {
          if ($(this).css('color') === 'rgb(217, 83, 79)') currentLevel = $(this).data('level');
        });
        var newLevel = (level === currentLevel) ? 0 : level;
        // 立即更新视觉
        flags.each(function() {
          var fl = $(this).data('level');
          $(this).css('color', fl <= newLevel ? '#d9534f' : '#ddd');
        });
        Shiny.setInputValue('task_importance_click', {id: taskId, level: newLevel}, {priority:'event'});
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
          column(2, uiOutput("tm_project_filter_ui")),
          column(2, uiOutput("tm_status_filter_ui")),
          column(2, uiOutput("tm_priority_filter_ui")),
          column(2, uiOutput("tm_assignee_filter_ui")),
          column(2, div(style = "margin-top:25px;",
            checkboxInput("tm_fav_only", "只看收藏", value = FALSE))),
          column(1, div(style = "margin-top:2px;",
            actionButton("tm_refresh", "刷新", class = "btn-info btn-sm")))
        ),
        DTOutput("tm_task_table"),
        hr(),
        uiOutput("tm_create_form")
      ),

      # ============ 子标签4：甘特图 ============
      tabPanel("甘特图", icon = icon("chart-gantt"),
        br(),
        fluidRow(
          column(4, uiOutput("gantt_project_filter_ui")),
          column(2, div(style = "margin-top:2px;",
            actionButton("gantt_refresh", "刷新", class = "btn-info btn-sm")))
        ),
        br(),
        uiOutput("gantt_chart_ui")
      )
    )
  )
}
