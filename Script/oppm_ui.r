# ================================================================
# OPPM 一页纸项目管理 - UI 定义
# 作为项目模块的子标签页（tabPanel），由 project_ui() 的 tabsetPanel 引用
# ================================================================

oppm_ui <- function() {
  tagList(
    # 全局事件代理：进度单元格 / RACI 单元格 / 删除按钮 → Shiny input
    tags$script(HTML("
      $(document).on('click', '.oppm-prog-cell', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('oppm_prog_click',
          {task_id: String($(this).data('task')), week: String($(this).data('week')),
           mark: $(this).data('mark'), color: $(this).data('color')}, {priority:'event'});
      });
      $(document).on('click', '.oppm-raci-cell', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('oppm_raci_click',
          {task_id: String($(this).data('task')), role_id: String($(this).data('role')),
           code: $(this).data('code')}, {priority:'event'});
      });
      $(document).on('click', '.oppm-task-del', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('oppm_task_del_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.oppm-role-del', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('oppm_role_del_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.oppm-task-edit', function(e) {
        e.stopPropagation(); e.preventDefault();
        Shiny.setInputValue('oppm_task_edit_click',
          {id: String($(this).data('id')), name: $(this).data('name'), seq: $(this).data('seq')}, {priority:'event'});
      });
    ")),

    # 顶部工具栏
    fluidRow(
      column(3, uiOutput("oppm_plan_filter_ui")),
      column(2, div(style = "margin-top:25px;", actionButton("oppm_new_plan", "新建 OPPM", icon = icon("plus"), class = "btn-primary btn-sm"))),
      column(2, div(style = "margin-top:25px;", actionButton("oppm_edit_plan", "编辑信息", icon = icon("edit"), class = "btn-info btn-sm"))),
      column(2, div(style = "margin-top:25px;", actionButton("oppm_refresh", "刷新", icon = icon("sync"), class = "btn-default btn-sm")))
    ),
    hr(style = "margin-top:10px; margin-bottom:10px;"),

    # 一页纸视图
    uiOutput("oppm_chart_ui"),

    # 管理面板（角色 + 任务）
    uiOutput("oppm_manage_ui")
  )
}
