# 流程模块 UI v3 — 简洁直白

process_ui <- function() {
  tagList(
    # JS 处理器（按钮点击 + 弹窗跳转）
    tags$script(HTML("
      $(document).on('click', '.process-publish-btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('process_publish_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.process-start-btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('process_start_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click', '.process-todo-btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('process_todo_click',
          {instance_id: String($(this).data('inst')), node_id: String($(this).data('node'))},
          {priority:'event'});
      });
      $(document).on('click', '.process-inst-log-btn', function(e) {
        e.preventDefault();
        Shiny.setInputValue('process_inst_log_click', $(this).data('inst'), {priority:'event'});
      });
    ")),

    fluidPage(
      # 标题区
      div(style = "text-align:center; margin:10px 0 20px;",
        h2(icon("project-diagram"), " 流程引擎"),
        p("将各模块串联为自动化业务流程", style = "color:#7f8c8d;")
      ),

      # 统计概览
      fluidRow(
        column(3, div(class="well well-sm", style="text-align:center; padding:8px;",
          h3(textOutput("proc_stat_total"), style="margin:0; color:#337ab7;"),
          p("流程定义", style="margin:3px 0 0; font-size:12px; color:#666;"))),
        column(3, div(class="well well-sm", style="text-align:center; padding:8px; background:#5cb85c; color:#fff;",
          h3(textOutput("proc_stat_running"), style="margin:0;"),
          p("运行中", style="margin:3px 0 0; font-size:12px;"))),
        column(3, div(class="well well-sm", style="text-align:center; padding:8px; background:#5bc0de; color:#fff;",
          h3(textOutput("proc_stat_completed"), style="margin:0;"),
          p("已完成", style="margin:3px 0 0; font-size:12px;"))),
        column(3, div(class="well well-sm", style="text-align:center; padding:8px;",
          h3(textOutput("proc_stat_todos"), style="margin:0; color:#f0ad4e;"),
          p("待处理", style="margin:3px 0 0; font-size:12px; color:#666;")))
      ),

      # 快捷入口
      fluidRow(
        column(12,
          div(style = "text-align:center; margin-bottom:15px;",
            actionButton("proc_create_demo", "🚀 一键体验流程", class = "btn-success btn-lg",
                         icon = icon("play"),
                         style = "padding:10px 30px; font-size:16px; font-weight:bold;"),
            span(style = "margin:0 15px; color:#bbb; font-size:18px;", "|"),
            actionButton("proc_create_def", "新建流程定义", class = "btn-primary",
                         icon = icon("plus"), style = "padding:10px 20px; font-size:14px;")
          )
        )
      ),

      # 子标签
      tabsetPanel(
        tabPanel("我的待办",
          br(),
          DT::DTOutput("proc_todo_table")
        ),
        tabPanel("流程定义",
          br(),
          fluidRow(
            column(2, selectInput("proc_def_status_filter", "状态",
                                  choices = c("全部"="", "草稿"="draft", "已发布"="published"), selected="")),
            column(1, div(style="margin-top:20px;", actionButton("proc_refresh_defs", "刷新", class="btn-info btn-sm")))
          ),
          br(),
          DT::DTOutput("proc_def_table")
        ),
        tabPanel("流程实例",
          br(),
          fluidRow(
            column(2, selectInput("proc_inst_status_filter", "状态",
                                  choices = c("全部"="", "运行中"="running", "已完成"="completed", "已终止"="terminated"),
                                  selected="running")),
            column(2, div(style="margin-top:20px;", actionButton("proc_refresh_insts", "刷新", class="btn-info btn-sm")))
          ),
          br(),
          DT::DTOutput("proc_instance_table")
        ),
        tabPanel("监控日志",
          br(),
          fluidRow(
            column(3, selectInput("proc_log_inst_select", "选择流程实例", choices=NULL)),
            column(2, div(style="margin-top:20px;", actionButton("proc_refresh_logs", "刷新日志", class="btn-info btn-sm")))
          ),
          br(),
          DT::DTOutput("proc_log_table")
        )
      )
    )
  )
}
