# 流程模块 UI v5 — 普通用户友好版
# 隐藏JSON复杂性，模板化创建，后台保留完整自定义能力

process_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.process-publish-btn',function(e){e.preventDefault();Shiny.setInputValue('process_publish_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.process-start-btn',function(e){e.preventDefault();Shiny.setInputValue('process_start_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.process-todo-btn',function(e){e.preventDefault();Shiny.setInputValue('process_todo_click',{instance_id:String($(this).data('inst')),node_id:String($(this).data('node'))},{priority:'event'});});
      $(document).on('click','.proc-inst-term-btn',function(e){e.preventDefault();if(confirm('确定终止此流程？'))Shiny.setInputValue('proc_inst_term_click',$(this).data('inst'),{priority:'event'});});
      $(document).on('click','.proc-inst-suspend-btn',function(e){e.preventDefault();Shiny.setInputValue('proc_inst_suspend_click',$(this).data('inst'),{priority:'event'});});
      $(document).on('click','.proc-inst-resume-btn',function(e){e.preventDefault();Shiny.setInputValue('proc_inst_resume_click',$(this).data('inst'),{priority:'event'});});
      $(document).on('click','.proc-inst-log-btn',function(e){e.preventDefault();Shiny.setInputValue('process_inst_log_click',$(this).data('inst'),{priority:'event'});});
      $(document).on('change','#proc_template_select',function(){
        var tpl = $(this).val();
        if(tpl=='custom'){ $('#proc_json_editor_row').show(); } else { $('#proc_json_editor_row').hide(); }
      });
    ")),

    fluidPage(
      div(style="text-align:center;margin:10px 0 5px;",
        h2(icon("project-diagram")," 流程引擎"),
        p(style="color:#7f8c8d;font-size:12px;margin-bottom:8px;",
          "创建流程 → 启动运行 → 处理待办 → 完成")),

      # 统计卡片行
      uiOutput("proc_stat_cards"),

      # 顶部操作区
      fluidRow(column(12, div(style="text-align:center;margin:8px 0;",
        actionButton("proc_create_def","+ 新建流程",class="btn-primary",
          icon=icon("plus"),style="padding:6px 20px;font-size:14px;font-weight:bold;margin:0 4px;"),
        span(style="margin:0 10px;color:#ddd;","|"),
        actionButton("proc_demo_simple","简单审批",class="btn-success",
          icon=icon("play"),style="padding:6px 12px;font-size:12px;margin:0 2px;"),
        actionButton("proc_demo_condition","条件分支",class="btn-warning",
          icon=icon("code-branch"),style="padding:6px 12px;font-size:12px;margin:0 2px;"),
        actionButton("proc_demo_auto","自动工单",class="btn-info",
          icon=icon("bolt"),style="padding:6px 12px;font-size:12px;margin:0 2px;")
      ))),

      tabsetPanel(
        # ===== 我的待办 =====
        tabPanel("我的待办", br(),
          DT::DTOutput("proc_todo_table")),

        # ===== 流程定义（普通用户友好版） =====
        tabPanel("流程定义", br(),
          fluidRow(
            column(3,
              selectInput("proc_def_status_filter","状态",
                choices=c("全部"="","草稿"="draft","已发布"="published"),selected="")),
            column(2,
              div(style="margin-top:25px;",actionButton("proc_refresh_defs","刷新",class="btn-info btn-sm")))
          ), br(),
          DT::DTOutput("proc_def_table"),
          # 高级折叠区：JSON编辑（默认隐藏）
          br(),
          div(style="border-top:1px solid #eee;padding-top:10px;margin-top:10px;",
            tags$details(
              tags$summary(style="cursor:pointer;color:#7f8c8d;font-size:12px;",
                icon("cog")," 高级设置（JSON编辑，仅管理员使用）"),
              br(),
              fluidRow(
                column(8,
                  h5("节点配置",style="margin-top:0;"),
                  lapply(1:6, function(i) {
                    fluidRow(style="margin-bottom:4px;",
                      column(2, selectInput(paste0("proc_bn_type",i),"",width="100%",
                        choices=c("(空)"="","开始"="start","任务"="task","自动"="auto","条件"="condition","结束"="end"),
                        selected=if(i==1)"start"else if(i==6)"end"else"")),
                      column(3, textInput(paste0("proc_bn_label",i),"",width="100%",
                        placeholder=switch(i,"1"="开始","2"="审批","3"="通知","4"="条件","5"="任务","6"="结束"))),
                      column(2, numericInput(paste0("proc_bn_timeout",i),"",width="100%",value=0,min=0,step=1,label="超时(分)"))
                    )
                  }),
                  actionButton("proc_build_json","生成 JSON",class="btn-success btn-sm",icon=icon("magic"))
                ),
                column(4,
                  h5("说明",style="margin-top:0;"),
                  p(style="font-size:12px;color:#666;","1. 从上到下按顺序配置节点"),
                  p(style="font-size:12px;color:#666;","2. 至少需要「开始」和「结束」"),
                  p(style="font-size:12px;color:#666;","3. 点击「生成 JSON」后到新建弹窗使用"),
                  p(style="font-size:12px;color:#666;","4. 只有管理员需要用到此功能")
                )
              )
            )
          )
        ),

        # ===== 流程实例 =====
        tabPanel("流程实例", br(),
          fluidRow(
            column(2,selectInput("proc_inst_status_filter","状态",
              choices=c("全部"="","运行中"="running","已完成"="completed","已暂停"="suspended","已终止"="terminated"),selected="")),
            column(2,div(style="margin-top:20px;",actionButton("proc_refresh_insts","刷新",class="btn-info btn-sm")))
          ), br(),
          DT::DTOutput("proc_instance_table")),

        # ===== 监控日志 =====
        tabPanel("监控日志", br(),
          fluidRow(
            column(3,selectInput("proc_log_inst_select","选择实例",choices=NULL)),
            column(2,div(style="margin-top:20px;",actionButton("proc_refresh_logs","刷新",class="btn-info btn-sm")))
          ),
          tabsetPanel(
            tabPanel("运行日志", br(), DT::DTOutput("proc_log_table")),
            tabPanel("事件记录", br(), DT::DTOutput("proc_event_table")),
            tabPanel("上下文历史", br(), DT::DTOutput("proc_context_table")),
            tabPanel("版本历史", br(), DT::DTOutput("proc_version_table"))
          ))
      )
    )
  )
}
