# 绩效模块 UI
# 月绩效表（指标×员工矩阵）+ 工作清单匹配

performance_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.perf-match-btn',function(e){
        e.preventDefault();
        Shiny.setInputValue('perf_match_click',{
          emp_id: $(this).data('emp'),
          source_type: $(this).data('stype'),
          source_id: $(this).data('sid'),
          source_title: $(this).data('stitle')
        },{priority:'event'});
      });
      $(document).on('click','.perf-unmatch-btn',function(e){
        e.preventDefault();if(confirm('确定移除该匹配项？')){
          Shiny.setInputValue('perf_unmatch_click',$(this).data('id'),{priority:'event'});
        }
      });
    ")),
    fluidPage(
      div(style="text-align:center;margin:10px 0 5px;",
        h2(icon("chart-bar")," 绩效管理"),
        p(style="color:#7f8c8d;font-size:12px;","月度绩效评分：指标×员工矩阵 | 从工单/项目/巡检自动加载工作清单")),
      uiOutput("perf_stat_cards"),
      hr(),
      fluidRow(
        column(3,selectInput("perf_month","选择月份",choices=NULL,width="100%")),
        column(2,div(style="margin-top:25px;",actionButton("perf_create_sheet","新建月表",class="btn-primary btn-sm",icon=icon("plus")))),
        column(2,div(style="margin-top:25px;",actionButton("perf_refresh","刷新",class="btn-info btn-sm")))
      ),
      # 绩效矩阵表
      h4(icon("table")," 绩效评分表",style="margin-bottom:10px;"),
      div(style="overflow-x:auto;",DT::DTOutput("perf_matrix_table")),
      hr(),
      # 工作清单
      h4(icon("list")," 工作清单（本月工单/项目任务/巡检）",style="margin-bottom:10px;"),
      fluidRow(
        column(2,selectInput("perf_ws_filter","来源",choices=c("全部"="","工单"="工单","项目任务"="项目任务","巡检"="巡检"),width="100%")),
        column(3,uiOutput("perf_emp_filter_ui"))
      ), br(),
      DT::DTOutput("perf_work_source_table"),
      hr(),
      # 已匹配清单
      h4(icon("check-circle")," 已匹配到指标的工作项",style="margin-bottom:10px;"),
      DT::DTOutput("perf_matched_table")
    )
  )
}
