# 绩效模块 UI — 参照 note_ui/sysmon_ui 重写

performance_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.perf-match-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('perf_match_click',{
          emp_id: $(this).data('emp'),
          source_type: $(this).data('stype'),
          source_id: $(this).data('sid'),
          source_title: $(this).data('stitle')
        },{priority:'event'});
      });
      $(document).on('click','.perf-unmatch-btn',function(e){
        e.stopPropagation();
        if(confirm('确定移除该匹配项？')){
          Shiny.setInputValue('perf_unmatch_click',$(this).data('id'),{priority:'event'});
        }
      });
    ")),
    # ★ header 先渲染（创建 perf_month 选择器），不依赖 perf_month
    uiOutput("perf_header"),
    # ★ main 后渲染（依赖 perf_month），此时 perf_month 已存在
    uiOutput("perf_main")
  )
}
