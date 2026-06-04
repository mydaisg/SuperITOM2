# 性能监控 UI — 参照 note_ui 完全重写

sysmon_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.sysmon-del-btn',function(e){
        e.stopPropagation();
        if(confirm('确定移除？')) Shiny.setInputValue('sysmon_del',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.sysmon-check-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('sysmon_check',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.sysmon-hist-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('sysmon_history',$(this).data('id'),{priority:'event'});
      });
    ")),
    uiOutput("sysmon_main")
  )
}
