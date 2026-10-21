# OKR 模块 UI

okr_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.okr-view-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('okr_view_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click','.okr-obj-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('okr_obj_edit_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click','.okr-obj-del-btn',function(e){
        e.stopPropagation();
        if(confirm('确定删除该目标及其全部关键结果？')){
          Shiny.setInputValue('okr_obj_del_click', $(this).data('id'), {priority:'event'});
        }
      });
      $(document).on('click','.okr-kr-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('okr_kr_edit_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click','.okr-kr-checkin-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('okr_kr_checkin_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click','.okr-kr-sync-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('okr_kr_sync_click', $(this).data('id'), {priority:'event'});
      });
      $(document).on('click','.okr-kr-del-btn',function(e){
        e.stopPropagation();
        if(confirm('确定删除该关键结果？')){
          Shiny.setInputValue('okr_kr_del_click', $(this).data('id'), {priority:'event'});
        }
      });
    ")),
    uiOutput("okr_header"),
    uiOutput("okr_main")
  )
}
