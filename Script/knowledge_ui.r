##################
# 知识库模块 — UI（方案模块「知识」Tab）
##################

knowledge_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.kb-view-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_view_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kb-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kb-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_del_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kb-rechunk-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_rechunk_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kb-goto-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_goto_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kb-tag-chip',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_tag_click',$(this).data('tag'),{priority:'event'});
      });
      $(document).on('click','.kb-cat-item',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kb_cat_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kbcat-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kbcat_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.kbcat-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('kbcat_del_click',$(this).data('id'),{priority:'event'});
      });
    ")),
    tags$style(HTML("
      .kb-cat-item { display:block; padding:6px 10px; border-radius:6px; cursor:pointer; color:#333; margin-bottom:2px; }
      .kb-cat-item:hover { background:#e3f2fd; }
      .kb-cat-item.active { background:#1976d2; color:#fff; }
      .kb-tag-chip { display:inline-block; background:#ede7f6; color:#5e35b1; border-radius:10px; padding:2px 10px; font-size:12px; margin:2px; cursor:pointer; }
      .kb-tag-chip:hover { background:#d1c4e9; }
    ")),
    wellPanel(
      fluidRow(
        column(3, textInput("kb_search", NULL, width = "100%", placeholder = "搜索知识库...")),
        column(2, selectInput("kb_search_mode", NULL, width = "100%",
          choices = c("标题/简介" = "meta", "正文片段" = "chunk"), selected = "meta")),
        column(1, actionButton("kb_search_btn", "检索", class = "btn-primary", style = "width:100%;")),
        column(3, div(style = "display:flex; gap:6px;",
          actionButton("kb_upload_btn", "上传文档", icon = icon("upload"), class = "btn-success"),
          actionButton("kb_manual_btn", "录入文档", icon = icon("pen"), class = "btn-info"),
          actionButton("kb_settings_btn", "设置", icon = icon("cog"), class = "btn-default"))),
        column(3, div(style = "text-align:right;", htmlOutput("kb_stats")))
      )
    ),
    fluidRow(
      column(3,
        tags$div(style = "display:flex; align-items:center; justify-content:space-between;",
          tags$h5(icon("folder"), " 分类", style = "margin:4px 0;"),
          actionButton("kb_cat_manage_btn", "管理", icon = icon("cog"), class = "btn-xs btn-default")
        ),
        uiOutput("kb_cat_list"),
        tags$hr(style = "margin:8px 0;"),
        tags$h5(icon("tags"), " 标签", style = "margin:4px 0;"),
        uiOutput("kb_tag_cloud"),
        tags$hr(style = "margin:8px 0;"),
        actionButton("kb_log_btn", "查看日志", icon = icon("file-alt"), class = "btn-xs btn-default", style = "width:100%;")
      ),
      column(9,
        uiOutput("kb_result_title"),
        DTOutput("kb_doc_table")
      )
    )
  )
}
