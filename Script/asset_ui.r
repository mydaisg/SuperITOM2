# 资产管理模块 - UI

asset_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.asset-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('asset_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.asset-del-btn',function(e){
        e.stopPropagation();
        if(!confirm('确定删除此资产？')) return;
        Shiny.setInputValue('asset_del_click',$(this).data('id'),{priority:'event'});
      });
    ")),
    tags$style(HTML("
      .asset-stat-box { text-align:center; padding:10px 6px; border-radius:6px; margin:0; }
      .asset-stat-box .num { font-size:22px; font-weight:bold; }
      .asset-stat-box .lbl { font-size:11px; }
    ")),
    fluidPage(
      # 统计栏
      uiOutput("asset_stats"),
      
      # 操作栏
      wellPanel(
        fluidRow(
          column(3, textInput("asset_search", NULL, placeholder = "搜索主机名/IP...")),
          column(3, selectInput("asset_status_filter", NULL,
            choices = c("全部","active"="active","maintenance","retired"), selected = "全部")),
          column(3, actionButton("asset_add_btn", "添加资产", icon = icon("plus"), class = "btn-primary")),
          column(3, div(style = "text-align:right;",
            actionButton("asset_refresh", icon("refresh"), class = "btn-default")))
        )
      ),
      
      # 资产表格
      DTOutput("asset_table"),
      
      # 状态徽章样式
      tags$style(HTML("
        .badge-active { background:#5cb85c; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
        .badge-maintenance { background:#f0ad4e; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
        .badge-retired { background:#d9534f; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
      "))
    )
  )
}
