# 岗职模块 UI - 岗位职责矩阵 + 卡片清单

duty_matrix_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.duty-cell',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_matrix_click',{
          sid: $(this).data('sid'),
          pid: $(this).data('pid'),
          did: $(this).data('did')
        }, {priority:'event'});
      });
      // 卡片按钮
      $(document).on('click','.duty-card-add-staff',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_add_staff',$(this).data('pid'),{priority:'event'});
      });
      $(document).on('click','.duty-card-add-duty',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_add_duty',$(this).data('sid'),{priority:'event'});
      });
      $(document).on('click','.duty-card-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_edit',{type:$(this).data('type'),id:$(this).data('id')},{priority:'event'});
      });
      $(document).on('click','.duty-card-del-btn',function(e){
        e.stopPropagation();
        if(!confirm('确定删除？')) return;
        Shiny.setInputValue('duty_card_del',{type:$(this).data('type'),id:$(this).data('id')},{priority:'event'});
      });
      $(document).on('click','.duty-card-rm-staff',function(e){
        e.stopPropagation();
        if(!confirm('从岗位移除该人员？')) return;
        Shiny.setInputValue('duty_card_rm_staff',{sid:$(this).data('sid'),pid:$(this).data('pid')},{priority:'event'});
      });
      $(document).on('click','.duty-card-rm-duty',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_rm_duty',{sid:$(this).data('sid'),pid:$(this).data('pid'),did:$(this).data('did')},{priority:'event'});
      });
    ")),
    tags$style(HTML("
      .duty-table th, .duty-table td { text-align:center; vertical-align:middle; font-size:12px; padding:4px 6px; }
      .duty-table th { background:#f5f5f5; font-weight:600; }
      .duty-cell { cursor:pointer; min-width:60px; border-radius:3px; padding:3px 6px; font-size:11px; }
      .duty-cell:hover { box-shadow:0 0 4px rgba(0,0,0,0.2); }
      .duty-cell.owner { background:#d4edda; color:#155724; font-weight:bold; }
      .duty-cell.exec  { background:#d1ecf1; color:#0c5460; }
      .duty-cell.know  { background:#fff3cd; color:#856404; }
      .duty-cell.empty { background:#f8f9fa; color:#ccc; }
      .duty-cell.empty:hover { color:#999; }
      .duty-card { background:white; border-radius:8px; padding:12px; margin-bottom:8px; box-shadow:0 1px 3px rgba(0,0,0,0.12); }
      .duty-card h5 { margin:0 0 8px; display:flex; justify-content:space-between; align-items:center; }
      .duty-card .card-actions { display:flex; gap:4px; }
      .duty-card .tag { display:inline-block; padding:1px 8px; border-radius:10px; font-size:11px; margin:2px 2px 2px 0; }
      .duty-card .tag.owner { background:#d4edda; color:#155724; }
      .duty-card .tag.exec { background:#d1ecf1; color:#0c5460; }
      .duty-card .tag.know { background:#fff3cd; color:#856404; }
      .duty-card-fields { font-size:12px; color:#666; }
    ")),
    fluidPage(
      div(style="text-align:center;margin:8px 0 4px;",
        h2(icon("sitemap")," 岗职矩阵"),
        p(style="color:#7f8c8d;font-size:12px;","岗位职责矩阵 | 人员×职责项 | RBAC级别")
      ),
      hr(),
      h4(icon("table")," 岗位职责矩阵", style="margin-bottom:10px;"),
      div(style="overflow-x:auto;", uiOutput("duty_matrix_view")),
      hr(),
      # 卡片清单区
      h4(icon("id-card")," 岗位 · 人员 · 职责项", style="margin-bottom:10px;"),
      # 创建区（紧凑内联）
      wellPanel(
        h5(icon("plus-circle")," 快速创建"),
        fluidRow(
          column(4,
            tags$div(style="display:flex; gap:4px;",
              textInput("duty_new_position_name", NULL, placeholder = "岗位名称"),
              actionButton("duty_add_position", NULL, icon=icon("plus"), class="btn-primary btn-sm", style="margin-top:22px;")
            )
          ),
          column(4,
            tags$div(style="display:flex; gap:4px;",
              selectInput("duty_new_staff_user", NULL, choices = c("(选择系统用户)" = ""), width="160px"),
              selectInput("duty_new_staff_position", NULL, choices = c("(无)" = ""), width="120px"),
              actionButton("duty_add_staff", NULL, icon=icon("plus"), class="btn-primary btn-sm", style="margin-top:22px;")
            )
          ),
          column(4,
            tags$div(style="display:flex; gap:4px;",
              textInput("duty_new_item_name", NULL, placeholder = "职责名称"),
              textInput("duty_new_item_cat", NULL, placeholder = "分类", width="90px"),
              numericInput("duty_new_item_sort", NULL, value = 0, min = 0, max = 999, width = "60px"),
              actionButton("duty_add_item", NULL, icon=icon("plus"), class="btn-primary btn-sm", style="margin-top:22px;")
            )
          )
        )
      ),
      fluidRow(
        column(4, uiOutput("duty_position_cards")),
        column(4, uiOutput("duty_staff_cards")),
        column(4, uiOutput("duty_item_cards"))
      )
    )
  )
}

