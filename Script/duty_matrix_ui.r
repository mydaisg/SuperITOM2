# 岗职模块 UI - 岗位职责矩阵

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
      $(document).on('click','.duty-edit-btn',function(e){
        e.stopPropagation();
        var t=$(this).data('table'), id=$(this).data('id'), row=$(this).data('row');
        Shiny.setInputValue('duty_edit_click',{table:t,id:id,row:row},{priority:'event'});
      });
      $(document).on('click','.duty-del-btn',function(e){
        e.stopPropagation();
        if(!confirm('确定删除？')) return;
        var t=$(this).data('table'), id=$(this).data('id');
        Shiny.setInputValue('duty_del_click',{table:t,id:id},{priority:'event'});
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
      h4(icon("list")," 岗位 · 人员 · 职责项清单"),
      fluidRow(
        column(4,
          wellPanel(
            h5("岗位清单"), DTOutput("duty_position_table")
          )
        ),
        column(4,
          wellPanel(
            h5("人员清单"), DTOutput("duty_staff_table")
          )
        ),
        column(4,
          wellPanel(
            h5("职责项清单"), DTOutput("duty_item_table")
          )
        )
      ),
      # 创建区
      wellPanel(
        h4(icon("plus-circle")," 创建"),
        tabsetPanel(
          tabPanel("岗位",
            fluidRow(
              column(4, textInput("duty_new_position_name","岗位名称")),
              column(6, textInput("duty_new_position_desc","描述")),
              column(2, div(style="margin-top:20px;",actionButton("duty_add_position","添加岗位",class="btn-primary btn-sm")))
            )
          ),
          tabPanel("人员",
            fluidRow(
              column(3, textInput("duty_new_staff_name","姓名")),
              column(3, selectInput("duty_new_staff_position","所属岗位", choices = c("(无)" = ""))),
              column(2, textInput("duty_new_staff_dept","部门")),
              column(2, textInput("duty_new_staff_email","邮箱")),
              column(2, div(style="margin-top:20px;",actionButton("duty_add_staff","添加人员",class="btn-primary btn-sm")))
            )
          ),
          tabPanel("职责项",
            fluidRow(
              column(3, textInput("duty_new_item_name","职责名称")),
              column(3, textInput("duty_new_item_cat","分类")),
              column(4, textInput("duty_new_item_desc","描述")),
              column(2, div(style="margin-top:20px;",actionButton("duty_add_item","添加职责",class="btn-primary btn-sm")))
            )
          )
        )
      )
    )
  )
}
