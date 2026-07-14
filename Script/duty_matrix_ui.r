# 岗职模块 UI - 岗位职责矩阵 + 卡片清单

duty_matrix_ui <- function() {
  tagList(
    tags$script(HTML("
      // RBAC 级别标签选择器 — 按钮组（保留兼容旧的标签按钮）
      $(document).on('click','.duty-level-btn',function(e){
        e.stopPropagation();
        $('.duty-level-btn').removeClass('active').css({border:'1px solid transparent',opacity:'0.55'});
        $(this).addClass('active').css({border:'2px solid #333',opacity:'1'});
        Shiny.setInputValue($(this).data('target'), $(this).data('val'), {priority:'event'});
      });
      $(document).on('click','.duty-cell',function(e){
        e.stopPropagation();
        var sdid = $(this).data('sdid');
        Shiny.setInputValue('duty_matrix_click',{
          sid: $(this).data('sid'),
          pid: $(this).data('pid'),
          did: $(this).data('did'),
          sdid: sdid || null,
          type: sdid ? 'sub' : 'item'
        }, {priority:'event'});
      });
      // 矩阵 [+] 展开/折叠二级行
      $(document).on('click','.duty-row-toggle',function(e){
        e.stopPropagation();
        var did = $(this).attr('data-did');
        var rows = $('.duty-sub-row[data-parent-did=' + did + ']');
        if (!rows.length) return;
        var showing = rows.first().css('display') !== 'none';
        rows.css('display', showing ? 'none' : '');
        $(this).text(showing ? '[+]' : '[-]');
      });
      // 子行列点击
      $(document).on('click','.duty-sub-cell',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_matrix_click',{
          sid: $(this).data('sid'),
          pid: $(this).data('pid'),
          sdid: $(this).data('sdid'),
          did: null,
          type: 'sub'
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
        Shiny.setInputValue('duty_card_del',{type:$(this).data('type'),id:$(this).data('id')},{priority:'event'});
      });
      $(document).on('click','.duty-card-rm-staff',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_rm_staff',{sid:$(this).data('sid'),pid:$(this).data('pid')},{priority:'event'});
      });
      $(document).on('click','.duty-card-rm-duty',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_rm_duty',{sid:$(this).data('sid'),pid:$(this).data('pid'),did:$(this).data('did')},{priority:'event'});
      });
      $(document).on('click','.duty-card-rm-sub-duty',function(e){
        e.stopPropagation();
        Shiny.setInputValue('duty_card_rm_sub_duty',{sid:$(this).data('sid'),pid:$(this).data('pid'),sdid:$(this).data('sdid')},{priority:'event'});
      });
    ")),
    tags$style(HTML("
      .duty-table th, .duty-table td { text-align:center; vertical-align:middle; font-size:12px; padding:4px 6px; }
      .duty-table th { background:#f5f5f5; font-weight:600; }
      .duty-cell { cursor:pointer; min-width:60px; border-radius:3px; padding:3px 6px; font-size:11px; }
      .duty-cell:hover { box-shadow:0 0 4px rgba(0,0,0,0.2); }
      .duty-cell.r { background:#d4edda; color:#155724; font-weight:bold; }
      .duty-cell.a { background:#f8d7da; color:#721c24; font-weight:bold; }
      .duty-cell.s { background:#d1ecf1; color:#0c5460; }
      .duty-cell.c { background:#fff3cd; color:#856404; }
      .duty-cell.i { background:#e2e3e5; color:#383d41; }
      .duty-cell.empty { background:#f8f9fa; color:#ccc; }
      .duty-cell.empty:hover { color:#999; }
      /* 二级子行 */
      .duty-sub-row td { background:#faf5ff; font-size:11px; padding-left:12px; border-top:1px dashed #e0d0f0; }
      .duty-sub-row td:first-child { padding-left:28px; font-style:italic; }
      .duty-sub-cell { cursor:pointer; min-width:60px; border-radius:3px; padding:2px 5px; font-size:10px; }
      .duty-sub-cell:hover { box-shadow:0 0 4px rgba(0,0,0,0.2); }
      .duty-sub-cell.r { background:#d4edda; color:#155724; font-weight:bold; }
      .duty-sub-cell.a { background:#f8d7da; color:#721c24; font-weight:bold; }
      .duty-sub-cell.s { background:#d1ecf1; color:#0c5460; }
      .duty-sub-cell.c { background:#fff3cd; color:#856404; }
      .duty-sub-cell.i { background:#e2e3e5; color:#383d41; }
      .duty-sub-cell.empty { background:#f8f9fa; color:#ccc; }
      .duty-row-toggle { cursor:pointer; color:#666; font-family:monospace; font-size:11px; margin-right:4px; user-select:none; }
      .duty-row-toggle:hover { color:#333; }
      .duty-card { background:white; border-radius:8px; padding:12px; margin-bottom:8px; box-shadow:0 1px 3px rgba(0,0,0,0.12); }
      .duty-card h5 { margin:0 0 8px; display:flex; justify-content:space-between; align-items:center; }
      .duty-card .card-actions { display:flex; gap:4px; }
      .duty-card .tag { display:inline-block; padding:1px 8px; border-radius:10px; font-size:11px; margin:2px 2px 2px 0; }
      .duty-card .tag.r { background:#d4edda; color:#155724; }
      .duty-card .tag.a { background:#f8d7da; color:#721c24; }
      .duty-card .tag.s { background:#d1ecf1; color:#0c5460; }
      .duty-card .tag.c { background:#fff3cd; color:#856404; }
      .duty-card .tag.i { background:#e2e3e5; color:#383d41; }
      .duty-card-fields { font-size:12px; color:#666; }
      /* 二级卡片 */
      .duty-sub-card { background:#faf5ff; border-radius:6px; padding:8px 10px; margin-bottom:4px; margin-left:12px; border-left:3px solid #c8b6e0; }
      .duty-sub-card .tag { font-size:10px; }
      /* RBAC 级别标签选择器 */
      .duty-level-btn {
        padding:6px 16px; border-radius:20px; cursor:pointer; font-size:13px; font-weight:500;
        border:1px solid transparent; margin:0 4px 8px 0; display:inline-block;
        transition:all 0.15s; opacity:0.55; user-select:none;
      }
      .duty-level-btn:hover { opacity:0.8; }
      .duty-level-btn.active { opacity:1 !important; border:2px solid #333 !important; }
      .duty-level-btn.r { background:#d4edda; color:#155724; }
      .duty-level-btn.a { background:#f8d7da; color:#721c24; }
      .duty-level-btn.s { background:#d1ecf1; color:#0c5460; }
      .duty-level-btn.c { background:#fff3cd; color:#856404; }
      .duty-level-btn.i { background:#e2e3e5; color:#383d41; }
      /* 岗职矩阵内 selectize 微调（继承全局 Material Design，仅调字号） */
      .duty-matrix-wrapper .selectize-input,
      .duty-create-row .selectize-input,
      .modal-body .selectize-input {
        font-size:13px !important; min-height:36px !important;
      }
      .duty-matrix-wrapper .selectize-dropdown,
      .duty-create-row .selectize-dropdown,
      .modal-body .selectize-dropdown {
        font-size:13px !important;
      }
    ")),
    fluidPage(
      tags$div(class="duty-matrix-wrapper",
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
        tags$style(HTML("
          .duty-create-row { display:flex; gap:6px; align-items:flex-end; flex-wrap:wrap; }
          .duty-create-row .shiny-input-container { margin-bottom:0; }
          .duty-create-row .btn { margin-bottom:0; }
        ")),
        fluidRow(
          column(4,
            tags$div(class="duty-create-row",
              textInput("duty_new_position_name", NULL, placeholder = "岗位名称"),
              tags$button(id="duty_add_position", type="button", class="btn btn-primary btn-sm action-button", disabled=NA, list(icon("plus")))
            )
          ),
          column(4,
            tags$div(class="duty-create-row",
              selectInput("duty_new_staff_user", NULL, choices = c("(选择系统用户)" = ""), width="170px"),
              selectInput("duty_new_staff_position", NULL, choices = c("(无)" = ""), width="130px"),
              tags$button(id="duty_add_staff", type="button", class="btn btn-primary btn-sm action-button", disabled=NA, list(icon("plus")))
            )
          ),
          column(4,
            tags$div(class="duty-create-row",
              textInput("duty_new_item_name", NULL, placeholder = "职责名称"),
              textInput("duty_new_item_cat", NULL, placeholder = "分类", width="90px"),
              numericInput("duty_new_item_sort", NULL, value = 0, min = 0, max = 999, width = "60px"),
              tags$button(id="duty_add_item", type="button", class="btn btn-primary btn-sm action-button", disabled=NA, list(icon("plus")))
            )
          )
        ),
        hr(style="margin:6px 0;"),
        h5(icon("level-down-alt")," 添加二级任务", style="font-size:12px; color:#666;"),
        fluidRow(
          column(6,
            tags$div(class="duty-create-row",
              selectInput("duty_new_sub_item_parent", NULL, choices = c("(选择上级职责)" = ""), width="180px"),
              textInput("duty_new_sub_item_name", NULL, placeholder = "二级任务名称", width="140px"),
              textInput("duty_new_sub_item_cat", NULL, placeholder = "分类", width="90px"),
              numericInput("duty_new_sub_item_sort", NULL, value = 0, min = 0, max = 999, width = "60px"),
              tags$button(id="duty_add_sub_item", type="button", class="btn btn-success btn-sm action-button", disabled=NA, list(icon("plus")))
            )
          )
        )
      ),
      fluidRow(
        column(4, uiOutput("duty_position_cards")),
        column(4, uiOutput("duty_staff_cards")),
        column(4, uiOutput("duty_item_cards"))
      )
    )  # end duty-matrix-wrapper
  )  # end fluidPage
  )  # end tagList
}

