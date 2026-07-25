# 工位图 V2 - UI 层
# 新特性：可视化拖拽布局、搜索筛选、批量编辑、侧边栏详情

seat_map_v2_ui <- function() {
  tagList(
    # ── CSS ──
    tags$style(HTML("
      /* ===== 整体布局 ===== */
      .smv2-wrap { display:flex; gap:0; height:calc(100vh - 180px); min-height:550px; border:1px solid #dee2e6; border-radius:8px; overflow:hidden; background:#fff; }
      .smv2-sidebar { width:280px; min-width:280px; border-right:1px solid #dee2e6; background:#f8f9fa; display:flex; flex-direction:column; overflow:hidden; }
      .smv2-main { flex:1; display:flex; flex-direction:column; overflow:hidden; }
      
      /* ===== 侧边栏 ===== */
      .smv2-sidebar-header { padding:10px 12px; border-bottom:1px solid #dee2e6; background:#fff; }
      .smv2-sidebar-search { padding:8px 12px; border-bottom:1px solid #dee2e6; }
      .smv2-sidebar-list { flex:1; overflow-y:auto; padding:4px; }
      .smv2-sidebar-footer { padding:8px 12px; border-top:1px solid #dee2e6; background:#fff; font-size:11px; color:#666; }
      
      /* 工位卡片 */
      .smv2-seat-card { background:#fff; border:1px solid #e0e0e0; border-radius:6px; padding:8px 10px; margin-bottom:4px; cursor:pointer; transition:all 0.15s; font-size:12px; }
      .smv2-seat-card:hover { border-color:#4f8ef7; box-shadow:0 2px 8px rgba(79,142,247,0.15); }
      .smv2-seat-card.active { border-color:#4f8ef7; background:#e8f0fe; }
      .smv2-seat-card .seat-code { font-weight:700; font-size:13px; color:#333; }
      .smv2-seat-card .seat-user { font-size:11px; color:#555; margin-top:2px; }
      .smv2-seat-card .seat-status { display:inline-block; width:8px; height:8px; border-radius:50%; margin-right:4px; vertical-align:middle; }
      .smv2-seat-card .seat-status.occupied { background:#4caf50; }
      .smv2-seat-card .seat-status.vacant-no-pc { background:#bbb; }
      .smv2-seat-card .seat-status.vacant-with-pc { background:#2196f3; }
      
      /* ===== 工具栏 ===== */
      .smv2-toolbar { display:flex; align-items:center; gap:8px; padding:8px 12px; border-bottom:1px solid #dee2e6; background:#fff; flex-wrap:wrap; }
      .smv2-toolbar .btn { font-size:12px; }
      .smv2-toolbar .divider { width:1px; height:20px; background:#dee2e6; margin:0 4px; }
      
      /* ===== 画布区 ===== */
      .smv2-canvas-wrap { flex:1; overflow:auto; position:relative; background:#e8ecf1; }
      .smv2-canvas-inner { position:relative; min-width:100%; min-height:100%; padding:20px; }
      
      /* 网格背景 */
      .smv2-grid-bg { position:absolute; top:0; left:0; right:0; bottom:0; 
        background-image: linear-gradient(rgba(0,0,0,0.05) 1px, transparent 1px), 
                          linear-gradient(90deg, rgba(0,0,0,0.05) 1px, transparent 1px);
        background-size: 40px 40px;
        pointer-events:none; z-index:0; }
      
      /* 区域块 */
      .smv2-zone { position:absolute; border:2px solid #999; border-radius:6px; display:flex; align-items:flex-start; justify-content:flex-start; padding:6px 8px; font-size:11px; font-weight:600; color:#666; z-index:1; opacity:0.85; pointer-events:none; }
      
      /* 工位块 */
      .smv2-seat { position:absolute; border-radius:6px; border:2px solid transparent; cursor:pointer; display:flex; flex-direction:column; align-items:center; justify-content:center; font-size:11px; transition:all 0.15s; z-index:2; user-select:none; }
      .smv2-seat:hover { z-index:10; transform:scale(1.05); box-shadow:0 4px 16px rgba(0,0,0,0.2); }
      .smv2-seat.selected { border-color:#ff9800 !important; box-shadow:0 0 0 3px rgba(255,152,0,0.25); z-index:9; }
      .smv2-seat.highlighted { border-color:#4f8ef7 !important; box-shadow:0 0 0 3px rgba(79,142,247,0.3); z-index:9; animation: smv2-pulse 0.6s ease-in-out 3; }
      @keyframes smv2-pulse { 0%,100% { box-shadow:0 0 0 3px rgba(79,142,247,0.3); } 50% { box-shadow:0 0 0 8px rgba(79,142,247,0.1); } }
      
      /* 工位状态颜色 */
      .smv2-seat.occupied { background:linear-gradient(135deg, #c8e6c9, #a5d6a7); border-color:#66bb6a; }
      .smv2-seat.vacant-no-pc { background:linear-gradient(135deg, #e0e0e0, #bdbdbd); border-color:#9e9e9e; }
      .smv2-seat.vacant-with-pc { background:linear-gradient(135deg, #bbdefb, #90caf9); border-color:#42a5f5; }
      
      .smv2-seat .s-code { font-weight:700; font-size:12px; color:#333; line-height:1.2; }
      .smv2-seat .s-user { font-size:10px; color:#444; line-height:1.2; max-width:90%; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
      .smv2-seat .s-host { font-size:9px; color:#777; line-height:1.2; }
      
      /* 右键菜单 */
      .smv2-context-menu { position:fixed; background:#fff; border:1px solid #ccc; border-radius:6px; box-shadow:0 4px 20px rgba(0,0,0,0.18); z-index:9999; min-width:160px; padding:4px 0; display:none; font-size:13px; }
      .smv2-context-menu .menu-item { padding:6px 16px; cursor:pointer; display:flex; align-items:center; gap:8px; }
      .smv2-context-menu .menu-item:hover { background:#e8f0fe; }
      .smv2-context-menu .menu-divider { height:1px; background:#eee; margin:4px 0; }
      
      /* 详情面板 */
      .smv2-detail-panel { position:absolute; right:0; top:0; bottom:0; width:320px; background:#fff; border-left:2px solid #dee2e6; z-index:20; display:none; flex-direction:column; box-shadow:-4px 0 20px rgba(0,0,0,0.1); }
      .smv2-detail-panel.show { display:flex; }
      .smv2-detail-header { padding:12px 16px; border-bottom:1px solid #eee; display:flex; align-items:center; justify-content:space-between; }
      .smv2-detail-body { flex:1; overflow-y:auto; padding:16px; }
      .smv2-detail-body .info-row { display:flex; margin-bottom:10px; font-size:13px; }
      .smv2-detail-body .info-label { width:60px; color:#888; flex-shrink:0; }
      .smv2-detail-body .info-value { flex:1; color:#333; font-weight:500; }
      
      /* 响应式 */
      @media (max-width:768px) {
        .smv2-sidebar { width:200px; min-width:200px; }
        .smv2-seat { width:60px !important; height:60px !important; }
      }
    ")),
    
    # ── 导航栏 ──
    fluidRow(
      column(3, selectizeInput("smv2_building", NULL, choices = c("\u2014 \u9009\u62E9\u697C\u680B \u2014" = ""), width = "100%",
        options = list(placeholder = "选择楼栋..."))),
      column(2, selectizeInput("smv2_floor", NULL, choices = c("\u2014 \u5148\u9009\u697C\u680B \u2014" = ""), width = "100%",
        options = list(placeholder = "选择楼层..."))),
      column(5, tags$div(style = "margin-top:25px; display:flex; gap:6px; flex-wrap:wrap;",
        actionButton("smv2_add_building", NULL, icon = icon("plus"), class = "btn-sm btn-default", title = "添加楼栋"),
        actionButton("smv2_add_floor",    NULL, icon = icon("plus"), class = "btn-sm btn-default", title = "添加楼层"),
        actionButton("smv2_add_zone",     NULL, icon = icon("plus"), class = "btn-sm btn-default", title = "添加区域"),
        actionButton("smv2_add_seat",     NULL, icon = icon("plus"), class = "btn-sm btn-success", title = "添加工位"),
        actionButton("smv2_batch_seats",  NULL, icon = icon("th"),   class = "btn-sm btn-info",    title = "批量生成"),
        actionButton("smv2_del_building", NULL, icon = icon("trash"),class = "btn-sm btn-danger",  title = "删除楼栋/楼层")
      )),
      column(2, tags$div(style = "margin-top:25px; text-align:right;",
        actionButton("smv2_refresh", NULL, icon = icon("sync"), class = "btn-sm btn-default"),
        actionButton("smv2_legend_toggle", NULL, icon = icon("info-circle"), class = "btn-sm btn-default", title = "图例")))
    ),
    
    # ── 图例（可折叠）──
    conditionalPanel("output.smv2_show_legend",
      div(class = "sm-legend", style = "display:flex; flex-wrap:wrap; gap:12px; align-items:center; padding:6px 0; font-size:12px;",
        tags$b("图例："),
        tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#c8e6c9; width:14px; height:14px; border-radius:3px; border:1px solid #66bb6a;"), "有员工"),
        tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#e0e0e0; width:14px; height:14px; border-radius:3px; border:1px solid #9e9e9e;"), "无员工无电脑"),
        tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#bbdefb; width:14px; height:14px; border-radius:3px; border:1px solid #42a5f5;"), "无员工有电脑"),
        tags$span(style = "color:#999;", "|"),
        tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#e8f5e9; width:14px; height:14px; border-radius:3px; border:1px solid #a5d6a7;"), "会议室"),
        tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#fff3e0; width:14px; height:14px; border-radius:3px; border:1px solid #ffcc80;"), "实验室"),
        tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#e3f2fd; width:14px; height:14px; border-radius:3px; border:1px solid #90caf9;"), "前台"),
        tags$span(style = "color:#999;", "| 右键工位查看菜单")
      )
    ),
    
    # ── 主体：左侧列表 + 右侧画布 ──
    div(class = "smv2-wrap",
      # 左侧边栏
      div(class = "smv2-sidebar",
        div(class = "smv2-sidebar-header",
          tags$b("工位列表"),
          tags$small(style = "float:right; color:#999;", textOutput("smv2_seat_count", inline = TRUE))
        ),
        div(class = "smv2-sidebar-search",
          textInput("smv2_search", NULL, placeholder = "搜索编号/人员/资产...", width = "100%")
        ),
        div(class = "smv2-sidebar-list",
          uiOutput("smv2_seat_list")
        ),
        div(class = "smv2-sidebar-footer",
          "点击左侧定位工位 | 拖拽移动工位"
        )
      ),
      # 右侧画布
      div(class = "smv2-main",
        # 工具栏
        div(class = "smv2-toolbar",
          tags$b(textOutput("smv2_floor_title", inline = TRUE)),
          tags$span(class = "divider"),
          actionButton("smv2_zoom_in",  NULL, icon = icon("search-plus"),  class = "btn-xs btn-default", title = "放大"),
          actionButton("smv2_zoom_out", NULL, icon = icon("search-minus"), class = "btn-xs btn-default", title = "缩小"),
          actionButton("smv2_zoom_fit", NULL, icon = icon("expand"),       class = "btn-xs btn-default", title = "适应"),
          tags$span(class = "divider"),
          selectInput("smv2_filter_status", NULL, width = "120px",
            choices = c("全部状态" = "all", "有员工" = "occupied", "无员工无电脑" = "vacant_no_pc", "无员工有电脑" = "vacant_with_pc")),
          tags$span(class = "divider"),
          actionButton("smv2_quick_edit", "快速编辑", icon = icon("edit"), class = "btn-xs btn-warning"),
          tags$span(style = "font-size:11px; color:#999;", "点击工位选中，拖拽移动，右键菜单")
        ),
        # 画布
        div(class = "smv2-canvas-wrap", id = "smv2_canvas_container",
          div(class = "smv2-canvas-inner",
            div(class = "smv2-grid-bg"),
            uiOutput("smv2_canvas")
          ),
          # 右侧详情面板
          div(class = "smv2-detail-panel", id = "smv2_detail_panel",
            div(class = "smv2-detail-header",
              tags$b("工位详情"),
              tags$a(href = "#", onclick = "document.getElementById('smv2_detail_panel').classList.remove('show'); return false;",
                icon("times"), style = "color:#999; font-size:16px;")
            ),
            div(class = "smv2-detail-body", uiOutput("smv2_detail_content"))
          )
        )
      )
    ),
    
    # ── 右键菜单 ──
    tags$div(class = "smv2-context-menu", id = "smv2_context_menu",
      tags$div(class = "menu-item", onclick = "Shiny.setInputValue('smv2_ctx_edit', $('#smv2_ctx_seat_id').val(), {priority:'event'});",
        icon("edit"), "编辑工位"),
      tags$div(class = "menu-item", onclick = "Shiny.setInputValue('smv2_ctx_status', {id:$('#smv2_ctx_seat_id').val(), status:'occupied'}, {priority:'event'});",
        icon("user-check"), "标记为有员工"),
      tags$div(class = "menu-item", onclick = "Shiny.setInputValue('smv2_ctx_status', {id:$('#smv2_ctx_seat_id').val(), status:'vacant_no_pc'}, {priority:'event'});",
        icon("user-slash"), "标记为无员工无电脑"),
      tags$div(class = "menu-item", onclick = "Shiny.setInputValue('smv2_ctx_status', {id:$('#smv2_ctx_seat_id').val(), status:'vacant_with_pc'}, {priority:'event'});",
        icon("desktop"), "标记为无员工有电脑"),
      tags$div(class = "menu-divider"),
      tags$div(class = "menu-item", onclick = "Shiny.setInputValue('smv2_ctx_delete', $('#smv2_ctx_seat_id').val(), {priority:'event'});",
        icon("trash"), "删除工位", style = "color:#d9534f;")
    ),
    tags$input(type = "hidden", id = "smv2_ctx_seat_id"),
    
    # ── JS：右键菜单 + 拖拽 + 画布交互 ──
    tags$script(HTML("
      // 右键菜单
      $(document).on('contextmenu', '.smv2-seat', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        $('#smv2_ctx_seat_id').val(id);
        var menu = $('#smv2_context_menu');
        menu.css({display:'block', left:e.pageX+'px', top:e.pageY+'px'});
        return false;
      });
      $(document).click(function() { $('#smv2_context_menu').hide(); });
      
      // 点击工位 → 选中 + 详情
      $(document).on('click', '.smv2-seat', function(e) {
        if (e.button !== 0) return; // 只处理左键
        $('.smv2-seat').removeClass('selected');
        $(this).addClass('selected');
        Shiny.setInputValue('smv2_seat_select', $(this).data('id'), {priority:'event'});
      });
      
      // 左侧列表点击 → 高亮画布中工位
      $(document).on('click', '.smv2-seat-card', function() {
        var id = $(this).data('id');
        $('.smv2-seat').removeClass('highlighted');
        $('.smv2-seat[data-id=\"' + id + '\"]').addClass('highlighted');
        // 滚动到可视区域
        var el = $('.smv2-seat[data-id=\"' + id + '\"]')[0];
        if (el) el.scrollIntoView({behavior:'smooth', block:'center'});
      });
      
      // 画布缩放
      var scale = 1;
      Shiny.addCustomMessageHandler('smv2_zoom', function(msg) {
        scale = msg.scale;
        $('.smv2-canvas-inner').css('transform', 'scale(' + scale + ')');
        $('.smv2-canvas-inner').css('transform-origin', '0 0');
      });
    "))
  )
}
