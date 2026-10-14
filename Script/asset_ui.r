# 资产管理模块 - UI（含工位图子标签）

asset_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.asset-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('asset_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.asset-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('asset_del_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.att-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('att_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.att-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('att_del_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.cfgitem-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgitem_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.cfgitem-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgitem_del_click',$(this).data('id'),{priority:'event'});
      });
      // 复制密码（复制明文）
      $(document).on('click','.cfgitem-copy-btn',function(e){
        e.stopPropagation();
        var pwd = $(this).data('pwd');
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(pwd).then(function(){
            Shiny.setInputValue('cfgitem_copy_feedback', Math.random(), {priority:'event'});
          });
        } else {
          var ta = document.createElement('textarea');
          ta.value = pwd; document.body.appendChild(ta); ta.select();
          document.execCommand('copy'); document.body.removeChild(ta);
          Shiny.setInputValue('cfgitem_copy_feedback', Math.random(), {priority:'event'});
        }
      });
      // 复制标签（国际访问，复制完整标签文本）
      $(document).on('click','.cfgitem-label-copy-btn',function(e){
        e.stopPropagation();
        var label = $(this).attr('data-label');
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(label).then(function(){
            Shiny.setInputValue('cfgitem_label_copy_feedback', Math.random(), {priority:'event'});
          });
        } else {
          var ta = document.createElement('textarea');
          ta.value = label; document.body.appendChild(ta); ta.select();
          document.execCommand('copy'); document.body.removeChild(ta);
          Shiny.setInputValue('cfgitem_label_copy_feedback', Math.random(), {priority:'event'});
        }
      });
      // 国际访问账号删除
      $(document).on('click','.cfgintl-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgintl_del_click',$(this).data('id'),{priority:'event'});
      });
      // 平台编辑/删除
      $(document).on('click','.cfgplat-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgplat_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.cfgplat-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgplat_del_click',$(this).data('id'),{priority:'event'});
      });
      // 账号编辑/删除
      $(document).on('click','.cfgacc-edit-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgacc_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.cfgacc-del-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('cfgacc_del_click',$(this).data('id'),{priority:'event'});
      });
      // 字段收集：保存时读取排序后的字段列表
      Shiny.addCustomMessageHandler('cfgitemCollectFields', function(msg) {
        var fields = $('#cfgitem_field_list .cfgitem-field-row').map(function(){ return $(this).data('field'); }).get();
        Shiny.setInputValue('cfgitem_fields_order', fields, {priority:'event'});
      });
      // 账号增删改后重新打开编辑弹窗
      Shiny.addCustomMessageHandler('cfgitemReopenEdit', function(msg) {
        setTimeout(function(){ Shiny.setInputValue('cfgitem_edit_click', msg.id, {priority:'event'}); }, 200);
      });
      // 平台添加后重新打开平台管理弹窗
      Shiny.addCustomMessageHandler('cfgplatReopen', function(msg) {
        setTimeout(function(){ Shiny.setInputValue('cfgitem_platform_manage_btn', Math.random(), {priority:'event'}); }, 200);
      });
    ")),
    tags$style(HTML("
      .asset-stat-box { text-align:center; padding:10px 6px; border-radius:6px; margin:0; }
      .asset-stat-box .num { font-size:22px; font-weight:bold; }
      .asset-stat-box .lbl { font-size:11px; }
      .badge-active { background:#5cb85c; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
      .badge-maintenance { background:#f0ad4e; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
      .badge-retired { background:#d9534f; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
    ")),
    tabsetPanel(
      id = "asset_tabs", type = "pills",
      # ── 标签1：资产列表 ──
      tabPanel("资产列表",
        uiOutput("asset_stats"),
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
        DTOutput("asset_table")
      ),
      # ── 标签2：工位图（旧版）──
      tabPanel("工位图", value = "seat",
        seat_map_content()
      ),
      # ── 标签2b：工位图 V2（新版）──
      tabPanel(tags$span("工位图", tags$sup(style = "color:#ff9800; font-size:10px;", "V2")), value = "seat_v2",
        seat_map_v2_ui()
      ),
      # ── 标签3：考勤设备 ──
      tabPanel("考勤设备", value = "attendance",
        uiOutput("attendance_device_stats"),
        wellPanel(
          fluidRow(
            column(2, selectInput("attendance_filter_area", "区域筛选",
              choices = c("全部"), selected = "全部")),
            column(2, selectInput("attendance_filter_type", "设备类型",
              choices = c("全部","人脸识别","指纹机"), selected = "全部")),
            column(2, selectInput("attendance_filter_brand", "品牌筛选",
              choices = c("全部","中控","钉钉"), selected = "全部")),
            column(3, actionButton("attendance_add_btn", "添加设备", icon = icon("plus"), class = "btn-primary")),
            column(3, div(style = "text-align:right;",
              actionButton("attendance_refresh", icon("refresh"), class = "btn-default")))
          )
        ),
        DTOutput("attendance_device_table")
      ),
      # ── 标签4：配置管理（多平台服务器资源清单 + 账号密码）──
      tabPanel("配置管理", value = "config",
        uiOutput("cfgitem_stats"),
        wellPanel(
          fluidRow(
            column(3, uiOutput("cfgitem_platform_ui")),
            column(9, uiOutput("cfgitem_dynamic_toolbar"))
          )
        ),
        uiOutput("cfgitem_table_area")
      )
    )
  )
}

# 工位图子标签内容（仅UI，不含 fluidPage 顶层）
seat_map_content <- function() {
  tagList(
    tags$style(HTML("
      .sm-floor-canvas { border:2px solid #dee2e6; border-radius:8px; padding:0; overflow:auto; background:#fafbfc; }
      .sm-legend { display:flex; flex-wrap:wrap; gap:12px; align-items:center; padding:8px 0; font-size:12px; }
      .sm-legend-item { display:flex; align-items:center; gap:4px; }
      .sm-legend-dot { width:14px; height:14px; border-radius:3px; border:1px solid #ccc; }
      .sm-grid-wrap { display:inline-block; min-width:100%; padding:16px; }
      .sm-grid { display:grid; gap:3px; }
      .sm-cell { border-radius:4px; border:1px solid #ddd; text-align:center; font-size:11px; cursor:pointer;
        transition:all 0.15s; display:flex; flex-direction:column; align-items:center; justify-content:center;
        min-width:60px; min-height:50px; padding:4px 2px; position:relative; }
      .sm-cell:hover { z-index:2; transform:scale(1.08); box-shadow:0 4px 14px rgba(0,0,0,0.18); border-color:#4f8ef7; }
      .sm-cell .sm-code { font-weight:700; font-size:12px; }
      .sm-cell .sm-name { font-size:10px; color:#555; margin-top:1px; max-width:56px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
      .sm-cell .sm-host { font-size:9px; color:#888; margin-top:1px; }
      .sm-occupied        { background:#d4edda; border-color:#a3cfb3; }
      .sm-vacant-no-pc    { background:#f0f0f0; border-color:#ccc; }
      .sm-vacant-with-pc  { background:#cce5ff; border-color:#9ac4f0; }
      .sm-zone-cell { border-radius:4px; border:2px solid #aaa; text-align:center; font-size:12px; font-weight:600;
        display:flex; align-items:center; justify-content:center; min-height:50px; }
      .sz-reception     { background:#e3f2fd; border-color:#90caf9; }
      .sz-open_desk     { background:#f5f5f5; border-color:#ccc; }
      .sz-meeting_room  { background:#e8f5e9; border-color:#a5d6a7; }
      .sz-lab           { background:#fff3e0; border-color:#ffcc80; }
      .sz-warehouse     { background:#fce4ec; border-color:#ef9a9a; }
      .sz-small_office  { background:#f3e5f5; border-color:#ce93d8; }
      .sz-tea_room      { background:#e0f2f1; border-color:#80cbc4; }
      .sz-smoking_room  { background:#eceff1; border-color:#b0bec5; }
    ")),
    fluidRow(
      column(3, selectizeInput("sm_building", "楼栋", choices = c("\u2014 \u8BF7\u5148\u6DFB\u52A0\u697C\u680B \u2014" = ""), width = "100%",
        options = list(placeholder = "选择楼栋..."))),
      column(2, selectizeInput("sm_floor", "楼层", choices = c("\u2014 \u5148\u9009\u697C\u680B \u2014" = ""), width = "100%",
        options = list(placeholder = "选择楼层..."))),
      column(5, tags$div(style = "margin-top:25px; display:flex; gap:6px; flex-wrap:wrap;",
        actionButton("sm_add_building", "楼栋", icon = icon("plus"), class = "btn-sm btn-default"),
        actionButton("sm_add_floor",    "楼层", icon = icon("plus"), class = "btn-sm btn-default"),
        actionButton("sm_add_zone",     "区域", icon = icon("plus"), class = "btn-sm btn-default"),
        actionButton("sm_add_seat",     "工位", icon = icon("plus"), class = "btn-sm btn-success"),
        actionButton("sm_batch_seats",  "批量", icon = icon("th"),   class = "btn-sm btn-info"),
        actionButton("sm_del_building", "删除", icon = icon("trash"),class = "btn-sm btn-danger")
      )),
      column(2, tags$div(style = "margin-top:25px; text-align:right;",
        actionButton("sm_refresh", icon("sync"), class = "btn-sm btn-default")))
    ),
    div(class = "sm-legend",
      tags$b("图例："),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#d4edda;"), "有员工"),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#f0f0f0;"), "无员工无电脑"),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#cce5ff;"), "无员工有电脑"),
      tags$span(style = "color:#999;", "|"),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#e8f5e9;"), "会议室"),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#fff3e0;"), "实验室"),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#e3f2fd;"), "前台"),
      tags$span(class = "sm-legend-item", tags$span(class = "sm-legend-dot", style = "background:#f5f5f5;"), "卡座/其他")
    ),
    br(),
    uiOutput("sm_canvas"),
    tags$script(HTML("
      $(document).on('click','.sm-cell',function(e){
        e.stopPropagation();
        Shiny.setInputValue('sm_seat_click',{id:$(this).data('id'),code:$(this).data('code')},{priority:'event'});
      });
    "))
  )
}
