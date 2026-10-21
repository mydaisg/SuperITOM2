# 流程资产中心 - UI 层
# 借鉴泛微 OA 流程中心：大类 → 流程 → 节点 → 流转分支 → 使用率 → 集成判定 → 问题自查

process_asset_ui <- function() {
  tagList(
    tags$style(HTML("
      /* ===== 主容器 ===== */
      .pfa-wrap { padding:0; }
      .pfa-header {
        background: linear-gradient(135deg, #722ed1, #531dab);
        color: #fff; padding: 14px 18px; border-radius: 8px 8px 0 0;
        display: flex; align-items: center; justify-content: space-between;
      }
      .pfa-header h2 { margin:0; font-size:18px; font-weight:600; }
      .pfa-header .sub { font-size:12px; opacity:0.85; margin-top:2px; }

      /* ===== 统计卡 ===== */
      .pfa-stat-card { background:#fff; border:1px solid #e8e8e8; border-radius:6px;
        padding:14px 16px; display:flex; align-items:center; gap:12px; }
      .pfa-stat-card .icon-wrap { width:42px; height:42px; border-radius:8px;
        display:flex; align-items:center; justify-content:center; font-size:18px; flex-shrink:0; }
      .pfa-stat-card .num { font-size:22px; font-weight:700; line-height:1; }
      .pfa-stat-card .lbl { font-size:12px; color:#999; margin-top:4px; }

      /* ===== 筛选栏 ===== */
      .pfa-filterbar { background:#fff; border-bottom:1px solid #e8e8e8; padding:12px 20px;
        display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap; }

      /* ===== 流程卡片 ===== */
      .pfa-card { background:#fff; border:1px solid #e8e8e8; border-radius:6px;
        padding:14px 16px; margin-bottom:10px; transition:all 0.2s; cursor:pointer; }
      .pfa-card:hover { border-color:#722ed1; box-shadow:0 2px 8px rgba(114,46,209,0.15); }
      .pfa-card .fname { font-size:14px; font-weight:600; color:#333; }
      .pfa-card .fdept { font-size:12px; color:#999; margin-left:8px; }
      .pfa-card .fmeta { font-size:12px; color:#666; margin-top:8px; display:flex; gap:16px; flex-wrap:wrap; }
      .pfa-card .fmeta .item { display:flex; align-items:center; gap:4px; }
      .pfa-card .fmeta .item .num { font-weight:700; color:#333; }
      .pfa-card .fsys { font-size:11px; color:#8c6bd0; margin-top:6px; }

      /* ===== 分类分组标题 ===== */
      .pfa-cat-head { display:flex; align-items:center; gap:8px; margin:18px 0 8px;
        padding-bottom:6px; border-bottom:1px solid #f0f0f0; }
      .pfa-cat-head .cat-name { font-size:14px; font-weight:700; color:#531dab; }
      .pfa-cat-head .cat-meta { font-size:12px; color:#999; }

      /* ===== 徽章 ===== */
      .pfa-badge { display:inline-block; padding:1px 9px; border-radius:10px;
        font-size:11px; font-weight:600; }
      .pfa-badge.要集成 { background:#f6ffed; color:#389e0d; border:1px solid #b7eb8f; }
      .pfa-badge.待定   { background:#fff7e6; color:#d46b08; border:1px solid #ffd591; }
      .pfa-badge.不需要 { background:#f5f5f5; color:#8c8c8c; border:1px solid #d9d9d9; }
      .pfa-badge.p-high { background:#fff1f0; color:#cf1322; border:1px solid #ffa39e; }
      .pfa-badge.p-mid  { background:#fff7e6; color:#d46b08; border:1px solid #ffd591; }
      .pfa-badge.p-low  { background:#f0f5ff; color:#1d39c4; border:1px solid #adc6ff; }

      /* ===== 节点表 / 分支 / 问题 ===== */
      .pfa-node-row { display:flex; align-items:center; gap:8px; padding:6px 10px;
        border-bottom:1px solid #f5f5f5; font-size:13px; }
      .pfa-node-seq { width:22px; height:22px; border-radius:50%; background:#f0f0f0;
        color:#666; display:flex; align-items:center; justify-content:center;
        font-size:11px; font-weight:600; flex-shrink:0; }
      .pfa-node-type { font-size:11px; padding:1px 7px; border-radius:8px; flex-shrink:0; }
      .pfa-node-type.发起 { background:#e6f7ff; color:#08979c; }
      .pfa-node-type.审批 { background:#fff7e6; color:#d46b08; }
      .pfa-node-type.结束 { background:#f6ffed; color:#389e0d; }
      .pfa-node-name { font-weight:600; color:#333; }
      .pfa-node-role { color:#999; }
      .pfa-node-cond { color:#8c6bd0; font-size:12px; margin-left:auto; max-width:45%;
        text-align:right; }
      .pfa-branch-row { padding:4px 0; font-size:12px; color:#555; }
      .pfa-branch-row .arrow { color:#722ed1; margin:0 6px; }
      .pfa-branch-cond { color:#999; font-style:italic; }
      .pfa-issue-item { padding:8px 10px; background:#fffbe6; border:1px solid #ffe58f;
        border-radius:4px; margin-bottom:6px; font-size:12px; }
      .pfa-issue-type { font-weight:700; color:#d46b08; }
    ")),

    div(class = "pfa-wrap",
      # 顶部标题
      div(class = "pfa-header",
        div(
          h2(icon("project-diagram"), " 流程资产中心"),
          div(class = "sub", "泛微OA流程梳理 · 19大类/136流程 · 节点与流转分支 · 集成判定 · 问题自查")
        ),
        div(
          actionButton("pfa_seed", "", icon = icon("download"),
            title = "导入/刷新泛微流程数据",
            class = "btn-sm", style = "background:rgba(255,255,255,0.2); color:#fff; border-color:rgba(255,255,255,0.3);")
        )
      ),

      # 统计卡片
      div(style = "padding:16px 20px; background:#fff;",
        fluidRow(
          column(3, uiOutput("pfa_stat_total")),
          column(3, uiOutput("pfa_stat_integrate")),
          column(3, uiOutput("pfa_stat_usage")),
          column(3, uiOutput("pfa_stat_issue"))
        )
      ),

      # 筛选栏
      div(class = "pfa-filterbar",
        selectInput("pfa_filter_cat", "流程大类", choices = c("全部"), width = "200px"),
        selectInput("pfa_filter_decision", "集成判定", width = "140px",
          choices = c("全部" = "全部", "要集成" = "要集成", "待定" = "待定", "不需要" = "不需要")),
        textInput("pfa_filter_keyword", NULL, width = "220px", placeholder = "搜索流程名/部门/对接系统"),
        actionButton("pfa_search", "查询", icon = icon("search"), class = "btn-primary btn-sm"),
        actionButton("pfa_reset", "重置", icon = icon("undo"), class = "btn-default btn-sm")
      ),

      # 流程列表
      div(style = "padding:8px 20px 20px; background:#fafbfc; min-height:400px;",
        uiOutput("pfa_list")
      )
    ),

    # JS: 卡片点击查看详情
    tags$script(HTML("
      $(document).on('click', '.pfa-card', function(e) {
        var id = $(this).data('id');
        Shiny.setInputValue('pfa_view', {id: id, ts: Date.now()}, {priority: 'event'});
      });
    "))
  )
}
