# 流程模块 V2 - UI 层
# 借鉴泛微OA标准流程页面样式（sp/workflow/flowpage/view）
# 核心特性：
#   - 顶部进度步骤条（横向流程图，状态图标：已通过/审批中/已驳回）
#   - 三栏布局：左侧基本信息 + 中间表单内容 + 右侧审批记录时间线
#   - 卡片式视觉，操作按钮融入底部

process_v2_ui <- function() {
  tagList(
    tags$style(HTML("
      /* ===== 主容器 ===== */
      .prv2-wrap { padding:0; }
      .prv2-header {
        background: linear-gradient(135deg, #1890ff, #096dd9);
        color: #fff; padding: 14px 18px; border-radius: 8px 8px 0 0;
        display: flex; align-items: center; justify-content: space-between;
      }
      .prv2-header h2 { margin:0; font-size:18px; font-weight:600; }
      .prv2-header .sub { font-size:12px; opacity:0.85; margin-top:2px; }

      /* ===== 进度条（借鉴泛微标准流程）===== */
      .prv2-stepper { display:flex; align-items:center; padding:18px 20px;
        background:#fafbfc; border-bottom:1px solid #e8e8e8; overflow-x:auto; }
      .prv2-step { display:flex; align-items:center; flex-shrink:0; }
      .prv2-step-icon { width:36px; height:36px; border-radius:50%; display:flex;
        align-items:center; justify-content:center; font-size:14px; font-weight:600;
        margin-right:10px; border:2px solid #d9d9d9; background:#fff; color:#999;
        transition:all 0.2s; }
      .prv2-step-icon .fa, .prv2-step-icon .glyphicon { font-size:14px; }
      .prv2-step-text { line-height:1.3; }
      .prv2-step-text .name { font-size:13px; font-weight:600; color:#333; }
      .prv2-step-text .meta { font-size:11px; color:#999; margin-top:2px; }
      .prv2-step-arrow { color:#d9d9d9; margin:0 14px; font-size:18px; flex-shrink:0; }

      /* 状态色：approved/active/pending/rejected/skipped */
      .prv2-step.approved .prv2-step-icon { background:#52c41a; border-color:#389e0d; color:#fff; }
      .prv2-step.approved .prv2-step-text .name { color:#389e0d; }
      .prv2-step.active .prv2-step-icon { background:#fff; border-color:#fa8c16; color:#fa8c16;
        box-shadow:0 0 0 4px rgba(250,140,22,0.15); animation:prv2-pulse 1.5s ease-in-out infinite; }
      .prv2-step.active .prv2-step-text .name { color:#fa8c16; }
      .prv2-step.rejected .prv2-step-icon { background:#f5222d; border-color:#a8071a; color:#fff; }
      .prv2-step.rejected .prv2-step-text .name { color:#cf1322; }
      .prv2-step.skipped .prv2-step-icon { background:#f0f0f0; border-color:#d9d9d9; color:#bfbfbf; }
      .prv2-step.skipped .prv2-step-text .name { color:#999; text-decoration:line-through; }

      @keyframes prv2-pulse {
        0%, 100% { box-shadow: 0 0 0 4px rgba(250,140,22,0.15); }
        50% { box-shadow: 0 0 0 8px rgba(250,140,22,0.08); }
      }

      /* ===== 主体三栏布局 ===== */
      .prv2-body { display:grid; grid-template-columns: 1fr 360px;
        gap:0; min-height: 480px; background:#fff; }
      .prv2-form-panel { padding: 20px; border-right: 1px solid #e8e8e8; }
      .prv2-side-panel { padding: 20px; background: #fafbfc; }

      /* ===== 卡片 ===== */
      .prv2-card { background:#fff; border:1px solid #e8e8e8; border-radius:6px;
        padding:16px; margin-bottom:14px; box-shadow:0 1px 2px rgba(0,0,0,0.03); }
      .prv2-card-title { font-size:14px; font-weight:600; color:#333;
        margin-bottom:12px; display:flex; align-items:center; gap:6px;
        padding-bottom:8px; border-bottom:1px solid #f0f0f0; }
      .prv2-card-title .badge { font-size:11px; padding:2px 8px; }

      /* ===== 信息行（基础信息/申请人/时间） ===== */
      .prv2-info-row { display:flex; margin-bottom:8px; font-size:13px; }
      .prv2-info-label { color:#999; width:80px; flex-shrink:0; }
      .prv2-info-value { color:#333; font-weight:500; flex:1; word-break:break-word; }

      /* ===== 表单字段渲染（key-value 列表）===== */
      .prv2-field { margin-bottom:10px; }
      .prv2-field .lbl { font-size:12px; color:#666; margin-bottom:4px; font-weight:500; }
      .prv2-field .val { font-size:14px; color:#333; line-height:1.6;
        padding:8px 10px; background:#fafbfc; border-radius:4px; min-height:18px; }
      .prv2-field .val.empty { color:#bbb; font-style:italic; }

      /* ===== 审批记录时间线 ===== */
      .prv2-timeline { position:relative; padding-left:24px; }
      .prv2-timeline::before { content:''; position:absolute; left:11px; top:8px;
        bottom:8px; width:2px; background:#e8e8e8; }
      .prv2-timeline-item { position:relative; padding-bottom:16px; }
      .prv2-timeline-item::before { content:''; position:absolute; left:-19px; top:6px;
        width:10px; height:10px; border-radius:50%; background:#fff;
        border:2px solid #d9d9d9; z-index:1; }
      .prv2-timeline-item.approve::before { border-color:#52c41a; background:#52c41a; }
      .prv2-timeline-item.reject::before  { border-color:#f5222d; background:#f5222d; }
      .prv2-timeline-item.urge::before    { border-color:#fa8c16; background:#fa8c16; }
      .prv2-timeline-item.withdraw::before { border-color:#8c8c8c; background:#8c8c8c; }
      .prv2-timeline-item.submit::before   { border-color:#1890ff; background:#1890ff; }
      .prv2-timeline-item .head { font-size:13px; font-weight:600; color:#333; }
      .prv2-timeline-item .meta { font-size:11px; color:#999; margin-top:2px; }
      .prv2-timeline-item .body { font-size:12px; color:#666; margin-top:4px;
        padding:6px 8px; background:#fff; border-radius:3px; border:1px solid #f0f0f0; }

      /* ===== 操作按钮 ===== */
      .prv2-actions { display:flex; gap:8px; padding:14px 20px;
        background:#fafbfc; border-top:1px solid #e8e8e8; border-radius:0 0 8px 8px; }
      .prv2-actions .btn { font-size:13px; }

      /* ===== 列表页样式 ===== */
      .prv2-card-tpl { background:#fff; border:1px solid #e8e8e8; border-radius:6px;
        padding:14px; margin-bottom:10px; transition:all 0.2s; }
      .prv2-card-tpl:hover { border-color:#1890ff; box-shadow:0 2px 8px rgba(24,144,255,0.15); }
      .prv2-card-tpl .tpl-name { font-size:14px; font-weight:600; color:#1890ff; }
      .prv2-card-tpl .tpl-desc { font-size:12px; color:#666; margin-top:4px; }
      .prv2-card-tpl .tpl-meta { font-size:11px; color:#999; margin-top:6px; display:flex; gap:8px; }

      /* ===== 状态徽章 ===== */
      .prv2-badge { display:inline-block; padding:2px 10px; border-radius:10px;
        font-size:11px; font-weight:600; }
      .prv2-badge.approved { background:#f6ffed; color:#389e0d; border:1px solid #b7eb8f; }
      .prv2-badge.pending  { background:#fff7e6; color:#d46b08; border:1px solid #ffd591; }
      .prv2-badge.rejected { background:#fff1f0; color:#cf1322; border:1px solid #ffa39e; }
      .prv2-badge.withdrawn{ background:#f5f5f5; color:#595959; border:1px solid #d9d9d9; }
      .prv2-badge.draft    { background:#f0f5ff; color:#1d39c4; border:1px solid #adc6ff; }
      .prv2-badge.published{ background:#e6f7ff; color:#08979c; border:1px solid #87e8de; }

      /* ===== 统计 ===== */
      .prv2-stat-card { background:#fff; border:1px solid #e8e8e8; border-radius:6px;
        padding:14px 16px; display:flex; align-items:center; gap:12px; }
      .prv2-stat-card .icon-wrap { width:42px; height:42px; border-radius:8px;
        display:flex; align-items:center; justify-content:center; font-size:18px; flex-shrink:0; }
      .prv2-stat-card .num { font-size:22px; font-weight:700; line-height:1; }
      .prv2-stat-card .lbl { font-size:12px; color:#999; margin-top:4px; }

      /* ===== 输入域（弹窗内）===== */
      .prv2-tpl-builder { background:#fafbfc; border:1px solid #e8e8e8; border-radius:6px;
        padding:12px; margin-bottom:10px; }
      .prv2-tpl-builder h6 { font-size:12px; font-weight:600; color:#1890ff; margin:0 0 8px; }

      /* 响应式 */
      @media (max-width: 992px) {
        .prv2-body { grid-template-columns: 1fr; }
        .prv2-form-panel { border-right: none; border-bottom: 1px solid #e8e8e8; }
      }
    ")),

    div(class = "prv2-wrap",
      # 顶部标题
      div(class = "prv2-header",
        div(
          h2(icon("clipboard-check"), " 流程审批"),
          div(class = "sub", "标准流程 · 步骤进度 · 全程留痕（V2 重构版，借鉴泛微OA流程中心）")
        ),
        div(
          actionButton("prv2_refresh_all", "", icon = icon("sync"),
            class = "btn-sm", style = "background:rgba(255,255,255,0.2); color:#fff; border-color:rgba(255,255,255,0.3);")
        )
      ),

      # 统计卡片
      div(style = "padding:16px 20px; background:#fff;",
        fluidRow(
          column(3, uiOutput("prv2_stat_pending")),
          column(3, uiOutput("prv2_stat_done")),
          column(3, uiOutput("prv2_stat_cc")),
          column(3, uiOutput("prv2_stat_tpl"))
        )
      ),

      # 主标签
      div(style = "padding:0 20px 20px; background:#fff;",
        tabsetPanel(id = "prv2_tabs", type = "pills",

          # ── 待审批 ──
          tabPanel("待审批", icon = icon("bell"),
            br(),
            uiOutput("prv2_pending_list")
          ),

          # ── 我发起的 ──
          tabPanel("我发起的", icon = icon("paper-plane"),
            br(),
            div(style = "margin-bottom:10px;",
              selectInput("prv2_my_status", NULL, width = "180px",
                choices = c("全部" = "", "审批中" = "pending", "已通过" = "approved",
                            "已驳回" = "rejected", "已撤销" = "withdrawn"))),
            uiOutput("prv2_my_list")
          ),

          # ── 我处理的 ──
          tabPanel("我处理的", icon = icon("check-double"),
            br(),
            uiOutput("prv2_done_list")
          ),

          # ── 抄送我的 ──
          tabPanel("抄送我的", icon = icon("share-square"),
            br(),
            uiOutput("prv2_cc_list")
          ),

          # ── 审批模板 ──
          tabPanel("审批模板", icon = icon("sitemap"),
            br(),
            div(style = "margin-bottom:12px; display:flex; gap:8px; align-items:center;",
              actionButton("prv2_new_tpl", "新建模板", icon = icon("plus"),
                class = "btn-primary btn-sm"),
              actionButton("prv2_create_demo", "示例模板", icon = icon("magic"),
                class = "btn-info btn-sm"),
              tags$span(style = "color:#999; font-size:12px; margin-left:8px;",
                "提示：先创建模板→发布→才能发起审批")
            ),
            uiOutput("prv2_tpl_list")
          )
        )
      )
    ),

    # JS: 行内按钮
    tags$script(HTML("
      $(document).on('click', '.prv2-act', function(e) {
        e.preventDefault();
        var btn = $(this);
        var action = btn.data('action');
        var id = btn.data('id');
        var confirmMsg = btn.data('confirm');
        if (confirmMsg && !window.confirm(confirmMsg)) return;
        Shiny.setInputValue('prv2_act', {action: action, id: id, ts: Date.now()}, {priority: 'event'});
      });
      $(document).on('click', '.prv2-view', function(e) {
        e.preventDefault();
        var id = $(this).data('id');
        var mode = $(this).data('mode') || 'instance';
        Shiny.setInputValue('prv2_view', {id: id, mode: mode, ts: Date.now()}, {priority: 'event'});
      });
    "))
  )
}