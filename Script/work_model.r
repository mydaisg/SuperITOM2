# 工作模型 — 纯 HTML/CSS 架构图

work_model_mermaid <- function(dept = "IT部") {
  tagList(
    tags$style(HTML("
      .wm-page { font-family:'Microsoft YaHei',sans-serif; padding:20px 12px; background:linear-gradient(160deg,#f8fafc,#e2e8f0); min-height:85vh; }
      .wm-title { text-align:center; margin-bottom:16px; font-family:'Microsoft YaHei',sans-serif; }
      .wm-title h2 { font-size:22px; font-weight:800; color:#1e293b; margin:0; font-family:'Microsoft YaHei',sans-serif; }
      .wm-canvas { position:relative; max-width:1060px; margin:0 auto; height:540px; overflow:visible; }
      .wm-canvas svg { position:absolute; top:0; left:0; width:100%; height:100%; pointer-events:none; z-index:1; overflow:visible; }
      .wm-canvas svg line { stroke:#94a3b8; stroke-width:2.5; stroke-linecap:round; }
      .wm-canvas svg line.dash { stroke-dasharray:7 5; stroke-width:2; }
      .wm-canvas svg line.xfer { stroke:#94a3b8; stroke-width:3.5; stroke-dasharray:7 5; }
      .wm-canvas svg polyline { fill:none; stroke-width:2; }
      .wm-canvas svg polyline.xfer { stroke:#94a3b8; stroke-width:3.5; }

      .wm-box { position:absolute; background:#fff; border-radius:14px; padding:10px 14px; box-shadow:0 2px 10px rgba(0,0,0,0.05); z-index:2; display:flex; flex-direction:column; }
      .wm-box-tag { font-size:12px; font-weight:700; padding:3px 12px; border-radius:4px; color:#fff; margin-bottom:6px; font-family:'Microsoft YaHei',sans-serif; align-self:flex-start; }
      .wm-box-tag.c1 { background:#3b82f6; }
      .wm-box-tag.c2 { background:#ef4444; }
      .wm-box-tag.c3 { background:#f59e0b; }
      .wm-box-tag.c4 { background:#8b5cf6; }
      .wm-box-tag.c5 { background:#06b6d4; }
      .wm-box-tag.c6 { background:#10b981; }

      .wm-flow { display:flex; align-items:center; gap:0; flex-wrap:nowrap; }
      .wm-node { padding:5px 10px; border-radius:20px; font-size:12px; font-weight:600; white-space:nowrap; font-family:'Microsoft YaHei',sans-serif; }
      .wm-node.c1 { background:#dbeafe; color:#1e40af; }
      .wm-node.c2 { background:#fee2e2; color:#991b1b; }
      .wm-node.c3 { background:#fef3c7; color:#92400e; }
      .wm-node.c4 { background:#ede9fe; color:#5b21b6; }
      .wm-node.c5 { background:#cffafe; color:#155e75; }
      .wm-node.c6 { background:#d1fae5; color:#065f46; }
      .wm-node.done { background:#22c55e; color:#fff; }
      .wm-node.xfer { background:#f1f5f9; color:#475569; border:1.5px solid #cbd5e1; padding:4px 8px; font-size:11px; }
      .wm-arrow { font-size:14px; color:#94a3b8; margin:0 5px; flex-shrink:0; font-weight:800; }
      .wm-arrow-sep { font-size:14px; color:#94a3b8; margin:0 4px; flex-shrink:0; font-weight:800; }
      .wm-vlist { display:flex; flex-direction:column; gap:3px; align-items:center; }

      .wm-legends { display:flex; justify-content:center; gap:14px; margin-top:12px; flex-wrap:wrap; font-family:'Microsoft YaHei',sans-serif; }
      .wm-legends span { font-size:11px; color:#64748b; display:flex; align-items:center; gap:4px; }
      .wm-legends .dot { width:9px; height:9px; border-radius:2px; }
    "))
  ,
    tags$div(class = "wm-page",
      tags$div(class = "wm-title",
        tags$h2(paste0("研发中心-", dept, " 工作模型"))
      ),
      tags$div(class = "wm-canvas",

        # ===== SVG 连线（加粗转问题/转项目箭头）=====
        tags$svg(
          tags$defs(
            tags$marker(id="arrowXfer", markerWidth=8, markerHeight=6, refX=7, refY=3, orient="auto",
              tags$polygon(points="0 0, 8 3, 0 6", fill="#94a3b8"))
          ),
          # 巡检反馈 → 转问题
          tags$line(x1=280,y1=135,x2=420,y2=200,stroke="#94a3b8",class="xfer","marker-end"="url(#arrowXfer)"),
          # 工单作业反馈 → 转问题
          tags$line(x1=730,y1=135,x2=590,y2=200,stroke="#94a3b8",class="xfer","marker-end"="url(#arrowXfer)"),
          # 问题执行/验证 → 转项目
          tags$line(x1=510,y1=275,x2=510,y2=365,stroke="#94a3b8",class="xfer","marker-end"="url(#arrowXfer)")
        ),

        # ===== 第一行：1巡检 | 2工单 =====
        tags$div(class = "wm-box", style = "left:20px; top:18px; width:450px;",
          tags$div(class = "wm-box-tag c1", "1、IT巡检巡查"),
          tags$div(class = "wm-flow",
            tags$span(class = "wm-node c1", "计划"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node c1", "检查任务"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node c1", "反馈"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node done", "完结"),
            tags$span(class = "wm-arrow-sep", "|"),
            tags$span(class = "wm-node xfer", "转问题")
          )
        ),
        tags$div(class = "wm-box", style = "left:510px; top:18px; width:450px;",
          tags$div(class = "wm-box-tag c2", "2、IT服务工单"),
          tags$div(class = "wm-flow",
            tags$span(class = "wm-node c2", "服务请求"),
            tags$span(class = "wm-node c2", "IT事件"),
            tags$span(class = "wm-node c2", "IT需求"),
            tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node c2", "作业反馈"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node done", "交付"),
            tags$span(class = "wm-arrow-sep", "|"),
            tags$span(class = "wm-node xfer", "转问题")
          )
        ),

        # ===== 第二行：3问题 =====
        tags$div(class = "wm-box", style = "left:290px; top:200px; width:400px;",
          tags$div(class = "wm-box-tag c3", "3、IT问题管理"),
          tags$div(class = "wm-flow",
            tags$span(class = "wm-node c3", "调研"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node c3", "方案"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node c3", "执行/验证"), tags$span(class = "wm-arrow", "→"),
            tags$span(class = "wm-node done", "交付结案"),
            tags$span(class = "wm-arrow-sep", "|"),
            tags$span(class = "wm-node xfer", "转项目")
          )
        ),

        # ===== 第三行：5发起 | 4项目 | 6处理 =====
        tags$div(class = "wm-box", style = "left:20px; top:385px; width:210px;",
          tags$div(class = "wm-box-tag c5", "5、流程·发起"),
          tags$div(class = "wm-vlist",
            tags$span(class = "wm-node c5", "采购申请"),
            tags$span(class = "wm-node c5", "合同"),
            tags$span(class = "wm-node c5", "培训"),
            tags$span(class = "wm-node c5", "费用"),
            tags$span(class = "wm-node c5", "......")
          )
        ),
        tags$div(class = "wm-box", style = "left:255px; top:385px; width:470px;",
          tags$div(class = "wm-box-tag c4", "4、项目管理"),
          tags$div(class = "wm-vlist",
            tags$span(class = "wm-node c4", "目标范围"),
            tags$span(class = "wm-arrow", "↓"),
            tags$span(class = "wm-node c4", "阶段"),
            tags$span(class = "wm-arrow", "↓"),
            tags$span(class = "wm-node c4", "工作包"),
            tags$span(class = "wm-arrow", "↓"),
            tags$span(class = "wm-node c4", "任务"),
            tags$span(class = "wm-arrow", "↓"),
            tags$span(class = "wm-node c4", "作业反馈"),
            tags$span(class = "wm-arrow", "↓"),
            tags$span(class = "wm-node done", "结案")
          )
        ),
        tags$div(class = "wm-box", style = "left:750px; top:385px; width:230px;",
          tags$div(class = "wm-box-tag c6", "6、流程·处理"),
          tags$div(class = "wm-vlist",
            tags$span(class = "wm-node c6", "账号/权限/变更/优化申请"),
            tags$span(class = "wm-node c6", "工单"),
            tags$span(class = "wm-node c6", "资产"),
            tags$span(class = "wm-node c6", "......")
          )
        )
      ),
      tags$div(class = "wm-legends",
        tags$span(tags$span(class = "dot", style = "background:#3b82f6;"), "巡检"),
        tags$span(tags$span(class = "dot", style = "background:#ef4444;"), "工单"),
        tags$span(tags$span(class = "dot", style = "background:#f59e0b;"), "问题"),
        tags$span(tags$span(class = "dot", style = "background:#8b5cf6;"), "项目"),
        tags$span(tags$span(class = "dot", style = "background:#06b6d4;"), "发起"),
        tags$span(tags$span(class = "dot", style = "background:#10b981;"), "处理"),
        tags$span(tags$span(class = "dot", style = "background:#22c55e;"), "完结/交付/结案"),
        tags$span(tags$span(class = "dot", style = "background:#f1f5f9; border:1px solid #cbd5e1;"), "转问题/转项目"),
        tags$span("— 虚线 = 跨模块关联")
      )
    )
  )
}
