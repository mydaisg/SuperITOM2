# 消息机制 UI — 消息中心 + 动作流模板（类似泛微 ESB 动作流）

message_ui <- function() {
  tagList(
    tags$style(HTML("
      .msg-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .msg-cap h3 { margin:0; color:#fff; }
      .msg-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
      .msg-card { background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:12px 14px; margin-bottom:8px; cursor:pointer; transition:all .15s; border-left:4px solid #ccc; }
      .msg-card:hover { border-color:#337ab7; box-shadow:0 2px 8px rgba(0,0,0,.08); }
      .msg-card.unread { border-left-color:#337ab7; background:#f5f9ff; }
      .msg-badge { display:inline-block; padding:1px 10px; border-radius:10px; font-size:11px; color:#fff; white-space:nowrap; }
      .msg-pri { display:inline-block; width:8px; height:8px; border-radius:50%; margin-right:6px; vertical-align:middle; }
      /* 动作流时间线 */
      .af-timeline { position:relative; padding-left:24px; }
      .af-timeline::before { content:''; position:absolute; left:7px; top:4px; bottom:4px; width:2px; background:#e0e7ff; }
      .af-item { position:relative; padding:6px 0 6px 10px; }
      .af-item::before { content:''; position:absolute; left:-21px; top:12px; width:10px; height:10px; border-radius:50%; background:#6366f1; box-shadow:0 0 0 3px #e0e7ff; }
      .af-item .af-hd { font-size:12px; color:#666; }
      .af-item .af-act { display:inline-block; font-size:11px; background:#ede9fe; color:#5b21b6; border-radius:6px; padding:1px 8px; margin-right:6px; }
      .af-item .af-node { display:inline-block; font-size:11px; background:#e0f2fe; color:#0369a1; border-radius:6px; padding:1px 8px; margin-right:6px; }
      .af-item .af-comment { font-size:13px; color:#333; margin-top:2px; white-space:pre-wrap; }
      /* 动作流模板 */
      .flow-card { background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:12px 14px; margin-bottom:8px; }
      .flow-node { display:inline-block; padding:4px 12px; border-radius:14px; background:#eef2ff; border:1px solid #c7d2fe; color:#4338ca; font-size:12px; }
      .flow-arrow { display:inline-block; color:#a5b4fc; margin:0 3px; font-weight:bold; }
    ")),
    fluidRow(
      column(12,
        div(class = "msg-cap", style = "background:linear-gradient(135deg,#0f2b5c,#6366f1);",
          h3(icon("envelope"), " 消息中心"),
          p("消息收发 · 动作流流转 · 动作流模板（类似泛微 ESB 动作流）")
        )
      )
    ),
    tabsetPanel(
      id = "msg_tabs", type = "pills",
      # ── Tab1：消息中心 ──
      tabPanel("消息中心", icon = icon("envelope"),
        br(),
        uiOutput("msg_center_ui")
      ),
      # ── Tab2：动作流模板 ──
      tabPanel("动作流模板", icon = icon("sitemap"),
        br(),
        uiOutput("msg_flow_ui")
      )
    )
  )
}
