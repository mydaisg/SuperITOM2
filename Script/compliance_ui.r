# 合规模块 UI（框架）
compliance_ui <- function() {
  tagList(
    tags$style(HTML("
      .cmp-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .cmp-cap h3 { margin:0; color:#fff; }
      .cmp-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
    ")),
    fluidRow(
      column(12,
        div(class = "cmp-cap", style = "background:linear-gradient(135deg,#1565c0,#42a5f5);",
          h3(icon("balance-scale"), " 合规管理"),
          p("IPO 审计信息化 · 规则库 · 解决 · 计划 · 行动 · 持续改善（框架）")
        )
      )
    ),
    tabsetPanel(
      id = "compliance_tabs", type = "pills",
      # ── 规则库 ──
      tabPanel("规则库",
        br(),
        fluidRow(
          column(3, selectInput("cmp_filter_cat", "分类", choices = c("全部分类"="", compliance_get_categories()), width = "100%")),
          column(4, textInput("cmp_search", NULL, width = "100%", placeholder = "搜索规则...")),
          column(3, actionButton("cmp_add_btn", "新增合规规则", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;"))
        ),
        br(),
        uiOutput("cmp_list")
      ),
      # ── 解决 ──
      tabPanel("解决",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("check-double", "fa-4x"), br(), br(),
          h4("合规解决（框架）"),
          p("合规问题的解决方案库。待后续实现。")
        )
      ),
      # ── 计划 ──
      tabPanel("计划",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("calendar-alt", "fa-4x"), br(), br(),
          h4("合规计划（框架）"),
          p("合规改进计划与里程碑。待后续实现。")
        )
      ),
      # ── 行动 ──
      tabPanel("行动",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("running", "fa-4x"), br(), br(),
          h4("合规行动（框架）"),
          p("合规行动项与执行跟踪。待后续实现。")
        )
      ),
      # ── 持续改善 ──
      tabPanel("持续改善",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("sync-alt", "fa-4x"), br(), br(),
          h4("持续改善（框架）"),
          p("PDCA 循环、合规成熟度评估。待后续实现。")
        )
      )
    )
  )
}
