# 行业模块 UI（框架）
industry_ui <- function() {
  tagList(
    tags$style(HTML("
      .ind-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .ind-cap h3 { margin:0; color:#fff; }
      .ind-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
    ")),
    fluidRow(
      column(12,
        div(class = "ind-cap", style = "background:linear-gradient(135deg,#1a237e,#3949ab);",
          h3(icon("industry"), " 行业情报中心"),
          p("行业情报搜集 · 爬虫 · 可被调用的库 · 可推送的库 · 可还原原文的 HTML（框架）")
        )
      )
    ),
    tabsetPanel(
      id = "industry_tabs", type = "pills",
      # ── 情报搜集 ──
      tabPanel("情报搜集",
        br(),
        fluidRow(
          column(3, selectInput("ind_filter_cat", "分类", choices = c("全部分类"="", industry_get_categories()), width = "100%")),
          column(4, textInput("ind_search", NULL, width = "100%", placeholder = "搜索标题/内容...")),
          column(2, actionButton("ind_search_btn", "搜索", icon = icon("search"), class = "btn-primary btn-sm")),
          column(3, actionButton("ind_add_btn", "新增情报", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;"))
        ),
        br(),
        uiOutput("ind_list")
      ),
      # ── 爬虫（框架占位）──
      tabPanel("爬虫",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("spider", "fa-4x"), br(), br(),
          h4("爬虫引擎（框架）"),
          p("配置数据源、抓取规则、调度策略。待后续实现。")
        )
      ),
      # ── 可被调用的库 ──
      tabPanel("可被调用的库",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("database", "fa-4x"), br(), br(),
          h4("可被调用的库（框架）"),
          p("本地情报知识库，供其它模块通过 API 调用。待后续实现。")
        )
      ),
      # ── 可推送的库 ──
      tabPanel("可推送的库",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("paper-plane", "fa-4x"), br(), br(),
          h4("可推送的库（框架）"),
          p("推送订阅、分发渠道、定时推送。待后续实现。")
        )
      ),
      # ── 可还原原文的 HTML ──
      tabPanel("可还原原文的 HTML",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("file-code", "fa-4x"), br(), br(),
          h4("可还原原文的 HTML（框架）"),
          p("结构化 HTML 存档，支持还原原文排版。待后续实现。")
        )
      )
    )
  )
}
