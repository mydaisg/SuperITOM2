# AI 模块 UI — 动态渲染，搜索引擎和AI工具从配置读取
ai_ui <- function() {
  tagList(
    fluidPage(
      titlePanel(""),
      tabsetPanel(
        tabPanel("全网搜索",
          fluidRow(
            column(12,
              h5("输入关键词，同时搜索多个搜索引擎"),
              div(style = "display:flex; gap:8px; margin-bottom:12px;",
                textInput("ai_search_kw", NULL, width = "100%", placeholder = "输入搜索内容..."),
                actionButton("ai_search_btn", "全网搜索", icon = icon("search"), class = "btn-primary")
              ),
              uiOutput("ai_search_frames")
            )
          )
        ),
        tabPanel("全网AI",
          fluidRow(
            column(12,
              h5("输入问题，同时咨询多个 AI"),
              div(style = "display:flex; gap:8px; margin-bottom:12px;",
                textAreaInput("ai_chat_input", NULL, width = "100%", rows = 3, placeholder = "输入你的问题..."),
                actionButton("ai_chat_btn", "全网AI", icon = icon("robot"), class = "btn-success")
              ),
              uiOutput("ai_chat_frames")
            )
          )
        ),
        tabPanel("配置",
          icon = icon("cog"),
          conditionalPanel("output.ai_is_admin",
            h5("搜索引擎配置"),
            uiOutput("ai_search_config"),
            tags$hr(),
            h5("AI 对话工具配置"),
            uiOutput("ai_chat_config")
          ),
          conditionalPanel("!output.ai_is_admin",
            tags$div(style = "text-align:center;padding:40px;color:#999;", "仅管理员可访问")
          )
        )
      )
    )
  )
}
