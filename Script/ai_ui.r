# AI 模块 UI
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
              tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
                tags$div(style = "flex:1; min-width:45%;",
                  tags$b("🔍 Google"), tags$br(),
                  tags$iframe(id = "ai_google", style = "width:100%;height:400px;border:1px solid #ddd;border-radius:4px;")
                ),
                tags$div(style = "flex:1; min-width:45%;",
                  tags$b("🔍 Bing"), tags$br(),
                  tags$iframe(id = "ai_bing", style = "width:100%;height:400px;border:1px solid #ddd;border-radius:4px;")
                )
              ),
              tags$div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-top:12px;",
                tags$div(style = "flex:1; min-width:45%;",
                  tags$b("🔍 百度"), tags$br(),
                  tags$iframe(id = "ai_baidu", style = "width:100%;height:400px;border:1px solid #ddd;border-radius:4px;")
                ),
                tags$div(style = "flex:1; min-width:45%;",
                  tags$b("🔍 DuckDuckGo"), tags$br(),
                  tags$iframe(id = "ai_ddg", style = "width:100%;height:400px;border:1px solid #ddd;border-radius:4px;")
                )
              )
            )
          )
        ),
        tabPanel("全网AI",
          fluidRow(
            column(12,
              h5("输入问题，同时咨询多个免费 AI"),
              div(style = "display:flex; gap:8px; margin-bottom:12px;",
                textAreaInput("ai_chat_input", NULL, width = "100%", rows = 3, placeholder = "输入你的问题..."),
                actionButton("ai_chat_btn", "全网AI", icon = icon("robot"), class = "btn-success")
              ),
              tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
                tags$div(style = "flex:1; min-width:30%;",
                  tags$b("🤖 ChatGPT"), tags$br(),
                  tags$iframe(id = "ai_chatgpt", style = "width:100%;height:500px;border:1px solid #ddd;border-radius:4px;")
                ),
                tags$div(style = "flex:1; min-width:30%;",
                  tags$b("🤖 Claude"), tags$br(),
                  tags$iframe(id = "ai_claude", style = "width:100%;height:500px;border:1px solid #ddd;border-radius:4px;")
                ),
                tags$div(style = "flex:1; min-width:30%;",
                  tags$b("🤖 DeepSeek"), tags$br(),
                  tags$iframe(id = "ai_deepseek", style = "width:100%;height:500px;border:1px solid #ddd;border-radius:4px;")
                )
              )
            )
          )
        )
      )
    )
  )
}
