# 方案模块 UI
solution_ui <- function() {
  tagList(
    fluidPage(
      titlePanel(""),
      fluidRow(
        column(3,
          div(style = "display:flex; gap:6px; align-items:center;",
            textInput("sol_search", NULL, width = "100%", placeholder = "搜索方案..."),
            actionButton("sol_search_btn", NULL, icon = icon("search"), class = "btn-sm btn-primary")
          )
        ),
        column(2, selectInput("sol_filter_cat", NULL, choices = c("全部分类"=""), width = "100%")),
        column(2,
          actionButton("sol_create_btn", "新建方案", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;")
        ),
        column(5, div(style = "text-align:right;", uiOutput("sol_stats")))
      ),
      hr(),
      uiOutput("sol_list")
    )
  )
}
