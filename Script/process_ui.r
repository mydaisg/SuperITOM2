# 流程模块 UI
# 流程引擎：实现各模块的流程化与协作
# 开发中...

process_ui <- function() {
  fluidPage(
    div(style = "text-align: center; margin: 80px 20px;",
      h1(icon("project-diagram"), " 流程引擎"),
      hr(style = "width: 200px;"),
      p("流程引擎开发中，敬请期待...",
        style = "color: #7f8c8d; font-size: 18px; margin-top: 20px;")
    )
  )
}
