library(shiny)

# 直接验证：list 依赖里含 NULL 元素（reactiveVal 初始 NULL）+ ignoreNULL 的行为
ui <- fluidPage(actionButton("btn", "click"), textOutput("out"))

server <- function(input, output, session) {
  mm <- reactiveVal(NULL)  # 初始 NULL，模拟 dr_month_mode
  runs <- reactiveVal(0)

  # 模拟 observeEvent(470)：依赖 list 含 mm()（初始 NULL）
  observeEvent(list(input$btn, mm()), {
    runs(runs() + 1)
    output$out <- renderText(paste("runs =", runs(), "| btn =", input$btn %||% "NULL"))
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # 点击按钮后设置 mm 为 NULL（不变）并触发 btn
}

testServer(server, {
  cat("=== ignoreNULL=TRUE 场景 ===\n")
  cat("初始 runs:", runs(), "\n")
  session$setInputs(btn = 1)
  session$flushReact()
  cat("点击 btn 后 runs:", runs(), " (btn 变成 1，mm 仍是 NULL)\n")
})
