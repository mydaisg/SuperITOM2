library(shiny)

ui <- fluidPage(
  textInput("dr_date", "日期", value = "2026-10-08"),
  actionButton("dr_yesterday", "昨天"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  observeEvent(input$dr_yesterday, {
    cat("[server] 点击昨天，准备 updateTextInput 为 2026-10-07\n")
    updateTextInput(session, "dr_date", value = "2026-10-07")
    cat("[server] updateTextInput 已调用\n")
  })

  observeEvent(input$dr_date, {
    cat("[server] input$dr_date 变化了！新值 =", input$dr_date, "\n")
  })

  output$out <- renderText({ input$dr_date })
}

testServer(server, {
  session$setInputs(dr_date = "2026-10-08")
  session$flushReact()
  cat("初始 dr_date =", input$dr_date, "\n")

  session$setInputs(dr_yesterday = 1)
  session$flushReact()
  cat("点昨天后 dr_date =", input$dr_date, "\n")
})
