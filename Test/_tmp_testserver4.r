library(shiny)

ui <- fluidPage(
  textInput("dr_date", "日期", value = "2026-10-08"),
  actionButton("dr_today", "今天"),
  actionButton("dr_yesterday", "昨天"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  dr_month_mode <- reactiveVal(NULL)
  runs <- reactiveVal(0)

  observeEvent(input$dr_today, {
    dr_month_mode(NULL)
    updateTextInput(session, "dr_date", value = "2026-10-08")
  })
  observeEvent(input$dr_yesterday, {
    dr_month_mode(NULL)
    updateTextInput(session, "dr_date", value = "2026-10-07")
  })

  observeEvent(list(input$dr_date, dr_month_mode()), {
    req(input$dr_date)
    runs(runs() + 1)
    output$out <- renderText({
      mm <- dr_month_mode()
      paste("runs=", runs(), "| date=", input$dr_date, "| mode=", if (is.null(mm)) "NULL" else "list")
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
}

testServer(server, {
  cat("初始 runs:", runs(), " date=", input$dr_date, "\n")

  session$setInputs(dr_yesterday = 1)
  session$flushReact()
  cat("点昨天后: runs=", runs(), " date=", input$dr_date, "\n")

  session$setInputs(dr_today = 1)
  session$flushReact()
  cat("点今天后: runs=", runs(), " date=", input$dr_date, "\n")

  cat("最终 output:\n")
  print(output$out)
})
