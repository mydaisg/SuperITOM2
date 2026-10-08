library(shiny)

ui <- fluidPage(
  dateInput("dr_date", "日期", value = Sys.Date()),
  actionButton("dr_today", "今天"),
  actionButton("dr_yesterday", "昨天"),
  actionButton("dr_this_week", "本周"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  dr_month_mode <- reactiveVal(NULL)
  runs <- reactiveVal(0)

  observeEvent(input$dr_today, {
    dr_month_mode(NULL)
    updateDateInput(session, "dr_date", value = Sys.Date())
  })
  observeEvent(input$dr_yesterday, {
    dr_month_mode(NULL)
    updateDateInput(session, "dr_date", value = Sys.Date() - 1)
  })
  observeEvent(input$dr_this_week, {
    d <- Sys.Date()
    monday <- d - as.integer(format(d, "%u")) + 1
    sunday <- min(monday + 6, d)
    dr_month_mode(list(start = monday, end = sunday, label = "本周"))
    updateDateInput(session, "dr_date", value = monday)
  })

  observeEvent(list(input$dr_date, dr_month_mode()), {
    req(input$dr_date)
    runs(runs() + 1)
    output$out <- renderText({
      mm <- dr_month_mode()
      paste("runs=", runs(),
            "| date=", as.character(input$dr_date),
            "| mode=", if (is.null(mm)) "NULL" else mm$label)
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
}

testServer(server, {
  cat("初始 runs:", runs(), "\n")

  session$setInputs(dr_yesterday = 1)
  session$flushReact()
  cat("点昨天后: runs=", runs(), " date=", as.character(input$dr_date), "\n")

  session$setInputs(dr_this_week = 1)
  session$flushReact()
  cat("点本周后: runs=", runs(), " date=", as.character(input$dr_date), "\n")

  cat("最终 output$out:\n")
  print(output$out)
})
