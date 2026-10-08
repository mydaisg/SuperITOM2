library(shiny)

# 用 testServer 验证 observeEvent 的 ignoreNULL 行为
ui <- fluidPage(
  dateInput("dr_date", "日期", value = Sys.Date()),
  actionButton("dr_today", "今天"),
  actionButton("dr_yesterday", "昨天"),
  textOutput("out")
)

server <- function(input, output, session) {
  dr_month_mode <- reactiveVal(NULL)
  counter <- reactiveVal(0)

  observeEvent(input$dr_today, {
    dr_month_mode(NULL)
    updateDateInput(session, "dr_date", value = Sys.Date())
  })
  observeEvent(input$dr_yesterday, {
    dr_month_mode(NULL)
    updateDateInput(session, "dr_date", value = Sys.Date() - 1)
  })

  # 模拟 daily_report 的 observeEvent(470)
  observeEvent(list(input$dr_date, dr_month_mode()), {
    req(input$dr_date)
    counter(counter() + 1)
    output$out <- renderText({
      paste("date =", as.character(input$dr_date), "| runs =", counter())
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
}

testServer(server, {
  cat("初始 counter:", counter(), "\n")
  cat("初始 input$dr_date:", as.character(input$dr_date), "\n")

  # 模拟点击"昨天"
  session$setInputs(dr_yesterday = 1)
  session$flushReact()
  cat("点击昨天后 counter:", counter(), "\n")
  cat("点击昨天后 input$dr_date:", as.character(input$dr_date), "\n")
})
