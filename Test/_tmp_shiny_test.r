# 最小 Shiny 测试：验证 dateInput + updateDateInput + observeEvent 模式
library(shiny)

ui <- fluidPage(
  dateInput("dr_date", "日期", value = Sys.Date(), width = "140px"),
  actionButton("dr_today", "今天"),
  actionButton("dr_yesterday", "昨天"),
  actionButton("dr_this_week", "本周"),
  br(), br(),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  dr_month_mode <- reactiveVal(NULL)

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
    mm <- dr_month_mode()
    output$out <- renderPrint({
      cat("dr_date =", as.character(input$dr_date), "\n")
      cat("month_mode =", if (is.null(mm)) "NULL" else mm$label, "\n")
    })
  })
}

shinyApp(ui, server)
