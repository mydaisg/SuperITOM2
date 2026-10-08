# 验证修复后的日期筛选逻辑（模拟 dr_apply + dr_trigger 机制）
library(shiny)

ui <- fluidPage(actionButton("dr_yesterday", "昨天"), textOutput("out"))

server <- function(input, output, session) {
  dr_month_mode <- reactiveVal(NULL)
  dr_selected_date <- reactiveVal(Sys.Date())
  dr_trigger <- reactiveVal(0)

  dr_apply <- function(sel_date, mode = NULL) {
    dr_selected_date(sel_date)
    dr_month_mode(mode)
    dr_trigger(dr_trigger() + 1)
  }

  observeEvent(input$dr_yesterday, {
    dr_apply(Sys.Date() - 1, NULL)
  })

  observeEvent(list(dr_trigger(), dr_month_mode()), {
    req(dr_selected_date())
    output$out <- renderText({
      paste("date =", as.character(dr_selected_date()),
            "| trigger =", dr_trigger())
    })
  })
}

testServer(server, {
  cat("初始 date =", as.character(dr_selected_date()), " trigger =", dr_trigger(), "\n")
  session$setInputs(dr_yesterday = 1)
  session$flushReact()
  cat("点昨天后 date =", as.character(dr_selected_date()), " trigger =", dr_trigger(), "\n")
  print(output$out)
})
