# 工具模块 UI
tools_ui <- function() {
  tagList(
    fluidPage(
      titlePanel(""),
      tabsetPanel(id = "tools_tabs",
        tabPanel("文本格式化",
          fluidRow(
            column(6,
              h5("输入"),
              textAreaInput("tool_text_in", NULL, width = "100%", rows = 15,
                placeholder = "每行一条文本..."),
              div(style = "display:flex; gap:8px; align-items:center;",
                selectInput("tool_sep", "分隔符", choices = c("逗号 ," = ",", "分号 ;" = ";", "空格" = " ", "逗号+空格" = ", ", "分号+空格" = "; "), width = "160px"),
                checkboxInput("tool_quote", "加引号", FALSE),
                actionButton("tool_format_btn", "格式化 →", icon = icon("arrow-right"), class = "btn-primary"),
                actionButton("tool_clear_btn", "清空", icon = icon("trash"), class = "btn-sm btn-default"),
                actionButton("tool_reverse_btn", "反向(行转列)", icon = icon("exchange"), class = "btn-sm btn-info")
              )
            ),
            column(6,
              h5("输出"),
              tags$div(style = "position:relative;",
                verbatimTextOutput("tool_text_out"),
                tags$button(class = "btn btn-xs", style = "position:absolute; top:4px; right:8px;",
                  onclick = "var t=document.getElementById('tool_text_out'); if(t){navigator.clipboard.writeText(t.innerText); this.textContent='已复制'; setTimeout(function(){this.textContent='复制'}.bind(this),1500)}", "复制")
              )
            )
          )
        )
      )
    )
  )
}
