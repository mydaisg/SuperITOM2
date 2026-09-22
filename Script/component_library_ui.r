# 组件库模块 UI（框架）
component_library_ui <- function() {
  tagList(
    tags$style(HTML("
      .cpt-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .cpt-cap h3 { margin:0; color:#fff; }
      .cpt-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
    ")),
    fluidRow(
      column(12,
        div(class = "cpt-cap", style = "background:linear-gradient(135deg,#4a148c,#7b1fa2);",
          h3(icon("puzzle-piece"), " 标准组件库"),
          p("落实 HungFo 思想 · 前后端分离 · 数据分离 · 配置可选独立（框架）")
        )
      )
    ),
    fluidRow(
      column(3, selectInput("cpt_filter_cat", "组件分类", choices = c("全部分类"="", component_get_categories()), width = "100%")),
      column(4, textInput("cpt_search", NULL, width = "100%", placeholder = "搜索组件...")),
      column(3, actionButton("cpt_add_btn", "登记组件", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;"))
    ),
    br(),
    uiOutput("cpt_list")
  )
}
