# 元任务模块 UI
meta_task_ui <- function() {
  tags$div(style = "padding-bottom:80px;",
    tags$h3(icon("code-branch"), " 元任务机制"),
    tags$p(style = "color:#7f8c8d;", "记录 CodeBuddy 自动执行元任务的逻辑和规则。优化时同步更新。"),
    tags$hr(),
    uiOutput("mt_rules_display"),
    tags$hr(),
    conditionalPanel("output.mt_is_admin",
      actionButton("mt_edit_btn", "编辑规则", icon = icon("edit"), class = "btn-warning btn-sm")
    )
  )
}
