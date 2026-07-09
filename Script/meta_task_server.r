# 元任务模块 Server
meta_task_server <- function(input, output, session, rv) {

  # Admin 权限
  is_admin <- reactive({
    !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
  })
  output$mt_is_admin <- reactive({ is_admin() })
  outputOptions(output, "mt_is_admin", suspendWhenHidden = FALSE)

  # 初始化种子数据
  observe({
    req(rv$logged_in)
    meta_task_init_seed()
  })

  # 当前规则
  mt_rules <- reactiveVal()
  observe({
    req(rv$logged_in)
    mt_rules(meta_task_get_rules())
  })

  # 渲染规则展示
  output$mt_rules_display <- renderUI({
    req(rv$logged_in)
    r <- mt_rules()
    if (is.null(r)) return(tags$p("加载中..."))

    tagList(
      # 基本信息卡片
      tags$div(style = "background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:16px;margin-bottom:16px;",
        tags$div(style = "display:flex;align-items:center;gap:16px;margin-bottom:12px;",
          tags$span(style = "font-size:18px;font-weight:bold;color:#1a237e;", icon("code-branch"), " ", r$title),
          tags$span(style = "font-family:Consolas,monospace;font-size:12px;background:#ede9fe;color:#5b21b6;padding:2px 10px;border-radius:10px;", r$note_no)
        ),
        if (!is.null(r$updated_at)) tags$div(style = "font-size:11px;color:#999;", "最后更新：", r$updated_at)
      ),

      # 核心规则
      tags$div(style = "background:#e8f5e9;border:1px solid #c8e6c9;border-radius:8px;padding:16px;margin-bottom:16px;",
        tags$h4(icon("cogs"), " 核心工作流", style = "margin-top:0;color:#2e7d32;"),
        tags$pre(style = "background:#fff;padding:12px;border-radius:6px;font-size:13px;white-space:pre-wrap;line-height:1.8;margin:0;", r$rules)
      ),

      # 开发日志规范
      tags$div(style = "background:#e3f2fd;border:1px solid #bbdefb;border-radius:8px;padding:16px;margin-bottom:16px;",
        tags$h4(icon("file-alt"), " 开发日志规范", style = "margin-top:0;color:#1565c0;"),
        tags$pre(style = "background:#fff;padding:12px;border-radius:6px;font-size:13px;white-space:pre-wrap;line-height:1.8;margin:0;", r$dev_log_spec)
      ),

      # 同步规则
      tags$div(style = "background:#fff3e0;border:1px solid #ffe0b2;border-radius:8px;padding:16px;",
        tags$h4(icon("sync"), " 规则同步", style = "margin-top:0;color:#e65100;"),
        tags$pre(style = "background:#fff;padding:12px;border-radius:6px;font-size:13px;white-space:pre-wrap;line-height:1.8;margin:0;", r$sync_rule)
      )
    )
  })

  # 编辑弹窗
  observeEvent(input$mt_edit_btn, {
    req(rv$logged_in, is_admin())
    r <- mt_rules()
    showModal(modalDialog(
      title = "编辑元任务规则",
      textInput("mt_edit_title", "标题", value = r$title, width = "100%"),
      textInput("mt_edit_note_no", "跟踪记事编号", value = r$note_no, width = "100%"),
      textAreaInput("mt_edit_rules", "核心工作流", value = r$rules, rows = 6, width = "100%"),
      textAreaInput("mt_edit_dev_log", "开发日志规范", value = r$dev_log_spec, rows = 6, width = "100%"),
      textAreaInput("mt_edit_sync", "规则同步", value = r$sync_rule, rows = 4, width = "100%"),
      footer = tagList(
        actionButton("mt_save_btn", "保存规则", class = "btn-primary"),
        modalButton("取消")
      ),
      size = "l", easyClose = TRUE
    ))
  })

  observeEvent(input$mt_save_btn, {
    req(rv$logged_in)
    result <- meta_task_save_rules(
      input$mt_edit_title, input$mt_edit_note_no,
      input$mt_edit_rules, input$mt_edit_dev_log, input$mt_edit_sync
    )
    if (result$success) {
      removeModal()
      mt_rules(meta_task_get_rules())
      showNotification("规则已保存", type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })
}
