# 流程实例数据模块 — 服务端

flow_monitor_server <- function(input, output, session, rv) {

  fmo_trigger <- reactiveVal(0)

  # 当前查看的批次 id
  fmo_current_batch <- reactiveVal(NULL)
  # 最近一次生成看板的结果
  fmo_gen_result <- reactiveVal(NULL)

  # 操作人
  fmo_operator <- function() {
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) {
      op <- rv$current_user$display_name[1]
      if (is.null(op) || nchar(trimws(op)) == 0) op <- rv$current_user$username[1]
      op
    } else "系统"
  }

  # 导入 Excel 明细
  observeEvent(input$fmo_import, {
    req(rv$logged_in)
    f <- input$fmo_file
    if (is.null(f)) {
      showNotification("请先选择要导入的 Excel 文件", type = "warning")
      return()
    }
    showNotification("正在导入数据...", type = "message", duration = NULL, id = "fmo_working")
    res <- flow_monitor_import_excel(f$datapath, f$name, fmo_operator())
    removeNotification(id = "fmo_working")

    output$fmo_import_result <- renderUI({
      if (isTRUE(res$success)) {
        tags$div(style = "color:#2e7d32;",
          icon("check-circle"), sprintf(" 导入成功：批次 %s，共 %d 条记录", res$batch_no, res$count))
      } else {
        tags$div(style = "color:#c62828;", icon("times-circle"), " ", res$message)
      }
    })
    if (isTRUE(res$success)) {
      showNotification("导入成功", type = "message", duration = 3)
      fmo_trigger(fmo_trigger() + 1)
    }
  })

  # 批次列表
  output$fmo_batch_list <- renderUI({
    req(rv$logged_in)
    fmo_trigger()
    batches <- flow_monitor_get_batches()
    if (nrow(batches) == 0) {
      return(tags$p(style = "color:#999; text-align:center; padding:20px 0;", "暂无数据批次"))
    }
    do.call(tagList, lapply(seq_len(nrow(batches)), function(i) {
      b <- batches[i, ]
      tags$div(class = "fmo-batch-row",
        tags$div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
          tags$b(b$batch_no),
          tags$span(style = "font-size:13px; color:#333;", b$src_name),
          tags$span(style = "font-size:11px; color:#999;", sprintf("共 %d 条", b$total)),
          tags$span(style = "font-size:11px; color:#999;", b$created_at),
          tags$span(style = "flex:1;"),
          tags$button(type = "button", class = "btn btn-xs btn-default fmo-view-btn",
            `data-id` = b$id, icon("table"), " 查看明细"),
          tags$button(type = "button", class = "btn btn-xs btn-success fmo-gen-btn",
            `data-id` = b$id, icon("chart-area"), " 生成看板")
        )
      )
    }))
  })

  # 查看明细
  observeEvent(input$fmo_view_click, {
    req(rv$logged_in)
    fmo_current_batch(as.integer(input$fmo_view_click$id))
    fmo_gen_result(NULL)
  })

  # 生成看板
  observeEvent(input$fmo_generate_click, {
    req(rv$logged_in)
    bid <- as.integer(input$fmo_generate_click$id)
    showNotification("正在从数据库生成看板...", type = "message", duration = NULL, id = "fmo_gen")
    res <- flow_monitor_generate_html(bid)
    removeNotification(id = "fmo_gen")

    if (isTRUE(res$success)) {
      s <- res$stats
      showNotification(
        sprintf("看板生成成功：%d 条流程，完成率 %s%%", s$total, s$completion_rate),
        type = "message", duration = 5)
      fmo_gen_result(res)
    } else {
      showNotification(res$message, type = "error", duration = 8)
    }
  })

  # 明细表格
  output$fmo_detail_table <- DT::renderDataTable({
    req(rv$logged_in)
    bid <- fmo_current_batch()
    req(bid)
    df <- flow_monitor_get_data(bid)
    if (nrow(df) == 0) return(data.frame())
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # 明细展示区（含生成看板结果）
  output$fmo_detail <- renderUI({
    req(rv$logged_in)
    bid <- fmo_current_batch()
    gen <- fmo_gen_result()

    if (is.null(bid) && is.null(gen)) return(NULL)

    tagList(
      # 生成看板结果（若有）
      if (!is.null(gen)) tagList(
        h5("看板生成结果"),
        div(class = "fvz-path-box", icon("folder-open"), " ", gen$html_path),
        br(),
        tags$a(href = paste0("www/flow_viz/", gen$out_name), target = "_blank",
               class = "fvz-link", icon("external-link-alt"), " 在浏览器新窗口打开看板"),
        hr()
      ),
      # 批次明细
      if (!is.null(bid)) tagList(
        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
          h5(icon("table"), " 批次明细", style = "margin:0;"),
          {
            batches <- flow_monitor_get_batches()
            info <- batches[batches$id == bid, ]
            if (nrow(info) > 0) tags$span(style = "color:#666; font-size:12px;",
              sprintf("%s · %s · 共 %d 条", info$batch_no[1], info$src_name[1], info$total[1]))
          }
        ),
        DT::DTOutput("fmo_detail_table")
      )
    )
  })

  # 手动刷新
  observeEvent(input$fmo_refresh, {
    req(rv$logged_in)
    fmo_trigger(fmo_trigger() + 1)
  })
}
