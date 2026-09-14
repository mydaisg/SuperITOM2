# 钉钉流程历史实例模块 — 服务端

dingtalk_instance_server <- function(input, output, session, rv) {

  dir_trigger <- reactiveVal(0)

  # 分类列表（从 DB 已有数据动态加载）
  dir_categories <- reactive({
    req(rv$logged_in)
    dir_trigger()
    dingtalk_instance_get_categories()
  })

  # 流程列表（按选中的分类过滤）
  dir_flows <- reactive({
    req(rv$logged_in)
    dir_trigger()
    seq <- input$dir_category
    if (is.null(seq) || seq == "") return(data.frame())
    dingtalk_instance_get_flows(as.integer(seq))
  })

  # 更新分类下拉
  observeEvent(dir_categories(), {
    cats <- dir_categories()
    choices <- setNames(as.character(cats$seq_no), sprintf("%s（%d项）", cats$category, cats$flows))
    if (length(choices) == 0) choices <- c("请选择..." = "")
    updateSelectInput(session, "dir_category", choices = choices)
  }, ignoreNULL = FALSE)

  # 更新流程下拉
  observeEvent(dir_flows(), {
    fs <- dir_flows()
    if (nrow(fs) == 0) {
      updateSelectInput(session, "dir_flow", choices = c("全部流程" = "all"))
      return()
    }
    ch <- c("全部流程" = "all")
    for (i in seq_len(nrow(fs))) {
      ch <- c(ch, setNames(as.character(fs$flow_no[i]),
                       sprintf("%d - %s", fs$flow_no[i], fs$flow_name[i])))
    }
    updateSelectInput(session, "dir_flow", choices = ch)
  })

  # 当前选中的 seq_no / flow_no
  dir_seq_no <- reactive({
    s <- input$dir_category
    if (is.null(s) || s == "") NA_integer_ else as.integer(s)
  })
  dir_flow_no <- reactive({
    f <- input$dir_flow
    if (is.null(f) || f == "all") NA_integer_ else as.integer(f)
  })

  # 当前选择的记录集（带筛选）
  dir_records <- reactive({
    req(rv$logged_in)
    dir_trigger()
    seq <- dir_seq_no(); req(!is.na(seq))
    flow <- dir_flow_no()
    dingtalk_instance_get_records(
      seq, flow,
      status_filter = input$dir_status_filter,
      result_filter = input$dir_result_filter)
  })

  # 概要信息 + badge
  output$dir_badge <- renderUI({
    seq <- dir_seq_no(); req(!is.na(seq))
    cats <- dir_categories()
    info <- cats[cats$seq_no == seq, ]
    if (nrow(info) > 0) sprintf("%s", info$category[1]) else ""
  })

  output$dir_summary <- renderUI({
    req(rv$logged_in)
    seq <- dir_seq_no(); if (is.na(seq)) return(tags$span("请选择分类"))
    flow <- dir_flow_no()
    df <- dir_records()
    tags$span(sprintf("共 %d 条记录", nrow(df)))
  })

  # HTML 看板：选中分类变化时自动生成（用 iframe srcdoc 内联，不写文件，避免触发 devmode 热重载）
  output$dir_html_block <- renderUI({
    req(rv$logged_in)
    seq <- dir_seq_no(); if (is.na(seq)) return(NULL)
    flow <- dir_flow_no()
    html <- dingtalk_instance_build_html(seq, flow)
    if (is.null(html)) return(tags$p(style = "color:#999;", "无数据可展示"))
    tags$div(style = "margin:12px 0;",
      tags$iframe(srcdoc = html,
                  style = "width:100%; height:780px; border:1px solid rgba(255,255,255,0.1); border-radius:10px;",
                  sandbox = "allow-scripts allow-same-origin"))
  })

  # 记录表格
  output$dir_records_table <- DT::renderDataTable({
    req(rv$logged_in)
    df <- dir_records()
    if (nrow(df) == 0) return(data.frame())
    disp <- df[, c("file_year", "title", "status", "result", "initiator",
                   "department", "applicant", "amount", "start_time", "end_time")]
    names(disp) <- c("年份", "标题", "状态", "结果", "发起人", "部门",
                     "申请人", "借款金额", "发起时间", "完成时间")
    DT::datatable(disp, options = list(pageLength = 15, scrollX = TRUE, dom = "ltip"), rownames = FALSE)
  })

  # 刷新
  observeEvent(input$dir_refresh, {
    req(rv$logged_in)
    dir_trigger(dir_trigger() + 1)
  })
}