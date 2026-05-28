# 绩效模块 - 服务端逻辑
# 月绩效表 crud + 工作清单匹配 + 自动计分

performance_server <- function(input, output, session, rv) {
  perf_refresh <- reactiveVal(0)

  # 月份选项
  observe({
    perf_refresh()
    sheets <- perf_sheet_list()
    months <- if (nrow(sheets)>0) sheets$year_month else character(0)
    current <- format(Sys.Date(),"%Y-%m")
    if (!(current %in% months)) months <- c(current, months)
    shiny::updateSelectInput(session,"perf_month",choices=months,selected=current)
  })

  # 当前 sheet_id
  current_sheet <- reactive({
    req(input$perf_month)
    sheet <- perf_sheet_get_by_month(input$perf_month)
    if (is.null(sheet)) {
      # 自动创建
      result <- perf_sheet_create(input$perf_month)
      if (result$success) {
        perf_refresh(perf_refresh()+1)
        perf_sheet_get_by_month(input$perf_month)
      } else NULL
    } else sheet
  })

  # 统计卡片
  output$perf_stat_cards <- renderUI({
    perf_refresh()
    sheets <- perf_sheet_list()
    sheet <- current_sheet()
    employees <- if (!is.null(sheet)) {
      tryCatch(perf_active_employees(input$perf_month),error=function(e)data.frame())
    } else data.frame()
    matched <- if (!is.null(sheet)) {
      tryCatch(nrow(perf_work_items_by_sheet(sheet$id[1])),error=function(e)0)
    } else 0
    fluidRow(
      column(3,div(class="well well-sm",style="text-align:center;padding:12px;",
        div(style="font-size:14px;color:#666;","总月表数"),
        div(style="font-size:26px;font-weight:bold;color:#333;",nrow(sheets)))),
      column(3,div(class="well well-sm",style="text-align:center;padding:12px;",
        div(style="font-size:14px;color:#666;","当月员工数"),
        div(style="font-size:26px;font-weight:bold;color:#337ab7;",nrow(employees)))),
      column(3,div(class="well well-sm",style="text-align:center;padding:12px;",
        div(style="font-size:14px;color:#666;","已匹配项"),
        div(style="font-size:26px;font-weight:bold;color:#27ae60;",matched))),
      column(3,div(class="well well-sm",style="text-align:center;padding:12px;",
        div(style="font-size:14px;color:#666;","当前月份"),
        div(style="font-size:26px;font-weight:bold;color:#e67e22;",input$perf_month%||%"-")))
    )
  })

  # 创建新表
  observeEvent(input$perf_create_sheet, {
    showModal(modalDialog(title="新建月绩效表",
      selectInput("perf_new_month","选择月份",choices=seq(2024,format(Sys.Date(),"%Y")%||%2026),selected=format(Sys.Date(),"%Y")),
      selectInput("perf_new_month2","月份",choices=sprintf("%02d",1:12),selected=format(Sys.Date(),"%m")),
      footer=tagList(modalButton("取消"),actionButton("perf_confirm_create","创建",class="btn-primary")),easyClose=TRUE))
  })

  observeEvent(input$perf_confirm_create, {
    ym <- sprintf("%s-%s",input$perf_new_month,input$perf_new_month2)
    result <- perf_sheet_create(ym)
    removeModal()
    if (result$success) {
      perf_refresh(perf_refresh()+1)
      shiny::updateSelectInput(session,"perf_month",selected=ym)
      showNotification(sprintf("绩效表 %s 已创建",ym),type="message")
    } else showNotification(result$message,type="error")
  })

  # 员工筛选UI
  output$perf_emp_filter_ui <- renderUI({
    perf_refresh()
    emps <- perf_active_employees(input$perf_month)
    choices <- c("全部"="")
    if (nrow(emps)>0) {
      names_vec <- ifelse(is.na(emps$display_name)%||%emps$display_name=="", emps$username, emps$display_name)
      choices <- c(choices, stats::setNames(as.character(emps$id), names_vec))
    }
    selectInput("perf_ws_emp","员工",choices=choices,width="100%")
  })

  # ========== 绩效矩阵表 ==========
  output$perf_matrix_table <- DT::renderDT({
    perf_refresh()
    sheet <- current_sheet()
    if (is.null(sheet)) return(DT::datatable(data.frame(信息="暂无绩效表"),options=list(dom='t')))
    matrix_data <- perf_calculate(sheet$id[1])
    if (nrow(matrix_data)==0) return(DT::datatable(data.frame(信息="暂无匹配数据"),options=list(dom='t')))

    # 构建显示表格
    display <- data.frame(
      类别=matrix_data$category,
      指标=matrix_data$indicator,
      stringsAsFactors=FALSE
    )
    # 员工列
    emp_cols <- setdiff(names(matrix_data), c("indicator","category","code"))
    for (col in emp_cols) {
      display[[col]] <- matrix_data[[col]]
    }
    
    # 渲染样式
    DT::datatable(display,escape=FALSE,rownames=FALSE,
      options=list(pageLength=50,dom='t',scrollX=TRUE,
        columnDefs=list(list(targets=0,width='50px',className='dt-center'),
          list(targets=1,width='200px')))
    ) %>%
      DT::formatStyle(columns=names(display),
        valueColumns="指标",
        backgroundColor=DT::styleEqual("总分", "#fff3cd"))
  })

  # ========== 工作清单（未匹配的） ==========
  output$perf_work_source_table <- DT::renderDT({
    perf_refresh()
    req(input$perf_month)
    sources <- perf_load_work_sources(input$perf_month)
    if (nrow(sources)==0) return(DT::datatable(data.frame(信息="本月无工作记录"),options=list(dom='t')))

    # 筛选
    if (!is.null(input$perf_ws_filter)&&nchar(input$perf_ws_filter)>0) {
      sources <- sources[sources$source_type==input$perf_ws_filter, ]
    }
    if (!is.null(input$perf_ws_emp)&&nchar(input$perf_ws_emp)>0) {
      sources <- sources[sources$employee_id==as.integer(input$perf_ws_emp), ]
    }
    if (nrow(sources)==0) return(DT::datatable(data.frame(信息="无匹配结果"),options=list(dom='t')))

    # 排除已匹配的
    sheet <- current_sheet()
    matched <- if (!is.null(sheet)) perf_work_items_by_sheet(sheet$id[1]) else data.frame()
    if (nrow(matched)>0) {
      # 按 source_type + source_id 排除
      for (i in 1:nrow(matched)) {
        sources <- sources[!(sources$source_type==matched$source_type[i] & sources$source_id==matched$source_id[i]), ]
      }
    }
    if (nrow(sources)==0) return(DT::datatable(data.frame(信息="所有工作项已匹配"),options=list(dom='t')))

    source_nos <- if ("source_no"%in%names(sources)) sources$source_no%||%"" else ""
    display <- data.frame(
      来源=sources$source_type,
      单号=source_nos,
      标题=substr(sources$source_title%||%"",1,50),
      员工=sources$employee_name%||%sources$username,
      操作=sprintf('<button class="btn btn-success btn-xs perf-match-btn" data-emp="%s" data-stype="%s" data-sid="%s" data-stitle="%s">匹配</button>',
        sources$employee_id, sources$source_type, sources$source_id,
        gsub('"',"&quot;",substr(gsub("'","",sources$source_title%||%""),1,30))),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=20,dom='rtip',scrollX=TRUE,
      columnDefs=list(list(targets=4,orderable=FALSE),
        list(targets=1,width='140px'))),rownames=FALSE,class='cell-border stripe hover')
  })

  # 匹配弹窗
  observeEvent(input$perf_match_click, {
    data <- input$perf_match_click
    showModal(modalDialog(title="匹配到绩效指标",
      p(sprintf("来源: %s | 标题: %s | 员工ID: %s", data$source_type, data$source_title, data$emp_id)),
      selectInput("perf_match_indicator","选择指标",choices=NULL),
      conditionalPanel(
        condition="input.perf_match_indicator.startsWith('A')",
        selectInput("perf_match_level","扣分等级",choices=c("请选择"="0","1级"="1","2级"="2","3级"="3"))
      ),
      footer=tagList(modalButton("取消"),actionButton("perf_confirm_match","确认匹配",class="btn-success")),
      easyClose=TRUE))
    # 填充指标选项
    inds <- perf_indicators()
    choices <- list()
    for (ind in inds) choices[[sprintf("%s-%s",ind$code,ind$name)]] <- ind$code
    shiny::updateSelectInput(session,"perf_match_indicator",choices=choices)
  })

  observeEvent(input$perf_confirm_match, {
    req(input$perf_match_click, input$perf_match_indicator)
    data <- input$perf_match_click
    sheet <- current_sheet()
    req(sheet)
    level <- if (startsWith(input$perf_match_indicator,"A")) as.integer(input$perf_match_level%||%0) else 0
    result <- perf_work_item_add(
      sheet_id=sheet$id[1],
      employee_id=as.integer(data$emp_id),
      indicator_code=input$perf_match_indicator,
      source_type=data$source_type,
      source_id=as.integer(data$source_id),
      source_title=data$source_title,
      deduction_level=level)
    removeModal()
    if (result$success) {
      perf_refresh(perf_refresh()+1)
      showNotification("已匹配到指标",type="message")
    } else showNotification(result$message,type="error")
  })

  # ========== 已匹配清单（含得分） ==========
  output$perf_matched_table <- DT::renderDT({
    perf_refresh()
    sheet <- current_sheet()
    if (is.null(sheet)) return(DT::datatable(data.frame(信息="暂无数据"),options=list(dom='t')))
    items <- perf_work_items_by_sheet(sheet$id[1])
    if (nrow(items)==0) return(DT::datatable(data.frame(信息="暂无匹配记录"),options=list(dom='t')))
    # 计算每项得分
    scores <- mapply(function(code, lvl) perf_item_score(code, lvl),
      items$indicator_code, items$deduction_level)
    display <- data.frame(
      员工=items$employee_name,
      指标=items$indicator_name,
      得分=scores,
      来源=items$source_type,
      标题=items$source_title%||%"",
      扣分等级=ifelse(is.na(items$deduction_level)%||%items$deduction_level==0,"-",sprintf("%d级",items$deduction_level)),
      操作=sprintf('<button class="btn btn-danger btn-xs perf-unmatch-btn" data-id="%s">移除</button>', items$id),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=20,dom='rtip',scrollX=TRUE,
      columnDefs=list(list(targets=6,orderable=FALSE),
        list(targets=2,className='dt-center'))),rownames=FALSE,class='cell-border stripe hover')
  })

  # 移除匹配
  observeEvent(input$perf_unmatch_click, {
    perf_work_item_remove(as.integer(input$perf_unmatch_click))
    perf_refresh(perf_refresh()+1)
    showNotification("已移除",type="message")
  })
}
