# 方案执行模块 - 服务端

solution_exec_server <- function(input, output, session, rv) {

  exec_trigger <- reactiveVal(0)

  # ── 通用统计渲染 ──
  render_exec_stats <- function(tbl, status_col, labels) {
    renderUI({
      exec_trigger()
      s <- exec_get_stats(tbl, status_col)
      fluidRow(
        column(3, div(class = "exec-stat-item", style = "background:#e3f2fd;",
          div(class = "n", style = "color:#1565c0;", s$total), div(class = "l", labels[1]))),
        if (!is.null(s$done)) column(3, div(class = "exec-stat-item", style = "background:#e8f5e9;",
          div(class = "n", style = "color:#2e7d32;", s$done), div(class = "l", labels[2]))),
        if (!is.null(s$pending)) column(3, div(class = "exec-stat-item", style = "background:#fff3e0;",
          div(class = "n", style = "color:#e65100;", s$pending), div(class = "l", labels[3])))
      )
    })
  }

  # ── 通用表格渲染 ──
  render_exec_table <- function(tbl, cols, col_names, status_col = NULL) {
    DT::renderDataTable({
      exec_trigger()
      items <- exec_get_table(tbl)
      if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))
      disp <- data.frame(ID = items$id, stringsAsFactors = FALSE, check.names = FALSE)
      for (i in seq_along(cols)) {
        disp[[col_names[i]]] <- items[[cols[i]]] %||% ""
      }
      # 操作列
      if (!is.null(status_col)) {
        disp$操作 <- sprintf(
          '<select class="exec-status-select" data-tbl="%s" data-id="%d" onchange="Shiny.setInputValue(\'exec_status_change\',{tbl:\'%s\',id:%d,val:this.value},{priority:\'event\'})">%s</select>',
          tbl, items$id, tbl, items$id,
          paste0(
            '<option value="', items[[status_col]], '" selected>', items[[status_col]] %||% '--', '</option>',
            '<option value="待测试">待测试</option>',
            '<option value="测试中">测试中</option>',
            '<option value="已通过">已通过</option>',
            '<option value="有问题">有问题</option>'
          )
        )
      }
      DT::datatable(disp, rownames = FALSE, escape = FALSE,
        options = list(pageLength = 25, dom = "ltip", scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))),
        class = "cell-border stripe compact")
    })
  }

  # ── 培训计划 ──
  output$exec_train_stats <- render_exec_stats("exec_train_plan", NULL, c("培训项"))
  output$exec_train_table <- render_exec_table("exec_train_plan",
    c("seq","module","content","target","duration","method","plan_date","responsible","remark"),
    c("序号","培训模块","培训内容","培训对象","培训时长","培训方式","计划日期","负责人","备注"))

  # ── 试运行计划 ──
  output$exec_pilot_stats <- render_exec_stats("exec_pilot_plan", NULL, c("阶段"))
  output$exec_pilot_table <- render_exec_table("exec_pilot_plan",
    c("phase","phase_name","time_range","departments","tasks","deliverables","responsible","acceptance"),
    c("阶段","阶段名称","起止时间","参与部门","主要任务","交付物","负责人","验收标准"))

  # ── 基础资料 ──
  output$exec_basic_stats <- render_exec_stats("exec_basic_data", "status", c("资料项","已维护","待维护"))
  output$exec_basic_table <- DT::renderDataTable({
    exec_trigger()
    items <- exec_get_table("exec_basic_data")
    if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))
    disp <- data.frame(
      ID = items$id, 序号 = items$seq %||% "", 所属部门 = items$department %||% "",
      资料类别 = items$data_type %||% "", 资料名称 = items$data_name %||% "",
      维护责任人 = items$responsible %||% "", 完成时限 = items$deadline %||% "",
      状态 = sprintf('<select class="exec-status-select" data-tbl="exec_basic_data" data-id="%d" onchange="Shiny.setInputValue(\'exec_status_change\',{tbl:\'exec_basic_data\',id:%d,val:this.value},{priority:\'event\'})">%s</select>',
        items$id, items$id,
        paste0('<option value="', items$status %||% '', '" selected>', items$status %||% '--', '</option>',
          '<option value="待维护">待维护</option><option value="维护中">维护中</option><option value="已完成">已完成</option>')),
      备注 = items$remark %||% "",
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(disp, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 25, dom = "ltip", scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))),
      class = "cell-border stripe compact")
  })

  # ── 测试用例（通用） ──
  .build_test_table <- function(tbl) {
    DT::renderDataTable({
      exec_trigger()
      items <- exec_get_table(tbl)
      if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))
      disp <- data.frame(
        ID = items$id, 用例编号 = items$case_no %||% "", 测试模块 = items$module %||% "",
        测试场景 = items$scenario %||% "", 测试步骤 = items$steps %||% "",
        预期结果 = items$expected %||% "", 实际结果 = items$actual %||% "",
        状态 = sprintf('<select class="exec-status-select" data-tbl="%s" data-id="%d" onchange="Shiny.setInputValue(\'exec_status_change\',{tbl:\'%s\',id:%d,val:this.value},{priority:\'event\'})">%s</select>',
          tbl, items$id, tbl, items$id,
          paste0('<option value="', items$status %||% '', '" selected>', items$status %||% '--', '</option>',
            '<option value="待测试">待测试</option><option value="测试中">测试中</option><option value="已通过">已通过</option><option value="有问题">有问题</option>')),
        优先级 = items$priority %||% "", 测试人 = items$tester %||% "",
        测试日期 = items$test_date %||% "", 问题描述 = items$issue_desc %||% "",
        stringsAsFactors = FALSE, check.names = FALSE)
      DT::datatable(disp, rownames = FALSE, escape = FALSE,
        options = list(pageLength = 25, dom = "ltip", scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))),
        class = "cell-border stripe compact")
    })
  }

  output$exec_hr_stats    <- render_exec_stats("exec_test_hr", "status", c("用例","已通过","待测试"))
  output$exec_admin_stats <- render_exec_stats("exec_test_admin", "status", c("用例","已通过","待测试"))
  output$exec_fin_stats   <- render_exec_stats("exec_test_fin", "status", c("用例","已通过","待测试"))
  output$exec_it_stats    <- render_exec_stats("exec_test_it", "status", c("用例","已通过","待测试"))

  output$exec_hr_table    <- .build_test_table("exec_test_hr")
  output$exec_admin_table <- .build_test_table("exec_test_admin")
  output$exec_fin_table   <- .build_test_table("exec_test_fin")
  output$exec_it_table    <- .build_test_table("exec_test_it")

  # ── 问题反馈 ──
  output$exec_issue_stats <- render_exec_stats("exec_issues", "status", c("问题","已修复","待处理"))
  output$exec_issue_table <- DT::renderDataTable({
    exec_trigger()
    items <- exec_get_table("exec_issues")
    if (nrow(items) == 0) return(data.frame(提示 = "暂无数据"))
    disp <- data.frame(
      ID = items$id, 问题编号 = items$issue_no %||% "", 问题标题 = items$title %||% "",
      所属模块 = items$module %||% "", 问题类型 = items$issue_type %||% "",
      严重程度 = items$severity %||% "", 问题描述 = items$description %||% "",
      状态 = sprintf('<select class="exec-status-select" data-tbl="exec_issues" data-id="%d" onchange="Shiny.setInputValue(\'exec_status_change\',{tbl:\'exec_issues\',id:%d,val:this.value},{priority:\'event\'})">%s</select>',
        items$id, items$id,
        paste0('<option value="', items$status %||% '', '" selected>', items$status %||% '--', '</option>',
          '<option value="待处理">待处理</option><option value="处理中">处理中</option><option value="已修复">已修复</option><option value="已关闭">已关闭</option>')),
      提交人 = items$reporter %||% "", 提交日期 = items$report_date %||% "",
      责任人 = items$responsible %||% "", 优化建议 = items$suggestion %||% "",
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(disp, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 25, dom = "ltip", scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))),
      class = "cell-border stripe compact")
  })

  # ── 状态变更 ──
  observeEvent(input$exec_status_change, {
    req(rv$logged_in)
    data <- input$exec_status_change
    tbl <- data$tbl; id <- as.integer(data$id); val <- data$val
    result <- exec_update_field(tbl, id, "status", val)
    exec_trigger(exec_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"), duration = 1.5)
  })
}
