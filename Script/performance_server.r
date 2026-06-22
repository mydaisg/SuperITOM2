# 绩效模块服务端 — 参照 note_server/sysmon_server 完全重写
# ★ 关键修复：header 先渲染创建 perf_month 选择器，main 后渲染依赖它
# ★ 避免 renderUI 中创建 perf_month 同时又 req(input$perf_month) 的循环依赖

performance_server <- function(input, output, session, rv) {
  perf_refresh <- reactiveVal(0)
  is_admin <- reactive({
    !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
  })
  current_uid <- reactive({
    if (is.null(rv$current_user) || nrow(rv$current_user) == 0) NULL else rv$current_user$id[1]
  })

  ##################
  # Header：标题 + 统计卡片 + 月份选择器（不依赖 perf_month）
  ##################
  output$perf_header <- renderUI({
    req(rv$logged_in); perf_refresh()
    sheets <- perf_sheet_list()
    months <- if (nrow(sheets) > 0) sheets$year_month else character(0)
    current <- format(Sys.Date(), "%Y-%m")
    if (!(current %in% months)) months <- c(current, months)

    # 预计算统计（不依赖 perf_month）
    employees <- tryCatch(perf_active_employees(current), error = function(e) data.frame())
    # 当前月绩效表员工列表
    sheet <- tryCatch(perf_sheet_get_by_month(current), error = function(e) NULL)
    sheet_emps <- if (!is.null(sheet)) tryCatch(perf_sheet_employee_list(sheet$id[1]), error = function(e) data.frame()) else data.frame()
    has_sheet_emps <- nrow(sheet_emps) > 0

    emp_tags <- if (has_sheet_emps) {
      tagList(lapply(seq_len(min(nrow(sheet_emps), 8)), function(i) {
        nm <- ifelse(is.na(sheet_emps$display_name[i]) | sheet_emps$display_name[i] == "", sheet_emps$username[i], sheet_emps$display_name[i])
        tags$span(class = "label label-info", style = "display:inline-block;margin:2px;font-size:12px;",
          nm,
          tags$span(style = "cursor:pointer;margin-left:4px;font-weight:bold;",
            `data-id` = sheet_emps$id[i], class = "perf-emp-remove", HTML("&times;")))
      }),
      if (nrow(sheet_emps) > 8) tags$span(class = "label label-default", sprintf("...共 %d 人", nrow(sheet_emps))) else "")
    } else {
      tags$span(style = "color:#999;font-size:12px;", "（未添加员工，将从全部活跃用户显示）")
    }

    tagList(
      div(style = "text-align:center;margin:10px 0 5px;",
        h2(icon("chart-bar"), " 绩效管理"),
        p(style = "color:#7f8c8d;font-size:12px;",
          "月度绩效评分 | 指标×员工矩阵 | 工单/项目/巡检工作清单")),
      fluidRow(
        column(3, div(class = "well well-sm", style = "text-align:center;padding:12px;",
          div(style = "font-size:14px;color:#666;", "总月表数"),
          div(style = "font-size:26px;font-weight:bold;color:#333;", nrow(sheets)))),
        column(3, div(class = "well well-sm", style = "text-align:center;padding:12px;",
          div(style = "font-size:14px;color:#666;", "当月员工数"),
          div(style = "font-size:26px;font-weight:bold;color:#337ab7;", if(has_sheet_emps) nrow(sheet_emps) else nrow(employees)))),
        column(3, div(class = "well well-sm", style = "text-align:center;padding:12px;",
          div(style = "font-size:14px;color:#666;", "当前月份"),
          div(style = "font-size:26px;font-weight:bold;color:#e67e22;", current))),
        column(3, div(class = "well well-sm", style = "text-align:center;padding:12px;",
          div(style = "font-size:14px;color:#666;", "指标总数"),
          div(style = "font-size:26px;font-weight:bold;color:#27ae60;", length(perf_indicators()))))
      ),
      hr(),
      fluidRow(
        column(2, selectInput("perf_month", "选择月份", choices = months, selected = current, width = "100%")),
        column(2, div(style = "margin-top:25px;",
          actionButton("perf_create_sheet", "新建月表", class = "btn-primary btn-sm", icon = icon("plus")))),
        column(2, div(style = "margin-top:25px;",
          actionButton("perf_add_employee", "添加员工", class = "btn-warning btn-sm", icon = icon("user-plus")))),
        column(2, div(style = "margin-top:25px;",
          actionButton("perf_refresh_btn", "刷新", class = "btn-info btn-sm"))),
        column(2, div(style = "margin-top:25px;",
          actionButton("perf_manual_add", "手工添加", class = "btn-success btn-sm", icon = icon("plus")))),
        column(2, div(style = "margin-top:25px;",
          actionButton("perf_rate", "结果评定", class = "btn-warning btn-sm", icon = icon("clipboard-check"))))
      ),
      # 已添加员工标签行
      div(style = "margin:10px 0 5px;",
        tags$b("已添加员工："), emp_tags
      ),
      tags$script(HTML("
        $(document).on('click','.perf-emp-remove',function(e){
          e.stopPropagation();
          if(confirm('确定移除此员工？')) {
            Shiny.setInputValue('perf_remove_employee', $(this).data('id'), {priority:'event'});
          }
        });
      "))
    )
  })

  ##################
  # 当前 sheet（依赖 perf_month）
  ##################
  current_sheet <- reactive({
    req(input$perf_month)
    sheet <- perf_sheet_get_by_month(input$perf_month)
    if (is.null(sheet) || nrow(sheet) == 0) {
      result <- perf_sheet_create(input$perf_month)
      if (result$success) {
        perf_refresh(perf_refresh() + 1)
        perf_sheet_get_by_month(input$perf_month)
      } else NULL
    } else sheet
  })

  ##################
  # Main：矩阵 + 工作清单 + 已匹配表（依赖 perf_month，此时已由 header 创建）
  ##################
  output$perf_main <- renderUI({
    req(rv$logged_in, input$perf_month); perf_refresh()
    sheet <- current_sheet()
    matched <- if (!is.null(sheet)) {
      tryCatch(nrow(perf_work_items_by_sheet(sheet$id[1])), error = function(e) 0)
    } else 0

    tagList(
      tags$hr(),
      h4(icon("table"), " 部门绩效汇总表", style = "margin-bottom:10px;"),
      uiOutput("perf_matrix_wrapper"),
      tags$hr(),
      h4(icon("check-circle"), " 工作项清单", style = "margin-bottom:10px;"),
      DT::DTOutput("perf_matched_table"),
      tags$hr(),
      h4(icon("chart-pie"), " 按员工分类统计", style = "margin-bottom:10px;"),
      DT::DTOutput("perf_summary_table"),
      tags$hr(),
      h4(icon("list"), " 工作清单（本月工单/项目任务/巡检）", style = "margin-bottom:10px;"),
      fluidRow(
        column(2, selectInput("perf_ws_filter", "来源",
          choices = c("全部" = "", "工单" = "工单", "项目任务" = "项目任务", "巡检" = "巡检"), width = "100%")),
        column(3, uiOutput("perf_emp_filter_ui"))
      ),
      br(), DT::DTOutput("perf_work_source_table")
    )
  })

  ##################
  # 员工筛选
  ##################
  output$perf_emp_filter_ui <- renderUI({
    req(rv$logged_in, input$perf_month)
    emps <- tryCatch(perf_active_employees(input$perf_month), error = function(e) data.frame())
    # 非admin用户：确保自己出现在列表中
    if (!is_admin()) {
      if (!is.null(current_uid())) {
        if (nrow(emps) == 0 || !any(emps$id == current_uid())) {
          own_info <- tryCatch({
            con <- db_connect()
            r <- dbGetQuery(con, sprintf("SELECT id, display_name, username FROM users WHERE id = %d", current_uid()))
            db_disconnect(con)
            if (nrow(r) > 0) r else NULL
          }, error = function(e) NULL)
          if (!is.null(own_info) && nrow(own_info) > 0) {
            own_info$employee_name <- ifelse(is.na(own_info$display_name[1]) | own_info$display_name[1] == "", own_info$username[1], own_info$display_name[1])
            emps <- rbind(emps, own_info)
          }
        }
      }
      emps <- emps[emps$id == current_uid(), , drop = FALSE]
    }
    choices <- c("全部" = "")
    if (nrow(emps) > 0) {
      names_vec <- ifelse(is.na(emps$display_name) | emps$display_name == "", emps$username, emps$display_name)
      choices <- c(choices, stats::setNames(as.character(emps$id), names_vec))
    }
    selectInput("perf_ws_emp", "员工", choices = choices, width = "100%")
  })

  ##################
  # 绩效矩阵表包装
  ##################
  output$perf_matrix_wrapper <- renderUI({
    req(rv$logged_in); perf_refresh()
    DT::DTOutput("perf_matrix_table")
  })
  # 绩效矩阵（类别·指标·员工计数）
  output$perf_matrix_table <- DT::renderDT({
    req(rv$logged_in); perf_refresh()
    sheet <- current_sheet()
    if (is.null(sheet)) return(DT::datatable(data.frame(信息 = "暂无绩效表"), options = list(dom = 't')))
    # 非admin只看自己的员工记录
    # 算全部矩阵，非admin再遮住别人数据
    calc <- tryCatch(perf_calculate(sheet$id[1]), error = function(e) list(matrix = data.frame(), summary = data.frame()))
    matrix_data <- calc$matrix
    if (nrow(matrix_data) == 0) return(DT::datatable(data.frame(信息 = "暂无匹配数据"), options = list(dom = 't')))
    if (!is_admin() && !is.null(current_uid()) && ncol(matrix_data) > 3) {
      # 找到当前用户在 employees 中的名字
      emp_name <- if (!is.null(rv$current_user)) {
        dn <- rv$current_user$display_name[1]
        if (is.na(dn) || dn == "") rv$current_user$username[1] else dn
      } else ""
      # 前3列是类别/指标/code，第4列起是员工列，最后一列是总分
      emp_cols <- seq(4, ncol(matrix_data) - 1)
      for (ci in emp_cols) {
        cname <- colnames(matrix_data)[ci]
        if (cname != emp_name) {
          matrix_data[[ci]] <- "-"
        }
      }
    }

    DT::datatable(matrix_data, escape = FALSE, rownames = FALSE,
      colnames = c("类别" = "category", "指标" = "indicator"),
      options = list(
        pageLength = 50, dom = 't', scrollX = TRUE,
        columnDefs = list(
          list(targets = 2, visible = FALSE),      # 隐藏 code
          list(targets = "_all", className = "dt-center")   # 全部居中
        ),
        rowCallback = DT::JS("function(row, data) {
          var ind = data[1];

          var fullMap = {'A类得分（30分）':30, 'B类得分（40分）':40, 'C类得分（30分）':30};
          if (fullMap.hasOwnProperty(ind)) {
            var full = fullMap[ind];
            $('td', row).each(function(i) {
              var v = parseFloat($(this).text());
              if (!isNaN(v) && i > 1) {
                var isLast = (i === $('td', row).length - 1);
                if (!isLast) {
                  if (v >= full) { $(this).css({'background-color':'#d4edda','color':'#155724'}); }
                  else if (v === 0) { $(this).css({'background-color':'#e0e0e0','color':'#999'}); }
                  else { $(this).css({'background-color':'#f8d7da','color':'#721c24'}); }
                }
              }
            });
          }
          // 总分/绩效得分行：加粗
          if (ind === '总分' || ind.indexOf('绩效得分') >= 0) {
            $('td', row).css({'background-color':'#f0f4f8','font-weight':'bold'});
          }
          // 人工填写行：浅灰背景
          if (ind === '绩效结果' || ind === '标杆' || ind === '签字确认') {
            $('td', row).css('background-color','#fafafa');
          }
          // 对齐：指标列（第2列，索引1）
          var summaryRows = ['A类得分（30分）','B类得分（40分）','C类得分（30分）','总分','绩效得分（10分）','绩效结果','标杆','签字确认'];
          if (summaryRows.indexOf(ind) >= 0) {
            // 汇总行：居右
            $('td:eq(1)', row).css('text-align', 'right');
          } else {
            // 指标行：居左
            $('td:eq(1)', row).css('text-align', 'left');
          }
        }")
      )) %>%
      DT::formatStyle(columns = "指标", valueColumns = "指标",
        fontWeight = DT::styleEqual("总分", "bold"))
  })

  # 人头ABC分类统计表
  output$perf_summary_table <- DT::renderDT({
    req(rv$logged_in); perf_refresh()
    sheet <- current_sheet()
    if (is.null(sheet)) return(NULL)
    calc <- tryCatch(perf_calculate(sheet$id[1]), error = function(e) list(matrix = data.frame(), summary = data.frame()))
    # 非admin只显示自己的行
    if (!is_admin() && !is.null(current_uid())) {
      emp_name <- if (!is.null(rv$current_user)) {
        dn <- rv$current_user$display_name[1]
        if (is.na(dn) || dn == "") rv$current_user$username[1] else dn
      } else ""
      if (nrow(calc$summary) > 0 && "员工" %in% names(calc$summary)) {
        calc$summary <- calc$summary[calc$summary$员工 == emp_name, , drop = FALSE]
      }
    }
    summary_data <- calc$summary
    if (nrow(summary_data) == 0 || "信息" %in% names(summary_data)) return(NULL)

      DT::datatable(summary_data, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 20, dom = 't', columnDefs = list(
        list(targets = c(1,2,3,4,5,6,7,8), className = "dt-center")
      ))) %>%
      DT::formatStyle("绩效得分（10分）", fontWeight = "bold", backgroundColor = "#fff3cd")
  })

  ##################
  # 工作清单表格
  ##################
  output$perf_work_source_table <- DT::renderDT({
    req(rv$logged_in); perf_refresh(); req(input$perf_month)
    sources <- tryCatch(perf_load_work_sources(input$perf_month), error = function(e) data.frame())
    # 非admin只看自己的
    if (!is_admin() && !is.null(current_uid()) && nrow(sources) > 0) {
      sources <- sources[sources$employee_id == current_uid(), , drop = FALSE]
    }
    if (nrow(sources) == 0) return(DT::datatable(data.frame(信息 = "本月无工作记录"), options = list(dom = 't')))
    if (!is.null(input$perf_ws_filter) && nchar(input$perf_ws_filter) > 0)
      sources <- sources[sources$source_type == input$perf_ws_filter, ]
    if (!is.null(input$perf_ws_emp) && nchar(input$perf_ws_emp) > 0)
      sources <- sources[sources$employee_id == as.integer(input$perf_ws_emp), ]
    if (nrow(sources) == 0) return(DT::datatable(data.frame(信息 = "无匹配结果"), options = list(dom = 't')))
    sheet <- current_sheet()
    matched <- if (!is.null(sheet)) {
      tryCatch(perf_work_items_by_sheet(sheet$id[1]), error = function(e) data.frame())
    } else data.frame()
    if (nrow(matched) > 0) {
      for (i in seq_len(nrow(matched)))
        sources <- sources[!(sources$source_type == matched$source_type[i] & sources$source_id == matched$source_id[i]), ]
    }
    if (nrow(sources) == 0) return(DT::datatable(data.frame(信息 = "所有工作项已匹配"), options = list(dom = 't')))
    source_nos <- if ("source_no" %in% names(sources)) sources$source_no else ""
    if (length(source_nos) == 0) source_nos <- rep("", nrow(sources))
    display <- data.frame(
      来源 = sources$source_type, 单号 = source_nos,
      标题 = substr(sources$source_title %||% "", 1, 50),
      员工 = sources$employee_name %||% sources$username,
      操作 = sprintf(
        '<button class="btn btn-success btn-xs perf-match-btn" data-emp="%s" data-stype="%s" data-sid="%s" data-stitle="%s">匹配</button>',
        sources$employee_id, sources$source_type, sources$source_id,
        gsub('"', "&quot;", substr(gsub("'", "", sources$source_title %||% ""), 1, 30))),
      stringsAsFactors = FALSE)
    DT::datatable(display, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 20, dom = 'rtip', scrollX = TRUE,
        columnDefs = list(list(targets = 4, orderable = FALSE))),
      class = 'cell-border stripe hover')
  })

  ##################
  # 工作项清单
  ##################
  output$perf_matched_table <- DT::renderDT({
    req(rv$logged_in); perf_refresh()
    sheet <- current_sheet()
    if (is.null(sheet)) return(DT::datatable(data.frame(信息 = "暂无数据"), options = list(dom = 't')))
    items <- tryCatch(perf_work_items_by_sheet(sheet$id[1]), error = function(e) data.frame())
    # 非admin只看自己的
    if (!is_admin() && !is.null(current_uid()) && nrow(items) > 0) {
      items <- items[items$employee_id == current_uid(), , drop = FALSE]
    }
    if (nrow(items) == 0) return(DT::datatable(data.frame(信息 = "暂无工作项"), options = list(dom = 't')))
    scores <- if (nrow(items) > 0) {
      mapply(function(code, lvl) perf_item_score(code, lvl), items$indicator_code, items$deduction_level)
    } else numeric(0)
    # B/C类 扣分等级显示"无"
    ded_level <- ifelse(is.na(items$deduction_level) | items$deduction_level == 0,
      ifelse(grepl("^[BC]", items$indicator_code), "无", "-"),
      sprintf("%d级", items$deduction_level))
    display <- data.frame(
      指标 = items$indicator_name,
      工作项 = items$source_title %||% "",
      员工 = items$employee_name,
      得分 = scores,
      来源 = items$source_type,
      扣分等级 = ded_level,
      操作 = paste0(
        sprintf('<button class="btn btn-info btn-xs perf-edit-btn" data-id="%s" data-icode="%s" data-iname="%s" data-ititle="%s" data-ilevel="%s">编辑</button> ',
          items$id, items$indicator_code, items$indicator_name,
          gsub("'", "&#39;", items$source_title %||% ""),
          items$deduction_level %||% 0),
        sprintf('<button class="btn btn-danger btn-xs perf-unmatch-btn" data-id="%s">移除</button>', items$id)
      ),
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(display, escape = FALSE, rownames = FALSE,
      options = list(pageLength = -1, dom = 't', scrollY = FALSE,
        columnDefs = list(list(targets = 6, orderable = FALSE)),
        rowCallback = DT::JS("function(row, data) {
          var colors = ['#FFB3BA','#BAFFC9','#BAE1FF','#FFFFBA','#FFDFBA','#E6E6FA','#FFD1DC','#B0E0E6','#98FB98','#F0E68C','#FFA07A','#DDA0DD','#87CEEB','#90EE90','#FFE4B5','#FFB6C1'];
          if (data[1] !== '') {
            var hash = 0, s = data[1];
            for (var i = 0; i < s.length; i++) hash = ((hash << 5) - hash) + s.charCodeAt(i);
            $('td:eq(1)', row).css('background-color', colors[Math.abs(hash) % colors.length]);
          }
        }")
      ),
      class = 'cell-border stripe hover')
  })

  ##################
  # 事件处理
  ##################
  observeEvent(input$perf_refresh_btn, { req(rv$logged_in); perf_refresh(perf_refresh() + 1) })

  # 添加员工到当月绩效表 → 弹窗
  observeEvent(input$perf_add_employee, {
    req(rv$logged_in, input$perf_month)
    sheet <- current_sheet()
    req(sheet)
    # 获取全部活跃用户（非仅表内员工）
    all_users <- tryCatch({
      con <- db_connect()
      on.exit(db_disconnect(con))
      dbGetQuery(con, "SELECT id, display_name, username FROM users WHERE active = 1 ORDER BY display_name")
    }, error = function(e) data.frame())
    if (nrow(all_users) == 0) {
      showNotification("没有可用员工", type = "warning")
      return()
    }
    # 拼接已添加标记
    existing <- perf_sheet_employee_list(sheet$id[1])
    existing_ids <- if (nrow(existing) > 0) existing$employee_id else integer(0)
    labels <- sapply(seq_len(nrow(all_users)), function(i) {
      nm <- ifelse(is.na(all_users$display_name[i]) | all_users$display_name[i] == "", all_users$username[i], all_users$display_name[i])
      if (all_users$id[i] %in% existing_ids) sprintf("%s（✓ 已添加）", nm) else nm
    })
    choices <- stats::setNames(as.character(all_users$id), labels)
    showModal(modalDialog(
      title = "添加员工到本月绩效表", size = "m",
      checkboxGroupInput("perf_new_employees", "选择员工（带 ✓ 的已添加）", choices = choices),
      footer = tagList(
        modalButton("取消"),
        actionButton("perf_confirm_add_employees", "确认添加", class = "btn-warning")
      ), easyClose = TRUE
    ))
  })

  # 确认添加员工
  observeEvent(input$perf_confirm_add_employees, {
    req(rv$logged_in, input$perf_new_employees)
    sheet <- current_sheet()
    req(sheet)
    result <- perf_sheet_employee_add(sheet$id[1], as.integer(input$perf_new_employees))
    removeModal()
    perf_refresh(perf_refresh() + 1)
    showNotification(result$message, type = "message")
  })

  # 移除员工
  observeEvent(input$perf_remove_employee, {
    req(rv$logged_in, is_admin())
    result <- perf_sheet_employee_remove(as.integer(input$perf_remove_employee))
    perf_refresh(perf_refresh() + 1)
    showNotification(result$message, type = "message")
  })

  # 手工添加工作项 → 弹窗（多选员工，显示已匹配计数）
  observeEvent(input$perf_manual_add, {
    req(rv$logged_in, input$perf_month)
    sheet <- current_sheet()
    emps <- tryCatch(perf_active_employees(input$perf_month), error = function(e) data.frame())
    # 非admin用户：确保自己出现在列表中（即使未被管理员添加或没有历史工单）
    if (!is_admin()) {
      if (!is.null(current_uid())) {
        if (nrow(emps) == 0 || !any(emps$id == current_uid())) {
          own_info <- tryCatch({
            con <- db_connect()
            r <- dbGetQuery(con, sprintf("SELECT id, display_name, username FROM users WHERE id = %d", current_uid()))
            db_disconnect(con)
            if (nrow(r) > 0) {
              r$employee_name <- ifelse(is.na(r$display_name[1]) | r$display_name[1] == "", r$username[1], r$display_name[1])
              r
            } else NULL
          }, error = function(e) NULL)
          if (!is.null(own_info)) emps <- rbind(emps, own_info)
        }
      }
      emps <- emps[emps$id == current_uid(), , drop = FALSE]
    }
    # 查询各员工已匹配计数
    matched_count <- setNames(integer(0), character(0))
    if (!is.null(sheet)) {
      items <- tryCatch(perf_work_items_by_sheet(sheet$id[1]), error = function(e) data.frame())
      if (nrow(items) > 0) {
        cnt <- table(items$employee_id)
        matched_count <- setNames(as.integer(cnt), names(cnt))
      }
    }
    if (nrow(emps) > 0) {
      nv <- ifelse(is.na(emps$display_name) | emps$display_name == "", emps$username, emps$display_name)
      emp_id_str <- as.character(emps$id)
      # 拼接已匹配计数
      labels <- sapply(seq_len(nrow(emps)), function(i) {
        cnt <- matched_count[emp_id_str[i]]
        if (is.na(cnt)) cnt <- 0
        sprintf("%s (已匹配%1.0f项)", nv[i], cnt)
      })
      emp_choices <- stats::setNames(emp_id_str, labels)
    } else {
      emp_choices <- c("无可用员工" = "")
    }
    inds <- perf_indicators()
    ind_choices <- list()
    for (ind in inds) ind_choices[[sprintf("%s-%s", ind$code, ind$name)]] <- ind$code
    showModal(modalDialog(
      title = "手工添加工作项", size = "m",
      checkboxGroupInput("perf_manual_emps", "员工（多选，括号内为已匹配数）", choices = emp_choices),
      selectInput("perf_manual_indicator", "绩效指标", choices = ind_choices),
      textInput("perf_manual_title", "工作描述", placeholder = "简要描述工作内容"),
      conditionalPanel(
        condition = "input.perf_manual_indicator.startsWith('A')",
        selectInput("perf_manual_level", "扣分等级",
          choices = c("请选择" = "0", "1级普通" = "1", "2级严重" = "2", "3级重大" = "3"))
      ),
      footer = tagList(
        modalButton("取消"),
        actionButton("perf_confirm_manual", "确认添加", class = "btn-success")
      ), easyClose = TRUE
    ))
  })

  # 确认手工添加（批量多员工）
  observeEvent(input$perf_confirm_manual, {
    req(rv$logged_in, input$perf_manual_emps, input$perf_manual_indicator)
    sheet <- current_sheet()
    req(sheet)
    level <- if (startsWith(input$perf_manual_indicator, "A")) {
      as.integer(input$perf_manual_level %||% 0)
    } else 0
    title <- trimws(input$perf_manual_title)
    emps <- tryCatch(perf_active_employees(input$perf_month), error = function(e) data.frame())
    # 批量添加
    added <- 0; failed <- 0
    for (emp_id_str in input$perf_manual_emps) {
      emp_id <- as.integer(emp_id_str)
      er <- if (nrow(emps) > 0) emps[emps$id == emp_id, ] else data.frame()
      emp_name <- if (nrow(er) > 0) {
        ifelse(is.na(er$display_name[1]) | er$display_name[1] == "", er$username[1], er$display_name[1])
      } else sprintf("员工#%d", emp_id)
      t <- if (is.null(title) || nchar(title) == 0) sprintf("%s 手工工作项", emp_name) else title
      result <- perf_work_item_add(
        sheet_id = sheet$id[1], employee_id = emp_id,
        indicator_code = input$perf_manual_indicator,
        source_type = "手工", source_id = 0,
        source_title = t, deduction_level = level)
      if (result$success) added <- added + 1 else failed <- failed + 1
    }
    removeModal()
    if (added > 0) {
      perf_refresh(perf_refresh() + 1)
      msg <- sprintf("已为%d位员工添加", added)
      if (failed > 0) msg <- paste0(msg, sprintf("（%d位失败）", failed))
      showNotification(msg, type = "message")
    } else {
      showNotification("添加失败", type = "error")
    }
  })

  observeEvent(input$perf_create_sheet, {
    req(rv$logged_in, is_admin())
    showModal(modalDialog(title = "新建月绩效表",
      selectInput("perf_new_month", "年份", choices = seq(2024, as.integer(format(Sys.Date(), "%Y"))), selected = format(Sys.Date(), "%Y")),
      selectInput("perf_new_month2", "月份", choices = sprintf("%02d", 1:12), selected = format(Sys.Date(), "%m")),
      footer = tagList(modalButton("取消"), actionButton("perf_confirm_create", "创建", class = "btn-primary")),
      easyClose = TRUE))
  })

  observeEvent(input$perf_confirm_create, {
    req(rv$logged_in, is_admin())
    ym <- sprintf("%s-%s", input$perf_new_month, input$perf_new_month2)
    result <- perf_sheet_create(ym)
    removeModal()
    if (result$success) {
      perf_refresh(perf_refresh() + 1)
      shiny::updateSelectInput(session, "perf_month", selected = ym)
      showNotification(sprintf("绩效表 %s 已创建", ym), type = "message")
    } else showNotification(result$message, type = "error")
  })

  observeEvent(input$perf_match_click, {
    req(rv$logged_in)
    data <- input$perf_match_click
    showModal(modalDialog(title = "匹配到绩效指标",
      p(sprintf("来源: %s | 员工ID: %s", data$source_type, data$emp_id)),
      selectInput("perf_match_indicator", "选择指标", choices = NULL),
      conditionalPanel(
        condition = "input.perf_match_indicator.startsWith('A')",
        selectInput("perf_match_level", "扣分等级",
          choices = c("请选择" = "0", "1级" = "1", "2级" = "2", "3级" = "3"))
      ),
      footer = tagList(modalButton("取消"), actionButton("perf_confirm_match", "确认匹配", class = "btn-success")),
      easyClose = TRUE))
    inds <- perf_indicators()
    choices <- list()
    for (ind in inds) choices[[sprintf("%s-%s", ind$code, ind$name)]] <- ind$code
    shiny::updateSelectInput(session, "perf_match_indicator", choices = choices)
  })

  observeEvent(input$perf_confirm_match, {
    req(rv$logged_in, input$perf_match_click, input$perf_match_indicator)
    data <- input$perf_match_click
    sheet <- current_sheet()
    req(sheet)
    level <- if (startsWith(input$perf_match_indicator, "A")) as.integer(input$perf_match_level %||% 0) else 0
    result <- perf_work_item_add(
      sheet_id = sheet$id[1], employee_id = as.integer(data$emp_id),
      indicator_code = input$perf_match_indicator,
      source_type = data$source_type, source_id = as.integer(data$source_id),
      source_title = data$source_title, deduction_level = level)
    removeModal()
    if (result$success) {
      perf_refresh(perf_refresh() + 1)
      showNotification("已匹配到指标", type = "message")
    } else showNotification(result$message, type = "error")
  })

  # 编辑工作项 → 弹窗
  observeEvent(input$perf_edit_click, {
    req(rv$logged_in)
    data <- input$perf_edit_click
    inds <- perf_indicators()
    ind_choices <- list()
    for (ind in inds) ind_choices[[sprintf("%s-%s", ind$code, ind$name)]] <- ind$code
    showModal(modalDialog(
      title = sprintf("编辑工作项 #%s", data$id), size = "m",
      selectInput("perf_edit_indicator", "指标", choices = ind_choices, selected = data$icode),
      textInput("perf_edit_title", "标题", value = data$ititle),
      conditionalPanel(
        condition = "input.perf_edit_indicator.startsWith('A')",
        selectInput("perf_edit_level", "扣分等级",
          choices = c("请选择" = "0", "1级" = "1", "2级" = "2", "3级" = "3"),
          selected = data$ilevel)
      ),
      footer = tagList(modalButton("取消"), actionButton("perf_confirm_edit", "保存", class = "btn-primary")),
      easyClose = TRUE
    ))
    rv$perf_edit_id <- as.integer(data$id)
  })

  # 确认编辑
  observeEvent(input$perf_confirm_edit, {
    req(rv$logged_in, input$perf_edit_indicator, rv$perf_edit_id)
    level <- if (startsWith(input$perf_edit_indicator, "A")) as.integer(input$perf_edit_level %||% 0) else 0
    result <- perf_work_item_update(
      item_id = rv$perf_edit_id,
      indicator_code = input$perf_edit_indicator,
      deduction_level = level,
      source_title = input$perf_edit_title %||% "")
    removeModal()
    if (result$success) {
      perf_refresh(perf_refresh() + 1)
      showNotification("已更新", type = "message")
    } else showNotification(result$message, type = "error")
  })

  observeEvent(input$perf_unmatch_click, {
    req(rv$logged_in)
    perf_work_item_remove(as.integer(input$perf_unmatch_click))
    perf_refresh(perf_refresh() + 1)
    showNotification("已移除", type = "message")
  })

  ##################
  # 结果评定
  ##################
  observeEvent(input$perf_rate, {
    req(rv$logged_in, is_admin())
    sheet <- current_sheet(); req(sheet)
    emps <- perf_sheet_employee_list(sheet$id[1])
    if (nrow(emps) == 0) { showNotification("请先添加员工", type="warning"); return() }
    ratings <- tryCatch(perf_result_get(sheet$id[1]), error=function(e) data.frame())
    emp_labels <- sapply(seq_len(nrow(emps)), function(i) {
      nm <- ifelse(is.na(emps$display_name[i])|emps$display_name[i]=="", emps$username[i], emps$display_name[i])
      nm
    })
    showModal(modalDialog(
      title = "绩效结果评定", size = "m",
      tags$table(class = "table table-condensed", style = "width:100%;",
        tags$thead(tags$tr(tags$th("员工"),tags$th("绩效结果"),tags$th("标杆"))),
        tags$tbody(
          lapply(seq_len(nrow(emps)), function(i) {
            e <- emps[i, ]; nm <- emp_labels[i]
            r <- if (nrow(ratings)>0) ratings[ratings$employee_id==e$employee_id,] else data.frame()
            prev_res <- if(nrow(r)>0) r$result[1]%||%"" else ""
            prev_bm  <- if(nrow(r)>0) r$benchmark[1]%||%"" else ""
            tags$tr(
              tags$td(nm, style="width:30%; font-weight:600;"),
              tags$td(style="width:35%;",
                tags$select(class="form-control input-sm perf-rate-result",
                  `data-eid` = as.character(e$employee_id),
                  tags$option(value="", selected=if(prev_res=="")NA else NULL, "—"),
                  tags$option(value="优秀", selected=if(prev_res=="优秀")NA else NULL, "优秀"),
                  tags$option(value="合格", selected=if(prev_res=="合格")NA else NULL, "合格"),
                  tags$option(value="不合格", selected=if(prev_res=="不合格")NA else NULL, "不合格"))),
              tags$td(style="width:35%;",
                tags$select(class="form-control input-sm perf-rate-benchmark",
                  `data-eid` = as.character(e$employee_id),
                  tags$option(value="", selected=if(prev_bm=="")NA else NULL, "—"),
                  tags$option(value="突出贡献", selected=if(prev_bm=="突出贡献")NA else NULL, "突出贡献")))
            )
          })
        )
      ),
      footer = tagList(
        modalButton("取消"),
        tags$button(class = "btn btn-warning", onclick = "
          var data = [];
          $('.perf-rate-result').each(function(){
            data.push({eid:$(this).data('eid'), type:'result', val:$(this).val()});
          });
          $('.perf-rate-benchmark').each(function(){
            data.push({eid:$(this).data('eid'), type:'benchmark', val:$(this).val()});
          });
          Shiny.setInputValue('perf_rate_data', JSON.stringify(data), {priority:'event'});
        ", "保存")
      ), easyClose = TRUE
    ))
  })

  observeEvent(input$perf_rate_data, {
    req(rv$logged_in, is_admin())
    sheet <- current_sheet(); req(sheet)
    data <- jsonlite::fromJSON(input$perf_rate_data)
    saved <- 0
    # 聚合: eid -> {result, benchmark}
    by_eid <- split(data, data$eid)
    for (eid in names(by_eid)) {
      entries <- by_eid[[eid]]
      res <- entries$val[entries$type == "result"]
      bm  <- entries$val[entries$type == "benchmark"]
      perf_result_set(sheet$id[1], as.integer(eid),
        result = if(length(res)>0 && res!="") res else NULL,
        benchmark = if(length(bm)>0 && bm!="") bm else NULL)
      saved <- saved + 1
    }
    removeModal()
    perf_refresh(perf_refresh() + 1)
    showNotification(sprintf("已保存 %d 位员工的评定结果", saved), type = "message")
  })
}
