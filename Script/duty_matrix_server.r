# 岗职模块 - 服务端

duty_matrix_server <- function(input, output, session, rv) {

  duty_trigger <- reactiveVal(0)
  refresh <- function() { duty_trigger(duty_trigger() + 1) }

  ##################
  # 矩阵视图：列=岗位(一级)+人员(二级)，行=职责项
  ##################
  output$duty_matrix_view <- renderUI({
    duty_trigger(); req(rv$logged_in)
    matrix <- duty_matrix_get()
    items     <- duty_item_get_all()
    positions <- duty_position_get_all()
    staff_all <- duty_staff_get_all()

    if (nrow(positions) == 0 || nrow(items) == 0) {
      return(tags$p(style="color:#999; padding:20px; text-align:center;",
                    "请先添加岗位和职责项"))
    }

    level_colors <- list(
      "负责人" = "owner",
      "执行"   = "exec",
      "知晓"   = "know"
    )

    # 计算每列的子列数（人员数），用于rowspan
    pos_staff_map <- lapply(1:nrow(positions), function(i) {
      staff_all[staff_all$position_id == positions$id[i], ]
    })
    names(pos_staff_map) <- positions$id

    # 岗位表头（一级，跨多个人员列）
    pos_header <- tags$tr(
      tags$th(style="width:100px;", rowspan=2, "职责项"),
      lapply(1:nrow(positions), function(i) {
        p <- positions[i, ]
        n_staff <- nrow(pos_staff_map[[as.character(p$id)]])
        if (n_staff == 0) n_staff <- 1  # 至少占一列
        tags$th(colspan = n_staff,
          tags$div(p$name),
          tags$div(style="font-size:10px; font-weight:normal; color:#999;", p$description %||% "")
        )
      })
    )

    # 人员表头（二级）
    staff_header_cells <- list()
    for (i in 1:nrow(positions)) {
      p <- positions[i, ]
      staff_list <- pos_staff_map[[as.character(p$id)]]
      if (nrow(staff_list) == 0) {
        staff_header_cells[[length(staff_header_cells)+1]] <- tags$th(style="color:#999;", "—")
      } else {
        for (s in 1:nrow(staff_list)) {
          st <- staff_list[s, ]
          staff_header_cells[[length(staff_header_cells)+1]] <- tags$th(
            style="font-size:11px; font-weight:normal; min-width:80px;",
            st$name
          )
        }
      }
    }
    staff_header <- do.call(tags$tr, staff_header_cells)

    # 数据行
    rows <- lapply(1:nrow(items), function(ii) {
      item <- items[ii, ]
      cells <- list()
      for (pi in 1:nrow(positions)) {
        pos <- positions[pi, ]
        staff_list <- pos_staff_map[[as.character(pos$id)]]
        if (nrow(staff_list) == 0) {
          # 该岗位无人员，显示空
          cells[[length(cells)+1]] <- tags$td(class = "duty-cell empty",
            `data-pid` = pos$id, `data-did` = item$id, "—")
        } else {
          for (si in 1:nrow(staff_list)) {
            st <- staff_list[si, ]
            # 查找该人员+岗位+职责项的RBAC
            row <- matrix[matrix$staff_id == st$id &
                         matrix$position_id == pos$id &
                         matrix$duty_item_id == item$id, ]
            if (nrow(row) == 0) {
              cells[[length(cells)+1]] <- tags$td(class = "duty-cell empty",
                `data-sid` = st$id, `data-pid` = pos$id, `data-did` = item$id,
                "—")
            } else {
              cls <- level_colors[[row$responsibility_level[1]]] %||% "empty"
              cells[[length(cells)+1]] <- tags$td(class = paste("duty-cell", cls),
                `data-sid` = st$id, `data-pid` = pos$id, `data-did` = item$id,
                row$responsibility_level[1])
            }
          }
        }
      }
      do.call(tags$tr, c(list(tags$td(style="font-weight:600; font-size:12px; text-align:left; padding-left:10px;", item$name)), cells))
    })

    tags$table(class = "table table-bordered table-condensed duty-table",
      tags$thead(pos_header, staff_header),
      tags$tbody(rows)
    )
  })

  ##################
  ##################
  # 点击单元格 → 弹窗设置/修改 RBAC
  ##################
  observeEvent(input$duty_matrix_click, {
    req(rv$logged_in)
    sid <- as.integer(input$duty_matrix_click$sid)
    pid <- as.integer(input$duty_matrix_click$pid)
    did <- as.integer(input$duty_matrix_click$did)

    pos  <- duty_position_get_all(); pos_row <- pos[pos$id == pid, ]
    item <- duty_item_get_all(); item_row <- item[item$id == did, ]
    st   <- duty_staff_get_all(); st_row <- st[st$id == sid, ]
    staff_name <- if (nrow(st_row) > 0) st_row$name[1] else "未知"
    title <- sprintf("%s / %s — %s", pos_row$name[1] %||% "?", staff_name, item_row$name[1] %||% "?")

    # 查当前值
    matrix <- duty_matrix_get()
    cur <- matrix[matrix$staff_id == sid & matrix$position_id == pid & matrix$duty_item_id == did, ]
    cur_level <- if (nrow(cur) > 0) cur$responsibility_level[1] else ""
    cur_comment <- if (nrow(cur) > 0) cur$comment[1] %||% "" else ""

    showModal(modalDialog(
      title = title, size = "s",
      selectInput("duty_modal_level","RBAC级别",
        choices = c("负责人","执行","知晓"), selected = if(cur_level != "") cur_level else "执行"),
      textAreaInput("duty_modal_comment","备注",rows=2,value=cur_comment),
      footer = tagList(
        actionButton("duty_modal_save","保存",class="btn-primary"),
        if(nrow(cur)>0) actionButton("duty_modal_delete","删除",class="btn-danger") else "",
        modalButton("取消")
      ), easyClose = TRUE
    ))
    rv$duty_modal_sid <- sid; rv$duty_modal_pid <- pid; rv$duty_modal_did <- did
  })

  observeEvent(input$duty_modal_save, {
    req(rv$logged_in, input$duty_modal_level)
    duty_matrix_set(rv$duty_modal_sid, rv$duty_modal_pid, rv$duty_modal_did,
      input$duty_modal_level, input$duty_modal_comment %||% "")
    removeModal(); refresh()
    showNotification("矩阵已更新", type = "message")
  })

  observeEvent(input$duty_modal_delete, {
    duty_matrix_delete(rv$duty_modal_sid, rv$duty_modal_pid, rv$duty_modal_did)
    removeModal(); refresh()
    showNotification("已清除", type = "message")
  })

  ##################
  # 清单表格（单击行弹出编辑弹窗）
  ##################
  output$duty_position_table <- DT::renderDT({
    duty_trigger()
    DT::datatable(duty_position_get_all()[, c("id","name","description")],
      colnames = c("ID","名称","描述"), options = list(pageLength=5, dom='tip'), rownames=FALSE,
      selection='single')
  })
  output$duty_staff_table <- DT::renderDT({
    duty_trigger()
    DT::datatable(duty_staff_get_all()[, c("id","name","position_name","department","email","username")],
      colnames = c("ID","姓名","岗位","部门","邮箱","关联用户"), options = list(pageLength=5, dom='tip'), rownames=FALSE,
      selection='single')
  })
  output$duty_item_table <- DT::renderDT({
    duty_trigger()
    DT::datatable(duty_item_get_all()[, c("id","name","category","description")],
      colnames = c("ID","名称","分类","描述"), options = list(pageLength=5, dom='tip'), rownames=FALSE,
      selection='single')
  })

  ##################
  # 单击编辑弹窗
  ##################
  observeEvent(input$duty_position_table_rows_selected, {
    req(rv$logged_in, input$duty_position_table_rows_selected)
    row <- duty_position_get_all()[input$duty_position_table_rows_selected, ]
    rv$duty_edit_type <- "position"; rv$duty_edit_id <- row$id[1]
    showModal(modalDialog(
      title = sprintf("编辑岗位 #%d", row$id[1]), size = "s",
      textInput("duty_edit_pos_name", "名称", value = row$name[1]),
      textInput("duty_edit_pos_desc", "描述", value = row$description[1] %||% ""),
      footer = tagList(
        actionButton("duty_edit_save", "保存", class = "btn-primary"),
        actionButton("duty_edit_delete", "删除", class = "btn-danger"),
        modalButton("取消")
      ), easyClose = TRUE
    ))
  })

  observeEvent(input$duty_staff_table_rows_selected, {
    req(rv$logged_in, input$duty_staff_table_rows_selected)
    row <- duty_staff_get_all()[input$duty_staff_table_rows_selected, ]
    rv$duty_edit_type <- "staff"; rv$duty_edit_id <- row$id[1]
    pos <- duty_position_get_all()
    pos_choices <- if (nrow(pos) > 0) stats::setNames(pos$id, pos$name) else c()
    showModal(modalDialog(
      title = sprintf("编辑人员 #%d — %s", row$id[1], row$name[1]), size = "s",
      textInput("duty_edit_staff_name", "姓名", value = row$name[1]),
      selectInput("duty_edit_staff_pos", "岗位", choices = c("(无)" = "", pos_choices), selected = row$position_id[1] %||% ""),
      textInput("duty_edit_staff_dept", "部门", value = row$department[1] %||% ""),
      textInput("duty_edit_staff_email", "邮箱", value = row$email[1] %||% ""),
      footer = tagList(
        actionButton("duty_edit_save", "保存", class = "btn-primary"),
        actionButton("duty_edit_delete", "删除", class = "btn-danger"),
        modalButton("取消")
      ), easyClose = TRUE
    ))
  })

  observeEvent(input$duty_item_table_rows_selected, {
    req(rv$logged_in, input$duty_item_table_rows_selected)
    row <- duty_item_get_all()[input$duty_item_table_rows_selected, ]
    rv$duty_edit_type <- "item"; rv$duty_edit_id <- row$id[1]
    showModal(modalDialog(
      title = sprintf("编辑职责项 #%d — %s", row$id[1], row$name[1]), size = "s",
      textInput("duty_edit_item_name", "名称", value = row$name[1]),
      textInput("duty_edit_item_cat", "分类", value = row$category[1] %||% ""),
      textInput("duty_edit_item_desc", "描述", value = row$description[1] %||% ""),
      footer = tagList(
        actionButton("duty_edit_save", "保存", class = "btn-primary"),
        actionButton("duty_edit_delete", "删除", class = "btn-danger"),
        modalButton("取消")
      ), easyClose = TRUE
    ))
  })

  # 保存编辑
  observeEvent(input$duty_edit_save, {
    req(rv$logged_in, rv$duty_edit_type, rv$duty_edit_id)
    result <- switch(rv$duty_edit_type,
      "position" = duty_position_update(rv$duty_edit_id,
        name = input$duty_edit_pos_name,
        description = if (isTRUE(nchar(trimws(input$duty_edit_pos_desc)) > 0)) input$duty_edit_pos_desc else NULL),
      "staff" = duty_staff_update(rv$duty_edit_id,
        name = input$duty_edit_staff_name,
        position_id = if (!is.null(input$duty_edit_staff_pos) && input$duty_edit_staff_pos != "") as.integer(input$duty_edit_staff_pos) else NULL,
        department = if (isTRUE(nchar(trimws(input$duty_edit_staff_dept)) > 0)) input$duty_edit_staff_dept else NULL,
        email = if (isTRUE(nchar(trimws(input$duty_edit_staff_email)) > 0)) input$duty_edit_staff_email else NULL),
      "item" = duty_item_update(rv$duty_edit_id,
        name = input$duty_edit_item_name,
        category = if (isTRUE(nchar(trimws(input$duty_edit_item_cat)) > 0)) input$duty_edit_item_cat else NULL,
        description = if (isTRUE(nchar(trimws(input$duty_edit_item_desc)) > 0)) input$duty_edit_item_desc else NULL)
    )
    removeModal()
    refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # 删除
  observeEvent(input$duty_edit_delete, {
    req(rv$logged_in, rv$duty_edit_type, rv$duty_edit_id)
    result <- switch(rv$duty_edit_type,
      "position" = duty_position_delete(rv$duty_edit_id),
      "staff" = duty_staff_delete(rv$duty_edit_id),
      "item" = duty_item_delete(rv$duty_edit_id)
    )
    removeModal()
    refresh()
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 创建岗位
  ##################
  observeEvent(input$duty_add_position, {
    req(rv$logged_in, input$duty_new_position_name)
    result <- duty_position_add(input$duty_new_position_name, input$duty_new_position_desc %||% "")
    if (result$success) {
      updateTextInput(session,"duty_new_position_name",value="")
      updateTextInput(session,"duty_new_position_desc",value="")
    }
    refresh(); showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  # 更新人员创建表单的岗位下拉选项
  observe({
    req(rv$logged_in)
    positions <- duty_position_get_all()
    choices <- if (nrow(positions) > 0) stats::setNames(positions$id, positions$name) else c("(请先创建岗位)" = "")
    updateSelectInput(session, "duty_new_staff_position", choices = c("(无)" = "", choices))
  })

  # 创建人员
  observeEvent(input$duty_add_staff, {
    req(rv$logged_in, input$duty_new_staff_name)
    pos_id <- if (!is.null(input$duty_new_staff_position) && input$duty_new_staff_position != "") as.integer(input$duty_new_staff_position) else NULL
    result <- duty_staff_add(input$duty_new_staff_name, input$duty_new_staff_dept %||% "", input$duty_new_staff_email %||% "", position_id = pos_id)
    if (result$success) {
      updateTextInput(session,"duty_new_staff_name",value="")
      updateTextInput(session,"duty_new_staff_dept",value="")
      updateTextInput(session,"duty_new_staff_email",value="")
    }
    refresh(); showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  # 创建职责
  observeEvent(input$duty_add_item, {
    req(rv$logged_in, input$duty_new_item_name)
    result <- duty_item_add(input$duty_new_item_name, input$duty_new_item_desc %||% "", input$duty_new_item_cat %||% "")
    if (result$success) {
      updateTextInput(session,"duty_new_item_name",value="")
      updateTextInput(session,"duty_new_item_cat",value="")
      updateTextInput(session,"duty_new_item_desc",value="")
    }
    refresh(); showNotification(result$message, type=ifelse(result$success,"message","error"))
  })
}
