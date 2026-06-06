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
  # 卡片清单视图
  ##################
  output$duty_position_cards <- renderUI({
    duty_trigger(); req(rv$logged_in)
    positions <- duty_position_get_all()
    staff <- duty_staff_get_all()
    if (nrow(positions) == 0) return(tags$p(style="color:#999; text-align:center; padding:10px;","暂无岗位"))
    tagList(
      tags$h5("岗位清单", style="margin-top:0;"),
      tags$div(style="max-height:500px; overflow-y:auto;",
        lapply(1:nrow(positions), function(i) {
          p <- positions[i, ]
          p_staff <- staff[staff$position_id == p$id, ]
          staff_tags <- if (nrow(p_staff) > 0) {
            lapply(1:nrow(p_staff), function(j) {
              s <- p_staff[j, ]
              tags$span(class = "tag", style = "background:#e8f4fd; color:#0c5460;",
                s$name,
                tags$span(style="cursor:pointer; margin-left:4px; color:#999;",
                  `data-sid`=s$id, `data-pid`=p$id, class="duty-card-rm-staff", "×"))
            })
          } else list(tags$span(style="color:#ccc; font-size:11px;", "暂无人员"))
          tags$div(class = "duty-card", style = "margin-bottom:6px;",
            tags$div(class = "duty-card-head", onclick = sprintf("var d=document.getElementById('dpos-%d');d.style.display=d.style.display==='none'?'block':'none';", p$id),
              style = "cursor:pointer; display:flex; justify-content:space-between; align-items:center; padding:6px 10px; background:#f8f4ff; border-radius:6px;",
              tags$b(p$name, style="font-size:13px;"),
              tags$span(style="font-size:10px; color:#999;", sprintf("%d人", nrow(p_staff)), " ▾")
            ),
            tags$div(id = sprintf("dpos-%d", p$id), style = "display:none; padding:8px 10px; border:1px solid #e8ecf1; border-top:none; border-radius:0 0 6px 6px;",
              if (nchar(p$description[1] %||% "") > 0) tags$div(class="duty-card-fields", style="margin-bottom:6px;", p$description[1]),
              do.call(tags$div, staff_tags),
              tags$div(style = "margin-top:6px; display:flex; gap:4px;",
                tags$button(class="btn btn-xs btn-info duty-card-add-staff", `data-pid`=p$id, "+人员"),
                tags$button(class="btn btn-xs btn-warning duty-card-edit-btn", `data-type`="position", `data-id`=p$id, "✏"),
                tags$button(class="btn btn-xs btn-danger duty-card-del-btn", `data-type`="position", `data-id`=p$id, "🗑")
              )
            )
          )
        })
      )
    )
  })

  output$duty_staff_cards <- renderUI({
    duty_trigger(); req(rv$logged_in)
    staff <- duty_staff_get_all()
    matrix <- duty_matrix_get()
    if (nrow(staff) == 0) return(tags$p(style="color:#999; text-align:center; padding:10px;","暂无人员"))
    tagList(
      tags$h5("人员清单", style="margin-top:0;"),
      tags$div(style="max-height:500px; overflow-y:auto;",
        lapply(1:nrow(staff), function(i) {
          s <- staff[i, ]
          duties <- matrix[matrix$staff_id == s$id, ]
          duty_tags <- if (nrow(duties) > 0) {
            lapply(1:nrow(duties), function(j) {
              d <- duties[j, ]
              lvl <- d$responsibility_level[1]
              cls <- switch(lvl, "负责人"="owner","执行"="exec","知晓"="know","")
              tags$span(class = paste("tag", cls),
                d$duty_name, "(", lvl, ")",
                tags$span(style="cursor:pointer; margin-left:2px;", `data-sid`=s$id, `data-pid`=d$position_id, `data-did`=d$duty_item_id, class="duty-card-rm-duty", "×"))
            })
          } else list(tags$span(style="color:#ccc; font-size:11px;", "暂无职责"))
          tags$div(class = "duty-card", style = "margin-bottom:6px;",
            tags$div(class = "duty-card-head", onclick = sprintf("var d=document.getElementById('dstf-%d');d.style.display=d.style.display==='none'?'block':'none';", s$id),
              style = "cursor:pointer; display:flex; justify-content:space-between; align-items:center; padding:6px 10px; background:#f0f7ff; border-radius:6px;",
              tags$b(s$name, style="font-size:13px;"),
              tags$span(style="font-size:10px; color:#999;", s$position_name[1] %||% "未分配", " · ", sprintf("%d职责", nrow(duties)), " ▾")
            ),
            tags$div(id = sprintf("dstf-%d", s$id), style = "display:none; padding:8px 10px; border:1px solid #e8ecf1; border-top:none; border-radius:0 0 6px 6px;",
              tags$div(class="duty-card-fields",
                tags$div("岗位: ", s$position_name[1] %||% "未分配"),
                if (nchar(s$department[1] %||% "") > 0) tags$div("部门: ", s$department[1]),
                if (nchar(s$email[1] %||% "") > 0) tags$div("邮箱: ", s$email[1])
              ),
              do.call(tags$div, duty_tags),
              tags$div(style = "margin-top:6px; display:flex; gap:4px;",
                tags$button(class="btn btn-xs btn-info duty-card-add-duty", `data-sid`=s$id, "+职责"),
                tags$button(class="btn btn-xs btn-warning duty-card-edit-btn", `data-type`="staff", `data-id`=s$id, "✏"),
                tags$button(class="btn btn-xs btn-danger duty-card-del-btn", `data-type`="staff", `data-id`=s$id, "🗑")
              )
            )
          )
        })
      )
    )
  })

  output$duty_item_cards <- renderUI({
    duty_trigger(); req(rv$logged_in)
    items <- duty_item_get_all()
    if (nrow(items) == 0) return(tags$p(style="color:#999; text-align:center; padding:10px;","暂无职责项"))
    matrix <- duty_matrix_get()
    tagList(
      tags$h5("职责项清单", style="margin-top:0;"),
      tags$div(style="max-height:500px; overflow-y:auto;",
        lapply(1:nrow(items), function(i) {
          it <- items[i, ]
          assigned <- matrix[matrix$duty_item_id == it$id, ]
          assigned_tags <- if (nrow(assigned) > 0) {
            lapply(1:nrow(assigned), function(j) {
              a <- assigned[j, ]
              lvl <- a$responsibility_level[1] %||% "—"
              cls <- switch(lvl, "负责人"="owner","执行"="exec","知晓"="know","")
              tags$span(class = paste("tag", cls),
                a$staff_name, "(", lvl, ")",
                tags$span(style="cursor:pointer; margin-left:2px;", `data-sid`=a$staff_id, `data-pid`=a$position_id, `data-did`=it$id, class="duty-card-rm-duty", "×"))
            })
          } else list(tags$span(style="color:#ccc; font-size:11px;", "未分配"))
          tags$div(class = "duty-card", style = "margin-bottom:6px;",
            tags$div(class = "duty-card-head", onclick = sprintf("var d=document.getElementById('ditem-%d');d.style.display=d.style.display==='none'?'block':'none';", it$id),
              style = "cursor:pointer; display:flex; justify-content:space-between; align-items:center; padding:6px 10px; background:#f0faf5; border-radius:6px;",
              tags$b(it$name, style="font-size:13px;"),
              tags$span(style="font-size:10px; color:#999;", if(nchar(it$category[1]%||%"")>0) it$category[1] else "", " · ", sprintf("%d人", nrow(assigned)), " ▾")
            ),
            tags$div(id = sprintf("ditem-%d", it$id), style = "display:none; padding:8px 10px; border:1px solid #e8ecf1; border-top:none; border-radius:0 0 6px 6px;",
              if (nchar(it$description[1] %||% "") > 0) tags$div(class="duty-card-fields", style="margin-bottom:6px;", it$description[1]),
              tags$div(style="margin-top:4px;", do.call(tags$div, assigned_tags)),
              tags$div(style = "margin-top:6px; display:flex; gap:4px;",
                tags$button(class="btn btn-xs btn-warning duty-card-edit-btn", `data-type`="item", `data-id`=it$id, "✏"),
                tags$button(class="btn btn-xs btn-danger duty-card-del-btn", `data-type`="item", `data-id`=it$id, "🗑")
              )
            )
          )
        })
      )
    )
  })

  ##################
  # 卡片按钮：添加人员到岗位
  ##################
  observeEvent(input$duty_card_add_staff, {
    req(rv$logged_in)
    pid <- as.integer(input$duty_card_add_staff)
    pos <- duty_position_get_all()[duty_position_get_all()$id == pid, ]
    staff <- duty_staff_get_all()
    choices <- if (nrow(staff) > 0) stats::setNames(staff$id, paste(staff$name, ifelse(is.na(staff$department) | staff$department=="","",staff$department), sep=ifelse(is.na(staff$department) | staff$department=="",""," / "))) else c()
    showModal(modalDialog(
      title = paste("添加人员到", pos$name[1] %||% ""),
      selectInput("duty_card_add_staff_sel", "选择人员", choices = c("(选择)" = "", choices)),
      footer = tagList(modalButton("取消"), actionButton("duty_card_add_staff_confirm","确定",class="btn-primary")),
      size = "s", easyClose = TRUE
    ))
    rv$duty_card_pid <- pid
  })
  observeEvent(input$duty_card_add_staff_confirm, {
    req(input$duty_card_add_staff_sel, rv$duty_card_pid)
    sid <- as.integer(input$duty_card_add_staff_sel)
    duty_staff_update(sid, position_id = rv$duty_card_pid)
    removeModal(); refresh()
  })

  ##################
  # 卡片按钮：添加职责到人员
  ##################
  observeEvent(input$duty_card_add_duty, {
    req(rv$logged_in)
    sid <- as.integer(input$duty_card_add_duty)
    st <- duty_staff_get_all()[duty_staff_get_all()$id == sid, ]
    if (is.na(st$position_id[1]) || st$position_id[1] == 0) {
      showNotification("请先将该人员分配岗位", type="warning"); return()
    }
    items <- duty_item_get_all()
    choices <- stats::setNames(items$id, items$name)
    showModal(modalDialog(
      title = paste("添加职责到", st$name[1]),
      selectInput("duty_card_add_duty_item", "选择职责项", choices = c("(选择)" = "", choices)),
      selectInput("duty_card_add_duty_level", "RBAC级别", choices = c("负责人","执行","知晓")),
      footer = tagList(modalButton("取消"), actionButton("duty_card_add_duty_confirm","确定",class="btn-primary")),
      size = "s", easyClose = TRUE
    ))
    rv$duty_card_sid <- sid; rv$duty_card_staff_pid <- st$position_id[1]
  })
  observeEvent(input$duty_card_add_duty_confirm, {
    req(input$duty_card_add_duty_item, input$duty_card_add_duty_level, rv$duty_card_sid, rv$duty_card_staff_pid)
    duty_matrix_set(rv$duty_card_sid, rv$duty_card_staff_pid,
      as.integer(input$duty_card_add_duty_item), input$duty_card_add_duty_level)
    removeModal(); refresh()
  })

  ##################
  # 卡片按钮：从岗位移除人员 / 从人员移除职责
  ##################
  observeEvent(input$duty_card_rm_staff, {
    req(rv$logged_in)
    duty_staff_update(as.integer(input$duty_card_rm_staff$sid), position_id = NULL)
    refresh()
  })
  observeEvent(input$duty_card_rm_duty, {
    req(rv$logged_in)
    duty_matrix_delete(
      as.integer(input$duty_card_rm_duty$sid),
      as.integer(input$duty_card_rm_duty$pid),
      as.integer(input$duty_card_rm_duty$did))
    refresh()
  })

  ##################
  # 卡片按钮：编辑/删除
  ##################
  observeEvent(input$duty_card_edit, {
    req(rv$logged_in)
    tp <- input$duty_card_edit$type; eid <- as.integer(input$duty_card_edit$id)
    if (tp == "position") {
      row <- duty_position_get_all()[duty_position_get_all()$id == eid, ]
      rv$duty_edit_type <- "position"; rv$duty_edit_id <- eid
      showModal(modalDialog(title="编辑岗位", size="s",
        textInput("duty_edit_pos_name","名称",value=row$name[1]),
        textInput("duty_edit_pos_desc","描述",value=row$description[1] %||% ""),
        footer=tagList(actionButton("duty_edit_save","保存",class="btn-primary"),
          actionButton("duty_edit_delete","删除",class="btn-danger"), modalButton("取消")), easyClose=TRUE))
    } else if (tp == "staff") {
      row <- duty_staff_get_all()[duty_staff_get_all()$id == eid, ]
      rv$duty_edit_type <- "staff"; rv$duty_edit_id <- eid
      pos <- duty_position_get_all()
      pos_choices <- if (nrow(pos) > 0) stats::setNames(pos$id, pos$name) else c()
      showModal(modalDialog(title=paste("编辑人员", row$name[1]), size="s",
        textInput("duty_edit_staff_name","姓名",value=row$name[1]),
        selectInput("duty_edit_staff_pos","岗位", choices=c("(无)"="", pos_choices), selected=row$position_id[1] %||% ""),
        textInput("duty_edit_staff_dept","部门",value=row$department[1] %||% ""),
        textInput("duty_edit_staff_email","邮箱",value=row$email[1] %||% ""),
        footer=tagList(actionButton("duty_edit_save","保存",class="btn-primary"),
          actionButton("duty_edit_delete","删除",class="btn-danger"), modalButton("取消")), easyClose=TRUE))
    } else if (tp == "item") {
      row <- duty_item_get_all()[duty_item_get_all()$id == eid, ]
      rv$duty_edit_type <- "item"; rv$duty_edit_id <- eid
      showModal(modalDialog(title=paste("编辑职责", row$name[1]), size="s",
        textInput("duty_edit_item_name","名称",value=row$name[1]),
        textInput("duty_edit_item_cat","分类",value=row$category[1] %||% ""),
        textInput("duty_edit_item_desc","描述",value=row$description[1] %||% ""),
        footer=tagList(actionButton("duty_edit_save","保存",class="btn-primary"),
          actionButton("duty_edit_delete","删除",class="btn-danger"), modalButton("取消")), easyClose=TRUE))
    }
  })

  observeEvent(input$duty_card_del, {
    req(rv$logged_in)
    tp <- input$duty_card_del$type; did <- as.integer(input$duty_card_del$id)
    result <- switch(tp,
      "position" = duty_position_delete(did),
      "staff" = duty_staff_delete(did),
      "item" = duty_item_delete(did))
    refresh(); showNotification(result$message, type=if(result$success)"message" else "error")
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
