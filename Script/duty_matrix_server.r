# 岗职模块 - 服务端

duty_matrix_server <- function(input, output, session, rv) {

  duty_trigger <- reactiveVal(0)
  refresh <- function() { duty_trigger(duty_trigger() + 1) }
  is_admin <- reactive({
    !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
  })
  current_uid <- reactive({
    if (is.null(rv$current_user) || nrow(rv$current_user) == 0) NULL else rv$current_user$id[1]
  })

  ##################
  # 矩阵视图：列=岗位(一级)+人员(二级)，行=职责项(可展开二级)
  ##################
  output$duty_matrix_view <- renderUI({
    duty_trigger(); req(rv$logged_in)
    matrix <- duty_matrix_get()
    sub_matrix <- tryCatch(duty_sub_matrix_get(), error = function(e) data.frame())
    items     <- duty_item_get_all()
    positions <- duty_position_get_all()
    staff_all <- duty_staff_get_all()
    # 非admin用户只看自己的记录
    if (!is_admin()) {
      staff_all <- staff_all[!is.na(staff_all$user_id) & staff_all$user_id == current_uid(), , drop = FALSE]
      if (nrow(staff_all) == 0) {
        return(tags$p(style="color:#999; padding:20px; text-align:center;", "你暂无岗职记录"))
      }
    }

    if (nrow(positions) == 0 || nrow(items) == 0) {
      return(tags$p(style="color:#999; padding:20px; text-align:center;",
                    "请先添加岗位和职责项"))
    }

    level_colors <- list(
      "负责人" = "owner",
      "执行"   = "exec",
      "知晓"   = "know"
    )

    # 计算每列的子列数（人员数）
    pos_staff_map <- lapply(1:nrow(positions), function(i) {
      staff_all[staff_all$position_id == positions$id[i], ]
    })
    names(pos_staff_map) <- positions$id

    # 岗位表头（一级，跨多个人员列）
    pos_header <- tags$tr(
      tags$th(style="min-width:100px; white-space:nowrap;", rowspan=2, "职责项"),
      lapply(1:nrow(positions), function(i) {
        p <- positions[i, ]
        n_staff <- nrow(pos_staff_map[[as.character(p$id)]])
        if (n_staff == 0) n_staff <- 1
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

    # 辅助函数：为一行的每个cell生成<td>
    build_row_cells <- function(item_id, is_sub = FALSE) {
      cells <- list()
      for (pi in 1:nrow(positions)) {
        pos <- positions[pi, ]
        staff_list <- pos_staff_map[[as.character(pos$id)]]
        if (nrow(staff_list) == 0) {
          if (is_sub) {
            cells[[length(cells)+1]] <- tags$td(class = "duty-sub-cell empty",
              `data-pid` = pos$id, `data-sdid` = item_id, "—")
          } else {
            cells[[length(cells)+1]] <- tags$td(class = "duty-cell empty",
              `data-pid` = pos$id, `data-did` = item_id, "—")
          }
        } else {
          for (si in 1:nrow(staff_list)) {
            st <- staff_list[si, ]
            if (is_sub) {
              row <- sub_matrix[sub_matrix$staff_id == st$id &
                               sub_matrix$position_id == pos$id &
                               sub_matrix$duty_sub_item_id == item_id, ]
              if (nrow(row) == 0) {
                cells[[length(cells)+1]] <- tags$td(class = "duty-sub-cell empty",
                  `data-sid` = st$id, `data-pid` = pos$id, `data-sdid` = item_id, "—")
              } else {
                cls <- level_colors[[row$responsibility_level[1]]] %||% "empty"
                cells[[length(cells)+1]] <- tags$td(class = paste("duty-sub-cell", cls),
                  `data-sid` = st$id, `data-pid` = pos$id, `data-sdid` = item_id,
                  row$responsibility_level[1])
              }
            } else {
              row <- matrix[matrix$staff_id == st$id &
                           matrix$position_id == pos$id &
                           matrix$duty_item_id == item_id, ]
              if (nrow(row) == 0) {
                cells[[length(cells)+1]] <- tags$td(class = "duty-cell empty",
                  `data-sid` = st$id, `data-pid` = pos$id, `data-did` = item_id,
                  "—")
              } else {
                cls <- level_colors[[row$responsibility_level[1]]] %||% "empty"
                cells[[length(cells)+1]] <- tags$td(class = paste("duty-cell", cls),
                  `data-sid` = st$id, `data-pid` = pos$id, `data-did` = item_id,
                  row$responsibility_level[1])
              }
            }
          }
        }
      }
      cells
    }

    # 数据行（一级 + 可展开二级子行）
    all_sub_items <- tryCatch(duty_sub_item_get_all_with_parent(), error = function(e) data.frame())
    rows <- list()
    for (ii in 1:nrow(items)) {
      item <- items[ii, ]
      # 获取该职责项的子任务
      subs <- if (nrow(all_sub_items) > 0) all_sub_items[all_sub_items$duty_item_id == item$id, ] else data.frame()
      has_subs <- nrow(subs) > 0
      # 一级行
      row_cells <- build_row_cells(item$id, is_sub = FALSE)
      name_cell <- tags$td(style="font-weight:600; font-size:12px; text-align:left; padding-left:6px;",
        if (has_subs) tags$span(class="duty-row-toggle", `data-did`=item$id,
            style="cursor:pointer; color:#666; margin-right:4px; font-family:monospace; font-size:11px; user-select:none;", "[+]") else "",
        item$name)
      rows[[length(rows)+1]] <- do.call(tags$tr, c(list(name_cell, class = "duty-row"), row_cells))
      # 二级子行（默认隐藏）
      if (has_subs) {
        for (si in 1:nrow(subs)) {
          sub <- subs[si, ]
          sub_cells <- build_row_cells(sub$id, is_sub = TRUE)
          sub_name_cell <- tags$td(style="font-size:11px; font-style:italic; text-align:left; padding-left:28px; color:#7b5ea7;",
            paste0("└ ", sub$name))
          rows[[length(rows)+1]] <- do.call(tags$tr, c(list(sub_name_cell, class = "duty-sub-row",
            `data-parent-did` = item$id, style = "display:none;"), sub_cells))
        }
      }
    }

    tags$table(class = "table table-bordered table-condensed duty-table",
      tags$thead(pos_header, staff_header),
      tags$tbody(rows)
    )
  })

  ##################
  ##################
  # 点击单元格 → 弹窗设置/修改 RBAC（支持一级+二级）
  ##################
  observeEvent(input$duty_matrix_click, {
    req(rv$logged_in, is_admin())
    sid <- as.integer(input$duty_matrix_click$sid)
    pid <- as.integer(input$duty_matrix_click$pid)
    is_sub <- !is.null(input$duty_matrix_click$sdid) && !is.na(input$duty_matrix_click$sdid)

    pos  <- duty_position_get_all(); pos_row <- pos[pos$id == pid, ]
    st   <- duty_staff_get_all(); st_row <- st[st$id == sid, ]
    staff_name <- if (nrow(st_row) > 0) st_row$name[1] else "未知"

    if (is_sub) {
      sdid <- as.integer(input$duty_matrix_click$sdid)
      all_subs <- duty_sub_item_get_all_with_parent()
      sub_row <- all_subs[all_subs$id == sdid, ]
      item_name <- sprintf("%s → %s", sub_row$parent_name[1] %||% "?", sub_row$name[1] %||% "?")
      title <- sprintf("%s / %s — %s", pos_row$name[1] %||% "?", staff_name, item_name)
      # 查当前值
      sub_matrix <- duty_sub_matrix_get()
      cur <- sub_matrix[sub_matrix$staff_id == sid & sub_matrix$position_id == pid & sub_matrix$duty_sub_item_id == sdid, ]
      cur_level <- if (nrow(cur) > 0) cur$responsibility_level[1] else ""
      cur_comment <- if (nrow(cur) > 0) cur$comment[1] %||% "" else ""
      rv$duty_modal_sid <- sid; rv$duty_modal_pid <- pid
      rv$duty_modal_sdid <- sdid; rv$duty_modal_is_sub <- TRUE
    } else {
      did <- as.integer(input$duty_matrix_click$did)
      item <- duty_item_get_all(); item_row <- item[item$id == did, ]
      title <- sprintf("%s / %s — %s", pos_row$name[1] %||% "?", staff_name, item_row$name[1] %||% "?")
      matrix <- duty_matrix_get()
      cur <- matrix[matrix$staff_id == sid & matrix$position_id == pid & matrix$duty_item_id == did, ]
      cur_level <- if (nrow(cur) > 0) cur$responsibility_level[1] else ""
      cur_comment <- if (nrow(cur) > 0) cur$comment[1] %||% "" else ""
      rv$duty_modal_sid <- sid; rv$duty_modal_pid <- pid
      rv$duty_modal_did <- did; rv$duty_modal_is_sub <- FALSE
    }

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
  })

  observeEvent(input$duty_modal_save, {
    req(rv$logged_in, input$duty_modal_level)
    if (isTRUE(rv$duty_modal_is_sub)) {
      duty_sub_matrix_set(rv$duty_modal_sid, rv$duty_modal_pid, rv$duty_modal_sdid,
        input$duty_modal_level, input$duty_modal_comment %||% "")
    } else {
      duty_matrix_set(rv$duty_modal_sid, rv$duty_modal_pid, rv$duty_modal_did,
        input$duty_modal_level, input$duty_modal_comment %||% "")
    }
    removeModal(); refresh()
    showNotification("矩阵已更新", type = "message")
  })

  observeEvent(input$duty_modal_delete, {
    if (isTRUE(rv$duty_modal_is_sub)) {
      duty_sub_matrix_delete(rv$duty_modal_sid, rv$duty_modal_pid, rv$duty_modal_sdid)
    } else {
      duty_matrix_delete(rv$duty_modal_sid, rv$duty_modal_pid, rv$duty_modal_did)
    }
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
    sub_matrix <- tryCatch(duty_sub_matrix_get(), error = function(e) data.frame())
    all_subs <- tryCatch(duty_sub_item_get_all_with_parent(), error = function(e) data.frame())
    tagList(
      tags$h5("职责项清单", style="margin-top:0;"),
      tags$div(
        lapply(1:nrow(items), function(i) {
          it <- items[i, ]
          assigned <- matrix[matrix$duty_item_id == it$id, ]
          cat_val <- it$category[1] %||% ""
          desc_val <- it$description[1] %||% ""
          sort_val <- it$sort_order[1]
          if (is.null(sort_val) || is.na(sort_val) || sort_val == 0) sort_val <- ""
          # 子任务
          subs <- if (nrow(all_subs) > 0) all_subs[all_subs$duty_item_id == it$id, ] else data.frame()
          # 一级已分配人员tag
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
          # 构建子任务卡片
          sub_cards <- list()
          if (nrow(subs) > 0) {
            for (si in 1:nrow(subs)) {
              sub <- subs[si, ]
              sub_assigned <- if (nrow(sub_matrix) > 0) sub_matrix[sub_matrix$duty_sub_item_id == sub$id, ] else data.frame()
              sub_tags <- if (nrow(sub_assigned) > 0) {
                lapply(1:nrow(sub_assigned), function(j) {
                  sa <- sub_assigned[j, ]
                  lvl <- sa$responsibility_level[1] %||% "—"
                  cls <- switch(lvl, "负责人"="owner","执行"="exec","知晓"="know","")
                  tags$span(class = paste("tag", cls),
                    sa$staff_name, "(", lvl, ")",
                    tags$span(style="cursor:pointer; margin-left:2px;", `data-sid`=sa$staff_id, `data-pid`=sa$position_id, `data-sdid`=sub$id, class="duty-card-rm-sub-duty", "×"))
                })
              } else list()
              sub_cards[[length(sub_cards)+1]] <- tags$div(class = "duty-sub-card",
                style = "margin-bottom:4px; margin-left:12px; border-left:3px solid #c8b6e0; padding:6px 10px;",
                tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:2px;",
                  tags$span(
                    paste0("└ ", sub$name),
                    if (nchar(sub$category[1] %||% "") > 0) tags$span(style="font-size:10px; color:#999; margin-left:6px; background:#e8f0fe; padding:1px 4px; border-radius:3px;", sub$category[1])
                  ),
                  if (nrow(sub_assigned) > 0) tags$span(style="font-size:10px; color:#999;", sprintf("%d人", nrow(sub_assigned)))
                ),
                if (nchar(sub$description[1] %||% "") > 0) tags$div(style="font-size:11px; color:#886; margin-bottom:4px; white-space:pre-wrap;", sub$description[1]),
                if (length(sub_tags) > 0) tags$div(style="margin-top:2px;", do.call(tags$div, sub_tags)),
                tags$div(style = "margin-top:4px; display:flex; gap:3px;",
                  tags$button(class="btn btn-xs btn-outline-warning duty-card-edit-btn", `data-type`="subitem", `data-id`=sub$id, "✏"),
                  tags$button(class="btn btn-xs btn-outline-danger duty-card-del-btn", `data-type`="subitem", `data-id`=sub$id, "🗑")
                )
              )
            }
          }
          # 一级卡片头 + 折叠区（含子任务卡片）
          tags$div(
            tags$div(class = "duty-card", style = "margin-bottom:4px;",
              tags$div(class = "duty-card-head",
                style = "padding:8px 10px; background:#f0faf5; border-radius:6px;",
                tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                  tags$div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
                    if (sort_val != "") tags$span(style="font-size:11px; color:#999; font-family:monospace; background:#e8e8e8; padding:1px 6px; border-radius:3px;", paste0("#", sort_val)),
                    tags$b(it$name, style="font-size:13px;"),
                    if (nchar(cat_val) > 0) tags$span(style="font-size:11px; color:#666; background:#e8f0fe; padding:1px 6px; border-radius:3px;", cat_val),
                    if (nrow(subs) > 0) tags$span(style="font-size:10px; color:#7b5ea7; background:#f0e8ff; padding:1px 5px; border-radius:3px;", paste0("+", nrow(subs), "子"))
                  ),
                  tags$span(
                    style = "font-size:10px; color:#999; cursor:pointer; user-select:none;",
                    onclick = sprintf("var d=document.getElementById('ditem-%d');if(d)d.style.display=d.style.display==='none'?'block':'none';", it$id),
                    {
                      sub_count <- 0
                      if (nrow(subs) > 0 && nrow(sub_matrix) > 0) {
                        for (si in seq_len(nrow(subs))) {
                          sub_count <- sub_count + sum(sub_matrix$duty_sub_item_id == subs$id[si], na.rm = TRUE)
                        }
                      }
                      sprintf("%d人 ▾", nrow(assigned) + sub_count)
                    }
                  )
                ),
                if (nchar(desc_val) > 0) tags$div(style = "margin-top:6px; font-size:12px; color:#555; line-height:1.5; white-space:pre-wrap;", desc_val)
              ),
              # 折叠区：已分配人员 + 编辑按钮 + 二级子任务卡片
              tags$div(id = sprintf("ditem-%d", it$id), style = "display:none; padding:8px 10px; border:1px solid #e8ecf1; border-top:none; border-radius:0 0 6px 6px;",
                tags$div(style="margin-top:4px;", do.call(tags$div, assigned_tags)),
                tags$div(style = "margin-top:6px; display:flex; gap:4px;",
                  tags$button(class="btn btn-xs btn-warning duty-card-edit-btn", `data-type`="item", `data-id`=it$id, "✏"),
                  tags$button(class="btn btn-xs btn-danger duty-card-del-btn", `data-type`="item", `data-id`=it$id, "🗑")
                ),
                # 二级子任务卡片（放入折叠区内）
                if (length(sub_cards) > 0) tags$div(style = "margin-top:8px; border-top:1px dashed #e0d0f0; padding-top:6px;",
                  do.call(tagList, sub_cards))
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
    req(rv$logged_in, is_admin())
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
  # 卡片按钮：添加职责到人员（支持一级+二级）
  ##################
  observeEvent(input$duty_card_add_duty, {
    req(rv$logged_in, is_admin())
    sid <- as.integer(input$duty_card_add_duty)
    st <- duty_staff_get_all()[duty_staff_get_all()$id == sid, ]
    if (is.na(st$position_id[1]) || st$position_id[1] == 0) {
      showNotification("请先将该人员分配岗位", type="warning"); return()
    }
    items <- duty_item_get_all()
    choices <- stats::setNames(items$id, items$name)
    showModal(modalDialog(
      title = paste("添加职责到", st$name[1]),
      selectInput("duty_card_add_duty_item", "一级职责项", choices = c("(选择)" = "", choices)),
      selectInput("duty_card_add_duty_sub", "二级任务 (可选)", choices = c("— 不指定二级 —" = "")),
      selectInput("duty_card_add_duty_level", "RBAC级别", choices = c("负责人","执行","知晓")),
      footer = tagList(modalButton("取消"), actionButton("duty_card_add_duty_confirm","确定",class="btn-primary")),
      size = "s", easyClose = TRUE
    ))
    rv$duty_card_sid <- sid; rv$duty_card_staff_pid <- st$position_id[1]
  })
  observeEvent(input$duty_card_add_duty_item, {
    req(input$duty_card_add_duty_item)
    item_id <- as.integer(input$duty_card_add_duty_item)
    subs <- duty_sub_item_get_by_item(item_id)
    sub_choices <- c("— 不指定二级 —" = "")
    if (nrow(subs) > 0) {
      sub_choices <- c(sub_choices, stats::setNames(subs$id, subs$name))
    }
    updateSelectInput(session, "duty_card_add_duty_sub", choices = sub_choices, selected = "")
  })
  observeEvent(input$duty_card_add_duty_confirm, {
    req(input$duty_card_add_duty_item, input$duty_card_add_duty_level, rv$duty_card_sid, rv$duty_card_staff_pid)
    sub_id <- input$duty_card_add_duty_sub
    if (!is.null(sub_id) && sub_id != "") {
      # 添加二级任务分配
      duty_sub_matrix_set(rv$duty_card_sid, rv$duty_card_staff_pid,
        as.integer(sub_id), input$duty_card_add_duty_level)
    } else {
      # 添加一级职责分配
      duty_matrix_set(rv$duty_card_sid, rv$duty_card_staff_pid,
        as.integer(input$duty_card_add_duty_item), input$duty_card_add_duty_level)
    }
    removeModal(); refresh()
  })

  ##################
  # 卡片按钮：从岗位移除人员 / 从人员移除职责
  ##################
  observeEvent(input$duty_card_rm_staff, {
    req(rv$logged_in, is_admin())
    duty_staff_update(as.integer(input$duty_card_rm_staff$sid), position_id = NULL)
    refresh()
  })
  observeEvent(input$duty_card_rm_duty, {
    req(rv$logged_in, is_admin())
    duty_matrix_delete(
      as.integer(input$duty_card_rm_duty$sid),
      as.integer(input$duty_card_rm_duty$pid),
      as.integer(input$duty_card_rm_duty$did))
    refresh()
  })
  # 移除二级任务分配
  observeEvent(input$duty_card_rm_sub_duty, {
    req(rv$logged_in, is_admin())
    duty_sub_matrix_delete(
      as.integer(input$duty_card_rm_sub_duty$sid),
      as.integer(input$duty_card_rm_sub_duty$pid),
      as.integer(input$duty_card_rm_sub_duty$sdid))
    refresh()
  })

  ##################
  # 卡片按钮：编辑/删除
  ##################
  observeEvent(input$duty_card_edit, {
    req(rv$logged_in, is_admin())
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
        numericInput("duty_edit_item_sort","显示顺序",value=row$sort_order[1] %||% 0, min=0, max=999),
        footer=tagList(actionButton("duty_edit_save","保存",class="btn-primary"),
          actionButton("duty_edit_delete","删除",class="btn-danger"), modalButton("取消")), easyClose=TRUE))
    } else if (tp == "subitem") {
      all_subs <- duty_sub_item_get_all_with_parent()
      row <- all_subs[all_subs$id == eid, ]
      rv$duty_edit_type <- "subitem"; rv$duty_edit_id <- eid
      showModal(modalDialog(title=paste("编辑二级任务", row$name[1]), size="s",
        textInput("duty_edit_item_name","名称",value=row$name[1]),
        textInput("duty_edit_item_cat","分类",value=row$category[1] %||% ""),
        textInput("duty_edit_item_desc","描述",value=row$description[1] %||% ""),
        numericInput("duty_edit_item_sort","显示顺序",value=row$sort_order[1] %||% 0, min=0, max=999),
        footer=tagList(actionButton("duty_edit_save","保存",class="btn-primary"),
          actionButton("duty_edit_delete","删除",class="btn-danger"), modalButton("取消")), easyClose=TRUE))
    }
  })

  observeEvent(input$duty_card_del, {
    req(rv$logged_in, is_admin())
    tp <- input$duty_card_del$type; did <- as.integer(input$duty_card_del$id)
    result <- switch(tp,
      "position" = duty_position_delete(did),
      "staff" = duty_staff_delete(did),
      "item" = duty_item_delete(did),
      "subitem" = duty_sub_item_delete(did))
    refresh(); showNotification(result$message, type=if(result$success)"message" else "error")
  })

  ##################
  # 编辑弹窗：保存
  ##################
  observeEvent(input$duty_edit_save, {
    req(rv$logged_in, is_admin())
    tp <- rv$duty_edit_type; eid <- rv$duty_edit_id
    if (is.null(tp) || is.null(eid)) return()
    result <- if (tp == "position") {
      duty_position_update(eid, input$duty_edit_pos_name, input$duty_edit_pos_desc %||% "")
    } else if (tp == "staff") {
      duty_staff_update(eid, input$duty_edit_staff_name,
        department = input$duty_edit_staff_dept %||% "",
        email = input$duty_edit_staff_email %||% "",
        position_id = if (input$duty_edit_staff_pos != "") as.integer(input$duty_edit_staff_pos) else NULL)
    } else if (tp == "item") {
      duty_item_update(eid, input$duty_edit_item_name,
        category = input$duty_edit_item_cat %||% "",
        description = input$duty_edit_item_desc %||% "",
        sort_order = input$duty_edit_item_sort %||% 0)
    } else if (tp == "subitem") {
      duty_sub_item_update(eid, input$duty_edit_item_name,
        category = input$duty_edit_item_cat %||% "",
        description = input$duty_edit_item_desc %||% "",
        sort_order = input$duty_edit_item_sort %||% 0)
    }
    removeModal(); refresh()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 编辑弹窗：删除
  ##################
  observeEvent(input$duty_edit_delete, {
    req(rv$logged_in, is_admin())
    tp <- rv$duty_edit_type; eid <- rv$duty_edit_id
    if (is.null(tp) || is.null(eid)) return()
    result <- switch(tp,
      "position" = duty_position_delete(eid),
      "staff" = duty_staff_delete(eid),
      "item" = duty_item_delete(eid),
      "subitem" = duty_sub_item_delete(eid))
    removeModal(); refresh()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 按钮禁用/启用控制
  ##################
  observe({
    session$sendCustomMessage("toggleBtn", list(id="duty_add_position", disabled=!nzchar(trimws(input$duty_new_position_name %||% ""))))
  })
  observe({
    ok <- !is.null(input$duty_new_staff_user) && input$duty_new_staff_user != ""
    session$sendCustomMessage("toggleBtn", list(id="duty_add_staff", disabled=!ok))
  })
  observe({
    session$sendCustomMessage("toggleBtn", list(id="duty_add_item", disabled=!nzchar(trimws(input$duty_new_item_name %||% ""))))
  })
  observe({
    ok <- !is.null(input$duty_new_sub_item_parent) && input$duty_new_sub_item_parent != "" &&
          nzchar(trimws(input$duty_new_sub_item_name %||% ""))
    session$sendCustomMessage("toggleBtn", list(id="duty_add_sub_item", disabled=!ok))
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

  # 更新人员创建表单的岗位下拉选项 和 系统用户下拉
  observe({
    req(rv$logged_in)
    positions <- duty_position_get_all()
    pchoices <- if (nrow(positions) > 0) stats::setNames(positions$id, positions$name) else c("(请先创建岗位)" = "")
    updateSelectInput(session, "duty_new_staff_position", choices = c("(无)" = "", pchoices))
    # 系统用户列表
    con <- db_connect()
    users <- tryCatch({
      dbGetQuery(con, "SELECT id, username, display_name FROM users WHERE active = 1 ORDER BY username")
    }, finally = { db_disconnect(con) })
    if (nrow(users) > 0) {
      labels <- ifelse(!is.na(users$display_name) & users$display_name != "",
                       sprintf("%s (%s)", users$username, users$display_name),
                       users$username)
      uchoices <- stats::setNames(as.character(users$id), labels)
    } else {
      uchoices <- c("(无系统用户)" = "")
    }
    updateSelectInput(session, "duty_new_staff_user", choices = c("(选择系统用户)" = "", uchoices))
  })

  # 创建人员（从系统用户中选择）
  observeEvent(input$duty_add_staff, {
    req(rv$logged_in, input$duty_new_staff_user)
    uid <- as.integer(input$duty_new_staff_user)
    pos_id <- if (!is.null(input$duty_new_staff_position) && input$duty_new_staff_position != "") as.integer(input$duty_new_staff_position) else NULL
    # 查询用户名和邮箱
    con <- db_connect()
    user_info <- tryCatch({
      dbGetQuery(con, sprintf("SELECT username, display_name, email FROM users WHERE id = %d", uid))
    }, finally = { db_disconnect(con) })
    name <- if (nrow(user_info) > 0 && !is.na(user_info$display_name[1]) && user_info$display_name[1] != "")
              user_info$display_name[1] else user_info$username[1]
    email <- if (nrow(user_info) > 0) user_info$email[1] %||% "" else ""
    result <- duty_staff_add(name, department = "", email = email, user_id = uid, position_id = pos_id)
    if (result$success) {
      updateSelectInput(session, "duty_new_staff_user", selected = "")
    }
    refresh(); showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  # 创建职责
  observeEvent(input$duty_add_item, {
    req(rv$logged_in, input$duty_new_item_name)
    result <- duty_item_add(input$duty_new_item_name, input$duty_new_item_desc %||% "", input$duty_new_item_cat %||% "", input$duty_new_item_sort %||% 0)
    if (result$success) {
      updateTextInput(session,"duty_new_item_name",value="")
      updateTextInput(session,"duty_new_item_cat",value="")
      updateTextInput(session,"duty_new_item_desc",value="")
    }
    refresh(); showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  # 创建二级任务
  observeEvent(input$duty_add_sub_item, {
    req(rv$logged_in, input$duty_new_sub_item_parent, input$duty_new_sub_item_name)
    result <- duty_sub_item_add(
      as.integer(input$duty_new_sub_item_parent),
      input$duty_new_sub_item_name,
      input$duty_new_sub_item_desc %||% "",
      input$duty_new_sub_item_cat %||% "",
      input$duty_new_sub_item_sort %||% 0
    )
    if (result$success) {
      updateTextInput(session,"duty_new_sub_item_name",value="")
      updateTextInput(session,"duty_new_sub_item_cat",value="")
      updateTextInput(session,"duty_new_sub_item_desc",value="")
    }
    refresh(); showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  # 更新二级任务父级下拉选项
  observe({
    req(rv$logged_in, is_admin())
    items <- duty_item_get_all()
    if (nrow(items) > 0) {
      choices <- stats::setNames(as.character(items$id), items$name)
      updateSelectInput(session, "duty_new_sub_item_parent", choices = c("(选择上级职责)" = "", choices))
    }
  })
}
