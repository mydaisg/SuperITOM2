# 工位图 V2 - 服务端
# 复用 seat_map_management.r 数据层，重写交互逻辑

seat_map_v2_server <- function(input, output, session, rv) {

  sm_trigger   <- reactiveVal(0)
  sm_struct_trigger <- reactiveVal(0)
  refresh_all  <- function() { sm_trigger(sm_trigger() + 1); sm_struct_trigger(sm_struct_trigger() + 1) }
  refresh_seats <- function() { sm_trigger(sm_trigger() + 1) }

  # 画布缩放
  zoom_level <- reactiveVal(1)

  # 图例显示
  output$smv2_show_legend <- reactive({ TRUE })
  outputOptions(output, "smv2_show_legend", suspendWhenHidden = FALSE)

  ##################
  # 楼栋/楼层数据
  ##################
  sm_buildings <- reactive({
    sm_struct_trigger(); req(rv$logged_in)
    building_get_all()
  })

  sm_floors <- reactive({
    sm_struct_trigger(); req(rv$logged_in)
    bid <- input$smv2_building
    if (is.null(bid) || bid == "") return(data.frame())
    floor_get_all(bid)
  })

  ##################
  # 楼层快照
  ##################
  sm_snapshot <- reactive({
    sm_trigger(); req(rv$logged_in)
    fid <- input$smv2_floor
    if (is.null(fid) || fid == "") return(NULL)
    seat_floor_snapshot(fid)
  })

  ##################
  # 楼层标题
  ##################
  output$smv2_floor_title <- renderText({
    snap <- sm_snapshot()
    if (is.null(snap)) return("请选择楼栋和楼层")
    bld <- sm_buildings()
    bname <- if (nrow(bld) > 0) bld$name[bld$id == as.integer(input$smv2_building)][1] %||% "" else ""
    paste0(bname, " / ", snap$floor$name[1])
  })

  ##################
  # 更新楼栋下拉
  ##################
  observe({
    req(rv$logged_in)
    bld <- sm_buildings()
    choices <- if (nrow(bld) > 0) setNames(as.character(bld$id), bld$name) else c("\u2014 \u8BF7\u5148\u6DFB\u52A0\u697C\u680B \u2014" = "")
    cur <- isolate(input$smv2_building)
    sel <- if (length(cur) == 0 || is.null(cur) || !cur %in% as.character(bld$id)) {
      if (nrow(bld) > 0) as.character(bld$id[1]) else "" } else cur
    updateSelectizeInput(session, "smv2_building", choices = choices, selected = sel, server = TRUE)
  })

  observe({
    req(rv$logged_in, length(isolate(input$smv2_building)) > 0)
    floors <- sm_floors()
    choices <- if (nrow(floors) > 0) setNames(as.character(floors$id), floors$name) else c("\u2014 \u5148\u9009\u697C\u680B \u2014" = "")
    cur <- isolate(input$smv2_floor)
    sel <- if (length(cur) == 0 || is.null(cur) || !cur %in% as.character(floors$id)) {
      if (nrow(floors) > 0) as.character(floors$id[1]) else "" } else cur
    updateSelectizeInput(session, "smv2_floor", choices = choices, selected = sel, server = TRUE)
  })

  ##################
  # 画布渲染（绝对定位 + 网格背景）
  ##################
  output$smv2_canvas <- renderUI({
    req(rv$logged_in)
    snap <- sm_snapshot()
    if (is.null(snap)) {
      return(tags$div(style = "text-align:center; padding:80px 20px; color:#999;",
        tags$div(icon("building", "fa-4x"), style = "margin-bottom:16px;"),
        tags$h4("请选择楼栋和楼层"),
        tags$p("在上方下拉框中选择楼栋和楼层，或点击按钮创建新的楼栋/楼层")))
    }

    seats <- snap$seats
    zones <- snap$zones

    # 计算画布大小：基于最大行列号
    max_r <- max(snap$max_row, 1)
    max_c <- max(snap$max_col, 1)

    # 每个工位格子大小
    cell_w <- 90   # 宽度
    cell_h <- 85   # 高度
    gap     <- 8   # 间距

    canvas_w <- max_c * (cell_w + gap) + 40
    canvas_h <- max_r * (cell_h + gap) + 40

    # 状态筛选
    status_filter <- input$smv2_filter_status
    search_kw <- trimws(input$smv2_search)

    elements <- list()

    # ── 渲染区域块 ──
    if (nrow(zones) > 0) {
      for (i in seq_len(nrow(zones))) {
        z <- zones[i, ]
        rs <- max(as.integer(z$row_start[1] %||% 1), 1)
        cs <- max(as.integer(z$col_start[1] %||% 1), 1)
        rp <- max(as.integer(z$row_span[1] %||% 1), 1)
        cp <- max(as.integer(z$col_span[1] %||% 1), 1)

        left   <- (cs - 1) * (cell_w + gap) + 20
        top    <- (rs - 1) * (cell_h + gap) + 20
        width  <- cp * (cell_w + gap) - gap
        height <- rp * (cell_h + gap) - gap

        z_color <- zone_type_color(z$zone_type[1])

        elements <- c(elements, list(
          tags$div(class = "smv2-zone",
            style = sprintf("left:%dpx; top:%dpx; width:%dpx; height:%dpx; background:%s;",
              left, top, width, height, z_color),
            tags$span(paste0(z$name[1], "\n", zone_type_label(z$zone_type[1])))
          )
        ))
      }
    }

    # ── 渲染工位 ──
    if (nrow(seats) > 0) {
      for (i in seq_len(nrow(seats))) {
        s <- seats[i, ]

        # 搜索筛选
        if (length(search_kw) > 0 && search_kw != "") {
          match_code <- grepl(search_kw, s$seat_code[1], ignore.case = TRUE)
          match_user <- grepl(search_kw, s$user_name[1] %||% "", ignore.case = TRUE)
          match_host <- grepl(search_kw, s$asset_hostname[1] %||% "", ignore.case = TRUE)
          if (!match_code && !match_user && !match_host) next
        }

        # 状态筛选
        if (length(status_filter) > 0 && status_filter != "all") {
          if (s$status[1] != status_filter) next
        }

        status_class <- switch(s$status[1],
          "occupied" = "occupied",
          "vacant_no_pc" = "vacant-no-pc",
          "vacant_with_pc" = "vacant-with-pc",
          "vacant-no-pc")

        rn <- as.integer(s$row_num[1])
        cn <- as.integer(s$col_num[1])
        left <- (cn - 1) * (cell_w + gap) + 20
        top  <- (rn - 1) * (cell_h + gap) + 20

        name_display <- s$user_name[1] %||% ""
        host_display <- s$asset_hostname[1] %||% ""

        elements <- c(elements, list(
          tags$div(class = paste("smv2-seat", status_class),
            style = sprintf("left:%dpx; top:%dpx; width:%dpx; height:%dpx;", left, top, cell_w, cell_h),
            `data-id` = s$id[1],
            `data-code` = s$seat_code[1],
            `data-status` = s$status[1],
            `data-row` = rn,
            `data-col` = cn,
            title = paste0(s$seat_code[1],
              if (nchar(name_display) > 0) paste0(" | ", name_display) else "",
              if (nchar(host_display) > 0) paste0(" | ", host_display) else ""),
            tags$span(class = "s-code", s$seat_code[1]),
            if (nchar(name_display) > 0) tags$span(class = "s-user", name_display) else "",
            if (nchar(host_display) > 0) tags$span(class = "s-host", host_display) else ""
          )
        ))
      }
    }

    if (length(elements) == 0) {
      return(tags$div(style = "text-align:center; padding:60px; color:#999;",
        tags$p(icon("chair", "fa-3x")), tags$p("该楼层暂无工位，请先添加区域和工位")))
    }

    tags$div(style = sprintf("position:relative; width:%dpx; height:%dpx;", canvas_w, canvas_h),
      elements
    )
  })

  ##################
  # 左侧工位列表
  ##################
  output$smv2_seat_list <- renderUI({
    req(rv$logged_in)
    snap <- sm_snapshot()
    if (is.null(snap) || nrow(snap$seats) == 0) {
      return(tags$div(style = "padding:20px; text-align:center; color:#999; font-size:12px;",
        "暂无工位"))
    }
    seats <- snap$seats
    search_kw <- trimws(input$smv2_search)

    # 筛选
    if (length(search_kw) > 0 && search_kw != "") {
      idx <- grepl(search_kw, seats$seat_code, ignore.case = TRUE) |
        grepl(search_kw, seats$user_name %||% "", ignore.case = TRUE) |
        grepl(search_kw, seats$asset_hostname %||% "", ignore.case = TRUE)
      seats <- seats[idx, , drop = FALSE]
    }

    status_filter <- input$smv2_filter_status
    if (length(status_filter) > 0 && status_filter != "all") {
      seats <- seats[seats$status == status_filter, , drop = FALSE]
    }

    if (nrow(seats) == 0) {
      return(tags$div(style = "padding:20px; text-align:center; color:#999; font-size:12px;",
        "无匹配结果"))
    }

    cards <- lapply(seq_len(nrow(seats)), function(i) {
      s <- seats[i, ]
      status_dot_class <- switch(s$status[1],
        "occupied" = "occupied",
        "vacant_no_pc" = "vacant-no-pc",
        "vacant_with_pc" = "vacant-with-pc",
        "vacant-no-pc")
      tags$div(class = "smv2-seat-card", `data-id` = s$id[1],
        tags$span(class = paste("seat-status", status_dot_class)),
        tags$span(class = "seat-code", s$seat_code[1]),
        tags$br(),
        if (!is.na(s$user_name[1]) && s$user_name[1] != "")
          tags$span(class = "seat-user", icon("user"), s$user_name[1]) else
          tags$span(class = "seat-user", style = "color:#aaa;", "(空)")
      )
    })
    tagList(cards)
  })

  # 工位计数
  output$smv2_seat_count <- renderText({
    snap <- sm_snapshot()
    if (is.null(snap)) return("")
    sprintf("共 %d 个", nrow(snap$seats))
  })

  ##################
  # 选中工位 → 右侧详情面板
  ##################
  observeEvent(input$smv2_seat_select, {
    req(rv$logged_in)
    sid <- as.integer(input$smv2_seat_select)
    seat <- seat_get_by_id(sid)
    if (is.null(seat) || nrow(seat) == 0) return()

    s <- seat[1, ]
    status_label <- seat_status_label(s$status[1])
    status_color <- switch(s$status[1],
      "occupied" = "#4caf50", "vacant_no_pc" = "#9e9e9e", "vacant_with_pc" = "#2196f3", "#999")

    output$smv2_detail_content <- renderUI({
      tagList(
        tags$div(class = "info-row",
          tags$span(class = "info-label", "编号"),
          tags$span(class = "info-value", tags$b(s$seat_code[1]))),
        tags$div(class = "info-row",
          tags$span(class = "info-label", "状态"),
          tags$span(class = "info-value",
            tags$span(style = sprintf("display:inline-block; width:10px; height:10px; border-radius:50%%; background:%s; margin-right:6px;", status_color)),
            status_label)),
        tags$div(class = "info-row",
          tags$span(class = "info-label", "使用者"),
          tags$span(class = "info-value", s$user_name[1] %||% tags$span(style = "color:#999;", "未分配"))),
        tags$div(class = "info-row",
          tags$span(class = "info-label", "资产"),
          tags$span(class = "info-value", s$asset_hostname[1] %||% tags$span(style = "color:#999;", "未绑定"))),
        tags$div(class = "info-row",
          tags$span(class = "info-label", "位置"),
          tags$span(class = "info-value", sprintf("行%02d 列%02d", as.integer(s$row_num[1]), as.integer(s$col_num[1])))),
        if (!is.null(s$zone_name[1]) && !is.na(s$zone_name[1]))
          tags$div(class = "info-row",
            tags$span(class = "info-label", "区域"),
            tags$span(class = "info-value", s$zone_name[1])) else "",
        if (!is.null(s$description[1]) && !is.na(s$description[1]) && s$description[1] != "")
          tags$div(class = "info-row",
            tags$span(class = "info-label", "备注"),
            tags$span(class = "info-value", s$description[1])) else "",
        tags$hr(),
        tags$div(style = "display:flex; gap:8px;",
          actionButton("smv2_detail_edit", "编辑", icon = icon("edit"), class = "btn-sm btn-primary"),
          actionButton("smv2_detail_delete", "删除", icon = icon("trash"), class = "btn-sm btn-danger")
        )
      )
    })

    # 显示面板
    session$sendCustomMessage("smv2_show_detail", TRUE)
  })

  # 详情面板编辑按钮
  observeEvent(input$smv2_detail_edit, {
    req(input$smv2_seat_select)
    show_seat_edit_modal(as.integer(input$smv2_seat_select))
  })

  # 详情面板删除按钮
  observeEvent(input$smv2_detail_delete, {
    req(input$smv2_seat_select)
    show_seat_delete_confirm(as.integer(input$smv2_seat_select))
  })

  ##################
  # 右键菜单：快速修改状态
  ##################
  observeEvent(input$smv2_ctx_status, {
    req(rv$logged_in)
    sid <- as.integer(input$smv2_ctx_status$id)
    new_status <- input$smv2_ctx_status$status
    result <- seat_update(sid, status = new_status)
    refresh_seats()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 右键菜单：编辑
  observeEvent(input$smv2_ctx_edit, {
    req(rv$logged_in)
    show_seat_edit_modal(as.integer(input$smv2_ctx_edit))
  })

  # 右键菜单：删除
  observeEvent(input$smv2_ctx_delete, {
    req(rv$logged_in)
    show_seat_delete_confirm(as.integer(input$smv2_ctx_delete))
  })

  ##################
  # 编辑工位弹窗（复用函数）
  ##################
  show_seat_edit_modal <- function(sid) {
    seat <- seat_get_by_id(sid)
    if (is.null(seat) || nrow(seat) == 0) return()
    s <- seat[1, ]
    users_choices <- c("(无)" = "", seat_user_choices())
    assets_list <- asset_get_all()
    asset_choices <- if (nrow(assets_list) > 0) {
      c("(无)" = "", setNames(as.character(assets_list$id),
        paste0(assets_list$hostname, " (", assets_list$ip_address %||% "", ")")))
    } else c("(无)" = "")

    showModal(modalDialog(
      title = paste("编辑工位", s$seat_code[1]),
      size = "m", easyClose = TRUE,
      fluidRow(
        column(6, selectizeInput("smv2_edit_status", "状态", width = "100%",
          choices = c("有员工" = "occupied", "无员工无电脑" = "vacant_no_pc", "无员工有电脑" = "vacant_with_pc"),
          selected = s$status[1])),
        column(6, textInput("smv2_edit_code", "工位编号", value = s$seat_code[1]))
      ),
      fluidRow(
        column(6, numericInput("smv2_edit_row", "行号", value = as.integer(s$row_num[1]), min = 1)),
        column(6, numericInput("smv2_edit_col", "列号", value = as.integer(s$col_num[1]), min = 1))
      ),
      fluidRow(
        column(6, selectizeInput("smv2_edit_user", "使用者", width = "100%",
          choices = users_choices, selected = as.character(s$user_id[1] %||% ""))),
        column(6, selectizeInput("smv2_edit_asset", "绑定资产", width = "100%",
          choices = asset_choices, selected = as.character(s$asset_id[1] %||% "")))
      ),
      textAreaInput("smv2_edit_desc", "备注", value = s$description[1] %||% "", rows = 2),
      tags$hr(),
      tags$div(style = "font-size:12px; color:#666;",
        tags$p(sprintf("当前位置: 行%02d 列%02d", as.integer(s$row_num[1]), as.integer(s$col_num[1]))),
        if (!is.null(s$zone_name[1]) && !is.na(s$zone_name[1]))
          tags$p(sprintf("区域: %s (%s)", s$zone_name[1], zone_type_label(s$zone_type[1] %||% ""))) else ""
      ),
      footer = tagList(
        modalButton("关闭"),
        actionButton("smv2_save_seat", "保存", class = "btn-primary")
      )
    ))
  }

  observeEvent(input$smv2_save_seat, {
    req(rv$logged_in)
    sid <- as.integer(input$smv2_seat_select)
    uid <- input$smv2_edit_user; if (is.null(uid) || uid == "") uid <- NA_character_
    aid <- input$smv2_edit_asset; if (is.null(aid) || aid == "") aid <- NA_character_
    result <- seat_update(sid,
      seat_code = input$smv2_edit_code,
      status = input$smv2_edit_status,
      user_id = uid, asset_id = aid,
      description = input$smv2_edit_desc)
    if (result$success) { removeModal(); refresh_seats() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 删除确认
  ##################
  show_seat_delete_confirm <- function(sid) {
    seat <- seat_get_by_id(sid)
    if (is.null(seat) || nrow(seat) == 0) return()
    s <- seat[1, ]
    showModal(modalDialog(
      title = "确认删除工位",
      tags$div(style = "font-size:13px;",
        tags$p(tags$b("即将删除以下工位，操作不可恢复：")),
        tags$table(class = "table table-bordered table-sm", style = "font-size:12px;",
          tags$tbody(
            tags$tr(tags$td(style = "font-weight:600;width:60px;", "编号"), tags$td(tags$b(s$seat_code[1]))),
            tags$tr(tags$td(style = "font-weight:600;", "使用者"), tags$td(s$user_name[1] %||% "—")),
            tags$tr(tags$td(style = "font-weight:600;", "资产"), tags$td(s$asset_hostname[1] %||% "—"))
          )
        )
      ),
      footer = tagList(modalButton("取消"),
        actionButton("smv2_del_seat_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  }

  observeEvent(input$smv2_del_seat_confirm, {
    req(rv$logged_in)
    sid <- as.integer(input$smv2_seat_select)
    result <- seat_delete(sid)
    removeModal(); refresh_seats()
    session$sendCustomMessage("smv2_show_detail", FALSE)
    showNotification(result$message, type = "message")
  })

  ##################
  # 快速编辑模式
  ##################
  observeEvent(input$smv2_quick_edit, {
    showNotification("快速编辑模式：点击工位后在右侧面板修改，修改即时生效", type = "message", duration = 3)
  })

  ##################
  # 添加楼栋/楼层/区域/工位/批量（复用旧版逻辑）
  ##################

  # 添加楼栋
  observeEvent(input$smv2_add_building, {
    req(rv$logged_in)
    showModal(modalDialog(title = "添加楼栋", size = "s", easyClose = TRUE,
      textInput("smv2_new_building_name", "楼栋名称 *", placeholder = "例如：A栋"),
      textInput("smv2_new_building_desc", "描述"),
      footer = tagList(modalButton("取消"),
        actionButton("smv2_add_building_confirm", "添加", class = "btn-primary"))))
  })
  observeEvent(input$smv2_add_building_confirm, {
    req(rv$logged_in, input$smv2_new_building_name)
    result <- building_add(input$smv2_new_building_name, input$smv2_new_building_desc %||% "")
    if (result$success) { removeModal(); refresh_all() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 添加楼层
  observeEvent(input$smv2_add_floor, {
    req(rv$logged_in, input$smv2_building)
    showModal(modalDialog(title = "添加楼层", size = "s", easyClose = TRUE,
      textInput("smv2_new_floor_name", "楼层名称 *", placeholder = "例如：2F"),
      numericInput("smv2_new_floor_number", "楼层号", value = NULL, min = 1),
      textInput("smv2_new_floor_desc", "描述"),
      footer = tagList(modalButton("取消"),
        actionButton("smv2_add_floor_confirm", "添加", class = "btn-primary"))))
  })
  observeEvent(input$smv2_add_floor_confirm, {
    req(rv$logged_in, input$smv2_building, input$smv2_new_floor_name)
    result <- floor_add(input$smv2_building, input$smv2_new_floor_name,
      input$smv2_new_floor_number, input$smv2_new_floor_desc %||% "")
    if (result$success) { removeModal(); refresh_all() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 添加区域
  observeEvent(input$smv2_add_zone, {
    req(rv$logged_in, input$smv2_floor)
    showModal(modalDialog(title = "添加区域", size = "s", easyClose = TRUE,
      textInput("smv2_new_zone_name", "区域名称 *", placeholder = "例如：开放办公区"),
      selectizeInput("smv2_new_zone_type", "类型", width = "100%",
        choices = c("前台" = "reception", "大厅卡座" = "open_desk", "会议室" = "meeting_room",
          "实验室" = "lab", "仓库" = "warehouse", "小办公室" = "small_office",
          "茶室" = "tea_room", "吸烟室" = "smoking_room")),
      fluidRow(
        column(6, numericInput("smv2_new_zone_rs", "起始行", value = 1, min = 1)),
        column(6, numericInput("smv2_new_zone_cs", "起始列", value = 1, min = 1))
      ),
      fluidRow(
        column(6, numericInput("smv2_new_zone_rspan", "行数", value = 1, min = 1)),
        column(6, numericInput("smv2_new_zone_cspan", "列数", value = 1, min = 1))
      ),
      textInput("smv2_new_zone_desc", "描述"),
      footer = tagList(modalButton("取消"),
        actionButton("smv2_add_zone_confirm", "添加", class = "btn-primary"))))
  })
  observeEvent(input$smv2_add_zone_confirm, {
    req(rv$logged_in, input$smv2_floor, input$smv2_new_zone_name)
    result <- zone_add(input$smv2_floor, input$smv2_new_zone_name, input$smv2_new_zone_type,
      input$smv2_new_zone_rs, input$smv2_new_zone_cs, input$smv2_new_zone_rspan, input$smv2_new_zone_cspan,
      input$smv2_new_zone_desc %||% "")
    if (result$success) { removeModal(); refresh_all() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 添加工位
  observeEvent(input$smv2_add_seat, {
    req(rv$logged_in, input$smv2_floor)
    snap <- sm_snapshot()
    zone_choices <- if (!is.null(snap) && nrow(snap$zones) > 0)
      c("\u2014 \u65E0\u533A\u57DF \u2014" = "", setNames(as.character(snap$zones$id), snap$zones$name)) else c("\u2014 \u65E0\u533A\u57DF \u2014" = "")
    showModal(modalDialog(title = "添加工位", size = "s", easyClose = TRUE,
      textInput("smv2_new_seat_code", "工位编号 *", placeholder = "例如：28-01"),
      fluidRow(
        column(6, numericInput("smv2_new_seat_row", "行号", value = 1, min = 1)),
        column(6, numericInput("smv2_new_seat_col", "列号", value = 1, min = 1))
      ),
      selectizeInput("smv2_new_seat_zone", "区域", choices = zone_choices, width = "100%"),
      selectizeInput("smv2_new_seat_status", "状态", width = "100%",
        choices = c("有员工" = "occupied", "无员工无电脑" = "vacant_no_pc", "无员工有电脑" = "vacant_with_pc"),
        selected = "vacant_no_pc"),
      footer = tagList(modalButton("取消"),
        actionButton("smv2_add_seat_confirm", "添加", class = "btn-success"))))
  })
  observeEvent(input$smv2_add_seat_confirm, {
    req(rv$logged_in, input$smv2_floor, input$smv2_new_seat_code)
    zid <- input$smv2_new_seat_zone; if (is.null(zid) || zid == "") zid <- NA
    result <- seat_add(input$smv2_floor, zid, input$smv2_new_seat_code,
      input$smv2_new_seat_row, input$smv2_new_seat_col, input$smv2_new_seat_status)
    if (result$success) { removeModal(); refresh_seats() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 批量生成
  observeEvent(input$smv2_batch_seats, {
    req(rv$logged_in, input$smv2_floor)
    snap <- sm_snapshot()
    zone_choices <- if (!is.null(snap) && nrow(snap$zones) > 0)
      c("\u2014 \u65E0\u533A\u57DF \u2014" = "", setNames(as.character(snap$zones$id), snap$zones$name)) else c("\u2014 \u65E0\u533A\u57DF \u2014" = "")
    showModal(modalDialog(title = "批量生成工位", size = "s", easyClose = TRUE,
      textInput("smv2_batch_prefix", "编号前缀 *", placeholder = "例如：28"),
      selectizeInput("smv2_batch_zone", "区域", choices = zone_choices, width = "100%"),
      fluidRow(
        column(6, numericInput("smv2_batch_start_row", "起始行", value = 1, min = 1)),
        column(6, numericInput("smv2_batch_start_col", "起始列", value = 1, min = 1))
      ),
      fluidRow(
        column(6, numericInput("smv2_batch_rows", "行数", value = 4, min = 1, max = 50)),
        column(6, numericInput("smv2_batch_cols", "列数", value = 6, min = 1, max = 50))
      ),
      numericInput("smv2_batch_start_num", "起始编号", value = 1, min = 1),
      tags$p(style = "font-size:11px;color:#888;",
        "示例：前缀28，起始编号1，4行6列 → 28-01 ~ 28-24"),
      footer = tagList(modalButton("取消"),
        actionButton("smv2_batch_seats_confirm", "生成", class = "btn-info"))))
  })
  observeEvent(input$smv2_batch_seats_confirm, {
    req(rv$logged_in, input$smv2_floor, input$smv2_batch_prefix)
    zid <- input$smv2_batch_zone; if (is.null(zid) || zid == "") zid <- NA
    result <- seat_batch_generate(input$smv2_floor, zid, input$smv2_batch_prefix,
      input$smv2_batch_start_row, input$smv2_batch_start_col,
      input$smv2_batch_rows, input$smv2_batch_cols, input$smv2_batch_start_num)
    if (result$success) { removeModal(); refresh_seats() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  # 删除楼栋/楼层
  observeEvent(input$smv2_del_building, {
    req(rv$logged_in)
    fid <- input$smv2_floor; bid <- input$smv2_building
    if (!is.null(fid) && fid != "") {
      fl <- floor_get_by_id(fid)
      if (is.null(fl) || nrow(fl) == 0) return()
      showModal(modalDialog(title = "确认删除楼层",
        tags$div(style = "font-size:13px;",
          tags$p(tags$b(sprintf("确定删除楼层 [%s] 吗？", fl$name[1]))),
          tags$p(style = "color:#d9534f;", "该楼层下的所有区域和工位将一并删除。")),
        footer = tagList(modalButton("取消"),
          actionButton("smv2_del_floor_confirm", "确认删除", class = "btn-danger")),
        size = "s", easyClose = TRUE))
    } else if (!is.null(bid) && bid != "") {
      bld <- building_get_by_id(bid)
      if (is.null(bld) || nrow(bld) == 0) return()
      showModal(modalDialog(title = "确认删除楼栋",
        tags$div(style = "font-size:13px;",
          tags$p(tags$b(sprintf("确定删除楼栋 [%s] 吗？", bld$name[1]))),
          tags$p(style = "color:#d9534f;", "该楼栋下的所有楼层、区域和工位将一并删除。")),
        footer = tagList(modalButton("取消"),
          actionButton("smv2_del_building_confirm", "确认删除", class = "btn-danger")),
        size = "s", easyClose = TRUE))
    } else {
      showNotification("请先选择楼栋或楼层", type = "warning")
    }
  })
  observeEvent(input$smv2_del_floor_confirm, {
    req(rv$logged_in, input$smv2_floor)
    result <- floor_delete(as.integer(input$smv2_floor))
    removeModal(); refresh_all()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })
  observeEvent(input$smv2_del_building_confirm, {
    req(rv$logged_in, input$smv2_building)
    result <- building_delete(as.integer(input$smv2_building))
    removeModal(); refresh_all()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 缩放控制
  ##################
  observeEvent(input$smv2_zoom_in, {
    zl <- zoom_level() + 0.1
    if (zl > 2.5) zl <- 2.5
    zoom_level(zl)
    session$sendCustomMessage("smv2_zoom", list(scale = zl))
  })
  observeEvent(input$smv2_zoom_out, {
    zl <- zoom_level() - 0.1
    if (zl < 0.3) zl <- 0.3
    zoom_level(zl)
    session$sendCustomMessage("smv2_zoom", list(scale = zl))
  })
  observeEvent(input$smv2_zoom_fit, {
    zoom_level(1)
    session$sendCustomMessage("smv2_zoom", list(scale = 1))
  })

  ##################
  # 图例切换
  ##################
  observeEvent(input$smv2_legend_toggle, {
    output$smv2_show_legend <- reactive({ !isolate(output$smv2_show_legend()) })
    outputOptions(output, "smv2_show_legend", suspendWhenHidden = FALSE)
  })

  ##################
  # 刷新
  ##################
  observeEvent(input$smv2_refresh, { refresh_all() })
}
