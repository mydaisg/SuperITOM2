# 工位图模块 - 服务端

seat_map_server <- function(input, output, session, rv) {

  sm_trigger <- reactiveVal(0)
  refresh <- function() { sm_trigger(sm_trigger() + 1) }

  ##################
  # 楼栋列表
  ##################
  sm_buildings <- reactive({
    sm_trigger(); req(rv$logged_in)
    building_get_all()
  })

  ##################
  # 楼层列表（依赖楼栋选择）
  ##################
  sm_floors <- reactive({
    sm_trigger(); req(rv$logged_in)
    bid <- input$sm_building
    if (is.null(bid) || bid == "") return(data.frame())
    floor_get_all(bid)
  })

  ##################
  # 当前楼层快照
  ##################
  sm_snapshot <- reactive({
    sm_trigger(); req(rv$logged_in)
    fid <- input$sm_floor
    if (is.null(fid) || fid == "") return(NULL)
    seat_floor_snapshot(fid)
  })

  ##################
  # 更新楼栋下拉
  ##################
  observe({
    req(input$asset_tabs == "seat")
    bld <- sm_buildings()
    req(nrow(bld) >= 0)
    choices <- if (nrow(bld) > 0) setNames(as.character(bld$id), bld$name) else c("\u2014 \u8BF7\u5148\u6DFB\u52A0\u697C\u680B \u2014" = "")
    selected <- input$sm_building
    if (is.null(selected) || !selected %in% as.character(bld$id)) selected <- if (nrow(bld) > 0) as.character(bld$id[1]) else ""
    updateSelectizeInput(session, "sm_building", choices = choices, selected = selected, server = TRUE)
  })

  observe({
    req(input$asset_tabs == "seat")
    floors <- sm_floors()
    choices <- if (nrow(floors) > 0) setNames(as.character(floors$id), floors$name) else c("\u2014 \u5148\u9009\u697C\u680B \u2014" = "")
    selected <- input$sm_floor
    if (is.null(selected) || !selected %in% as.character(floors$id)) selected <- if (nrow(floors) > 0) as.character(floors$id[1]) else ""
    updateSelectizeInput(session, "sm_floor", choices = choices, selected = selected, server = TRUE)
  })

  ##################
  # 工位图主体渲染
  ##################
  output$sm_canvas <- renderUI({
    req(input$asset_tabs == "seat")
    snap <- sm_snapshot()
    if (is.null(snap)) return(tags$div(style = "text-align:center;padding:60px;color:#999;",
      tags$p(icon("building", "fa-3x")), tags$p("请选择楼栋和楼层")))
    seats <- snap$seats; zones <- snap$zones
    max_r <- max(snap$max_row, 1); max_c <- max(snap$max_col, 1)

    # 构建网格单元格
    cells <- list()
    # 先渲染区域块
    if (nrow(zones) > 0) {
      for (i in seq_len(nrow(zones))) {
        z <- zones[i, ]
        z_type_css <- paste0("sz-", z$zone_type[1])
        rs <- as.integer(z$row_start[1] %||% 1); cs <- as.integer(z$col_start[1] %||% 1)
        rp <- as.integer(z$row_span[1] %||% 1); cp <- as.integer(z$col_span[1] %||% 1)
        rs <- max(rs, 1); cs <- max(cs, 1); rp <- max(rp, 1); cp <- max(cp, 1)
        label <- paste0(z$name[1], "\n", zone_type_label(z$zone_type[1]))
        cells <- c(cells, list(
          tags$div(class = paste("sm-zone-cell", z_type_css),
            style = sprintf("grid-row:%d/span %d; grid-column:%d/span %d;",
              rs, rp, cs, cp),
            title = paste(z$name[1], "-", zone_type_label(z$zone_type[1])),
            HTML(gsub("\n", "<br>", label))
          )
        ))
      }
    }

    # 再渲染工位
    if (nrow(seats) > 0) {
      for (i in seq_len(nrow(seats))) {
        s <- seats[i, ]
        status_css <- switch(s$status[1],
          "occupied" = "sm-occupied",
          "vacant_no_pc" = "sm-vacant-no-pc",
          "vacant_with_pc" = "sm-vacant-with-pc",
          "sm-vacant-no-pc")
        name_display <- s$user_name[1] %||% ""
        host_display <- s$asset_hostname[1] %||% ""
        cells <- c(cells, list(
          tags$div(class = paste("sm-cell", status_css),
            style = sprintf("grid-row:%d; grid-column:%d;",
              as.integer(s$row_num[1]), as.integer(s$col_num[1])),
            `data-id` = s$id[1], `data-code` = s$seat_code[1],
            title = paste0(s$seat_code[1],
              if (nchar(name_display) > 0) paste0(" | ", name_display) else "",
              if (nchar(host_display) > 0) paste0(" | ", host_display) else ""),
            tags$span(class = "sm-code", s$seat_code[1]),
            if (nchar(name_display) > 0) tags$span(class = "sm-name", name_display) else "",
            if (nchar(host_display) > 0) tags$span(class = "sm-host", host_display) else ""
          )
        ))
      }
    }

    if (length(cells) == 0) {
      return(tags$div(style = "text-align:center;padding:40px;color:#999;",
        tags$p("暂无工位数据，请添加区域和工位")))
    }

    tags$div(class = "sm-floor-canvas", style = "max-height:70vh;",
      tags$div(class = "sm-grid-wrap",
        tags$div(class = "sm-grid",
          style = sprintf("grid-template-rows: repeat(%d, auto); grid-template-columns: repeat(%d, 1fr);",
            max(max_r, 1), max(max_c, 1)),
          cells
        )
      )
    )
  })

  ##################
  # 点击工位 → 弹出详情
  ##################
  observeEvent(input$sm_seat_click, {
    req(rv$logged_in)
    sid <- as.integer(input$sm_seat_click$id)
    seat <- seat_get_by_id(sid)
    if (is.null(seat) || nrow(seat) == 0) return()
    s <- seat[1, ]
    users_choices <- c("(无)" = "", seat_user_choices())
    assets_list <- asset_get_all()
    asset_choices <- if (nrow(assets_list) > 0) {
      c("(无)" = "", setNames(as.character(assets_list$id), paste0(assets_list$hostname, " (", assets_list$ip_address %||% "", ")")))
    } else c("(无)" = "")

    showModal(modalDialog(
      title = paste("工位", s$seat_code[1]),
      size = "m", easyClose = TRUE,
      tags$div(style = "display:flex; gap:10px; margin-bottom:12px;",
        tags$div(style = "flex:1;",
          selectizeInput("sm_edit_status", "状态", width = "100%",
            choices = c("有员工" = "occupied", "无员工无电脑" = "vacant_no_pc", "无员工有电脑" = "vacant_with_pc"),
            selected = s$status[1])),
        tags$div(style = "flex:1;",
          selectizeInput("sm_edit_user", "使用者", width = "100%",
            choices = users_choices, selected = as.character(s$user_id[1] %||% ""))),
        tags$div(style = "flex:1;",
          selectizeInput("sm_edit_asset", "绑定资产", width = "100%",
            choices = asset_choices, selected = as.character(s$asset_id[1] %||% "")))
      ),
      textInput("sm_edit_code", "工位编号", value = s$seat_code[1]),
      textAreaInput("sm_edit_desc", "备注", value = s$description[1] %||% "", rows = 2),
      tags$hr(),
      tags$div(style = "font-size:12px; color:#666;",
        tags$p(sprintf("位置: 行%02d 列%02d", as.integer(s$row_num[1]), as.integer(s$col_num[1]))),
        if (!is.null(s$zone_name[1]) && !is.na(s$zone_name[1]))
          tags$p(sprintf("区域: %s (%s)", s$zone_name[1], zone_type_label(s$zone_type[1] %||% ""))) else ""
      ),
      footer = tagList(
        modalButton("关闭"),
        actionButton("sm_save_seat", "保存", class = "btn-primary"),
        actionButton("sm_del_seat", "删除工位", class = "btn-danger")
      )
    ))
  })

  ##################
  # 保存工位修改
  ##################
  observeEvent(input$sm_save_seat, {
    req(rv$logged_in, input$sm_seat_click)
    sid <- as.integer(input$sm_seat_click$id)
    uid <- input$sm_edit_user; if (is.null(uid) || uid == "") uid <- NA_character_
    aid <- input$sm_edit_asset; if (is.null(aid) || aid == "") aid <- NA_character_
    result <- seat_update(sid,
      seat_code = input$sm_edit_code,
      status = input$sm_edit_status,
      user_id = uid, asset_id = aid,
      description = input$sm_edit_desc)
    if (result$success) { removeModal(); refresh() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 删除工位
  ##################
  observeEvent(input$sm_del_seat, {
    req(rv$logged_in, input$sm_seat_click)
    sid <- as.integer(input$sm_seat_click$id)
    seat <- seat_get_by_id(sid)
    if (is.null(seat) || nrow(seat) == 0) return()
    showModal(modalDialog(
      title = "确认删除工位",
      tags$div(style = "font-size:13px;",
        tags$p(tags$b("即将删除以下工位，操作不可恢复：")),
        tags$table(class = "table table-bordered table-sm", style = "font-size:12px;",
          tags$tbody(
            tags$tr(tags$td(style = "font-weight:600;width:60px;","编号"), tags$td(tags$b(seat$seat_code[1]))),
            tags$tr(tags$td(style = "font-weight:600;","使用者"), tags$td(seat$user_name[1] %||% "—")),
            tags$tr(tags$td(style = "font-weight:600;","资产"), tags$td(seat$asset_hostname[1] %||% "—"))
          )
        )
      ),
      footer = tagList(modalButton("取消"),
        actionButton("sm_del_seat_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$sm_del_seat_confirm, {
    req(rv$logged_in, input$sm_seat_click)
    result <- seat_delete(as.integer(input$sm_seat_click$id))
    removeModal(); refresh()
    showNotification(result$message, type = "message")
  })

  ##################
  # 添加楼栋
  ##################
  observeEvent(input$sm_add_building, {
    req(rv$logged_in)
    showModal(modalDialog(title = "添加楼栋", size = "s", easyClose = TRUE,
      textInput("sm_new_building_name", "楼栋名称 *", placeholder = "例如：A栋"),
      textInput("sm_new_building_desc", "描述"),
      footer = tagList(modalButton("取消"),
        actionButton("sm_add_building_confirm", "添加", class = "btn-primary"))))
  })
  observeEvent(input$sm_add_building_confirm, {
    req(rv$logged_in, input$sm_new_building_name)
    result <- building_add(input$sm_new_building_name, input$sm_new_building_desc %||% "")
    if (result$success) { removeModal(); refresh() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 添加楼层
  ##################
  observeEvent(input$sm_add_floor, {
    req(rv$logged_in, input$sm_building)
    showModal(modalDialog(title = "添加楼层", size = "s", easyClose = TRUE,
      textInput("sm_new_floor_name", "楼层名称 *", placeholder = "例如：2F"),
      numericInput("sm_new_floor_number", "楼层号", value = NULL, min = 1),
      textInput("sm_new_floor_desc", "描述"),
      footer = tagList(modalButton("取消"),
        actionButton("sm_add_floor_confirm", "添加", class = "btn-primary"))))
  })
  observeEvent(input$sm_add_floor_confirm, {
    req(rv$logged_in, input$sm_building, input$sm_new_floor_name)
    result <- floor_add(input$sm_building, input$sm_new_floor_name,
      input$sm_new_floor_number, input$sm_new_floor_desc %||% "")
    if (result$success) { removeModal(); refresh() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 添加区域
  ##################
  observeEvent(input$sm_add_zone, {
    req(rv$logged_in, input$sm_floor)
    showModal(modalDialog(title = "添加区域", size = "s", easyClose = TRUE,
      textInput("sm_new_zone_name", "区域名称 *", placeholder = "例如：开放办公区"),
      selectizeInput("sm_new_zone_type", "类型", width = "100%",
        choices = c("前台" = "reception", "大厅卡座" = "open_desk", "会议室" = "meeting_room",
          "实验室" = "lab", "仓库" = "warehouse", "小办公室" = "small_office",
          "茶室" = "tea_room", "吸烟室" = "smoking_room")),
      fluidRow(
        column(6, numericInput("sm_new_zone_rs", "起始行", value = 1, min = 1)),
        column(6, numericInput("sm_new_zone_cs", "起始列", value = 1, min = 1))
      ),
      fluidRow(
        column(6, numericInput("sm_new_zone_rspan", "行数", value = 1, min = 1)),
        column(6, numericInput("sm_new_zone_cspan", "列数", value = 1, min = 1))
      ),
      textInput("sm_new_zone_desc", "描述"),
      footer = tagList(modalButton("取消"),
        actionButton("sm_add_zone_confirm", "添加", class = "btn-primary"))))
  })
  observeEvent(input$sm_add_zone_confirm, {
    req(rv$logged_in, input$sm_floor, input$sm_new_zone_name)
    result <- zone_add(input$sm_floor, input$sm_new_zone_name, input$sm_new_zone_type,
      input$sm_new_zone_rs, input$sm_new_zone_cs, input$sm_new_zone_rspan, input$sm_new_zone_cspan,
      input$sm_new_zone_desc %||% "")
    if (result$success) { removeModal(); refresh() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 添加工位
  ##################
  observeEvent(input$sm_add_seat, {
    req(rv$logged_in, input$sm_floor)
    snap <- sm_snapshot()
    zone_choices <- if (!is.null(snap) && nrow(snap$zones) > 0)
      c("\u2014 \u65E0\u533A\u57DF \u2014" = "", setNames(as.character(snap$zones$id), snap$zones$name)) else c("\u2014 \u65E0\u533A\u57DF \u2014" = "")
    showModal(modalDialog(title = "添加工位", size = "s", easyClose = TRUE,
      textInput("sm_new_seat_code", "工位编号 *", placeholder = "例如：28-01"),
      fluidRow(
        column(6, numericInput("sm_new_seat_row", "行号", value = 1, min = 1)),
        column(6, numericInput("sm_new_seat_col", "列号", value = 1, min = 1))
      ),
      selectizeInput("sm_new_seat_zone", "区域", choices = zone_choices, width = "100%"),
      selectizeInput("sm_new_seat_status", "状态", width = "100%",
        choices = c("有员工" = "occupied", "无员工无电脑" = "vacant_no_pc", "无员工有电脑" = "vacant_with_pc"),
        selected = "vacant_no_pc"),
      footer = tagList(modalButton("取消"),
        actionButton("sm_add_seat_confirm", "添加", class = "btn-success"))))
  })
  observeEvent(input$sm_add_seat_confirm, {
    req(rv$logged_in, input$sm_floor, input$sm_new_seat_code)
    zid <- input$sm_new_seat_zone; if (is.null(zid) || zid == "") zid <- NA
    result <- seat_add(input$sm_floor, zid, input$sm_new_seat_code,
      input$sm_new_seat_row, input$sm_new_seat_col, input$sm_new_seat_status)
    if (result$success) { removeModal(); refresh() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 批量生成工位
  ##################
  observeEvent(input$sm_batch_seats, {
    req(rv$logged_in, input$sm_floor)
    snap <- sm_snapshot()
    zone_choices <- if (!is.null(snap) && nrow(snap$zones) > 0)
      c("\u2014 \u65E0\u533A\u57DF \u2014" = "", setNames(as.character(snap$zones$id), snap$zones$name)) else c("\u2014 \u65E0\u533A\u57DF \u2014" = "")
    showModal(modalDialog(title = "批量生成工位", size = "s", easyClose = TRUE,
      textInput("sm_batch_prefix", "编号前缀 *", placeholder = "例如：28"),
      selectizeInput("sm_batch_zone", "区域", choices = zone_choices, width = "100%"),
      fluidRow(
        column(6, numericInput("sm_batch_start_row", "起始行", value = 1, min = 1)),
        column(6, numericInput("sm_batch_start_col", "起始列", value = 1, min = 1))
      ),
      fluidRow(
        column(6, numericInput("sm_batch_rows", "行数", value = 4, min = 1, max = 50)),
        column(6, numericInput("sm_batch_cols", "列数", value = 6, min = 1, max = 50))
      ),
      numericInput("sm_batch_start_num", "起始编号", value = 1, min = 1),
      tags$p(style = "font-size:11px;color:#888;",
        paste0("示例：前缀", "28", "，起始编号", "1", "，4行6列 → 28-01 ~ 28-24")),
      footer = tagList(modalButton("取消"),
        actionButton("sm_batch_seats_confirm", "生成", class = "btn-info"))))
  })
  observeEvent(input$sm_batch_seats_confirm, {
    req(rv$logged_in, input$sm_floor, input$sm_batch_prefix)
    zid <- input$sm_batch_zone; if (is.null(zid) || zid == "") zid <- NA
    result <- seat_batch_generate(input$sm_floor, zid, input$sm_batch_prefix,
      input$sm_batch_start_row, input$sm_batch_start_col,
      input$sm_batch_rows, input$sm_batch_cols, input$sm_batch_start_num)
    if (result$success) { removeModal(); refresh() }
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 删除楼栋/楼层/区域（根据选择上下文）
  ##################
  observeEvent(input$sm_del_building, {
    req(rv$logged_in)
    fid <- input$sm_floor; bid <- input$sm_building
    if (!is.null(fid) && fid != "") {
      # 删除当前楼层
      fl <- floor_get_by_id(fid)
      if (is.null(fl) || nrow(fl) == 0) return()
      showModal(modalDialog(title = "确认删除楼层",
        tags$div(style = "font-size:13px;",
          tags$p(tags$b(sprintf("确定删除楼层 [%s] 吗？", fl$name[1]))),
          tags$p(style = "color:#d9534f;", "该楼层下的所有区域和工位将一并删除。")),
        footer = tagList(modalButton("取消"),
          actionButton("sm_del_floor_confirm", "确认删除", class = "btn-danger")),
        size = "s", easyClose = TRUE))
    } else if (!is.null(bid) && bid != "") {
      bld <- building_get_by_id(bid)
      if (is.null(bld) || nrow(bld) == 0) return()
      showModal(modalDialog(title = "确认删除楼栋",
        tags$div(style = "font-size:13px;",
          tags$p(tags$b(sprintf("确定删除楼栋 [%s] 吗？", bld$name[1]))),
          tags$p(style = "color:#d9534f;", "该楼栋下的所有楼层、区域和工位将一并删除。")),
        footer = tagList(modalButton("取消"),
          actionButton("sm_del_building_confirm", "确认删除", class = "btn-danger")),
        size = "s", easyClose = TRUE))
    } else {
      showNotification("请先选择楼栋或楼层", type = "warning")
    }
  })
  observeEvent(input$sm_del_floor_confirm, {
    req(rv$logged_in, input$sm_floor)
    result <- floor_delete(as.integer(input$sm_floor))
    removeModal(); refresh()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })
  observeEvent(input$sm_del_building_confirm, {
    req(rv$logged_in, input$sm_building)
    result <- building_delete(as.integer(input$sm_building))
    removeModal(); refresh()
    showNotification(result$message, type = if(result$success) "message" else "error")
  })

  ##################
  # 刷新
  ##################
  observeEvent(input$sm_refresh, { refresh() })
}
