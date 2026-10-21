# 小程序用户 + VIP 会员管理 - 服务端
mini_program_server <- function(input, output, session, rv) {
  miniprogram_db_init()

  mp_trigger <- reactiveVal(0)
  vip_trigger <- reactiveVal(0)
  editing_uid <- reactiveVal(NULL)

  # 用户下拉选项（供 VIP 开通选择）
  mp_user_choices <- function() {
    users <- mp_user_get_all()
    if (nrow(users) == 0) return(c("(暂无用户)" = ""))
    labels <- ifelse(is.na(users$nickname) | users$nickname == "", users$openid, users$nickname)
    if ("phone" %in% names(users)) {
      labels <- ifelse(is.na(users$phone) | users$phone == "", labels, paste0(labels, " (", users$phone, ")"))
    }
    setNames(as.character(users$id), labels)
  }

  ##################
  # 统计卡片
  ##################
  output$mp_stat_total <- renderText({ mp_trigger(); mp_user_stats()$total })
  output$mp_stat_today <- renderText({ mp_trigger(); mp_user_stats()$today_new })
  output$mp_stat_vip <- renderText({ mp_trigger(); mp_user_stats()$vip })
  output$mp_stat_disabled <- renderText({ mp_trigger(); mp_user_stats()$disabled })

  output$vip_stat_total <- renderText({ vip_trigger(); vip_stats()$total })
  output$vip_stat_expiring <- renderText({ vip_trigger(); vip_stats()$expiring })
  output$vip_stat_expired <- renderText({ vip_trigger(); vip_stats()$expired })

  ##################
  # 用户列表
  ##################
  mp_user_data <- reactive({
    mp_trigger()
    req(rv$logged_in)
    mp_user_get_all(
      status = input$mp_filter_status %||% "all",
      keyword = input$mp_search %||% "",
      vip = input$mp_filter_vip %||% "all")
  })

  output$mp_user_table <- DT::renderDT({
    users <- mp_user_data()
    if (is.null(users) || nrow(users) == 0) {
      return(DT::datatable(data.frame(提示 = "暂无用户"), options = list(dom = "t")))
    }
    disp <- data.frame(
      ID = users$id,
      昵称 = ifelse(is.na(users$nickname) | users$nickname == "", "-", users$nickname),
      手机号 = ifelse(is.na(users$phone) | users$phone == "", "-", users$phone),
      性别 = ifelse(is.na(users$gender) | users$gender == "", "-", users$gender),
      地区 = paste0(
        ifelse(is.na(users$province) | users$province == "", "", users$province),
        ifelse(is.na(users$city) | users$city == "", "", users$city)),
      VIP = sapply(users$vip_level, vip_level_label),
      "VIP到期" = ifelse(is.na(users$vip_expire_at) | users$vip_expire_at == "", "-", users$vip_expire_at),
      状态 = ifelse(users$status == 1, "正常", "已禁用"),
      openid = users$openid,
      注册时间 = users$created_at,
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(disp, rownames = FALSE, selection = "single",
      options = list(pageLength = 25, dom = "ltip"))
  })

  observeEvent(input$mp_refresh, { mp_trigger(mp_trigger() + 1) })

  # 添加用户
  observeEvent(input$mp_user_add, {
    req(rv$logged_in)
    showModal(modalDialog(
      title = "添加小程序用户", size = "m", easyClose = TRUE,
      textInput("mp_e_openid", "openid *"),
      textInput("mp_e_nickname", "昵称"),
      textInput("mp_e_phone", "手机号"),
      textInput("mp_e_unionid", "unionid"),
      radioButtons("mp_e_gender", "性别", choices = c("未知" = "", "男" = "M", "女" = "F"), selected = "", inline = TRUE),
      textInput("mp_e_city", "城市"),
      textInput("mp_e_province", "省份"),
      textInput("mp_e_country", "国家", value = "中国"),
      selectInput("mp_e_status", "状态", choices = c("正常" = "1", "已禁用" = "0")),
      footer = tagList(modalButton("取消"), actionButton("mp_e_add_confirm", "添加", class = "btn-primary"))
    ))
  })
  observeEvent(input$mp_e_add_confirm, {
    req(rv$logged_in, input$mp_e_openid)
    result <- mp_user_add(input$mp_e_openid, nickname = input$mp_e_nickname, phone = input$mp_e_phone,
      unionid = input$mp_e_unionid, gender = input$mp_e_gender, city = input$mp_e_city,
      province = input$mp_e_province, country = input$mp_e_country, status = as.integer(input$mp_e_status))
    if (result$success) { removeModal(); mp_trigger(mp_trigger() + 1) }
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 编辑用户
  observeEvent(input$mp_user_edit, {
    req(rv$logged_in)
    sel <- input$mp_user_table_rows_selected
    if (length(sel) == 0) { showNotification("请先选择用户", type = "warning"); return() }
    users <- mp_user_data(); u <- users[sel, ]
    editing_uid(u$id[1])
    showModal(modalDialog(
      title = "编辑小程序用户", size = "m", easyClose = TRUE,
      textInput("mp_e_openid", "openid *", value = u$openid[1]),
      textInput("mp_e_nickname", "昵称", value = if (is.na(u$nickname[1])) "" else u$nickname[1]),
      textInput("mp_e_phone", "手机号", value = if (is.na(u$phone[1])) "" else u$phone[1]),
      textInput("mp_e_unionid", "unionid", value = if (is.na(u$unionid[1])) "" else u$unionid[1]),
      radioButtons("mp_e_gender", "性别", choices = c("未知" = "", "男" = "M", "女" = "F"),
        selected = if (!is.na(u$gender[1]) && u$gender[1] %in% c("M", "F")) u$gender[1] else "", inline = TRUE),
      textInput("mp_e_city", "城市", value = if (is.na(u$city[1])) "" else u$city[1]),
      textInput("mp_e_province", "省份", value = if (is.na(u$province[1])) "" else u$province[1]),
      textInput("mp_e_country", "国家", value = if (is.na(u$country[1])) "" else u$country[1]),
      selectInput("mp_e_status", "状态", choices = c("正常" = "1", "已禁用" = "0"), selected = as.character(u$status[1])),
      footer = tagList(modalButton("取消"), actionButton("mp_e_edit_confirm", "保存", class = "btn-primary"))
    ))
  })
  observeEvent(input$mp_e_edit_confirm, {
    req(rv$logged_in, editing_uid())
    result <- mp_user_update(editing_uid(), openid = input$mp_e_openid, nickname = input$mp_e_nickname,
      phone = input$mp_e_phone, unionid = input$mp_e_unionid, gender = input$mp_e_gender,
      city = input$mp_e_city, province = input$mp_e_province, country = input$mp_e_country,
      status = as.integer(input$mp_e_status))
    if (result$success) { removeModal(); editing_uid(NULL); mp_trigger(mp_trigger() + 1) }
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 删除用户
  observeEvent(input$mp_user_del, {
    req(rv$logged_in)
    sel <- input$mp_user_table_rows_selected
    if (length(sel) == 0) { showNotification("请先选择用户", type = "warning"); return() }
    users <- mp_user_data(); u <- users[sel, ]
    showModal(modalDialog(
      title = "确认删除用户",
      tags$p(tags$b(sprintf("确定删除用户 [%s] 吗？", if (is.na(u$nickname[1]) || u$nickname[1] == "") u$openid[1] else u$nickname[1]))),
      tags$p(style = "color:#d9534f;font-size:12px;", "将同时删除其所有 VIP 记录，此操作不可恢复。"),
      footer = tagList(modalButton("取消"), actionButton("mp_user_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$mp_user_del_confirm, {
    req(rv$logged_in)
    sel <- input$mp_user_table_rows_selected
    if (length(sel) == 0) return()
    users <- mp_user_data(); uid <- users$id[sel]
    result <- mp_user_delete(uid)
    removeModal(); mp_trigger(mp_trigger() + 1); vip_trigger(vip_trigger() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  # 禁用/启用
  observeEvent(input$mp_user_act, {
    req(rv$logged_in)
    sel <- input$mp_user_table_rows_selected
    if (length(sel) == 0) { showNotification("请先选择用户", type = "warning"); return() }
    users <- mp_user_data(); uid <- users$id[sel]
    result <- mp_user_toggle_status(uid)
    mp_trigger(mp_trigger() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  ##################
  # VIP 开通/续费弹窗
  ##################
  # 通用 VIP 表单弹窗：user_id 为 NULL 时显示用户下拉
  show_vip_modal <- function(user_id = NULL) {
    if (is.null(user_id)) {
      user_input <- selectizeInput("vip_e_user", "选择用户 *", choices = mp_user_choices(), width = "100%")
    } else {
      user_input <- tags$div(style = "display:none;", textInput("vip_e_user", NULL, value = as.character(user_id)))
    }
    showModal(modalDialog(
      title = "VIP 开通/续费", size = "m", easyClose = TRUE,
      user_input,
      selectInput("vip_e_level", "VIP 等级", choices = c("VIP1" = "1", "VIP2" = "2", "VIP3" = "3")),
      numericInput("vip_e_days", "时长（天）", value = 30, min = 1),
      numericInput("vip_e_amount", "金额（元）", value = 0, min = 0),
      textInput("vip_e_source", "来源", value = "admin"),
      textInput("vip_e_remark", "备注"),
      footer = tagList(modalButton("取消"), actionButton("vip_e_confirm", "确认开通", class = "btn-warning"))
    ))
  }

  # 用户页：对选中用户开通/续费
  observeEvent(input$mp_user_vip, {
    req(rv$logged_in)
    sel <- input$mp_user_table_rows_selected
    if (length(sel) == 0) { showNotification("请先选择用户", type = "warning"); return() }
    users <- mp_user_data(); uid <- users$id[sel]
    show_vip_modal(user_id = uid)
  })

  # VIP 页：开通（选用户）
  observeEvent(input$vip_add, {
    req(rv$logged_in)
    show_vip_modal(user_id = NULL)
  })

  # VIP 页：续费（选中的 VIP 记录）
  observeEvent(input$vip_renew, {
    req(rv$logged_in)
    sel <- input$vip_record_table_rows_selected
    if (length(sel) == 0) { showNotification("请先选择一条 VIP 记录", type = "warning"); return() }
    recs <- vip_record_data()
    r <- recs[sel, ]
    show_vip_modal(user_id = r$user_id[1])
  })

  # 确认开通/续费
  observeEvent(input$vip_e_confirm, {
    req(rv$logged_in, input$vip_e_user)
    uid <- suppressWarnings(as.integer(input$vip_e_user))
    if (is.na(uid)) { showNotification("请选择用户", type = "warning"); return() }
    result <- vip_record_add(uid, level = as.integer(input$vip_e_level),
      duration_days = as.integer(input$vip_e_days), amount = input$vip_e_amount,
      source = input$vip_e_source, remark = input$vip_e_remark)
    if (result$success) { removeModal(); mp_trigger(mp_trigger() + 1); vip_trigger(vip_trigger() + 1) }
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  ##################
  # VIP 记录列表
  ##################
  vip_record_data <- reactive({
    vip_trigger()
    req(rv$logged_in)
    vip_record_get_all(level = input$vip_filter_level %||% "all", keyword = input$vip_search %||% "")
  })

  output$vip_record_table <- DT::renderDT({
    recs <- vip_record_data()
    if (is.null(recs) || nrow(recs) == 0) {
      return(DT::datatable(data.frame(提示 = "暂无 VIP 记录"), options = list(dom = "t")))
    }
    today <- Sys.Date()
    user_label <- ifelse(is.na(recs$nickname) | recs$nickname == "", recs$openid, recs$nickname)
    status_label <- ifelse(recs$status == 0, "已作废",
      ifelse(!is.na(recs$expire_at) & as.Date(recs$expire_at) < today, "已过期", "有效"))
    disp <- data.frame(
      ID = recs$id,
      用户 = user_label,
      手机号 = ifelse(is.na(recs$phone) | recs$phone == "", "-", recs$phone),
      等级 = sapply(recs$level, vip_level_label),
      开始时间 = recs$start_at,
      到期时间 = recs$expire_at,
      时长天数 = recs$duration_days,
      金额 = recs$amount,
      来源 = ifelse(is.na(recs$source) | recs$source == "", "-", recs$source),
      状态 = status_label,
      备注 = ifelse(is.na(recs$remark) | recs$remark == "", "-", recs$remark),
      开通时间 = recs$created_at,
      stringsAsFactors = FALSE, check.names = FALSE)
    DT::datatable(disp, rownames = FALSE, selection = "single",
      options = list(pageLength = 25, dom = "ltip"))
  })

  observeEvent(input$vip_refresh, { vip_trigger(vip_trigger() + 1) })

  # 删除 VIP 记录
  observeEvent(input$vip_del, {
    req(rv$logged_in)
    sel <- input$vip_record_table_rows_selected
    if (length(sel) == 0) { showNotification("请先选择一条 VIP 记录", type = "warning"); return() }
    recs <- vip_record_data(); r <- recs[sel, ]
    showModal(modalDialog(
      title = "确认删除 VIP 记录",
      tags$p(tags$b(sprintf("确定删除记录 #%d 吗？", r$id[1]))),
      tags$p(style = "color:#d9534f;font-size:12px;", "删除后将重新计算该用户的 VIP 状态。"),
      footer = tagList(modalButton("取消"), actionButton("vip_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$vip_del_confirm, {
    req(rv$logged_in)
    sel <- input$vip_record_table_rows_selected
    if (length(sel) == 0) return()
    recs <- vip_record_data(); rid <- recs$id[sel]
    result <- vip_record_delete(rid)
    removeModal(); mp_trigger(mp_trigger() + 1); vip_trigger(vip_trigger() + 1)
    showNotification(result$message, type = if (result$success) "message" else "error")
  })

  outputOptions(output, "mp_user_table", suspendWhenHidden = FALSE)
  outputOptions(output, "vip_record_table", suspendWhenHidden = FALSE)
}
