# 流程模块 V2 - 详情弹窗 UI
# 借鉴泛微OA标准流程页面布局
# 结构：
#   顶部标题栏
#   进度步骤条（横向）
#   左侧表单 + 右侧时间线
#   底部操作栏

# 步骤进度条（横线连接圆圈）
prv2_stepper_ui <- function(steps) {
  if (is.null(steps) || nrow(steps) == 0) return(NULL)

  # 给每个步骤生成图标
  status_icon <- function(status) {
    switch(status,
      "approved" = "✓",
      "active"   = "●",
      "rejected" = "✗",
      "skipped"  = "—",
      "pending"  = "○",
      "?")
  }

  status_label <- function(status) {
    switch(status,
      "approved" = "已通过",
      "active"   = "审批中",
      "rejected" = "已驳回",
      "skipped"  = "已跳过",
      "pending"  = "待审批",
      "?")
  }

  items <- list()
  for (i in seq_len(nrow(steps))) {
    s <- steps[i, ]
    # 解析审批人
    op_nms <- tryCatch(jsonlite::fromJSON(s$approver_names), error = function(e) c())
    op_str <- if (length(op_nms) > 0) paste(op_nms, collapse = " / ") else "—"
    # 时间
    op_time <- s$done_at %||% ""
    if (!is.na(s$status) && s$status == "active") op_time <- "审批中..."

    step_div <- tags$div(class = paste("prv2-step", s$status),
      tags$div(class = "prv2-step-icon", status_icon(s$status)),
      tags$div(class = "prv2-step-text",
        tags$div(class = "name", sprintf("第%d步 · %s", i, op_str)),
        tags$div(class = "meta", paste0(status_label(s$status),
          if (op_time != "" && !is.na(op_time)) paste0(" · ", op_time) else ""))
      )
    )
    items[[length(items) + 1]] <- step_div

    # 箭头（最后一个不画）
    if (i < nrow(steps)) {
      items[[length(items) + 1]] <-
        tags$div(class = "prv2-step-arrow", icon("chevron-right"))
    }
  }
  tags$div(class = "prv2-stepper", items)
}

# 时间线（审批记录）
prv2_timeline_ui <- function(records) {
  if (is.null(records) || nrow(records) == 0) {
    return(tags$div(style = "text-align:center; padding:20px; color:#bbb; font-size:12px;",
      "暂无审批记录"))
  }

  action_label <- function(a) {
    switch(a,
      "submit"   = "提交申请",
      "approve"  = "审批通过",
      "reject"   = "审批驳回",
      "withdraw" = "撤销申请",
      "urge"     = "催办",
      a)
  }

  action_color <- function(a) {
    switch(a,
      "submit"   = "submit",
      "approve"  = "approve",
      "reject"   = "reject",
      "withdraw" = "withdraw",
      "urge"     = "urge",
      "")
  }

  items <- list()
  # 时间线按时间正序
  for (i in seq_len(nrow(records))) {
    r <- records[i, ]
    items[[length(items) + 1]] <- tags$div(
      class = paste("prv2-timeline-item", action_color(r$action)),
      tags$div(class = "head",
        sprintf("%s · %s", r$operator_name %||% "系统", action_label(r$action))),
      tags$div(class = "meta", r$created_at %||% ""),
      if (!is.null(r$comment) && !is.na(r$comment) && nchar(r$comment) > 0)
        tags$div(class = "body", r$comment) else NULL
    )
  }
  tags$div(class = "prv2-timeline", items)
}

# 表单字段渲染
prv2_form_ui <- function(form_data) {
  if (!is.list(form_data) || length(form_data) == 0) {
    return(tags$div(style = "text-align:center; color:#bbb; padding:20px;",
      "（无表单数据）"))
  }
  items <- list()
  for (k in names(form_data)) {
    v <- form_data[[k]]
    is_empty <- is.null(v) || is.na(v) || v == ""
    items[[length(items) + 1]] <- tags$div(class = "prv2-field",
      tags$div(class = "lbl", k),
      tags$div(class = if (is_empty) "val empty" else "val",
        if (is_empty) "（未填写）" else as.character(v))
    )
  }
  tagList(items)
}

# 详情弹窗（完整布局）
prv2_detail_modal <- function(inst, tpl, steps, records, form_data, can_approve) {
  status_label <- switch(inst$status[1],
    "pending" = list(text = "审批中", cls = "pending"),
    "approved" = list(text = "已通过", cls = "approved"),
    "rejected" = list(text = "已驳回", cls = "rejected"),
    "withdrawn" = list(text = "已撤销", cls = "withdrawn"),
    list(text = inst$status[1], cls = ""))

  applicant <- inst$display_name %||% inst$username %||% "—"

  tagList(
    # 顶部标题
    div(class = "prv2-header",
      div(
        h2(inst$title[1] %||% "—"),
        div(class = "sub", sprintf("模板: %s · 编号: %s",
          inst$template_name %||% "—", inst$instance_no %||% "—"))
      ),
      span(class = paste("prv2-badge", status_label$cls), status_label$text)
    ),

    # 进度步骤条
    prv2_stepper_ui(steps),

    # 主体
    div(class = "prv2-body",
      # 左侧：基本信息 + 表单
      div(class = "prv2-form-panel",
        # 基本信息卡片
        div(class = "prv2-card",
          div(class = "prv2-card-title", icon("info-circle"), "基本信息"),
          div(class = "prv2-info-row",
            tags$span(class = "prv2-info-label", "申请人"),
            tags$span(class = "prv2-info-value", applicant)),
          div(class = "prv2-info-row",
            tags$span(class = "prv2-info-label", "提交时间"),
            tags$span(class = "prv2-info-value", inst$started_at %||% "—")),
          if (!is.null(inst$template_name) && !is.na(inst$template_name))
            div(class = "prv2-info-row",
              tags$span(class = "prv2-info-label", "模板分类"),
              tags$span(class = "prv2-info-value", inst$template_name[1])) else NULL
        ),
        # 表单内容卡片
        div(class = "prv2-card",
          div(class = "prv2-card-title", icon("file-alt"), "表单内容"),
          prv2_form_ui(form_data)
        )
      ),
      # 右侧：审批记录时间线
      div(class = "prv2-side-panel",
        div(class = "prv2-card-title", icon("history"), "审批记录"),
        prv2_timeline_ui(records)
      )
    ),

    # 底部操作栏
    div(class = "prv2-actions",
      # 待审批时显示操作按钮
      if (can_approve && inst$status[1] == "pending") {
        tagList(
          textAreaInput("prv2_comment", NULL, placeholder = "审批意见（选填）",
            rows = 1, width = "300px"),
          actionButton("prv2_do_approve", "通过", icon = icon("check"),
            class = "btn-success btn-sm"),
          actionButton("prv2_do_reject", "驳回", icon = icon("times"),
            class = "btn-dark btn-sm")
        )
      },
      # 我发起的可以撤销/催办
      if (inst$status[1] == "pending") {
        list(
          actionButton("prv2_do_urge", "催办", icon = icon("bell"),
            class = "btn-warning btn-sm"),
          actionButton("prv2_do_withdraw", "撤销", icon = icon("undo"),
            class = "btn-default btn-sm")
        )
      },
      div(style = "flex:1;"),
      actionButton("prv2_close", "关闭", class = "btn-default btn-sm")
    )
  )
}

# 模板列表卡片
prv2_tpl_card_ui <- function(tpl) {
  status_cls <- if (tpl$status[1] == "published") "published" else "draft"
  status_text <- if (tpl$status[1] == "published") "已发布" else "草稿"

  # 解析字段数量
  n_fields <- 0
  n_steps <- 0
  tryCatch({
    ff <- jsonlite::fromJSON(tpl$form_fields[1])
    if (length(ff) > 0) n_fields <- length(ff)
    ac <- jsonlite::fromJSON(tpl$approver_config[1])
    if (length(ac) > 0) n_steps <- length(ac)
  }, error = function(e) {})

  div(class = "prv2-card-tpl",
    div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
      div(
        div(class = "tpl-name", icon("file-alt"), " ", tpl$name[1]),
        div(class = "tpl-desc", tpl$description[1] %||% ""),
        div(class = "tpl-meta",
          tags$span(icon("th-list"), sprintf(" %d 个字段", n_fields)),
          tags$span(icon("sitemap"), sprintf(" %d 个审批步骤", n_steps)),
          tags$span(icon("clock"), " ", tpl$updated_at[1] %||% ""))
      ),
      div(style = "display:flex; flex-direction:column; gap:6px; align-items:flex-end;",
        span(class = paste("prv2-badge", status_cls), status_text),
        div(style = "display:flex; gap:4px;",
          if (tpl$status[1] == "published")
            actionButton(paste0("prv2_tpl_start_", tpl$id[1]), "发起",
              icon = icon("play"), class = "btn-success btn-xs") else
            actionButton(paste0("prv2_tpl_pub_", tpl$id[1]), "发布",
              icon = icon("check", lib = "glyphicon"), class = "btn-primary btn-xs"),
          actionButton(paste0("prv2_tpl_edit_", tpl$id[1]), "编辑",
            icon = icon("edit"), class = "btn-warning btn-xs"),
          actionButton(paste0("prv2_tpl_del_", tpl$id[1]), "删除",
            icon = icon("trash"), class = "btn-dark btn-xs")
        )
      )
    )
  )
}