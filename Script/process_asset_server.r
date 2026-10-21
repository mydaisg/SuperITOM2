# 流程资产中心 - 服务端

process_asset_server <- function(input, output, session, rv) {

  pfa_trigger <- reactiveVal(0)
  pfa_seeded  <- reactiveVal(FALSE)

  # 首次进入：表为空则自动导入种子数据
  observe({
    req(rv$logged_in)
    if (pfa_seeded()) return()
    pfa_seeded(TRUE)
    s <- flow_asset_stats()
    if (!is.null(s) && nrow(s) > 0 && s$total[1] == 0) {
      res <- flow_asset_seed()
      if (isTRUE(res$success)) {
        showNotification(res$message, type = "message", duration = 4)
        pfa_trigger(pfa_trigger() + 1)
      }
    }
  })

  # 更新分类下拉
  observe({
    req(rv$logged_in)
    cats <- flow_asset_categories()
    choices <- c("全部")
    if (nrow(cats) > 0) {
      labeled <- setNames(cats$category, cats$category)
      choices <- c("全部" = "全部", labeled)
    }
    updateSelectInput(session, "pfa_filter_cat", choices = choices)
  })

  # 统计卡
  output$pfa_stat_total <- renderUI({
    req(rv$logged_in)
    pfa_trigger()
    s <- flow_asset_stats()
    tags$div(class = "pfa-stat-card",
      tags$div(class = "icon-wrap", style = "background:#f9f0ff; color:#722ed1;", icon("project-diagram")),
      tags$div(
        tags$div(class = "num", s$total),
        tags$div(class = "lbl", "流程总数（19大类）")
      ))
  })

  output$pfa_stat_integrate <- renderUI({
    req(rv$logged_in)
    pfa_trigger()
    s <- flow_asset_stats()
    tags$div(class = "pfa-stat-card",
      tags$div(class = "icon-wrap", style = "background:#f6ffed; color:#389e0d;", icon("plug")),
      tags$div(
        tags$div(class = "num", style = "color:#389e0d;", s$integrate),
        tags$div(class = "lbl", sprintf("要集成（待定 %d）", s$pending))
      ))
  })

  output$pfa_stat_usage <- renderUI({
    req(rv$logged_in)
    pfa_trigger()
    s <- flow_asset_stats()
    tags$div(class = "pfa-stat-card",
      tags$div(class = "icon-wrap", style = "background:#e6f7ff; color:#08979c;", icon("bolt")),
      tags$div(
        tags$div(class = "num", style = "color:#08979c;", s$usage_total),
        tags$div(class = "lbl", "本月在途单量")
      ))
  })

  output$pfa_stat_issue <- renderUI({
    req(rv$logged_in)
    pfa_trigger()
    s <- flow_asset_stats()
    tags$div(class = "pfa-stat-card",
      tags$div(class = "icon-wrap", style = "background:#fff7e6; color:#d46b08;", icon("exclamation-triangle")),
      tags$div(
        tags$div(class = "num", style = "color:#d46b08;", s$issue_flows),
        tags$div(class = "lbl", "存在问题流程")
      ))
  })

  # 查询条件
  pfa_filters <- reactive({
    list(
      category = input$pfa_filter_cat %||% "全部",
      decision = input$pfa_filter_decision %||% "全部",
      keyword  = input$pfa_filter_keyword %||% ""
    )
  })

  # 流程列表
  output$pfa_list <- renderUI({
    req(rv$logged_in)
    pfa_trigger()
    f <- pfa_filters()
    df <- flow_asset_list(category = f$category, decision = f$decision, keyword = f$keyword)
    if (nrow(df) == 0) {
      return(tags$div(style = "text-align:center; padding:60px 0; color:#999;",
        icon("inbox", "fa-3x"), tags$p("暂无流程资产，点击右上角「导入」按钮加载泛微流程数据")))
    }
    # 分组渲染
    cats <- unique(df$category)
    tagList(lapply(cats, function(cat) {
      sub <- df[df$category == cat, ]
      tags$div(
        tags$div(class = "pfa-cat-head",
          tags$span(class = "cat-name", icon("folder-open"), " ", cat),
          tags$span(class = "cat-meta", sprintf("%d 个流程", nrow(sub))),
          tags$span(class = "cat-meta", sprintf("在途 %d", sum(sub$usage_count, na.rm = TRUE))),
          tags$span(class = "cat-meta",
            sprintf("要集成 %d", sum(sub$integration_decision == "要集成", na.rm = TRUE)))
        ),
        do.call(tagList, lapply(seq_len(nrow(sub)), function(i) pfa_card(sub[i, ])))
      )
    }))
  })

  # 单个流程卡片
  pfa_card <- function(r) {
    decision <- if (is.na(r$integration_decision) || r$integration_decision == "") "不需要" else r$integration_decision
    pri <- if (is.na(r$priority) || r$priority == "") "" else r$priority
    pri_cls <- if (pri == "高") "p-high" else if (pri == "中") "p-mid" else if (pri == "低") "p-low" else ""
    tags$div(class = "pfa-card", `data-id` = r$id,
      tags$div(style = "display:flex; align-items:center; flex-wrap:wrap; gap:8px;",
        tags$span(class = "fname", r$flow_name),
        if (!is.na(r$owner_dept) && r$owner_dept != "") tags$span(class = "fdept", r$owner_dept),
        tags$span(class = paste("pfa-badge", decision), decision),
        if (pri != "") tags$span(class = paste("pfa-badge", pri_cls), pri)
      ),
      tags$div(class = "fmeta",
        tags$span(class = "item", icon("bolt"), tags$span(class = "num", r$usage_count), "在途"),
        tags$span(class = "item", icon("users"), tags$span(class = "num", r$initiator_count), "发起人"),
        tags$span(class = "item", icon("sitemap"), tags$span(class = "num", r$node_count), "节点")
      ),
      if (!is.na(r$target_system) && r$target_system != "" && r$target_system != "—")
        tags$div(class = "fsys", icon("exchange-alt"), " ", r$target_system)
    )
  }

  # 查看详情
  observeEvent(input$pfa_view, {
    req(rv$logged_in, input$pfa_view$id)
    a <- flow_asset_get(as.integer(input$pfa_view$id))
    if (is.null(a)) return()
    showModal(modalDialog(
      title = div(
        span(a$flow_name, style = "font-weight:600;"),
        span(a$category, class = "pfa-badge", style = "margin-left:10px; background:#f9f0ff; color:#722ed1; border:1px solid #d3adf7;"),
        if (!is.na(a$owner_dept) && a$owner_dept != "") span(a$owner_dept, style = "color:#999; font-size:12px; margin-left:8px;")
      ),
      size = "l", easyClose = TRUE, footer = modalButton("关闭"),
      fluidRow(
        column(6, tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "在途单量"),
          tags$div(class = "prv2-info-value", a$usage_count))),
        column(6, tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "发起人数"),
          tags$div(class = "prv2-info-value", a$initiator_count)))
      ),
      fluidRow(
        column(6, tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "节点数"),
          tags$div(class = "prv2-info-value", a$node_count))),
        column(6, tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "集成判定"),
          tags$div(class = "prv2-info-value", a$integration_decision)))
      ),
      if (!is.na(a$target_system) && a$target_system != "" && a$target_system != "—")
        tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "对接系统"),
          tags$div(class = "prv2-info-value", a$target_system)),
      if (!is.na(a$top_initiators) && a$top_initiators != "")
        tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "高频发起人"),
          tags$div(class = "prv2-info-value", a$top_initiators)),
      if (!is.na(a$remark) && a$remark != "")
        tags$div(class = "prv2-info-row",
          tags$div(class = "prv2-info-label", "备注"),
          tags$div(class = "prv2-info-value", a$remark)),
      hr(),
      tags$h5(icon("sitemap"), " 节点构成", style = "color:#531dab;"),
      pfa_nodes_ui(a$nodes),
      tags$h5(icon("code-branch"), " 流转分支", style = "color:#531dab; margin-top:16px;"),
      pfa_branches_ui(a$branches),
      if (length(a$issues) > 0) tagList(
        tags$h5(icon("exclamation-triangle"), " 问题自查", style = "color:#d46b08; margin-top:16px;"),
        pfa_issues_ui(a$issues)
      )
    ))
  })

  pfa_nodes_ui <- function(nodes) {
    if (length(nodes) == 0) return(tags$p(style = "color:#999;", "（无节点明细）"))
    do.call(tagList, lapply(nodes, function(n) {
      ty <- n$type %||% "审批/办理"
      ty_cls <- if (ty == "发起") "发起" else if (ty == "结束") "结束" else "审批"
      tags$div(class = "pfa-node-row",
        tags$span(class = "pfa-node-seq", n$seq),
        tags$span(class = paste("pfa-node-type", ty_cls), ty),
        tags$span(class = "pfa-node-name", n$name),
        if (!is.null(n$role) && !is.na(n$role) && n$role != "") tags$span(class = "pfa-node-role", n$role),
        if (!is.null(n$cond) && !is.na(n$cond) && n$cond != "") tags$span(class = "pfa-node-cond", n$cond)
      )
    }))
  }

  pfa_branches_ui <- function(branches) {
    if (length(branches) == 0) return(tags$p(style = "color:#999;", "（无流转分支）"))
    do.call(tagList, lapply(branches, function(b) {
      tags$div(class = "pfa-branch-row",
        tags$span(b$path),
        if (!is.null(b$condition) && !is.na(b$condition) && b$condition != "")
          tags$span(class = "pfa-branch-cond", "　[", b$condition, "]")
      )
    }))
  }

  pfa_issues_ui <- function(issues) {
    do.call(tagList, lapply(issues, function(it) {
      tags$div(class = "pfa-issue-item",
        tags$div(tags$span(class = "pfa-issue-type", it$type %||% ""),
          if (!is.null(it$description) && !is.na(it$description) && it$description != "")
            tags$span(" · ", it$description)),
        if (!is.null(it$suggestion) && !is.na(it$suggestion) && it$suggestion != "")
          tags$div(style = "margin-top:4px; color:#666;", icon("lightbulb"), " ", it$suggestion)
      )
    }))
  }

  # 手动导入/刷新
  observeEvent(input$pfa_seed, {
    req(rv$logged_in)
    showNotification("正在导入泛微流程数据...", type = "message", duration = NULL, id = "pfa_importing")
    res <- flow_asset_seed(force = TRUE)
    removeNotification(id = "pfa_importing")
    if (isTRUE(res$success)) {
      showNotification(res$message, type = "message", duration = 4)
      pfa_trigger(pfa_trigger() + 1)
    } else {
      showNotification(res$message, type = "error", duration = 8)
    }
  })

  # 查询 / 重置
  observeEvent(input$pfa_search, { pfa_trigger(pfa_trigger() + 1) })
  observeEvent(input$pfa_reset, {
    updateSelectInput(session, "pfa_filter_cat", selected = "全部")
    updateSelectInput(session, "pfa_filter_decision", selected = "全部")
    updateTextInput(session, "pfa_filter_keyword", value = "")
    pfa_trigger(pfa_trigger() + 1)
  })
}
