# 流程实例清单模块 — 服务端
# 数据来源：flow_monitor_records（最新批次），按 flow_catalog 分类分组渲染

flow_instance_list_server <- function(input, output, session, rv) {

  fil_trigger <- reactiveVal(0)

  # 当前批次 id（默认最新批次）
  fil_batch <- reactive({
    fil_trigger()
    flow_instance_latest_batch()
  })

  # 实例记录（含 flow_body 与分类归属）
  fil_records <- reactive({
    fil_trigger()
    bid <- fil_batch()
    req(bid)
    df <- flow_instance_get_records(bid)
    if (nrow(df) == 0) return(df)

    # 建立 flow_catalog 的 flow_name -> category 映射
    catalog <- flow_catalog_get_all()
    name2cat <- setNames(catalog$category, catalog$flow_name)
    name2catno <- setNames(catalog$category_no, catalog$flow_name)
    name2flowno <- setNames(catalog$flow_no, catalog$flow_name)

    matched <- df$flow_body %in% names(name2cat)
    df$category <- ifelse(matched, name2cat[df$flow_body], "其他（未在基础资料）")
    df$category_no <- ifelse(matched, name2catno[df$flow_body], 999)
    df$flow_no <- ifelse(matched, name2flowno[df$flow_body], NA_integer_)
    df
  })

  # 状态筛选后的数据
  fil_filtered <- reactive({
    df <- fil_records()
    req(nrow(df) > 0)
    st <- input$fil_status_filter
    if (!is.null(st) && st != "all") {
      df <- df[df$is_done == (st == "已完成"), ]
    }
    df
  })

  # 汇总信息
  output$fil_summary <- renderUI({
    req(rv$logged_in)
    df <- fil_records()
    if (nrow(df) == 0) return(tags$span("暂无实例数据"))
    tags$span(sprintf("共 %d 条实例 · 已完成 %d · 进行中 %d",
      nrow(df), sum(df$is_done), sum(!df$is_done)))
  })

  # 分类分组可折叠表格
  output$fil_table <- renderUI({
    req(rv$logged_in)
    df <- fil_filtered()
    if (nrow(df) == 0) {
      return(tags$div(class = "fil-empty", "暂无匹配的流程实例"))
    }

    # 按分类分组：先按 category_no 排序分类，分类内标准流程按 flow_no，其他按名称
    df$category_no[is.na(df$category_no)] <- 999
    df$flow_no[is.na(df$flow_no)] <- 0
    df <- df[order(df$category_no, df$flow_no, df$flow_body), ]

    cats <- split(seq_len(nrow(df)), df$category)

    rows_html <- character()
    for (cat in names(cats)) {
      idx <- cats[[cat]]
      rows_html <- c(rows_html, sprintf(
        '<tr class="fil-cat-row" data-cat="%s" data-collapsed="0"><td colspan="6"><span class="fil-cat-toggle">&#9660;</span> %s（%d项）</td></tr>',
        gsub('"', '', cat), cat, length(idx)))

      for (i in idx) {
        d <- df[i, ]
        badge <- if (d$is_done == 1)
          '<span class="fil-badge done">已完成</span>' else
          '<span class="fil-badge active">进行中</span>'
        node_color <- if (d$is_done == 1) "#00e676" else "#ffd700"
        rows_html <- c(rows_html, sprintf(
          '<tr class="fil-item-row" data-cat="%s"><td style="color:#8892b0;font-size:12px;">%d</td><td>%s</td><td>%s</td><td style="color:#8892b0;">%s</td><td style="color:%s;">%s</td><td>%s</td></tr>',
          gsub('"', '', cat),
          i,
          d$flow_name,
          d$initiator,
          d$start_time,
          node_color,
          d$current_node,
          badge))
      }
    }

    tags$table(class = "fil-table",
      tags$thead(tags$tr(
        tags$th(style = "width:50px;", "#"),
        tags$th("流程名称"),
        tags$th(style = "width:90px;", "发起人"),
        tags$th(style = "width:160px;", "发起时间"),
        tags$th(style = "width:150px;", "当前节点"),
        tags$th(style = "width:80px;", "状态")
      )),
      tags$tbody(HTML(paste(rows_html, collapse = "\n")))
    )
  })

  # 刷新
  observeEvent(input$fil_refresh, {
    req(rv$logged_in)
    fil_trigger(fil_trigger() + 1)
  })
}
