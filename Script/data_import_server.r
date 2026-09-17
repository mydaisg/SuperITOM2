# 通用数据导入模块 - Server

data_import_server <- function(input, output, session, rv) {
  # 当前选中的数据集 ID（点击数据列表表格的行）
  selected_dataset_id <- reactiveVal(NULL)

  # 上传后读取 Sheet 列表
  sheets <- reactiveVal(character(0))
  # 当前预览数据
  preview_df <- reactiveVal(NULL)
  # 当前完整数据
  full_df <- reactiveVal(NULL)
  # 字段映射（源列名 → 目标字段名）
  field_map <- reactiveVal(NULL)
  # 源文件名
  src_name <- reactiveVal("")

  # 文件上传后：读取 Sheet 列表
  observeEvent(input$di_file, {
    req(rv$logged_in, input$di_file)
    f <- input$di_file
    src_name(f$name)
    s <- data_import_list_sheets(f$datapath)
    sheets(s)
    full_df(NULL)
    preview_df(NULL)
    field_map(NULL)
    if (length(s) == 0) {
      showNotification("无法读取 Excel 的 Sheet，请确认文件格式", type = "error")
    }
  })

  # Sheet 选择 UI
  output$di_sheet_ui <- renderUI({
    req(rv$logged_in)
    s <- sheets()
    if (length(s) == 0) return(NULL)
    tagList(
      fluidRow(
        column(6, selectInput("di_sheet", "Sheet", choices = s, selected = s[1])),
        column(6, div(style = "margin-top: 20px;",
          actionButton("di_load_preview", "加载预览", icon = icon("eye"), class = "btn-info btn-sm")))
      )
    )
  })

  # 加载预览 + 读取完整数据
  observeEvent(input$di_load_preview, {
    req(rv$logged_in, input$di_file, input$di_sheet)
    f <- input$di_file
    pv <- data_import_preview(f$datapath, input$di_sheet, n = 10)
    if (nrow(pv) == 0) {
      showNotification("该 Sheet 无数据或读取失败", type = "warning")
      return()
    }
    preview_df(pv)
    # 初始化字段映射：源列名 → 源列名
    field_map(setNames(as.list(names(pv)), names(pv)))
    # 读取完整数据
    full <- data_import_read_full(f$datapath, input$di_sheet)
    if (full$success) {
      full_df(full$data)
    } else {
      showNotification(full$message, type = "error")
    }
  })

  # 字段映射 UI（每个源列一行，可编辑目标字段名）
  output$di_field_map_ui <- renderUI({
    req(rv$logged_in)
    pv <- preview_df()
    if (is.null(pv) || nrow(pv) == 0) {
      return(p(style = "color:#999;", "上传文件并加载预览后，这里显示字段映射配置"))
    }
    cols <- names(pv)
    tagList(
      p(style = "color:#666; font-size:12px;",
        "预览前 10 行。可在右侧编辑目标字段名（用于导入后的数据引用）。"),
      div(class = "di-preview-table",
        tags$table(class = "table table-condensed table-bordered", style = "font-size:12px; margin-bottom:0;",
          tags$thead(
            tags$tr(lapply(cols, function(c) tags$th(c)))
          ),
          tags$tbody(
            lapply(seq_len(min(nrow(pv), 10)), function(i) {
              tags$tr(lapply(cols, function(c) {
                v <- pv[[c]][i]
                if (length(v) == 0 || is.na(v)) v <- ""
                tags$td(as.character(v))
              }))
            })
          )
        )
      ),
      br(),
      tags$b("字段映射（源列 → 目标字段名）", style = "font-size:12px;"),
      lapply(cols, function(c) {
        tags$div(class = "di-field-row",
          tags$span(class = "src", c),
          tags$span(class = "arrow", icon("arrow-right")),
          tags$input(type = "text", class = "form-control di-field-input",
            `data-src` = c, value = c,
            style = "font-size:12px; height:28px;")
        )
      }),
      # 隐藏输入用于同步字段映射到 server
      tags$div(style = "display:none;", textInput("di_field_map_json", NULL, value = ""))
    )
  })

  # 从 UI 收集字段映射（通过 JS 同步到隐藏输入）
  observeEvent(input$di_field_map_json, {
    req(rv$logged_in)
    v <- input$di_field_map_json
    if (is.null(v) || !nzchar(v)) return()
    tryCatch({
      fm <- jsonlite::fromJSON(v)
      field_map(fm)
    }, error = function(e) NULL)
  })

  # 数据集名称/追加选择 UI（根据导入方式切换）
  output$di_dataset_name_ui <- renderUI({
    req(rv$logged_in)
    mode <- input$di_import_mode %||% "create"
    if (mode == "append") {
      ds <- data_import_get_datasets()
      if (nrow(ds) == 0) {
        return(p(style = "color:#999; font-size:12px; margin-top:8px;", "暂无数据集可追加"))
      }
      choices <- setNames(as.character(ds$id),
        sprintf("%s · %s", ds$dataset_no, ds$name))
      selectInput("di_append_target", "目标数据集", choices = choices, width = "100%")
    } else {
      textInput("di_dataset_name", "数据集名称",
        placeholder = "例如：流程实例0917", width = "100%")
    }
  })

  # 导入按钮 + 灰化
  output$di_import_btn <- renderUI({
    req(rv$logged_in)
    tags$button(id = "di_import_go", type = "button",
      class = "btn btn-primary action-button", disabled = NA,
      icon("database"), " 导入到系统")
  })

  observe({
    req(rv$logged_in)
    mode <- input$di_import_mode %||% "create"
    has_data <- !is.null(full_df()) && nrow(full_df()) > 0
    if (mode == "append") {
      ok <- has_data && !is.null(input$di_append_target) && nzchar(input$di_append_target %||% "")
    } else {
      ok <- has_data && nzchar(trimws(input$di_dataset_name %||% ""))
    }
    session$sendCustomMessage("toggleBtn", list(id = "di_import_go", disabled = !isTRUE(ok)))
  })

  # 执行导入
  observeEvent(input$di_import_go, {
    req(rv$logged_in)
    df <- full_df()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("请先加载数据预览", type = "warning")
      return()
    }
    mode <- input$di_import_mode %||% "create"
    operator <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0)
      rv$current_user$username[1] else "系统"

    if (mode == "append") {
      # 追加模式：往现有数据集追加
      target <- input$di_append_target
      if (is.null(target) || !nzchar(target)) {
        showNotification("请选择目标数据集", type = "warning")
        return()
      }
      res <- data_import_append(as.integer(target), df)
      if (res$success) {
        showNotification(sprintf("已追加 %d 条记录（数据集现有 %d 条）", res$count, res$total), type = "message")
        full_df(NULL); preview_df(NULL); field_map(NULL)
        rv$di_refresh <- (rv$di_refresh %||% 0) + 1
      } else {
        showNotification(res$message, type = "error")
      }
      return()
    }

    # 新建模式
    name <- trimws(input$di_dataset_name)
    if (!nzchar(name)) {
      showNotification("请输入数据集名称", type = "warning")
      return()
    }
    fm <- field_map()
    res <- data_import_create(
      name = name,
      source = src_name(),
      sheet = input$di_sheet %||% "",
      df = df,
      field_map = fm,
      operator = operator
    )
    if (res$success) {
      showNotification(sprintf("数据集 %s 已导入（%d 条记录）", res$dataset_no, res$count), type = "message")
      full_df(NULL); preview_df(NULL); field_map(NULL)
      updateTextInput(session, "di_dataset_name", value = "")
      rv$di_refresh <- (rv$di_refresh %||% 0) + 1
    } else {
      showNotification(res$message, type = "error")
    }
  })

  # 数据集列表
  output$di_dataset_table <- renderDT({
    req(rv$logged_in)
    rv$di_refresh
    ds <- data_import_get_datasets()
    if (nrow(ds) == 0) {
      return(DT::datatable(data.frame(提示 = "暂无数据集"), options = list(dom = "t"),
        rownames = FALSE))
    }
    show <- data.frame(
      编号 = ds$dataset_no,
      名称 = ds$name,
      来源 = ds$source,
      Sheet = ds$sheet,
      记录数 = sapply(ds$id, function(id) {
        con <- db_connect()
        on.exit(db_disconnect(con))
        tryCatch(dbGetQuery(con, sprintf("SELECT COUNT(*) c FROM import_records WHERE dataset_id = %d", id))$c[1],
          error = function(e) 0)
      }),
      创建人 = ds$created_by,
      创建时间 = ds$created_at,
      stringsAsFactors = FALSE
    )
    # 引用代码列：可点击复制的代码片段
    show$引用代码 <- sprintf(
      paste0(
        '<span style="font-family:Consolas,monospace;font-size:11px;color:#337ab7;">',
        'data_import_get_records_by_name(&quot;%s&quot;)</span> ',
        '<button type="button" class="btn btn-xs btn-info" style="padding:1px 6px;font-size:11px;" ',
        'onclick="copyText(&#39;data_import_get_records_by_name(&quot;%s&quot;)&#39;)" ',
        'title="复制引用代码">复制</button>'
      ),
      ds$name, ds$name
    )
    DT::datatable(show, rownames = FALSE, selection = "single",
      escape = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE))
  })

  # 复制引用代码成功提示
  observeEvent(input$di_copy_done, {
    showNotification("引用代码已复制到剪贴板", type = "message", duration = 2)
  })

  # 点击列表行 → 选中数据集
  observeEvent(input$di_dataset_table_rows_selected, {
    req(rv$logged_in)
    sel <- input$di_dataset_table_rows_selected
    if (length(sel) == 0) { selected_dataset_id(NULL); return() }
    ds <- data_import_get_datasets()
    if (nrow(ds) >= sel) {
      selected_dataset_id(ds$id[sel])
    }
  })

  # 明细表头（数据集信息 + 删除按钮）
  output$di_record_header <- renderUI({
    req(rv$logged_in)
    id <- selected_dataset_id()
    if (is.null(id)) {
      return(p(style = "color:#999;", "点击上方数据集列表查看明细数据"))
    }
    ds <- data_import_get_dataset(id)
    if (is.null(ds)) return(p(style = "color:#999;", "数据集不存在"))
    tagList(
      fluidRow(
        column(8,
          tags$div(
            tags$b(ds$dataset_no[1]), " ", tags$b(ds$name[1]),
            tags$span(style = "color:#999; font-size:12px; margin-left:8px;",
              sprintf("来源: %s · Sheet: %s · 创建于 %s", ds$source[1], ds$sheet[1], ds$created_at[1]))
          )
        ),
        column(4, div(style = "text-align:right;",
          actionButton("di_delete_btn", "删除数据集", icon = icon("trash"),
            class = "btn-danger btn-sm")))
      )
    )
  })

  # 明细数据
  output$di_record_table <- renderDT({
    req(rv$logged_in)
    id <- selected_dataset_id()
    if (is.null(id)) {
      return(DT::datatable(data.frame(提示 = "请选择数据集"), options = list(dom = "t"), rownames = FALSE))
    }
    df <- data_import_get_records(id)
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(提示 = "无数据"), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  # 删除数据集
  observeEvent(input$di_delete_btn, {
    req(rv$logged_in)
    id <- selected_dataset_id()
    if (is.null(id)) return()
    showModal(modalDialog(
      title = "确认删除",
      "删除后该数据集及其所有记录将无法恢复，确定删除？",
      footer = tagList(
        modalButton("取消"),
        actionButton("di_delete_confirm", "确认删除", class = "btn-danger")
      ), easyClose = TRUE
    ))
  })
  observeEvent(input$di_delete_confirm, {
    req(rv$logged_in)
    id <- selected_dataset_id()
    res <- data_import_delete(id)
    if (res$success) {
      selected_dataset_id(NULL)
      rv$di_refresh <- (rv$di_refresh %||% 0) + 1
      showNotification("已删除", type = "message")
    } else {
      showNotification(res$message, type = "error")
    }
    removeModal()
  })
}
