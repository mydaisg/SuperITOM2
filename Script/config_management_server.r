##################
# 配置管理模块 - 服务端
# 资产模块「配置管理」子标签：多平台服务器/资源清单 + 账号密码 + 字段自定义
##################

config_management_server <- function(input, output, session, rv) {

  cfgitem_trigger <- reactiveVal(0)

  # 种子数据（登录后幂等写入）
  observe({
    req(rv$logged_in)
    config_item_seed()
  })

  # 当前平台筛选（-1 表示全部）
  current_platform <- reactiveVal(-1)

  ##################
  # 平台列表下拉 + 平台管理按钮
  ##################
  output$cfgitem_platform_ui <- renderUI({
    req(rv$logged_in)
    cfgitem_trigger()
    plats <- config_platform_get_all()
    choices <- c("全部平台" = "-1")
    if (nrow(plats) > 0) choices <- c(choices, setNames(as.character(plats$id), plats$platform_name))
    tagList(
      selectInput("cfgitem_platform_filter", NULL, choices = choices, selected = as.character(current_platform())),
      actionButton("cfgitem_platform_manage_btn", "平台管理", icon = icon("layer-group"), class = "btn-sm btn-default"),
      actionButton("cfgitem_fields_btn", "字段设置", icon = icon("columns"), class = "btn-sm btn-default")
    )
  })
  outputOptions(output, "cfgitem_platform_ui", suspendWhenHidden = FALSE)

  ##################
  # 统计栏
  ##################
  output$cfgitem_stats <- renderUI({
    cfgitem_trigger()
    req(rv$logged_in)
    items <- config_item_get_all()
    stats <- config_item_get_stats()
    tags$div(style = "display:flex; gap:10px; margin-bottom:8px;",
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#e3f2fd;",
        tags$div(class = "num", style = "color:#1565c0;", stats$total),
        tags$div(class = "lbl", style = "color:#1565c0;", "资源总数")),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#e8f5e9;",
        tags$div(class = "num", style = "color:#2e7d32;", stats$platform_n),
        tags$div(class = "lbl", style = "color:#2e7d32;", "平台数")),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#fff3e0;",
        tags$div(class = "num", style = "color:#e65100;",
          sum(items$category == "服务器", na.rm = TRUE)),
        tags$div(class = "lbl", style = "color:#e65100;", "服务器")),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#f3e5f5;",
        tags$div(class = "num", style = "color:#6a1b9a;",
          sum(items$category == "数据库", na.rm = TRUE)),
        tags$div(class = "lbl", style = "color:#6a1b9a;", "数据库")),
      tags$div(class = "well well-sm asset-stat-box", style = "flex:1; background:#e0f2f1;",
        tags$div(class = "num", style = "color:#00695c;",
          sum(items$category == "网络", na.rm = TRUE)),
        tags$div(class = "lbl", style = "color:#00695c;", "网络"))
    )
  })
  outputOptions(output, "cfgitem_stats", suspendWhenHidden = FALSE)

  ##################
  # 配置项表格（字段可配置 + 密码脱敏 + 复制按钮）
  ##################
  output$cfgitem_table <- renderDT({
    cfgitem_trigger()
    req(rv$logged_in)
    items <- config_item_get_all()
    if (nrow(items) == 0) {
      return(datatable(data.frame(提示 = "暂无配置项"), options = list(dom = "t")))
    }
    # 平台筛选
    pf <- as.character(current_platform())
    if (pf != "-1") {
      items <- items[!is.na(items$platform_id) & items$platform_id == as.integer(pf), ]
    }
    # 搜索过滤
    kw <- trimws(input$cfgitem_search %||% "")
    if (length(kw) > 0 && kw != "") {
      hay <- paste(items$resource_name, items$internal_ip, items$public_addr, items$remark, sep = " ")
      items <- items[grepl(kw, hay, ignore.case = TRUE), ]
    }
    cf <- input$cfgitem_cat_filter %||% "全部"
    if (length(cf) > 0 && cf != "全部") items <- items[items$category == cf, ]

    if (nrow(items) == 0) return(datatable(data.frame(提示 = "无匹配数据"), options = list(dom = "t")))

    # 读取可见字段配置
    visible_fields <- config_field_get_visible()

    # 构造每列数据（全部预计算）
    col_data <- list(
      seq_no        = items$seq_no %||% "",
      platform_name = items$platform_name %||% "",
      category      = items$category %||% "",
      env           = items$env %||% "",
      resource_name = items$resource_name %||% "",
      internal_ip   = items$internal_ip %||% "",
      os_spec       = items$os_spec %||% "",
      cpu           = items$cpu %||% "",
      ram           = items$ram %||% "",
      sys_disk      = items$sys_disk %||% "",
      data_disk     = items$data_disk %||% "",
      public_addr   = items$public_addr %||% "",
      public_port   = items$public_port %||% "",
      internal_port = items$internal_port %||% "",
      account       = sapply(seq_len(nrow(items)), function(i) {
        accs <- config_account_get_by_item(items$id[i])
        if (nrow(accs) == 0) return("")
        paste(sapply(seq_len(nrow(accs)), function(j) {
          u <- accs$username[j] %||% ""
          p <- accs$password[j] %||% ""
          masked <- config_password_mask(p)
          # 复制按钮携带明文（data-pwd），展示脱敏
          copy_btn <- if (nchar(p) > 0) {
            sprintf('<button class="btn btn-xs btn-link cfgitem-copy-btn" data-pwd="%s" style="padding:0 2px;" title="复制密码">%s<i class="fa fa-copy"></i></button>',
              gsub('"', '&quot;', p), masked)
          } else {
            masked
          }
          sprintf('<div style="white-space:nowrap;">%s / %s</div>', u, copy_btn)
        }), collapse = "")
      }),
      remark        = items$remark %||% ""
    )

    # 按可见字段顺序组装 display
    display <- data.frame(row.names = seq_len(nrow(items)))
    for (f in visible_fields) {
      display[[ CONFIG_ITEM_FIELDS[[f]] ]] <- col_data[[f]]
    }
    display[[ "操作" ]] <- sprintf(
      '<button class="btn btn-xs btn-info cfgitem-edit-btn" data-id="%d">编辑</button> <button class="btn btn-xs btn-danger cfgitem-del-btn" data-id="%d">删除</button>',
      items$id, items$id)

    # 操作列索引（最后一列）
    op_idx <- ncol(display) - 1

    datatable(display, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(pageLength = 50, autoWidth = TRUE, dom = "ltip", scrollX = TRUE,
        columnDefs = list(list(targets = op_idx, orderable = FALSE))),
      class = "cell-border stripe compact")
  })
  outputOptions(output, "cfgitem_table", suspendWhenHidden = FALSE)

  ##################
  # 类别筛选下拉
  ##################
  observe({
    req(rv$logged_in)
    cfgitem_trigger()
    items <- config_item_get_all()
    cats <- unique(c("全部", items$category[!is.na(items$category) & items$category != ""]))
    updateSelectInput(session, "cfgitem_cat_filter", choices = cats, selected = "全部")
  })

  ##################
  # 平台筛选变化
  ##################
  observeEvent(input$cfgitem_platform_filter, {
    current_platform(as.character(input$cfgitem_platform_filter %||% "-1"))
    cfgitem_trigger(cfgitem_trigger() + 1)
  })

  ##################
  # 刷新
  ##################
  observeEvent(input$cfgitem_refresh, {
    cfgitem_trigger(cfgitem_trigger() + 1)
  })

  ##################
  # 复制密码反馈
  ##################
  observeEvent(input$cfgitem_copy_feedback, {
    showNotification("密码已复制到剪贴板", type = "message", duration = 2)
  })

  ##################
  # 平台管理弹窗
  ##################
  observeEvent(input$cfgitem_platform_manage_btn, {
    req(rv$logged_in)
    plats <- config_platform_get_all()
    rows_html <- if (nrow(plats) == 0) {
      tags$p(style = "color:#999;", "暂无平台")
    } else {
      tagList(lapply(seq_len(nrow(plats)), function(i) {
        p <- plats[i, ]
        tags$div(style = "display:flex; align-items:center; gap:6px; padding:4px 0; border-bottom:1px dotted #eee;",
          tags$span(style = "flex:1;", p$platform_name),
          tags$button(class = "btn btn-xs btn-warning cfgplat-edit-btn", `data-id` = p$id, "编辑"),
          tags$button(class = "btn btn-xs btn-danger cfgplat-del-btn", `data-id` = p$id, "删除")
        )
      }))
    }
    showModal(modalDialog(
      title = "平台管理",
      size = "m", easyClose = TRUE,
      rows_html,
      hr(),
      fluidRow(
        column(8, textInput("cfgplat_new_name", NULL, placeholder = "新平台名称")),
        column(4, textInput("cfgplat_new_sort", NULL, placeholder = "排序", value = "0"))
      ),
      footer = tagList(
        actionButton("cfgplat_add_btn", "添加平台", class = "btn-primary"),
        modalButton("关闭")
      )
    ))
  })

  observeEvent(input$cfgplat_add_btn, {
    req(rv$logged_in, input$cfgplat_new_name)
    result <- config_platform_add(input$cfgplat_new_name, sort_order = as.integer(input$cfgplat_new_sort %||% 0))
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) {
      removeModal()
      # 重新打开平台管理弹窗
      session$sendCustomMessage("cfgplatReopen", list())
    }
  })

  # 平台编辑
  observeEvent(input$cfgplat_edit_click, {
    req(rv$logged_in)
    pid <- as.integer(input$cfgplat_edit_click)
    plats <- config_platform_get_all()
    p <- plats[plats$id == pid, ]
    if (nrow(p) == 0) return()
    showModal(modalDialog(
      title = "编辑平台",
      size = "s", easyClose = TRUE,
      textInput("cfgplat_edit_name", "平台名称", value = p$platform_name[1]),
      textInput("cfgplat_edit_desc", "描述", value = p$description[1] %||% ""),
      textInput("cfgplat_edit_sort", "排序", value = p$sort_order[1] %||% 0),
      footer = tagList(modalButton("取消"), actionButton("cfgplat_edit_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$cfgplat_edit_confirm, {
    req(rv$logged_in)
    pid <- as.integer(input$cfgplat_edit_click)
    result <- config_platform_update(pid,
      platform_name = input$cfgplat_edit_name, description = input$cfgplat_edit_desc,
      sort_order = as.integer(input$cfgplat_edit_sort %||% 0))
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) {
      removeModal()
      session$sendCustomMessage("cfgplatReopen", list())
    }
  })

  # 平台删除
  observeEvent(input$cfgplat_del_click, {
    req(rv$logged_in)
    pid <- as.integer(input$cfgplat_del_click)
    showModal(modalDialog(
      title = "确认删除平台",
      "删除平台后，该平台下的配置项将变为「未分类」。确定继续吗？",
      footer = tagList(modalButton("取消"), actionButton("cfgplat_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$cfgplat_del_confirm, {
    req(rv$logged_in)
    pid <- as.integer(input$cfgplat_del_click)
    result <- config_platform_delete(pid)
    removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = "warning")
    if (result$success) {
      session$sendCustomMessage("cfgplatReopen", list())
    }
  })

  ##################
  # 字段设置弹窗（可勾选 + 排序）
  ##################
  observeEvent(input$cfgitem_fields_btn, {
    req(rv$logged_in)
    current <- config_field_get_visible()
    all_fields <- names(CONFIG_ITEM_FIELDS)
    # 生成可排序的字段列表（隐藏序号 + 拖拽用 sortable）
    items_html <- tagList(lapply(seq_along(current), function(i) {
      f <- current[i]
      tags$div(class = "cfgitem-field-row", `data-field` = f,
        style = "display:flex; align-items:center; gap:8px; padding:5px 8px; background:#f5f7fa; border:1px solid #e0e4e8; border-radius:4px; margin-bottom:4px; cursor:move;",
        tags$i(class = "fa fa-bars", style = "color:#aaa;"),
        tags$span(style = "flex:1;", CONFIG_ITEM_FIELDS[[f]]),
        tags$span(style = "font-size:11px; color:#999;", f)
      )
    }))
    hidden_html <- tagList(lapply(all_fields[!all_fields %in% current], function(f) {
      tags$div(class = "cfgitem-field-avail", `data-field` = f,
        style = "display:flex; align-items:center; gap:8px; padding:4px 8px; background:#fff; border:1px dashed #ccc; border-radius:4px; margin-bottom:4px; cursor:pointer;",
        tags$i(class = "fa fa-plus", style = "color:#5cb85c;"),
        tags$span(style = "flex:1;", CONFIG_ITEM_FIELDS[[f]]),
        tags$span(style = "font-size:11px; color:#999;", f)
      )
    }))
    if (length(hidden_html) == 0) hidden_html <- tags$p(style = "color:#999;", "所有字段均已展示")
    showModal(modalDialog(
      title = "字段设置（点击下方字段添加，拖拽上方排序）",
      size = "m", easyClose = TRUE,
      tags$p(style = "font-size:12px; color:#666;", "展示字段："),
      tags$div(id = "cfgitem_field_list", style = "max-height:300px; overflow-y:auto;",
        items_html),
      tags$hr(),
      tags$p(style = "font-size:12px; color:#666;", "隐藏字段（点击添加）："),
      tags$div(id = "cfgitem_field_avail", style = "max-height:200px; overflow-y:auto;",
        hidden_html),
      tags$script(HTML("
        // 点击隐藏字段 -> 添加到展示列表末尾
        $(document).off('click.cfgfield').on('click.cfgfield', '.cfgitem-field-avail', function() {
          var f = $(this).data('field');
          var lbl = $(this).find('span').eq(0).text();
          var row = $('<div class=\"cfgitem-field-row\" data-field=\"' + f + '\" style=\"display:flex;align-items:center;gap:8px;padding:5px 8px;background:#f5f7fa;border:1px solid #e0e4e8;border-radius:4px;margin-bottom:4px;cursor:move;\"><i class=\"fa fa-bars\" style=\"color:#aaa;\"></i><span style=\"flex:1;\">' + lbl + '</span><span style=\"font-size:11px;color:#999;\">' + f + '</span></div>');
          $('#cfgitem_field_list').append(row);
          $(this).remove();
        });
        // 拖拽排序（用原生 drag 事件，避免额外依赖）
        $('#cfgitem_field_list').each(function() {
          var list = this;
          var dragEl = null;
          $(list).off('dragstart').on('dragstart', '.cfgitem-field-row', function(e) {
            dragEl = this; e.originalEvent.dataTransfer.effectAllowed = 'move';
          });
          $(list).off('dragover').on('dragover', '.cfgitem-field-row', function(e) {
            e.preventDefault();
            if (dragEl && dragEl !== this) {
              var rect = this.getBoundingClientRect();
              var after = (e.clientY - rect.top) > rect.height / 2;
              if (after) $(this).after(dragEl); else $(this).before(dragEl);
            }
          });
          $(list).off('dragend').on('dragend', function() { dragEl = null; });
        });
      ")),
      footer = tagList(
        actionButton("cfgitem_fields_reset", "恢复默认", class = "btn-default"),
        modalButton("取消"),
        actionButton("cfgitem_fields_save", "保存", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$cfgitem_fields_reset, {
    req(rv$logged_in)
    config_field_set_visible(names(CONFIG_ITEM_FIELDS))
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification("已恢复默认字段", type = "message")
    removeModal()
  })

  observeEvent(input$cfgitem_fields_save, {
    req(rv$logged_in)
    # 通过 JS 读取当前排序后的字段列表
    session$sendCustomMessage("cfgitemCollectFields", list())
  })

  # 接收 JS 收集的字段顺序并保存
  observeEvent(input$cfgitem_fields_order, {
    req(rv$logged_in)
    fields <- input$cfgitem_fields_order
    if (length(fields) == 0) return()
    result <- config_field_set_visible(fields)
    removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 添加配置项弹窗
  ##################
  observeEvent(input$cfgitem_add_btn, {
    req(rv$logged_in)
    plats <- config_platform_get_all()
    plat_choices <- c("未分类" = "")
    if (nrow(plats) > 0) plat_choices <- c(plat_choices, setNames(as.character(plats$id), plats$platform_name))
    showModal(modalDialog(
      title = "添加配置项",
      size = "l", easyClose = TRUE,
      fluidRow(
        column(3, numericInput("cfgitem_add_seq", "序号", value = NA, min = 1)),
        column(3, selectInput("cfgitem_add_plat", "平台", choices = plat_choices)),
        column(3, selectInput("cfgitem_add_cat", "资源类别", choices = c("服务器","数据库","网络","其他"), selected = "服务器")),
        column(3, textInput("cfgitem_add_env", "环境", value = "业务环境"))
      ),
      textInput("cfgitem_add_name", "资源名称 *"),
      fluidRow(
        column(6, textInput("cfgitem_add_ip", "内网 IP")),
        column(6, textInput("cfgitem_add_os", "操作系统 / 规格"))
      ),
      fluidRow(
        column(3, textInput("cfgitem_add_cpu", "CPU(核)")),
        column(3, textInput("cfgitem_add_ram", "内存(G)")),
        column(3, textInput("cfgitem_add_sys", "系统盘(G)")),
        column(3, textInput("cfgitem_add_data", "数据盘(G)"))
      ),
      fluidRow(
        column(4, textInput("cfgitem_add_pubaddr", "公网地址")),
        column(4, textInput("cfgitem_add_pubport", "公网端口")),
        column(4, textInput("cfgitem_add_inport", "内网端口"))
      ),
      textInput("cfgitem_add_remark", "备注"),
      footer = tagList(modalButton("取消"), actionButton("cfgitem_add_confirm", "确认添加", class = "btn-primary"))
    ))
  })

  observeEvent(input$cfgitem_add_confirm, {
    req(rv$logged_in, input$cfgitem_add_name)
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- config_item_add(
      seq_no = input$cfgitem_add_seq, platform_id = input$cfgitem_add_plat,
      category = input$cfgitem_add_cat, env = input$cfgitem_add_env,
      resource_name = input$cfgitem_add_name, internal_ip = input$cfgitem_add_ip,
      os_spec = input$cfgitem_add_os, cpu = input$cfgitem_add_cpu, ram = input$cfgitem_add_ram,
      sys_disk = input$cfgitem_add_sys, data_disk = input$cfgitem_add_data,
      public_addr = input$cfgitem_add_pubaddr, public_port = input$cfgitem_add_pubport,
      internal_port = input$cfgitem_add_inport, remark = input$cfgitem_add_remark,
      created_by = uid
    )
    if (result$success) {
      removeModal()
      rv$cfgitem_new_id <- result$id
    }
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 编辑弹窗（含账号密码管理，密码明文）
  ##################
  observeEvent(input$cfgitem_edit_click, {
    req(rv$logged_in)
    item <- config_item_get_by_id(as.integer(input$cfgitem_edit_click))
    if (is.null(item) || nrow(item) == 0) return()
    rv$cfgitem_edit_id <- item$id[1]
    plats <- config_platform_get_all()
    plat_choices <- c("未分类" = "")
    if (nrow(plats) > 0) plat_choices <- c(plat_choices, setNames(as.character(plats$id), plats$platform_name))

    accs <- config_account_get_by_item(item$id[1])
    acc_html <- if (nrow(accs) == 0) {
      tags$p(style = "color:#999;", "暂无账号")
    } else {
      tagList(lapply(seq_len(nrow(accs)), function(i) {
        a <- accs[i, ]
        tags$div(style = "display:flex; align-items:center; gap:6px; padding:4px 0; border-bottom:1px dotted #eee;",
          tags$span(style = "flex:1; font-family:Consolas,monospace;", a$username %||% ""),
          tags$span(style = "flex:2; font-family:Consolas,monospace;", a$password %||% ""),
          tags$button(class = "btn btn-xs btn-warning cfgacc-edit-btn", `data-id` = a$id, "编辑"),
          tags$button(class = "btn btn-xs btn-danger cfgacc-del-btn", `data-id` = a$id, "删除")
        )
      }))
    }

    showModal(modalDialog(
      title = paste("编辑配置项 -", item$resource_name[1] %||% ""),
      size = "l", easyClose = TRUE,
      fluidRow(
        column(3, numericInput("cfgitem_edit_seq", "序号", value = item$seq_no[1] %||% NA, min = 1)),
        column(3, selectInput("cfgitem_edit_plat", "平台", choices = plat_choices,
          selected = if (is.na(item$platform_id[1]) || item$platform_id[1] == "") "" else as.character(item$platform_id[1]))),
        column(3, selectInput("cfgitem_edit_cat", "资源类别", choices = c("服务器","数据库","网络","其他"), selected = item$category[1] %||% "服务器")),
        column(3, textInput("cfgitem_edit_env", "环境", value = item$env[1] %||% ""))
      ),
      textInput("cfgitem_edit_name", "资源名称 *", value = item$resource_name[1] %||% ""),
      fluidRow(
        column(6, textInput("cfgitem_edit_ip", "内网 IP", value = item$internal_ip[1] %||% "")),
        column(6, textInput("cfgitem_edit_os", "操作系统 / 规格", value = item$os_spec[1] %||% ""))
      ),
      fluidRow(
        column(3, textInput("cfgitem_edit_cpu", "CPU(核)", value = item$cpu[1] %||% "")),
        column(3, textInput("cfgitem_edit_ram", "内存(G)", value = item$ram[1] %||% "")),
        column(3, textInput("cfgitem_edit_sys", "系统盘(G)", value = item$sys_disk[1] %||% "")),
        column(3, textInput("cfgitem_edit_data", "数据盘(G)", value = item$data_disk[1] %||% ""))
      ),
      fluidRow(
        column(4, textInput("cfgitem_edit_pubaddr", "公网地址", value = item$public_addr[1] %||% "")),
        column(4, textInput("cfgitem_edit_pubport", "公网端口", value = item$public_port[1] %||% "")),
        column(4, textInput("cfgitem_edit_inport", "内网端口", value = item$internal_port[1] %||% ""))
      ),
      textInput("cfgitem_edit_remark", "备注", value = item$remark[1] %||% ""),
      tags$hr(),
      tags$h5(icon("key"), " 账号密码（编辑显示明文）"),
      tags$div(style = "max-height:200px; overflow-y:auto; border:1px solid #e0e4e8; border-radius:4px; padding:8px;",
        acc_html),
      tags$div(style = "margin-top:8px;",
        actionButton("cfgacc_add_btn", "添加账号", icon = icon("plus"), class = "btn-sm btn-success")),
      footer = tagList(modalButton("取消"), actionButton("cfgitem_edit_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$cfgitem_edit_confirm, {
    req(rv$logged_in, rv$cfgitem_edit_id, input$cfgitem_edit_name)
    result <- config_item_update(
      rv$cfgitem_edit_id,
      seq_no = input$cfgitem_edit_seq, platform_id = input$cfgitem_edit_plat,
      category = input$cfgitem_edit_cat, env = input$cfgitem_edit_env,
      resource_name = input$cfgitem_edit_name, internal_ip = input$cfgitem_edit_ip,
      os_spec = input$cfgitem_edit_os, cpu = input$cfgitem_edit_cpu, ram = input$cfgitem_edit_ram,
      sys_disk = input$cfgitem_edit_sys, data_disk = input$cfgitem_edit_data,
      public_addr = input$cfgitem_edit_pubaddr, public_port = input$cfgitem_edit_pubport,
      internal_port = input$cfgitem_edit_inport, remark = input$cfgitem_edit_remark
    )
    if (result$success) removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 账号增删改
  ##################
  observeEvent(input$cfgacc_add_btn, {
    req(rv$logged_in, rv$cfgitem_edit_id)
    showModal(modalDialog(
      title = "添加账号",
      size = "s", easyClose = TRUE,
      textInput("cfgacc_new_user", "用户名"),
      textInput("cfgacc_new_pwd", "密码（明文）"),
      textInput("cfgacc_new_remark", "备注"),
      footer = tagList(modalButton("取消"), actionButton("cfgacc_add_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$cfgacc_add_confirm, {
    req(rv$logged_in, rv$cfgitem_edit_id)
    result <- config_account_add(rv$cfgitem_edit_id,
      username = input$cfgacc_new_user, password = input$cfgacc_new_pwd,
      remark = input$cfgacc_new_remark)
    removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    # 重新打开编辑弹窗以刷新账号列表
    if (result$success) session$sendCustomMessage("cfgitemReopenEdit", list(id = rv$cfgitem_edit_id))
  })

  observeEvent(input$cfgacc_edit_click, {
    req(rv$logged_in)
    aid <- as.integer(input$cfgacc_edit_click)
    accs <- config_account_get_by_item(rv$cfgitem_edit_id)
    a <- accs[accs$id == aid, ]
    if (nrow(a) == 0) return()
    rv$cfgacc_edit_id <- aid
    showModal(modalDialog(
      title = "编辑账号",
      size = "s", easyClose = TRUE,
      textInput("cfgacc_edit_user", "用户名", value = a$username[1] %||% ""),
      textInput("cfgacc_edit_pwd", "密码（明文）", value = a$password[1] %||% ""),
      textInput("cfgacc_edit_remark", "备注", value = a$remark[1] %||% ""),
      footer = tagList(modalButton("取消"), actionButton("cfgacc_edit_confirm", "保存", class = "btn-primary"))
    ))
  })

  observeEvent(input$cfgacc_edit_confirm, {
    req(rv$logged_in, rv$cfgacc_edit_id)
    result <- config_account_update(rv$cfgacc_edit_id,
      username = input$cfgacc_edit_user, password = input$cfgacc_edit_pwd,
      remark = input$cfgacc_edit_remark)
    removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    if (result$success) session$sendCustomMessage("cfgitemReopenEdit", list(id = rv$cfgitem_edit_id))
  })

  observeEvent(input$cfgacc_del_click, {
    req(rv$logged_in)
    aid <- as.integer(input$cfgacc_del_click)
    showModal(modalDialog(
      title = "确认删除账号",
      "确定删除此账号吗？",
      footer = tagList(modalButton("取消"), actionButton("cfgacc_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$cfgacc_del_confirm, {
    req(rv$logged_in)
    aid <- as.integer(input$cfgacc_del_click)
    result <- config_account_delete(aid)
    removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = "warning")
    if (result$success) session$sendCustomMessage("cfgitemReopenEdit", list(id = rv$cfgitem_edit_id))
  })

  ##################
  # 删除配置项
  ##################
  observeEvent(input$cfgitem_del_click, {
    req(rv$logged_in)
    cid <- as.integer(input$cfgitem_del_click)
    item <- config_item_get_by_id(cid)
    if (is.null(item) || nrow(item) == 0) return()
    showModal(modalDialog(
      title = "确认删除配置项",
      tags$p(sprintf("确定要删除「%s」吗？关联的账号也会一并删除。", item$resource_name[1] %||% "")),
      footer = tagList(modalButton("取消"), actionButton("cfgitem_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$cfgitem_del_confirm, {
    req(rv$logged_in)
    cid <- as.integer(input$cfgitem_del_click)
    result <- config_item_delete(cid)
    removeModal()
    cfgitem_trigger(cfgitem_trigger() + 1)
    showNotification(result$message, type = "warning")
  })
}
