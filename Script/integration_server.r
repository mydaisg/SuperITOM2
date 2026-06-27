# 集成模块 - 服务端

integration_server <- function(input, output, session, rv) {

  integ_refresh <- reactiveVal(0)
  integ_selected_id <- reactiveVal(NULL)

  output$integ_selected <- reactive({ !is.null(integ_selected_id()) })
  outputOptions(output, "integ_selected", suspendWhenHidden=FALSE)

  # 配置列表
  output$integ_config_list <- renderUI({
    integ_refresh()
    items <- integration_get_all()
    if (nrow(items) == 0) return(tags$p(style="color:#999;", "暂无配置，点下方新增"))
    tagList(lapply(1:nrow(items), function(i) {
      r <- items[i,]
      is_active <- !is.null(integ_selected_id()) && integ_selected_id() == r$id
      desc_short <- r$description[1] %||% ""
      if (nchar(desc_short) > 60) desc_short <- paste0(substr(desc_short,1,60),"...")
      tags$div(class=paste("int-card", if(is_active) "active" else ""),
        onclick=sprintf("Shiny.setInputValue('integ_select',%d,{priority:'event'})", r$id),
        tags$div(class="int-name", r$name),
        tags$div(class="int-desc", if(desc_short!="") desc_short else paste(r$method, substring(r$base_url,1,40))))
    }))
  })

  observeEvent(input$integ_select, {
    integ_selected_id(as.integer(input$integ_select))
    integ <- integration_get_by_id(as.integer(input$integ_select))
    output$integ_method <- renderText(integ$method[1])
    output$integ_url <- renderText(integ$base_url[1])
  })

  # 新增配置弹窗
  observeEvent(input$integ_add_btn, {
    req(rv$logged_in)
    showModal(modalDialog(title="新增集成配置", size="m",
      textInput("integ_new_name","名称*"),
      textInput("integ_new_url","URL*", placeholder="https://api.example.com/v1/..."),
      selectInput("integ_new_method","方法", choices=c("POST","GET")),
      textInput("integ_new_auth_h","认证头", placeholder="如: Authorization"),
      textInput("integ_new_auth_v","认证值", placeholder="如: Bearer xxx"),
      textAreaInput("integ_new_desc","描述", rows=2),
      footer=tagList(modalButton("取消"), actionButton("integ_new_save","保存",class="btn-primary")),
      easyClose=TRUE
    ))
  })

  observeEvent(input$integ_new_save, {
    req(rv$logged_in, input$integ_new_name, input$integ_new_url)
    result <- integration_add(input$integ_new_name, input$integ_new_url,
      input$integ_new_auth_h, input$integ_new_auth_v, input$integ_new_method, input$integ_new_desc)
    removeModal(); integ_refresh(integ_refresh()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 编辑
  observeEvent(input$integ_edit_btn, {
    req(rv$logged_in, integ_selected_id())
    integ <- integration_get_by_id(integ_selected_id())
    if (is.null(integ)) return()
    showModal(modalDialog(title="编辑集成配置", size="m",
      textInput("integ_edit_name","名称", value=integ$name[1]),
      textInput("integ_edit_url","URL", value=integ$base_url[1]),
      selectInput("integ_edit_method","方法", choices=c("POST","GET"), selected=integ$method[1]),
      textInput("integ_edit_auth_h","认证头", value=integ$auth_header[1] %||% ""),
      textInput("integ_edit_auth_v","认证值", value=integ$auth_value[1] %||% ""),
      textAreaInput("integ_edit_desc","描述", value=integ$description[1] %||% "", rows=2),
      footer=tagList(modalButton("取消"), actionButton("integ_edit_save","保存",class="btn-primary")),
      easyClose=TRUE
    ))
  })

  observeEvent(input$integ_edit_save, {
    req(rv$logged_in, integ_selected_id())
    result <- integration_update(integ_selected_id(),
      input$integ_edit_name, input$integ_edit_url, input$integ_edit_auth_h,
      input$integ_edit_auth_v, input$integ_edit_method, input$integ_edit_desc)
    removeModal(); integ_refresh(integ_refresh()+1)
    showNotification(result$message, type=if(result$success)"message" else "error")
  })

  # 删除（Modal 确认 + 显示详情）
  observeEvent(input$integ_del_btn, {
    req(rv$logged_in, integ_selected_id())
    intg <- integration_get_by_id(integ_selected_id())
    if (is.null(intg) || nrow(intg) == 0) return()
    showModal(modalDialog(
      title = "确认删除集成",
      tags$div(style = "font-size:13px;",
        tags$p(tags$b("即将删除以下集成配置，操作不可恢复：")),
        tags$table(class = "table table-bordered table-sm", style = "font-size:12px;",
          tags$thead(tags$tr(tags$th("属性"), tags$th("值"))),
          tags$tbody(
            tags$tr(tags$td("名称"), tags$td(tags$b(intg$name[1] %||% "—"))),
            tags$tr(tags$td("URL"), tags$td(intg$base_url[1] %||% "—")),
            tags$tr(tags$td("方法"), tags$td(intg$method[1] %||% "—")),
            tags$tr(tags$td("描述"), tags$td(intg$description[1] %||% "—"))
          )
        )
      ),
      footer = tagList(modalButton("取消"),
        actionButton("integ_del_confirm", "确认删除", class = "btn-danger")),
      size = "s", easyClose = TRUE
    ))
  })
  observeEvent(input$integ_del_confirm, {
    req(integ_selected_id())
    result <- integration_delete(integ_selected_id())
    removeModal()
    integ_selected_id(NULL)
    integ_refresh(integ_refresh()+1)
    showNotification(result$message, type="warning")
  })

  # 执行
  observeEvent(input$integ_exec_btn, {
    req(rv$logged_in, integ_selected_id(), input$integ_json_input)
    json <- input$integ_json_input
    # 验证 JSON 格式
    if (!jsonlite::validate(json)) {
      output$integ_resp_status <- renderText("JSON 格式错误")
      output$integ_resp_body <- renderText("请检查 JSON 语法")
      output$integ_resp_time <- renderText("—")
      return()
    }
    result <- integration_execute(integ_selected_id(), json)
    if (result$success) {
      output$integ_resp_status <- renderText({
        HTML(sprintf('<span class="int-status-ok">%d OK</span>', result$status_code))
      })
      output$integ_resp_time <- renderText(sprintf("%d ms", result$elapsed_ms))
      # 尝试格式化 JSON
      body <- result$body
      tryCatch({
        parsed <- jsonlite::fromJSON(body, simplifyVector=FALSE)
        body <- jsonlite::toJSON(parsed, pretty=TRUE, auto_unbox=TRUE)
      }, error=function(e) NULL)
      output$integ_resp_body <- renderText(body)
    } else {
      output$integ_resp_status <- renderText({
        HTML(sprintf('<span class="int-status-err">错误</span>'))
      })
      output$integ_resp_time <- renderText("—")
      output$integ_resp_body <- renderText(result$message)
    }
  })
}
