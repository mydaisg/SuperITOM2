# 流程模块服务端 v3 — 完整功能

process_server <- function(input, output, session, rv) {

  process_refresh_trigger <- reactiveVal(0)

  # 超时触发时自动刷新（rv$proc_timeout_counter 由超时检测递增）
  observe({
    rv$proc_timeout_counter
    process_refresh_trigger(isolate(process_refresh_trigger()) + 1)
  })

  # 选择要处理的待办节点详情（用于表单渲染）
  current_todo_details <- reactiveVal(NULL)

  # 存储构建的定义JSON（跨模态使用）
  built_json <- reactiveVal("")

  ##################
  # 统计卡片（与项目模块风格一致）
  ##################
  output$proc_stat_cards <- renderUI({
    process_refresh_trigger()
    stats <- process_get_stats()
    fluidRow(
      column(2, div(class="well well-sm",style="text-align:center;padding:12px 8px;",
        div(style="font-size:14px;color:#666;font-weight:500;","流程定义"),
        div(style="font-size:26px;font-weight:bold;color:#333;",stats$defs))),
      column(2, div(class="well well-sm",style="text-align:center;padding:12px 8px;",
        div(style="font-size:14px;color:#666;font-weight:500;","运行中"),
        div(style="font-size:26px;font-weight:bold;color:#e67e22;",stats$running))),
      column(2, div(class="well well-sm",style="text-align:center;padding:12px 8px;",
        div(style="font-size:14px;color:#666;font-weight:500;","已完成"),
        div(style="font-size:26px;font-weight:bold;color:#27ae60;",stats$completed))),
      column(2, div(class="well well-sm",style="text-align:center;padding:12px 8px;",
        div(style="font-size:14px;color:#666;font-weight:500;","已终止"),
        div(style="font-size:26px;font-weight:bold;color:#e74c3c;",stats$terminated))),
      column(2, div(class="well well-sm",style="text-align:center;padding:12px 8px;",
        div(style="font-size:14px;color:#666;font-weight:500;","待处理"),
        div(style="font-size:26px;font-weight:bold;color:#3498db;",
          if(!is.null(rv$current_user)&&nrow(rv$current_user)>0) nrow(process_get_todos(rv$current_user$id[1])) else 0))),
      column(2, div(class="well well-sm",style="text-align:center;padding:12px 8px;",
        div(style="font-size:14px;color:#666;font-weight:500;","总实例"),
        div(style="font-size:26px;font-weight:bold;color:#795548;",stats$total)))
    )
  })

  ##################
  # 统计概览
  ##################
  output$proc_stat_total <- renderText({ process_refresh_trigger(); nrow(process_def_list()) })
  output$proc_stat_running <- renderText({ process_refresh_trigger(); nrow(process_instance_list(status="running")) })
  output$proc_stat_completed <- renderText({ process_refresh_trigger(); nrow(process_instance_list(status="completed")) })
  output$proc_stat_todos <- renderText({
    process_refresh_trigger()
    if (!is.null(rv$current_user)&&nrow(rv$current_user)>0) nrow(process_get_todos(rv$current_user$id[1])) else 0
  })
  output$proc_stat_total2 <- renderText({ process_refresh_trigger(); nrow(process_instance_list()) })

  ##################
  # 我的待办
  ##################
  output$proc_todo_table <- DT::renderDT({
    process_refresh_trigger()
    if (is.null(rv$current_user)||nrow(rv$current_user)==0) return(DT::datatable(data.frame(信息="请先登录"),options=list(dom='t')))
    todos <- process_get_todos(rv$current_user$id[1])
    if (nrow(todos)==0) return(DT::datatable(data.frame(信息="暂无待办任务"),options=list(dom='t')))
    display <- data.frame(
      流程名称=todos$instance_title, 流程编号=todos$instance_no,
      节点=todos$node_name, 到达时间=todos$entered_at,
      操作=sprintf('<button class="btn btn-success btn-xs process-todo-btn" data-inst="%s" data-node="%s">处理</button>',
                    todos$instance_id, todos$node_instance_id),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=10,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 流程定义
  ##################
  output$proc_def_table <- DT::renderDT({
    process_refresh_trigger()
    defs <- process_def_list(input$proc_def_status_filter)
    if (nrow(defs)==0) return(DT::datatable(data.frame(信息="暂无流程定义"),options=list(dom='t')))
    display <- data.frame(
      编号=defs$def_no, 名称=defs$name, 分类=defs$category,
      版本=sprintf("v%d",defs$version), 状态=process_status_label(defs$status),
      创建时间=defs$created_at,
      操作=ifelse(defs$status=="draft",
        sprintf('<button class="btn btn-info btn-xs process-publish-btn" data-id="%d">发布</button>',defs$id),
        sprintf('<button class="btn btn-success btn-xs process-start-btn" data-id="%d">启动</button>',defs$id)),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=15,dom='rtip',scrollX=TRUE,columnDefs=list(list(targets=6,orderable=FALSE))),
      rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 流程实例
  ##################
  output$proc_instance_table <- DT::renderDT({
    process_refresh_trigger()
    insts <- process_instance_list(input$proc_inst_status_filter)
    if (nrow(insts)==0) return(DT::datatable(data.frame(信息="暂无流程实例"),options=list(dom='t')))
    current_nodes <- sapply(insts$id,function(id){
      nodes<-process_get_active_nodes(id); active<-nodes[nodes$status=="active",]
      if(nrow(active)>0) sprintf("%s(%s)",active$node_name[1],process_status_label(active$node_type[1])) else "-"})
    actions <- mapply(function(id, status) {
      log_btn <- sprintf('<button class="btn btn-default btn-xs proc-inst-log-btn" data-inst="%s">日志</button>', id)
      if (status == "running") {
        paste0(log_btn,
          sprintf(' <button class="btn btn-warning btn-xs proc-inst-suspend-btn" data-inst="%s">暂停</button>', id),
          sprintf(' <button class="btn btn-danger btn-xs proc-inst-term-btn" data-inst="%s">终止</button>', id))
      } else if (status == "suspended") {
        paste0(log_btn,
          sprintf(' <button class="btn btn-success btn-xs proc-inst-resume-btn" data-inst="%s">恢复</button>', id))
      } else {
        log_btn
      }
    }, insts$id, insts$status, SIMPLIFY=TRUE)
    display <- data.frame(
      实例编号=insts$instance_no, 流程=insts$def_name%||%insts$title,
      状态=process_status_label(insts$status), 当前节点=current_nodes,
      启动时间=insts$started_at, 操作=actions, stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=15,dom='rtip',scrollX=TRUE,columnDefs=list(list(targets=5,orderable=FALSE))),
      rownames=FALSE,class='cell-border stripe hover')
  })

  # 暂停/恢复按钮JS（已内联到UI模板中）

  ##################
  # 监控日志
  ##################
  observe({
    insts <- process_instance_list()
    choices <- stats::setNames(insts$id, sprintf("%s - %s", insts$instance_no, insts$title%||%""))
    updateSelectInput(session,"proc_log_inst_select",choices=c("请选择"="",choices))
  })

  output$proc_log_table <- DT::renderDT({
    req(input$proc_log_inst_select); process_refresh_trigger()
    logs <- process_get_logs(as.integer(input$proc_log_inst_select))
    if (nrow(logs)==0) return(DT::datatable(data.frame(信息="暂无日志"),options=list(dom='t')))
    display <- data.frame(时间=logs$created_at,级别=logs$log_level,类型=logs$log_type,
      节点=logs$node_id%||%"",消息=logs$message,stringsAsFactors=FALSE)
    DT::datatable(display,options=list(pageLength=20,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  output$proc_event_table <- DT::renderDT({
    req(input$proc_log_inst_select); process_refresh_trigger()
    events <- process_get_events(as.integer(input$proc_log_inst_select))
    if (nrow(events)==0) return(DT::datatable(data.frame(信息="暂无事件"),options=list(dom='t')))
    display <- data.frame(类型=events$event_type,状态=events$status,消息=events$message%||%"",来源=events$source,时间=events$created_at,stringsAsFactors=FALSE)
    DT::datatable(display,options=list(pageLength=10,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  output$proc_context_table <- DT::renderDT({
    req(input$proc_log_inst_select); process_refresh_trigger()
    ctx <- process_context_get_history(as.integer(input$proc_log_inst_select))
    if (nrow(ctx)==0) return(DT::datatable(data.frame(信息="无上下文变更历史"),options=list(dom='t')))
    display <- data.frame(版本=ctx$version,变更人=ctx$changed_by,原因=ctx$change_reason,时间=ctx$created_at,stringsAsFactors=FALSE)
    DT::datatable(display,options=list(pageLength=10,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 操作处理
  ##################
  observeEvent(input$proc_refresh_defs, { process_refresh_trigger(process_refresh_trigger()+1) })
  observeEvent(input$proc_refresh_insts, { process_refresh_trigger(process_refresh_trigger()+1) })
  observeEvent(input$proc_refresh_logs, { process_refresh_trigger(process_refresh_trigger()+1) })

  # ===== 一键体验（三种类型） =====
  observeEvent(input$proc_demo_simple, { launch_demo("simple") })
  observeEvent(input$proc_demo_condition, { launch_demo("condition") })
  observeEvent(input$proc_demo_auto, { launch_demo("auto") })

  launch_demo <- function(demo_type) {
    user_id <- if(!is.null(rv$current_user)&&nrow(rv$current_user)>0) rv$current_user$id[1] else NULL
    result <- process_create_and_start(demo_type=demo_type, started_by=user_id)
    process_refresh_trigger(process_refresh_trigger()+1)
    if (result$success) {
      labels <- c("simple"="简单审批","condition"="条件分支审批","auto"="自动工单流程")
      next_node <- ""
      if (!is.null(result$advance$next_node_label)) next_node <- result$advance$next_node_label
      showModal(modalDialog(
        title=sprintf("已创建：%s", labels[[demo_type]]),
        p(sprintf("定义: %s | 实例: %s", result$def_no, result$instance_no)),
        hr(),
        if (next_node!="") p(sprintf("当前已推进到「%s」节点", next_node)),
        p("请在「我的待办」中处理"),
        footer=modalButton("知道了"), easyClose=TRUE))
    } else {
      showNotification(result$message, type="error")
    }
  }

  # ===== 定义构建辅助（高级设置底部使用） =====
  observeEvent(input$proc_build_json, {
    nodes <- list()
    for (i in 1:6) {
      type <- input[[paste0("proc_bn_type",i)]]
      if (is.null(type)||type=="") next
      label <- input[[paste0("proc_bn_label",i)]]%||%type
      node <- list(id=paste0("node",i), type=type, label=label)
      timeout <- input[[paste0("proc_bn_timeout",i)]]
      if (!is.null(timeout)&&timeout>0) node$timeout_minutes <- as.integer(timeout)
      nodes[[length(nodes)+1]] <- node
    }
    if (length(nodes)<2) {
      showNotification("至少需要2个节点（开始和结束）",type="warning"); return()
    }
    json <- process_build_definition(nodes)
    built_json(json)
    showModal(modalDialog(
      title="生成的流程定义 JSON",
      textAreaInput("proc_built_json_show","",value=json,rows=15),
      footer=tagList(
        actionButton("proc_use_built_json","使用此 JSON 创建定义（自动打开新建弹窗）",class="btn-success",icon=icon("check")),
        modalButton("关闭")
      ), size="l", easyClose=TRUE
    ))
  })

  observeEvent(input$proc_use_built_json, {
    built_json(input$proc_built_json_show)
    removeModal()
    # 自动打开新建弹窗，选中"自定义"模板并填入JSON
    showModal(build_create_modal(template="custom"))
  })

  # ===== 新建流程（模板化，普通用户友好） =====

  # 模板定义：用 R list + toJSON，彻底避免引号问题
  .tpl_simple <- function() {
    jsonlite::toJSON(list(
      nodes=list(
        list(id="start", type="start", label="开始"),
        list(id="approve", type="task", label="审批确认", timeout_minutes=1440L),
        list(id="end", type="end", label="结束")
      ),
      transitions=list(
        list(from="start", to="approve", condition=""),
        list(from="approve", to="end", condition="")
      )
    ), auto_unbox=TRUE, pretty=TRUE)
  }
  .tpl_condition <- function() {
    jsonlite::toJSON(list(
      nodes=list(
        list(id="start", type="start", label="开始"),
        list(id="approve", type="task", label="审批确认", timeout_minutes=1440L,
          form=list(fields=list(
            list(key="result", label="审批意见", type="select", options=list("同意","驳回"), required=TRUE),
            list(key="remark", label="备注", type="textarea")
          ))),
        list(id="condition", type="condition", label="审批判断"),
        list(id="notify", type="auto", label="发送通知"),
        list(id="reject_end", type="end", label="已驳回"),
        list(id="end", type="end", label="结束")
      ),
      transitions=list(
        list(from="start", to="approve", condition=""),
        list(from="approve", to="condition", condition=""),
        list(from="condition", to="notify", condition="result=='同意'"),
        list(from="condition", to="reject_end", condition="result=='驳回'"),
        list(from="notify", to="end", condition="")
      )
    ), auto_unbox=TRUE, pretty=TRUE)
  }
  .tpl_auto <- function() {
    jsonlite::toJSON(list(
      nodes=list(
        list(id="start", type="start", label="开始"),
        list(id="auto_create", type="auto", label="自动创建工单"),
        list(id="approve", type="task", label="审批工单", timeout_minutes=1440L),
        list(id="end", type="end", label="结束")
      ),
      transitions=list(
        list(from="start", to="auto_create", condition=""),
        list(from="auto_create", to="approve", condition=""),
        list(from="approve", to="end", condition="")
      )
    ), auto_unbox=TRUE, pretty=TRUE)
  }

  build_create_modal <- function(template="simple") {
    tpl <- list(
      simple=list(name="简单审批", desc="创建后→审批确认→自动完成", category="approval", json=.tpl_simple()),
      condition=list(name="条件审批", desc="审批→同意(通过)/驳回(结束)", category="approval", json=.tpl_condition()),
      auto=list(name="自动工单", desc="启动后自动创建工单→审批→完成", category="work_order", json=.tpl_auto()),
      custom=list(name="自定义", desc="高级用户自行编辑JSON", category="general",
        json=if(is.null(built_json())||nchar(built_json())<20).tpl_simple()else built_json())
    )
    sel <- tpl[[template]]%||%tpl$simple

    modalDialog(title="新建流程",
      textInput("proc_new_def_name","流程名称",value=sel$name),
      textInput("proc_new_def_desc","描述",value=sel$desc),
      fluidRow(
        column(6, selectInput("proc_new_def_category","分类",
          choices=c("通用"="general","故障"="fault","变更"="change","审批"="approval","工单"="work_order"),
          selected=sel$category)),
        column(6, selectInput("proc_template_select","流程模板",
          choices=c("简单审批"="simple","条件审批"="condition","自动工单"="auto","自定义(JSON)"="custom"),
          selected=template))
      ),
      fluidRow(id="proc_json_editor_row", column(12,
        textAreaInput("proc_new_def_json","流程定义 (JSON)",rows=8,value=sel$json)
      )),
      tags$script(HTML("
        if($('#proc_template_select').val()=='custom'){ $('#proc_json_editor_row').show(); }
        else { $('#proc_json_editor_row').hide(); }
      ")),
      size="l",
      footer=tagList(
        modalButton("取消"),
        actionButton("proc_save_new_def","创建并发布",class="btn-primary",icon=icon("rocket"))
      ), easyClose=TRUE
    )
  }

  observeEvent(input$proc_create_def, {
    showModal(build_create_modal(template="simple"))
  })

  # 模板切换时更新表单
  observeEvent(input$proc_template_select, {
    template <- input$proc_template_select
    if (template=="custom") return()  # 自定义模式保留当前值
    vals <- switch(template,
      simple = list(name="简单审批", desc="创建后→审批确认→自动完成", category="approval", json=.tpl_simple()),
      condition = list(name="条件审批", desc="审批→同意(通过)/驳回(结束)", category="approval", json=.tpl_condition()),
      auto = list(name="自动工单", desc="启动后自动创建工单→审批→完成", category="work_order", json=.tpl_auto())
    )
    if (!is.null(vals)) {
      shiny::updateTextInput(session, "proc_new_def_name", value=vals$name)
      shiny::updateTextInput(session, "proc_new_def_desc", value=vals$desc)
      shiny::updateSelectInput(session, "proc_new_def_category", selected=vals$category)
      shiny::updateTextAreaInput(session, "proc_new_def_json", value=vals$json)
    }
  })

  observeEvent(input$proc_save_new_def, {
    req(input$proc_new_def_name)
    result <- process_def_create(name=input$proc_new_def_name,description=input$proc_new_def_desc%||%"",
      category=input$proc_new_def_category,definition=input$proc_new_def_json,
      created_by=if(!is.null(rv$current_user))rv$current_user$id[1]else NULL)
    removeModal()
    if (result$success) {
      # 自动发布
      publish <- process_def_publish(result$id, change_log="从模板创建后自动发布")
      process_refresh_trigger(process_refresh_trigger()+1)
      if (publish$success) {
        showNotification(sprintf("%s 已创建并发布，可以启动了",result$message),type="message",duration=8)
      } else {
        showNotification(sprintf("%s（发布失败:%s）",result$message,publish$message),type="warning",duration=8)
      }
    } else showNotification(result$message,type="error")
  })

  # ===== 发布 =====
  observeEvent(input$process_publish_click, {
    result <- process_def_publish(input$process_publish_click,change_log="手动发布")
    if (result$success) { process_refresh_trigger(process_refresh_trigger()+1); showNotification(result$message,type="message",duration=5)
    } else showNotification(result$message,type="error")
  })

  # ===== 启动 =====
  observeEvent(input$process_start_click, {
    def_id <- as.integer(input$process_start_click)
    def <- process_def_get(def_id)
    if (is.null(def)) { showNotification("流程定义不存在",type="error"); return() }
    showModal(modalDialog(title=sprintf("启动流程: %s",def$name[1]),
      textInput("proc_start_title","实例标题",value=def$name[1]),
      textAreaInput("proc_start_context","上下文 (JSON)",rows=4,value='{"title":"示例","priority":"normal"}'),
      footer=tagList(modalButton("取消"),actionButton("proc_confirm_start","启动",class="btn-success"))))
  })

  observeEvent(input$proc_confirm_start, {
    req(input$process_start_click)
    context <- tryCatch(jsonlite::fromJSON(input$proc_start_context,simplifyVector=FALSE),error=function(e)list(title=input$proc_start_title,priority="normal"))
    result <- process_instance_start(def_id=as.integer(input$process_start_click),title=input$proc_start_title,context_data=context,
      started_by=if(!is.null(rv$current_user))rv$current_user$id[1]else NULL)
    removeModal()
    if (result$success) {
      process_refresh_trigger(process_refresh_trigger()+1)
      advance <- process_advance(result$id)
      process_refresh_trigger(process_refresh_trigger()+1)
      if (advance$success) {
        if (isTRUE(advance$completed)) showNotification(sprintf("%s → 已完成",result$message),type="success",duration=5)
        else { showNotification(sprintf("%s，当前在「%s」",result$message,advance$next_node_label),type="message",duration=8)
          if (advance$next_node_type=="task") showNotification("请在「我的待办」中处理",type="warning",duration=8) }
      } else showNotification(sprintf("%s（推进:%s）",result$message,advance$message),type="warning",duration=8)
    } else showNotification(result$message,type="error")
  })

  # ===== 版本历史 =====
  output$proc_version_table <- DT::renderDT({
    req(input$proc_log_inst_select); process_refresh_trigger()
    inst <- process_instance_get(as.integer(input$proc_log_inst_select))
    if (is.null(inst)) return(DT::datatable(data.frame(信息="请先选择实例"),options=list(dom='t')))
    versions <- process_def_get_versions(inst$def_id[1])
    if (nrow(versions)==0) return(DT::datatable(data.frame(信息="无版本历史"),options=list(dom='t')))
    display <- data.frame(版本=sprintf("v%d",versions$version),变更说明=versions$change_log%||%"",发布时间=versions$published_at,stringsAsFactors=FALSE)
    DT::datatable(display,options=list(pageLength=10,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  # ===== 暂停/恢复/终止 =====
  observeEvent(input$proc_inst_term_click, {
    result <- process_instance_terminate(as.integer(input$proc_inst_term_click))
    process_refresh_trigger(process_refresh_trigger()+1)
    showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$proc_inst_suspend_click, {
    result <- process_instance_suspend(as.integer(input$proc_inst_suspend_click))
    process_refresh_trigger(process_refresh_trigger()+1)
    showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$proc_inst_resume_click, {
    result <- process_instance_resume(as.integer(input$proc_inst_resume_click))
    process_refresh_trigger(process_refresh_trigger()+1)
    showNotification(result$message, type=ifelse(result$success,"message","error"))
  })

  # ===== 日志 =====
  observeEvent(input$process_inst_log_click, {
    inst_id <- as.integer(input$process_inst_log_click)
    instance <- process_instance_get(inst_id)
    if (is.null(instance)) { showNotification("实例不存在",type="error"); return() }
    logs <- process_get_logs(inst_id); events <- process_get_events(inst_id)
    nodes <- process_get_active_nodes(inst_id)
    log_text <- paste(
      sprintf("=== %s ===\n状态:%s | 当前节点:%s\n",instance$instance_no[1],instance$status[1],instance$current_node[1]%||%"-"),
      "--- 节点 ---\n",paste(capture.output(if(nrow(nodes)>0)print(nodes[,c("node_id","node_name","node_type","status","entered_at","completed_at")])else "无"),collapse="\n"),
      "\n--- 事件 ---\n",paste(capture.output(if(nrow(events)>0)print(events[,c("event_type","status","message","created_at")])else "无"),collapse="\n"),
      "\n--- 日志 ---\n",paste(capture.output(if(nrow(logs)>0)print(logs[,c("log_level","log_type","message","created_at")])else "无"),collapse="\n"),sep="")
    showModal(modalDialog(title=sprintf("日志: %s",instance$instance_no[1]),size="l",
      pre(log_text,style="font-size:12px;max-height:500px;overflow:auto;background:#f5f5f5;padding:10px;"),footer=modalButton("关闭"),easyClose=TRUE))
  })

  # ===== 待办处理（带表单） =====
  observeEvent(input$process_todo_click, {
    click_data <- input$process_todo_click
    instance_id <- as.integer(click_data$instance_id)
    node_inst_id <- as.integer(click_data$node_id)
    instance <- process_instance_get(instance_id)
    if (is.null(instance)) { showNotification("实例不存在",type="error"); return() }
    definition <- tryCatch(jsonlite::fromJSON(instance$definition[1],simplifyVector=FALSE),error=function(e)NULL)
    current_node_id <- instance$current_node[1]
    # 查找节点定义中的表单
    node_def <- NULL; form_fields <- NULL
    if (!is.null(definition$nodes)) {
      for (n in definition$nodes) { if (!is.null(n$id)&&n$id==current_node_id) { node_def<-n; break } }
    }
    if (!is.null(node_def)&&!is.null(node_def$form)&&!is.null(node_def$form$fields)) {
      form_fields <- node_def$form$fields
    }
    current_todo_details(list(instance_id=instance_id, node_inst_id=node_inst_id, node_def=node_def, form_fields=form_fields))
    # 有表单 -> 弹窗渲染
    if (!is.null(form_fields)) {
      modal_body <- tagList(
        h4(sprintf("处理: %s", node_def$label%||%current_node_id)),
        p(sprintf("流程实例: %s", instance$instance_no[1])),
        hr()
      )
      for (f in form_fields) {
        fid <- paste0("proc_form_", f$key)
        label <- f$label%||%f$key
        if (f$type=="select") {
          choices <- f$options; names(choices) <- choices
          modal_body <- tagList(modal_body, selectInput(fid, label, choices=choices))
        } else if (f$type=="textarea") {
          modal_body <- tagList(modal_body, textAreaInput(fid, label, rows=3))
        } else {
          modal_body <- tagList(modal_body, textInput(fid, label))
        }
      }
      showModal(modalDialog(title=sprintf("处理节点: %s", node_def$label%||%current_node_id),
        modal_body, footer=tagList(modalButton("取消"),actionButton("proc_submit_form","提交",class="btn-success")),
        easyClose=TRUE))
    } else {
      # 无表单 -> 直接完成
      process_complete_todo(instance_id, node_inst_id, list(result="approved"))
    }
  })

  # 表单提交
  observeEvent(input$proc_submit_form, {
    info <- current_todo_details()
    if (is.null(info)) { showNotification("信息丢失",type="error"); return() }
    form_data <- list()
    if (!is.null(info$form_fields)) {
      for (f in info$form_fields) {
        val <- input[[paste0("proc_form_",f$key)]]
        if (!is.null(val)) form_data[[f$key]] <- val
      }
    }
    removeModal()
    process_complete_todo(info$instance_id, info$node_inst_id, form_data)
  })

  # 执行待办完成
  process_complete_todo <- function(instance_id, node_inst_id, form_data) {
    con <- db_connect()
    tryCatch({
      remark <- if (!is.null(form_data$remark)) form_data$remark else "已处理"
      result_val <- if (!is.null(form_data$result)) form_data$result else "approved"
      dbExecute(con, sprintf("UPDATE process_nodes SET status='completed',result='%s',completed_at=datetime('now','localtime'),remark='%s' WHERE id=%d",
        gsub("'","''",result_val),gsub("'","''",remark),node_inst_id))
    }, finally={ db_disconnect(con) })
    process_log_write(instance_id,as.character(node_inst_id),"info","task_complete",
      sprintf("用户处理待办: result=%s, remark=%s", result_val, remark))
    process_event_record("node_complete",instance_id,as.character(node_inst_id),source="user",status="success",
      message=sprintf("待办处理: %s",result_val))
    # 更新上下文
    inst <- process_instance_get(instance_id)
    if (!is.null(inst)) {
      ctx <- tryCatch(jsonlite::fromJSON(inst$context_data[1],simplifyVector=FALSE),error=function(e)list())
      ctx$result <- result_val; ctx$remark <- remark
      process_context_save(instance_id, ctx, changed_by=as.character(node_inst_id), reason=sprintf("节点处理: %s",result_val))
    }
    advance <- process_advance(instance_id)
    process_refresh_trigger(process_refresh_trigger()+1)
    if (advance$success) {
      if (isTRUE(advance$completed)) showNotification("流程已完成！",type="message",duration=5)
      else showNotification(sprintf("已处理，%s",advance$message),type="message",duration=5)
    } else showNotification(sprintf("处理完成:%s",advance$message),type="warning",duration=8)
  }
}
