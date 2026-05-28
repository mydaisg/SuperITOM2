# 审批模块服务端（企业微信风格）

process_server <- function(input, output, session, rv) {
  appr_trigger <- reactiveVal(0)
  current_user_id <- reactive({
    if (!is.null(rv$current_user) && nrow(rv$current_user)>0) rv$current_user$id[1] else NULL
  })
  current_user_name <- reactive({
    if (!is.null(rv$current_user) && nrow(rv$current_user)>0)
      (rv$current_user$display_name%||%rv$current_user$username)[1] else ""
  })

  # 统计卡片
  output$appr_stat_cards <- renderUI({
    appr_trigger()
    s <- appr_stats()
    uid <- current_user_id()
    my_pending <- if (!is.null(uid)) nrow(appr_pending_list(uid)) else 0
    my_cc <- if (!is.null(uid)) nrow(appr_cc_list(uid)) else 0
    fluidRow(
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#e3f2fd;",
        div(style="font-size:13px;color:#666;","我的待审批"),
        div(style="font-size:28px;font-weight:bold;color:#1565c0;",my_pending))),
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#e8f5e9;",
        div(style="font-size:13px;color:#666;","已通过"),
        div(style="font-size:28px;font-weight:bold;color:#2e7d32;",s$approved))),
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#fff3e0;",
        div(style="font-size:13px;color:#666;","抄送我的"),
        div(style="font-size:28px;font-weight:bold;color:#e65100;",my_cc))),
      column(3, div(class="well well-sm",style="text-align:center;padding:12px;background:#f3e5f5;",
        div(style="font-size:13px;color:#666;","审批模板"),
        div(style="font-size:28px;font-weight:bold;color:#7b1fa2;",s$tpls)))
    )
  })

  ##################
  # 待审批
  ##################
  output$appr_pending_table <- DT::renderDT({
    appr_trigger()
    uid <- current_user_id()
    if (is.null(uid)) return(DT::datatable(data.frame(信息="请先登录"),options=list(dom='t')))
    items <- appr_pending_list(uid)
    if (nrow(items)==0) return(DT::datatable(data.frame(信息="暂无待审批"),options=list(dom='t')))
    display <- data.frame(
      编号=items$instance_no, 模板=items$template_name, 标题=items$title,
      申请人=sprintf("<span style='color:#666;'>%s</span>",items$applicant_name%||%items$applicant_username),
      到达时间=items$entered_at,
      操作=sprintf('<button class="btn btn-success btn-xs appr-detail-btn" data-id="%s">处理</button>', items$id),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=20,dom='rtip',scrollX=TRUE,
      columnDefs=list(list(targets=5,orderable=FALSE))),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 我发起的
  ##################
  output$appr_my_table <- DT::renderDT({
    appr_trigger()
    uid <- current_user_id()
    if (is.null(uid)) return(DT::datatable(data.frame(信息="请先登录"),options=list(dom='t')))
    items <- appr_inst_list(applicant_id=uid, status=input$appr_my_status)
    if (nrow(items)==0) return(DT::datatable(data.frame(信息="暂无记录"),options=list(dom='t')))
    status_labels <- c("pending"="审批中","approved"="已通过","rejected"="已驳回","withdrawn"="已撤销")
    status_colors <- c("pending"="#f39c12","approved"="#27ae60","rejected"="#e74c3c","withdrawn"="#95a5a6")
    sts <- sapply(items$status, function(s)
      sprintf("<span style='color:%s;font-weight:bold;'>%s</span>",status_colors[s]%||%"#666",status_labels[s]%||%s))
    actions <- mapply(function(id, status) {
      btns <- sprintf('<button class="btn btn-info btn-xs appr-detail-btn" data-id="%s">详情</button>', id)
      if (status=="pending") btns <- paste0(btns,
        sprintf(' <button class="btn btn-warning btn-xs appr-urge-btn" data-id="%s">催办</button>',id),
        sprintf(' <button class="btn btn-default btn-xs appr-withdraw-btn" data-id="%s">撤销</button>',id))
      btns
    }, items$id, items$status, SIMPLIFY=TRUE)
    display <- data.frame(编号=items$instance_no,模板=items$template_name,标题=items$title,
      状态=sts,提交时间=items$started_at,操作=actions,stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=20,dom='rtip',scrollX=TRUE,
      columnDefs=list(list(targets=5,orderable=FALSE))),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 我处理的
  ##################
  output$appr_done_table <- DT::renderDT({
    appr_trigger()
    uid <- current_user_id()
    if (is.null(uid)) return(DT::datatable(data.frame(信息="请先登录"),options=list(dom='t')))
    items <- appr_done_list(uid)
    if (nrow(items)==0) return(DT::datatable(data.frame(信息="暂无处理记录"),options=list(dom='t')))
    action_labels <- c("approve"="已通过","reject"="已驳回","withdraw"="已撤销","urge"="催办")
    display <- data.frame(编号=items$instance_no,模板=items$template_name,标题=items$title,
      申请人=items$applicant_name%||%items$applicant_username,
      我的操作=action_labels[items$my_action]%||%items$my_action,
      操作时间=items$my_done_at,stringsAsFactors=FALSE)
    DT::datatable(display,options=list(pageLength=20,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 抄送我的
  ##################
  output$appr_cc_table <- DT::renderDT({
    appr_trigger()
    uid <- current_user_id()
    if (is.null(uid)) return(DT::datatable(data.frame(信息="请先登录"),options=list(dom='t')))
    items <- appr_cc_list(uid)
    if (nrow(items)==0) return(DT::datatable(data.frame(信息="暂无抄送"),options=list(dom='t')))
    display <- data.frame(编号=items$instance_no,模板=items$template_name,标题=items$title,
      申请人=items$applicant_name%||%items$applicant_username,
      状态=items$status,时间=items$started_at,stringsAsFactors=FALSE)
    DT::datatable(display,options=list(pageLength=20,dom='rtip',scrollX=TRUE),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 审批模板列表
  ##################
  output$appr_tpl_table <- DT::renderDT({
    appr_trigger()
    tpls <- appr_tpl_list()
    if (nrow(tpls)==0) return(DT::datatable(data.frame(信息="暂无模板"),options=list(dom='t')))
    display <- data.frame(
      名称=tpls$name, 描述=tpls$description%||%"", 分类=tpls$category,
      状态=ifelse(tpls$status=="published","已发布","草稿"),
      操作=sprintf(
        '<button class="btn btn-success btn-xs appr-start-btn" data-id="%s">发起</button>
         <button class="btn btn-info btn-xs appr-detail-btn" data-id="%s">编辑</button>
         %s
         <button class="btn btn-danger btn-xs appr-del-tpl-btn" data-id="%s">删除</button>',
        tpls$id, tpls$id,
        ifelse(tpls$status=="draft",sprintf('<button class="btn btn-primary btn-xs appr-publish-btn" data-id="%s">发布</button>',tpls$id),""),
        tpls$id),
      stringsAsFactors=FALSE)
    DT::datatable(display,escape=FALSE,options=list(pageLength=20,dom='rtip',scrollX=TRUE,
      columnDefs=list(list(targets=4,orderable=FALSE))),rownames=FALSE,class='cell-border stripe hover')
  })

  ##################
  # 操作处理
  ##################
  observeEvent(input$appr_refresh_my, { appr_trigger(appr_trigger()+1) })
  observeEvent(input$appr_refresh_tpl, { appr_trigger(appr_trigger()+1) })

  # 创建示例模板
  observeEvent(input$appr_create_demo, {
    result <- appr_create_demo_tpl(created_by=current_user_id())
    if (result$success) {
      appr_trigger(appr_trigger()+1)
      showNotification(sprintf("已创建「%s」，请发布后使用",result$message),type="message",duration=5)
    } else showNotification(result$message,type="error")
  })

  # 新建模板
  observeEvent(input$appr_new_tpl, {
    showModal(modalDialog(title="新建审批模板",size="l",
      textInput("appr_new_name","模板名称"),
      textInput("appr_new_desc","描述"),
      selectInput("appr_new_cat","分类",
        choices=c("通用"="general","请假"="leave","报销"="expense","加班"="overtime","出差"="travel","审批"="approval")),
      h5("表单字段配置"),
      p(style="font-size:12px;color:#666;","每行一个字段: key|标签|类型(text/textarea/select/number/date)"),
      textAreaInput("appr_new_fields","",rows=4,value="reason|审批事由|text\ndetail|详细说明|textarea"),
      h5("审批人配置"),
      p(style="font-size:12px;color:#666;","每行一个步骤: 步骤名|审批人ID(逗号分隔)"),
      textAreaInput("appr_new_approvers","",rows=3,value="直属上级|1\n负责人审批|1"),
      h5("抄送人配置"),
      p(style="font-size:12px;color:#666;","每行: 用户ID|用户名"),
      textAreaInput("appr_new_cc","",rows=2,value="1|管理员"),
      footer=tagList(modalButton("取消"),actionButton("appr_save_tpl","创建",class="btn-primary")),easyClose=TRUE))
  })

  observeEvent(input$appr_save_tpl, {
    req(input$appr_new_name)
    lines <- strsplit(input$appr_new_fields,"\n")[[1]]
    fields <- list()
    for (line in lines) {
      parts <- trimws(strsplit(line,"\\|")[[1]])
      if (length(parts)>=2) fields[[length(fields)+1]] <- list(key=parts[1],label=parts[2],
        type=ifelse(length(parts)>=3,parts[3],"text"),required=TRUE)
    }
    alines <- strsplit(input$appr_new_approvers,"\n")[[1]]
    approvers <- list()
    for (line in alines) {
      parts <- trimws(strsplit(line,"\\|")[[1]])
      if (length(parts)>=2) {
        ids <- as.integer(trimws(strsplit(parts[2],",")[[1]]))
        con <- db_connect()
        names <- tryCatch({
          n <- dbGetQuery(con,sprintf("SELECT display_name,username FROM users WHERE id IN (%s)",paste(ids,collapse=",")))
          if (nrow(n)>0) ifelse(is.na(n$display_name)%||%n$display_name=="", n$username, n$display_name) else as.character(ids)
        }, finally={ db_disconnect(con) })
        approvers[[length(approvers)+1]] <- list(step_name=parts[1],operator_type="fixed",
          approver_ids=as.list(ids), approver_names=as.list(names))
      }
    }
    clines <- strsplit(input$appr_new_cc,"\n")[[1]]
    cc_list <- list(user_ids=list(), user_names=list())
    for (line in clines) {
      parts <- trimws(strsplit(line,"\\|")[[1]])
      if (length(parts)>=1 && nchar(parts[1])>0) {
        cc_list$user_ids[[length(cc_list$user_ids)+1]] <- as.integer(parts[1])
        cc_list$user_names[[length(cc_list$user_names)+1]] <- ifelse(length(parts)>=2,parts[2],parts[1])
      }
    }
    result <- appr_tpl_create(name=input$appr_new_name,description=input$appr_new_desc%||%"",
      category=input$appr_new_cat,
      form_fields=jsonlite::toJSON(fields,auto_unbox=TRUE,pretty=TRUE),
      approver_config=jsonlite::toJSON(approvers,auto_unbox=TRUE,pretty=TRUE),
      cc_config=if(length(cc_list$user_ids)>0) jsonlite::toJSON(list(cc_list),auto_unbox=TRUE,pretty=TRUE) else "[]",
      created_by=current_user_id())
    removeModal()
    if (result$success) { appr_trigger(appr_trigger()+1); showNotification(result$message,type="message",duration=5)
    } else showNotification(result$message,type="error")
  })

  observeEvent(input$appr_publish_click, {
    result <- appr_tpl_publish(as.integer(input$appr_publish_click))
    appr_trigger(appr_trigger()+1)
    showNotification(result$message,type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$appr_del_tpl_click, {
    appr_tpl_delete(as.integer(input$appr_del_tpl_click))
    appr_trigger(appr_trigger()+1)
    showNotification("已删除",type="message")
  })

  # 发起审批
  observeEvent(input$appr_start_click, {
    tid <- as.integer(input$appr_start_click)
    tpl <- appr_tpl_get(tid)
    if (is.null(tpl)||tpl$status[1]!="published") { showNotification("模板未发布",type="warning"); return() }
    fields <- tryCatch(jsonlite::fromJSON(tpl$form_fields[1],simplifyVector=FALSE),error=function(e)list())
    modal_body <- tagList(
      h4(sprintf("发起审批: %s",tpl$name[1])),
      p(tpl$description[1]%||%"",style="color:#7f8c8d;font-size:12px;"),
      hr(),
      textInput("appr_start_title","审批标题",value=tpl$name[1]))
    for (f in fields) {
      fid <- paste0("appr_f_",f$key)
      label <- f$label%||%f$key
      if (f$type=="textarea") modal_body <- tagList(modal_body, textAreaInput(fid,label,rows=3,placeholder=f$placeholder%||%""))
      else if (f$type=="select") modal_body <- tagList(modal_body, selectInput(fid,label,choices=unlist(f$options%||%list())))
      else modal_body <- tagList(modal_body, textInput(fid,label,placeholder=f$placeholder%||%""))
    }
    showModal(modalDialog(title=sprintf("发起审批: %s",tpl$name[1]),modal_body,
      footer=tagList(modalButton("取消"),actionButton("appr_confirm_start","提交审批",class="btn-primary")),size="m",easyClose=TRUE))
  })

  observeEvent(input$appr_confirm_start, {
    req(input$appr_start_click, input$appr_start_title)
    tid <- as.integer(input$appr_start_click)
    tpl <- appr_tpl_get(tid)
    if (is.null(tpl)) { showNotification("模板不存在",type="error"); return() }
    fields <- tryCatch(jsonlite::fromJSON(tpl$form_fields[1],simplifyVector=FALSE),error=function(e)list())
    form_data <- list()
    for (f in fields) form_data[[f$key]] <- input[[paste0("appr_f_",f$key)]]
    result <- appr_inst_create(template_id=tid,title=input$appr_start_title,form_data=form_data,applicant_id=current_user_id())
    removeModal()
    if (result$success) { appr_trigger(appr_trigger()+1)
      showNotification(sprintf("已提交，编号: %s",result$instance_no),type="message",duration=8)
    } else showNotification(result$message,type="error")
  })

  # 详情弹窗（判断是实例还是模板）
  observeEvent(input$appr_detail_click, {
    id <- as.integer(input$appr_detail_click)
    inst <- appr_inst_get(id)
    if (!is.null(inst)) {
      records <- appr_records_get(id)
      steps <- appr_steps_get(id)
      form_data <- tryCatch(jsonlite::fromJSON(inst$form_data[1],simplifyVector=FALSE),error=function(e)list())
      status_label <- c("pending"="审批中","approved"="已通过","rejected"="已驳回","withdrawn"="已撤销")[inst$status[1]]%||%inst$status[1]
      form_html <- ""
      if (is.list(form_data)) for (nm in names(form_data)) form_html <- paste0(form_html,sprintf("<p><strong>%s:</strong> %s</p>",nm,form_data[[nm]]%||%""))
      record_html <- ""
      if (nrow(records)>0) for (i in 1:nrow(records)) {
        r <- records[i,]
        act <- c("submit"="提交","approve"="通过","reject"="驳回","withdraw"="撤销","urge"="催办")[r$action]%||%r$action
        col <- switch(r$action,approve="#27ae60",reject="#e74c3c",withdraw="#95a5a6",urge="#f39c12","#333")
        record_html <- paste0(record_html,sprintf("<p style='margin:2px 0;'><span style='color:%s;font-weight:bold;'>[%s]</span> %s: %s <span style='color:#999;font-size:11px;'>%s</span></p>",
          col,act,r$operator_name%||%"系统",r$comment%||%"",r$created_at%||%""))
      }
      step_html <- ""
      if (nrow(steps)>0) for (i in 1:nrow(steps)) {
        s <- steps[i,]
        icons <- c("pending"="○","active"="●","approved"="✓","rejected"="✗","skipped"="-")
        cols <- c("pending"="#ccc","active"="#f39c12","approved"="#27ae60","rejected"="#e74c3c","skipped"="#ddd")
        op_ids <- tryCatch(jsonlite::fromJSON(s$operator_ids),error=function(e)c())
        op_nms <- tryCatch(jsonlite::fromJSON(s$approver_names),error=function(e)c())
        op_str <- if (length(op_nms)>0) paste(op_nms,collapse=",") else paste(op_ids,collapse=",")
        step_html <- paste0(step_html,sprintf("<p style='margin:3px 0;'><span style='color:%s;font-size:18px;margin-right:8px;'>%s</span> 第%d步: %s</p>",
          cols[s$status]%||%"#ccc",icons[s$status]%||%"?",i,op_str))
      }
      uid <- current_user_id()
      can_approve <- FALSE
      active_step <- steps[steps$status=="active",]
      if (nrow(active_step)>0 && inst$status[1]=="pending") {
        ops <- tryCatch(jsonlite::fromJSON(active_step$operator_ids[1]),error=function(e)c())
        if (uid %in% ops) can_approve <- TRUE
      }
      showModal(modalDialog(title=sprintf("审批详情: %s",inst$instance_no[1]),size="l",
        tagList(
          div(style="background:#f8f9fa;padding:12px;border-radius:4px;margin-bottom:10px;",
            h5(inst$title[1],style="margin:0 0 5px;"),
            p(sprintf("模板: %s | 状态: %s",inst$template_name[1],status_label),style="margin:0;font-size:12px;color:#666;"),
            p(sprintf("申请人: %s | 提交时间: %s",inst$display_name%||%inst$username,inst$started_at),style="margin:0;font-size:12px;color:#666;")),
          h5("表单内容"),div(HTML(form_html)),
          h5("审批进度"),div(HTML(step_html)),
          if (nchar(record_html)>0) tagList(h5("审批记录"),div(HTML(record_html))),
          if (can_approve) tagList(hr(),textAreaInput("appr_comment","审批意见",rows=2,placeholder="选填"),
            div(style="text-align:right;",
              actionButton("appr_do_approve","通过",class="btn-success",icon=icon("check"),style="margin-right:8px;"),
              actionButton("appr_do_reject","驳回",class="btn-danger",icon=icon("times"))))
        ),footer=modalButton("关闭"),easyClose=TRUE))
    } else {
      # 编辑模板
      tpl <- appr_tpl_get(id)
      if (is.null(tpl)) return()
      fields <- tryCatch(jsonlite::fromJSON(tpl$form_fields[1],simplifyVector=FALSE),error=function(e)list())
      fstr <- paste(sapply(fields,function(f) paste(f$key,f$label,f$type%||%"text",sep="|")),collapse="\n")
      apprs <- tryCatch(jsonlite::fromJSON(tpl$approver_config[1],simplifyVector=FALSE),error=function(e)list())
      astr <- paste(sapply(apprs,function(a) paste(a$step_name%||%"",paste(unlist(a$approver_ids),collapse=","),sep="|")),collapse="\n")
      cc <- tryCatch(jsonlite::fromJSON(tpl$cc_config[1],simplifyVector=FALSE),error=function(e)list())
      cstr <- ""
      if (length(cc)>0 && !is.null(cc[[1]]$user_ids)) {
        cstr <- paste(sapply(seq_along(cc[[1]]$user_ids),function(i){
          uid_val <- cc[[1]]$user_ids[[i]]
          uname <- if(length(cc[[1]]$user_names)>=i) cc[[1]]$user_names[[i]] else ""
          paste(uid_val,uname,sep="|")
        }),collapse="\n")
      }
      showModal(modalDialog(title=sprintf("编辑模板: %s",tpl$name[1]),size="l",
        textInput("appr_edit_name","模板名称",value=tpl$name[1]),
        textInput("appr_edit_desc","描述",value=tpl$description[1]%||%""),
        selectInput("appr_edit_cat","分类",choices=c("通用"="general","请假"="leave","报销"="expense","加班"="overtime","出差"="travel","审批"="approval"),selected=tpl$category[1]%||%"general"),
        h5("表单字段"),textAreaInput("appr_edit_fields","",rows=4,value=fstr),
        h5("审批人配置"),textAreaInput("appr_edit_approvers","",rows=3,value=astr),
        h5("抄送人配置"),textAreaInput("appr_edit_cc","",rows=2,value=cstr),
        footer=tagList(modalButton("取消"),actionButton("appr_save_edit","保存",class="btn-primary")),easyClose=TRUE))
    }
  })

  observeEvent(input$appr_save_edit, {
    req(input$appr_detail_click)
    tpl <- appr_tpl_get(as.integer(input$appr_detail_click))
    if (is.null(tpl)) return()
    id <- tpl$id[1]
    lines <- strsplit(input$appr_edit_fields,"\n")[[1]]
    fields <- list()
    for (line in lines) { parts <- trimws(strsplit(line,"\\|")[[1]]); if (length(parts)>=2) fields[[length(fields)+1]] <- list(key=parts[1],label=parts[2],type=ifelse(length(parts)>=3,parts[3],"text"),required=TRUE) }
    alines <- strsplit(input$appr_edit_approvers,"\n")[[1]]
    approvers <- list()
    for (line in alines) {
      parts <- trimws(strsplit(line,"\\|")[[1]]); if (length(parts)>=2) {
        ids <- as.integer(trimws(strsplit(parts[2],",")[[1]]))
        con <- db_connect()
        names <- tryCatch({ n <- dbGetQuery(con,sprintf("SELECT display_name,username FROM users WHERE id IN (%s)",paste(ids,collapse=",")))
          if (nrow(n)>0) ifelse(is.na(n$display_name)%||%n$display_name=="", n$username, n$display_name) else as.character(ids)
        }, finally={ db_disconnect(con) })
        approvers[[length(approvers)+1]] <- list(step_name=parts[1],operator_type="fixed",approver_ids=as.list(ids),approver_names=as.list(names))
      }
    }
    clines <- strsplit(input$appr_edit_cc,"\n")[[1]]
    cc_list <- list(user_ids=list(),user_names=list())
    for (line in clines) {
      parts <- trimws(strsplit(line,"\\|")[[1]]); if (length(parts)>=1 && nchar(parts[1])>0) {
        cc_list$user_ids[[length(cc_list$user_ids)+1]] <- as.integer(parts[1])
        cc_list$user_names[[length(cc_list$user_names)+1]] <- ifelse(length(parts)>=2,parts[2],parts[1])
      }
    }
    result <- appr_tpl_update(id=id,name=input$appr_edit_name,description=input$appr_edit_desc%||%"",category=input$appr_edit_cat,
      form_fields=jsonlite::toJSON(fields,auto_unbox=TRUE,pretty=TRUE),
      approver_config=jsonlite::toJSON(approvers,auto_unbox=TRUE,pretty=TRUE),
      cc_config=if(length(cc_list$user_ids)>0) jsonlite::toJSON(list(cc_list),auto_unbox=TRUE,pretty=TRUE) else "[]")
    removeModal()
    if (result$success) { appr_trigger(appr_trigger()+1); showNotification("保存成功",type="message")
    } else showNotification(result$message,type="error")
  })

  observeEvent(input$appr_do_approve, {
    id <- isolate(input$appr_detail_click)
    inst <- appr_inst_get(as.integer(id))
    if (is.null(inst)) return()
    steps <- appr_steps_get(as.integer(id))
    active_step <- steps[steps$status=="active",]
    if (nrow(active_step)==0) { showNotification("没有活跃的审批步骤",type="warning"); return() }
    result <- appr_approve(instance_id=as.integer(id),step_id=active_step$id[1],
      operator_id=current_user_id(),operator_name=current_user_name(),comment=input$appr_comment%||%"")
    removeModal(); appr_trigger(appr_trigger()+1)
    showNotification(result$message,type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$appr_do_reject, {
    id <- isolate(input$appr_detail_click)
    inst <- appr_inst_get(as.integer(id))
    if (is.null(inst)) return()
    steps <- appr_steps_get(as.integer(id))
    active_step <- steps[steps$status=="active",]
    if (nrow(active_step)==0) { showNotification("没有活跃的审批步骤",type="warning"); return() }
    result <- appr_reject(instance_id=as.integer(id),step_id=active_step$id[1],
      operator_id=current_user_id(),operator_name=current_user_name(),comment=input$appr_comment%||%"")
    removeModal(); appr_trigger(appr_trigger()+1)
    showNotification(result$message,type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$appr_withdraw_click, {
    result <- appr_withdraw(as.integer(input$appr_withdraw_click),current_user_id())
    appr_trigger(appr_trigger()+1)
    showNotification(result$message,type=ifelse(result$success,"message","error"))
  })

  observeEvent(input$appr_urge_click, {
    result <- appr_urge(as.integer(input$appr_urge_click),current_user_id())
    showNotification(result$message,type="message")
  })
}
