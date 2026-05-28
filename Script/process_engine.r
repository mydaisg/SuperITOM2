# 审批引擎（企业微信风格）
# 模板+表单+审批链，简单直观

`%||%` <- function(a, b) if (is.null(a)) b else a

##################
# 编号生成
##################
appr_gen_no <- function(prefix = "APR") {
  date_str <- format(Sys.Date(), "%Y%m%d")
  con <- db_connect()
  tryCatch({
    today_prefix <- sprintf("%s%s", prefix, date_str)
    max_no <- dbGetQuery(con, sprintf("SELECT MAX(instance_no) as max_no FROM appr_instances WHERE instance_no LIKE '%s%%'", today_prefix))
    seq <- 1
    if (!is.na(max_no$max_no[1])) {
      last_seq <- as.integer(substr(max_no$max_no[1], nchar(today_prefix) + 1, nchar(today_prefix) + 3))
      if (!is.na(last_seq)) seq <- last_seq + 1
    }
    sprintf("%s%03d", today_prefix, seq)
  }, finally = { db_disconnect(con) })
}

##################
# 模板管理
##################
appr_tpl_list <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT * FROM appr_templates ORDER BY updated_at DESC") },
  finally={ db_disconnect(con) })
}

appr_tpl_get <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM appr_templates WHERE id=%d", as.integer(id)))
    if (nrow(r)==0) NULL else r
  }, finally={ db_disconnect(con) })
}

appr_tpl_create <- function(name, description="", category="general", icon="file-text",
                             form_fields="[]", approver_config="[]", cc_config="[]", created_by=NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO appr_templates (name,description,category,icon,form_fields,approver_config,cc_config,created_by) VALUES ('%s','%s','%s','%s','%s','%s','%s',%s)",
      gsub("'","''",name),gsub("'","''",description),category,icon,
      gsub("'","''",form_fields),gsub("'","''",approver_config),gsub("'","''",cc_config),
      ifelse(is.null(created_by),"NULL",as.character(created_by))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success=TRUE, id=id, message=sprintf("模板「%s」创建成功",name))
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

appr_tpl_update <- function(id, name, description="", category=NULL, icon=NULL, form_fields=NULL, approver_config=NULL, cc_config=NULL) {
  con <- db_connect()
  tryCatch({
    sets <- sprintf("name='%s',description='%s',updated_at=datetime('now','localtime')",
      gsub("'","''",name),gsub("'","''",description))
    if (!is.null(category)) sets <- paste0(sets, sprintf(",category='%s'",category))
    if (!is.null(icon)) sets <- paste0(sets, sprintf(",icon='%s'",icon))
    if (!is.null(form_fields)) sets <- paste0(sets, sprintf(",form_fields='%s'",gsub("'","''",form_fields)))
    if (!is.null(approver_config)) sets <- paste0(sets, sprintf(",approver_config='%s'",gsub("'","''",approver_config)))
    if (!is.null(cc_config)) sets <- paste0(sets, sprintf(",cc_config='%s'",gsub("'","''",cc_config)))
    dbExecute(con, sprintf("UPDATE appr_templates SET %s WHERE id=%d", sets, as.integer(id)))
    list(success=TRUE, message="更新成功")
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

appr_tpl_publish <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE appr_templates SET status='published',updated_at=datetime('now','localtime') WHERE id=%d",as.integer(id)))
    list(success=TRUE, message="已发布")
  }, finally={ db_disconnect(con) })
}

appr_tpl_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM appr_templates WHERE id=%d",as.integer(id)))
    list(success=TRUE, message="已删除")
  }, finally={ db_disconnect(con) })
}

##################
# 审批实例管理
##################
appr_inst_create <- function(template_id, title, form_data="{}", applicant_id=NULL) {
  tpl <- appr_tpl_get(template_id)
  if (is.null(tpl) || tpl$status[1]!="published") return(list(success=FALSE, message="模板不存在或未发布"))
  con <- db_connect()
  tryCatch({
    no <- appr_gen_no()
    form_json <- if (is.character(form_data)) form_data else jsonlite::toJSON(form_data, auto_unbox=TRUE)
    dbExecute(con, sprintf(
      "INSERT INTO appr_instances (instance_no,template_id,template_name,title,form_data,current_step,status,applicant_id) VALUES ('%s',%d,'%s','%s','%s',0,'pending',%s)",
      no,as.integer(template_id),gsub("'","''",tpl$name[1]),gsub("'","''",title),
      gsub("'","''",form_json),ifelse(is.null(applicant_id),"NULL",as.character(applicant_id))))
    inst_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    # 创建审批步骤
    approver_config <- tryCatch(jsonlite::fromJSON(tpl$approver_config[1],simplifyVector=FALSE),error=function(e)list())
    for (i in seq_along(approver_config)) {
      step <- approver_config[[i]]
      operator_ids <- if (!is.null(step$approver_ids)) jsonlite::toJSON(step$approver_ids, auto_unbox=TRUE) else "[]"
      approver_names <- if (!is.null(step$approver_names)) jsonlite::toJSON(step$approver_names, auto_unbox=TRUE) else "[]"
      dbExecute(con, sprintf(
        "INSERT INTO appr_steps (instance_id,step_index,step_type,operator_type,operator_ids,approver_names,status) VALUES (%d,%d,'approver','%s','%s','%s','pending')",
        inst_id,i,step$operator_type%||%"fixed",operator_ids,approver_names))
    }
    # 创建抄送记录
    cc_config <- tryCatch(jsonlite::fromJSON(tpl$cc_config[1],simplifyVector=FALSE),error=function(e)list())
    for (cc in cc_config) {
      for (uid in cc$user_ids) {
        uname <- cc$user_names%||%""
        dbExecute(con, sprintf("INSERT INTO appr_cc_records (instance_id,user_id,user_name) VALUES (%d,%d,'%s')",
          inst_id,as.integer(uid),gsub("'","''",uname)))
      }
    }
    # 激活第一步
    dbExecute(con, sprintf("UPDATE appr_steps SET status='active',entered_at=datetime('now','localtime') WHERE instance_id=%d AND step_index=1",inst_id))
    dbExecute(con, sprintf("UPDATE appr_instances SET current_step=1 WHERE id=%d",inst_id))
    list(success=TRUE, id=inst_id, instance_no=no, message=sprintf("审批 %s 已提交",no))
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

appr_inst_list <- function(applicant_id=NULL, status=NULL) {
  con <- db_connect()
  tryCatch({
    where <- "WHERE 1=1"
    if (!is.null(applicant_id)) where <- paste0(where, sprintf(" AND a.applicant_id=%d",as.integer(applicant_id)))
    if (!is.null(status) && nchar(status)>0) where <- paste0(where, sprintf(" AND a.status='%s'",status))
    dbGetQuery(con, sprintf(
      "SELECT a.*,u.display_name,u.username FROM appr_instances a LEFT JOIN users u ON a.applicant_id=u.id %s ORDER BY a.created_at DESC",where))
  }, finally={ db_disconnect(con) })
}

appr_inst_get <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT a.*,u.display_name,u.username FROM appr_instances a LEFT JOIN users u ON a.applicant_id=u.id WHERE a.id=%d",as.integer(id)))
    if (nrow(r)==0) NULL else r
  }, finally={ db_disconnect(con) })
}

# 待我审批的
appr_pending_list <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT a.id,a.instance_no,a.template_name,a.title,a.status,a.applicant_id,a.started_at,
              u.display_name as applicant_name,u.username as applicant_username,
              s.id as step_id,s.step_index,s.entered_at
       FROM appr_instances a
       JOIN appr_steps s ON a.id=s.instance_id
       LEFT JOIN users u ON a.applicant_id=u.id
       WHERE s.status='active' AND s.step_type='approver'
         AND s.operator_ids LIKE '%%\"%d\"%%'
       ORDER BY s.entered_at DESC", as.integer(user_id)))
  }, finally={ db_disconnect(con) })
}

# 我已处理的
appr_done_list <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT DISTINCT a.id,a.instance_no,a.template_name,a.title,a.status,a.applicant_id,a.started_at,a.completed_at,
              u.display_name as applicant_name,u.username as applicant_username,
              r.action as my_action,r.created_at as my_done_at
       FROM appr_instances a
       JOIN appr_records r ON a.id=r.instance_id AND r.operator_id=%d
       LEFT JOIN users u ON a.applicant_id=u.id
       ORDER BY r.created_at DESC", as.integer(user_id)))
  }, finally={ db_disconnect(con) })
}

# 抄送我的
appr_cc_list <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT c.id as cc_id,c.is_read,c.read_at,
              a.id as instance_id,a.instance_no,a.template_name,a.title,a.status,a.applicant_id,a.started_at,a.completed_at,
              u.display_name as applicant_name,u.username as applicant_username
       FROM appr_cc_records c
       JOIN appr_instances a ON c.instance_id=a.id
       LEFT JOIN users u ON a.applicant_id=u.id
       WHERE c.user_id=%d
       ORDER BY a.created_at DESC", as.integer(user_id)))
  }, finally={ db_disconnect(con) })
}

##################
# 审批操作
##################

# 通过
appr_approve <- function(instance_id, step_id, operator_id, operator_name, comment="") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE appr_steps SET status='approved' WHERE id=%d", as.integer(step_id)))
    dbExecute(con, sprintf(
      "INSERT INTO appr_records (instance_id,step_id,operator_id,operator_name,action,comment) VALUES (%d,%d,%d,'%s','approve','%s')",
      as.integer(instance_id),as.integer(step_id),as.integer(operator_id),
      gsub("'","''",operator_name%||%""),gsub("'","''",comment)))
    # 检查是否有下一步
    cur_step <- dbGetQuery(con, sprintf("SELECT step_index FROM appr_steps WHERE id=%d",as.integer(step_id)))$step_index[1]
    next_step <- dbGetQuery(con, sprintf("SELECT id,step_index FROM appr_steps WHERE instance_id=%d AND step_index=%d",
      as.integer(instance_id), as.integer(cur_step)+1))
    if (nrow(next_step)>0) {
      dbExecute(con, sprintf("UPDATE appr_steps SET status='active',entered_at=datetime('now','localtime') WHERE id=%d",next_step$id[1]))
      dbExecute(con, sprintf("UPDATE appr_instances SET current_step=%d WHERE id=%d",next_step$step_index[1],as.integer(instance_id)))
      list(success=TRUE, message="已通过，流转到下一步", done=FALSE)
    } else {
      dbExecute(con, sprintf("UPDATE appr_instances SET status='approved',completed_at=datetime('now','localtime'),current_step=99 WHERE id=%d",as.integer(instance_id)))
      list(success=TRUE, message="审批已全部通过，流程完成", done=TRUE)
    }
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

# 驳回
appr_reject <- function(instance_id, step_id, operator_id, operator_name, comment="") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE appr_steps SET status='rejected' WHERE id=%d", as.integer(step_id)))
    dbExecute(con, sprintf(
      "INSERT INTO appr_records (instance_id,step_id,operator_id,operator_name,action,comment) VALUES (%d,%d,%d,'%s','reject','%s')",
      as.integer(instance_id),as.integer(step_id),as.integer(operator_id),
      gsub("'","''",operator_name%||%""),gsub("'","''",comment)))
    dbExecute(con, sprintf("UPDATE appr_instances SET status='rejected',completed_at=datetime('now','localtime') WHERE id=%d",as.integer(instance_id)))
    list(success=TRUE, message="已驳回", done=TRUE)
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

# 撤销
appr_withdraw <- function(instance_id, applicant_id) {
  con <- db_connect()
  tryCatch({
    inst <- appr_inst_get(instance_id)
    if (is.null(inst) || inst$applicant_id[1]!=applicant_id) return(list(success=FALSE, message="无权撤销"))
    dbExecute(con, sprintf("UPDATE appr_instances SET status='withdrawn',completed_at=datetime('now','localtime') WHERE id=%d",as.integer(instance_id)))
    dbExecute(con, sprintf("UPDATE appr_steps SET status='skipped' WHERE instance_id=%d AND status='active'",as.integer(instance_id)))
    dbExecute(con, sprintf("INSERT INTO appr_records (instance_id,operator_id,action,comment) VALUES (%d,%d,'withdraw','申请人撤销')",
      as.integer(instance_id),as.integer(applicant_id)))
    list(success=TRUE, message="已撤销")
  }, finally={ db_disconnect(con) })
}

# 催办
appr_urge <- function(instance_id, operator_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO appr_records (instance_id,operator_id,action,comment) VALUES (%d,%d,'urge','催办')",
      as.integer(instance_id),as.integer(operator_id)))
    list(success=TRUE, message="已催办")
  }, finally={ db_disconnect(con) })
}

##################
# 操作记录
##################
appr_records_get <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM appr_records WHERE instance_id=%d ORDER BY created_at", as.integer(instance_id)))
  }, finally={ db_disconnect(con) })
}

appr_steps_get <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM appr_steps WHERE instance_id=%d ORDER BY step_index", as.integer(instance_id)))
  }, finally={ db_disconnect(con) })
}

##################
# 统计
##################
appr_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) as c FROM appr_instances")$c[1]
    pending <- dbGetQuery(con, "SELECT COUNT(*) as c FROM appr_instances WHERE status='pending'")$c[1]
    approved <- dbGetQuery(con, "SELECT COUNT(*) as c FROM appr_instances WHERE status='approved'")$c[1]
    rejected <- dbGetQuery(con, "SELECT COUNT(*) as c FROM appr_instances WHERE status='rejected'")$c[1]
    tpls <- nrow(appr_tpl_list())
    list(total=total, pending=pending, approved=approved, rejected=rejected, tpls=tpls)
  }, finally={ db_disconnect(con) })
}

##################
# 示例模板
##################
appr_create_demo_tpl <- function(created_by=NULL) {
  form_fields <- jsonlite::toJSON(list(
    list(key="reason", label="审批事由", type="text", required=TRUE, placeholder="请填写"),
    list(key="detail", label="详细说明", type="textarea", required=FALSE, placeholder="选填")
  ), auto_unbox=TRUE, pretty=TRUE)
  approver_config <- jsonlite::toJSON(list(
    list(step_name="直属上级审批", operator_type="fixed", approver_ids=list(1), approver_names=list("管理员")),
    list(step_name="负责人审批", operator_type="fixed", approver_ids=list(1), approver_names=list("管理员"))
  ), auto_unbox=TRUE, pretty=TRUE)
  cc_config <- jsonlite::toJSON(list(
    list(user_ids=list(1), user_names=list("管理员"))
  ), auto_unbox=TRUE, pretty=TRUE)
  appr_tpl_create(name="通用审批", description="适用于日常审批场景，支持两级审批+抄送",
    category="general", icon="file-text",
    form_fields=form_fields, approver_config=approver_config, cc_config=cc_config,
    created_by=created_by)
}
