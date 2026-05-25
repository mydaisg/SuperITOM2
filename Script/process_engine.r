# 流程引擎核心模块 v3 — 完整功能

`%||%` <- function(a, b) if (is.null(a)) b else a

##################
# 编号生成
##################

process_generate_no <- function(prefix = "PRC") {
  date_str <- format(Sys.Date(), "%Y%m%d")
  con <- db_connect()
  tryCatch({
    today_prefix <- sprintf("%s%s", prefix, date_str)
    max_no <- dbGetQuery(con, sprintf("SELECT MAX(def_no) as max_no FROM process_definitions WHERE def_no LIKE '%s%%'", today_prefix))
    seq <- 1
    if (!is.na(max_no$max_no[1])) {
      last_seq <- as.integer(substr(max_no$max_no[1], nchar(today_prefix) + 1, nchar(today_prefix) + 3))
      if (!is.na(last_seq)) seq <- last_seq + 1
    }
    sprintf("%s%03d", today_prefix, seq)
  }, finally = { db_disconnect(con) })
}

process_instance_generate_no <- function() {
  date_str <- format(Sys.Date(), "%Y%m%d")
  con <- db_connect()
  tryCatch({
    today_prefix <- sprintf("PFI%s", date_str)
    max_no <- dbGetQuery(con, sprintf("SELECT MAX(instance_no) as max_no FROM process_instances WHERE instance_no LIKE '%s%%'", today_prefix))
    seq <- 1
    if (!is.na(max_no$max_no[1])) {
      last_seq <- as.integer(substr(max_no$max_no[1], nchar(today_prefix) + 1, nchar(today_prefix) + 3))
      if (!is.na(last_seq)) seq <- last_seq + 1
    }
    sprintf("%s%03d", today_prefix, seq)
  }, finally = { db_disconnect(con) })
}

##################
# 日志与事件
##################

process_log_write <- function(instance_id, node_id = NULL, log_level = "info", log_type = "general", message, duration_ms = NULL, detail = NULL) {
  tryCatch({
    con <- db_connect(); on.exit(db_disconnect(con))
    dbExecute(con, sprintf(
      "INSERT INTO process_logs (instance_id,node_id,log_level,log_type,message,duration_ms,detail) VALUES (%s,%s,'%s','%s','%s',%s,%s)",
      ifelse(is.null(instance_id),"NULL",as.character(instance_id)),
      ifelse(is.null(node_id),"NULL",sprintf("'%s'",gsub("'","''",node_id))),
      log_level,log_type,gsub("'","''",substr(message,1,500)),
      ifelse(is.null(duration_ms),"NULL",as.character(duration_ms)),
      ifelse(is.null(detail),"NULL",sprintf("'%s'",gsub("'","''",detail)))))
  }, error = function(e) { warning("日志写入失败:", e$message) })
}

process_event_record <- function(event_type, instance_id = NULL, node_id = NULL, source = "engine", status = "success", message = NULL, payload = NULL) {
  tryCatch({
    con <- db_connect(); on.exit(db_disconnect(con))
    dbExecute(con, sprintf(
      "INSERT INTO process_events (event_type,instance_id,node_id,source,status,message,payload) VALUES ('%s',%s,%s,'%s','%s',%s,%s)",
      event_type,
      ifelse(is.null(instance_id),"NULL",as.character(instance_id)),
      ifelse(is.null(node_id),"NULL",sprintf("'%s'",gsub("'","''",node_id))),
      source,status,
      ifelse(is.null(message),"NULL",sprintf("'%s'",gsub("'","''",message))),
      ifelse(is.null(payload),"NULL",sprintf("'%s'",gsub("'","''",payload)))))
  }, error = function(e) { warning("事件记录失败:", e$message) })
}

##################
# 流程定义管理
##################

process_def_create <- function(name, description = "", category = "general", definition = "{}", created_by = NULL) {
  con <- db_connect()
  tryCatch({
    def_no <- process_generate_no()
    dbExecute(con, sprintf(
      "INSERT INTO process_definitions (def_no,name,description,category,definition,created_by) VALUES ('%s','%s','%s','%s','%s',%s)",
      def_no,gsub("'","''",name),gsub("'","''",description),category,gsub("'","''",definition),
      ifelse(is.null(created_by),"NULL",as.character(created_by))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    process_log_write(NULL,NULL,"info","def_create",sprintf("创建流程定义: %s (%s)",name,def_no))
    list(success=TRUE, id=id, def_no=def_no, message=sprintf("流程定义「%s」创建成功",name))
  }, error=function(e) { list(success=FALSE, message=paste("创建失败:",e$message))
  }, finally={ db_disconnect(con) })
}

process_def_list <- function(category = NULL, status = NULL) {
  con <- db_connect()
  tryCatch({
    where <- "WHERE 1=1"
    if (!is.null(category) && nchar(category)>0) where <- paste0(where, sprintf(" AND category='%s'",category))
    if (!is.null(status) && nchar(status)>0) where <- paste0(where, sprintf(" AND status='%s'",status))
    dbGetQuery(con, sprintf(
      "SELECT pd.*,u.username as creator_name FROM process_definitions pd LEFT JOIN users u ON pd.created_by=u.id %s ORDER BY pd.updated_at DESC",where))
  }, finally={ db_disconnect(con) })
}

process_def_get <- function(def_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf(
      "SELECT pd.*,u.username as creator_name FROM process_definitions pd LEFT JOIN users u ON pd.created_by=u.id WHERE pd.id=%d",as.integer(def_id)))
    if (nrow(result)==0) NULL else result
  }, finally={ db_disconnect(con) })
}

process_def_publish <- function(def_id, change_log = "") {
  con <- db_connect()
  tryCatch({
    def <- process_def_get(def_id)
    if (is.null(def)) return(list(success=FALSE, message="流程定义不存在"))
    if (def$status[1]=="published") return(list(success=TRUE, message="已发布"))
    new_ver <- def$version[1] + 1
    dbExecute(con, sprintf("INSERT INTO process_definition_versions (def_id,version,definition,change_log) VALUES (%d,%d,'%s','%s')",
      as.integer(def_id),new_ver,gsub("'","''",def$definition[1]),gsub("'","''",change_log)))
    dbExecute(con, sprintf("UPDATE process_definitions SET version=%d,status='published',updated_at=datetime('now','localtime') WHERE id=%d",new_ver,as.integer(def_id)))
    process_log_write(NULL,NULL,"info","def_publish",sprintf("发布流程定义: %s v%d",def$name[1],new_ver))
    list(success=TRUE, message=sprintf("发布成功（v%d）",new_ver))
  }, error=function(e) { list(success=FALSE, message=paste("发布失败:",e$message))
  }, finally={ db_disconnect(con) })
}

process_def_get_versions <- function(def_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_definition_versions WHERE def_id=%d ORDER BY version DESC",as.integer(def_id)))
  }, finally={ db_disconnect(con) })
}

##################
# 流程实例管理
##################

process_instance_start <- function(def_id, title = NULL, context_data = NULL, started_by = NULL) {
  con <- db_connect()
  tryCatch({
    def <- process_def_get(def_id)
    if (is.null(def)) return(list(success=FALSE, message="流程定义不存在"))
    if (def$status[1]!="published") return(list(success=FALSE, message="流程定义未发布"))
    instance_no <- process_instance_generate_no()
    if (is.null(title)||title=="") title <- def$name[1]
    if (is.null(context_data)) context_data <- list()
    context_json <- jsonlite::toJSON(context_data, auto_unbox=TRUE)
    dbExecute(con, sprintf(
      "INSERT INTO process_instances (instance_no,def_id,def_version,title,status,context_data,context_version,started_by) VALUES ('%s',%d,%d,'%s','running','%s',1,%s)",
      instance_no,as.integer(def_id),def$version[1],gsub("'","''",title),gsub("'","''",context_json),
      ifelse(is.null(started_by),"NULL",as.character(started_by))))
    instance_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    process_log_write(instance_id,NULL,"info","instance_start",sprintf("启动流程: %s (%s)",title,instance_no))
    process_event_record("instance_start",instance_id,NULL,source="engine",status="success",message=sprintf("流程 %s 已启动",instance_no))
    # 保存初始上下文版本
    process_context_save(instance_id, context_data, changed_by=ifelse(is.null(started_by),"system",as.character(started_by)), reason="流程启动")
    # 激活开始节点
    definition <- tryCatch(jsonlite::fromJSON(def$definition[1],simplifyVector=FALSE),error=function(e)NULL)
    if (!is.null(definition$nodes)) {
      for (node in definition$nodes) { if (!is.null(node$type)&&node$type=="start") { process_activate_node(instance_id,node); break } }
    }
    list(success=TRUE, id=instance_id, instance_no=instance_no, def_name=def$name[1], message=sprintf("流程实例 %s 已启动",instance_no))
  }, error=function(e) { list(success=FALSE, message=paste("启动失败:",e$message))
  }, finally={ db_disconnect(con) })
}

process_instance_list <- function(status = NULL, user_id = NULL) {
  con <- db_connect()
  tryCatch({
    where <- "WHERE 1=1"
    if (!is.null(status)&&nchar(status)>0) where <- paste0(where, sprintf(" AND pi.status='%s'",status))
    if (!is.null(user_id)) where <- paste0(where, sprintf(" AND pi.started_by=%d",as.integer(user_id)))
    dbGetQuery(con, sprintf(
      "SELECT pi.*,pd.name as def_name,u.username as started_by_name FROM process_instances pi LEFT JOIN process_definitions pd ON pi.def_id=pd.id LEFT JOIN users u ON pi.started_by=u.id %s ORDER BY pi.started_at DESC",where))
  }, finally={ db_disconnect(con) })
}

process_instance_get <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf(
      "SELECT pi.*,pd.name as def_name,pd.definition FROM process_instances pi LEFT JOIN process_definitions pd ON pi.def_id=pd.id WHERE pi.id=%d",as.integer(instance_id)))
    if (nrow(result)==0) NULL else result
  }, finally={ db_disconnect(con) })
}

process_instance_terminate <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE process_instances SET status='terminated',completed_at=datetime('now','localtime'),updated_at=datetime('now','localtime') WHERE id=%d",as.integer(instance_id)))
    dbExecute(con, sprintf("UPDATE process_nodes SET status='skipped' WHERE instance_id=%d AND status='active'",as.integer(instance_id)))
    process_log_write(instance_id,NULL,"warn","instance_terminate","流程已终止")
    list(success=TRUE, message="流程已终止")
  }, finally={ db_disconnect(con) })
}

process_instance_suspend <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE process_instances SET status='suspended',updated_at=datetime('now','localtime') WHERE id=%d AND status='running'",as.integer(instance_id)))
    process_log_write(instance_id,NULL,"info","instance_suspend","流程已暂停")
    list(success=TRUE, message="流程已暂停")
  }, finally={ db_disconnect(con) })
}

process_instance_resume <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE process_instances SET status='running',updated_at=datetime('now','localtime') WHERE id=%d AND status='suspended'",as.integer(instance_id)))
    process_log_write(instance_id,NULL,"info","instance_resume","流程已恢复")
    list(success=TRUE, message="流程已恢复")
  }, finally={ db_disconnect(con) })
}

##################
# 节点管理
##################

process_activate_node <- function(instance_id, node_def) {
  tryCatch({
    existing <- process_get_node(instance_id, node_def$id)
    if (!is.null(existing)) return(TRUE)
    con <- db_connect(); on.exit(db_disconnect(con))
    dbExecute(con, sprintf(
      "INSERT INTO process_nodes (instance_id,node_id,node_type,node_name,status,auto_action,timeout_minutes,timeout_action,max_retries,entered_at) VALUES (%d,'%s','%s','%s','active',%s,%d,'%s',%d,datetime('now','localtime'))",
      as.integer(instance_id),node_def$id,node_def$type,gsub("'","''",node_def$label%||%node_def$id),
      if(is.null(node_def$action))"NULL" else sprintf("'%s'",gsub("'","''",jsonlite::toJSON(node_def$action,auto_unbox=TRUE))),
      node_def$timeout_minutes%||%0,node_def$timeout_action%||%"terminate",node_def$max_retries%||%3))
    dbExecute(con, sprintf("UPDATE process_instances SET current_node='%s',updated_at=datetime('now','localtime') WHERE id=%d",node_def$id,as.integer(instance_id)))
    TRUE
  }, error=function(e) { process_log_write(instance_id,node_def$id%||%"","error","node_error",sprintf("激活节点失败:%s",e$message)); FALSE })
}

process_get_node <- function(instance_id, node_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf("SELECT * FROM process_nodes WHERE instance_id=%d AND node_id='%s'",as.integer(instance_id),node_id))
    if (nrow(result)==0) NULL else result
  }, finally={ db_disconnect(con) })
}

process_get_active_nodes <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_nodes WHERE instance_id=%d ORDER BY id",as.integer(instance_id)))
  }, finally={ db_disconnect(con) })
}

##################
# 上下文管理
##################

process_context_save <- function(instance_id, context_data, changed_by = "system", reason = "") {
  con <- db_connect()
  tryCatch({
    inst <- process_instance_get(instance_id)
    if (is.null(inst)) return()
    new_version <- inst$context_version[1] + 1
    context_json <- jsonlite::toJSON(context_data, auto_unbox=TRUE)
    dbExecute(con, sprintf(
      "INSERT INTO process_context_history (instance_id,version,context_data,changed_by,change_reason) VALUES (%d,%d,'%s','%s','%s')",
      as.integer(instance_id),new_version,gsub("'","''",context_json),gsub("'","''",as.character(changed_by)),gsub("'","''",reason)))
    dbExecute(con, sprintf(
      "UPDATE process_instances SET context_data='%s',context_version=%d,updated_at=datetime('now','localtime') WHERE id=%d",
      gsub("'","''",context_json),new_version,as.integer(instance_id)))
  }, finally={ db_disconnect(con) })
}

process_context_get_history <- function(instance_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_context_history WHERE instance_id=%d ORDER BY version DESC",as.integer(instance_id)))
  }, finally={ db_disconnect(con) })
}

##################
# 流转引擎
##################

process_advance <- function(instance_id) {
  instance <- process_instance_get(instance_id)
  if (is.null(instance)) return(list(success=FALSE, message="实例不存在"))
  if (instance$status[1]!="running") return(list(success=FALSE, message="流程不在运行状态"))
  current_node_id <- instance$current_node[1]
  if (is.null(current_node_id)||is.na(current_node_id)) return(list(success=FALSE, message="无当前节点"))
  definition <- tryCatch(jsonlite::fromJSON(instance$definition[1],simplifyVector=FALSE),error=function(e)NULL)
  if (is.null(definition)||is.null(definition$nodes)) return(list(success=FALSE, message="流程定义JSON解析失败"))
  # 查找当前节点
  current_node <- NULL
  for (n in definition$nodes) { if (!is.null(n$id)&&n$id==current_node_id) { current_node<-n; break } }
  if (is.null(current_node)) return(list(success=FALSE, message=sprintf("节点 %s 未在定义中找到",current_node_id)))
  # -- end →
  if (current_node$type=="end") {
    con <- db_connect()
    tryCatch({ dbExecute(con, sprintf("UPDATE process_instances SET status='completed',completed_at=datetime('now','localtime'),updated_at=datetime('now','localtime') WHERE id=%d",as.integer(instance_id)))
    }, finally={ db_disconnect(con) })
    process_log_write(instance_id,current_node_id,"info","instance_complete","流程已完成")
    process_event_record("instance_end",instance_id,current_node_id,source="engine",status="success",message="流程已完成")
    return(list(success=TRUE, message="流程已完成", completed=TRUE, instance_id=instance_id))
  }
  # 获取上下文
  context_data <- tryCatch(jsonlite::fromJSON(instance$context_data[1],simplifyVector=FALSE),error=function(e)list())
  # 查找出线
  next_node_id <- NULL
  for (t in definition$transitions) {
    if (!is.null(t$from)&&t$from==current_node_id) {
      cond <- t$condition%||%""
      if (nchar(cond)==0||evaluate_condition(cond,context_data)) { next_node_id<-t$to; break }
    }
  }
  if (is.null(next_node_id)) {
    process_log_write(instance_id,current_node_id,"warn","transition","无匹配条件分支")
    return(list(success=FALSE, message="无匹配条件分支"))
  }
  # 查找目标节点
  next_node_def <- NULL
  for (n in definition$nodes) { if (!is.null(n$id)&&n$id==next_node_id) { next_node_def<-n; break } }
  if (is.null(next_node_def)) return(list(success=FALSE, message=sprintf("目标节点 %s 未定义",next_node_id)))
  # 标记当前节点完成
  con <- db_connect()
  tryCatch({ dbExecute(con, sprintf("UPDATE process_nodes SET status='completed',completed_at=datetime('now','localtime') WHERE instance_id=%d AND node_id='%s' AND status='active'",as.integer(instance_id),current_node_id))
  }, finally={ db_disconnect(con) })
  # 激活下一个节点
  node_label <- next_node_def$label%||%next_node_id
  process_activate_node(instance_id, next_node_def)
  process_log_write(instance_id,next_node_id,"info","node_enter",sprintf("进入节点: %s (%s)",node_label,next_node_def$type))
  process_event_record("node_activate",instance_id,next_node_id,source="engine",status="success",message=sprintf("激活节点: %s",node_label))
  # 自动节点：执行适配器调用，再推进
  if (next_node_def$type=="auto") {
    process_execute_auto_node_now(instance_id)
    return(list(success=TRUE, message=sprintf("自动节点「%s」已执行",node_label)))
  }
  # 结束节点：推进
  if (next_node_def$type=="end") return(process_advance(instance_id))
  # 条件节点：直接评估并推进
  if (next_node_def$type=="condition") {
    process_log_write(instance_id,next_node_id,"info","condition_eval","评估条件节点，自动推进")
    return(process_advance(instance_id))
  }
  list(success=TRUE, message=sprintf("已流转到「%s」（%s）",node_label,next_node_def$type),
       next_node=next_node_id, next_node_type=next_node_def$type, next_node_label=node_label)
}

##################
# 条件表达式评估
##################

evaluate_condition <- function(condition_str, context) {
  if (is.null(condition_str)||nchar(condition_str)==0||condition_str=="true") return(TRUE)
  sandbox_env <- new.env(parent=emptyenv())
  sandbox_env$context <- context
  if (!is.null(context$result)) sandbox_env$result <- context$result
  tryCatch(isTRUE(eval(parse(text=condition_str),envir=sandbox_env)), error=function(e)FALSE)
}

##################
# 超时检测
##################

process_check_timeouts <- function() {
  con <- db_connect()
  tryCatch({
    overdue <- dbGetQuery(con, "
      SELECT pn.id as node_id, pn.instance_id, pn.node_id as node_def_id, pn.node_name, pn.node_type,
             pn.timeout_minutes, pn.timeout_action, pn.entered_at,
             pi.instance_no, pi.title
      FROM process_nodes pn
      JOIN process_instances pi ON pn.instance_id = pi.id
      WHERE pn.status = 'active'
        AND pn.timeout_minutes > 0
        AND pn.entered_at IS NOT NULL
        AND (julianday('now') - julianday(pn.entered_at)) * 1440 > pn.timeout_minutes
    ")
    if (nrow(overdue) > 0) {
      for (i in 1:nrow(overdue)) {
        node <- overdue[i, ]
        process_log_write(node$instance_id, node$node_def_id, "warn", "timeout",
          sprintf("节点超时: %s (%d分钟)", node$node_name, node$timeout_minutes))
        process_event_record("node_timeout", node$instance_id, node$node_def_id,
          source="scheduler", status="success",
          message=sprintf("超时策略: %s", node$timeout_action))
        switch(node$timeout_action,
          "terminate" = process_instance_terminate(node$instance_id),
          "notify" = process_log_write(node$instance_id, node$node_def_id, "info", "timeout_notify", "超时通知（待实现）"),
          "skip" = process_advance(node$instance_id)
        )
      }
    }
    nrow(overdue)
  }, finally={ db_disconnect(con) })
}

##################
# 自动节点适配器执行
##################

process_execute_auto_node_now <- function(instance_id) {
  instance <- process_instance_get(instance_id)
  if (is.null(instance)) return(list(success=FALSE, message="实例不存在"))
  current_node_id <- instance$current_node[1]
  definition <- tryCatch(jsonlite::fromJSON(instance$definition[1],simplifyVector=FALSE),error=function(e)NULL)
  if (is.null(definition)||is.null(definition$nodes)) return(list(success=FALSE))
  node_def <- NULL
  for (n in definition$nodes) { if (!is.null(n$id)&&n$id==current_node_id) { node_def<-n; break } }
  if (is.null(node_def)||node_def$type!="auto") return(list(success=FALSE, message="当前节点不是自动节点"))
  start_time <- Sys.time()
  result <- list(success=TRUE, message="自动执行完成")
  # 调用适配器
  action_info <- node_def$action
  if (!is.null(action_info)) {
    if (is.character(action_info)) {
      action_info <- tryCatch(jsonlite::fromJSON(action_info,simplifyVector=FALSE),error=function(e)list(module="",method=""))
    }
    module <- action_info$module%||%""
    method <- action_info$method%||%""
    args <- action_info$args%||%list()
    if (nchar(module)>0 && nchar(method)>0) {
      result <- process_module_adapter_invoke(module, method, args)
    }
  }
  elapsed_ms <- as.integer(difftime(Sys.time(), start_time, units="secs")*1000)
  process_log_write(instance_id, current_node_id, "info", "auto_exec",
    sprintf("自动节点执行: %s", result$message), duration_ms=elapsed_ms)
  process_event_record("auto_exec", instance_id, current_node_id, source="engine",
    status=ifelse(result$success,"success","failure"), message=result$message)
  # 标记节点完成并推进
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE process_nodes SET result='%s',remark='%s',completed_at=datetime('now','localtime') WHERE instance_id=%d AND node_id='%s' AND status='active'",
      ifelse(result$success,"success","failed"),gsub("'","''",result$message%||%""),as.integer(instance_id),current_node_id))
  }, finally={ db_disconnect(con) })
  process_advance(instance_id)
  result
}

##################
# 查询
##################

process_get_todos <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT pn.id as node_instance_id,pn.node_name,pn.node_type,pn.entered_at,pn.timeout_minutes,
              pi.instance_no,pi.title as instance_title,pd.name as def_name,pi.id as instance_id
       FROM process_nodes pn JOIN process_instances pi ON pn.instance_id=pi.id
       JOIN process_definitions pd ON pi.def_id=pd.id
       WHERE pn.status='active' AND pn.node_type='task' AND (pn.assignee IS NULL OR pn.assignee=%d) AND pi.status='running'
       ORDER BY pn.entered_at DESC",as.integer(user_id)))
  }, finally={ db_disconnect(con) })
}

process_get_logs <- function(instance_id, limit=100) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_logs WHERE instance_id=%d ORDER BY created_at DESC LIMIT %d",as.integer(instance_id),as.integer(limit)))
  }, finally={ db_disconnect(con) })
}

process_get_events <- function(instance_id, limit=100) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_events WHERE instance_id=%d ORDER BY created_at DESC LIMIT %d",as.integer(instance_id),as.integer(limit)))
  }, finally={ db_disconnect(con) })
}

##################
# 模块适配器框架
##################

module_adapters <- list()

process_register_module_adapter <- function(module_type, adapter) {
  module_adapters[[module_type]] <<- adapter
}

process_module_adapter_invoke <- function(module_type, method, args = list()) {
  adapter <- module_adapters[[module_type]]
  if (is.null(adapter)) return(list(success=FALSE, message=sprintf("模块适配器未注册: %s",module_type)))
  func <- adapter[[method]]
  if (is.null(func)) return(list(success=FALSE, message=sprintf("适配器方法未实现: %s.%s",module_type,method)))
  do.call(func, args)
}

register_builtin_adapters <- function() {
  process_register_module_adapter("work_order", list(
    get = function(id) work_order_get_by_id(id),
    create = function(title, description="", category="故障", priority="normal", request_user="", creator=NULL) {
      result <- work_order_add(title=title, description=description, category=category,
                                priority=priority, request_user=request_user, creator=creator)
      if (result$success) list(success=TRUE, id=result$id, no=result$order_no) else result
    },
    update = function(id, ...) work_order_edit(id, list(...)),
    get_status = function(id) { wo <- work_order_get_by_id(id); if(is.null(wo))NULL else wo$status }
  ))
  process_register_module_adapter("project", list(
    get = function(id) project_get_by_id(id),
    get_status = function(id) { p <- project_get_by_id(id); if(is.null(p))NULL else p$status[1] }
  ))
  cat("流程引擎：内置模块适配器已注册（工单/项目）\n")
}

# 初始化标志：确保注册只执行一次
process_adapters_initialized <- FALSE

##################
# 流程状态统计
##################

process_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances")$c[1]
    running <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances WHERE status='running'")$c[1]
    completed <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances WHERE status='completed'")$c[1]
    terminated <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances WHERE status='terminated'")$c[1]
    list(total=total, running=running, completed=completed, terminated=terminated, defs=nrow(process_def_list()))
  }, finally={ db_disconnect(con) })
}

#' 监控指标
process_get_monitor_metrics <- function() {
  con <- db_connect()
  tryCatch({
    # 完成率
    total <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances")$c[1]
    completed <- if (total>0) dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances WHERE status='completed'")$c[1] else 0
    complete_rate <- if (total>0) round(completed/total*100,1) else 0
    # 超时率
    timed_out <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_events WHERE event_type='node_timeout'")$c[1]
    total_events <- dbGetQuery(con, "SELECT COUNT(*) as c FROM process_events")$c[1]
    timeout_rate <- if (total_events>0) round(timed_out/total_events*100,1) else 0
    # 平均耗时
    avg_duration <- dbGetQuery(con, "SELECT AVG((julianday(completed_at)-julianday(started_at))*1440) as avg_min FROM process_instances WHERE status='completed' AND completed_at IS NOT NULL")$avg_min[1]
    if (is.na(avg_duration)) avg_duration <- 0
    # 今日启动/完成
    today <- format(Sys.Date(),"%Y-%m-%d")
    today_started <- dbGetQuery(con, sprintf("SELECT COUNT(*) as c FROM process_instances WHERE started_at LIKE '%s%%'",today))$c[1]
    today_completed <- dbGetQuery(con, sprintf("SELECT COUNT(*) as c FROM process_instances WHERE completed_at LIKE '%s%%'",today))$c[1]
    # 各类型节点执行次数
    node_counts <- dbGetQuery(con, "SELECT node_type,COUNT(*) as cnt FROM process_nodes GROUP BY node_type ORDER BY cnt DESC")
    list(
      complete_rate=complete_rate, timeout_rate=timeout_rate,
      avg_duration_min=round(avg_duration,1),
      today_started=today_started, today_completed=today_completed,
      total_instances=total, running_instances=dbGetQuery(con,"SELECT COUNT(*) as c FROM process_instances WHERE status='running'")$c[1],
      node_counts=node_counts
    )
  }, finally={ db_disconnect(con) })
}

#' 定义编辑辅助：从简单配置生成JSON
process_build_definition <- function(nodes_config) {
  # nodes_config: list of node specs
  # 每个 node: list(id, type, label, timeout_minutes=0, form_fields=NULL)
  # 自动连接：按顺序 start -> ... -> end
  nodes <- list()
  transitions <- list()
  for (i in seq_along(nodes_config)) {
    n <- nodes_config[[i]]
    node <- list(id=n$id, type=n$type, label=n$label)
    if (!is.null(n$timeout_minutes)&&n$timeout_minutes>0) node$timeout_minutes <- n$timeout_minutes
    if (!is.null(n$timeout_action)) node$timeout_action <- n$timeout_action
    if (!is.null(n$form_fields)) node$form <- list(fields=n$form_fields)
    if (!is.null(n$action)) node$action <- n$action
    nodes[[i]] <- node
    if (i > 1) {
      transitions[[i-1]] <- list(from=nodes_config[[i-1]]$id, to=n$id, condition="")
    }
  }
  jsonlite::toJSON(list(nodes=nodes, transitions=transitions), auto_unbox=TRUE, pretty=TRUE)
}

##################
# 表单模板管理
##################

# 模板 CRUD
form_template_list <- function(category = NULL) {
  con <- db_connect()
  tryCatch({
    where <- "WHERE 1=1"
    if (!is.null(category) && nchar(category)>0) where <- paste0(where, sprintf(" AND category='%s'",category))
    dbGetQuery(con, sprintf("SELECT * FROM process_form_templates %s ORDER BY updated_at DESC", where))
  }, finally={ db_disconnect(con) })
}

form_template_get <- function(template_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_form_templates WHERE id=%d", as.integer(template_id)))
  }, finally={ db_disconnect(con) })
}

form_template_create <- function(name, description = "", category = "general", created_by = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO process_form_templates (name,description,category,created_by) VALUES ('%s','%s','%s',%s)",
      gsub("'","''",name),gsub("'","''",description),category,
      ifelse(is.null(created_by),"NULL",as.character(created_by))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success=TRUE, id=id, message=sprintf("表单模板「%s」创建成功",name))
  }, error=function(e) list(success=FALSE, message=paste("创建失败:",e$message)),
  finally={ db_disconnect(con) })
}

form_template_update <- function(template_id, name, description, category) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE process_form_templates SET name='%s',description='%s',category='%s',updated_at=datetime('now','localtime') WHERE id=%d",
      gsub("'","''",name),gsub("'","''",description),category,as.integer(template_id)))
    list(success=TRUE, message="更新成功")
  }, error=function(e) list(success=FALSE, message=paste("更新失败:",e$message)),
  finally={ db_disconnect(con) })
}

form_template_delete <- function(template_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM process_form_template_fields WHERE template_id=%d", as.integer(template_id)))
    dbExecute(con, sprintf("DELETE FROM process_form_templates WHERE id=%d", as.integer(template_id)))
    list(success=TRUE, message="已删除")
  }, error=function(e) list(success=FALSE, message=paste("删除失败:",e$message)),
  finally={ db_disconnect(con) })
}

# 字段管理
form_template_get_fields <- function(template_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM process_form_template_fields WHERE template_id=%d ORDER BY sort_order,id", as.integer(template_id)))
  }, finally={ db_disconnect(con) })
}

form_template_add_field <- function(template_id, field_key, field_label, field_type = "text", field_options = NULL, required = FALSE, sort_order = 0, default_value = "") {
  con <- db_connect()
  tryCatch({
    options_json <- if (is.null(field_options)) "NULL" else sprintf("'%s'", gsub("'","''",jsonlite::toJSON(field_options,auto_unbox=TRUE)))
    dbExecute(con, sprintf("INSERT INTO process_form_template_fields (template_id,field_key,field_label,field_type,field_options,required,sort_order,default_value) VALUES (%d,'%s','%s','%s',%s,%d,%d,'%s')",
      as.integer(template_id),gsub("'","''",field_key),gsub("'","''",field_label),field_type,
      options_json,as.integer(required),as.integer(sort_order),gsub("'","''",default_value)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success=TRUE, id=id, message=sprintf("字段「%s」已添加",field_label))
  }, error=function(e) list(success=FALSE, message=paste("添加字段失败:",e$message)),
  finally={ db_disconnect(con) })
}

form_template_remove_field <- function(field_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM process_form_template_fields WHERE id=%d", as.integer(field_id)))
    list(success=TRUE, message="字段已移除")
  }, error=function(e) list(success=FALSE, message=paste("移除失败:",e$message)),
  finally={ db_disconnect(con) })
}

# 模板 → JSON 生成
form_template_to_json <- function(template_id) {
  tpl <- form_template_get(template_id)
  fields <- form_template_get_fields(template_id)
  if (nrow(fields)==0) return("")
  field_list <- list()
  for (i in 1:nrow(fields)) {
    f <- fields[i, ]
    fi <- list(key=f$field_key, label=f$field_label, type=f$field_type, required=as.logical(f$required))
    if (!is.null(f$field_options) && !is.na(f$field_options) && nchar(f$field_options)>0) {
      opts <- tryCatch(jsonlite::fromJSON(f$field_options), error=function(e) NULL)
      if (!is.null(opts)) fi$options <- opts
    }
    if (!is.null(f$default_value) && !is.na(f$default_value) && nchar(f$default_value)>0) fi$default <- f$default_value
    field_list[[i]] <- fi
  }
  jsonlite::toJSON(list(fields=field_list), auto_unbox=TRUE, pretty=TRUE)
}
##################

#' 简单审批：开始 → 审批 → 结束
process_create_demo_simple <- function(created_by = NULL) {
  process_create_demo_def("简单审批", "开始→审批→结束", category="审批", created_by=created_by)
}

#' 条件分支：开始 → 审批 → [条件] → 同意→通知→结束 / 驳回→结束
process_create_demo_condition <- function(created_by = NULL) {
  process_create_demo_def("条件分支审批", "开始→审批→条件判断→同意/驳回", category="审批", created_by=created_by,
    demo_type = "condition")
}

#' 自动创建工单：开始 → 创建工单(自动) → 审批 → 结束
process_create_demo_auto <- function(created_by = NULL) {
  process_create_demo_def("自动工单流程", "开始→创建工单→审批→结束", category="工单", created_by=created_by,
    demo_type = "auto")
}

process_create_demo_def <- function(name, description, category, created_by = NULL, demo_type = "simple") {
  if (demo_type == "condition") {
    nodes <- list(
      list(id="start", type="start", label="开始"),
      list(id="approve", type="task", label="审批确认", timeout_minutes=1440,
           form=list(fields=list(
             list(key="result", label="审批意见", type="select", options=list("同意","驳回"), required=TRUE),
             list(key="remark", label="审批备注", type="textarea")
           ))),
      list(id="condition", type="condition", label="审批判断"),
      list(id="notify", type="auto", label="发送通知"),
      list(id="reject_end", type="end", label="已驳回"),
      list(id="end", type="end", label="结束")
    )
    transitions <- list(
      list(from="start", to="approve", condition=""),
      list(from="approve", to="condition", condition=""),
      list(from="condition", to="notify", condition="result=='同意'"),
      list(from="condition", to="reject_end", condition="result=='驳回'"),
      list(from="notify", to="end", condition="")
    )
  } else if (demo_type == "auto") {
    nodes <- list(
      list(id="start", type="start", label="开始"),
      list(id="auto_create", type="auto", label="自动创建工单"),
      list(id="approve", type="task", label="审批工单", timeout_minutes=1440),
      list(id="end", type="end", label="结束")
    )
    transitions <- list(
      list(from="start", to="auto_create", condition=""),
      list(from="auto_create", to="approve", condition=""),
      list(from="approve", to="end", condition="")
    )
  } else {
    nodes <- list(
      list(id="start", type="start", label="开始"),
      list(id="approve", type="task", label="审批确认", timeout_minutes=1440),
      list(id="end", type="end", label="结束")
    )
    transitions <- list(
      list(from="start", to="approve", condition=""),
      list(from="approve", to="end", condition="")
    )
  }
  json <- jsonlite::toJSON(list(nodes=nodes, transitions=transitions), auto_unbox=TRUE, pretty=TRUE)
  result <- process_def_create(name, description, category=category, definition=json, created_by=created_by)
  if (result$success) process_def_publish(result$id, change_log="自动发布")
  result
}

#' 一键创建并启动（指定类型）
process_create_and_start <- function(demo_type = "simple", started_by = NULL) {
  def_result <- switch(demo_type,
    "condition" = process_create_demo_condition(created_by = started_by),
    "auto" = process_create_demo_auto(created_by = started_by),
    process_create_demo_simple(created_by = started_by))
  if (!def_result$success) return(def_result)
  inst_result <- process_instance_start(def_id=def_result$id,
    title=sprintf("示例-%s",switch(demo_type,"simple"="简单审批","condition"="条件分支","auto"="自动工单")),
    context_data=list(title="示例",priority="normal"), started_by=started_by)
  if (!inst_result$success) return(inst_result)
  advance <- process_advance(inst_result$id)
  list(success=TRUE, def_id=def_result$id, def_no=def_result$def_no,
       instance_id=inst_result$id, instance_no=inst_result$instance_no,
       advance=advance, def_name=def_result$def_no,
       demo_type=demo_type,
       message=sprintf("已创建并启动「%s」", switch(demo_type,"simple"="简单审批","condition"="条件分支审批","auto"="自动工单流程")))
}

##################
# 状态标签
##################

process_status_label <- function(status) {
  labels <- c("running"="运行中","completed"="已完成","terminated"="已终止","suspended"="已暂停",
              "draft"="草稿","published"="已发布","archived"="已归档",
              "active"="进行中","pending"="待处理","skipped"="已跳过","failed"="失败","timeout"="超时",
              "task"="任务","auto"="自动","condition"="条件","start"="开始","end"="结束")
  result <- labels[status]
  result[is.na(result)] <- status[is.na(result)]
  unname(result)
}

# 注册内置适配器（在 server.R 中调用，避免 source 时执行）
