# 绩效管理 - 数据层
# 月绩效表：指标×员工矩阵 + 工作清单匹配

##################
# 固定指标定义
##################
PERF_INDICATORS <- list(
  list(code="A1", category="A", name="用户投诉", scoring="deduct", max_score=30,
    deduct_levels=list(list(level=1,label="普通投诉",points=1),list(level=2,label="严重投诉",points=3),list(level=3,label="重大投诉",points=5))),
  list(code="A2", category="A", name="公司高管投诉", scoring="deduct", max_score=30,
    deduct_levels=list(list(level=1,label="一般投诉",points=2),list(level=2,label="较严重",points=4),list(level=3,label="非常严重",points=6))),
  list(code="A3", category="A", name="违规违纪", scoring="deduct", max_score=30,
    deduct_levels=list(list(level=1,label="轻度违规",points=3),list(level=2,label="中度违规",points=5),list(level=3,label="严重违规",points=10))),
  list(code="B4", category="B", name="主导或参与IT项目", scoring="add", unit_score=8, max_count=5),
  list(code="B5", category="B", name="主导或参与IT合规/持续改进", scoring="add", unit_score=8, max_count=5),
  list(code="B6", category="B", name="制订《标准》/《方案》/《SOP》", scoring="add", unit_score=8, max_count=5),
  list(code="B7", category="B", name="发现和解决《问题》", scoring="add", unit_score=8, max_count=5),
  list(code="B8", category="B", name="创造或引入生产力工具", scoring="add", unit_score=8, max_count=5),
  list(code="C9", category="C", name="业务需求", scoring="add", unit_score=5, max_count=6),
  list(code="C10", category="C", name="管理需求", scoring="add", unit_score=5, max_count=6)
)

perf_indicators <- function() PERF_INDICATORS
perf_indicator_get <- function(code) {
  for (ind in PERF_INDICATORS) { if (ind$code==code) return(ind) }
  NULL
}

##################
# 月表管理
##################
perf_sheet_create <- function(year_month) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT OR IGNORE INTO performance_sheets (year_month) VALUES ('%s')",year_month))
    id <- dbGetQuery(con, sprintf("SELECT id FROM performance_sheets WHERE year_month='%s'",year_month))$id[1]
    list(success=TRUE, id=id, message=sprintf("绩效表 %s 已创建",year_month))
  }, error=function(e) list(success=FALSE, message=paste("创建失败:",e$message)),
  finally={ db_disconnect(con) })
}

perf_sheet_list <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM performance_sheets ORDER BY year_month DESC")
  }, error = function(e) {
    message("[perf] 获取绩效表列表失败: ", e$message)
    data.frame()
  }, finally = {
    db_disconnect(con)
  })
}

perf_sheet_get <- function(sheet_id) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf("SELECT * FROM performance_sheets WHERE id=%d",as.integer(sheet_id)))
    if (nrow(result)==0) NULL else result
  }, finally={ db_disconnect(con) })
}

perf_sheet_get_by_month <- function(year_month) {
  con <- db_connect()
  tryCatch({
    result <- dbGetQuery(con, sprintf("SELECT * FROM performance_sheets WHERE year_month='%s'",year_month))
    if (nrow(result)==0) NULL else result
  }, finally={ db_disconnect(con) })
}

##################
# 工作清单管理
##################
perf_work_item_add <- function(sheet_id, employee_id, indicator_code, source_type=NULL, source_id=NULL, source_title=NULL, deduction_level=0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO performance_work_items (sheet_id,employee_id,indicator_code,source_type,source_id,source_title,deduction_level) VALUES (%d,%d,'%s',%s,%s,'%s',%d)",
      as.integer(sheet_id),as.integer(employee_id),indicator_code,
      ifelse(is.null(source_type)||source_type=="","NULL",sprintf("'%s'",source_type)),
      ifelse(is.null(source_id)||is.na(source_id),"NULL",as.character(as.integer(source_id))),
      gsub("'","''",source_title%||%""),as.integer(deduction_level)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success=TRUE, id=id, message="已添加")
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

perf_work_item_remove <- function(item_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM performance_work_items WHERE id=%d",as.integer(item_id)))
  }, finally={ db_disconnect(con) })
}

perf_work_items_by_sheet <- function(sheet_id) {
  con <- db_connect()
  tryCatch({
    items <- dbGetQuery(con, sprintf(
      "SELECT pwi.*,u.display_name,u.username FROM performance_work_items pwi LEFT JOIN users u ON pwi.employee_id=u.id WHERE pwi.sheet_id=%d ORDER BY pwi.employee_id,pwi.indicator_code",as.integer(sheet_id)))
    if (nrow(items)>0) {
      # 添加指标名称列
      ind_names <- sapply(items$indicator_code, function(code) {
        ind <- perf_indicator_get(code)
        if (is.null(ind)) code else sprintf("%s-%s",code,ind$name)
      })
      items$indicator_name <- ind_names
      # 员工显示名
      items$employee_name <- ifelse(is.na(items$display_name)%||%items$display_name=="", items$username, items$display_name)
    }
    items
  }, finally={ db_disconnect(con) })
}

##################
# 单项得分计算
##################
perf_item_score <- function(indicator_code, deduction_level=0) {
  ind <- perf_indicator_get(indicator_code)
  if (is.null(ind)) return(0)
  if (ind$scoring=="deduct") {
    for (dl in ind$deduct_levels) {
      if (dl$level==deduction_level) return(-dl$points)
    }
    return(0)
  } else {
    return(ind$unit_score%||%5)
  }
}

##################
# 从各模块加载本月工作清单
##################
perf_load_work_sources <- function(year_month) {
  con <- db_connect()
  tryCatch({
    start_date <- paste0(year_month,"-01")
    # 计算下个月第一天
    ym_parts <- as.integer(strsplit(year_month,"-")[[1]])
    if (ym_parts[2]==12) { next_ym <- sprintf("%d-01",ym_parts[1]+1) } else { next_ym <- sprintf("%d-%02d-01",ym_parts[1],ym_parts[2]+1) }
    
    # 1. 工单：本月创建的、有处理人的
    work_orders <- tryCatch(
      dbGetQuery(con, sprintf(
        "SELECT '工单' as source_type,wo.id as source_id,wo.title as source_title,
                wo.order_no as source_no,wo.assigned_to as employee_id,
                u.display_name,u.username
         FROM work_orders wo LEFT JOIN users u ON wo.assigned_to=u.id
         WHERE wo.assigned_to IS NOT NULL
           AND wo.created_at >= '%s' AND wo.created_at < '%s'
         ORDER BY wo.created_at DESC", start_date, next_ym)),
      error=function(e) data.frame())
    if (nrow(work_orders)>0) work_orders$employee_name <- ifelse(is.na(work_orders$display_name)%||%work_orders$display_name=="", work_orders$username, work_orders$display_name)
    
    # 2. 项目任务：本月创建的、有分配人的
    tasks <- tryCatch(
      dbGetQuery(con, sprintf(
        "SELECT '项目任务' as source_type,pt.id as source_id,pt.name as source_title,
                pt.task_no as source_no,pt.assigned_to as employee_id,
                u.display_name,u.username
         FROM project_tasks pt LEFT JOIN users u ON pt.assigned_to=u.id
         WHERE pt.assigned_to IS NOT NULL
           AND pt.created_at >= '%s' AND pt.created_at < '%s'
         ORDER BY pt.created_at DESC", start_date, next_ym)),
      error=function(e) data.frame())
    if (nrow(tasks)>0) tasks$employee_name <- ifelse(is.na(tasks$display_name)%||%tasks$display_name=="", tasks$username, tasks$display_name)
    
    # 3. 巡检任务：本月已执行的
    insp <- tryCatch(
      dbGetQuery(con, sprintf(
        "SELECT '巡检' as source_type,it.id as source_id,ip.name as source_title,
                it.task_no as source_no,it.inspector as employee_id,
                u.display_name,u.username
         FROM inspection_tasks it
         JOIN inspection_plans ip ON it.plan_id=ip.id
         LEFT JOIN users u ON it.inspector=u.id
         WHERE it.inspector IS NOT NULL AND it.status='completed'
           AND it.updated_at >= '%s' AND it.updated_at < '%s'
         ORDER BY it.updated_at DESC", start_date, next_ym)),
      error=function(e) data.frame())
    if (nrow(insp)>0) insp$employee_name <- ifelse(is.na(insp$display_name)%||%insp$display_name=="", insp$username, insp$display_name)
    
    rbind(work_orders, tasks, insp)
  }, finally={ db_disconnect(con) })
}

##################
# 计分计算
##################
perf_calculate <- function(sheet_id) {
  items <- perf_work_items_by_sheet(sheet_id)
  if (nrow(items)==0) return(data.frame())
  # 按员工分组
  employees <- unique(items[,c("employee_id","employee_name")])
  employees <- employees[order(employees$employee_name), ]
  result <- list()
  row_idx <- 1
  for (ind in PERF_INDICATORS) {
    # 该指标下所有匹配项
    ind_items <- items[items$indicator_code==ind$code, ]
    row <- list(indicator=sprintf("%s-%s",ind$code,ind$name), category=ind$category, code=ind$code)
    for (ei in 1:nrow(employees)) {
      emp_id <- employees$employee_id[ei]
      emp_name <- employees$employee_name[ei]
      emp_items <- ind_items[ind_items$employee_id==emp_id, ]
      row[[emp_name]] <- nrow(emp_items)
    }
    result[[row_idx]] <- row
    row_idx <- row_idx + 1
  }
  # 总分行
  score_row <- list(indicator="总分", category="", code="")
  for (ei in 1:nrow(employees)) {
    emp_id <- employees$employee_id[ei]
    emp_name <- employees$employee_name[ei]
    emp_items <- items[items$employee_id==emp_id, ]
    total <- 0
    # 按指标统计
    for (ind in PERF_INDICATORS) {
      ind_items <- emp_items[emp_items$indicator_code==ind$code, ]
      count <- nrow(ind_items)
      if (ind$scoring=="deduct") {
        if (count>0) {
          # 累加扣分值
          deduct_sum <- sum(sapply(ind_items$deduction_level, function(lvl) {
            for (dl in ind$deduct_levels) { if (dl$level==lvl) return(dl$points) }
            0
          }))
          total <- total + max(0, ind$max_score - deduct_sum)
        }
      } else if (ind$scoring=="add") {
        score <- min(count * ind$unit_score, ind$max_count * ind$unit_score)
        total <- total + score
      }
    }
    score_row[[emp_name]] <- total
  }
  result[[row_idx]] <- score_row
  do.call(rbind, lapply(result, function(r) as.data.frame(r, stringsAsFactors=FALSE)))
}

##################
# 活跃员工列表（有本月工作记录的）
##################
perf_active_employees <- function(year_month) {
  ym_parts <- as.integer(strsplit(year_month,"-")[[1]])
  if (ym_parts[2]==12) { next_ym <- sprintf("%d-01",ym_parts[1]+1) } else { next_ym <- sprintf("%d-%02d-01",ym_parts[1],ym_parts[2]+1) }
  start_date <- paste0(year_month,"-01")
  con <- db_connect()
  tryCatch({
    # 从工单/任务/巡检中找到本月有活动的用户
    users <- dbGetQuery(con, sprintf("
      SELECT DISTINCT u.id,u.display_name,u.username FROM users u WHERE u.active=1 AND (
        EXISTS(SELECT 1 FROM work_orders wo WHERE wo.assigned_to=u.id AND wo.created_at>='%s' AND wo.created_at<'%s')
        OR EXISTS(SELECT 1 FROM project_tasks pt WHERE pt.assigned_to=u.id AND pt.created_at>='%s' AND pt.created_at<'%s')
        OR EXISTS(SELECT 1 FROM inspection_tasks it WHERE it.inspector=u.id AND it.updated_at>='%s' AND it.updated_at<'%s')
      ) ORDER BY u.display_name", start_date, next_ym, start_date, next_ym, start_date, next_ym))
    if (nrow(users)>0) {
      users$employee_name <- ifelse(is.na(users$display_name)%||%users$display_name=="", users$username, users$display_name)
    }
    users
  }, finally={ db_disconnect(con) })
}
