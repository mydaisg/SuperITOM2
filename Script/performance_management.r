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

perf_work_item_update <- function(item_id, indicator_code = NULL, deduction_level = NULL, source_title = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(indicator_code)) sets <- c(sets, sprintf("indicator_code='%s'", indicator_code))
    if (!is.null(deduction_level)) sets <- c(sets, sprintf("deduction_level=%d", as.integer(deduction_level)))
    if (!is.null(source_title)) sets <- c(sets, sprintf("source_title='%s'", gsub("'","''", source_title)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE performance_work_items SET %s WHERE id=%d",
      paste(sets, collapse = ", "), as.integer(item_id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
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
# 计分计算（employees=NULL 时自动从 items 提取全部员工）
##################
perf_calculate <- function(sheet_id, employees = NULL) {
  items <- perf_work_items_by_sheet(sheet_id)
  # 确定显示哪些员工
  if (!is.null(employees) && nrow(employees) > 0) {
    emp_list <- employees
  } else if (nrow(items) > 0) {
    emp_list <- unique(items[, c("employee_id", "employee_name")])
  } else {
    return(list(matrix = data.frame(), summary = data.frame()))
  }
  emp_list <- emp_list[order(emp_list$employee_name), ]

  # ---- 矩阵表：每指标每员工计数+计分 ----
  calc_score <- function(emp_id) {  # 返回 c(A,B,C,总分,indicator_scores)
    emp_items <- if (nrow(items) > 0) items[items$employee_id == emp_id, ] else data.frame()
    scores <- c(A = 0, B = 0, C = 0)
    a_deduct <- 0
    ind_scores <- numeric(length(PERF_INDICATORS))
    names(ind_scores) <- sapply(PERF_INDICATORS, `[[`, "code")
    for (i in seq_along(PERF_INDICATORS)) {
      ind <- PERF_INDICATORS[[i]]
      ind_it <- if (nrow(emp_items) > 0) emp_items[emp_items$indicator_code == ind$code, ] else data.frame()
      count <- nrow(ind_it)
      if (ind$scoring == "deduct") {
        if (count > 0) {
          ds <- sum(sapply(ind_it$deduction_level, function(l) {
            for (dl in ind$deduct_levels) if (dl$level == l) return(dl$points)
            0
          }))
          a_deduct <- a_deduct + ds
          ind_scores[i] <- -ds
        }
      } else {
        sc <- min(count * ind$unit_score, ind$max_count * ind$unit_score)
        scores[ind$category] <- scores[ind$category] + sc
        ind_scores[i] <- sc
      }
    }
    scores["A"] <- max(0, 30 - a_deduct)
    total <- sum(scores)
    c(scores, 总分 = total, 绩效总分 = min(total, 100), ind_scores)
  }

  result <- list(); row_idx <- 1
  for (i in seq_along(PERF_INDICATORS)) {
    ind <- PERF_INDICATORS[[i]]
    ind_items <- if (nrow(items) > 0) items[items$indicator_code == ind$code, ] else items
    row <- list(category = ind$category, indicator = sprintf("%s-%s", ind$code, ind$name), code = ind$code)
    col_score <- 0
    for (ei in seq_len(nrow(emp_list))) {
      eid <- emp_list$employee_id[ei]; nm <- emp_list$employee_name[ei]
      eitems <- if (nrow(ind_items) > 0) ind_items[ind_items$employee_id == eid, ] else data.frame()
      cnt <- as.integer(nrow(eitems))
      row[[nm]] <- cnt
      # 计每指标每人得分
      if (ind$scoring == "add") {
        col_score <- col_score + min(cnt * ind$unit_score, ind$max_count * ind$unit_score)
      } else if (cnt > 0) {
        ds <- sum(sapply(eitems$deduction_level, function(l) {
          for (dl in ind$deduct_levels) if (dl$level == l) return(dl$points)
          0
        }))
        col_score <- col_score - ds
      }
    }
    row[["计分"]] <- col_score
    result[[row_idx]] <- row; row_idx <- row_idx + 1
  }

  # A/B/C 分类得分行
  score_label <- c(A = "A类得分（30分）", B = "B类得分（40分）", C = "C类得分（30分）")
  for (cn in c("A", "B", "C")) {
    r <- list(category = "", indicator = score_label[cn], code = "")
    col_sum <- 0
    for (ei in seq_len(nrow(emp_list))) {
      nm <- emp_list$employee_name[ei]; sc <- calc_score(emp_list$employee_id[ei])
      v <- sc[cn]; r[[nm]] <- v; col_sum <- col_sum + v
    }
    r[["计分"]] <- col_sum
    result[[row_idx]] <- r; row_idx <- row_idx + 1
  }

  # 总分行
  score_row <- list(category = "", indicator = "总分", code = "")
  total_sum <- 0
  for (ei in seq_len(nrow(emp_list))) {
    nm <- emp_list$employee_name[ei]; sc <- calc_score(emp_list$employee_id[ei])
    v <- sc["总分"]; score_row[[nm]] <- v; total_sum <- total_sum + v
  }
  score_row[["计分"]] <- total_sum
  result[[row_idx]] <- score_row; row_idx <- row_idx + 1

  # 绩效总分行（封顶100）
  perf_row <- list(category = "", indicator = "绩效总分", code = "")
  perf_sum <- 0
  for (ei in seq_len(nrow(emp_list))) {
    nm <- emp_list$employee_name[ei]; sc <- calc_score(emp_list$employee_id[ei])
    v <- sc["绩效总分"]; perf_row[[nm]] <- v; perf_sum <- perf_sum + v
  }
  perf_row[["计分"]] <- perf_sum
  result[[row_idx]] <- perf_row

  matrix_df <- do.call(rbind, lapply(result, function(r) as.data.frame(r, stringsAsFactors = FALSE)))

  # ---- 按人头分ABC类统计（复用 calc_score）----
  summary_rows <- list()
  for (ei in seq_len(nrow(emp_list))) {
    emp_name <- emp_list$employee_name[ei]
    sc <- calc_score(emp_list$employee_id[ei])
    summary_rows[[ei]] <- data.frame(
      员工 = emp_name,
      "A类得分（30分）" = sc["A"],
      "B类得分（40分）" = sc["B"],
      "C类得分（30分）" = sc["C"],
      "总分" = sc["总分"],
      "绩效总分" = sc["绩效总分"],
      stringsAsFactors = FALSE, check.names = FALSE)
  }
  summary_df <- do.call(rbind, summary_rows)
  if (nrow(summary_df) == 0) summary_df <- data.frame(信息 = "暂无数据", stringsAsFactors = FALSE)

  list(matrix = matrix_df, summary = summary_df)
}

##################
# 绩效表内员工（手动添加，独立管理）
##################
perf_sheet_employee_add <- function(sheet_id, employee_ids) {
  con <- db_connect()
  tryCatch({
    added <- 0
    for (eid in employee_ids) {
      dbExecute(con, sprintf("INSERT OR IGNORE INTO performance_sheet_employees (sheet_id, employee_id) VALUES (%d, %d)",
        as.integer(sheet_id), as.integer(eid)))
      added <- added + 1
    }
    list(success = TRUE, message = sprintf("已添加 %d 位员工", added), count = added)
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

perf_sheet_employee_list <- function(sheet_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT pse.*, u.display_name, u.username
       FROM performance_sheet_employees pse
       LEFT JOIN users u ON pse.employee_id = u.id
       WHERE pse.sheet_id = %d ORDER BY u.display_name", as.integer(sheet_id)))
  }, error = function(e) data.frame(),
  finally = { db_disconnect(con) })
}

perf_sheet_employee_remove <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM performance_sheet_employees WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已移除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 活跃员工列表（优先从绩效表内员工读取，否则回退到全量活跃用户）
##################
perf_active_employees <- function(year_month) {
  con <- db_connect()
  tryCatch({
    sheet <- dbGetQuery(con, sprintf("SELECT id FROM performance_sheets WHERE year_month='%s'", year_month))
    if (nrow(sheet) > 0) {
      # 从表内员工读取
      users <- dbGetQuery(con, sprintf(
        "SELECT DISTINCT u.id, u.display_name, u.username
         FROM performance_sheet_employees pse
         JOIN users u ON pse.employee_id = u.id
         WHERE pse.sheet_id = %d ORDER BY u.display_name", sheet$id[1]))
      if (nrow(users) > 0) {
        users$employee_name <- ifelse(is.na(users$display_name) | users$display_name == "", users$username, users$display_name)
        return(users)
      }
    }
    # 兜底：无表内员工时从工单/任务/巡检获取活跃用户
    ym_parts <- as.integer(strsplit(year_month, "-")[[1]])
    if (ym_parts[2] == 12) { next_ym <- sprintf("%d-01", ym_parts[1] + 1) } else { next_ym <- sprintf("%d-%02d-01", ym_parts[1], ym_parts[2] + 1) }
    start_date <- paste0(year_month, "-01")
    users <- dbGetQuery(con, sprintf("
      SELECT DISTINCT u.id,u.display_name,u.username FROM users u WHERE u.active=1 AND (
        EXISTS(SELECT 1 FROM work_orders wo WHERE wo.assigned_to=u.id AND wo.created_at>='%s' AND wo.created_at<'%s')
        OR EXISTS(SELECT 1 FROM project_tasks pt WHERE pt.assigned_to=u.id AND pt.created_at>='%s' AND pt.created_at<'%s')
        OR EXISTS(SELECT 1 FROM inspection_tasks it WHERE it.inspector=u.id AND it.updated_at>='%s' AND it.updated_at<'%s')
      ) ORDER BY u.display_name", start_date, next_ym, start_date, next_ym, start_date, next_ym))
    if (nrow(users) > 0) {
      users$employee_name <- ifelse(is.na(users$display_name) | users$display_name == "", users$username, users$display_name)
    }
    users
  }, finally = { db_disconnect(con) })
}
