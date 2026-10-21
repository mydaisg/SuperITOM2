# OKR 管理 - 数据层
# 全球通用 OKR（Objectives and Key Results）框架
# 结构：周期(Cycle) → 目标(O) → 关键结果(KR) → 进度检查(Check-in)
# 打通已有数据：工单 / 项目任务 / 巡检 / 月度绩效

##################
# 常量定义
##################
OKR_LEVELS      <- c("公司" = "company", "团队" = "team", "个人" = "individual")
OKR_OBJ_STATUS  <- c("进行中" = "active", "已达成" = "achieved", "已结束" = "closed")
OKR_CONFIDENCE  <- c("已达成" = "achieved", "正常" = "on_track", "有风险" = "at_risk", "已偏离" = "off_track")
OKR_SOURCE_TYPES <- c("手工维护" = "manual", "工单" = "工单", "项目任务" = "项目任务", "巡检" = "巡检", "月度绩效" = "绩效")

okr_levels <- function() OKR_LEVELS
okr_obj_statuses <- function() OKR_OBJ_STATUS
okr_source_types <- function() OKR_SOURCE_TYPES

okr_level_label <- function(level) {
  idx <- match(level, OKR_LEVELS)
  if (is.na(idx)) level else names(OKR_LEVELS)[idx]
}
okr_obj_status_label <- function(status) {
  idx <- match(status, OKR_OBJ_STATUS)
  if (is.na(idx)) status else names(OKR_OBJ_STATUS)[idx]
}
okr_confidence_label <- function(progress) {
  s <- okr_kr_status(progress)
  idx <- match(s, OKR_CONFIDENCE)
  if (is.na(idx)) s else names(OKR_CONFIDENCE)[idx]
}

##################
# 内部 SQL 工具
##################
okr_text <- function(x) {
  if (length(x) == 0 || is.null(x)) return("NULL")
  if (is.na(x) || as.character(x) == "") return("NULL")
  sprintf("'%s'", gsub("'", "''", as.character(x)))
}
okr_num <- function(x) {
  if (length(x) == 0 || is.null(x)) return("NULL")
  if (is.na(x)) return("NULL")
  as.character(as.numeric(x))
}
okr_end_exclusive <- function(end_date) {
  d <- tryCatch(as.Date(end_date), error = function(e) NA)
  if (length(d) == 0 || is.na(d)) return("9999-12-31")
  as.character(d + 1)
}

##################
# 进度计算
##################
okr_kr_calc_progress <- function(baseline, target, current) {
  b <- if (length(baseline) == 0 || is.null(baseline) || is.na(baseline)) 0 else as.numeric(baseline)
  t <- if (length(target) == 0 || is.null(target) || is.na(target)) 0 else as.numeric(target)
  c <- if (length(current) == 0 || is.null(current) || is.na(current)) 0 else as.numeric(current)
  if (t == b) return(if (c == t) 100 else 0)
  p <- (c - b) / (t - b) * 100
  max(0, min(100, p))
}
okr_kr_status <- function(progress) {
  p <- if (length(progress) == 0 || is.null(progress) || is.na(progress)) 0 else as.numeric(progress)
  if (p >= 100) "achieved"
  else if (p >= 70) "on_track"
  else if (p >= 40) "at_risk"
  else "off_track"
}

##################
# 用户名称
##################
okr_user_name <- function(uid) {
  if (length(uid) == 0 || is.null(uid) || is.na(uid)) return("未指定")
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT display_name, username FROM users WHERE id=%d", as.integer(uid)))
    if (nrow(r) == 0) return(sprintf("用户#%d", as.integer(uid)))
    ifelse(is.na(r$display_name[1]) | r$display_name[1] == "", r$username[1], r$display_name[1])
  }, error = function(e) sprintf("用户#%d", as.integer(uid)),
  finally = { db_disconnect(con) })
}

##################
# 周期 (Cycle)
##################
okr_cycle_create <- function(year, quarter = 0, name = NULL, start_date = NULL, end_date = NULL) {
  if (is.null(name) || is.na(name) || name == "") {
    name <- if (as.integer(quarter) == 0) sprintf("%d年度", as.integer(year))
            else sprintf("%d Q%d", as.integer(year), as.integer(quarter))
  }
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO okr_cycles (name, year, quarter, start_date, end_date, status) VALUES (%s, %s, %s, %s, %s, 'active')",
      okr_text(name), okr_num(year), okr_num(quarter), okr_text(start_date), okr_text(end_date)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, message = sprintf("OKR周期 %s 已创建", name))
  }, error = function(e) list(success = FALSE, message = paste("创建失败:", e$message)),
  finally = { db_disconnect(con) })
}

okr_cycle_list <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM okr_cycles ORDER BY year DESC, quarter DESC, id DESC")
  }, error = function(e) {
    message("[okr] 获取周期列表失败: ", e$message)
    data.frame()
  }, finally = { db_disconnect(con) })
}

okr_cycle_get <- function(cycle_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM okr_cycles WHERE id=%d", as.integer(cycle_id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

# 当前默认周期：优先当前季度/年度 active，否则最新 active，否则最新
okr_cycle_active <- function() {
  cycles <- okr_cycle_list()
  if (nrow(cycles) == 0) return(NULL)
  today <- Sys.Date()
  y <- as.integer(format(today, "%Y"))
  q <- (as.integer(format(today, "%m")) - 1) %/% 3 + 1
  hit <- cycles[cycles$year == y & cycles$quarter == q & cycles$status == "active", , drop = FALSE]
  if (nrow(hit) > 0) return(hit[1, , drop = FALSE])
  act <- cycles[cycles$status == "active", , drop = FALSE]
  if (nrow(act) > 0) return(act[1, , drop = FALSE])
  cycles[1, , drop = FALSE]
}

##################
# 目标 (Objective)
##################
okr_objective_add <- function(cycle_id, title, description = NULL, level = "team",
                              owner_id = NULL, parent_id = NULL, weight = 1) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO okr_objectives (cycle_id, title, description, level, owner_id, parent_id, weight, status)
       VALUES (%d, %s, %s, %s, %s, %s, %s, 'active')",
      as.integer(cycle_id), okr_text(title), okr_text(description), okr_text(level),
      okr_num(owner_id), okr_num(parent_id), okr_num(weight)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, message = "目标已创建")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

okr_objective_get <- function(objective_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM okr_objectives WHERE id=%d", as.integer(objective_id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

okr_objective_list <- function(cycle_id, owner_id = NULL) {
  con <- db_connect()
  tryCatch({
    sql <- sprintf(
      "SELECT o.*, u.display_name, u.username
       FROM okr_objectives o LEFT JOIN users u ON o.owner_id = u.id
       WHERE o.cycle_id = %d", as.integer(cycle_id))
    if (!is.null(owner_id) && !is.na(owner_id)) sql <- paste0(sql, sprintf(" AND o.owner_id = %d", as.integer(owner_id)))
    sql <- paste0(sql, " ORDER BY o.level, o.id")
    objs <- dbGetQuery(con, sql)
    if (nrow(objs) > 0) {
      objs$owner_name <- ifelse(is.na(objs$display_name) | objs$display_name == "", objs$username, objs$display_name)
      objs$level_label <- sapply(objs$level, okr_level_label)
      objs$status_label <- sapply(objs$status, okr_obj_status_label)
      objs$progress <- 0; objs$kr_count <- 0
      krs <- dbGetQuery(con, sprintf(
        "SELECT kr.objective_id, kr.progress, kr.weight FROM okr_key_results kr
         JOIN okr_objectives o ON kr.objective_id = o.id WHERE o.cycle_id = %d",
        as.integer(cycle_id)))
      if (nrow(krs) > 0) {
        for (i in seq_len(nrow(objs))) {
          okrs <- krs[krs$objective_id == objs$id[i], , drop = FALSE]
          if (nrow(okrs) > 0) {
            objs$kr_count[i] <- nrow(okrs)
            w <- ifelse(is.na(okrs$weight) | okrs$weight == 0, 1, okrs$weight)
            objs$progress[i] <- sum(okrs$progress * w) / sum(w)
          }
        }
      }
      objs$progress <- round(objs$progress, 1)
    }
    objs
  }, error = function(e) {
    message("[okr] 获取目标列表失败: ", e$message)
    data.frame()
  }, finally = { db_disconnect(con) })
}

okr_objective_progress <- function(objective_id) {
  krs <- okr_kr_list(objective_id)
  if (nrow(krs) == 0) return(0)
  w <- ifelse(is.na(krs$weight) | krs$weight == 0, 1, krs$weight)
  round(sum(krs$progress * w) / sum(w), 1)
}

okr_objective_update <- function(objective_id, title = NULL, description = NULL, level = NULL,
                                 owner_id = NULL, parent_id = NULL, weight = NULL, status = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(title)) sets <- c(sets, sprintf("title=%s", okr_text(title)))
    if (!is.null(description)) sets <- c(sets, sprintf("description=%s", okr_text(description)))
    if (!is.null(level)) sets <- c(sets, sprintf("level=%s", okr_text(level)))
    if (!is.null(owner_id)) sets <- c(sets, sprintf("owner_id=%s", okr_num(owner_id)))
    if (!is.null(parent_id)) sets <- c(sets, sprintf("parent_id=%s", okr_num(parent_id)))
    if (!is.null(weight)) sets <- c(sets, sprintf("weight=%s", okr_num(weight)))
    if (!is.null(status)) sets <- c(sets, sprintf("status=%s", okr_text(status)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE okr_objectives SET %s, updated_at=datetime('now','localtime') WHERE id=%d",
      paste(sets, collapse = ", "), as.integer(objective_id)))
    list(success = TRUE, message = "目标已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

okr_objective_delete <- function(objective_id) {
  con <- db_connect()
  tryCatch({
    krs <- dbGetQuery(con, sprintf("SELECT id FROM okr_key_results WHERE objective_id=%d", as.integer(objective_id)))
    if (nrow(krs) > 0) {
      dbExecute(con, sprintf("DELETE FROM okr_updates WHERE kr_id IN (%s)", paste(krs$id, collapse = ",")))
      dbExecute(con, sprintf("DELETE FROM okr_key_results WHERE objective_id=%d", as.integer(objective_id)))
    }
    dbExecute(con, sprintf("DELETE FROM okr_objectives WHERE id=%d", as.integer(objective_id)))
    list(success = TRUE, message = "目标及其关键结果已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 关键结果 (KR)
##################
okr_kr_add <- function(objective_id, title, metric = NULL, unit = NULL, baseline = 0, target = 100,
                       current_value = 0, weight = 1, source_type = "manual", source_id = NULL) {
  progress <- okr_kr_calc_progress(baseline, target, current_value)
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO okr_key_results (objective_id, title, metric, unit, baseline, target, current_value, progress, weight, source_type, source_id)
       VALUES (%d, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
      as.integer(objective_id), okr_text(title), okr_text(metric), okr_text(unit),
      okr_num(baseline), okr_num(target), okr_num(current_value), okr_num(progress),
      okr_num(weight), okr_text(source_type), okr_num(source_id)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    dbExecute(con, sprintf("UPDATE okr_objectives SET updated_at=datetime('now','localtime') WHERE id=%d", as.integer(objective_id)))
    list(success = TRUE, id = id, message = "关键结果已添加")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

okr_kr_get <- function(kr_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM okr_key_results WHERE id=%d", as.integer(kr_id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

okr_kr_list <- function(objective_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM okr_key_results WHERE objective_id=%d ORDER BY id", as.integer(objective_id)))
  }, error = function(e) {
    message("[okr] 获取关键结果失败: ", e$message)
    data.frame()
  }, finally = { db_disconnect(con) })
}

okr_kr_list_by_cycle <- function(cycle_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT kr.* FROM okr_key_results kr JOIN okr_objectives o ON kr.objective_id = o.id WHERE o.cycle_id = %d ORDER BY kr.id",
      as.integer(cycle_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

okr_kr_update <- function(kr_id, title = NULL, metric = NULL, unit = NULL, baseline = NULL, target = NULL,
                          current_value = NULL, weight = NULL, source_type = NULL, source_id = NULL) {
  con <- db_connect()
  tryCatch({
    kr <- dbGetQuery(con, sprintf("SELECT * FROM okr_key_results WHERE id=%d", as.integer(kr_id)))
    if (nrow(kr) == 0) return(list(success = FALSE, message = "关键结果不存在"))
    sets <- c()
    if (!is.null(title)) sets <- c(sets, sprintf("title=%s", okr_text(title)))
    if (!is.null(metric)) sets <- c(sets, sprintf("metric=%s", okr_text(metric)))
    if (!is.null(unit)) sets <- c(sets, sprintf("unit=%s", okr_text(unit)))
    if (!is.null(baseline)) sets <- c(sets, sprintf("baseline=%s", okr_num(baseline)))
    if (!is.null(target)) sets <- c(sets, sprintf("target=%s", okr_num(target)))
    if (!is.null(current_value)) sets <- c(sets, sprintf("current_value=%s", okr_num(current_value)))
    if (!is.null(weight)) sets <- c(sets, sprintf("weight=%s", okr_num(weight)))
    if (!is.null(source_type)) sets <- c(sets, sprintf("source_type=%s", okr_text(source_type)))
    if (!is.null(source_id)) sets <- c(sets, sprintf("source_id=%s", okr_num(source_id)))
    # 重新计算进度
    if (!is.null(baseline) || !is.null(target) || !is.null(current_value)) {
      b <- if (!is.null(baseline)) as.numeric(baseline) else as.numeric(kr$baseline)
      t <- if (!is.null(target)) as.numeric(target) else as.numeric(kr$target)
      cv <- if (!is.null(current_value)) as.numeric(current_value) else as.numeric(kr$current_value)
      sets <- c(sets, sprintf("progress=%s", okr_num(okr_kr_calc_progress(b, t, cv))))
    }
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE okr_key_results SET %s, updated_at=datetime('now','localtime') WHERE id=%d",
      paste(sets, collapse = ", "), as.integer(kr_id)))
    list(success = TRUE, message = "关键结果已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

okr_kr_delete <- function(kr_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM okr_updates WHERE kr_id=%d", as.integer(kr_id)))
    dbExecute(con, sprintf("DELETE FROM okr_key_results WHERE id=%d", as.integer(kr_id)))
    list(success = TRUE, message = "关键结果已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 进度检查 (Check-in)
##################
okr_kr_checkin <- function(kr_id, new_value, note = NULL, updated_by = NULL) {
  kr <- okr_kr_get(kr_id)
  if (is.null(kr)) return(list(success = FALSE, message = "关键结果不存在"))
  old_value <- as.numeric(kr$current_value)
  progress <- okr_kr_calc_progress(kr$baseline, kr$target, new_value)
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO okr_updates (kr_id, old_value, new_value, progress, note, updated_by) VALUES (%d, %s, %s, %s, %s, %s)",
      as.integer(kr_id), okr_num(old_value), okr_num(new_value), okr_num(progress),
      okr_text(note), okr_num(updated_by)))
    dbExecute(con, sprintf(
      "UPDATE okr_key_results SET current_value=%s, progress=%s, updated_at=datetime('now','localtime') WHERE id=%d",
      okr_num(new_value), okr_num(progress), as.integer(kr_id)))
    list(success = TRUE, message = sprintf("进度已更新为 %.1f%%", progress), progress = progress)
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

okr_updates_list <- function(kr_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT ku.*, u.display_name, u.username FROM okr_updates ku
       LEFT JOIN users u ON ku.updated_by = u.id
       WHERE ku.kr_id = %d ORDER BY ku.created_at DESC, ku.id DESC", as.integer(kr_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

okr_updates_by_objective <- function(objective_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT ku.*, kr.title AS kr_title, u.display_name, u.username
       FROM okr_updates ku
       JOIN okr_key_results kr ON ku.kr_id = kr.id
       LEFT JOIN users u ON ku.updated_by = u.id
       WHERE kr.objective_id = %d ORDER BY ku.created_at DESC, ku.id DESC", as.integer(objective_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

##################
# 打通已有数据
##################
# 统计某类工作项在周期内完成的数量（用于自动计算 KR 当前值）
okr_count_completed <- function(source_type, start_date, end_date, owner_id = NULL) {
  con <- db_connect()
  tryCatch({
    own_wo <- if (is.null(owner_id) || is.na(owner_id)) "" else sprintf(" AND wo.assigned_to=%d", as.integer(owner_id))
    own_pt <- if (is.null(owner_id) || is.na(owner_id)) "" else sprintf(" AND pt.assigned_to=%d", as.integer(owner_id))
    own_it <- if (is.null(owner_id) || is.na(owner_id)) "" else sprintf(" AND it.inspector=%d", as.integer(owner_id))
    sql <- if (source_type == "工单") {
      sprintf("SELECT COUNT(*) AS cnt FROM work_orders wo WHERE wo.status IN ('completed','closed') AND COALESCE(wo.completed_at, wo.updated_at) >= '%s' AND COALESCE(wo.completed_at, wo.updated_at) < '%s'%s",
        start_date, end_date, own_wo)
    } else if (source_type == "项目任务") {
      sprintf("SELECT COUNT(*) AS cnt FROM project_tasks pt WHERE pt.status IN ('completed','closed') AND COALESCE(pt.completed_at, pt.updated_at) >= '%s' AND COALESCE(pt.completed_at, pt.updated_at) < '%s'%s",
        start_date, end_date, own_pt)
    } else if (source_type == "巡检") {
      sprintf("SELECT COUNT(*) AS cnt FROM inspection_tasks it WHERE it.status='completed' AND it.updated_at >= '%s' AND it.updated_at < '%s'%s",
        start_date, end_date, own_it)
    } else {
      return(0)
    }
    dbGetQuery(con, sql)$cnt[1]
  }, error = function(e) 0, finally = { db_disconnect(con) })
}

# 读取员工在指定绩效月表的「绩效得分（10分制）」，打通绩效模块
okr_perf_score <- function(sheet_id, owner_id) {
  if (length(sheet_id) == 0 || is.null(sheet_id) || is.na(sheet_id)) return(0)
  items <- perf_work_items_by_sheet(sheet_id)
  if (nrow(items) == 0) return(0)
  emp_items <- items[items$employee_id == owner_id, , drop = FALSE]
  if (nrow(emp_items) == 0) return(0)
  a_deduct <- 0; b_score <- 0; c_score <- 0
  for (i in seq_len(nrow(emp_items))) {
    code <- emp_items$indicator_code[i]
    lvl <- if (is.na(emp_items$deduction_level[i])) 0 else emp_items$deduction_level[i]
    ind <- perf_indicator_get(code)
    if (is.null(ind)) next
    if (ind$scoring == "deduct") {
      for (dl in ind$deduct_levels) if (dl$level == lvl) a_deduct <- a_deduct + dl$points
    } else {
      sc <- if (is.null(ind$unit_score)) 5 else ind$unit_score
      if (ind$category == "B") b_score <- b_score + sc else if (ind$category == "C") c_score <- c_score + sc
    }
  }
  a_score <- max(0, 30 - a_deduct)
  total <- a_score + b_score + c_score
  round(min(total, 100) / 10, 1)
}

# 按数据源自动同步 KR 当前值与进度
okr_kr_sync_progress <- function(kr_id) {
  kr <- okr_kr_get(kr_id)
  if (is.null(kr)) return(list(success = FALSE, message = "关键结果不存在"))
  st <- kr$source_type
  if (is.null(st) || is.na(st) || st %in% c("", "manual", "手工")) {
    return(list(success = FALSE, message = "该关键结果未关联自动数据源"))
  }
  obj <- okr_objective_get(kr$objective_id)
  if (is.null(obj)) return(list(success = FALSE, message = "目标不存在"))
  cycle <- okr_cycle_get(obj$cycle_id)
  start_date <- if (!is.null(cycle) && nrow(cycle) > 0 && !is.na(cycle$start_date)) cycle$start_date else "1900-01-01"
  end_date <- if (!is.null(cycle) && nrow(cycle) > 0 && !is.na(cycle$end_date)) cycle$end_date else "9999-12-31"
  owner_id <- obj$owner_id

  new_value <- if (st == "绩效") {
    okr_perf_score(kr$source_id, owner_id)
  } else {
    okr_count_completed(st, start_date, okr_end_exclusive(end_date), owner_id)
  }
  progress <- okr_kr_calc_progress(kr$baseline, kr$target, new_value)

  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE okr_key_results SET current_value=%s, progress=%s, updated_at=datetime('now','localtime') WHERE id=%d",
      okr_num(new_value), okr_num(progress), as.integer(kr_id)))
  }, error = function(e) return(list(success = FALSE, message = e$message)),
  finally = { db_disconnect(con) })

  list(success = TRUE,
       message = sprintf("已同步：当前值 %s，进度 %.1f%%", format(new_value, trim = TRUE, scientific = FALSE), progress),
       value = new_value, progress = progress)
}
