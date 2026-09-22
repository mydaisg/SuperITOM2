##################
# 需求模块 — 数据层
# 需求主表 requirements + 进度条目子表 requirement_progress
##################

# ── 需求主表 CRUD ──
requirement_get_all <- function(status = NULL, keyword = NULL) {
  con <- db_connect()
  tryCatch({
    q <- "SELECT * FROM requirements WHERE 1=1"
    if (!is.null(status) && nzchar(status)) {
      q <- sprintf("%s AND status='%s'", q, gsub("'", "''", status))
    }
    if (!is.null(keyword) && nzchar(keyword)) {
      q <- sprintf("%s AND (title LIKE '%%%s%%' OR description LIKE '%%%s%%')", q,
                   gsub("'", "''", keyword), gsub("'", "''", keyword))
    }
    q <- paste0(q, " ORDER BY updated_at DESC, id DESC")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

requirement_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM requirements WHERE id=%d", as.integer(id)))
    if (nrow(r) == 0) return(NULL)
    r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

requirement_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("REQ", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT req_no FROM requirements WHERE req_no LIKE '%s%%' ORDER BY req_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$req_no[1], 12, 14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "REQ00000000001",
  finally = { db_disconnect(con) })
}

requirement_add <- function(title, description = NULL, status = "进行中", priority = "中",
                            owner = NULL, start_date = NULL, due_date = NULL, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    no <- requirement_generate_no()
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO requirements (req_no, title, description, status, priority, owner, start_date, due_date, created_by, created_at, updated_at) VALUES ('%s','%s','%s','%s','%s','%s','%s','%s',%s,'%s','%s')",
      no, gsub("'","''",title), gsub("'","''",description%||%""),
      gsub("'","''",status%||%"进行中"), gsub("'","''",priority%||%"中"),
      gsub("'","''",owner%||%""),
      gsub("'","''",as.character(start_date%||%"")),
      gsub("'","''",as.character(due_date%||%"")),
      if(is.null(created_by)) "NULL" else as.character(created_by), now, now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, req_no = no, message = paste("需求", no, "已创建"))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

requirement_update <- function(id, title = NULL, description = NULL, status = NULL,
                               priority = NULL, owner = NULL, start_date = NULL, due_date = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c("updated_at = datetime('now','localtime')")
    if (!is.null(title)) sets <- c(sets, sprintf("title='%s'", gsub("'","''",title)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    if (!is.null(status)) sets <- c(sets, sprintf("status='%s'", gsub("'","''",status)))
    if (!is.null(priority)) sets <- c(sets, sprintf("priority='%s'", gsub("'","''",priority)))
    if (!is.null(owner)) sets <- c(sets, sprintf("owner='%s'", gsub("'","''",owner)))
    if (!is.null(start_date)) sets <- c(sets, sprintf("start_date='%s'", gsub("'","''",as.character(start_date))))
    if (!is.null(due_date)) sets <- c(sets, sprintf("due_date='%s'", gsub("'","''",as.character(due_date))))
    dbExecute(con, sprintf("UPDATE requirements SET %s WHERE id=%d",
      paste(sets, collapse=","), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

requirement_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM requirement_progress WHERE requirement_id=%d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM requirements WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# ── 进度条目子表 CRUD ──
requirement_progress_get_by_req <- function(requirement_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM requirement_progress WHERE requirement_id=%d ORDER BY sort_order, id",
      as.integer(requirement_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

requirement_progress_add <- function(requirement_id, dept = NULL, person = NULL,
                                     status = NULL, progress_date = NULL, content = NULL,
                                     sort_order = 0, parent_id = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO requirement_progress (requirement_id, dept, person, status, progress_date, content, sort_order, parent_id) VALUES (%d,%s,%s,%s,%s,%s,%d,%d)",
      as.integer(requirement_id),
      if(is.null(dept)||dept=="") "NULL" else sprintf("'%s'", gsub("'","''",dept)),
      if(is.null(person)||person=="") "NULL" else sprintf("'%s'", gsub("'","''",person)),
      if(is.null(status)||status=="") "NULL" else sprintf("'%s'", gsub("'","''",status)),
      if(is.null(progress_date)||progress_date=="") "NULL" else sprintf("'%s'", gsub("'","''",as.character(progress_date))),
      if(is.null(content)||content=="") "NULL" else sprintf("'%s'", gsub("'","''",content)),
      as.integer(sort_order), as.integer(parent_id)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, message = "已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

requirement_progress_update <- function(id, dept = NULL, person = NULL, status = NULL,
                                        progress_date = NULL, content = NULL, sort_order = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(dept)) sets <- c(sets, sprintf("dept='%s'", gsub("'","''",dept)))
    if (!is.null(person)) sets <- c(sets, sprintf("person='%s'", gsub("'","''",person)))
    if (!is.null(status)) sets <- c(sets, sprintf("status='%s'", gsub("'","''",status)))
    if (!is.null(progress_date)) sets <- c(sets, sprintf("progress_date='%s'", gsub("'","''",as.character(progress_date))))
    if (!is.null(content)) sets <- c(sets, sprintf("content='%s'", gsub("'","''",content)))
    if (!is.null(sort_order)) sets <- c(sets, sprintf("sort_order=%d", as.integer(sort_order)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE requirement_progress SET %s WHERE id=%d",
      paste(sets, collapse=","), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

requirement_progress_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM requirement_progress WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 切换进度条目完成状态（绿勾）：已完成 ↔ 进行中
requirement_progress_toggle_done <- function(id) {
  con <- db_connect()
  tryCatch({
    p <- dbGetQuery(con, sprintf("SELECT status FROM requirement_progress WHERE id=%d", as.integer(id)))
    if (nrow(p) == 0) return(list(success = FALSE, message = "记录不存在"))
    cur <- p$status[1]
    new_status <- if (!is.na(cur) && cur == "已完成") "进行中" else "已完成"
    dbExecute(con, sprintf("UPDATE requirement_progress SET status='%s' WHERE id=%d",
      new_status, as.integer(id)))
    list(success = TRUE, message = if (new_status == "已完成") "已标记完成" else "已取消完成")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 需求状态 / 优先级选项
requirement_status_choices <- function() c("进行中", "已完成", "未开始", "暂停", "已关闭")
requirement_priority_choices <- function() c("高", "中", "低")

# 状态 → 颜色
requirement_status_color <- function(status) {
  switch(status,
    "已完成" = "#5cb85c",
    "进行中" = "#337ab7",
    "未开始" = "#f0ad4e",
    "暂停"   = "#d9534f",
    "已关闭" = "#999999",
    "#337ab7")
}

# 进度状态 → 颜色（用于进度条目）
requirement_progress_status_color <- function(status) {
  if (is.null(status) || is.na(status) || status == "") return("#94a3b8")
  if (grepl("已完成", status)) return("#059669")
  if (grepl("未回复", status)) return("#f59e0b")
  if (grepl("进行中", status)) return("#2563eb")
  if (grepl("暂停|取消|关闭", status)) return("#94a3b8")
  "#0ea5e9"
}

# 数字 → 中文序号（1~99 → 一、二、...、十、十一、...、九十九）
requirement_num_to_cn <- function(n) {
  digits <- c("零","一","二","三","四","五","六","七","八","九")
  n <- as.integer(n)
  if (is.na(n) || n <= 0) return("零")
  if (n < 10) return(digits[n + 1])
  if (n < 20) {
    return(if (n %% 10 == 0) "十" else paste0("十", digits[n %% 10 + 1]))
  }
  paste0(digits[n %/% 10 + 1], "十", if (n %% 10 == 0) "" else digits[n %% 10 + 1])
}

# 彩虹色（参照记事模块评论的 8 色循环）
requirement_rainbow_colors <- function() {
  c("#e74c3c","#e67e22","#f1c40f","#2ecc71","#1abc9c","#3498db","#9b59b6","#e91e63")
}
