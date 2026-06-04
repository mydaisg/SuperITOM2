# 记事模块 - 数据层 (v3: 编号 + 评论管理)
# NTE+YYYYMMDD+3位流水，评论可编辑删除

##################
# 编号生成 NTE+YYYYMMDD+3位流水
##################
note_generate_number <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("NTE", today)
    existing <- dbGetQuery(con, sprintf("SELECT note_no FROM notes WHERE note_no LIKE '%s%%' ORDER BY note_no DESC LIMIT 1", prefix))
    if (nrow(existing) > 0 && !is.na(existing$note_no[1])) {
      last_seq <- as.integer(substr(existing$note_no[1], nchar(prefix)+1, nchar(prefix)+3))
      seq <- if (is.na(last_seq)) 1 else last_seq + 1
    } else { seq <- 1 }
    sprintf("%s%03d", prefix, seq)
  }, finally = { db_disconnect(con) })
}

##################
# 获取所有记事
##################
note_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT n.*, u.username as creator_name FROM notes n LEFT JOIN users u ON n.created_by = u.id ORDER BY n.importance DESC, n.updated_at DESC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

##################
# 获取单条
##################
note_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT n.*, u.username as creator_name FROM notes n LEFT JOIN users u ON n.created_by = u.id WHERE n.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

##################
# 新增记事
# text: 全文，首行自动提取为 title
##################
note_add <- function(text, created_by = NULL, reminder_hours = NULL, due_hour = NULL) {
  lines <- strsplit(trimws(text), "\n")[[1]]
  title <- if (length(lines) > 0 && nchar(lines[1]) > 0) lines[1] else "未命名记事"
  content <- text
  
  # 提醒时间：可配置，默认 3 小时
  if (is.null(reminder_hours)) reminder_hours <- as.numeric(config_get_value("note_reminder_hours", "3"))
  reminder_at <- format(Sys.time() + reminder_hours * 3600, "%Y-%m-%d %H:%M")
  
  # 完成时间：可配置，默认 18:00
  if (is.null(due_hour)) due_hour <- as.integer(config_get_value("note_due_hour", "18"))
  due_at <- format(as.POSIXct(paste(Sys.Date(), sprintf("%02d:00:00", due_hour))), "%Y-%m-%d %H:%M")
  
  con <- db_connect()
  tryCatch({
    note_no <- note_generate_number()
    query <- sprintf(
      "INSERT INTO notes (note_no, title, content, reminder_at, due_at, created_by) VALUES ('%s','%s','%s','%s','%s',%s)",
      note_no, gsub("'","''",title), gsub("'","''",content), reminder_at, due_at,
      ifelse(is.null(created_by),"NULL",as.character(created_by)))
    dbExecute(con, query)
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, note_no = note_no, title = title, message = paste("已添加", note_no))
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 补充旧数据缺失的 note_no
##################
note_fill_missing_no <- function() {
  con <- db_connect()
  tryCatch({
    orphans <- dbGetQuery(con, "SELECT id FROM notes WHERE note_no IS NULL OR note_no = ''")
    if (nrow(orphans) > 0) {
      for (oid in orphans$id) {
        new_no <- note_generate_number()
        dbExecute(con, sprintf("UPDATE notes SET note_no = '%s' WHERE id = %d", new_no, oid))
        message("[NOTE-INIT] 补充旧数据 note_no: id=", oid, " -> ", new_no)
      }
    }
  }, finally = { db_disconnect(con) })
}

##################
# 更新记事（编辑弹窗用）
##################
note_update <- function(id, title = NULL, content = NULL, reminder_at = NULL, due_at = NULL, note_no = NULL, created_at = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(title))       sets <- paste0(sets, sprintf(", title='%s'", gsub("'","''",title)))
    if (!is.null(content))     sets <- paste0(sets, sprintf(", content='%s'", gsub("'","''",content)))
    if (!is.null(reminder_at)) sets <- paste0(sets, sprintf(", reminder_at='%s'", reminder_at))
    if (!is.null(due_at))      sets <- paste0(sets, sprintf(", due_at='%s'", due_at))
    if (!is.null(note_no))     sets <- paste0(sets, sprintf(", note_no='%s'", gsub("'","''",note_no)))
    if (!is.null(created_at))  sets <- paste0(sets, sprintf(", created_at='%s'", created_at))
    dbExecute(con, sprintf("UPDATE notes SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 快速更新字段（状态/重要性/提醒/到期）
##################
note_patch <- function(id, ...) {
  con <- db_connect()
  tryCatch({
    args <- list(...)
    sets <- c()
    if ("status" %in% names(args))     sets <- c(sets, sprintf("status='%s'", args$status))
    if ("importance" %in% names(args)) sets <- c(sets, sprintf("importance=%d", as.integer(args$importance)))
    if ("reminder_at" %in% names(args)) sets <- c(sets, sprintf("reminder_at='%s'", args$reminder_at))
    if ("due_at" %in% names(args))     sets <- c(sets, sprintf("due_at='%s'", args$due_at))
    if ("reported_to_daily" %in% names(args)) sets <- c(sets, sprintf("reported_to_daily=%d", as.integer(args$reported_to_daily)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    sets <- c(sets, "updated_at = datetime('now','localtime')")
    dbExecute(con, sprintf("UPDATE notes SET %s WHERE id = %d", paste(sets, collapse=", "), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 删除
##################
note_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM notes WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 转工单
##################
note_convert_to_work_order <- function(note_id, current_user) {
  note <- note_get_by_id(note_id)
  if (is.null(note) || nrow(note) == 0) return(list(success = FALSE, message = "记事不存在"))
  
  result <- work_order_add(
    title = paste0("[记事转] ", note$title[1]),
    description = note$content[1] %||% note$title[1],
    priority = "中",
    category = "其他", subcategory = "",
    request_user = "",
    current_user = current_user
  )
  
  if (result$success) {
    con <- db_connect()
    tryCatch({
      dbExecute(con, sprintf("UPDATE notes SET related_work_order_id=%d, status='completed', updated_at=datetime('now','localtime') WHERE id=%d", result$id, as.integer(note_id)))
    }, finally = { db_disconnect(con) })
    list(success = TRUE, message = paste("已转为工单", result$order_no))
  } else result
}

##################
# 评论相关
##################
note_comment_add <- function(note_id, content, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO note_comments (note_id, content, created_by) VALUES (%d,'%s',%s)",
      as.integer(note_id), gsub("'","''",content),
      ifelse(is.null(created_by),"NULL",as.character(created_by))))
    list(success = TRUE, message = "评论已添加")
  }, error = function(e) list(success = FALSE, message = paste("评论失败:", e$message)),
  finally = { db_disconnect(con) })
}

note_comment_get_last <- function(note_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf(
      "SELECT c.*, u.username as creator_name FROM note_comments c LEFT JOIN users u ON c.created_by = u.id WHERE c.note_id = %d ORDER BY c.created_at DESC LIMIT 1",
      as.integer(note_id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

note_comment_get_all <- function(note_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT c.*, u.username as creator_name FROM note_comments c LEFT JOIN users u ON c.created_by = u.id WHERE c.note_id = %d ORDER BY c.created_at ASC",
      as.integer(note_id)))
  }, finally = { db_disconnect(con) })
}

note_comment_update <- function(comment_id, content) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE note_comments SET content='%s' WHERE id=%d",
      gsub("'","''",content), as.integer(comment_id)))
    list(success = TRUE, message = "评论已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

note_comment_delete <- function(comment_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM note_comments WHERE id=%d", as.integer(comment_id)))
    list(success = TRUE, message = "评论已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 今日记事（供日报用）
##################
note_get_today <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y-%m-%d")
    dbGetQuery(con, sprintf("SELECT id, title, content, status, importance, created_at FROM notes WHERE date(created_at) = '%s' ORDER BY created_at DESC", today))
  }, finally = { db_disconnect(con) })
}
