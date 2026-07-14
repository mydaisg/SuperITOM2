# 记事模块 - 数据层 (v3: 编号 + 评论管理)
# NTE+YYYYMMDD+3位流水，评论可编辑删除

##################
# 权限辅助：非admin用户返回其ID用于过滤，admin返回NULL表示不过滤
##################
note_visible_user_id <- function(current_user) {
  if (is.null(current_user) || nrow(current_user) == 0) return(NULL)
  if (current_user$role[1] == "admin") return(NULL)
  as.integer(current_user$id[1])
}

# 检查记事所有权：非admin用户只能操作自己的记事
note_check_ownership <- function(note_id, current_user, con) {
  uid <- note_visible_user_id(current_user)
  if (is.null(uid)) return(TRUE)  # admin 无限制
  note <- dbGetQuery(con, sprintf(
    "SELECT id FROM notes WHERE id = %d AND created_by = %d",
    as.integer(note_id), uid))
  nrow(note) > 0
}

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
note_get_all <- function(current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    if (is.null(uid)) {
      dbGetQuery(con, "SELECT n.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM notes n LEFT JOIN users u ON n.created_by = u.id ORDER BY n.pinned DESC, n.updated_at DESC")
    } else {
      dbGetQuery(con, sprintf("SELECT n.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM notes n LEFT JOIN users u ON n.created_by = u.id WHERE (n.created_by = %d OR n.id IN (SELECT note_id FROM note_dispatches WHERE user_id = %d)) ORDER BY n.pinned DESC, n.updated_at DESC", uid, uid))
    }
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

##################
# 搜索记事（标题 + 评论内容）
##################
note_search <- function(keyword, current_user = NULL) {
  if (is.null(keyword) || trimws(keyword) == "") return(note_get_all(current_user))
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    user_filter <- if (is.null(uid)) "" else sprintf("AND (n.created_by = %d OR n.id IN (SELECT note_id FROM note_dispatches WHERE user_id = %d))", uid, uid)
    kw <- trimws(keyword)
    # 判断精确搜索：用 "" 或 <> 包裹
    if (grepl('^".*"$', kw) || grepl("^<.*>$", kw)) {
      kw <- gsub('^["<]|["<]$', '', kw)  # 精确匹配
      safe <- gsub("'", "''", kw)
      query <- sprintf("
        SELECT DISTINCT n.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name
        FROM notes n LEFT JOIN users u ON n.created_by = u.id
        LEFT JOIN note_comments c ON c.note_id = n.id
        WHERE (n.title = '%s' OR c.content = '%s') %s
        ORDER BY n.pinned DESC, n.updated_at DESC", safe, safe, user_filter)
    } else {
      # 空格分隔 → AND 关系，每个词匹配标题/正文/评论任一即可
      words <- strsplit(kw, "\\s+")[[1]]
      words <- words[words != ""]
      conditions <- sapply(words, function(w) {
        sw <- gsub("'", "''", w)
        sprintf("(n.title LIKE '%%%s%%' OR n.content LIKE '%%%s%%' OR c.content LIKE '%%%s%%')", sw, sw, sw)
      })
      query <- sprintf("
        SELECT DISTINCT n.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name
        FROM notes n LEFT JOIN users u ON n.created_by = u.id
        LEFT JOIN note_comments c ON c.note_id = n.id
        WHERE (%s) %s
        ORDER BY n.pinned DESC, n.updated_at DESC", paste(conditions, collapse = " AND "), user_filter)
    }
    dbGetQuery(con, query)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 搜索时获取指定记事中匹配关键字的评论（用于卡片展示）
note_search_get_matching_comments <- function(note_ids, keyword) {
  if (length(note_ids) == 0 || is.null(keyword) || trimws(keyword) == "") return(data.frame())
  con <- db_connect()
  tryCatch({
    words <- strsplit(trimws(keyword), "\\s+")[[1]]
    words <- words[words != ""]
    if (length(words) == 0) return(data.frame())
    conditions <- sapply(words, function(w) {
      sw <- gsub("'", "''", w)
      sprintf("c.content LIKE '%%%s%%'", sw)
    })
    ids_str <- paste(as.integer(note_ids), collapse = ",")
    dbGetQuery(con, sprintf("
      SELECT c.id, c.note_id, c.content, c.created_at,
             u.username as creator_name
      FROM note_comments c
      LEFT JOIN users u ON c.created_by = u.id
      WHERE c.note_id IN (%s) AND (%s)
      ORDER BY c.created_at ASC", ids_str, paste(conditions, collapse = " AND ")))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

##################
# 获取单条
##################
note_get_by_id <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    if (is.null(uid)) {
      r <- dbGetQuery(con, sprintf("SELECT n.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM notes n LEFT JOIN users u ON n.created_by = u.id WHERE n.id = %d", as.integer(id)))
    } else {
      r <- dbGetQuery(con, sprintf("SELECT n.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM notes n LEFT JOIN users u ON n.created_by = u.id WHERE n.id = %d AND (n.created_by = %d OR n.id IN (SELECT note_id FROM note_dispatches WHERE user_id = %d))", as.integer(id), uid, uid))
    }
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
note_update <- function(id, title = NULL, content = NULL, reminder_at = NULL, due_at = NULL, note_no = NULL, created_at = NULL, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) return(list(success = FALSE, message = "无权操作此记事"))
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
note_patch <- function(id, ..., current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) return(list(success = FALSE, message = "无权操作此记事"))
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

# 挂起记事（设 status='suspended'，移除提醒和到期时间）
note_suspend <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) {
      return(list(success = FALSE, message = "无权操作此记事"))
    }
    dbExecute(con, sprintf(
      "UPDATE notes SET status='suspended', reminder_at=NULL, due_at=NULL, updated_at=datetime('now','localtime') WHERE id=%d",
      as.integer(id)))
    list(success = TRUE, message = "已挂起")
  }, error = function(e) list(success = FALSE, message = paste("挂起失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 重启挂起记事（恢复为 pending，到期=当月最后一天15:00，提醒=当月最后一天14:00）
note_resume <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) {
      return(list(success = FALSE, message = "无权操作此记事"))
    }
    d <- Sys.Date()
    first_of_month <- as.Date(format(d, "%Y-%m-01"))
    last_of_month <- seq(first_of_month, by = "month", length.out = 2)[2] - 1
    due_at <- sprintf("%s 15:00:00", format(last_of_month, "%Y-%m-%d"))
    reminder_at <- sprintf("%s 14:00:00", format(last_of_month, "%Y-%m-%d"))
    dbExecute(con, sprintf(
      "UPDATE notes SET status='pending', due_at='%s', reminder_at='%s', updated_at=datetime('now','localtime') WHERE id=%d",
      due_at, reminder_at, as.integer(id)))
    list(success = TRUE, message = sprintf("已重启，到期：%s", format(last_of_month, "%Y-%m-%d")))
  }, error = function(e) list(success = FALSE, message = paste("重启失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 取消提醒 / 延长到期
##################
note_cancel_reminder <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) return(list(success = FALSE, message = "无权操作此记事"))
    dbExecute(con, sprintf("UPDATE notes SET reminder_at = NULL, updated_at = datetime('now','localtime') WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已取消提醒")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

note_extend_due <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) return(list(success = FALSE, message = "无权操作此记事"))
    note <- dbGetQuery(con, sprintf("SELECT reminder_at FROM notes WHERE id = %d", as.integer(id)))
    if (nrow(note) == 0 || is.na(note$reminder_at[1]) || note$reminder_at[1] == "") return(list(success=FALSE, message="无提醒时间"))
    new_rem <- as.character(as.POSIXct(note$reminder_at[1]) + 86400)
    dbExecute(con, sprintf("UPDATE notes SET reminder_at='%s', updated_at=datetime('now','localtime') WHERE id=%d", new_rem, as.integer(id)))
    list(success = TRUE, message = "提醒时间已延长1天")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 置顶切换（最多5条）
##################
note_toggle_pin <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) return(list(success = FALSE, message = "无权操作此记事"))
    note <- dbGetQuery(con, sprintf("SELECT pinned FROM notes WHERE id = %d", as.integer(id)))
    if (nrow(note) == 0) return(list(success = FALSE, message = "记事不存在"))
    pinned <- note$pinned[1] %||% 0
    if (pinned == 0) {
      # 检查已置顶数量
      count <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM notes WHERE pinned = 1")$cnt[1]
      if (count >= 5) return(list(success = FALSE, message = "最多置顶5条"))
      dbExecute(con, sprintf("UPDATE notes SET pinned = 1, updated_at = datetime('now','localtime') WHERE id = %d", as.integer(id)))
      list(success = TRUE, message = "已置顶")
    } else {
      dbExecute(con, sprintf("UPDATE notes SET pinned = 0 WHERE id = %d", as.integer(id)))
      list(success = TRUE, message = "已取消置顶")
    }
  }, error = function(e) list(success = FALSE, message = paste("操作失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 删除
##################
note_delete <- function(id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    if (!note_check_ownership(id, current_user, con)) return(list(success = FALSE, message = "无权操作此记事"))
    dbExecute(con, sprintf("DELETE FROM note_comments WHERE note_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM note_dispatches WHERE note_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM notes WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 转工单
##################
note_convert_to_work_order <- function(note_id, current_user) {
  note <- note_get_by_id(note_id, current_user)
  if (is.null(note) || nrow(note) == 0) return(list(success = FALSE, message = "记事不存在或无权操作"))
  
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
note_comment_add <- function(note_id, content, created_by = NULL, parent_id = NULL, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    # 非admin用户只能在自己可见的记事下评论（创建人或被派发人）
    uid <- note_visible_user_id(current_user)
    if (!is.null(uid)) {
      owner_check <- dbGetQuery(con, sprintf("SELECT id FROM notes WHERE id = %d AND (created_by = %d OR id IN (SELECT note_id FROM note_dispatches WHERE user_id = %d))", as.integer(note_id), uid, uid))
      if (nrow(owner_check) == 0) return(list(success = FALSE, message = "无权在此记事下评论"))
    }
    pid <- if (is.null(parent_id)) "NULL" else as.character(as.integer(parent_id))
    query <- if (pid == "NULL") {
      sprintf("INSERT INTO note_comments (note_id, content, created_by) VALUES (%d,'%s',%s)",
        as.integer(note_id), gsub("'","''",content),
        ifelse(is.null(created_by),"NULL",as.character(created_by)))
    } else {
      sprintf("INSERT INTO note_comments (note_id, content, created_by, parent_id) VALUES (%d,'%s',%s,%s)",
        as.integer(note_id), gsub("'","''",content),
        ifelse(is.null(created_by),"NULL",as.character(created_by)), pid)
    }
    dbExecute(con, query)
    # 同步更新记事的 updated_at
    dbExecute(con, sprintf("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = %d", as.integer(note_id)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    r <- dbGetQuery(con, sprintf(
      "SELECT c.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM note_comments c LEFT JOIN users u ON c.created_by = u.id WHERE c.id = %d", id))
    list(success = TRUE, message = "评论已添加", id = id,
      created_at = if (nrow(r) > 0) r$created_at[1] else format(Sys.time(), "%Y-%m-%d %H:%M"),
      creator_name = if (nrow(r) > 0) r$creator_name[1] %||% "匿名" else "匿名",
      parent_id = parent_id)
  }, error = function(e) list(success = FALSE, message = paste("评论失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 评论状态标记（默认NULL，可标记为 completed 等）
##################
note_comment_mark_status <- function(comment_id, status, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    # 非admin用户只能标记自己的评论
    uid <- note_visible_user_id(current_user)
    if (!is.null(uid)) {
      owner_check <- dbGetQuery(con, sprintf("SELECT id FROM note_comments WHERE id = %d AND created_by = %d", as.integer(comment_id), uid))
      if (nrow(owner_check) == 0) return(list(success = FALSE, message = "无权操作此评论"))
    }
    val <- if (is.null(status) || status == "") "NULL" else sprintf("'%s'", status)
    completed_val <- if (isTRUE(status == "completed")) sprintf("'%s'", format(Sys.time(), "%Y-%m-%d %H:%M")) else "NULL"
    if (val == "NULL") {
      dbExecute(con, sprintf("UPDATE note_comments SET status = NULL, completed_at = NULL WHERE id = %d", as.integer(comment_id)))
    } else {
      dbExecute(con, sprintf("UPDATE note_comments SET status = '%s', completed_at = %s WHERE id = %d", status, completed_val, as.integer(comment_id)))
    }
    # 同步更新记事的 updated_at
    note_id <- dbGetQuery(con, sprintf("SELECT note_id FROM note_comments WHERE id = %d", as.integer(comment_id)))$note_id[1]
    if (!is.na(note_id)) dbExecute(con, sprintf("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = %d", note_id))
    list(success = TRUE, message = paste("已标记为", status))
  }, error = function(e) list(success = FALSE, message = paste("标记失败:", e$message)),
  finally = { db_disconnect(con) })
}

note_comment_get_by_id <- function(comment_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf(
      "SELECT c.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM note_comments c LEFT JOIN users u ON c.created_by = u.id WHERE c.id = %d",
      as.integer(comment_id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

note_comment_get_last <- function(note_id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    extra <- ""
    if (!is.null(uid)) {
      note <- dbGetQuery(con, sprintf("SELECT created_by FROM notes WHERE id = %d", as.integer(note_id)))
      is_owner <- nrow(note) > 0 && note$created_by[1] == uid
      is_dispatched <- !is_owner && nrow(dbGetQuery(con, sprintf("SELECT 1 FROM note_dispatches WHERE note_id = %d AND user_id = %d", as.integer(note_id), uid))) > 0
      if (is_dispatched) extra <- sprintf("AND c.created_by = %d", uid)
    }
    r <- dbGetQuery(con, sprintf(
      "SELECT c.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM note_comments c LEFT JOIN users u ON c.created_by = u.id WHERE c.note_id = %d %s ORDER BY c.created_at DESC LIMIT 1",
      as.integer(note_id), extra))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

note_comment_get_all <- function(note_id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    # 派发用户只看自己的评论；创建人和admin看全部
    extra <- ""
    if (!is.null(uid)) {
      note <- dbGetQuery(con, sprintf("SELECT created_by FROM notes WHERE id = %d", as.integer(note_id)))
      is_owner <- nrow(note) > 0 && note$created_by[1] == uid
      is_dispatched <- !is_owner && nrow(dbGetQuery(con, sprintf("SELECT 1 FROM note_dispatches WHERE note_id = %d AND user_id = %d", as.integer(note_id), uid))) > 0
      if (is_dispatched) extra <- sprintf("AND c.created_by = %d", uid)
    }
    dbGetQuery(con, sprintf(
      "SELECT c.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM note_comments c LEFT JOIN users u ON c.created_by = u.id WHERE c.note_id = %d %s ORDER BY c.created_at ASC",
      as.integer(note_id), extra))
  }, finally = { db_disconnect(con) })
}

note_comment_update <- function(comment_id, content, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    if (!is.null(uid)) {
      owner_check <- dbGetQuery(con, sprintf("SELECT id FROM note_comments WHERE id = %d AND created_by = %d", as.integer(comment_id), uid))
      if (nrow(owner_check) == 0) return(list(success = FALSE, message = "无权修改此评论"))
    }
    dbExecute(con, sprintf("UPDATE note_comments SET content='%s' WHERE id=%d",
      gsub("'","''",content), as.integer(comment_id)))
    note_id <- dbGetQuery(con, sprintf("SELECT note_id FROM note_comments WHERE id = %d", as.integer(comment_id)))$note_id[1]
    if (!is.na(note_id)) dbExecute(con, sprintf("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = %d", note_id))
    list(success = TRUE, message = "评论已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

note_comment_update_time <- function(comment_id, created_at) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE note_comments SET created_at='%s' WHERE id=%d",
      created_at, as.integer(comment_id)))
    note_id <- dbGetQuery(con, sprintf("SELECT note_id FROM note_comments WHERE id = %d", as.integer(comment_id)))$note_id[1]
    if (!is.na(note_id)) dbExecute(con, sprintf("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = %d", note_id))
    list(success = TRUE, message = "评论时间已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

note_comment_delete <- function(comment_id, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    if (!is.null(uid)) {
      owner_check <- dbGetQuery(con, sprintf("SELECT id FROM note_comments WHERE id = %d AND created_by = %d", as.integer(comment_id), uid))
      if (nrow(owner_check) == 0) return(list(success = FALSE, message = "无权删除此评论"))
    }
    note_id <- dbGetQuery(con, sprintf("SELECT note_id FROM note_comments WHERE id = %d", as.integer(comment_id)))$note_id[1]
    dbExecute(con, sprintf("DELETE FROM note_comments WHERE id=%d", as.integer(comment_id)))
    if (!is.na(note_id)) dbExecute(con, sprintf("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = %d", note_id))
    list(success = TRUE, message = "评论已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 今日记事（供日报用）
##################
note_get_today <- function(current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    today <- format(Sys.Date(), "%Y-%m-%d")
    if (is.null(uid)) {
      dbGetQuery(con, sprintf("SELECT id, title, content, status, importance, created_at FROM notes WHERE date(created_at) = '%s' ORDER BY created_at DESC", today))
    } else {
      dbGetQuery(con, sprintf("SELECT id, title, content, status, importance, created_at FROM notes WHERE date(created_at) = '%s' AND created_by = %d ORDER BY created_at DESC", today, uid))
    }
  }, finally = { db_disconnect(con) })
}

# 从所有标题提取 TOP N 关键字（供快速筛选）
# 策略：CJK bigram 提取 + 非中文词抽取 + 分隔符切分 → 频次排序 → 过滤
note_get_top_keywords <- function(n = 10, current_user = NULL) {
  con <- db_connect()
  tryCatch({
    uid <- note_visible_user_id(current_user)
    if (is.null(uid)) {
      titles <- dbGetQuery(con, "SELECT title FROM notes WHERE title IS NOT NULL AND title != ''")$title
    } else {
      titles <- dbGetQuery(con, sprintf("SELECT title FROM notes WHERE title IS NOT NULL AND title != '' AND created_by = %d", uid))$title
    }
    if (length(titles) == 0) return(character(0))
    n_titles <- length(titles)
    all_words <- character(0)
    for (t in titles) {
      # 1) CJK bigram：连续中文每相邻两字组成词
      cjk_runs <- regmatches(t, gregexpr("[\u4e00-\u9fff\u3400-\u4dbf\uf900-\ufaff]+", t, perl = TRUE))[[1]]
      for (run in cjk_runs) {
        chars <- strsplit(run, "")[[1]]
        if (length(chars) >= 2) {
          for (i in seq_len(length(chars) - 1)) {
            all_words <- c(all_words, paste0(chars[i], chars[i + 1]))
          }
        }
      }
      # 2) 非中文词：英文字母序列（如 SuperITOM、ssh、API 等）
      en_runs <- strsplit(t, "[^A-Za-z]+")[[1]]
      en_runs <- en_runs[nchar(en_runs) >= 2]
      if (length(en_runs) > 0) all_words <- c(all_words, en_runs)
      # 3) 分隔符切分：按标点/数字切开后保留 ≥2 字的片段
      segs <- strsplit(t, "[\\s,，。；;：:、（）()\\[\\]【】《》\"'\\-_/|#＠@＋+＝=&%°℃\\r\\n\\t\\d]+")[[1]]
      segs <- trimws(segs)
      segs <- segs[nchar(segs) >= 2]
      if (length(segs) > 0) all_words <- c(all_words, segs)
    }
    if (length(all_words) == 0) return(character(0))
    freq <- table(all_words)
    # 停用词（常见虚词 + 日期时间成分）
    stopwords <- tolower(c(
      "的","是","在","和","与","或","及","之","不","了","也","就","都","这","那","但",
      "而","且","所","为","被","把","从","对","向","以","到","要","会","能","可以",
      "需要","进行","一个","这个","那个","每个","一些","还有","出来","起来","一下",
      "就是","还是","不是","没有","已经","因为","所以","如果","虽然","但是","然后",
      "因此","不过","可能","应该","必须","一定",
      "什么","怎么","怎样","如何","为什么","多少","哪个","哪里","什么时候","谁",
      "年","月","日","时","分","秒","周","星期",
      "今天","明天","昨天","上午","下午","晚上","中午",
      "说","想","看","做","来","去","给","让","用","有","知道","觉得","认为"
    ))
    # 过滤条件（严格版）：
    # 1) 停用词/单字/纯数字/日期 直接排除
    # 2) 仅2字词（bigram）：只保留出现≥2次的（过滤跨边界垃圾如"础服"）
    # 3) 过于泛化（>50%标题）排除
    bad <- names(freq) %in% stopwords |
      nchar(names(freq)) < 2 |
      grepl("^[\\d\\s\\.\\-:\\/]+$", names(freq), perl = TRUE) |      # 纯数字/日期(20260606,2026-06-06)
      grepl("^\\d+[年月日时分秒周]+$", names(freq), perl = TRUE) |    # 数字+日期后缀(2026年,06月)
      (nchar(names(freq)) == 2 & freq < 2) |                          # 2字词必须出现≥2次
      freq > n_titles * 0.5                                           # 出现在>50%标题中，过于泛化
    freq <- freq[!bad]
    if (length(freq) == 0) return(character(0))
    freq <- sort(freq, decreasing = TRUE)
    names(freq)[seq_len(min(n, length(freq)))]
  }, error = function(e) character(0), finally = { db_disconnect(con) })
}

##################
# 记事派发（admin → 多个user）
##################
note_dispatch_set <- function(note_id, user_ids) {
  con <- db_connect()
  tryCatch({
    nid <- as.integer(note_id)
    # 清除旧派发
    dbExecute(con, sprintf("DELETE FROM note_dispatches WHERE note_id = %d", nid))
    # 添加新派发
    for (uid in user_ids) {
      if (is.null(uid) || uid == "") next
      dbExecute(con, sprintf("INSERT OR IGNORE INTO note_dispatches (note_id, user_id) VALUES (%d, %d)", nid, as.integer(uid)))
    }
    list(success = TRUE, message = sprintf("已派发给 %d 人", length(user_ids)))
  }, error = function(e) list(success = FALSE, message = paste("派发失败:", e$message)),
  finally = { db_disconnect(con) })
}

note_dispatch_get_users <- function(note_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT u.id, u.username, COALESCE(NULLIF(u.display_name,''), u.username) as display_name
       FROM note_dispatches nd JOIN users u ON nd.user_id = u.id WHERE nd.note_id = %d ORDER BY u.username", as.integer(note_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

note_is_dispatched_user <- function(note_id, user_id, con = NULL) {
  own_con <- is.null(con)
  if (own_con) con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT 1 FROM note_dispatches WHERE note_id = %d AND user_id = %d", as.integer(note_id), as.integer(user_id)))
    nrow(r) > 0
  }, error = function(e) FALSE, finally = { if (own_con) db_disconnect(con) })
}

# 获取评论总数和今日新增数
note_comment_count_today <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y-%m-%d")
    total <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM note_comments")$cnt[1]
    today_cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS cnt FROM note_comments WHERE created_at LIKE '%s%%'", today))$cnt[1]
    list(total = total %||% 0L, today = today_cnt %||% 0L)
  }, error = function(e) list(total = 0L, today = 0L),
  finally = { db_disconnect(con) })
}

##################
# 记事关键词统计（分类记忆）
##################

# 记录关键词点击
note_kw_record_click <- function(keyword) {
  if (is.null(keyword) || nchar(trimws(keyword)) == 0) return()
  con <- db_connect()
  tryCatch({
    kw <- gsub("'", "''", trimws(keyword))
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO note_kw_stats (keyword, click_count, last_clicked_at) VALUES ('%s', 1, '%s')
       ON CONFLICT(keyword) DO UPDATE SET click_count = click_count + 1, last_clicked_at = '%s'",
      kw, now, now))
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# 获取热门关键词（按点击次数排序）
note_kw_get_top <- function(n = 8) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf(
      "SELECT keyword, click_count FROM note_kw_stats ORDER BY click_count DESC, last_clicked_at DESC LIMIT %d", n))
    if (nrow(r) == 0) character(0) else r$keyword
  }, error = function(e) character(0),
  finally = { db_disconnect(con) })
}

##################
# 周次自动编号：从所有标题含"Week"的笔记中取最大周号+1
##################
note_week_gen_next <- function() {
  con <- db_connect()
  tryCatch({
    titles <- dbGetQuery(con, "SELECT title FROM notes WHERE title LIKE '%Week%'")
    max_yr <- 0L; max_wk <- 0L
    if (nrow(titles) > 0) {
      for (t in titles$title) {
        m <- regmatches(t, gregexpr("\\d{4}Week\\d+", t))[[1]]
        if (length(m) == 0) next
        m <- m[length(m)]  # 取最后一个匹配
        parts <- strsplit(m, "Week")[[1]]
        yr <- as.integer(parts[1]); wk <- as.integer(parts[2])
        if (is.na(yr) || is.na(wk)) next
        if (yr > max_yr || (yr == max_yr && wk > max_wk)) { max_yr <- yr; max_wk <- wk }
      }
    }
    if (max_yr == 0L) {
      today <- Sys.Date()
      max_yr <- as.integer(format(today, "%Y"))
      max_wk <- as.integer(format(today, "%V"))  # 当前周，+1得下周
    }
    # 年末翻年判定
    dec28 <- as.Date(paste0(max_yr, "-12-28"))
    yr_max_wk <- if (as.integer(format(dec28, "%V")) == 52) 52L else 53L
    if (max_wk >= yr_max_wk) { max_yr <- max_yr + 1L; max_wk <- 0L }
    next_wk <- max_wk + 1L
    label <- sprintf("%dWeek%d", max_yr, next_wk)
    list(success = TRUE, label = label)
  }, error = function(e) list(success = FALSE, message = e$message, label = ""),
  finally = { db_disconnect(con) })
}


