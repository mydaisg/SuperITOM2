##################
# 消息机制 - 数据层
# 消息中心 + 动作流模板（类似泛微 ESB 动作流）
# 表：messages / message_recipients / message_actions / message_flows
##################

# ── 编号生成 MSG+YYYYMMDD+3位流水 ──
message_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("MSG", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT msg_no FROM messages WHERE msg_no LIKE '%s%%' ORDER BY msg_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$msg_no[1], nchar(prefix) + 1, nchar(prefix) + 3))
    sprintf("%s%03d", prefix, if (is.na(last)) 1 else last + 1)
  }, error = function(e) "MSG00000000001", finally = { db_disconnect(con) })
}

# ── 用户列表（供选择接收人） ──
message_get_users <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT id, username, display_name, role FROM users WHERE active=1 ORDER BY username")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# ── 动作流轨迹（内部：在给定连接里插入，不管理连接） ──
msg_action_insert <- function(con, message_id, action, actor_id = NULL, actor_name = "",
                              node_index = NULL, node_name = NULL, comment = NULL) {
  dbExecute(con, sprintf(
    "INSERT INTO message_actions (message_id, node_index, node_name, action, actor_id, actor_name, comment)
     VALUES (%d, %s, %s, '%s', %s, '%s', %s)",
    as.integer(message_id),
    if (is.null(node_index)) "NULL" else as.character(as.integer(node_index)),
    if (is.null(node_name)) "NULL" else sprintf("'%s'", gsub("'", "''", node_name)),
    action,
    if (is.null(actor_id)) "NULL" else as.character(as.integer(actor_id)),
    gsub("'", "''", actor_name %||% ""),
    if (is.null(comment) || comment == "") "NULL" else sprintf("'%s'", gsub("'", "''", comment))))
}

message_action_add <- function(message_id, action, actor_id = NULL, actor_name = "",
                               node_index = NULL, node_name = NULL, comment = NULL) {
  con <- db_connect()
  tryCatch({
    msg_action_insert(con, message_id, action, actor_id, actor_name, node_index, node_name, comment)
    list(success = TRUE)
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# ── 获取用户显示名 ──
message_user_name <- function(con, user_id) {
  u <- dbGetQuery(con, sprintf("SELECT display_name, username FROM users WHERE id=%d", as.integer(user_id)))
  if (nrow(u) == 0) return("")
  if (!is.na(u$display_name[1]) && u$display_name[1] != "") u$display_name[1] else u$username[1]
}

##################
# 消息 CRUD
##################
# 创建消息（含接收人 + 初始发送动作；绑定动作流时进入第 1 节点）
message_add <- function(title, content = NULL, msg_type = "站内信", priority = "中",
                        sender_id = NULL, recipient_ids = NULL, flow_id = NULL) {
  con <- db_connect()
  tryCatch({
    no <- message_generate_no()
    is_flow <- !is.null(flow_id) && !is.na(flow_id) && as.integer(flow_id) > 0
    dbExecute(con, sprintf(
      "INSERT INTO messages (msg_no, title, content, msg_type, priority, flow_id, sender_id, status, current_node)
       VALUES ('%s','%s',%s,'%s','%s',%s,%s,'%s',%d)",
      no, gsub("'", "''", title),
      if (is.null(content) || content == "") "NULL" else sprintf("'%s'", gsub("'", "''", content)),
      gsub("'", "''", msg_type %||% "站内信"), gsub("'", "''", priority %||% "中"),
      if (is_flow) as.character(as.integer(flow_id)) else "NULL",
      if (is.null(sender_id)) "NULL" else as.character(as.integer(sender_id)),
      if (is_flow) "流转中" else "已发送",
      if (is_flow) 1L else 0L))
    mid <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]

    # 接收人
    if (!is.null(recipient_ids) && length(recipient_ids) > 0) {
      for (uid in recipient_ids) {
        if (is.null(uid) || is.na(uid) || uid == "") next
        dbExecute(con, sprintf("INSERT OR IGNORE INTO message_recipients (message_id, user_id) VALUES (%d, %d)",
          mid, as.integer(uid)))
      }
    }

    # 初始动作：发送
    node_index <- NULL; node_name <- "发送"
    if (is_flow) {
      flow <- dbGetQuery(con, sprintf("SELECT nodes FROM message_flows WHERE id=%d", as.integer(flow_id)))
      if (nrow(flow) > 0) {
        nodes <- tryCatch(jsonlite::fromJSON(flow$nodes[1], simplifyVector = FALSE), error = function(e) list())
        if (length(nodes) > 0) { node_index <- 1L; node_name <- nodes[[1]]$name %||% "发送" }
      }
    }
    msg_action_insert(con, mid, "send", sender_id, if (!is.null(sender_id)) message_user_name(con, sender_id) else "",
                      node_index, node_name)
    list(success = TRUE, id = mid, msg_no = no, message = paste("消息", no, "已发送"))
  }, error = function(e) list(success = FALSE, message = paste("发送失败:", e$message)),
  finally = { db_disconnect(con) })
}

message_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf(
      "SELECT m.*, COALESCE(NULLIF(s.display_name,''), s.username) as sender_name
       FROM messages m LEFT JOIN users s ON s.id = m.sender_id WHERE m.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# 收件箱（我是接收人的消息）
message_list_inbox <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT m.*, r.is_read, r.read_at,
              COALESCE(NULLIF(s.display_name,''), s.username) as sender_name
       FROM messages m
       JOIN message_recipients r ON r.message_id = m.id AND r.user_id = %d
       LEFT JOIN users s ON s.id = m.sender_id
       ORDER BY m.created_at DESC, m.id DESC", as.integer(user_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 发件箱（我发的消息）
message_list_outbox <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT m.*, (SELECT COUNT(*) FROM message_recipients r WHERE r.message_id = m.id) as rcpt_count
       FROM messages m WHERE m.sender_id = %d ORDER BY m.created_at DESC, m.id DESC", as.integer(user_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

message_get_recipients <- function(message_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT r.*, COALESCE(NULLIF(u.display_name,''), u.username) as display_name, u.username
       FROM message_recipients r LEFT JOIN users u ON u.id = r.user_id
       WHERE r.message_id = %d ORDER BY r.id", as.integer(message_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

message_actions_get <- function(message_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM message_actions WHERE message_id=%d ORDER BY id, created_at", as.integer(message_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

message_unread_count <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT COUNT(*) as c FROM message_recipients WHERE user_id=%d AND is_read=0", as.integer(user_id)))$c[1]
  }, error = function(e) 0, finally = { db_disconnect(con) })
}

##################
# 消息动作
##################
message_mark_read <- function(message_id, user_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE message_recipients SET is_read=1, read_at=datetime('now','localtime') WHERE message_id=%d AND user_id=%d AND is_read=0",
      as.integer(message_id), as.integer(user_id)))
    list(success = TRUE)
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

message_reply <- function(message_id, actor_id = NULL, actor_name = "", comment = NULL) {
  message_action_add(message_id, "reply", actor_id, actor_name, comment = comment)
}

message_forward <- function(message_id, actor_id = NULL, actor_name = "", target_user_ids = NULL, comment = NULL) {
  con <- db_connect()
  tryCatch({
    msg_action_insert(con, message_id, "forward", actor_id, actor_name, comment = comment)
    if (!is.null(target_user_ids) && length(target_user_ids) > 0) {
      for (uid in target_user_ids) {
        if (is.null(uid) || is.na(uid) || uid == "") next
        dbExecute(con, sprintf("INSERT OR IGNORE INTO message_recipients (message_id, user_id) VALUES (%d, %d)",
          as.integer(message_id), as.integer(uid)))
      }
    }
    list(success = TRUE, message = "已转发")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

message_complete <- function(message_id, actor_id = NULL, actor_name = "", comment = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE messages SET status='已完成', updated_at=datetime('now','localtime') WHERE id=%d", as.integer(message_id)))
    msg_action_insert(con, message_id, "complete", actor_id, actor_name, comment = comment)
    list(success = TRUE, message = "已完成")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

message_revoke <- function(message_id, actor_id = NULL, actor_name = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE messages SET status='已撤回', updated_at=datetime('now','localtime') WHERE id=%d", as.integer(message_id)))
    msg_action_insert(con, message_id, "revoke", actor_id, actor_name)
    list(success = TRUE, message = "已撤回")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 推进动作流（处理当前节点 → 流转到下一节点；最后一个节点处理完即完成）
message_advance_flow <- function(message_id, actor_id = NULL, actor_name = "", comment = NULL) {
  con <- db_connect()
  tryCatch({
    m <- dbGetQuery(con, sprintf("SELECT * FROM messages WHERE id=%d", as.integer(message_id)))
    if (nrow(m) == 0) return(list(success = FALSE, message = "消息不存在"))
    flow_id <- m$flow_id[1]
    if (is.na(flow_id) || is.null(flow_id)) return(list(success = FALSE, message = "该消息未绑定动作流"))
    flow <- dbGetQuery(con, sprintf("SELECT nodes FROM message_flows WHERE id=%d", as.integer(flow_id)))
    if (nrow(flow) == 0) return(list(success = FALSE, message = "动作流模板不存在"))
    nodes <- tryCatch(jsonlite::fromJSON(flow$nodes[1], simplifyVector = FALSE), error = function(e) list())
    if (length(nodes) == 0) return(list(success = FALSE, message = "动作流无节点"))
    cur <- as.integer(m$current_node[1] %||% 1L)
    if (cur > length(nodes)) return(list(success = TRUE, done = TRUE, message = "动作流已完成"))
    node <- nodes[[cur]]
    node_name <- node$name %||% sprintf("节点%d", cur)
    # 记录当前节点处理动作
    msg_action_insert(con, message_id, "process", actor_id, actor_name, cur, node_name, comment)
    nxt <- cur + 1L
    if (nxt > length(nodes)) {
      dbExecute(con, sprintf("UPDATE messages SET status='已完成', current_node=%d, updated_at=datetime('now','localtime') WHERE id=%d", cur, as.integer(message_id)))
      return(list(success = TRUE, done = TRUE, message = sprintf("已处理「%s」，动作流完成", node_name)))
    }
    dbExecute(con, sprintf("UPDATE messages SET current_node=%d, updated_at=datetime('now','localtime') WHERE id=%d", nxt, as.integer(message_id)))
    nxt_name <- nodes[[nxt]]$name %||% sprintf("节点%d", nxt)
    list(success = TRUE, done = FALSE, message = sprintf("已处理「%s」，流转到「%s」", node_name, nxt_name))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 动作流模板
##################
message_flow_list <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT * FROM message_flows ORDER BY id DESC") },
  error = function(e) data.frame(), finally = { db_disconnect(con) })
}

message_flow_get <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM message_flows WHERE id=%d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# nodes：R list（list(list(name=, action_type=, assignee=), ...)）
message_flow_add <- function(name, description = NULL, nodes = NULL) {
  con <- db_connect()
  tryCatch({
    nodes_json <- if (is.character(nodes)) nodes else jsonlite::toJSON(nodes %||% list(), auto_unbox = TRUE)
    dbExecute(con, sprintf(
      "INSERT INTO message_flows (name, description, nodes) VALUES ('%s', %s, '%s')",
      gsub("'", "''", name),
      if (is.null(description) || description == "") "NULL" else sprintf("'%s'", gsub("'", "''", description)),
      gsub("'", "''", nodes_json)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, message = paste("动作流「", name, "」已创建"))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

message_flow_update <- function(id, name = NULL, description = NULL, nodes = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(name)) sets <- paste0(sets, sprintf(", name='%s'", gsub("'", "''", name)))
    if (!is.null(description)) sets <- paste0(sets, sprintf(", description='%s'", gsub("'", "''", description)))
    if (!is.null(nodes)) {
      nodes_json <- if (is.character(nodes)) nodes else jsonlite::toJSON(nodes, auto_unbox = TRUE)
      sets <- paste0(sets, sprintf(", nodes='%s'", gsub("'", "''", nodes_json)))
    }
    dbExecute(con, sprintf("UPDATE message_flows SET %s WHERE id=%d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

message_flow_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM message_flows WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 通用通知钩子（供其他模块事件接入，静默失败不影响主业务）
##################
message_notify <- function(sender_id = NULL, recipient_ids = NULL, title, content = NULL,
                           msg_type = "事件", priority = "中") {
  tryCatch({
    # 未指定接收人时默认通知所有启用状态的 admin
    if (is.null(recipient_ids) || length(recipient_ids) == 0) {
      con <- db_connect()
      admins <- tryCatch(dbGetQuery(con, "SELECT id FROM users WHERE role='admin' AND active=1"),
        error = function(e) data.frame())
      db_disconnect(con)
      recipient_ids <- if (!is.null(admins) && nrow(admins) > 0) admins$id else NULL
    }
    message_add(title, content, msg_type, priority, sender_id, recipient_ids, flow_id = NULL)
    invisible(NULL)
  }, error = function(e) invisible(NULL))
}
