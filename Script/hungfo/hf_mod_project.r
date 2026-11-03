##################
# HungFo 环节 — 项目 (HF-PROJECT)
# 职责：集合父项目 SuperITOM2 的「需求 / 记事 / 总结 / 项目模板」四类能力为一体，
#       通过内部 Tab 快速切换，并以 project_id 打通数据联动、增删改后自动刷新。
# 四张表：hf_projects（项目模板）/ hf_project_requirements（需求）/
#         hf_project_notes（记事）/ hf_project_summaries（总结）。
# 说明：本环节 id 取 hf081，字符串排序排在「元任务 hf08」之后、「开发日志 hf09」之前。
##################

HF_MOD_PROJECT <- list(
  id = "hf081", key = "project", name = "项目", icon = "project-diagram",
  desc = "集合需求、方案、记事、总结、项目模板于一体：快速 Tab 切换，按项目打通数据联动，增删改后自动刷新。",
  api_desc = "list/get/add/update/delete 维护项目 · requirements/solutions/notes/summaries 维护子项 · note_comment_* 维护记事评论 · summary_build(pid) 一键生成总结",
  status = "scaffold", version = "v0.2"
)

# ── 建表（自包含，供 hf_ui.r 初始化时调用；CREATE IF NOT EXISTS 幂等）──
hf_project_init <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    # 项目表（项目模板：项目主表，含优先级/状态/起止日期）
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_projects (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_no TEXT,
      name TEXT NOT NULL,
      description TEXT DEFAULT '',
      priority TEXT DEFAULT '中',
      status TEXT DEFAULT 'planning',
      start_date TEXT DEFAULT '',
      end_date TEXT DEFAULT '',
      created_by INTEGER,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      updated_at TEXT DEFAULT (datetime('now','localtime'))
    )")
    # 需求表（需求模块：关联 project_id 打通联动）
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_project_requirements (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      req_no TEXT,
      project_id INTEGER,
      title TEXT NOT NULL,
      description TEXT DEFAULT '',
      status TEXT DEFAULT '进行中',
      priority TEXT DEFAULT '中',
      owner TEXT DEFAULT '',
      due_date TEXT DEFAULT '',
      created_by INTEGER,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      updated_at TEXT DEFAULT (datetime('now','localtime'))
    )")
    # 记事表（记事模块：Trello 看板三列，关联 project_id）
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_project_notes (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      note_no TEXT,
      project_id INTEGER,
      title TEXT NOT NULL,
      content TEXT DEFAULT '',
      status TEXT DEFAULT 'pending',
      priority TEXT DEFAULT '中',
      created_by INTEGER,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      updated_at TEXT DEFAULT (datetime('now','localtime'))
    )")
    # 总结表（总结模块：分段内容，关联 project_id，可自动聚合生成）
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_project_summaries (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      summary_no TEXT,
      project_id INTEGER,
      title TEXT NOT NULL,
      content TEXT DEFAULT '',
      summary_date TEXT DEFAULT (date('now','localtime')),
      created_by INTEGER,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      updated_at TEXT DEFAULT (datetime('now','localtime'))
    )")
    # 方案表（方案模块：技术/企业方案文档，content 存 HTML，关联 project_id，参照父项目 solutions）
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_project_solutions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      sol_no TEXT,
      project_id INTEGER,
      title TEXT NOT NULL,
      content TEXT DEFAULT '',
      category TEXT DEFAULT '技术方案',
      related_project TEXT DEFAULT '',
      created_by INTEGER,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      updated_at TEXT DEFAULT (datetime('now','localtime'))
    )")
    # 记事评论表（迁移父项目 note_comments；parent_id 自关联保留回复层级）
    dbExecute(con, "CREATE TABLE IF NOT EXISTS hf_project_note_comments (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      note_id INTEGER NOT NULL,
      content TEXT NOT NULL,
      created_by INTEGER,
      created_by_name TEXT DEFAULT '',
      created_at TEXT DEFAULT (datetime('now','localtime')),
      status TEXT,
      completed_at TEXT,
      parent_id INTEGER
    )")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_hf_note_comments_note ON hf_project_note_comments(note_id)")
    invisible(TRUE)
  }, error = function(e) {
    warning(paste("hf_project_init 建表失败:", e$message))
    invisible(FALSE)
  })
}

# ── 编号生成器（统一前缀 + 日期 + 3 位流水）──
hf_project_generate_no <- function(table, column, prefix) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    day <- format(Sys.Date(), "%Y%m%d")
    pat <- paste0(prefix, day)
    ex <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s LIKE '%s%%' ORDER BY %s DESC LIMIT 1",
                                  column, table, column, pat, column))
    if (nrow(ex) == 0 || is.na(ex[[column]][1]) || !nzchar(ex[[column]][1])) {
      return(paste0(pat, "001"))
    }
    last <- ex[[column]][1]
    seq <- as.integer(substr(last, nchar(pat) + 1, nchar(last))) + 1L
    sprintf("%s%03d", pat, seq)
  }, error = function(e) paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S")))
}

# ══════════════════════════════════════════════════════════════════
# 项目 CRUD（项目模板）
# ══════════════════════════════════════════════════════════════════
hf_project_list <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, "SELECT * FROM hf_projects ORDER BY id DESC"),
    error = function(e) data.frame())
}

hf_project_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_projects WHERE id=%s", as.character(as.integer(id)))),
    error = function(e) data.frame())
}

hf_project_add <- function(name, description = "", priority = "中", status = "planning",
                           start_date = "", end_date = "", created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "项目名称不能为空"))
  no <- hf_project_generate_no("hf_projects", "project_no", "PRJ")
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_projects (project_no,name,description,priority,status,start_date,end_date,created_by)
       VALUES ('%s','%s','%s','%s','%s','%s','%s',%s)",
      hf_esc(no), hf_esc(name), hf_esc(hf_or(description, "")), hf_esc(hf_or(priority, "中")),
      hf_esc(hf_or(status, "planning")), hf_esc(hf_or(start_date, "")), hf_esc(hf_or(end_date, "")),
      if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, project_no = no, message = paste0("已新增项目：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_update <- function(id, name, description = "", priority = "中", status = "planning",
                              start_date = "", end_date = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "项目 id 无效"))
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "项目名称不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_projects SET name='%s',description='%s',priority='%s',status='%s',
       start_date='%s',end_date='%s',updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(name), hf_esc(hf_or(description, "")), hf_esc(hf_or(priority, "中")),
      hf_esc(hf_or(status, "planning")), hf_esc(hf_or(start_date, "")), hf_esc(hf_or(end_date, "")),
      as.character(as.integer(id))))
    list(success = TRUE, message = paste0("已更新项目：", name))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 删除项目：级联删除其下需求/记事/总结（数据联动闭环）
hf_project_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    id <- as.character(as.integer(id))
    dbExecute(con, sprintf("DELETE FROM hf_project_requirements WHERE project_id=%s", id))
    dbExecute(con, sprintf("DELETE FROM hf_project_solutions WHERE project_id=%s", id))
    # 先删该项目下记事的评论，再删记事（避免孤儿评论）
    dbExecute(con, sprintf(
      "DELETE FROM hf_project_note_comments WHERE note_id IN (SELECT id FROM hf_project_notes WHERE project_id=%s)", id))
    dbExecute(con, sprintf("DELETE FROM hf_project_notes WHERE project_id=%s", id))
    dbExecute(con, sprintf("DELETE FROM hf_project_summaries WHERE project_id=%s", id))
    dbExecute(con, sprintf("DELETE FROM hf_projects WHERE id=%s", id))
    list(success = TRUE, message = "已删除项目及其下需求/方案/记事/总结")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_count <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbGetQuery(con, "SELECT COUNT(*) n FROM hf_projects")$n[1], error = function(e) 0)
}

# ══════════════════════════════════════════════════════════════════
# 需求 CRUD（需求模块，按 project_id 联动）
# ══════════════════════════════════════════════════════════════════
hf_project_req_list <- function(project_id = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT * FROM hf_project_requirements"
    if (!is.null(project_id) && !is.na(project_id) && project_id != "") {
      sql <- sprintf("%s WHERE project_id=%s", sql, as.character(as.integer(project_id)))
    }
    dbGetQuery(con, paste0(sql, " ORDER BY id DESC"))
  }, error = function(e) data.frame())
}

hf_project_req_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_project_requirements WHERE id=%s", as.character(as.integer(id)))),
    error = function(e) data.frame())
}

hf_project_req_add <- function(project_id, title, description = "", status = "进行中",
                               priority = "中", owner = "", due_date = "", created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "需求标题不能为空"))
  no <- hf_project_generate_no("hf_project_requirements", "req_no", "REQ")
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_project_requirements (req_no,project_id,title,description,status,priority,owner,due_date,created_by)
       VALUES ('%s',%s,'%s','%s','%s','%s','%s','%s',%s)",
      hf_esc(no), if (is.null(project_id) || is.na(project_id) || project_id == "") "NULL" else as.character(as.integer(project_id)),
      hf_esc(title), hf_esc(hf_or(description, "")), hf_esc(hf_or(status, "进行中")),
      hf_esc(hf_or(priority, "中")), hf_esc(hf_or(owner, "")), hf_esc(hf_or(due_date, "")),
      if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, req_no = no, message = paste0("已新增需求：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_req_update <- function(id, title, description = "", status = "进行中",
                                  priority = "中", owner = "", due_date = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "需求 id 无效"))
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "需求标题不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_project_requirements SET title='%s',description='%s',status='%s',priority='%s',
       owner='%s',due_date='%s',updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(title), hf_esc(hf_or(description, "")), hf_esc(hf_or(status, "进行中")),
      hf_esc(hf_or(priority, "中")), hf_esc(hf_or(owner, "")), hf_esc(hf_or(due_date, "")),
      as.character(as.integer(id))))
    list(success = TRUE, message = paste0("已更新需求：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_req_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_project_requirements WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已删除需求")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_req_toggle_done <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_project_requirements SET status = CASE WHEN status='已完成' THEN '进行中' ELSE '已完成' END,
       updated_at=datetime('now','localtime') WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已切换需求状态")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ══════════════════════════════════════════════════════════════════
# 记事 CRUD（记事模块，Trello 看板三列，按 project_id 联动）
# ══════════════════════════════════════════════════════════════════
hf_project_note_list <- function(project_id = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT * FROM hf_project_notes"
    if (!is.null(project_id) && !is.na(project_id) && project_id != "") {
      sql <- sprintf("%s WHERE project_id=%s", sql, as.character(as.integer(project_id)))
    }
    dbGetQuery(con, paste0(sql, " ORDER BY id DESC"))
  }, error = function(e) data.frame())
}

hf_project_note_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_project_notes WHERE id=%s", as.character(as.integer(id)))),
    error = function(e) data.frame())
}

hf_project_note_add <- function(project_id, title, content = "", status = "pending",
                                priority = "中", created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "记事标题不能为空"))
  no <- hf_project_generate_no("hf_project_notes", "note_no", "NOTE")
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_project_notes (note_no,project_id,title,content,status,priority,created_by)
       VALUES ('%s',%s,'%s','%s','%s','%s',%s)",
      hf_esc(no), if (is.null(project_id) || is.na(project_id) || project_id == "") "NULL" else as.character(as.integer(project_id)),
      hf_esc(title), hf_esc(hf_or(content, "")), hf_esc(hf_or(status, "pending")),
      hf_esc(hf_or(priority, "中")), if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, note_no = no, message = paste0("已新增记事：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_note_update <- function(id, title, content = "", status = "pending", priority = "中") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "记事 id 无效"))
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "记事标题不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_project_notes SET title='%s',content='%s',status='%s',priority='%s',
       updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(title), hf_esc(hf_or(content, "")), hf_esc(hf_or(status, "pending")),
      hf_esc(hf_or(priority, "中")), as.character(as.integer(id))))
    list(success = TRUE, message = paste0("已更新记事：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 看板列间移动：直接改 status（pending/in_progress/done）
hf_project_note_move <- function(id, status) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_project_notes SET status='%s',updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(status), as.character(as.integer(id))))
    list(success = TRUE, message = "记事状态已更新")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_note_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    id <- as.character(as.integer(id))
    # 级联删除该记事下的评论，避免孤儿数据
    dbExecute(con, sprintf("DELETE FROM hf_project_note_comments WHERE note_id=%s", id))
    dbExecute(con, sprintf("DELETE FROM hf_project_notes WHERE id=%s", id))
    list(success = TRUE, message = "已删除记事及其评论")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ── 记事评论 CRUD（parent_id 自关联保留回复层级）──
hf_project_note_comment_list <- function(note_id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf(
      "SELECT * FROM hf_project_note_comments WHERE note_id=%s ORDER BY id ASC",
      as.character(as.integer(note_id)))),
    error = function(e) data.frame())
}

hf_project_note_comment_count <- function(note_id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf(
      "SELECT COUNT(*) n FROM hf_project_note_comments WHERE note_id=%s",
      as.character(as.integer(note_id))))$n[1],
    error = function(e) 0)
}

hf_project_note_comment_add <- function(note_id, content, parent_id = NULL, created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(content) || !nzchar(trimws(content))) return(list(success = FALSE, message = "评论内容不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_project_note_comments (note_id, content, created_by, parent_id)
       VALUES (%s, '%s', %s, %s)",
      as.character(as.integer(note_id)), hf_esc(content),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by)),
      if (is.null(parent_id) || is.na(parent_id)) "NULL" else as.character(as.integer(parent_id))))
    # 同步更新记事 updated_at
    dbExecute(con, sprintf(
      "UPDATE hf_project_notes SET updated_at=datetime('now','localtime') WHERE id=%s",
      as.character(as.integer(note_id))))
    list(success = TRUE, message = "已添加评论")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_note_comment_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf(
      "DELETE FROM hf_project_note_comments WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已删除评论")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ══════════════════════════════════════════════════════════════════
# 总结 CRUD（总结模块，按 project_id 联动；支持一键自动聚合）
# ══════════════════════════════════════════════════════════════════
hf_project_summary_list <- function(project_id = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT * FROM hf_project_summaries"
    if (!is.null(project_id) && !is.na(project_id) && project_id != "") {
      sql <- sprintf("%s WHERE project_id=%s", sql, as.character(as.integer(project_id)))
    }
    dbGetQuery(con, paste0(sql, " ORDER BY id DESC"))
  }, error = function(e) data.frame())
}

hf_project_summary_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_project_summaries WHERE id=%s", as.character(as.integer(id)))),
    error = function(e) data.frame())
}

hf_project_summary_add <- function(project_id, title, content = "", summary_date = NULL, created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "总结标题不能为空"))
  no <- hf_project_generate_no("hf_project_summaries", "summary_no", "SUM")
  d <- if (is.null(summary_date) || is.na(summary_date) || !nzchar(summary_date)) format(Sys.Date(), "%Y-%m-%d") else summary_date
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_project_summaries (summary_no,project_id,title,content,summary_date,created_by)
       VALUES ('%s',%s,'%s','%s','%s',%s)",
      hf_esc(no), if (is.null(project_id) || is.na(project_id) || project_id == "") "NULL" else as.character(as.integer(project_id)),
      hf_esc(title), hf_esc(hf_or(content, "")), hf_esc(d),
      if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, summary_no = no, message = paste0("已新增总结：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_summary_update <- function(id, title, content = "", summary_date = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "总结 id 无效"))
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "总结标题不能为空"))
  tryCatch({
    d <- if (is.null(summary_date) || is.na(summary_date) || !nzchar(summary_date)) format(Sys.Date(), "%Y-%m-%d") else summary_date
    dbExecute(con, sprintf(
      "UPDATE hf_project_summaries SET title='%s',content='%s',summary_date='%s',
       updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(title), hf_esc(hf_or(content, "")), hf_esc(d), as.character(as.integer(id))))
    list(success = TRUE, message = paste0("已更新总结：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_summary_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_project_summaries WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已删除总结")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ══════════════════════════════════════════════════════════════════
# 方案 CRUD（方案模块，按 project_id 联动；content 存 HTML 文档，参照父项目 solutions）
# ══════════════════════════════════════════════════════════════════
hf_project_solution_list <- function(project_id = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT * FROM hf_project_solutions"
    if (!is.null(project_id) && !is.na(project_id) && project_id != "") {
      sql <- sprintf("%s WHERE project_id=%s", sql, as.character(as.integer(project_id)))
    }
    dbGetQuery(con, paste0(sql, " ORDER BY id DESC"))
  }, error = function(e) data.frame())
}

hf_project_solution_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, sprintf("SELECT * FROM hf_project_solutions WHERE id=%s", as.character(as.integer(id)))),
    error = function(e) data.frame())
}

hf_project_solution_add <- function(project_id, title, content = "", category = "技术方案",
                                    related_project = "", created_by = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "方案标题不能为空"))
  no <- hf_project_generate_no("hf_project_solutions", "sol_no", "SOL")
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_project_solutions (sol_no,project_id,title,content,category,related_project,created_by)
       VALUES ('%s',%s,'%s','%s','%s','%s',%s)",
      hf_esc(no), if (is.null(project_id) || is.na(project_id) || project_id == "") "NULL" else as.character(as.integer(project_id)),
      hf_esc(title), hf_esc(hf_or(content, "")), hf_esc(hf_or(category, "技术方案")),
      hf_esc(hf_or(related_project, "")), if (is.null(created_by)) "NULL" else as.character(created_by)))
    list(success = TRUE, sol_no = no, message = paste0("已新增方案：", no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_solution_update <- function(id, title, content = "", category = "技术方案", related_project = "") {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "方案 id 无效"))
  if (is.null(title) || !nzchar(trimws(title))) return(list(success = FALSE, message = "方案标题不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_project_solutions SET title='%s',content='%s',category='%s',related_project='%s',
       updated_at=datetime('now','localtime') WHERE id=%s",
      hf_esc(title), hf_esc(hf_or(content, "")), hf_esc(hf_or(category, "技术方案")),
      hf_esc(hf_or(related_project, "")), as.character(as.integer(id))))
    list(success = TRUE, message = paste0("已更新方案：", title))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_project_solution_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_project_solutions WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已删除方案")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 一键自动聚合：根据项目 id 聚合「项目信息 + 需求 + 记事」生成分段总结文本。
# 返回分段文本（【项目】【需求】【记事】【结果】），供 UI 回填 / 直接入库。
hf_project_summary_build <- function(project_id) {
  p <- hf_project_get(project_id)
  if (is.null(p) || nrow(p) == 0) return(list(success = FALSE, message = "项目不存在"))
  p <- p[1, ]
  reqs <- hf_project_req_list(project_id)
  notes <- hf_project_note_list(project_id)
  lines <- c()
  lines <- c(lines, sprintf("【项目】%s（%s）", hf_or(p$name, ""), hf_or(p$project_no, "")))
  desc <- hf_or(p$description, "")
  if (nzchar(trimws(desc))) lines <- c(lines, paste0("    描述：", desc))
  lines <- c(lines, sprintf("    状态：%s · 优先级：%s · 周期：%s ~ %s",
                            hf_or(p$status, "planning"), hf_or(p$priority, "中"),
                            hf_or(p$start_date, "未定"), hf_or(p$end_date, "未定")))
  lines <- c(lines, "")
  lines <- c(lines, "【需求】")
  if (is.null(reqs) || nrow(reqs) == 0) {
    lines <- c(lines, "    （暂无需求）")
  } else {
    done <- sum(reqs$status == "已完成", na.rm = TRUE)
    lines <- c(lines, sprintf("    共 %d 条，已完成 %d 条", nrow(reqs), done))
    for (i in seq_len(nrow(reqs))) {
      r <- reqs[i, ]
      lines <- c(lines, sprintf("    %d. [%s] %s（优先级 %s，负责人 %s）",
                                i, hf_or(r$status, "进行中"), hf_or(r$title, ""),
                                hf_or(r$priority, "中"), hf_or(r$owner, "未指定")))
    }
  }
  lines <- c(lines, "")
  lines <- c(lines, "【记事】")
  if (is.null(notes) || nrow(notes) == 0) {
    lines <- c(lines, "    （暂无记事）")
  } else {
    st_cn <- function(s) switch(hf_or(s, "pending"), pending = "待处理", in_progress = "进行中", done = "已完成", s)
    lines <- c(lines, sprintf("    共 %d 条", nrow(notes)))
    for (i in seq_len(nrow(notes))) {
      n <- notes[i, ]
      lines <- c(lines, sprintf("    %d. [%s] %s", i, st_cn(n$status), hf_or(n$title, "")))
    }
  }
  lines <- c(lines, "")
  lines <- c(lines, "【结果】")
  lines <- c(lines, sprintf("    汇总时间：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  list(success = TRUE, title = paste0("项目总结 · ", hf_or(p$name, "")),
       content = paste(lines, collapse = "\n"))
}

# ── 对外接口透传（供 hf_call("project", ...) 跨环节调用）──
hf_project_api_list       <- function() hf_project_list()
hf_project_api_get        <- function(id) hf_project_get(id)
hf_project_api_add        <- function(name, description = "", priority = "中", status = "planning",
                                      start_date = "", end_date = "", created_by = NULL)
  hf_project_add(name, description, priority, status, start_date, end_date, created_by)
hf_project_api_update     <- function(id, name, description = "", priority = "中", status = "planning",
                                      start_date = "", end_date = "")
  hf_project_update(id, name, description, priority, status, start_date, end_date)
hf_project_api_delete     <- function(id) hf_project_delete(id)

hf_project_req_api_list   <- function(project_id = NULL) hf_project_req_list(project_id)
hf_project_req_api_add    <- function(project_id, title, description = "", status = "进行中",
                                      priority = "中", owner = "", due_date = "", created_by = NULL)
  hf_project_req_add(project_id, title, description, status, priority, owner, due_date, created_by)
hf_project_req_api_update <- function(id, title, description = "", status = "进行中",
                                      priority = "中", owner = "", due_date = "")
  hf_project_req_update(id, title, description, status, priority, owner, due_date)
hf_project_req_api_delete <- function(id) hf_project_req_delete(id)

hf_project_note_api_list  <- function(project_id = NULL) hf_project_note_list(project_id)
hf_project_note_api_add   <- function(project_id, title, content = "", status = "pending",
                                      priority = "中", created_by = NULL)
  hf_project_note_add(project_id, title, content, status, priority, created_by)
hf_project_note_api_move  <- function(id, status) hf_project_note_move(id, status)
hf_project_note_api_delete <- function(id) hf_project_note_delete(id)

hf_project_note_comment_api_list  <- function(note_id) hf_project_note_comment_list(note_id)
hf_project_note_comment_api_add   <- function(note_id, content, parent_id = NULL, created_by = NULL)
  hf_project_note_comment_add(note_id, content, parent_id, created_by)
hf_project_note_comment_api_delete <- function(id) hf_project_note_comment_delete(id)

hf_project_summary_api_list <- function(project_id = NULL) hf_project_summary_list(project_id)
hf_project_summary_api_add  <- function(project_id, title, content = "", summary_date = NULL, created_by = NULL)
  hf_project_summary_add(project_id, title, content, summary_date, created_by)
hf_project_summary_api_delete <- function(id) hf_project_summary_delete(id)
hf_project_summary_api_build  <- function(project_id) hf_project_summary_build(project_id)

hf_project_solution_api_list   <- function(project_id = NULL) hf_project_solution_list(project_id)
hf_project_solution_api_add    <- function(project_id, title, content = "", category = "技术方案",
                                          related_project = "", created_by = NULL)
  hf_project_solution_add(project_id, title, content, category, related_project, created_by)
hf_project_solution_api_update <- function(id, title, content = "", category = "技术方案", related_project = "")
  hf_project_solution_update(id, title, content, category, related_project)
hf_project_solution_api_delete <- function(id) hf_project_solution_delete(id)

hf_register(HF_MOD_PROJECT, hf_make_api(
  "project",
  list = hf_project_api_list,
  get = hf_project_api_get,
  add = hf_project_api_add,
  update = hf_project_api_update,
  delete = hf_project_api_delete,
  req_list = hf_project_req_api_list,
  req_add = hf_project_req_api_add,
  req_update = hf_project_req_api_update,
  req_delete = hf_project_req_api_delete,
  note_list = hf_project_note_api_list,
  note_add = hf_project_note_api_add,
  note_move = hf_project_note_api_move,
  note_delete = hf_project_note_api_delete,
  note_comment_list = hf_project_note_comment_api_list,
  note_comment_add = hf_project_note_comment_api_add,
  note_comment_delete = hf_project_note_comment_api_delete,
  summary_list = hf_project_summary_api_list,
  summary_add = hf_project_summary_api_add,
  summary_delete = hf_project_summary_api_delete,
  summary_build = hf_project_summary_api_build,
  solution_list = hf_project_solution_api_list,
  solution_add = hf_project_solution_api_add,
  solution_update = hf_project_solution_api_update,
  solution_delete = hf_project_solution_api_delete
))
