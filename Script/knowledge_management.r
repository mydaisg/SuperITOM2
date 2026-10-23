##################
# 知识库模块 — 数据层
# 需求：方案模块新增「知识」Tab（后续独立成知识模块），知识库管理。
# 本次 MVP：分类/标签 + 文档(TXT/MD/手动录入) + 文本分片 + 关键词检索 + 日志。
# 预留（后续迭代）：语义/向量检索（需 embedding 依赖）、PDF/DOCX 解析（需 pdftools/officer）。
# 表：kb_categories / kb_documents / kb_chunks / kb_tags（自包含建表，不改 global.R）
##################

KB_FILE_DIR_DEFAULT <- "KB_files"

# ── 自包含建表 ──
kb_table_init <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS kb_categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    parent_id INTEGER,
    sort_order INTEGER DEFAULT 0,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS kb_documents (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    doc_no TEXT,
    title TEXT NOT NULL,
    category_id INTEGER,
    tags TEXT,
    summary TEXT,
    content TEXT,
    source_type TEXT DEFAULT 'manual',
    file_name TEXT,
    file_path TEXT,
    file_size INTEGER DEFAULT 0,
    chunk_count INTEGER DEFAULT 0,
    status TEXT DEFAULT 'active',
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS kb_chunks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    document_id INTEGER NOT NULL,
    chunk_index INTEGER,
    content TEXT
  )")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_kb_chunks_doc ON kb_chunks(document_id)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS kb_tags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    tag TEXT NOT NULL UNIQUE,
    created_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  invisible(TRUE)
}

# ── 配置（分片长度/重叠度/存储目录，存 system_config）──
kb_get_config <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  get <- function(k, d) {
    r <- dbGetQuery(con, sprintf("SELECT config_value FROM system_config WHERE config_key='%s'", k))
    if (nrow(r) == 0 || is.na(r$config_value[1]) || r$config_value[1] == "") d else r$config_value[1]
  }
  list(
    chunk_size = as.integer(get("kb_chunk_size", "500") %||% 500),
    chunk_overlap = as.integer(get("kb_chunk_overlap", "50") %||% 50),
    file_dir = get("kb_file_dir", KB_FILE_DIR_DEFAULT),
    topn = as.integer(get("kb_topn", "5") %||% 5)
  )
}

kb_set_config <- function(chunk_size = NULL, chunk_overlap = NULL, file_dir = NULL, topn = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  set <- function(k, v) {
    if (is.null(v)) return()
    ex <- dbGetQuery(con, sprintf("SELECT id FROM system_config WHERE config_key='%s'", k))
    if (nrow(ex) > 0) {
      dbExecute(con, sprintf("UPDATE system_config SET config_value='%s', updated_at=datetime('now','localtime') WHERE config_key='%s'", gsub("'","''",as.character(v)), k))
    } else {
      dbExecute(con, sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('%s','%s','知识库配置')", k, gsub("'","''",as.character(v))))
    }
  }
  set("kb_chunk_size", chunk_size)
  set("kb_chunk_overlap", chunk_overlap)
  set("kb_file_dir", file_dir)
  set("kb_topn", topn)
  list(success = TRUE, message = "配置已保存")
}

# ── 文件存储目录 ──
kb_file_dir <- function() {
  cfg <- kb_get_config()
  d <- cfg$file_dir %||% KB_FILE_DIR_DEFAULT
  if (!grepl("^[A-Za-z]:[/\\\\]", d) && !grepl("^/", d)) {
    d <- file.path(getwd(), d)
  }
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# ── 编号生成 KB+YYYYMMDD+3位 ──
kb_doc_gen_no <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  today <- format(Sys.Date(), "%Y%m%d")
  prefix <- paste0("KB", today)
  ex <- dbGetQuery(con, sprintf("SELECT doc_no FROM kb_documents WHERE doc_no LIKE '%s%%' ORDER BY doc_no DESC LIMIT 1", prefix))
  seq <- if (nrow(ex) == 0 || is.na(ex$doc_no[1])) 1L else as.integer(substr(ex$doc_no[1], nchar(prefix) + 1, nchar(prefix) + 3)) + 1L
  sprintf("%s%03d", prefix, seq)
}

# ── 文本分片（纯 R，按字符 + 重叠）──
kb_chunk_text <- function(text, chunk_size = 500, overlap = 50) {
  if (is.null(text) || !nzchar(text)) return(character(0))
  text <- gsub("\r\n", "\n", text)
  n <- nchar(text)
  if (n <= chunk_size) return(text)
  chunks <- character(0)
  start <- 1L
  while (start <= n) {
    end <- min(start + chunk_size - 1L, n)
    chunks <- c(chunks, substr(text, start, end))
    if (end >= n) break
    start <- start + chunk_size - overlap
    if (start <= end) start <- end + 1L  # 防止死循环
  }
  chunks
}

# ── 重建文档分片 ──
kb_rebuild_chunks <- function(con, doc_id, content) {
  dbExecute(con, sprintf("DELETE FROM kb_chunks WHERE document_id = %d", as.integer(doc_id)))
  cfg <- kb_get_config()
  chunks <- kb_chunk_text(content, cfg$chunk_size, cfg$chunk_overlap)
  n <- 0L
  if (length(chunks) > 0) {
    for (i in seq_along(chunks)) {
      dbExecute(con, sprintf("INSERT INTO kb_chunks (document_id, chunk_index, content) VALUES (%d, %d, '%s')",
        as.integer(doc_id), i, gsub("'","''",chunks[i])))
    }
    n <- length(chunks)
  }
  dbExecute(con, sprintf("UPDATE kb_documents SET chunk_count=%d, updated_at=datetime('now','localtime') WHERE id=%d", n, as.integer(doc_id)))
  n
}

# ── 分类 CRUD ──
kb_cat_get_all <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbGetQuery(con, "SELECT c.*, (SELECT COUNT(*) FROM kb_documents d WHERE d.category_id=c.id) AS doc_count FROM kb_categories c ORDER BY c.sort_order, c.id")
}

kb_cat_add <- function(name, parent_id = NULL, sort_order = 0) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, sprintf("INSERT INTO kb_categories (name, parent_id, sort_order) VALUES ('%s',%s,%d)",
    gsub("'","''",name), if (is.null(parent_id) || is.na(parent_id)) "NULL" else as.character(as.integer(parent_id)), as.integer(sort_order %||% 0)))
  list(success = TRUE, message = "分类已添加")
}

kb_cat_update <- function(id, name = NULL, sort_order = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  sets <- "updated_at=datetime('now','localtime')"
  if (!is.null(name)) sets <- paste0(sets, sprintf(", name='%s'", gsub("'","''",name)))
  if (!is.null(sort_order)) sets <- paste0(sets, sprintf(", sort_order=%d", as.integer(sort_order)))
  dbExecute(con, sprintf("UPDATE kb_categories SET %s WHERE id=%d", sets, as.integer(id)))
  list(success = TRUE, message = "分类已更新")
}

# 删除分类：mode='move' 移动文档到 move_to；mode='delete' 一并删除文档
kb_cat_delete <- function(id, mode = "move", move_to = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  docs <- dbGetQuery(con, sprintf("SELECT id, file_path FROM kb_documents WHERE category_id=%d", as.integer(id)))
  if (mode == "move") {
    target <- if (is.null(move_to) || is.na(move_to) || move_to == "") "NULL" else as.character(as.integer(move_to))
    dbExecute(con, sprintf("UPDATE kb_documents SET category_id=%s, updated_at=datetime('now','localtime') WHERE category_id=%d", target, as.integer(id)))
  } else {
    for (i in seq_len(nrow(docs))) {
      if (!is.na(docs$file_path[i]) && docs$file_path[i] != "" && file.exists(docs$file_path[i])) try(unlink(docs$file_path[i]), silent = TRUE)
      dbExecute(con, sprintf("DELETE FROM kb_chunks WHERE document_id=%d", docs$id[i]))
    }
    dbExecute(con, sprintf("DELETE FROM kb_documents WHERE category_id=%d", as.integer(id)))
  }
  dbExecute(con, sprintf("DELETE FROM kb_categories WHERE id=%d", as.integer(id)))
  list(success = TRUE, message = "分类已删除")
}

# ── 文档 CRUD ──
kb_doc_get_all <- function(category_id = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  sql <- "SELECT d.*, c.name AS category_name FROM kb_documents d LEFT JOIN kb_categories c ON d.category_id=c.id"
  if (!is.null(category_id) && !is.na(category_id) && category_id != "") {
    sql <- paste0(sql, sprintf(" WHERE d.category_id=%d", as.integer(category_id)))
  }
  sql <- paste0(sql, " ORDER BY d.updated_at DESC")
  dbGetQuery(con, sql)
}

kb_doc_get_by_id <- function(id) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  r <- dbGetQuery(con, sprintf("SELECT d.*, c.name AS category_name FROM kb_documents d LEFT JOIN kb_categories c ON d.category_id=c.id WHERE d.id=%d", as.integer(id)))
  if (nrow(r) == 0) NULL else r
}

kb_doc_add <- function(title, category_id = NULL, tags = "", summary = "", content = "",
  source_type = "manual", file_name = NULL, file_path = NULL, file_size = 0, created_by = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  doc_no <- kb_doc_gen_no()
  dbExecute(con, sprintf(
    "INSERT INTO kb_documents (doc_no, title, category_id, tags, summary, content, source_type, file_name, file_path, file_size, created_by)
     VALUES ('%s','%s',%s,'%s','%s','%s','%s','%s','%s',%d,%s)",
    doc_no, gsub("'","''",title),
    if (is.null(category_id) || is.na(category_id) || category_id == "") "NULL" else as.character(as.integer(category_id)),
    gsub("'","''",tags %||% ""), gsub("'","''",summary %||% ""), gsub("'","''",content %||% ""),
    gsub("'","''",source_type %||% "manual"), gsub("'","''",file_name %||% ""), gsub("'","''",file_path %||% ""),
    as.integer(file_size %||% 0),
    if (is.null(created_by)) "NULL" else as.character(as.integer(created_by))))
  id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  kb_rebuild_chunks(con, id, content)
  kb_tag_remember(tags)
  kb_log("上传/录入", title, created_by, sprintf("文档 %s 已创建，分片 %d 个", doc_no, dbGetQuery(con, sprintf("SELECT chunk_count FROM kb_documents WHERE id=%d", id))$chunk_count[1] %||% 0))
  list(success = TRUE, id = id, doc_no = doc_no, message = paste("已创建", doc_no))
}

kb_doc_update <- function(id, title = NULL, category_id = NULL, tags = NULL, summary = NULL, content = NULL, changed_by = NULL) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  sets <- "updated_at=datetime('now','localtime')"
  if (!is.null(title)) sets <- paste0(sets, sprintf(", title='%s'", gsub("'","''",title)))
  if (!is.null(category_id)) sets <- paste0(sets, sprintf(", category_id=%s", if (is.na(category_id) || category_id == "") "NULL" else as.character(as.integer(category_id))))
  if (!is.null(tags)) sets <- paste0(sets, sprintf(", tags='%s'", gsub("'","''",tags)))
  if (!is.null(summary)) sets <- paste0(sets, sprintf(", summary='%s'", gsub("'","''",summary)))
  if (!is.null(content)) sets <- paste0(sets, sprintf(", content='%s'", gsub("'","''",content)))
  dbExecute(con, sprintf("UPDATE kb_documents SET %s WHERE id=%d", sets, as.integer(id)))
  # 正文变更则重新分片
  if (!is.null(content)) kb_rebuild_chunks(con, id, content)
  if (!is.null(tags)) kb_tag_remember(tags)
  list(success = TRUE, message = "已更新")
}

kb_doc_delete <- function(id) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  r <- dbGetQuery(con, sprintf("SELECT file_path FROM kb_documents WHERE id=%d", as.integer(id)))
  if (nrow(r) > 0 && !is.na(r$file_path[1]) && r$file_path[1] != "" && file.exists(r$file_path[1])) try(unlink(r$file_path[1]), silent = TRUE)
  dbExecute(con, sprintf("DELETE FROM kb_chunks WHERE document_id=%d", as.integer(id)))
  dbExecute(con, sprintf("DELETE FROM kb_documents WHERE id=%d", as.integer(id)))
  list(success = TRUE, message = "已删除")
}

# 重新分片（重新向量化占位入口）
kb_doc_rechunk <- function(id) {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  r <- dbGetQuery(con, sprintf("SELECT content FROM kb_documents WHERE id=%d", as.integer(id)))
  if (nrow(r) == 0) return(list(success = FALSE, message = "文档不存在"))
  n <- kb_rebuild_chunks(con, id, r$content[1] %||% "")
  list(success = TRUE, message = sprintf("已重新分片，共 %d 个分片", n))
}

# ── 标签 ──
kb_tag_get_all <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbGetQuery(con, "SELECT tag FROM kb_tags ORDER BY id DESC LIMIT 200")$tag
}

kb_tag_remember <- function(tags) {
  if (is.null(tags) || !nzchar(trimws(tags))) return(invisible(NULL))
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  for (t in strsplit(tags, "[,，;；]")[[1]]) {
    t <- trimws(t)
    if (nzchar(t)) dbExecute(con, sprintf("INSERT OR IGNORE INTO kb_tags (tag) VALUES ('%s')", gsub("'","''",t)))
  }
  invisible(NULL)
}

# ── 检索 ──
# 关键词检索：标题/简介匹配（列表搜索）
kb_search_docs <- function(keyword, category_id = NULL) {
  if (is.null(keyword) || !nzchar(trimws(keyword))) return(kb_doc_get_all(category_id))
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  kw <- gsub("'","''", trimws(keyword))
  sql <- sprintf("SELECT d.*, c.name AS category_name FROM kb_documents d LEFT JOIN kb_categories c ON d.category_id=c.id WHERE (d.title LIKE '%%%s%%' OR d.summary LIKE '%%%s%%' OR d.tags LIKE '%%%s%%')", kw, kw, kw)
  if (!is.null(category_id) && !is.na(category_id) && category_id != "") sql <- paste0(sql, sprintf(" AND d.category_id=%d", as.integer(category_id)))
  sql <- paste0(sql, " ORDER BY d.updated_at DESC")
  dbGetQuery(con, sql)
}

# 正文片段检索：基于分片，返回来源文档 + 片段
kb_search_chunks <- function(keyword, topn = 5) {
  if (is.null(keyword) || !nzchar(trimws(keyword))) return(data.frame())
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  kw <- gsub("'","''", trimws(keyword))
  topn <- as.integer(topn %||% 5)
  r <- dbGetQuery(con, sprintf(
    "SELECT ch.id AS chunk_id, ch.document_id, ch.content, d.title, d.doc_no, c.name AS category_name
     FROM kb_chunks ch
     JOIN kb_documents d ON ch.document_id = d.id
     LEFT JOIN kb_categories c ON d.category_id = c.id
     WHERE ch.content LIKE '%%%s%%'
     ORDER BY d.updated_at DESC LIMIT %d", kw, topn))
  if (nrow(r) == 0) return(data.frame())
  r
}

# ── 日志（文件日志）──
kb_log <- function(action, target, operator = NULL, details = "") {
  tryCatch({
    log_dir <- file.path(getwd(), "Logs")
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    line <- sprintf("[%s] 操作:%s, 对象:%s, 操作者:%s, 详情:%s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), action, target %||% "",
      if (is.null(operator)) "系统" else as.character(operator), details %||% "")
    cat(line, "\n", file = file.path(log_dir, "kb_operations.log"), append = TRUE)
  }, error = function(e) NULL)
  invisible(NULL)
}

kb_log_get_recent <- function(n = 100) {
  f <- file.path(getwd(), "Logs", "kb_operations.log")
  if (!file.exists(f)) return(character(0))
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0) return(character(0))
  n <- min(n, length(lines))
  tail(lines, n)
}
