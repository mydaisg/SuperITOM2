# 方案模块 - 数据层

solution_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT s.*, u.display_name as creator_name FROM solutions s LEFT JOIN users u ON s.created_by=u.id ORDER BY s.updated_at DESC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

solution_add <- function(title, content, category = NULL, related_project = NULL, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    no <- solution_gen_no()
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO solutions (sol_no, title, content, category, related_project, created_by, created_at, updated_at) VALUES ('%s','%s','%s','%s','%s',%s,'%s','%s')",
      no, gsub("'","''",title), gsub("'","''",content%||%""),
      gsub("'","''",category%||%""), gsub("'","''",related_project%||%""),
      if(is.null(created_by)) "NULL" else as.character(created_by), now, now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success=TRUE, id=id, sol_no=no, message=paste("方案",no,"已创建"))
  }, error = function(e) list(success=FALSE, message=e$message),
  finally = { db_disconnect(con) })
}

solution_gen_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("SOL", format(Sys.Date(),"%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT sol_no FROM solutions WHERE sol_no LIKE '%s%%' ORDER BY sol_no DESC LIMIT 1", prefix))
    if (nrow(existing)==0) return(paste0(prefix,"001"))
    last <- as.integer(substr(existing$sol_no[1],12,14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "SOL00000000001",
  finally = { db_disconnect(con) })
}

solution_update <- function(id, title = NULL, content = NULL, category = NULL, related_project = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(title)) sets <- c(sets, sprintf("title='%s'", gsub("'","''",title)))
    if (!is.null(content)) sets <- c(sets, sprintf("content='%s'", gsub("'","''",content)))
    if (!is.null(category)) sets <- c(sets, sprintf("category='%s'", gsub("'","''",category)))
    if (!is.null(related_project)) sets <- c(sets, sprintf("related_project='%s'", gsub("'","''",related_project)))
    if (length(sets)==0) return(list(success=FALSE, message="无变更"))
    dbExecute(con, sprintf("UPDATE solutions SET %s, updated_at=datetime('now','localtime') WHERE id=%d",
      paste(sets, collapse=","), as.integer(id)))
    list(success=TRUE, message="已更新")
  }, error = function(e) list(success=FALSE, message=e$message),
  finally = { db_disconnect(con) })
}

# 净化方案 HTML：返回 list(styles="CSS内容(无style标签)", body="HTML内容")
sol_sanitize_html <- function(raw) {
  if (is.null(raw) || nchar(raw) == 0) return(list(styles = "", body = ""))
  txt <- raw

  # 1. 提取并清理 <style> 块 → 输出纯 CSS 文本（不含 <style> 标签）
  css_text <- ""
  style_re <- gregexpr("(?s)<style[^>]*>(.*?)</style>", txt, ignore.case = TRUE, perl = TRUE)
  if (style_re[[1]][1] > 0) {
    style_texts <- regmatches(txt, style_re)[[1]]
    for (s in style_texts) {
      inner <- gsub("</?style[^>]*>", "", s, ignore.case = TRUE)
      inner <- gsub("\\*\\s*\\{[^}]*\\}", "/* removed global * */", inner, ignore.case = TRUE)
      inner <- gsub("\\bhtml\\b\\s*,\\s*body\\s*\\{[^}]*\\}", "/* removed html,body */", inner, ignore.case = TRUE)
      inner <- gsub("\\bbody\\b\\s*\\{[^}]*\\}", "/* removed body */", inner, ignore.case = TRUE)
      inner <- gsub("\\bhtml\\b\\s*\\{[^}]*\\}", "/* removed html */", inner, ignore.case = TRUE)
      css_text <- paste0(css_text, inner, "\n")
    }
    txt <- gsub("(?s)<style[^>]*>.*?</style>", "", txt, ignore.case = TRUE, perl = TRUE)
  }

  # 2. 提取 <body> 内容
  if (grepl("<body", txt, ignore.case = TRUE)) {
    body_match <- regexpr("(?s)<body[^>]*>(.*)</body>", txt, ignore.case = TRUE, perl = TRUE)
    if (body_match > 0) {
      # 用捕获组提取 body 内部
      caps <- attr(body_match, "capture.start")
      if (!is.null(caps) && caps[1] > 0) {
        cap_len <- attr(body_match, "capture.length")[1]
        txt <- substr(txt, caps[1], caps[1] + cap_len - 1L)
      }
    }
  }
  # 保险：再去一次 body 标签
  txt <- gsub("<body[^>]*>", "", txt, ignore.case = TRUE)
  txt <- gsub("</body>", "", txt, ignore.case = TRUE)

  # 3. 去掉文档级标签
  txt <- gsub("<!DOCTYPE[^>]*>", "", txt, ignore.case = TRUE)
  txt <- gsub("<html[^>]*>", "", txt, ignore.case = TRUE)
  txt <- gsub("</html>", "", txt, ignore.case = TRUE)
  txt <- gsub("(?s)<head>.*?</head>", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("<meta[^>]*>", "", txt, ignore.case = TRUE)
  txt <- gsub("<title>.*?</title>", "", txt, ignore.case = TRUE)
  txt <- gsub("<link[^>]*>", "", txt, ignore.case = TRUE)

  # 4. 去除 <script>
  txt <- gsub("(?s)<script[^>]*>.*?</script>", "", txt, ignore.case = TRUE, perl = TRUE)

  # 5. 修复内联 onclick：替换外部函数调用为内联 DOM 操作
  txt <- gsub("onclick=\"toggleSec\\(this\\)\"",
    "onclick=\"this.nextElementSibling.classList.toggle('open')\"", txt)
  txt <- gsub("onclick=\"switchTab\\('[^']*'\\)\"",
    "onclick=\"\"", txt)  # switchTab 无脚本支持，禁用

  list(styles = trimws(css_text), body = trimws(txt))
}

solution_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM solutions WHERE id=%d", as.integer(id)))
    list(success=TRUE, message="已删除")
  }, error = function(e) list(success=FALSE, message=e$message),
  finally = { db_disconnect(con) })
}
