##################
# 行业模块 — 数据层（框架）
# 行业情报搜集、爬虫、可被调用的库、可推送的库、可还原原文的HTML
##################

# 行业情报分类列表（框架种子）
industry_get_categories <- function() {
  c("政策法规", "市场动态", "技术趋势", "竞争情报", "行业报告")
}

# 获取行业情报列表（框架占位，后续扩展爬虫/库）
industry_get_all <- function(category = NULL, keyword = NULL) {
  con <- db_connect()
  tryCatch({
    q <- "SELECT * FROM industry_intelligence WHERE 1=1"
    if (!is.null(category) && nzchar(category)) {
      q <- sprintf("%s AND category='%s'", q, gsub("'", "''", category))
    }
    if (!is.null(keyword) && nzchar(keyword)) {
      q <- sprintf("%s AND (title LIKE '%%%s%%' OR content LIKE '%%%s%%')", q,
                   gsub("'", "''", keyword), gsub("'", "''", keyword))
    }
    q <- paste0(q, " ORDER BY created_at DESC")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 添加行业情报记录
industry_add <- function(title, content, category, source, url = NULL, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    no <- industry_generate_no()
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO industry_intelligence (intel_no, title, content, category, source, url, created_by, created_at) VALUES ('%s','%s','%s','%s','%s','%s',%s,'%s')",
      no, gsub("'","''",title), gsub("'","''",content%||%""), gsub("'","''",category%||%""),
      gsub("'","''",source%||%""), gsub("'","''",url%||%""),
      if(is.null(created_by)) "NULL" else as.character(created_by), now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, intel_no = no, message = paste("行业情报", no, "已创建"))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 生成行业情报编号
industry_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("IND", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT intel_no FROM industry_intelligence WHERE intel_no LIKE '%s%%' ORDER BY intel_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$intel_no[1], 12, 14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "IND00000000001",
  finally = { db_disconnect(con) })
}

# 行业情报统计
industry_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) n FROM industry_intelligence")$n[1]
    cats <- dbGetQuery(con, "SELECT category, COUNT(*) n FROM industry_intelligence GROUP BY category")
    list(total = total, by_category = cats)
  }, error = function(e) list(total = 0, by_category = data.frame()),
  finally = { db_disconnect(con) })
}
