##################
# 治理模块 — 数据层（框架）
# 企业治理和风险管理（波特五力、电子设备行业生命周期、PESTEL 六维度）分析、行动、复盘
##################

# 治理分析框架（种子）
governance_get_frameworks <- function() {
  c("行业周期", "治理模型", "风险模型", "PESTEL 六维度", "PEST-SWOT")
}

# 获取治理条目列表（框架占位）
governance_get_all <- function(framework = NULL) {
  con <- db_connect()
  tryCatch({
    q <- "SELECT * FROM governance_items WHERE 1=1"
    if (!is.null(framework) && nzchar(framework)) {
      q <- sprintf("%s AND framework='%s'", q, gsub("'", "''", framework))
    }
    q <- paste0(q, " ORDER BY created_at DESC")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 添加治理条目
governance_add <- function(title, framework, analysis, action, review, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    no <- governance_generate_no()
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO governance_items (gov_no, title, framework, analysis, action, review, created_by, created_at) VALUES ('%s','%s','%s','%s','%s','%s',%s,'%s')",
      no, gsub("'","''",title), gsub("'","''",framework%||%""),
      gsub("'","''",analysis%||%""), gsub("'","''",action%||%""), gsub("'","''",review%||%""),
      if(is.null(created_by)) "NULL" else as.character(created_by), now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, gov_no = no, message = paste("治理条目", no, "已创建"))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 生成治理条目编号
governance_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("GOV", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT gov_no FROM governance_items WHERE gov_no LIKE '%s%%' ORDER BY gov_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$gov_no[1], 12, 14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "GOV00000000001",
  finally = { db_disconnect(con) })
}
