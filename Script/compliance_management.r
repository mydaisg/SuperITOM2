##################
# 合规模块 — 数据层（框架）
# 合规管理，IPO审计信息化的规则库、解决、计划、行动、持续改善
##################

# 合规规则库分类（种子，与 seed_compliance_rules 分类对齐）
compliance_get_categories <- function() {
  c("网络安全与数据安全", "数据安全与数据出境", "个人信息保护",
    "内部控制与审计", "财务信息化与电子凭证", "业务连续性与系统可靠性",
    "软件正版化与知识产权", "电子签名与电子合同", "招股书披露与监管科技")
}

# 获取合规规则列表（按 sort_order 排序）
compliance_get_all <- function(category = NULL) {
  con <- db_connect()
  tryCatch({
    q <- "SELECT * FROM compliance_rules WHERE 1=1"
    if (!is.null(category) && nzchar(category)) {
      q <- sprintf("%s AND category='%s'", q, gsub("'", "''", category))
    }
    q <- paste0(q, " ORDER BY sort_order, id")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取分类列表（含每类规则数）
compliance_get_categories_with_count <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT category, COUNT(*) n FROM compliance_rules GROUP BY category ORDER BY MIN(sort_order)")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 添加合规规则
compliance_add <- function(title, category, rule_content, law_basis = NULL, solution = NULL, plan = NULL, action = NULL, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    no <- compliance_generate_no()
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO compliance_rules (comp_no, title, category, rule_content, law_basis, solution, plan, action, created_by, created_at) VALUES ('%s','%s','%s','%s','%s','%s','%s','%s',%s,'%s')",
      no, gsub("'","''",title), gsub("'","''",category%||%""),
      gsub("'","''",rule_content%||%""), gsub("'","''",law_basis%||%""),
      gsub("'","''",solution%||%""), gsub("'","''",plan%||%""),
      gsub("'","''",action%||%""),
      if(is.null(created_by)) "NULL" else as.character(created_by), now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, comp_no = no, message = paste("合规规则", no, "已创建"))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 生成合规规则编号
compliance_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("CMP", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT comp_no FROM compliance_rules WHERE comp_no LIKE '%s%%' ORDER BY comp_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$comp_no[1], 12, 14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "CMP00000000001",
  finally = { db_disconnect(con) })
}
