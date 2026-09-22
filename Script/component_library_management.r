##################
# 组件库模块 — 数据层（框架）
# 标准组件库列表，落实 HungFo 思想，本网站各模块调用，实现前后端分离、数据分离、配置可选独立
##################

# 组件分类（框架种子）
component_get_categories <- function() {
  c("基础组件", "表单组件", "数据展示", "导航组件", "反馈组件", "业务组件")
}

# 获取组件列表（框架占位）
component_get_all <- function(category = NULL) {
  con <- db_connect()
  tryCatch({
    q <- "SELECT * FROM component_library WHERE 1=1"
    if (!is.null(category) && nzchar(category)) {
      q <- sprintf("%s AND category='%s'", q, gsub("'", "''", category))
    }
    q <- paste0(q, " ORDER BY sort_order, id")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 添加组件
component_add <- function(name, category, description, usage, config_schema, created_by = NULL) {
  con <- db_connect()
  tryCatch({
    no <- component_generate_no()
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO component_library (comp_no, name, category, description, usage, config_schema, created_by, created_at) VALUES ('%s','%s','%s','%s','%s','%s',%s,'%s')",
      no, gsub("'","''",name), gsub("'","''",category%||%""),
      gsub("'","''",description%||%""), gsub("'","''",usage%||%""), gsub("'","''",config_schema%||%""),
      if(is.null(created_by)) "NULL" else as.character(created_by), now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, comp_no = no, message = paste("组件", no, "已登记"))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 生成组件编号
component_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- paste0("CPT", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf("SELECT comp_no FROM component_library WHERE comp_no LIKE '%s%%' ORDER BY comp_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$comp_no[1], 12, 14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "CPT00000000001",
  finally = { db_disconnect(con) })
}
