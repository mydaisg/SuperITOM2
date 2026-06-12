# 集成模块 - 数据层

integration_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM integrations ORDER BY name")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

integration_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM integrations WHERE id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

integration_add <- function(name, base_url, auth_header = NULL, auth_value = NULL, method = "POST", description = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO integrations (name, base_url, auth_header, auth_value, method, description) VALUES ('%s','%s',%s,%s,'%s',%s)",
      gsub("'","''",name), gsub("'","''",base_url),
      if(is.null(auth_header)||auth_header=="") "NULL" else sprintf("'%s'",gsub("'","''",auth_header)),
      if(is.null(auth_value)||auth_value=="") "NULL" else sprintf("'%s'",gsub("'","''",auth_value)),
      method,
      if(is.null(description)||description=="") "NULL" else sprintf("'%s'",gsub("'","''",description))))
    list(success=TRUE, message="已添加")
  }, error=function(e) list(success=FALSE, message=paste("失败:",e$message)),
  finally={db_disconnect(con)})
}

integration_update <- function(id, name = NULL, base_url = NULL, auth_header = NULL, auth_value = NULL, method = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(name))        sets <- paste0(sets, sprintf(", name='%s'", gsub("'","''",name)))
    if (!is.null(base_url))    sets <- paste0(sets, sprintf(", base_url='%s'", gsub("'","''",base_url)))
    if (!is.null(auth_header)) sets <- paste0(sets, sprintf(", auth_header='%s'", gsub("'","''",auth_header)))
    if (!is.null(auth_value))  sets <- paste0(sets, sprintf(", auth_value='%s'", gsub("'","''",auth_value)))
    if (!is.null(method))      sets <- paste0(sets, sprintf(", method='%s'", method))
    if (!is.null(description)) sets <- paste0(sets, sprintf(", description='%s'", gsub("'","''",description)))
    dbExecute(con, sprintf("UPDATE integrations SET %s WHERE id=%d", sets, as.integer(id)))
    list(success=TRUE, message="已更新")
  }, error=function(e) list(success=FALSE, message=paste("失败:",e$message)),
  finally={db_disconnect(con)})
}

integration_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM integrations WHERE id=%d", as.integer(id)))
    list(success=TRUE, message="已删除")
  }, error=function(e) list(success=FALSE, message=paste("失败:",e$message)),
  finally={db_disconnect(con)})
}

# 执行 HTTP 请求（使用 base R，无需额外包）
integration_execute <- function(integ_id, json_body) {
  integ <- integration_get_by_id(integ_id)
  if (is.null(integ)) return(list(success=FALSE, message="集成配置不存在"))
  tryCatch({
    start <- Sys.time()
    # 使用 httr 包（Shiny 常用）
    if (!requireNamespace("httr", quietly=TRUE)) {
      return(list(success=FALSE, message="请安装 httr 包: install.packages('httr')"))
    }
    url <- integ$base_url[1]
    headers <- c("Content-Type" = "application/json", "Accept" = "application/json")
    if (!is.na(integ$auth_header[1]) && integ$auth_header[1] != "" && !is.na(integ$auth_value[1]) && integ$auth_value[1] != "") {
      headers <- c(headers, setNames(integ$auth_value[1], integ$auth_header[1]))
    }
    if (integ$method[1] == "GET") {
      resp <- httr::GET(url, httr::add_headers(.headers=headers))
    } else {
      resp <- httr::POST(url, body = json_body, httr::content_type("application/json"), httr::add_headers(.headers=headers))
    }
    elapsed <- round(as.numeric(difftime(Sys.time(), start, units="secs")) * 1000)
    body <- httr::content(resp, "text", encoding="UTF-8")
    list(success=TRUE, status_code=httr::status_code(resp), body=body, elapsed_ms=elapsed)
  }, error=function(e) list(success=FALSE, message=paste("请求失败:", e$message)))
}
