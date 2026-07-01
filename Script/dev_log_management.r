##################
# 开发日志模块 — 数据层
# 自动记录每次开发的需求、方案、结果
##################

# 获取所有开发日志（可筛选模块/日期）
dev_log_get_all <- function(module = NULL, from_date = NULL, to_date = NULL) {
  con <- db_connect()
  tryCatch({
    sql <- "SELECT * FROM dev_logs WHERE 1=1"
    if (!is.null(module) && module != "") {
      sql <- paste0(sql, sprintf(" AND module = '%s'", module))
    }
    if (!is.null(from_date)) {
      sql <- paste0(sql, sprintf(" AND DATE(created_at) >= '%s'", from_date))
    }
    if (!is.null(to_date)) {
      sql <- paste0(sql, sprintf(" AND DATE(created_at) <= '%s'", to_date))
    }
    sql <- paste0(sql, " ORDER BY created_at DESC")
    dbGetQuery(con, sql)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 写入一条开发日志
dev_log_add <- function(module, title, requirement, solution, result, code_snippet = NULL, files_changed = NULL, operator = NULL) {
  con <- db_connect()
  tryCatch({
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    op <- if (is.null(operator) || length(operator) == 0) "系统" else operator$username[1] %||% "系统"
    dbExecute(con, sprintf(
      "INSERT INTO dev_logs (module, title, requirement, solution, result, code_snippet, files_changed, created_by, created_at)
       VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
      gsub("'","''", module %||% ""),
      gsub("'","''", title %||% ""),
      gsub("'","''", requirement %||% ""),
      gsub("'","''", solution %||% ""),
      gsub("'","''", result %||% ""),
      gsub("'","''", code_snippet %||% ""),
      gsub("'","''", files_changed %||% ""),
      gsub("'","''", op),
      now))
    log_user_operation("开发日志-新增", title, op)
    list(success = TRUE, message = "开发日志已记录")
  }, error = function(e) list(success = FALSE, message = paste("记录失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 获取所有不重复的模块名（用于筛选下拉）
dev_log_get_modules <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT DISTINCT module FROM dev_logs WHERE module IS NOT NULL ORDER BY module")$module
  }, error = function(e) character(0), finally = { db_disconnect(con) })
}

# 统计（总数 + 今天新增 + 按模块）
dev_log_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y-%m-%d")
    total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM dev_logs")$n[1]
    today_n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM dev_logs WHERE DATE(created_at) = '%s'", today))$n[1]
    by_module <- dbGetQuery(con, "SELECT module, COUNT(*) AS n FROM dev_logs GROUP BY module ORDER BY n DESC")
    list(total = total %||% 0L, today = today_n %||% 0L, by_module = by_module)
  }, error = function(e) list(total = 0L, today = 0L, by_module = data.frame()),
  finally = { db_disconnect(con) })
}
