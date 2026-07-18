# 方案执行模块 - 数据层

exec_get_table <- function(tbl_name) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM %s ORDER BY id", tbl_name))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

exec_update_field <- function(tbl_name, id, field, value) {
  con <- db_connect()
  tryCatch({
    val <- if (is.null(value) || value == "") "NULL" else sprintf("'%s'", gsub("'","''", as.character(value)))
    dbExecute(con, sprintf("UPDATE %s SET %s = %s WHERE id = %d", tbl_name, field, val, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 测试用例专用：更新状态
exec_update_test_status <- function(tbl_name, id, status) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE %s SET status = '%s' WHERE id = %d", tbl_name, gsub("'","''",status), as.integer(id)))
    list(success = TRUE, message = "状态已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 统计函数
exec_get_stats <- function(tbl_name, status_field = NULL) {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM %s", tbl_name))$cnt[1]
    if (!is.null(status_field)) {
      done <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM %s WHERE %s IN ('已完成','已通过','已修复','通过')", tbl_name, status_field))$cnt[1]
      pending <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM %s WHERE %s IN ('待测试','待维护','待处理','未开始','进行中')", tbl_name, status_field))$cnt[1]
      list(total = total, done = done, pending = pending)
    } else {
      list(total = total)
    }
  }, error = function(e) list(total = 0), finally = { db_disconnect(con) })
}
