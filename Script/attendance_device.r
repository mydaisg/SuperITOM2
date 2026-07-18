# 考勤设备模块 - 数据层

attendance_device_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM attendance_devices ORDER BY area, id")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

attendance_device_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM attendance_devices WHERE id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

attendance_device_add <- function(area, location, device_type, brand, quantity, applicable_users, special_users, remark) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO attendance_devices (area, location, device_type, brand, quantity, applicable_users, special_users, remark) VALUES (%s,%s,%s,%s,%d,%s,%s,%s)",
      if(is.null(area)||area=="") "NULL" else sprintf("'%s'",gsub("'","''",area)),
      if(is.null(location)||location=="") "NULL" else sprintf("'%s'",gsub("'","''",location)),
      if(is.null(device_type)||device_type=="") "NULL" else sprintf("'%s'",gsub("'","''",device_type)),
      if(is.null(brand)||brand=="") "NULL" else sprintf("'%s'",gsub("'","''",brand)),
      as.integer(quantity %||% 1),
      if(is.null(applicable_users)||applicable_users=="") "NULL" else sprintf("'%s'",gsub("'","''",applicable_users)),
      if(is.null(special_users)||special_users=="") "NULL" else sprintf("'%s'",gsub("'","''",special_users)),
      if(is.null(remark)||remark=="") "NULL" else sprintf("'%s'",gsub("'","''",remark))
    ))
    list(success = TRUE, message = "已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

attendance_device_update <- function(id, area, location, device_type, brand, quantity, applicable_users, special_users, remark) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(area))             sets <- paste0(sets, sprintf(", area='%s'", gsub("'","''",area)))
    if (!is.null(location))         sets <- paste0(sets, sprintf(", location='%s'", gsub("'","''",location)))
    if (!is.null(device_type))      sets <- paste0(sets, sprintf(", device_type='%s'", gsub("'","''",device_type)))
    if (!is.null(brand))            sets <- paste0(sets, sprintf(", brand='%s'", gsub("'","''",brand)))
    if (!is.null(quantity))         sets <- paste0(sets, sprintf(", quantity=%d", as.integer(quantity)))
    if (!is.null(applicable_users)) sets <- paste0(sets, sprintf(", applicable_users='%s'", gsub("'","''",applicable_users)))
    if (!is.null(special_users))    sets <- paste0(sets, sprintf(", special_users='%s'", gsub("'","''",special_users)))
    if (!is.null(remark))           sets <- paste0(sets, sprintf(", remark='%s'", gsub("'","''",remark)))
    dbExecute(con, sprintf("UPDATE attendance_devices SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

attendance_device_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM attendance_devices WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

attendance_device_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM attendance_devices")$cnt[1]
    total_qty <- dbGetQuery(con, "SELECT COALESCE(SUM(quantity),0) as cnt FROM attendance_devices")$cnt[1]
    face_count <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM attendance_devices WHERE device_type LIKE '%人脸%'")$cnt[1]
    fingerprint_count <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM attendance_devices WHERE device_type LIKE '%指纹%'")$cnt[1]
    zk_count <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM attendance_devices WHERE brand = '中控'")$cnt[1]
    dd_count <- dbGetQuery(con, "SELECT COUNT(*) as cnt FROM attendance_devices WHERE brand = '钉钉'")$cnt[1]
    list(total = total, total_qty = total_qty, face_count = face_count,
      fingerprint_count = fingerprint_count, zk_count = zk_count, dd_count = dd_count)
  }, error = function(e) list(total=0,total_qty=0,face_count=0,fingerprint_count=0,zk_count=0,dd_count=0),
  finally = { db_disconnect(con) })
}
