# 资产管理模块 - 数据层
# AST+YYYYMMDD+3位流水

##################
# 编号生成
##################
asset_generate_number <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("AST", today)
    existing <- dbGetQuery(con, sprintf("SELECT asset_no FROM assets WHERE asset_no LIKE '%s%%' ORDER BY asset_no DESC LIMIT 1", prefix))
    if (nrow(existing) > 0 && !is.na(existing$asset_no[1])) {
      last_seq <- as.integer(substr(existing$asset_no[1], nchar(prefix)+1, nchar(prefix)+3))
      seq <- if (is.na(last_seq)) 1 else last_seq + 1
    } else { seq <- 1 }
    sprintf("%s%03d", prefix, seq)
  }, finally = { db_disconnect(con) })
}

##################
# 获取所有资产
##################
asset_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT a.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM assets a LEFT JOIN users u ON a.created_by = u.id ORDER BY a.hostname ASC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

##################
# 获取单条
##################
asset_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT a.*, COALESCE(NULLIF(u.display_name,''), u.username) as creator_name FROM assets a LEFT JOIN users u ON a.created_by = u.id WHERE a.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

##################
# 新增资产
##################
asset_add <- function(hostname, ip_address = "", os = "", cpu = "", ram = "", disk = "",
  manufacturer = "", model = "", serial_number = "", location = "", department = "",
  notes = "", created_by = NULL) {
  con <- db_connect()
  tryCatch({
    asset_no <- asset_generate_number()
    query <- sprintf(
      "INSERT INTO assets (asset_no, hostname, ip_address, os, cpu, ram, disk, manufacturer, model, serial_number, location, department, notes, created_by)
       VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s',%s)",
      asset_no, gsub("'","''",hostname), gsub("'","''",ip_address), gsub("'","''",os),
      gsub("'","''",cpu), gsub("'","''",ram), gsub("'","''",disk),
      gsub("'","''",manufacturer), gsub("'","''",model), gsub("'","''",serial_number),
      gsub("'","''",location), gsub("'","''",department), gsub("'","''",notes),
      ifelse(is.null(created_by),"NULL",as.character(created_by)))
    dbExecute(con, query)
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, asset_no = asset_no, message = paste("已添加", asset_no))
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 更新资产
##################
asset_update <- function(id, hostname = NULL, ip_address = NULL, os = NULL, cpu = NULL,
  ram = NULL, disk = NULL, manufacturer = NULL, model = NULL, serial_number = NULL,
  location = NULL, department = NULL, status = NULL, notes = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(hostname))      sets <- paste0(sets, sprintf(", hostname='%s'", gsub("'","''",hostname)))
    if (!is.null(ip_address))    sets <- paste0(sets, sprintf(", ip_address='%s'", gsub("'","''",ip_address)))
    if (!is.null(os))            sets <- paste0(sets, sprintf(", os='%s'", gsub("'","''",os)))
    if (!is.null(cpu))           sets <- paste0(sets, sprintf(", cpu='%s'", gsub("'","''",cpu)))
    if (!is.null(ram))           sets <- paste0(sets, sprintf(", ram='%s'", gsub("'","''",ram)))
    if (!is.null(disk))          sets <- paste0(sets, sprintf(", disk='%s'", gsub("'","''",disk)))
    if (!is.null(manufacturer))  sets <- paste0(sets, sprintf(", manufacturer='%s'", gsub("'","''",manufacturer)))
    if (!is.null(model))         sets <- paste0(sets, sprintf(", model='%s'", gsub("'","''",model)))
    if (!is.null(serial_number)) sets <- paste0(sets, sprintf(", serial_number='%s'", gsub("'","''",serial_number)))
    if (!is.null(location))      sets <- paste0(sets, sprintf(", location='%s'", gsub("'","''",location)))
    if (!is.null(department))    sets <- paste0(sets, sprintf(", department='%s'", gsub("'","''",department)))
    if (!is.null(status))        sets <- paste0(sets, sprintf(", status='%s'", status))
    if (!is.null(notes))         sets <- paste0(sets, sprintf(", notes='%s'", gsub("'","''",notes)))
    dbExecute(con, sprintf("UPDATE assets SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 删除资产
##################
asset_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM assets WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 快速导入（从目标主机用 system2 采集信息）
##################
asset_scan <- function(hostname, ip = NULL) {
  target <- if (!is.null(ip) && ip != "") ip else hostname
  info <- list(hostname = hostname, ip_address = target, os = "", cpu = "", ram = "", disk = "")
  tryCatch({
    # 尝试通过 ping 确认在线
    ping_cmd <- if (.Platform$OS.type == "windows") {
      sprintf("ping -n 1 -w 1000 %s", target)
    } else { sprintf("ping -c 1 -W 1 %s", target) }
    ping_result <- system(ping_cmd, intern = TRUE, ignore.stderr = TRUE)
    online <- any(grepl("TTL=", ping_result, ignore.case = TRUE))
    if (!online) return(list(success = FALSE, message = paste(hostname, "不在线")))
    info$last_seen <- format(Sys.time(), "%Y-%m-%d %H:%M")
  }, error = function(e) {
    return(list(success = FALSE, message = paste("扫描失败:", e$message)))
  })
  list(success = TRUE, info = info, message = paste(hostname, "在线"))
}
