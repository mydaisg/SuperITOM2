# 工位图模块 - 数据层
# 层级：楼栋(building) → 楼层(floor) → 区域(zone) → 工位(seat)

##################
# 楼栋 CRUD
##################
building_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM seat_buildings ORDER BY name")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

building_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM seat_buildings WHERE id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

building_add <- function(name, description = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO seat_buildings (name, description) VALUES ('%s','%s')",
      gsub("'","''",name), gsub("'","''",description)))
    list(success = TRUE, message = paste("已添加楼栋", name))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

building_update <- function(id, name, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- sprintf("name='%s'", gsub("'","''",name))
    if (!is.null(description)) sets <- paste0(sets, sprintf(", description='%s'", gsub("'","''",description)))
    dbExecute(con, sprintf("UPDATE seat_buildings SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

building_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM seat_floors WHERE building_id = %d", as.integer(id)))$n[1]
    if (cnt > 0) return(list(success = FALSE, message = sprintf("楼栋下有 %d 个楼层，无法删除", cnt)))
    dbExecute(con, sprintf("DELETE FROM seat_buildings WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 楼层 CRUD
##################
floor_get_all <- function(building_id = NULL) {
  con <- db_connect()
  tryCatch({
    if (!is.null(building_id)) {
      dbGetQuery(con, sprintf("SELECT * FROM seat_floors WHERE building_id = %d ORDER BY floor_number", as.integer(building_id)))
    } else {
      dbGetQuery(con, "SELECT f.*, b.name as building_name FROM seat_floors f LEFT JOIN seat_buildings b ON f.building_id = b.id ORDER BY b.name, f.floor_number")
    }
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

floor_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT f.*, b.name as building_name FROM seat_floors f LEFT JOIN seat_buildings b ON f.building_id = b.id WHERE f.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

floor_add <- function(building_id, name, floor_number = NULL, description = "") {
  con <- db_connect()
  tryCatch({
    if (is.null(floor_number)) floor_number <- "NULL" else floor_number <- as.integer(floor_number)
    dbExecute(con, sprintf("INSERT INTO seat_floors (building_id, name, floor_number, description) VALUES (%d,'%s',%s,'%s')",
      as.integer(building_id), gsub("'","''",name), floor_number, gsub("'","''",description)))
    list(success = TRUE, message = paste("已添加楼层", name))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

floor_update <- function(id, name, floor_number = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- sprintf("name='%s'", gsub("'","''",name))
    if (!is.null(floor_number)) sets <- paste0(sets, sprintf(", floor_number=%d", as.integer(floor_number)))
    if (!is.null(description)) sets <- paste0(sets, sprintf(", description='%s'", gsub("'","''",description)))
    dbExecute(con, sprintf("UPDATE seat_floors SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

floor_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    zcnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM seat_zones WHERE floor_id = %d", as.integer(id)))$n[1]
    scnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM seats WHERE floor_id = %d", as.integer(id)))$n[1]
    if (zcnt + scnt > 0) return(list(success = FALSE, message = sprintf("楼层下有 %d 区域 + %d 工位，无法删除", zcnt, scnt)))
    dbExecute(con, sprintf("DELETE FROM seat_floors WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 区域 CRUD
##################
zone_get_all <- function(floor_id = NULL) {
  con <- db_connect()
  tryCatch({
    if (!is.null(floor_id)) {
      dbGetQuery(con, sprintf("SELECT * FROM seat_zones WHERE floor_id = %d ORDER BY name", as.integer(floor_id)))
    } else {
      dbGetQuery(con, "SELECT z.*, f.name as floor_name, b.name as building_name FROM seat_zones z LEFT JOIN seat_floors f ON z.floor_id = f.id LEFT JOIN seat_buildings b ON f.building_id = b.id ORDER BY b.name, f.floor_number, z.name")
    }
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

zone_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT z.*, f.name as floor_name, f.building_id FROM seat_zones z LEFT JOIN seat_floors f ON z.floor_id = f.id WHERE z.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

zone_add <- function(floor_id, name, zone_type, row_start = 1, col_start = 1, row_span = 1, col_span = 1, description = "") {
  con <- db_connect()
  tryCatch({
    if (is.null(row_start)) row_start <- 1
    if (is.null(col_start)) col_start <- 1
    if (is.null(row_span)) row_span <- 1
    if (is.null(col_span)) col_span <- 1
    dbExecute(con, sprintf(
      "INSERT INTO seat_zones (floor_id, name, zone_type, row_start, col_start, row_span, col_span, description) VALUES (%d,'%s','%s',%d,%d,%d,%d,'%s')",
      as.integer(floor_id), gsub("'","''",name), zone_type,
      as.integer(row_start), as.integer(col_start), as.integer(row_span), as.integer(col_span),
      gsub("'","''",description)))
    list(success = TRUE, message = paste("已添加区域", name))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

zone_update <- function(id, name = NULL, zone_type = NULL, row_start = NULL, col_start = NULL,
  row_span = NULL, col_span = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- character(0)
    if (!is.null(name))        sets <- c(sets, sprintf("name='%s'", gsub("'","''",name)))
    if (!is.null(zone_type))   sets <- c(sets, sprintf("zone_type='%s'", zone_type))
    if (!is.null(row_start))   sets <- c(sets, sprintf("row_start=%d", as.integer(row_start)))
    if (!is.null(col_start))   sets <- c(sets, sprintf("col_start=%d", as.integer(col_start)))
    if (!is.null(row_span))    sets <- c(sets, sprintf("row_span=%d", as.integer(row_span)))
    if (!is.null(col_span))    sets <- c(sets, sprintf("col_span=%d", as.integer(col_span)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无更新内容"))
    dbExecute(con, sprintf("UPDATE seat_zones SET %s WHERE id = %d", paste(sets, collapse=","), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

zone_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    scnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM seats WHERE zone_id = %d", as.integer(id)))$n[1]
    if (scnt > 0) return(list(success = FALSE, message = sprintf("区域下有 %d 个工位，无法删除", scnt)))
    dbExecute(con, sprintf("DELETE FROM seat_zones WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 工位 CRUD
##################
seat_get_all <- function(floor_id = NULL, zone_id = NULL) {
  con <- db_connect()
  tryCatch({
    base <- "SELECT s.*, COALESCE(NULLIF(u.display_name,''), u.username) as user_name, a.hostname as asset_hostname, a.ip_address as asset_ip, z.name as zone_name, z.zone_type, f.name as floor_name, f.building_id FROM seats s LEFT JOIN users u ON s.user_id = u.id LEFT JOIN assets a ON s.asset_id = a.id LEFT JOIN seat_zones z ON s.zone_id = z.id LEFT JOIN seat_floors f ON s.floor_id = f.id"
    if (!is.null(zone_id)) {
      dbGetQuery(con, sprintf("%s WHERE s.zone_id = %d ORDER BY s.row_num, s.col_num", base, as.integer(zone_id)))
    } else if (!is.null(floor_id)) {
      dbGetQuery(con, sprintf("%s WHERE s.floor_id = %d ORDER BY s.row_num, s.col_num", base, as.integer(floor_id)))
    } else {
      dbGetQuery(con, paste(base, "ORDER BY s.floor_id, s.row_num, s.col_num"))
    }
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

seat_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT s.*, COALESCE(NULLIF(u.display_name,''), u.username) as user_name, a.hostname as asset_hostname, a.ip_address as asset_ip FROM seats s LEFT JOIN users u ON s.user_id = u.id LEFT JOIN assets a ON s.asset_id = a.id WHERE s.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

seat_add <- function(floor_id, zone_id, seat_code, row_num, col_num, status = "vacant_no_pc", user_id = NULL, asset_id = NULL, description = "") {
  con <- db_connect()
  tryCatch({
    uid <- if (is.null(user_id) || user_id == "") "NULL" else as.character(as.integer(user_id))
    aid <- if (is.null(asset_id) || asset_id == "") "NULL" else as.character(as.integer(asset_id))
    dbExecute(con, sprintf(
      "INSERT INTO seats (floor_id, zone_id, seat_code, row_num, col_num, status, user_id, asset_id, description) VALUES (%d,%d,'%s',%d,%d,'%s',%s,%s,'%s')",
      as.integer(floor_id), as.integer(zone_id), gsub("'","''",seat_code),
      as.integer(row_num), as.integer(col_num), status, uid, aid,
      gsub("'","''",description)))
    list(success = TRUE, message = paste("已添加工位", seat_code))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

seat_update <- function(id, seat_code = NULL, row_num = NULL, col_num = NULL, status = NULL,
  zone_id = NULL, user_id = NULL, asset_id = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- character(0)
    if (!is.null(seat_code))  sets <- c(sets, sprintf("seat_code='%s'", gsub("'","''",seat_code)))
    if (!is.null(row_num))    sets <- c(sets, sprintf("row_num=%d", as.integer(row_num)))
    if (!is.null(col_num))    sets <- c(sets, sprintf("col_num=%d", as.integer(col_num)))
    if (!is.null(status))     sets <- c(sets, sprintf("status='%s'", status))
    if (!is.null(zone_id))    sets <- c(sets, sprintf("zone_id=%d", as.integer(zone_id)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    # user_id: 传 NA 则置 NULL，传数字则设置
    if (!is.null(user_id)) {
      if (is.na(user_id) || user_id == "" || user_id == "NA") {
        sets <- c(sets, "user_id=NULL")
      } else {
        sets <- c(sets, sprintf("user_id=%d", as.integer(user_id)))
      }
    }
    if (!is.null(asset_id)) {
      if (is.na(asset_id) || asset_id == "" || asset_id == "NA") {
        sets <- c(sets, "asset_id=NULL")
      } else {
        sets <- c(sets, sprintf("asset_id=%d", as.integer(asset_id)))
      }
    }
    if (length(sets) == 0) return(list(success = FALSE, message = "无更新内容"))
    dbExecute(con, sprintf("UPDATE seats SET %s WHERE id = %d", paste(sets, collapse=","), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

seat_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM seats WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 批量生成工位
##################
seat_batch_generate <- function(floor_id, zone_id, prefix, start_row, start_col, rows, cols,
  start_number = 1, orientation = "horizontal") {
  con <- db_connect()
  tryCatch({
    cnt <- 0
    n <- start_number - 1
    for (r in seq_len(rows)) {
      for (c in seq_len(cols)) {
        n <- n + 1
        code <- sprintf("%s-%02d", prefix, n)
        row_pos <- start_row + r - 1
        col_pos <- start_col + c - 1
        dbExecute(con, sprintf(
          "INSERT INTO seats (floor_id, zone_id, seat_code, row_num, col_num, status) VALUES (%d,%d,'%s',%d,%d,'vacant_no_pc')",
          as.integer(floor_id), as.integer(zone_id), code, row_pos, col_pos))
        cnt <- cnt + 1
      }
    }
    list(success = TRUE, message = sprintf("已生成 %d 个工位", cnt))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 楼层数据汇总（工位图渲染用）
##################
seat_floor_snapshot <- function(floor_id) {
  floor <- floor_get_by_id(floor_id)
  if (is.null(floor)) return(NULL)
  zones <- zone_get_all(floor_id)
  seats <- seat_get_all(floor_id = floor_id)
  list(floor = floor, zones = zones, seats = seats,
    max_row = if (nrow(seats) > 0) max(seats$row_num, na.rm = TRUE) else 1,
    max_col = if (nrow(seats) > 0) max(seats$col_num, na.rm = TRUE) else 1)
}

# 区域类型中文标签
zone_type_label <- function(zt) {
  switch(zt,
    "reception"      = "前台",
    "open_desk"      = "大厅卡座",
    "meeting_room"   = "会议室",
    "lab"            = "实验室",
    "warehouse"      = "仓库",
    "small_office"   = "小办公室",
    "tea_room"       = "茶室",
    "smoking_room"   = "吸烟室",
    zt)
}

# 区域类型颜色
zone_type_color <- function(zt) {
  switch(zt,
    "reception"    = "#e3f2fd",
    "open_desk"    = "#f5f5f5",
    "meeting_room" = "#e8f5e9",
    "lab"          = "#fff3e0",
    "warehouse"    = "#fce4ec",
    "small_office" = "#f3e5f5",
    "tea_room"     = "#e0f2f1",
    "smoking_room" = "#eceff1",
    "#f5f5f5")
}

# 工位状态
seat_status_label <- function(st) {
  switch(st,
    "occupied"       = "有员工",
    "vacant_no_pc"   = "无员工无电脑",
    "vacant_with_pc" = "无员工有电脑",
    st)
}

# 用户列表（供下拉框）
seat_user_choices <- function() {
  con <- db_connect()
  tryCatch({
    u <- dbGetQuery(con, "SELECT id, COALESCE(NULLIF(display_name,''), username) as label FROM users WHERE active = 1 ORDER BY username")
    setNames(as.character(u$id), u$label)
  }, error = function(e) c(), finally = { db_disconnect(con) })
}
