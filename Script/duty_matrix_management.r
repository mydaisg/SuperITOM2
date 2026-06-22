# 岗职模块 - 数据层
# 岗位职责矩阵：岗位 + 人员 + 职责项 + RBAC级别

##################
# 岗位 CRUD
##################
duty_position_get_all <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT * FROM duty_positions ORDER BY sort_order, id") },
    finally = { db_disconnect(con) })
}
duty_position_add <- function(name, description = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO duty_positions (name, description) VALUES ('%s','%s')",
      gsub("'","''",name), gsub("'","''",description)))
    list(success = TRUE, message = "岗位已添加")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
duty_position_update <- function(id, name = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(name)) sets <- c(sets, sprintf("name='%s'", gsub("'","''",name)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE duty_positions SET %s WHERE id = %d", paste(sets, collapse=", "), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
duty_position_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM duty_positions WHERE id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM duty_matrix WHERE position_id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 人员 CRUD
##################
duty_staff_get_all <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT s.*, u.username, p.name as position_name FROM duty_staff s LEFT JOIN users u ON s.user_id = u.id LEFT JOIN duty_positions p ON s.position_id = p.id ORDER BY p.name, s.name") },
    finally = { db_disconnect(con) })
}
duty_staff_add <- function(name, department = "", email = "", user_id = NULL, position_id = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO duty_staff (name, department, email, user_id, position_id) VALUES ('%s','%s','%s',%s,%s)",
      gsub("'","''",name), gsub("'","''",department), gsub("'","''",email),
      ifelse(is.null(user_id),"NULL",as.character(user_id)),
      ifelse(is.null(position_id),"NULL",as.character(position_id))))
    list(success = TRUE, message = "人员已添加")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
duty_staff_update <- function(id, name = NULL, department = NULL, email = NULL, position_id = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(name)) sets <- c(sets, sprintf("name='%s'", gsub("'","''",name)))
    if (!is.null(department)) sets <- c(sets, sprintf("department='%s'", gsub("'","''",department)))
    if (!is.null(email)) sets <- c(sets, sprintf("email='%s'", gsub("'","''",email)))
    if (!is.null(position_id)) sets <- c(sets, sprintf("position_id=%d", as.integer(position_id)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE duty_staff SET %s WHERE id = %d", paste(sets, collapse=", "), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
duty_staff_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM duty_staff WHERE id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM duty_matrix WHERE staff_id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 职责项 CRUD
##################
duty_item_get_all <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT * FROM duty_items ORDER BY sort_order, id") },
    finally = { db_disconnect(con) })
}
duty_item_add <- function(name, description = "", category = "", sort_order = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO duty_items (name, description, category, sort_order) VALUES ('%s','%s','%s',%d)",
      gsub("'","''",name), gsub("'","''",description), gsub("'","''",category), as.integer(sort_order)))
    list(success = TRUE, message = "职责项已添加")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
duty_item_update <- function(id, name = NULL, description = NULL, category = NULL, sort_order = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(name)) sets <- c(sets, sprintf("name='%s'", gsub("'","''",name)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    if (!is.null(category)) sets <- c(sets, sprintf("category='%s'", gsub("'","''",category)))
    if (!is.null(sort_order)) sets <- c(sets, sprintf("sort_order=%d", as.integer(sort_order)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE duty_items SET %s WHERE id = %d", paste(sets, collapse=", "), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
duty_item_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM duty_items WHERE id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM duty_matrix WHERE duty_item_id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 矩阵 CRUD
##################
duty_matrix_get <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT dm.*, s.name as staff_name, s.department, p.name as position_name, di.name as duty_name, di.category as duty_category
      FROM duty_matrix dm
      JOIN duty_staff s ON dm.staff_id = s.id
      JOIN duty_positions p ON dm.position_id = p.id
      JOIN duty_items di ON dm.duty_item_id = di.id
      ORDER BY s.department, s.name, p.name, di.sort_order")
  }, finally = { db_disconnect(con) })
}

duty_matrix_set <- function(staff_id, position_id, duty_item_id, responsibility_level, comment = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT OR REPLACE INTO duty_matrix (staff_id, position_id, duty_item_id, responsibility_level, comment, updated_at)
       VALUES (%d, %d, %d, '%s', '%s', datetime('now','localtime'))",
      as.integer(staff_id), as.integer(position_id), as.integer(duty_item_id),
      responsibility_level, gsub("'","''",comment)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

duty_matrix_delete <- function(staff_id, position_id, duty_item_id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM duty_matrix WHERE staff_id=%d AND position_id=%d AND duty_item_id=%d",
      as.integer(staff_id), as.integer(position_id), as.integer(duty_item_id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}
