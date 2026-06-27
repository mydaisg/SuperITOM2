# RBAC 授权管理 - 数据层

##################
# 权限检查核心函数
##################
rbac_check <- function(current_user, perm_code) {
  if (is.null(current_user) || nrow(current_user) == 0) return(FALSE)
  uid <- current_user$id[1]
  con <- db_connect()
  tryCatch({
    # admin角色始终通过
    is_admin <- current_user$role[1] == "admin"
    if (is_admin) return(TRUE)
    # 检查用户是否拥有此权限（多角色，允许覆盖禁止）
    r <- dbGetQuery(con, sprintf(
      "SELECT COUNT(*) as cnt FROM rbac_user_roles ur
       JOIN rbac_role_permissions rp ON ur.role_id = rp.role_id
       JOIN rbac_permissions p ON rp.permission_id = p.id
       WHERE ur.user_id = %d AND p.code = '%s'", uid, perm_code))
    r$cnt[1] > 0
  }, error = function(e) { warning("RBAC check error: ", e$message); TRUE },
  finally = { db_disconnect(con) })
}

rbac_get_user_perms <- function(uid) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT DISTINCT p.code FROM rbac_user_roles ur
       JOIN rbac_role_permissions rp ON ur.role_id = rp.role_id
       JOIN rbac_permissions p ON rp.permission_id = p.id
       WHERE ur.user_id = %d", as.integer(uid)))$code
  }, error = function(e) character(0), finally = { db_disconnect(con) })
}

##################
# 权限 CRUD
##################
rbac_permission_get_all <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT id, module, component, code, name, description FROM rbac_permissions ORDER BY module, component, id") },
    finally = { db_disconnect(con) })
}

##################
# 角色 CRUD
##################
rbac_role_get_all <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT * FROM rbac_roles ORDER BY id") },
    finally = { db_disconnect(con) })
}

rbac_role_add <- function(name, description = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO rbac_roles (name, description) VALUES ('%s','%s')",
      gsub("'","''",name), gsub("'","''",description)))
    list(success = TRUE, message = "角色已添加")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

rbac_role_update <- function(id, name = NULL, description = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- c()
    if (!is.null(name)) sets <- c(sets, sprintf("name='%s'", gsub("'","''",name)))
    if (!is.null(description)) sets <- c(sets, sprintf("description='%s'", gsub("'","''",description)))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE rbac_roles SET %s WHERE id = %d", paste(sets, collapse=", "), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

rbac_role_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM rbac_role_permissions WHERE role_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM rbac_user_roles WHERE role_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM rbac_roles WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

# 获取角色下的用户列表
rbac_role_get_users <- function(role_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT u.id, u.username, COALESCE(NULLIF(u.display_name,''), u.username) as display_name, u.role, u.active
       FROM rbac_user_roles ur JOIN users u ON ur.user_id = u.id
       WHERE ur.role_id = %d ORDER BY u.username", as.integer(role_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取角色详情（角色信息+权限数+用户列表，用于删除确认）
rbac_role_get_detail <- function(role_id) {
  con <- db_connect()
  tryCatch({
    role <- dbGetQuery(con, sprintf("SELECT * FROM rbac_roles WHERE id = %d", as.integer(role_id)))
    if (nrow(role) == 0) return(NULL)
    user_cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM rbac_user_roles WHERE role_id = %d", as.integer(role_id)))$cnt[1]
    perm_cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) as cnt FROM rbac_role_permissions WHERE role_id = %d", as.integer(role_id)))$cnt[1]
    list(role = role, user_count = user_cnt, perm_count = perm_cnt)
  }, finally = { db_disconnect(con) })
}

# 批量删除角色（带用户检查）
rbac_role_batch_delete <- function(ids) {
  ids <- as.integer(ids)
  if (length(ids) == 0) return(list(success = FALSE, message = "请选择要删除的角色"))
  con <- db_connect()
  tryCatch({
    # 检查每个角色下是否有用户
    blocked <- character(0)
    for (id in ids) {
      user_cnt <- dbGetQuery(con, sprintf(
        "SELECT COUNT(*) as cnt FROM rbac_user_roles WHERE role_id = %d", id))$cnt[1]
      if (user_cnt > 0) {
        role_name <- dbGetQuery(con, sprintf("SELECT name FROM rbac_roles WHERE id = %d", id))$name[1]
        blocked <- c(blocked, sprintf("[%s] %s (ID=%d, %d个用户)",
          role_name, role_name, id, user_cnt))
      }
    }
    if (length(blocked) > 0) {
      return(list(success = FALSE, message = paste(c("以下角色仍有用户关联，无法删除：", blocked), collapse = "\n  ")))
    }
    # 执行删除
    for (id in ids) {
      dbExecute(con, sprintf("DELETE FROM rbac_role_permissions WHERE role_id = %d", id))
      dbExecute(con, sprintf("DELETE FROM rbac_roles WHERE id = %d", id))
    }
    list(success = TRUE, message = sprintf("已删除 %d 个角色", length(ids)))
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 角色权限 CRUD
##################
rbac_role_perms_get <- function(role_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT p.code FROM rbac_role_permissions rp JOIN rbac_permissions p ON rp.permission_id = p.id WHERE rp.role_id = %d", as.integer(role_id)))$code
  }, error = function(e) character(0), finally = { db_disconnect(con) })
}

rbac_role_perms_set <- function(role_id, perm_codes) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM rbac_role_permissions WHERE role_id = %d", as.integer(role_id)))
    for (code in perm_codes) {
      pid <- dbGetQuery(con, sprintf("SELECT id FROM rbac_permissions WHERE code='%s'", code))
      if (nrow(pid) > 0) dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_role_permissions (role_id, permission_id) VALUES (%d, %d)", as.integer(role_id), pid$id[1]))
    }
    list(success = TRUE, message = "权限已保存")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 用户角色 CRUD
##################
rbac_user_roles_get <- function(user_id) {
  con <- db_connect()
  tryCatch({
    rs <- dbGetQuery(con, sprintf(
      "SELECT r.id, r.name FROM rbac_user_roles ur JOIN rbac_roles r ON ur.role_id = r.id WHERE ur.user_id = %d", as.integer(user_id)))
    if (nrow(rs) > 0) rs$id else integer(0)
  }, error = function(e) integer(0), finally = { db_disconnect(con) })
}

rbac_user_roles_set <- function(user_id, role_ids) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM rbac_user_roles WHERE user_id = %d", as.integer(user_id)))
    for (rid in role_ids) {
      if (is.null(rid) || rid == "") next
      dbExecute(con, sprintf("INSERT OR IGNORE INTO rbac_user_roles (user_id, role_id) VALUES (%d, %d)", as.integer(user_id), as.integer(rid)))
    }
    list(success = TRUE, message = "角色已分配")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

rbac_user_get_all <- function() {
  con <- db_connect()
  tryCatch({
    users <- dbGetQuery(con, "SELECT id, username, COALESCE(NULLIF(display_name,''), username) as display_name, role, active FROM users ORDER BY username")
    for (i in seq_len(nrow(users))) {
      rs <- rbac_user_roles_get(users$id[i])
      users$role_ids[i] <- if (length(rs) > 0) paste(rs, collapse=",") else ""
    }
    users
  }, finally = { db_disconnect(con) })
}

##################
# 模块级别访问控制
##################
# 返回用户可访问的模块名向量；admin 返回 NULL 表示全部可访问
rbac_get_user_modules <- function(current_user) {
  if (is.null(current_user) || nrow(current_user) == 0) return(character(0))
  if (current_user$role[1] == "admin") return(NULL)  # NULL = 全部可见
  uid <- current_user$id[1]
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT DISTINCT p.module FROM rbac_user_roles ur
       JOIN rbac_role_permissions rp ON ur.role_id = rp.role_id
       JOIN rbac_permissions p ON rp.permission_id = p.id
       WHERE ur.user_id = %d", uid))$module
  }, error = function(e) character(0),
  finally = { db_disconnect(con) })
}

# 获取所有在 RBAC 中注册的模块名（用于判断哪些模块受 RBAC 管控）
rbac_all_modules <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT DISTINCT module FROM rbac_permissions ORDER BY module")$module
  }, error = function(e) character(0),
  finally = { db_disconnect(con) })
}
