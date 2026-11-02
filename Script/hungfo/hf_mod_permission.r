##################
# HungFo 环节 12 — 权限体系 (HF-PERM)
# 职责：子项目内的用户 / 安全组 / 角色 / 权限，作为独立子项目自带的安全边界。
# 模型（参照父项目 RBAC 三层授权）：用户 → 安全组 → 角色 → 权限。
# 数据表由 hf_core.r 的 hf_db_init() 建表并播种（hf_perm_seed），本文件只做数据层与鉴权。
##################

HF_MOD_PERMISSION <- list(
  id = "hf12", key = "permission", name = "权限体系", icon = "user-shield",
  desc = "子项目内的用户 / 安全组 / 角色 / 权限校验，作为独立子项目自带的安全边界。",
  api_desc = "login(user, pass) 登录 · check(user, action) 校验 · users/groups/roles/permissions 维护",
  status = "active", version = "v1.0"
)

# ══════════════════════════════════════════════════════════════════
# 鉴权
# ══════════════════════════════════════════════════════════════════

# 登录：校验用户名 + 密码（密码存储与校验一致，见 hf_users.hashing_algo=plain）
hf_auth_login <- function(username, password) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(username) || !nzchar(trimws(username))) return(list(success = FALSE, message = "请输入用户名"))
  if (is.null(password)) password <- ""
  tryCatch({
    u <- dbGetQuery(con, sprintf(
      "SELECT id, username, password, display_name, active FROM hf_users WHERE username='%s'",
      hf_esc(username)))
    if (nrow(u) == 0) return(list(success = FALSE, message = "用户名或密码错误"))
    if (!identical(as.character(u$password[1]), as.character(password))) {
      return(list(success = FALSE, message = "用户名或密码错误"))
    }
    if (!is.na(u$active[1]) && u$active[1] == 0) {
      return(list(success = FALSE, message = "该账号已被禁用，请联系管理员"))
    }
    # 组装 user 记录（含组/角色信息，供界面与鉴权使用）
    user <- hf_perm_user_join(con, u$id[1])
    list(success = TRUE, user = user, message = "登录成功")
  }, error = function(e) list(success = FALSE, message = paste0("登录失败：", e$message)))
}

# 当前用户是否为管理员（拥有 admin 角色）
hf_perm_is_admin <- function(user = NULL) {
  if (is.null(user)) return(FALSE)
  if (!is.data.frame(user) || nrow(user) == 0) return(FALSE)
  if (!is.null(user$is_admin) && length(user$is_admin) > 0) return(isTRUE(user$is_admin[1] == 1))
  if (!is.null(user$role_names) && length(user$role_names) > 0 && !is.na(user$role_names[1]) &&
      nzchar(user$role_names[1]) && grepl("(^|,)admin(,|$)", user$role_names[1])) return(TRUE)
  FALSE
}

# 取用户全部权限码（经 组→角色→权限 链路聚合去重）
hf_perm_get_user_perms <- function(user = NULL, uid = NULL) {
  if (is.null(uid) && !is.null(user) && nrow(user) > 0 && !is.null(user$id)) uid <- user$id[1]
  if (is.null(uid)) return(character(0))
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT DISTINCT p.code FROM hf_user_groups ug
       JOIN hf_role_groups rg ON ug.group_id = rg.group_id
       JOIN hf_role_permissions rp ON rg.role_id = rp.role_id
       JOIN hf_permissions p ON rp.permission_id = p.id
       WHERE ug.user_id = %d", as.integer(uid)))$code
  }, error = function(e) character(0))
}

# 鉴权：校验 user 是否拥有指定 action 权限（admin 角色始终通过）
hf_perm_check <- function(user = NULL, action = NULL) {
  if (is.null(user) || nrow(user) == 0) {
    return(list(success = FALSE, user = NULL, action = action, granted = FALSE, message = "未登录"))
  }
  if (hf_perm_is_admin(user)) {
    return(list(success = TRUE, user = user, action = action, granted = TRUE, message = "管理员权限通过"))
  }
  if (is.null(action) || !nzchar(trimws(action))) {
    return(list(success = TRUE, user = user, action = action, granted = TRUE, message = "未指定权限，放行"))
  }
  perms <- hf_perm_get_user_perms(user)
  granted <- action %in% perms
  list(success = granted, user = user, action = action, granted = granted,
       message = if (granted) "权限校验通过" else sprintf("缺少权限：%s", action))
}

# ══════════════════════════════════════════════════════════════════
# 查询（供 UI 与接口使用）
# ══════════════════════════════════════════════════════════════════

# 用户列表（含所属组、角色、权限数）
hf_perm_users_list <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    u <- dbGetQuery(con, "SELECT id, username, password, display_name, active, hashing_algo, created_at, updated_at FROM hf_users ORDER BY id ASC")
    if (nrow(u) == 0) return(u)
    for (i in seq_len(nrow(u))) {
      grp <- dbGetQuery(con, sprintf(
        "SELECT g.id, g.name FROM hf_user_groups ug JOIN hf_groups g ON ug.group_id = g.id WHERE ug.user_id = %d ORDER BY g.name", u$id[i]))
      u$group_ids[i]   <- if (nrow(grp) > 0) paste(grp$id, collapse = ",") else ""
      u$group_names[i] <- if (nrow(grp) > 0) paste(grp$name, collapse = ",") else ""
    }
    u
  }, error = function(e) data.frame())
}

# 组装单个用户（含组名、角色名、是否管理员）
hf_perm_user_join <- function(con, uid) {
  u <- dbGetQuery(con, sprintf(
    "SELECT id, username, password, display_name, active, hashing_algo FROM hf_users WHERE id=%d", as.integer(uid)))
  if (nrow(u) == 0) return(data.frame())
  grp <- dbGetQuery(con, sprintf(
    "SELECT g.id, g.name FROM hf_user_groups ug JOIN hf_groups g ON ug.group_id = g.id WHERE ug.user_id = %d", as.integer(uid)))
  rol <- dbGetQuery(con, sprintf(
    "SELECT DISTINCT r.id, r.name FROM hf_user_groups ug
     JOIN hf_role_groups rg ON ug.group_id = rg.group_id
     JOIN hf_roles r ON rg.role_id = r.id WHERE ug.user_id = %d", as.integer(uid)))
  u$group_ids   <- if (nrow(grp) > 0) paste(grp$id, collapse = ",") else ""
  u$group_names <- if (nrow(grp) > 0) paste(grp$name, collapse = ",") else ""
  u$role_names  <- if (nrow(rol) > 0) paste(rol$name, collapse = ",") else ""
  u$is_admin    <- if (nrow(rol) > 0 && any(rol$name == "admin")) 1 else 0
  u
}

# 安全组列表（含角色名、成员数）
hf_perm_groups_list <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    g <- dbGetQuery(con, "SELECT id, name, description, active, created_at, updated_at FROM hf_groups ORDER BY id ASC")
    if (nrow(g) == 0) return(g)
    for (i in seq_len(nrow(g))) {
      rol <- dbGetQuery(con, sprintf(
        "SELECT r.id, r.name FROM hf_role_groups rg JOIN hf_roles r ON rg.role_id = r.id WHERE rg.group_id = %d ORDER BY r.name", g$id[i]))
      g$role_ids[i]   <- if (nrow(rol) > 0) paste(rol$id, collapse = ",") else ""
      g$role_names[i] <- if (nrow(rol) > 0) paste(rol$name, collapse = ",") else ""
      g$member_count[i] <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_user_groups WHERE group_id = %d", g$id[i]))$n[1]
    }
    g
  }, error = function(e) data.frame())
}

# 角色列表（含权限码）
hf_perm_roles_list <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    r <- dbGetQuery(con, "SELECT id, name, description, created_at, updated_at FROM hf_roles ORDER BY id ASC")
    if (nrow(r) == 0) return(r)
    for (i in seq_len(nrow(r))) {
      p <- dbGetQuery(con, sprintf(
        "SELECT p.id, p.code FROM hf_role_permissions rp JOIN hf_permissions p ON rp.permission_id = p.id WHERE rp.role_id = %d ORDER BY p.code", r$id[i]))
      r$perm_ids[i]  <- if (nrow(p) > 0) paste(p$id, collapse = ",") else ""
      r$perm_codes[i] <- if (nrow(p) > 0) paste(p$code, collapse = ",") else ""
      r$perm_count[i] <- nrow(p)
    }
    r
  }, error = function(e) data.frame())
}

# 权限清单
hf_perm_permissions_list <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    dbGetQuery(con, "SELECT id, module_key, code, name, description FROM hf_permissions ORDER BY module_key, code"),
    error = function(e) data.frame())
}

# ══════════════════════════════════════════════════════════════════
# 用户 CRUD
# ══════════════════════════════════════════════════════════════════

hf_perm_user_add <- function(username, password, display_name = "", group_ids = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(username) || !nzchar(trimws(username))) return(list(success = FALSE, message = "用户名不能为空"))
  if (is.null(password) || !nzchar(password)) return(list(success = FALSE, message = "密码不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_users (username, password, display_name) VALUES ('%s','%s','%s')",
      hf_esc(username), hf_esc(password), hf_esc(hf_or(display_name, ""))))
    uid <- dbGetQuery(con, sprintf("SELECT id FROM hf_users WHERE username='%s'", hf_esc(username)))
    hf_perm_user_set_groups(con, uid$id[1], group_ids)
    list(success = TRUE, message = paste0("已新增用户：", username))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_user_update <- function(id, username, display_name = "", password = NULL, active = 1, group_ids = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "用户 id 无效"))
  if (is.null(username) || !nzchar(trimws(username))) return(list(success = FALSE, message = "用户名不能为空"))
  tryCatch({
    sets <- sprintf("username='%s', display_name='%s', active=%d, updated_at=datetime('now','localtime')",
      hf_esc(username), hf_esc(hf_or(display_name, "")), as.integer(active))
    if (!is.null(password) && nzchar(password)) sets <- paste0(sets, sprintf(", password='%s'", hf_esc(password)))
    dbExecute(con, sprintf("UPDATE hf_users SET %s WHERE id=%d", sets, as.integer(id)))
    hf_perm_user_set_groups(con, as.integer(id), group_ids)
    list(success = TRUE, message = paste0("已更新用户：", username))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_user_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM hf_user_groups WHERE user_id=%d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM hf_users WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除用户")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_user_toggle <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("UPDATE hf_users SET active = 1 - active, updated_at=datetime('now','localtime') WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已切换账号状态")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 重设用户所属组（组内赋值，先清后插）
hf_perm_user_set_groups <- function(con, uid, group_ids = NULL) {
  dbExecute(con, sprintf("DELETE FROM hf_user_groups WHERE user_id=%d", as.integer(uid)))
  for (gid in group_ids) {
    if (is.null(gid) || is.na(gid) || gid == "") next
    dbExecute(con, sprintf("INSERT OR IGNORE INTO hf_user_groups (user_id, group_id) VALUES (%d, %d)", as.integer(uid), as.integer(gid)))
  }
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════
# 安全组 CRUD
# ══════════════════════════════════════════════════════════════════

hf_perm_group_add <- function(name, description = "", role_ids = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "组名不能为空"))
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO hf_groups (name, description) VALUES ('%s','%s')",
      hf_esc(name), hf_esc(hf_or(description, ""))))
    gid <- dbGetQuery(con, sprintf("SELECT id FROM hf_groups WHERE name='%s'", hf_esc(name)))
    hf_perm_group_set_roles(con, gid$id[1], role_ids)
    list(success = TRUE, message = paste0("已新增安全组：", name))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_group_update <- function(id, name, description = "", active = 1, role_ids = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "组 id 无效"))
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "组名不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_groups SET name='%s', description='%s', active=%d, updated_at=datetime('now','localtime') WHERE id=%d",
      hf_esc(name), hf_esc(hf_or(description, "")), as.integer(active), as.integer(id)))
    hf_perm_group_set_roles(con, as.integer(id), role_ids)
    list(success = TRUE, message = paste0("已更新安全组：", name))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_group_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    n <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_user_groups WHERE group_id=%d", as.integer(id)))$n[1]
    if (n > 0) return(list(success = FALSE, message = sprintf("该组下仍有 %d 名用户，请先移出", n)))
    dbExecute(con, sprintf("DELETE FROM hf_role_groups WHERE group_id=%d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM hf_groups WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除安全组")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_group_set_roles <- function(con, gid, role_ids = NULL) {
  dbExecute(con, sprintf("DELETE FROM hf_role_groups WHERE group_id=%d", as.integer(gid)))
  for (rid in role_ids) {
    if (is.null(rid) || is.na(rid) || rid == "") next
    dbExecute(con, sprintf("INSERT OR IGNORE INTO hf_role_groups (role_id, group_id) VALUES (%d, %d)", as.integer(rid), as.integer(gid)))
  }
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════
# 角色 CRUD
# ══════════════════════════════════════════════════════════════════

hf_perm_role_add <- function(name, description = "", perm_ids = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "角色名不能为空"))
  tryCatch({
    dbExecute(con, sprintf("INSERT INTO hf_roles (name, description) VALUES ('%s','%s')",
      hf_esc(name), hf_esc(hf_or(description, ""))))
    rid <- dbGetQuery(con, sprintf("SELECT id FROM hf_roles WHERE name='%s'", hf_esc(name)))
    hf_perm_role_set_perms(con, rid$id[1], perm_ids)
    list(success = TRUE, message = paste0("已新增角色：", name))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_role_update <- function(id, name, description = "", perm_ids = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "角色 id 无效"))
  if (is.null(name) || !nzchar(trimws(name))) return(list(success = FALSE, message = "角色名不能为空"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_roles SET name='%s', description='%s', updated_at=datetime('now','localtime') WHERE id=%d",
      hf_esc(name), hf_esc(hf_or(description, "")), as.integer(id)))
    hf_perm_role_set_perms(con, as.integer(id), perm_ids)
    list(success = TRUE, message = paste0("已更新角色：", name))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_role_delete <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    n <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_role_groups WHERE role_id=%d", as.integer(id)))$n[1]
    if (n > 0) return(list(success = FALSE, message = sprintf("该角色仍被 %d 个安全组引用，请先解除", n)))
    dbExecute(con, sprintf("DELETE FROM hf_role_permissions WHERE role_id=%d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM hf_roles WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除角色")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_perm_role_set_perms <- function(con, rid, perm_ids = NULL) {
  dbExecute(con, sprintf("DELETE FROM hf_role_permissions WHERE role_id=%d", as.integer(rid)))
  for (pid in perm_ids) {
    if (is.null(pid) || is.na(pid) || pid == "") next
    dbExecute(con, sprintf("INSERT OR IGNORE INTO hf_role_permissions (role_id, permission_id) VALUES (%d, %d)", as.integer(rid), as.integer(pid)))
  }
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════
# 接口透传 + 注册
# ══════════════════════════════════════════════════════════════════

hf_perm_api_users_list <- function() hf_perm_users_list()
hf_perm_api_groups_list <- function() hf_perm_groups_list()
hf_perm_api_roles_list  <- function() hf_perm_roles_list()
hf_perm_api_perms_list  <- function() hf_perm_permissions_list()

hf_register(HF_MOD_PERMISSION, hf_make_api(
  "permission",
  login = hf_auth_login,
  check = hf_perm_check,
  is_admin = hf_perm_is_admin,
  user_perms = hf_perm_get_user_perms,
  users = hf_perm_api_users_list,
  groups = hf_perm_api_groups_list,
  roles = hf_perm_api_roles_list,
  permissions = hf_perm_api_perms_list,
  user_add = hf_perm_user_add,
  user_update = hf_perm_user_update,
  user_delete = hf_perm_user_delete,
  user_toggle = hf_perm_user_toggle,
  group_add = hf_perm_group_add,
  group_update = hf_perm_group_update,
  group_delete = hf_perm_group_delete,
  role_add = hf_perm_role_add,
  role_update = hf_perm_role_update,
  role_delete = hf_perm_role_delete
))
