# 小程序用户 + VIP 会员管理 - 数据层
# 面向小程序端 C 端用户（微信 openid 体系）与 VIP 会员权益管理

##################
# 自包含建表（幂等）
##################
miniprogram_db_init <- function() {
  con <- db_connect()
  tryCatch({
    dbExecute(con, "CREATE TABLE IF NOT EXISTS mini_program_users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      openid TEXT UNIQUE NOT NULL,
      unionid TEXT,
      nickname TEXT,
      avatar_url TEXT,
      phone TEXT,
      gender TEXT DEFAULT '',
      city TEXT DEFAULT '',
      province TEXT DEFAULT '',
      country TEXT DEFAULT '',
      status INTEGER DEFAULT 1,
      vip_level INTEGER DEFAULT 0,
      vip_expire_at TEXT,
      created_at TEXT DEFAULT (datetime('now','localtime')),
      updated_at TEXT DEFAULT (datetime('now','localtime'))
    )")
    dbExecute(con, "CREATE TABLE IF NOT EXISTS vip_records (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      level INTEGER DEFAULT 1,
      start_at TEXT,
      expire_at TEXT,
      duration_days INTEGER DEFAULT 0,
      amount REAL DEFAULT 0,
      source TEXT DEFAULT 'admin',
      status INTEGER DEFAULT 1,
      remark TEXT DEFAULT '',
      created_at TEXT DEFAULT (datetime('now','localtime')),
      FOREIGN KEY (user_id) REFERENCES mini_program_users(id)
    )")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_mp_users_openid ON mini_program_users(openid)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_vip_records_user ON vip_records(user_id)")
    list(success = TRUE)
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 小程序用户 CRUD
##################
mp_user_get_all <- function(status = "all", keyword = NULL, vip = "all") {
  con <- db_connect()
  tryCatch({
    q <- "SELECT * FROM mini_program_users WHERE 1=1"
    if (!is.null(status) && status != "all") q <- paste0(q, sprintf(" AND status=%d", as.integer(status)))
    if (!is.null(vip) && vip == "vip") q <- paste0(q, " AND vip_level > 0")
    if (!is.null(vip) && vip == "novip") q <- paste0(q, " AND (vip_level IS NULL OR vip_level = 0)")
    if (!is.null(keyword) && nzchar(trimws(keyword))) {
      kw <- gsub("'", "''", trimws(keyword))
      q <- paste0(q, sprintf(" AND (nickname LIKE '%%%s%%' OR phone LIKE '%%%s%%' OR openid LIKE '%%%s%%' OR unionid LIKE '%%%s%%')", kw, kw, kw, kw))
    }
    q <- paste0(q, " ORDER BY created_at DESC")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

mp_user_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM mini_program_users WHERE id=%d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

mp_user_get_by_openid <- function(openid) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM mini_program_users WHERE openid='%s'", gsub("'","''",openid)))
    if (nrow(r) == 0) NULL else r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

mp_user_add <- function(openid, nickname = NULL, phone = NULL, unionid = NULL, avatar_url = NULL,
                        gender = "", city = "", province = "", country = "",
                        status = 1, vip_level = 0, vip_expire_at = NULL) {
  con <- db_connect()
  tryCatch({
    if (is.null(openid) || !nzchar(trimws(openid))) return(list(success = FALSE, message = "openid 不能为空"))
    dup <- dbGetQuery(con, sprintf("SELECT id FROM mini_program_users WHERE openid='%s'", gsub("'","''",trimws(openid))))
    if (nrow(dup) > 0) return(list(success = FALSE, message = "该 openid 已存在"))
    dbExecute(con, sprintf(
      "INSERT INTO mini_program_users (openid, unionid, nickname, avatar_url, phone, gender, city, province, country, status, vip_level, vip_expire_at)
       VALUES ('%s', %s, %s, %s, %s, '%s', '%s', '%s', '%s', %d, %d, %s)",
      gsub("'","''",trimws(openid)),
      if (is.null(unionid) || unionid == "") "NULL" else sprintf("'%s'", gsub("'","''",unionid)),
      if (is.null(nickname) || nickname == "") "NULL" else sprintf("'%s'", gsub("'","''",nickname)),
      if (is.null(avatar_url) || avatar_url == "") "NULL" else sprintf("'%s'", gsub("'","''",avatar_url)),
      if (is.null(phone) || phone == "") "NULL" else sprintf("'%s'", gsub("'","''",phone)),
      gsub("'","''", gender %||% ""), gsub("'","''", city %||% ""),
      gsub("'","''", province %||% ""), gsub("'","''", country %||% ""),
      as.integer(status), as.integer(vip_level),
      if (is.null(vip_expire_at) || vip_expire_at == "") "NULL" else sprintf("'%s'", gsub("'","''",vip_expire_at))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, message = "用户已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

mp_user_update <- function(id, openid = NULL, nickname = NULL, phone = NULL, unionid = NULL, avatar_url = NULL,
                           gender = NULL, city = NULL, province = NULL, country = NULL,
                           status = NULL, vip_level = NULL, vip_expire_at = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- character(0)
    if (!is.null(openid) && nzchar(trimws(openid))) sets <- c(sets, sprintf("openid='%s'", gsub("'","''",trimws(openid))))
    if (!is.null(nickname)) sets <- c(sets, sprintf("nickname=%s", if(nickname=="") "NULL" else sprintf("'%s'", gsub("'","''",nickname))))
    if (!is.null(phone))    sets <- c(sets, sprintf("phone=%s", if(phone=="") "NULL" else sprintf("'%s'", gsub("'","''",phone))))
    if (!is.null(unionid))  sets <- c(sets, sprintf("unionid=%s", if(unionid=="") "NULL" else sprintf("'%s'", gsub("'","''",unionid))))
    if (!is.null(avatar_url)) sets <- c(sets, sprintf("avatar_url=%s", if(avatar_url=="") "NULL" else sprintf("'%s'", gsub("'","''",avatar_url))))
    if (!is.null(gender))   sets <- c(sets, sprintf("gender='%s'", gsub("'","''",gender %||% "")))
    if (!is.null(city))     sets <- c(sets, sprintf("city='%s'", gsub("'","''",city %||% "")))
    if (!is.null(province)) sets <- c(sets, sprintf("province='%s'", gsub("'","''",province %||% "")))
    if (!is.null(country))  sets <- c(sets, sprintf("country='%s'", gsub("'","''",country %||% "")))
    if (!is.null(status))   sets <- c(sets, sprintf("status=%d", as.integer(status)))
    if (!is.null(vip_level)) sets <- c(sets, sprintf("vip_level=%d", as.integer(vip_level)))
    if (!is.null(vip_expire_at)) sets <- c(sets, sprintf("vip_expire_at=%s", if(is.null(vip_expire_at)||vip_expire_at=="") "NULL" else sprintf("'%s'", gsub("'","''",vip_expire_at))))
    if (length(sets) == 0) return(list(success = FALSE, message = "无变更"))
    sets <- c(sets, "updated_at=datetime('now','localtime')")
    dbExecute(con, sprintf("UPDATE mini_program_users SET %s WHERE id=%d", paste(sets, collapse=","), as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

mp_user_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM vip_records WHERE user_id=%d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM mini_program_users WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

mp_user_toggle_status <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE mini_program_users SET status = CASE WHEN status=1 THEN 0 ELSE 1 END, updated_at=datetime('now','localtime') WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "状态已切换")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# VIP 会员记录
##################
vip_record_get_all <- function(level = "all", status = "all", keyword = NULL) {
  con <- db_connect()
  tryCatch({
    q <- "SELECT vr.*, u.nickname, u.phone, u.openid FROM vip_records vr LEFT JOIN mini_program_users u ON vr.user_id=u.id WHERE 1=1"
    if (!is.null(level) && level != "all") q <- paste0(q, sprintf(" AND vr.level=%d", as.integer(level)))
    if (!is.null(status) && status != "all") q <- paste0(q, sprintf(" AND vr.status=%d", as.integer(status)))
    if (!is.null(keyword) && nzchar(trimws(keyword))) {
      kw <- gsub("'", "''", trimws(keyword))
      q <- paste0(q, sprintf(" AND (u.nickname LIKE '%%%s%%' OR u.phone LIKE '%%%s%%' OR u.openid LIKE '%%%s%%')", kw, kw, kw))
    }
    q <- paste0(q, " ORDER BY vr.created_at DESC")
    dbGetQuery(con, q)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

vip_record_get_by_user <- function(user_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM vip_records WHERE user_id=%d ORDER BY created_at DESC", as.integer(user_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 开通/续费 VIP：记明细并更新用户表 VIP 状态（从有效到期日续期）
vip_record_add <- function(user_id, level = 1, duration_days = 30, amount = 0, source = "admin", remark = "") {
  con <- db_connect()
  tryCatch({
    u <- dbGetQuery(con, sprintf("SELECT * FROM mini_program_users WHERE id=%d", as.integer(user_id)))
    if (nrow(u) == 0) return(list(success = FALSE, message = "用户不存在"))
    today <- Sys.Date()
    start_date <- today
    # 已有有效 VIP 则续期
    if (!is.null(u$vip_expire_at) && !is.na(u$vip_expire_at) && u$vip_expire_at != "") {
      cur_exp <- as.Date(u$vip_expire_at)
      if (!is.na(cur_exp) && cur_exp >= today) start_date <- cur_exp
    }
    expire_date <- start_date + as.integer(duration_days)
    start_s <- format(start_date, "%Y-%m-%d")
    expire_s <- format(expire_date, "%Y-%m-%d")
    dbExecute(con, sprintf(
      "INSERT INTO vip_records (user_id, level, start_at, expire_at, duration_days, amount, source, status, remark)
       VALUES (%d, %d, '%s', '%s', %d, %f, '%s', 1, '%s')",
      as.integer(user_id), as.integer(level), start_s, expire_s, as.integer(duration_days),
      as.numeric(amount), gsub("'","''",source %||% "admin"), gsub("'","''",remark %||% "")))
    # 更新用户表 VIP 状态
    dbExecute(con, sprintf(
      "UPDATE mini_program_users SET vip_level=%d, vip_expire_at='%s', updated_at=datetime('now','localtime') WHERE id=%d",
      as.integer(level), expire_s, as.integer(user_id)))
    list(success = TRUE, message = sprintf("VIP 已开通/续期至 %s", expire_s))
  }, error = function(e) list(success = FALSE, message = paste("操作失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 删除 VIP 记录：删除后重算该用户当前 VIP 状态（取最新有效记录）
vip_record_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    rec <- dbGetQuery(con, sprintf("SELECT * FROM vip_records WHERE id=%d", as.integer(id)))
    if (nrow(rec) == 0) return(list(success = FALSE, message = "记录不存在"))
    user_id <- rec$user_id[1]
    dbExecute(con, sprintf("DELETE FROM vip_records WHERE id=%d", as.integer(id)))
    # 重算用户 VIP 状态
    latest <- dbGetQuery(con, sprintf(
      "SELECT * FROM vip_records WHERE user_id=%d AND status=1 ORDER BY expire_at DESC LIMIT 1", as.integer(user_id)))
    today <- Sys.Date()
    if (nrow(latest) > 0 && !is.na(as.Date(latest$expire_at[1])) && as.Date(latest$expire_at[1]) >= today) {
      dbExecute(con, sprintf("UPDATE mini_program_users SET vip_level=%d, vip_expire_at='%s', updated_at=datetime('now','localtime') WHERE id=%d",
        latest$level[1], latest$expire_at[1], as.integer(user_id)))
    } else {
      dbExecute(con, sprintf("UPDATE mini_program_users SET vip_level=0, vip_expire_at=NULL, updated_at=datetime('now','localtime') WHERE id=%d", as.integer(user_id)))
    }
    list(success = TRUE, message = "记录已删除")
  }, error = function(e) list(success = FALSE, message = e$message),
  finally = { db_disconnect(con) })
}

##################
# 统计与标签
##################
mp_user_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) n FROM mini_program_users")$n[1]
    today_new <- dbGetQuery(con, "SELECT COUNT(*) n FROM mini_program_users WHERE date(created_at)=date('now','localtime')")$n[1]
    vip_cnt <- dbGetQuery(con, "SELECT COUNT(*) n FROM mini_program_users WHERE vip_level > 0")$n[1]
    disabled <- dbGetQuery(con, "SELECT COUNT(*) n FROM mini_program_users WHERE status=0")$n[1]
    list(total = total, today_new = today_new, vip = vip_cnt, disabled = disabled)
  }, error = function(e) list(total = 0, today_new = 0, vip = 0, disabled = 0),
  finally = { db_disconnect(con) })
}

vip_stats <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y-%m-%d")
    total <- dbGetQuery(con, sprintf("SELECT COUNT(DISTINCT user_id) n FROM vip_records WHERE status=1 AND expire_at >= '%s'", today))$n[1]
    expiring <- dbGetQuery(con, sprintf(
      "SELECT COUNT(*) n FROM mini_program_users WHERE vip_level > 0 AND vip_expire_at >= '%s' AND vip_expire_at <= '%s'",
      today, format(Sys.Date() + 30, "%Y-%m-%d")))$n[1]
    expired <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM mini_program_users WHERE vip_level > 0 AND vip_expire_at < '%s'", today))$n[1]
    list(total = total, expiring = expiring, expired = expired)
  }, error = function(e) list(total = 0, expiring = 0, expired = 0),
  finally = { db_disconnect(con) })
}

vip_level_label <- function(level) {
  lv <- as.integer(level)
  if (is.na(lv) || lv <= 0) return("非VIP")
  sprintf("VIP%d", lv)
}
