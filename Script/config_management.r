##################
# 配置管理模块 — 数据层
# 资产模块「配置管理」子标签：多平台服务器/资源清单 + 账号密码管理
# 表：config_platforms, config_items, config_item_accounts
##################

# 配置项可展示字段清单（键 -> 中文标签）
CONFIG_ITEM_FIELDS <- c(
  seq_no        = "序号",
  platform_name = "平台",
  category      = "资源类别",
  env           = "环境",
  resource_name = "资源名称",
  internal_ip   = "内网IP",
  os_spec       = "操作系统/规格",
  cpu           = "CPU(核)",
  ram           = "内存(G)",
  sys_disk      = "系统盘(G)",
  data_disk     = "数据盘(G)",
  public_addr   = "公网地址",
  public_port   = "公网端口",
  internal_port = "内网端口",
  account       = "账号/密码",
  remark        = "备注"
)

##################
# 平台管理
##################
config_platform_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM config_platforms ORDER BY sort_order, id")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

config_platform_add <- function(platform_name, description = "", sort_order = 0) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO config_platforms (platform_name, description, sort_order) VALUES ('%s','%s',%d)",
      gsub("'","''",platform_name), gsub("'","''",description %||% ""), as.integer(sort_order %||% 0)))
    list(success = TRUE, message = "平台已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

config_platform_update <- function(id, platform_name = NULL, description = NULL, sort_order = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(platform_name)) sets <- paste0(sets, sprintf(", platform_name='%s'", gsub("'","''",platform_name)))
    if (!is.null(description))  sets <- paste0(sets, sprintf(", description='%s'", gsub("'","''",description)))
    if (!is.null(sort_order))   sets <- paste0(sets, sprintf(", sort_order=%d", as.integer(sort_order)))
    dbExecute(con, sprintf("UPDATE config_platforms SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "平台已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

config_platform_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    # 删除前把该平台下配置项的 platform_id 置空
    dbExecute(con, sprintf("UPDATE config_items SET platform_id = NULL WHERE platform_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM config_platforms WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "平台已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 获取全部配置项（联表平台名，按平台+序号排序）
##################
config_item_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT ci.*, p.platform_name
      FROM config_items ci
      LEFT JOIN config_platforms p ON ci.platform_id = p.id
      ORDER BY p.sort_order, COALESCE(ci.seq_no, 999999), ci.id")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

##################
# 获取单条
##################
config_item_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT ci.*, p.platform_name
      FROM config_items ci
      LEFT JOIN config_platforms p ON ci.platform_id = p.id
      WHERE ci.id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

##################
# 新增配置项（返回新 id）
##################
config_item_add <- function(seq_no = NULL, platform_id = NULL, category = "", env = "", resource_name = "",
  internal_ip = "", os_spec = "", cpu = "", ram = "", sys_disk = "", data_disk = "",
  public_addr = "", public_port = "", internal_port = "", remark = "", created_by = NULL) {
  con <- db_connect()
  tryCatch({
    s <- function(x) if (is.null(x) || is.na(x) || x == "") "NULL" else sprintf("'%s'", gsub("'", "''", as.character(x)))
    seq_val <- if (is.null(seq_no) || is.na(seq_no) || seq_no == "") "NULL" else as.character(as.integer(seq_no))
    pid_val <- if (is.null(platform_id) || is.na(platform_id) || platform_id == "") "NULL" else as.character(as.integer(platform_id))
    dbExecute(con, sprintf(
      "INSERT INTO config_items (seq_no, platform_id, category, env, resource_name, internal_ip, os_spec, cpu, ram, sys_disk, data_disk, public_addr, public_port, internal_port, remark, created_by)
       VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)",
      seq_val, pid_val, s(category), s(env), s(resource_name), s(internal_ip), s(os_spec),
      s(cpu), s(ram), s(sys_disk), s(data_disk), s(public_addr), s(public_port),
      s(internal_port), s(remark), if (is.null(created_by)) "NULL" else as.character(as.integer(created_by))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    list(success = TRUE, id = id, message = "已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 更新配置项
##################
config_item_update <- function(id, seq_no = NULL, platform_id = NULL, category = NULL, env = NULL, resource_name = NULL,
  internal_ip = NULL, os_spec = NULL, cpu = NULL, ram = NULL, sys_disk = NULL, data_disk = NULL,
  public_addr = NULL, public_port = NULL, internal_port = NULL, remark = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(seq_no))         sets <- paste0(sets, sprintf(", seq_no=%d", as.integer(seq_no)))
    if (!is.null(platform_id))    sets <- paste0(sets, sprintf(", platform_id=%s",
      if (is.na(platform_id) || platform_id == "") "NULL" else as.character(as.integer(platform_id))))
    if (!is.null(category))       sets <- paste0(sets, sprintf(", category='%s'", gsub("'","''",category)))
    if (!is.null(env))            sets <- paste0(sets, sprintf(", env='%s'", gsub("'","''",env)))
    if (!is.null(resource_name))  sets <- paste0(sets, sprintf(", resource_name='%s'", gsub("'","''",resource_name)))
    if (!is.null(internal_ip))    sets <- paste0(sets, sprintf(", internal_ip='%s'", gsub("'","''",internal_ip)))
    if (!is.null(os_spec))        sets <- paste0(sets, sprintf(", os_spec='%s'", gsub("'","''",os_spec)))
    if (!is.null(cpu))            sets <- paste0(sets, sprintf(", cpu='%s'", gsub("'","''",cpu)))
    if (!is.null(ram))            sets <- paste0(sets, sprintf(", ram='%s'", gsub("'","''",ram)))
    if (!is.null(sys_disk))       sets <- paste0(sets, sprintf(", sys_disk='%s'", gsub("'","''",sys_disk)))
    if (!is.null(data_disk))      sets <- paste0(sets, sprintf(", data_disk='%s'", gsub("'","''",data_disk)))
    if (!is.null(public_addr))    sets <- paste0(sets, sprintf(", public_addr='%s'", gsub("'","''",public_addr)))
    if (!is.null(public_port))    sets <- paste0(sets, sprintf(", public_port='%s'", gsub("'","''",public_port)))
    if (!is.null(internal_port))  sets <- paste0(sets, sprintf(", internal_port='%s'", gsub("'","''",internal_port)))
    if (!is.null(remark))         sets <- paste0(sets, sprintf(", remark='%s'", gsub("'","''",remark)))
    dbExecute(con, sprintf("UPDATE config_items SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 删除配置项（级联删除账号）
##################
config_item_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM config_item_accounts WHERE item_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM config_items WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 账号管理（一条配置项多个账号）
##################
config_account_get_by_item <- function(item_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM config_item_accounts WHERE item_id = %d ORDER BY sort_order, id", as.integer(item_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

config_account_add <- function(item_id, username = "", password = "", remark = "", sort_order = 0) {
  con <- db_connect()
  tryCatch({
    s <- function(x) if (is.null(x) || is.na(x) || x == "") "NULL" else sprintf("'%s'", gsub("'","''",as.character(x)))
    dbExecute(con, sprintf(
      "INSERT INTO config_item_accounts (item_id, username, password, remark, sort_order) VALUES (%d,%s,%s,%s,%d)",
      as.integer(item_id), s(username), s(password), s(remark), as.integer(sort_order %||% 0)))
    list(success = TRUE, message = "账号已添加")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

config_account_update <- function(id, username = NULL, password = NULL, remark = NULL, sort_order = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- ""
    if (!is.null(username))  sets <- paste0(sets, sprintf("username='%s'", gsub("'","''",username)))
    if (!is.null(password))  {
      sets <- paste0(sets, if (nchar(sets) > 0) "," else "", sprintf("password='%s'", gsub("'","''",password)))
    }
    if (!is.null(remark))    sets <- paste0(sets, if (nchar(sets) > 0) "," else "", sprintf("remark='%s'", gsub("'","''",remark)))
    if (!is.null(sort_order)) sets <- paste0(sets, if (nchar(sets) > 0) "," else "", sprintf("sort_order=%d", as.integer(sort_order)))
    if (nchar(sets) == 0) return(list(success = TRUE, message = "无变更"))
    dbExecute(con, sprintf("UPDATE config_item_accounts SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "账号已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

config_account_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM config_item_accounts WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "账号已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 密码脱敏：前4位明文 + 后续 *
##################
config_password_mask <- function(pwd) {
  if (is.null(pwd) || is.na(pwd) || nchar(pwd) == 0) return("")
  n <- nchar(pwd)
  if (n <= 4) return(paste(rep("*", n), collapse = ""))
  paste0(substr(pwd, 1, 4), paste(rep("*", n - 4), collapse = ""))
}

##################
# 统计（总数 + 按资源类别 + 平台数）
##################
config_item_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_items")$n[1]
    platform_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_platforms")$n[1]
    by_cat <- dbGetQuery(con, "SELECT category, COUNT(*) AS n FROM config_items GROUP BY category ORDER BY n DESC")
    list(total = total %||% 0L, platform_n = platform_n %||% 0L, by_category = by_cat)
  }, error = function(e) list(total = 0L, platform_n = 0L, by_category = data.frame()),
  finally = { db_disconnect(con) })
}

##################
# 展示字段配置（存 system_config，JSON）
##################
config_field_get_visible <- function() {
  default_fields <- names(CONFIG_ITEM_FIELDS)
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT config_value FROM system_config WHERE config_key='cfgitem_visible_fields'")
    if (nrow(r) == 0 || is.na(r$config_value[1]) || r$config_value[1] == "") return(default_fields)
    fields <- tryCatch(jsonlite::fromJSON(r$config_value[1]), error = function(e) NULL)
    if (is.null(fields) || length(fields) == 0) return(default_fields)
    # 只保留合法字段，并补上漏掉的默认字段（追加在后）
    fields <- fields[fields %in% names(CONFIG_ITEM_FIELDS)]
    missing <- default_fields[!default_fields %in% fields]
    c(fields, missing)
  }, error = function(e) default_fields, finally = { db_disconnect(con) })
}

config_field_set_visible <- function(fields) {
  con <- db_connect()
  tryCatch({
    fields <- fields[fields %in% names(CONFIG_ITEM_FIELDS)]
    json <- jsonlite::toJSON(fields, auto_unbox = TRUE)
    exists <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='cfgitem_visible_fields'")
    if (nrow(exists) > 0) {
      dbExecute(con, sprintf("UPDATE system_config SET config_value='%s', updated_at=datetime('now','localtime') WHERE config_key='cfgitem_visible_fields'",
        gsub("'","''",as.character(json))))
    } else {
      dbExecute(con, sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('cfgitem_visible_fields','%s','配置管理展示字段顺序')",
        gsub("'","''",as.character(json))))
    }
    list(success = TRUE, message = "字段配置已保存")
  }, error = function(e) list(success = FALSE, message = paste("保存失败:", e$message)),
  finally = { db_disconnect(con) })
}

##################
# 种子数据（幂等：仅空表时写入）
# 平台：托管云平台；配置项：15 条服务器资源清单
##################
config_item_seed <- function() {
  con <- db_connect()
  tryCatch({
    if (!"config_platforms" %in% dbListTables(con)) return(invisible(NULL))
    # 种子平台
    pcnt <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_platforms")$n[1]
    if (pcnt == 0) {
      dbExecute(con, "INSERT INTO config_platforms (platform_name, description, sort_order) VALUES ('托管云平台','托管云平台的服务器资源清单',1)")
    }
    if (!"config_items" %in% dbListTables(con)) return(invisible(NULL))
    cnt <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_items")$n[1]
    if (cnt > 0) return(invisible(NULL))
    pid <- dbGetQuery(con, "SELECT id FROM config_platforms WHERE platform_name='托管云平台' ORDER BY id LIMIT 1")$id[1]
    rows <- list(
      list(1,   "服务器", "业务环境", "10 主服务 + 前端服务",              "10.100.0.2", "CentOS 9.2 (x86)", "8",  "32", "60",  "240", "117.162.0.171", "22222", "22", "业务核心"),
      list(2,   "服务器", "业务环境", "二开服务(含检查服务) + 统一认证服务", "10.100.0.3", "CentOS 9.2 (x86)", "8",  "8",  "60",  "240", "117.162.0.171", "22223", "22", ""),
      list(3,   "服务器", "业务环境", "周边服务 + 微搜(ES) 服务",          "10.100.0.4", "CentOS 9.2 (x86)", "8",  "8",  "60",  "440", "117.162.0.171", "22224", "22", ""),
      list(4,   "服务器", "业务环境", "Nginx + 运维平台服务",              "10.100.0.5", "CentOS 9.2 (x86)", "8",  "32", "60",  "440", "117.162.0.171", "22225", "22", "运维平台入口，含监控面板"),
      list(5,   "服务器", "业务环境", "即时通讯 IM 服务 1",                "10.100.0.6", "CentOS 9.2 (x86)", "16", "32", "60",  "440", "117.162.0.171", "22226", "22", ""),
      list(6,   "服务器", "业务环境", "即时通讯 IM 服务 1",                "10.100.0.7", "CentOS 9.2 (x86)", "8",  "32", "100", "200", "117.162.0.171", "22227", "22", "原清单与上行同名，建议核实为 IM 节点 2"),
      list(7,   "服务器", "业务环境", "NAS 存储服务器",                    "10.100.0.8", "CentOS 9.2 (x86)", "8",  "8",  "60",  "940", "117.162.0.171", "22228", "22", ""),
      list(8,   "数据库", "业务环境", "平台 MySQL 8.0.37（江西驴充充充数据库01）", "10.200.0.2", "CentOS 9.2 (x86)", "16", "32", "100", "450", "", "", "3306", "读写地址 10.200.0.2:3306；另含 200G 日志盘"),
      list(9,   "数据库", "业务环境", "内网连接数据库主机",                "10.200.0.3", "Windows Server 2016", "4", "8", "80", "", "", "", "", "通过 ToDesk 远程接入"),
      list(10,  "数据库", "业务环境", "VJX03ADS03",                        "10.100.0.9", "Windows Server 2025", "4", "16", "100", "400", "", "", "", ""),
      list(11,  "数据库", "业务环境", "RDS MySQL 16C32G 单机版（专属区）", "",           "MySQL 5.7 / 8.0",   "16", "32", "",  "",   "", "", "", "含备份恢复、监控告警服务"),
      list(12,  "网络",   "业务环境", "移动带宽（江西南昌一区）/电信线路", "",           "100Mbps 共享/80Mbps", "", "", "", "", "", "", "", ""),
      list(13,  "网络",   "业务环境", "移动公网 IP（江西南昌一区）/电信",  "",           "2 个/1个",           "", "", "", "", "", "", "", "移动：117.162.0.171/117.162.0.172；电信：218.87.13.52；域名：ecs.lvcchong.com:20600"),
      list(14,  "网络",   "业务环境", "下一代防火墙 vAF-100M",             "",           "应用层吞吐 100M",    "2", "4",  "120", "",  "", "", "", ""),
      list(15,  "网络",   "业务环境", "应用交付 vAD-500M",                 "",           "四层吞吐 500Mbps",   "",  "",   "",    "",  "", "", "", "IPSec 授权（IPsec-50M 随版），单臂部署")
    )
    for (r in rows) {
      config_item_add(
        seq_no = r[[1]], platform_id = pid, category = r[[2]], env = r[[3]], resource_name = r[[4]],
        internal_ip = r[[5]], os_spec = r[[6]], cpu = r[[7]], ram = r[[8]],
        sys_disk = r[[9]], data_disk = r[[10]], public_addr = r[[11]],
        public_port = r[[12]], internal_port = r[[13]], remark = r[[14]]
      )
    }
    cat("数据库迁移完成：已写入 config_platforms/config_items 种子数据\n")
  }, error = function(e) cat("config_items 种子数据写入失败:", e$message, "\n"),
  finally = { db_disconnect(con) })
}

##################
# 国际访问平台（LightWAN 联通国际访问）— 账号清单
# 需求：资产→配置管理 新增平台「国际访问」，字段与服务器/数据库/网络平台不同，
#       独立使用 config_intl_accounts 表（账号/密码/使用人/日期/备注）。
#       支持单条「复制标签」和整单「导出」。
##################

# 平台固定信息
CONFIG_INTL <- list(
  platform_name = "国际访问",
  title         = "LightWAN（联通国际访问）",
  customer_id   = "917530003"
)

# 自包含建表（不修改 global.R）
config_intl_table_init <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS config_intl_accounts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    account TEXT NOT NULL,
    password TEXT,
    user_name TEXT,
    assign_date TEXT,
    remark TEXT,
    sort_order INTEGER DEFAULT 0,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  invisible(TRUE)
}

# 使用人展示文本：使用人 + 日期（有日期拼日期，没日期空着）
config_intl_user_display <- function(user_name, assign_date, remark) {
  un <- if (is.null(user_name) || is.na(user_name)) "" else as.character(user_name)
  ad <- if (is.null(assign_date) || is.na(assign_date)) "" else as.character(assign_date)
  rk <- if (is.null(remark) || is.na(remark)) "" else as.character(remark)
  if (nzchar(un)) {
    s <- un
    if (nzchar(rk) && rk != "未分配") s <- paste0(s, "（", rk, "）")
    if (nzchar(ad)) s <- paste0(s, ad)
    return(s)
  }
  if (nzchar(rk)) return(rk)
  "未分配"
}

# 生成单条标签文本（复制给用户）
config_intl_label_text <- function(account, password, user_name) {
  un <- if (is.null(user_name) || is.na(user_name) || as.character(user_name) == "") "未分配" else as.character(user_name)
  paste0(
    CONFIG_INTL$title, "\n",
    "客户ID：", CONFIG_INTL$customer_id, "\n",
    "账号：", account, "\n",
    "密码：", password, "\n",
    "使用人：", un
  )
}

# 获取全部国际访问账号
config_intl_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM config_intl_accounts ORDER BY sort_order, id")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 删除单条账号
config_intl_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM config_intl_accounts WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 幂等写入：平台 + 账号清单（独立表，仅空表时写入）
config_intl_seed <- function() {
  config_intl_table_init()
  con <- db_connect()
  tryCatch({
    if (!"config_platforms" %in% dbListTables(con)) return(invisible(NULL))
    plat <- dbGetQuery(con, sprintf("SELECT id FROM config_platforms WHERE platform_name='%s'", CONFIG_INTL$platform_name))
    if (nrow(plat) == 0) {
      dbExecute(con, sprintf(
        "INSERT INTO config_platforms (platform_name, description, sort_order) VALUES ('%s','%s',2)",
        CONFIG_INTL$platform_name,
        paste0(CONFIG_INTL$title, "，客户ID：", CONFIG_INTL$customer_id)))
    }
    cnt <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_intl_accounts")$n[1]
    if (cnt > 0) return(invisible(NULL))

    # 账号 / 密码 / 使用人 / 日期 / 备注
    rows <- list(
      c("LVCC001", "Lvcc@2026#4459", "宋春艳", "", ""),
      c("LVCC002", "Lvcc@2026#0831", "徐花", "", "重置1次"),
      c("LVCC003", "Lvcc@2026#9127", "", "", "未分配"),
      c("LVCC004", "Lvcc@2026#8648", "", "", "未分配"),
      c("LVCC005", "Lvcc@2026#9278", "", "", "未分配"),
      c("LVCC006", "Lvcc@2026#2342", "", "", "未分配"),
      c("LVCC007", "Lvcc@2026#7386", "", "", "未分配"),
      c("LVCC008", "Lvcc@2026#8784", "", "", "未分配"),
      c("LVCC009", "Lvcc@2026#4426", "", "", "未分配"),
      c("LVCC010", "Lvcc@2026#4407", "", "", "未分配"),
      c("LVCC011", "Lvcc@2026#9127", "邓毅林", "10月7", ""),
      c("LVCC012", "Lvcc@2026#6085", "郭庆", "9月18", ""),
      c("LVCC013", "Lvcc@2026#5663", "刘嘉林", "9月18", ""),
      c("LVCC014", "Lvcc@2026#9984", "邱祖荣", "9月18", ""),
      c("LVCC015", "Lvcc@2026#2780", "朱德龙", "9月18", ""),
      c("LVCC016", "Lvcc@2026#3214", "谢金隆", "9月18", ""),
      c("LVCC017", "Lvcc@2026#8705", "黄太财", "9月18", ""),
      c("LVCC018", "Lvcc@2026#1062", "易伟海", "9月18", ""),
      c("LVCC019", "Lvcc@2026#5548", "陈思宇", "9月24", ""),
      c("LVCC020", "Lvcc@2026#3944", "肖橦", "", "9-23重置1次"),
      c("LVCC101", "Lvcc@2026#3359", "郭庆", "10月14", ""),
      c("LVCC102", "Lvcc@2026#6267", "邱祖荣", "10月14", ""),
      c("LVCC103", "Lvcc@2026#3657", "吴隆生", "10月14", ""),
      c("LVCC104", "Lvcc@2026#5369", "易伟海", "10月14", ""),
      c("LVCC105", "Lvcc@2026#9389", "谢金隆", "10月14", ""),
      c("LVCC106", "Lvcc@2026#6689", "陈坤", "10月14", ""),
      c("LVCC107", "Lvcc@2026#2379", "陈慈鸿", "10月14", ""),
      c("LVCC108", "Lvcc@2026#7356", "肖鹏", "10月14", ""),
      c("LVCC109", "Lvcc@2026#3769", "李诚", "10月14", ""),
      c("LVCC110", "Lvcc@2026#3659", "龙永鹏", "10月14", ""),
      c("LVCC111", "Lvcc@2026#6339", "朱向东", "10月14", ""),
      c("LVCC112", "Lvcc@2026#1357", "刘晓华", "10月14", ""),
      c("jxlcc01", "GXlcc@2099..", "", "", "扩容前多人共用")
    )
    for (i in seq_along(rows)) {
      r <- rows[[i]]
      dbExecute(con, sprintf(
        "INSERT INTO config_intl_accounts (account, password, user_name, assign_date, remark, sort_order)
         VALUES ('%s','%s','%s','%s','%s',%d)",
        gsub("'","''",r[1]), gsub("'","''",r[2]), gsub("'","''",r[3]),
        gsub("'","''",r[4]), gsub("'","''",r[5]), i))
    }
    cat("数据库迁移完成：已写入「国际访问」账号清单", length(rows), "条\n")
  }, error = function(e) cat("国际访问账号清单写入失败:", e$message, "\n"),
  finally = { db_disconnect(con) })
}

# 导出国际访问账号清单（文本格式）
config_intl_export_text <- function() {
  con <- db_connect()
  tryCatch({
    items <- dbGetQuery(con, "SELECT * FROM config_intl_accounts ORDER BY sort_order, id")
    if (nrow(items) == 0) return("")
    lines <- c(
      paste0(CONFIG_INTL$title, "分配清单："),
      paste0("客户ID：", CONFIG_INTL$customer_id)
    )
    for (i in seq_len(nrow(items))) {
      u <- config_intl_user_display(items$user_name[i], items$assign_date[i], items$remark[i])
      lines <- c(lines, sprintf("%s，PW：%s，%s",
        items$account[i] %||% "", items$password[i] %||% "", u))
    }
    paste(lines, collapse = "\n")
  }, error = function(e) "", finally = { db_disconnect(con) })
}

##################
# 数据库平台 — 数据层
# 需求：配置管理新增「数据库」平台，记录数据库连接信息，字段充足，支持查询/复制/更新/版本回看
# 表：config_databases（主表）+ config_database_versions（版本历史快照）
##################

CONFIG_DB_TYPES <- c("MySQL", "Oracle", "SQL Server", "PostgreSQL", "Redis", "MongoDB", "其他")

# 自包含建表（不修改 global.R 迁移逻辑）
config_db_table_init <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS config_databases (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    db_no TEXT,
    db_name TEXT NOT NULL,
    db_type TEXT,
    jdbc_url TEXT,
    internal_addr TEXT,
    username TEXT,
    password TEXT,
    remark TEXT,
    sort_order INTEGER DEFAULT 0,
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS config_database_versions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_id INTEGER NOT NULL,
    version_no INTEGER DEFAULT 1,
    db_name TEXT, db_type TEXT, jdbc_url TEXT, internal_addr TEXT,
    username TEXT, password TEXT, remark TEXT,
    changed_by INTEGER,
    changed_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_cfgdb_versions_db ON config_database_versions(database_id)")
  invisible(TRUE)
}

# 编号生成 DB+YYYYMMDD+3位流水
config_db_generate_number <- function() {
  con <- db_connect()
  tryCatch({
    today <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("DB", today)
    existing <- dbGetQuery(con, sprintf("SELECT db_no FROM config_databases WHERE db_no LIKE '%s%%' ORDER BY db_no DESC LIMIT 1", prefix))
    if (nrow(existing) > 0 && !is.na(existing$db_no[1])) {
      last_seq <- as.integer(substr(existing$db_no[1], nchar(prefix) + 1, nchar(prefix) + 3))
      seq <- if (is.na(last_seq)) 1 else last_seq + 1
    } else { seq <- 1 }
    sprintf("%s%03d", prefix, seq)
  }, finally = { db_disconnect(con) })
}

# 字符串安全（NULL/NA/空 -> NULL 字面量）
.cfgdb_s <- function(x) if (is.null(x) || is.na(x) || as.character(x) == "") "NULL" else sprintf("'%s'", gsub("'", "''", as.character(x)))

# 获取全部数据库
config_db_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM config_databases ORDER BY sort_order, id")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取单条
config_db_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM config_databases WHERE id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

# 新增数据库（写入主表 + 记录版本 v1）
config_db_add <- function(db_name, db_type = "", jdbc_url = "", internal_addr = "",
  username = "", password = "", remark = "", created_by = NULL, sort_order = 0) {
  con <- db_connect()
  tryCatch({
    db_no <- config_db_generate_number()
    dbExecute(con, sprintf(
      "INSERT INTO config_databases (db_no, db_name, db_type, jdbc_url, internal_addr, username, password, remark, sort_order, created_by)
       VALUES ('%s',%s,%s,%s,%s,%s,%s,%s,%d,%s)",
      db_no, .cfgdb_s(db_name), .cfgdb_s(db_type), .cfgdb_s(jdbc_url), .cfgdb_s(internal_addr),
      .cfgdb_s(username), .cfgdb_s(password), .cfgdb_s(remark), as.integer(sort_order %||% 0),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by))))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    # 记录初始版本 v1
    dbExecute(con, sprintf(
      "INSERT INTO config_database_versions (database_id, version_no, db_name, db_type, jdbc_url, internal_addr, username, password, remark, changed_by)
       VALUES (%d, 1, %s, %s, %s, %s, %s, %s, %s, %s)",
      id, .cfgdb_s(db_name), .cfgdb_s(db_type), .cfgdb_s(jdbc_url), .cfgdb_s(internal_addr),
      .cfgdb_s(username), .cfgdb_s(password), .cfgdb_s(remark),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by))))
    list(success = TRUE, id = id, db_no = db_no, message = paste("已添加", db_no))
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 更新数据库（更新主表 + 追加新版本快照）
config_db_update <- function(id, db_name = NULL, db_type = NULL, jdbc_url = NULL,
  internal_addr = NULL, username = NULL, password = NULL, remark = NULL,
  sort_order = NULL, changed_by = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(db_name))       sets <- paste0(sets, sprintf(", db_name=%s", .cfgdb_s(db_name)))
    if (!is.null(db_type))       sets <- paste0(sets, sprintf(", db_type=%s", .cfgdb_s(db_type)))
    if (!is.null(jdbc_url))      sets <- paste0(sets, sprintf(", jdbc_url=%s", .cfgdb_s(jdbc_url)))
    if (!is.null(internal_addr)) sets <- paste0(sets, sprintf(", internal_addr=%s", .cfgdb_s(internal_addr)))
    if (!is.null(username))      sets <- paste0(sets, sprintf(", username=%s", .cfgdb_s(username)))
    if (!is.null(password))      sets <- paste0(sets, sprintf(", password=%s", .cfgdb_s(password)))
    if (!is.null(remark))        sets <- paste0(sets, sprintf(", remark=%s", .cfgdb_s(remark)))
    if (!is.null(sort_order))    sets <- paste0(sets, sprintf(", sort_order=%d", as.integer(sort_order)))
    dbExecute(con, sprintf("UPDATE config_databases SET %s WHERE id = %d", sets, as.integer(id)))

    # 追加新版本快照（取更新后的完整行）
    row <- dbGetQuery(con, sprintf("SELECT * FROM config_databases WHERE id = %d", as.integer(id)))
    if (nrow(row) > 0) {
      cur <- dbGetQuery(con, sprintf("SELECT COALESCE(MAX(version_no),0) AS v FROM config_database_versions WHERE database_id = %d", as.integer(id)))$v[1]
      dbExecute(con, sprintf(
        "INSERT INTO config_database_versions (database_id, version_no, db_name, db_type, jdbc_url, internal_addr, username, password, remark, changed_by)
         VALUES (%d, %d, %s, %s, %s, %s, %s, %s, %s, %s)",
        as.integer(id), cur + 1L,
        .cfgdb_s(row$db_name[1]), .cfgdb_s(row$db_type[1]), .cfgdb_s(row$jdbc_url[1]),
        .cfgdb_s(row$internal_addr[1]), .cfgdb_s(row$username[1]), .cfgdb_s(row$password[1]),
        .cfgdb_s(row$remark[1]),
        if (is.null(changed_by)) "NULL" else as.character(as.integer(changed_by))))
    }
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 删除数据库（级联删除版本）
config_db_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM config_database_versions WHERE database_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM config_databases WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 获取某数据库的版本历史（倒序）
config_db_versions <- function(database_id) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT v.*, COALESCE(NULLIF(u.display_name,''), u.username) AS changer_name
       FROM config_database_versions v LEFT JOIN users u ON v.changed_by = u.id
       WHERE v.database_id = %d ORDER BY v.version_no DESC", as.integer(database_id)))
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 复制标签文本（完整连接信息，供复制给用户）
config_db_label_text <- function(db_name, db_type, jdbc_url, internal_addr, username, password) {
  paste0(
    "数据库：", db_name %||% "", "\n",
    "类型：", db_type %||% "", "\n",
    "JDBC：", jdbc_url %||% "", "\n",
    "内网：", internal_addr %||% "", "\n",
    "账号：", username %||% "", "\n",
    "密码：", password %||% ""
  )
}

# 统计
config_db_get_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_databases")$n[1]
    by_type <- dbGetQuery(con, "SELECT db_type, COUNT(*) AS n FROM config_databases GROUP BY db_type ORDER BY n DESC")
    list(total = total %||% 0L, by_type = by_type)
  }, error = function(e) list(total = 0L, by_type = data.frame()),
  finally = { db_disconnect(con) })
}

# 种子数据（幂等：平台 + 仅空表时写入 CE10PDB / VE10TDB）
config_db_seed <- function() {
  config_db_table_init()
  con <- db_connect()
  tryCatch({
    if (!"config_platforms" %in% dbListTables(con)) return(invisible(NULL))
    plat <- dbGetQuery(con, "SELECT id FROM config_platforms WHERE platform_name='数据库'")
    if (nrow(plat) == 0) {
      dbExecute(con, "INSERT INTO config_platforms (platform_name, description, sort_order) VALUES ('数据库','数据库连接信息清单',3)")
    }
    cnt <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM config_databases")$n[1]
    if (cnt > 0) return(invisible(NULL))

    rows <- list(
      c("CE10PDB", "MySQL", "jdbc:mysql://117.162.0.171:13306/", "10.200.0.2:3306", "root", "PMGn9Lnp325JBm1I", ""),
      c("VE10TDB", "MySQL", "jdbc:mysql://10.10.50.76:3306/", "", "root", "Lvcce10@db..", "")
    )
    for (i in seq_along(rows)) {
      r <- rows[[i]]
      config_db_add(
        db_name = r[1], db_type = r[2], jdbc_url = r[3], internal_addr = r[4],
        username = r[5], password = r[6], remark = r[7], sort_order = i
      )
    }
    cat("数据库迁移完成：已写入「数据库」平台连接信息", length(rows), "条\n")
  }, error = function(e) cat("数据库平台种子数据写入失败:", e$message, "\n"),
  finally = { db_disconnect(con) })
}
