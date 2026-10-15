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

##################
# 产品模块 — 数据层
# 产品价格数据来源于 D:/Tai_LVCC_2027 下的价格表，每条产品记录来源文件名
##################

PRODUCT_PRICE_DIR <- "D:/Tai_LVCC_2027"
PRODUCT_PRICE_FILES <- c(
  "电瓶车价格表2026.6.16版--全员.xlsx",
  "汽车直流桩价格表2026.08.26版--全员.xlsx"
)

product_get_all <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM products ORDER BY category, id ASC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

product_get_by_id <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM products WHERE id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, finally = { db_disconnect(con) })
}

product_add <- function(name, category = "", model = "", network = "", retail_price = "",
  warranty = "", service_fee = "", features = "", weight = "", source_file = "",
  notes = "") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO products (category, name, model, network, retail_price, warranty, service_fee, features, weight, source_file, notes)
       VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
      gsub("'","''",category), gsub("'","''",name), gsub("'","''",model),
      gsub("'","''",network), gsub("'","''",retail_price), gsub("'","''",warranty),
      gsub("'","''",service_fee), gsub("'","''",features), gsub("'","''",weight),
      gsub("'","''",source_file), gsub("'","''",notes)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success = TRUE, id = id, message = "已添加产品")
  }, error = function(e) list(success = FALSE, message = paste("添加失败:", e$message)),
  finally = { db_disconnect(con) })
}

product_update <- function(id, name = NULL, category = NULL, model = NULL, network = NULL,
  retail_price = NULL, warranty = NULL, service_fee = NULL, features = NULL,
  weight = NULL, source_file = NULL, notes = NULL) {
  con <- db_connect()
  tryCatch({
    sets <- "updated_at = datetime('now','localtime')"
    if (!is.null(name))         sets <- paste0(sets, sprintf(", name='%s'", gsub("'","''",name)))
    if (!is.null(category))     sets <- paste0(sets, sprintf(", category='%s'", gsub("'","''",category)))
    if (!is.null(model))        sets <- paste0(sets, sprintf(", model='%s'", gsub("'","''",model)))
    if (!is.null(network))      sets <- paste0(sets, sprintf(", network='%s'", gsub("'","''",network)))
    if (!is.null(retail_price)) sets <- paste0(sets, sprintf(", retail_price='%s'", gsub("'","''",retail_price)))
    if (!is.null(warranty))     sets <- paste0(sets, sprintf(", warranty='%s'", gsub("'","''",warranty)))
    if (!is.null(service_fee))  sets <- paste0(sets, sprintf(", service_fee='%s'", gsub("'","''",service_fee)))
    if (!is.null(features))     sets <- paste0(sets, sprintf(", features='%s'", gsub("'","''",features)))
    if (!is.null(weight))       sets <- paste0(sets, sprintf(", weight='%s'", gsub("'","''",weight)))
    if (!is.null(source_file))  sets <- paste0(sets, sprintf(", source_file='%s'", gsub("'","''",source_file)))
    if (!is.null(notes))        sets <- paste0(sets, sprintf(", notes='%s'", gsub("'","''",notes)))
    dbExecute(con, sprintf("UPDATE products SET %s WHERE id = %d", sets, as.integer(id)))
    list(success = TRUE, message = "已更新")
  }, error = function(e) list(success = FALSE, message = paste("更新失败:", e$message)),
  finally = { db_disconnect(con) })
}

product_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM products WHERE id = %d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = paste("删除失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 解析单个价格表数据框为产品行列表
# kind: "ebike" 电瓶车(8列) / "dc" 汽车直流桩(6列)
product_parse_price_table <- function(df, source_file, kind) {
  rows <- list()
  last_name <- ""
  n <- nrow(df)
  for (i in seq_len(n)) {
    vals <- as.character(df[i, ])
    vals[is.na(vals)] <- ""
    vals <- trimws(vals)
    if (length(vals) < 2) next
    c1 <- vals[1]
    if (grepl("价格表", c1)) next                     # 标题行
    if (grepl("^产品名称", c1)) next                   # 表头行
    if (grepl("^注[:：]", c1)) next                    # 注释行
    if (all(vals == "")) next                         # 空行
    if (c1 != "") last_name <- c1                     # 新名称，否则继承上一行

    if (identical(kind, "ebike")) {
      rows[[length(rows) + 1]] <- list(
        category = "电瓶车", name = last_name,
        model = vals[2], network = vals[3], retail_price = vals[4],
        warranty = vals[5], service_fee = vals[6], features = vals[7],
        weight = vals[8], source_file = source_file
      )
    } else {
      rows[[length(rows) + 1]] <- list(
        category = "汽车直流桩", name = last_name,
        model = vals[2], retail_price = vals[3], service_fee = vals[4],
        warranty = vals[5], weight = vals[6], source_file = source_file
      )
    }
  }
  rows
}

# 批量插入产品行（返回插入条数）
product_insert_rows <- function(con, rows) {
  n <- 0
  for (r in rows) {
    r$category   <- r$category   %||% ""
    r$name       <- r$name       %||% ""
    r$model      <- r$model      %||% ""
    r$network    <- r$network    %||% ""
    r$retail_price <- r$retail_price %||% ""
    r$warranty   <- r$warranty   %||% ""
    r$service_fee <- r$service_fee %||% ""
    r$features   <- r$features   %||% ""
    r$weight     <- r$weight     %||% ""
    r$source_file <- r$source_file %||% ""
    dbExecute(con, sprintf(
      "INSERT INTO products (category, name, model, network, retail_price, warranty, service_fee, features, weight, source_file)
       VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
      gsub("'","''",r$category), gsub("'","''",r$name), gsub("'","''",r$model),
      gsub("'","''",r$network), gsub("'","''",r$retail_price), gsub("'","''",r$warranty),
      gsub("'","''",r$service_fee), gsub("'","''",r$features), gsub("'","''",r$weight),
      gsub("'","''",r$source_file)))
    n <- n + 1
  }
  n
}

# 从价格表目录导入产品（先清除已导入的，再重新导入，保证幂等）
product_import_from_price_tables <- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(list(success = FALSE, message = "未安装 readxl 包，请先运行 install.packages('readxl')"))
  }
  con <- db_connect()
  tryCatch({
    # 清除之前从价格表导入的产品（source_file 非空）
    dbExecute(con, "DELETE FROM products WHERE source_file IS NOT NULL AND source_file != ''")

    total <- 0
    eb_file <- file.path(PRODUCT_PRICE_DIR, PRODUCT_PRICE_FILES[1])
    dc_file <- file.path(PRODUCT_PRICE_DIR, PRODUCT_PRICE_FILES[2])

    if (file.exists(eb_file)) {
      df <- readxl::read_excel(eb_file, sheet = "二轮车", col_names = FALSE)
      rows <- product_parse_price_table(df, basename(eb_file), "ebike")
      total <- total + product_insert_rows(con, rows)
    }
    if (file.exists(dc_file)) {
      df <- readxl::read_excel(dc_file, sheet = "Sheet2", col_names = FALSE)
      rows <- product_parse_price_table(df, basename(dc_file), "dc")
      total <- total + product_insert_rows(con, rows)
    }

    list(success = TRUE, message = paste("已从价格表导入", total, "条产品"))
  }, error = function(e) list(success = FALSE, message = paste("导入失败:", e$message)),
  finally = { db_disconnect(con) })
}
