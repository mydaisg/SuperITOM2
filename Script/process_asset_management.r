# 流程资产中心 - 数据层
# 数据来源：泛微 OA 流程中心（流程梳理 + 集成调研 V2 + 流程监控）
# 沉淀 19 大类 / 136 个流程的：节点构成、流转分支、使用率、集成判定、问题自查

# SQL 字符串安全包装（NULL / 转义单引号）
flow_asset_squote <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || as.character(x[1]) == "") "NULL"
  else paste0("'", gsub("'", "''", as.character(x[1])), "'")
}

# 种子数据文件路径（DB/process_assets_seed.json）
flow_asset_seed_path <- function() {
  file.path(dirname(get_db_path()), "process_assets_seed.json")
}

# 分类列表（含流程数/在途量汇总）
flow_asset_categories <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT category, COUNT(*) AS flow_cnt,
      SUM(usage_count) AS usage_total,
      SUM(CASE WHEN integration_decision='要集成' THEN 1 ELSE 0 END) AS integrate_cnt,
      SUM(CASE WHEN integration_decision='待定' THEN 1 ELSE 0 END) AS pending_cnt
      FROM flow_process_assets GROUP BY category ORDER BY category")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 流程资产列表（分类/关键字/集成判定过滤）
flow_asset_list <- function(category = NULL, keyword = NULL, decision = NULL) {
  con <- db_connect()
  tryCatch({
    where <- character(0)
    if (!is.null(category) && category != "" && category != "全部") {
      where <- c(where, sprintf("category = %s", flow_asset_squote(category)))
    }
    if (!is.null(keyword) && keyword != "") {
      like <- paste0("'%", gsub("'", "''", keyword), "%'")
      where <- c(where, sprintf("(flow_name LIKE %s OR owner_dept LIKE %s OR target_system LIKE %s)", like, like, like))
    }
    if (!is.null(decision) && decision != "" && decision != "全部") {
      where <- c(where, sprintf("integration_decision = %s", flow_asset_squote(decision)))
    }
    sql <- "SELECT * FROM flow_process_assets"
    if (length(where) > 0) sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
    sql <- paste0(sql, " ORDER BY category, usage_count DESC")
    dbGetQuery(con, sql)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 单条资产（含解析后的 nodes/branches/issues）
flow_asset_get <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM flow_process_assets WHERE id=%d", as.integer(id)))
    if (nrow(r) == 0) return(NULL)
    flow_asset_attach_detail(r)
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

flow_asset_get_by_name <- function(name) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM flow_process_assets WHERE flow_name=%s", flow_asset_squote(name)))
    if (nrow(r) == 0) NULL else flow_asset_attach_detail(r)
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# 解析 JSON 列（nodes/branches/issues）附加到 data.frame
flow_asset_attach_detail <- function(df) {
  json_col <- function(x) {
    if (length(x) == 0 || is.na(x[1]) || x[1] == "" || x[1] == "null") list() else {
      tryCatch(jsonlite::fromJSON(x[1], simplifyDataFrame = FALSE, simplifyVector = FALSE),
               error = function(e) list())
    }
  }
  n <- nrow(df)
  df$nodes    <- vector("list", n)
  df$branches <- vector("list", n)
  df$issues   <- vector("list", n)
  for (i in seq_len(n)) {
    df$nodes[[i]]    <- json_col(df$nodes_json[i])
    df$branches[[i]] <- json_col(df$branches_json[i])
    df$issues[[i]]   <- json_col(df$issues_json[i])
  }
  df
}

# 统计卡
flow_asset_stats <- function() {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT
      COUNT(*) AS total,
      SUM(CASE WHEN integration_decision='要集成' THEN 1 ELSE 0 END) AS integrate,
      SUM(CASE WHEN integration_decision='待定' THEN 1 ELSE 0 END) AS pending,
      SUM(usage_count) AS usage_total,
      SUM(node_count) AS node_total,
      SUM(CASE WHEN issues_json IS NOT NULL AND issues_json != '' AND issues_json != '[]' AND issues_json != 'null' THEN 1 ELSE 0 END) AS issue_flows
      FROM flow_process_assets")
    if (nrow(r) == 0) return(data.frame(total=0, integrate=0, pending=0, usage_total=0, node_total=0, issue_flows=0))
    r[is.na(r)] <- 0
    r
  }, error = function(e) data.frame(total=0, integrate=0, pending=0, usage_total=0, node_total=0, issue_flows=0),
  finally = { db_disconnect(con) })
}

# 导入/刷新种子数据（幂等：按 flow_name INSERT OR REPLACE）
flow_asset_seed <- function(force = FALSE) {
  sp <- flow_asset_seed_path()
  if (!file.exists(sp)) return(list(success = FALSE, message = "种子文件不存在：DB/process_assets_seed.json"))
  assets <- tryCatch(jsonlite::fromJSON(sp, simplifyDataFrame = FALSE, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(assets) || length(assets) == 0) return(list(success = FALSE, message = "种子数据解析失败"))
  con <- db_connect()
  n <- 0
  tryCatch({
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    for (a in assets) {
      nodes_json    <- if (length(a$nodes) > 0)    jsonlite::toJSON(a$nodes, auto_unbox = TRUE)    else "[]"
      branches_json <- if (length(a$branches) > 0) jsonlite::toJSON(a$branches, auto_unbox = TRUE) else "[]"
      issues_json   <- if (length(a$issues) > 0)   jsonlite::toJSON(a$issues, auto_unbox = TRUE)   else "[]"
      dbExecute(con, sprintf(
        "INSERT OR REPLACE INTO flow_process_assets
         (category, flow_name, owner_dept, usage_count, usage_rate, initiator_count, top_initiators,
          node_count, integration_decision, priority, target_system, remark,
          nodes_json, branches_json, issues_json, source, created_at, updated_at)
         VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,'泛微OA',%s,%s)",
        flow_asset_squote(a$category),
        flow_asset_squote(a$flow_name),
        flow_asset_squote(a$owner_dept),
        if (is.null(a$usage_count) || is.na(a$usage_count)) "0" else as.character(as.integer(a$usage_count)),
        if (is.null(a$usage_rate) || is.na(a$usage_rate)) "0" else as.character(as.numeric(a$usage_rate)),
        if (is.null(a$initiator_count) || is.na(a$initiator_count)) "0" else as.character(as.integer(a$initiator_count)),
        flow_asset_squote(a$top_initiators),
        if (is.null(a$node_count) || is.na(a$node_count)) "0" else as.character(as.integer(a$node_count)),
        flow_asset_squote(a$integration_decision),
        flow_asset_squote(a$priority),
        flow_asset_squote(a$target_system),
        flow_asset_squote(a$remark),
        flow_asset_squote(nodes_json),
        flow_asset_squote(branches_json),
        flow_asset_squote(issues_json),
        flow_asset_squote(now),
        flow_asset_squote(now)))
      n <- n + 1
    }
    list(success = TRUE, imported = n, message = sprintf("已导入/刷新 %d 个流程资产", n))
  }, error = function(e) list(success = FALSE, message = paste("导入失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 删除单条资产
flow_asset_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM flow_process_assets WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message), finally = { db_disconnect(con) })
}
