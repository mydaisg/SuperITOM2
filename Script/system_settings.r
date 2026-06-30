config_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM system_config ORDER BY config_key"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

config_get <- function(config_key) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("SELECT * FROM system_config WHERE config_key = '%s'", config_key)
    result <- dbGetQuery(con, query)
    if (nrow(result) > 0) {
      return(result$config_value[1])
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  }, finally = {
    db_disconnect(con)
  })
}

# 获取配置值（带默认值）
config_get_value <- function(config_key, default = "") {
  val <- config_get(config_key)
  if (is.null(val) || val == "") default else val
}

config_add <- function(config_key, config_value, description = "") {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('%s', '%s', '%s')", 
                     config_key, config_value, description)
    dbExecute(con, query)
    return(list(success = TRUE, message = "配置添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

config_update <- function(id, config_key, config_value, description = "") {
  con <- db_connect()
  tryCatch({
    query <- sprintf("UPDATE system_config SET config_key = '%s', config_value = '%s', description = '%s', updated_at = CURRENT_TIMESTAMP WHERE id = %d", 
                     config_key, config_value, description, id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "配置更新成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("更新失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

config_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("DELETE FROM system_config WHERE id = %d", id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "配置删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

##################
# 全局彩虹色配置
##################

# 默认 20 色（无红色系，Xmind 风格，有顺序）
.RAINBOW_DEFAULT <- c(
  "#2196F3","#4CAF50","#FF9800","#9C27B0","#00BCD4",
  "#FFC107","#3F51B5","#009688","#795548","#607D8B",
  "#8BC34A","#673AB7","#03A9F4","#CDDC39","#1E88E5",
  "#43A047","#00ACC1","#FFB300","#5C6BC0","#26A69A"
)

# 读取彩虹色（返回字符向量，有顺序）
config_get_rainbow <- function() {
  val <- config_get("rainbow_colors")
  if (is.null(val) || val == "") return(.RAINBOW_DEFAULT)
  colors <- tryCatch(jsonlite::fromJSON(val), error = function(e) NULL)
  if (is.null(colors) || length(colors) == 0) return(.RAINBOW_DEFAULT)
  # 确保都是合法 HEX 色号
  colors <- colors[grepl("^#[0-9A-Fa-f]{6}$", colors)]
  if (length(colors) == 0) return(.RAINBOW_DEFAULT)
  colors
}

# 保存彩虹色
config_set_rainbow <- function(colors) {
  colors <- colors[grepl("^#[0-9A-Fa-f]{6}$", colors)]
  if (length(colors) == 0) return(list(success = FALSE, message = "无有效颜色"))
  # 去重保留顺序
  colors <- unique(colors)
  json_val <- jsonlite::toJSON(colors, auto_unbox = TRUE)
  
  existing <- config_get("rainbow_colors")
  if (is.null(existing)) {
    config_add("rainbow_colors", as.character(json_val), "全局彩虹色配置(JSON数组)")
  } else {
    con <- db_connect()
    tryCatch({
      dbExecute(con, sprintf("UPDATE system_config SET config_value='%s', updated_at=datetime('now','localtime') WHERE config_key='rainbow_colors'",
        gsub("'","''",as.character(json_val))))
      list(success = TRUE, message = "彩虹色已更新")
    }, error = function(e) list(success = FALSE, message = e$message),
    finally = { db_disconnect(con) })
  }
}

# 按顺序取第 N 个颜色（循环）
config_rainbow_nth <- function(n) {
  colors <- config_get_rainbow()
  colors[((n - 1) %% length(colors)) + 1]
}

# 初始化种子数据（idempotent）
config_init_rainbow <- function() {
  if (is.null(config_get("rainbow_colors"))) {
    config_set_rainbow(.RAINBOW_DEFAULT)
  }
}
