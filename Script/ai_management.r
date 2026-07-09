##################
# AI 模块数据层 — 搜索和 AI 工具配置管理
# 配置存于 system_config 表，JSON 格式
##################

# 默认搜索引擎（目前只有 Bing 能用 iframe）
AI_DEFAULT_SEARCH <- list(
  list(id = "bing", name = "Bing", icon = "search", url = "https://www.bing.com/search?q={query}", height = 400, enabled = TRUE)
)

# 默认 AI 对话工具（主流服务均因 X-Frame-Options 禁止 iframe，默认空）
AI_DEFAULT_CHAT <- list()

# 读取搜索引擎配置
ai_get_search_engines <- function() {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT config_value FROM system_config WHERE config_key='ai_search_engines'")
    if (nrow(r) == 0 || is.na(r$config_value[1]) || nchar(r$config_value[1]) == 0) return(AI_DEFAULT_SEARCH)
    cfg <- tryCatch(jsonlite::fromJSON(r$config_value[1], simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(cfg) || length(cfg) == 0) return(AI_DEFAULT_SEARCH)
    cfg
  }, error = function(e) AI_DEFAULT_SEARCH,
  finally = { db_disconnect(con) })
}

# 读取 AI 对话工具配置
ai_get_chat_tools <- function() {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT config_value FROM system_config WHERE config_key='ai_chat_tools'")
    if (nrow(r) == 0 || is.na(r$config_value[1]) || nchar(r$config_value[1]) == 0) return(AI_DEFAULT_CHAT)
    cfg <- tryCatch(jsonlite::fromJSON(r$config_value[1], simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(cfg)) return(AI_DEFAULT_CHAT)
    cfg
  }, error = function(e) AI_DEFAULT_CHAT,
  finally = { db_disconnect(con) })
}

# 保存搜索引擎配置
ai_save_search_engines <- function(cfg_list) {
  con <- db_connect()
  tryCatch({
    json <- jsonlite::toJSON(cfg_list, auto_unbox = TRUE)
    r <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='ai_search_engines'")
    if (nrow(r) > 0) {
      dbExecute(con, sprintf("UPDATE system_config SET config_value='%s' WHERE config_key='ai_search_engines'",
        gsub("'","''", as.character(json))))
    } else {
      dbExecute(con, sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('ai_search_engines','%s','AI全网搜索引擎配置')",
        gsub("'","''", as.character(json))))
    }
    list(success = TRUE, message = "搜索引擎配置已保存")
  }, error = function(e) list(success = FALSE, message = paste("保存失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 保存 AI 对话工具配置
ai_save_chat_tools <- function(cfg_list) {
  con <- db_connect()
  tryCatch({
    json <- jsonlite::toJSON(cfg_list, auto_unbox = TRUE)
    r <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='ai_chat_tools'")
    if (nrow(r) > 0) {
      dbExecute(con, sprintf("UPDATE system_config SET config_value='%s' WHERE config_key='ai_chat_tools'",
        gsub("'","''", as.character(json))))
    } else {
      dbExecute(con, sprintf("INSERT INTO system_config (config_key, config_value, description) VALUES ('ai_chat_tools','%s','AI全网对话工具配置')",
        gsub("'","''", as.character(json))))
    }
    list(success = TRUE, message = "AI工具配置已保存")
  }, error = function(e) list(success = FALSE, message = paste("保存失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 初始化种子数据（如果不存在则写入默认配置）
ai_init_seed <- function() {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='ai_search_engines'")
    if (nrow(r) == 0) {
      ai_save_search_engines(AI_DEFAULT_SEARCH)
    }
    r2 <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='ai_chat_tools'")
    if (nrow(r2) == 0) {
      ai_save_chat_tools(AI_DEFAULT_CHAT)
    }
  }, error = function(e) NULL,
  finally = { db_disconnect(con) })
}
