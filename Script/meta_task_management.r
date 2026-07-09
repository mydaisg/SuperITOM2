##################
# 元任务模块 — 数据层
# 记录元任务的逻辑和规则，存于 system_config 表
##################

# 获取元任务规则
meta_task_get_rules <- function() {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT config_value FROM system_config WHERE config_key='meta_task_rules'")
    if (nrow(r) == 0) {
      return(list(
        title = "元任务机制",
        note_no = "NTE20260606002",
        rules = "1. CodeBuddy 查询 NTE20260606002 未完成评论（status != 'completed'）\n2. 分析每条评论判断是优化（实现）还是排障（排查）\n3. 执行任务\n4. 标记完成：UPDATE note_comments SET status='completed' WHERE id=?\n5. 开发日志含中英文描述和 commit_msg",
        dev_log_spec = "1. 每条元任务独立写一条开发日志（不合并多条）\n2. 必须填写：requirement_en, solution_en, result_en, commit_msg\n3. 需求/方案/结果中多条有序号的，要换行显示\n4. 参照 DL20260702007 格式",
        sync_rule = "优化元任务时，同步更新此页面规则。规则版本跟随更新日期自动记录。"
      ))
    }
    rules <- tryCatch(jsonlite::fromJSON(r$config_value[1]), error = function(e) NULL)
    if (is.null(rules)) return(meta_task_get_rules())  # fallback to default
    rules
  }, error = function(e) {
    meta_task_get_rules()  # fallback on error
  }, finally = { db_disconnect(con) })
}

# 保存元任务规则
meta_task_save_rules <- function(title, note_no, rules_text, dev_log_spec, sync_rule) {
  con <- db_connect()
  tryCatch({
    rules <- list(
      title = title %||% "元任务机制",
      note_no = note_no %||% "NTE20260606002",
      rules = rules_text %||% "",
      dev_log_spec = dev_log_spec %||% "",
      sync_rule = sync_rule %||% "",
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    json <- jsonlite::toJSON(rules, auto_unbox = TRUE)
    exists <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='meta_task_rules'")
    if (nrow(exists) > 0) {
      dbExecute(con, sprintf(
        "UPDATE system_config SET config_value='%s' WHERE config_key='meta_task_rules'",
        gsub("'","''", as.character(json))))
    } else {
      dbExecute(con, sprintf(
        "INSERT INTO system_config (config_key, config_value, description) VALUES ('meta_task_rules','%s','元任务逻辑和规则')",
        gsub("'","''", as.character(json))))
    }
    list(success = TRUE, message = "元任务规则已保存")
  }, error = function(e) list(success = FALSE, message = paste("保存失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 初始化种子数据（首次访问时自动写入）
meta_task_init_seed <- function() {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, "SELECT id FROM system_config WHERE config_key='meta_task_rules'")
    if (nrow(r) == 0) {
      meta_task_save_rules(
        title = "元任务机制",
        note_no = "NTE20260606002",
        rules_text = "1. CodeBuddy 查询 NTE20260606002 未完成评论（status != 'completed'）\n2. 分析每条评论判断是优化（实现）还是排障（排查）\n3. 执行任务\n4. 标记完成：UPDATE note_comments SET status='completed' WHERE id=?\n5. 开发日志含中英文描述和 commit_msg",
        dev_log_spec = "1. 每条元任务独立写一条开发日志（不合并多条）\n2. 必须填写：requirement_en, solution_en, result_en, commit_msg\n3. 需求/方案/结果中多条有序号的，要换行显示\n4. 参照 DL20260702007 格式",
        sync_rule = "优化元任务时，同步更新此页面规则。规则版本跟随更新日期自动记录。"
      )
    }
  }, error = function(e) NULL,
  finally = { db_disconnect(con) })
}
