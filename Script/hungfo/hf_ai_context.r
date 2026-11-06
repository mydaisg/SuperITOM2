##################
# HungFo — AI 上下文层（数据层 + 规则/记忆装配 + 导出）
#
# 文件名刻意不使用 hf_mod_ 前缀：本文件只提供函数，不注册导航模块，
# 规则/记忆的界面以「子 Tab」形式挂在规则模块内（符合项目约定）。
# 加载位置：app.R / server.R 在 hf_core.r 之后 source 本文件。
#
# 职责：
#   1. 通用可配置项 hf_config_kv（key → JSON），承载模型路由表与 CLI 默认参数
#   2. 规则分类元数据：适用任务类型 / 默认模型 / effort / 轮数 / 是否常驻注入
#   3. 规则修订条目 hf_rule_items：append-only，parent_id 保留层级（照记事评论模式）
#   4. 项目与 AI 记忆 hf_memory：append-only，可导出 MEMORY.md 便于移植
#   5. 按任务类型自动装配「相关规则 + 相关记忆」→ system prompt 文件
#   6. 导出 .codebuddy/rules/hf-gen-*.md，作为 CLI 静态兜底与移植包
#
# 约束：所有建表/加列在 hf_core.r::hf_db_init() 中幂等完成，本文件只做读写。
##################

# ══════════════════════════════════════════════════════════════════
# 一、通用可配置项
# ══════════════════════════════════════════════════════════════════

hf_cfg_get <- function(key, default = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT cfg_value FROM hf_config_kv WHERE cfg_key='%s'", hf_esc(key)))
    if (nrow(r) == 0 || is.na(r$cfg_value[1]) || !nzchar(r$cfg_value[1])) return(default)
    v <- jsonlite::fromJSON(r$cfg_value[1], simplifyVector = FALSE)
    if (is.null(v)) default else v
  }, error = function(e) default)
}

hf_cfg_set <- function(key, value, desc = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    txt <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"))
    ex <- dbGetQuery(con, sprintf("SELECT cfg_key FROM hf_config_kv WHERE cfg_key='%s'", hf_esc(key)))
    if (nrow(ex) > 0) {
      if (is.null(desc)) {
        dbExecute(con, sprintf(
          "UPDATE hf_config_kv SET cfg_value='%s', updated_at=datetime('now','localtime') WHERE cfg_key='%s'",
          hf_esc(txt), hf_esc(key)))
      } else {
        dbExecute(con, sprintf(
          "UPDATE hf_config_kv SET cfg_value='%s', cfg_desc='%s', updated_at=datetime('now','localtime') WHERE cfg_key='%s'",
          hf_esc(txt), hf_esc(desc), hf_esc(key)))
      }
    } else {
      dbExecute(con, sprintf(
        "INSERT INTO hf_config_kv (cfg_key,cfg_value,cfg_desc) VALUES ('%s','%s','%s')",
        hf_esc(key), hf_esc(txt), hf_esc(hf_or(desc, ""))))
    }
    list(success = TRUE, message = paste0("已保存配置：", key))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_cfg_list <- function() {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbGetQuery(con, "SELECT * FROM hf_config_kv ORDER BY cfg_key"),
           error = function(e) data.frame())
}

# ══════════════════════════════════════════════════════════════════
# 二、任务类型 → 模型 / 分类 路由表
# ══════════════════════════════════════════════════════════════════

HF_TASK_TYPES <- c(
  "default"    = "通用任务",
  "new_module" = "新增模块",
  "bugfix"     = "修复功能",
  "ui"         = "优化UI",
  "doc"        = "文档/整理",
  "chat"       = "只读问答",
  "review"     = "代码评审"
)

# 默认路由：简单任务走便宜模型 / 低 effort，复杂任务才用默认（贵）模型。
# model="" 表示不传 --model（用 CLI 账号默认模型）；fallback_model 在主模型过载时降级。
HF_DEFAULT_ROUTES <- list(
  list(task_type = "default",    label = "通用任务", categories = "",              model = "",              effort = "medium",  max_turns = 30, fallback_model = ""),
  list(task_type = "new_module", label = "新增模块", categories = "架构规则,UI规则", model = "",              effort = "high",    max_turns = 50, fallback_model = "fast-model"),
  list(task_type = "bugfix",     label = "修复功能", categories = "架构规则",        model = "",              effort = "medium",  max_turns = 30, fallback_model = "fast-model"),
  list(task_type = "ui",         label = "优化UI",   categories = "UI规则",          model = "fast-model",    effort = "low",     max_turns = 20, fallback_model = ""),
  list(task_type = "doc",        label = "文档/整理", categories = "",              model = "fast-model",    effort = "minimal", max_turns = 12, fallback_model = ""),
  list(task_type = "chat",       label = "只读问答", categories = "",              model = "fast-model",    effort = "minimal", max_turns = 5,  fallback_model = ""),
  list(task_type = "review",     label = "代码评审", categories = "架构规则",        model = "fast-model",    effort = "low",     max_turns = 15, fallback_model = "")
)

hf_model_routes <- function() {
  v <- hf_cfg_get("ai_model_routes", NULL)
  if (!is.list(v) || length(v) == 0) return(HF_DEFAULT_ROUTES)
  # 只有 1 条路由时 toJSON 可能退化成对象，统一包回 list
  if (!is.null(names(v)) && "task_type" %in% names(v)) v <- list(v)
  v
}

hf_model_routes_save <- function(routes) {
  hf_cfg_set("ai_model_routes", routes, desc = "任务类型 → 分类/模型/effort/轮数 路由表")
}

hf_model_route_get <- function(task_type = NULL) {
  rs <- hf_model_routes()
  key <- hf_or(task_type, "default")
  if (!nzchar(key)) key <- "default"
  for (r in rs) if (identical(as.character(hf_or(r$task_type, "")), key)) return(r)
  for (r in rs) if (identical(as.character(hf_or(r$task_type, "")), "default")) return(r)
  HF_DEFAULT_ROUTES[[1]]
}

hf_task_type_choices <- function() {
  nms <- names(HF_TASK_TYPES)
  setNames(nms, as.character(HF_TASK_TYPES))
}

# CLI 默认参数（cwd / 超时 / 并发上限），集中可配置
hf_cb_defaults <- function() {
  d <- hf_cfg_get("cb_defaults", NULL)
  if (!is.list(d)) d <- list()
  list(
    cwd            = hf_or(d$cwd, ""),
    timeout_sec    = as.integer(hf_or(d$timeout_sec, 1800)),
    max_concurrent = as.integer(hf_or(d$max_concurrent, 3)),
    inject_context = isTRUE(hf_or(d$inject_context, TRUE)),
    include_memory = isTRUE(hf_or(d$include_memory, TRUE)),
    # auto=每次挑空闲端口（避开 WorkBuddy/CB 共用守护端口 62541 的 EADDRINUSE）；
    # off=不加 --port，适用于确认端口不冲突的旧环境
    port_mode      = hf_or(d$port_mode, "auto")
  )
}

# ══════════════════════════════════════════════════════════════════
# 三、规则分类元数据
# ══════════════════════════════════════════════════════════════════

hf_rules_category_set_meta <- function(id, applies_to = NULL, model = NULL, effort = NULL,
                                       max_turns = NULL, enabled = NULL, always_apply = NULL,
                                       description = NULL) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  if (is.null(id) || is.na(id)) return(list(success = FALSE, message = "分类 id 无效"))
  sets <- character(0)
  put <- function(col, val, quote = TRUE) {
    if (is.null(val)) return(invisible(NULL))
    v <- if (quote) sprintf("'%s'", hf_esc(val)) else as.character(val)
    sets <<- c(sets, sprintf("%s=%s", col, v))
  }
  put("applies_to", applies_to); put("model", model); put("effort", effort)
  put("max_turns", if (is.null(max_turns)) NULL else as.integer(max_turns), quote = FALSE)
  put("enabled", if (is.null(enabled)) NULL else as.integer(enabled), quote = FALSE)
  put("always_apply", if (is.null(always_apply)) NULL else as.integer(always_apply), quote = FALSE)
  put("description", description)
  if (length(sets) == 0) return(list(success = TRUE, message = "无字段需要更新"))
  tryCatch({
    dbExecute(con, sprintf(
      "UPDATE hf_rules_categories SET %s, updated_at=datetime('now','localtime') WHERE id=%s",
      paste(sets, collapse = ","), as.character(as.integer(id))))
    list(success = TRUE, message = "已更新分类配置")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# ══════════════════════════════════════════════════════════════════
# 四、规则修订（append-only，层级：分类 → 规则 → 修订条目）
# ══════════════════════════════════════════════════════════════════

hf_rule_items_list <- function(rule_id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbGetQuery(con, sprintf(
    "SELECT * FROM hf_rule_items WHERE rule_id=%s ORDER BY id ASC", as.character(as.integer(rule_id)))),
    error = function(e) data.frame())
}

hf_rule_item_count <- function(rule_id) {
  d <- hf_rule_items_list(rule_id)
  if (is.null(d) || nrow(d) == 0) 0L else nrow(d)
}

hf_rule_item_add <- function(rule_id, content, parent_id = 0, created_by = NULL,
                             created_by_name = "") {
  if (is.null(rule_id) || is.na(rule_id)) return(list(success = FALSE, message = "规则 id 无效"))
  content <- trimws(hf_or(content, ""))
  if (!nzchar(content)) return(list(success = FALSE, message = "修订内容不能为空"))
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    n <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM hf_rule_items WHERE rule_id=%s",
                                 as.character(as.integer(rule_id))))$n[1]
    dbExecute(con, sprintf(
      "INSERT INTO hf_rule_items (rule_id,parent_id,content,version_no,status,created_by,created_by_name)
       VALUES (%s,%s,'%s',%d,'effective',%s,'%s')",
      as.character(as.integer(rule_id)), as.character(as.integer(hf_or(parent_id, 0))),
      hf_esc(content), as.integer(n) + 1L,
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by)),
      hf_esc(created_by_name)))
    list(success = TRUE, version_no = as.integer(n) + 1L,
         message = sprintf("已追加第 %d 版修订", as.integer(n) + 1L))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 当前生效内容：优先取最新 effective 修订，没有修订则回退 hf_rules.payload
hf_rule_current <- function(rule_id, fallback_payload = "") {
  d <- hf_rule_items_list(rule_id)
  if (!is.null(d) && nrow(d) > 0) {
    eff <- d[!is.na(d$status) & d$status == "effective", , drop = FALSE]
    if (nrow(eff) == 0) eff <- d
    return(list(content = as.character(eff$content[nrow(eff)]),
                version_no = as.integer(eff$version_no[nrow(eff)]),
                revisions = nrow(d)))
  }
  list(content = as.character(hf_or(fallback_payload, "")), version_no = 0L, revisions = 0L)
}

# 层级树：分类（一级）→ 规则（二级）→ 修订（三级）
hf_rule_tree <- function(categories = NULL) {
  cats <- hf_rules_categories_df()
  rules <- hf_rules_list()
  out <- list()
  if (is.null(cats) || nrow(cats) == 0) return(out)
  for (i in seq_len(nrow(cats))) {
    cn <- as.character(cats$name[i])
    if (!is.null(categories) && length(categories) > 0 && !(cn %in% categories)) next
    rs <- if (!is.null(rules) && nrow(rules) > 0) rules[rules$category == cn, , drop = FALSE] else data.frame()
    rlist <- list()
    if (!is.null(rs) && nrow(rs) > 0) {
      for (j in seq_len(nrow(rs))) {
        cur <- hf_rule_current(rs$id[j], rs$payload[j])
        rlist[[length(rlist) + 1L]] <- list(
          id = rs$id[j], rule_no = hf_or(rs$rule_no[j], ""), title = hf_or(rs$title[j], ""),
          status = hf_or(rs$status[j], "draft"), created_at = hf_or(rs$created_at[j], ""),
          current = cur$content, version_no = cur$version_no, revisions = cur$revisions,
          items = hf_rule_items_list(rs$id[j]))
      }
    }
    out[[length(out) + 1L]] <- list(
      id = cats$id[i], name = cn,
      applies_to = if (is.null(cats$applies_to)) "" else hf_or(cats$applies_to[i], ""),
      enabled = if (is.null(cats$enabled)) 1 else hf_or(cats$enabled[i], 1),
      always_apply = if (is.null(cats$always_apply)) 0 else hf_or(cats$always_apply[i], 0),
      rule_count = length(rlist), rules = rlist)
  }
  out
}

# ══════════════════════════════════════════════════════════════════
# 五、项目与 AI 记忆
# ══════════════════════════════════════════════════════════════════

HF_MEMORY_CATEGORIES <- c("项目事实", "决策与约定", "AI 协作经验", "跨会话待办")

hf_memory_list <- function(category = NULL, topic_kw = "", limit = 200) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    sql <- "SELECT * FROM hf_memory WHERE status<>'deprecated'"
    if (!is.null(category) && nzchar(trimws(category)))
      sql <- sprintf("%s AND category='%s'", sql, hf_esc(category))
    d <- dbGetQuery(con, sprintf("%s ORDER BY id DESC LIMIT %d", sql, as.integer(limit)))
    if (!is.null(topic_kw) && nzchar(trimws(topic_kw)) && nrow(d) > 0) {
      kw <- trimws(topic_kw)
      hit <- grepl(kw, d$topic, ignore.case = TRUE) |
             grepl(kw, hf_or(d$content, ""), ignore.case = TRUE) |
             grepl(kw, hf_or(d$tags, ""), ignore.case = TRUE)
      d <- d[hit, , drop = FALSE]
    }
    d
  }, error = function(e) data.frame())
}

hf_memory_get <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(dbGetQuery(con, sprintf("SELECT * FROM hf_memory WHERE id=%s", as.character(as.integer(id)))),
           error = function(e) data.frame())
}

hf_memory_add <- function(topic, category = "项目事实", content = "", source = "", tags = "",
                          created_by = NULL, created_by_name = "") {
  topic <- trimws(hf_or(topic, ""))
  if (!nzchar(topic)) return(list(success = FALSE, message = "记忆主题不能为空"))
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO hf_memory (topic,category,content,source,tags,parent_id,status,created_by,created_by_name)
       VALUES ('%s','%s','%s','%s','%s',0,'effective',%s,'%s')",
      hf_esc(topic), hf_esc(hf_or(category, "项目事实")), hf_esc(hf_or(content, "")),
      hf_esc(hf_or(source, "")), hf_esc(hf_or(tags, "")),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by)),
      hf_esc(created_by_name)))
    list(success = TRUE, message = paste0("已新增记忆：", topic))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 追加修订：INSERT 新行并把 parent_id 指向根记忆，形成可回溯的修订链（不覆盖旧内容）
hf_memory_revise <- function(id, content, created_by = NULL, created_by_name = "") {
  r <- hf_memory_get(id)
  if (is.null(r) || nrow(r) == 0) return(list(success = FALSE, message = "记忆不存在"))
  content <- trimws(hf_or(content, ""))
  if (!nzchar(content)) return(list(success = FALSE, message = "修订内容不能为空"))
  root <- if (is.na(r$parent_id[1]) || r$parent_id[1] == 0) r$id[1] else r$parent_id[1]
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    n <- dbGetQuery(con, sprintf(
      "SELECT COUNT(*) n FROM hf_memory WHERE parent_id=%s OR id=%s", as.character(root), as.character(root)))$n[1]
    dbExecute(con, sprintf(
      "INSERT INTO hf_memory (topic,category,content,source,tags,parent_id,status,created_by,created_by_name)
       VALUES ('%s','%s','%s','%s','%s',%s,'effective',%s,'%s')",
      hf_esc(r$topic[1]), hf_esc(r$category[1]), hf_esc(content),
      hf_esc(hf_or(r$source[1], "")), hf_esc(hf_or(r$tags[1], "")), as.character(root),
      if (is.null(created_by)) "NULL" else as.character(as.integer(created_by)),
      hf_esc(created_by_name)))
    # 旧版本标记为历史（仍可查，不删除）
    dbExecute(con, sprintf("UPDATE hf_memory SET status='history' WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, version_no = as.integer(n) + 1L,
         message = sprintf("已追加第 %d 版记忆修订", as.integer(n) + 1L))
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_memory_deprecate <- function(id) {
  con <- hf_db_connect()
  on.exit(try(hf_db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("UPDATE hf_memory SET status='deprecated' WHERE id=%s", as.character(as.integer(id))))
    list(success = TRUE, message = "已停用该记忆（数据保留，可回溯）")
  }, error = function(e) list(success = FALSE, message = e$message))
}

hf_memory_stats <- function() {
  d <- hf_memory_list(limit = 10000)
  list(total = if (is.null(d)) 0 else nrow(d),
       categories = if (is.null(d) || nrow(d) == 0) character(0) else sort(unique(d$category)))
}

# ══════════════════════════════════════════════════════════════════
# 六、按任务类型自动装配规则 + 记忆
# ══════════════════════════════════════════════════════════════════

# 选出本次要读的分类：显式 category 优先；否则路由表 categories + always_apply 常驻分类 + 任务类型标签匹配
hf_ai_categories_for <- function(task_type = NULL, category = NULL) {
  cats <- hf_rules_categories_df()
  if (is.null(cats) || nrow(cats) == 0) return(character(0))
  if (!is.null(category) && nzchar(trimws(category))) return(trimws(category))
  tt <- hf_or(task_type, "")
  route <- hf_model_route_get(tt)
  rc <- trimws(hf_or(route$categories, ""))
  wanted <- if (nzchar(rc)) trimws(strsplit(rc, "[,，;；]")[[1]]) else character(0)
  for (i in seq_len(nrow(cats))) {
    ap <- hf_or(cats$always_apply[i], 0)
    if (!is.na(ap) && as.integer(ap) == 1) wanted <- c(wanted, as.character(cats$name[i]))
    applies <- hf_or(cats$applies_to[i], "")
    if (nzchar(tt) && nzchar(applies) && grepl(tt, applies, fixed = TRUE))
      wanted <- c(wanted, as.character(cats$name[i]))
  }
  unique(wanted[nzchar(wanted)])
}

hf_ai_context_text <- function(task_type = NULL, category = NULL, include_memory = TRUE,
                               max_rules = 80, max_memory = 20) {
  tt <- hf_or(task_type, "default")
  cats <- hf_ai_categories_for(tt, category)
  tree <- if (length(cats) > 0) hf_rule_tree(cats) else list()
  lines <- c(
    "# HungFo AI 执行上下文（自动生成，勿手改）",
    sprintf("生成时间：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("任务类型：%s", hf_or(HF_TASK_TYPES[[tt]], tt)),
    sprintf("匹配分类：%s", if (length(cats) > 0) paste(cats, collapse = "、") else "（无，仅常驻纪律）"),
    "",
    "## 一、必须遵守的规则（按分类自动装配，其余分类不读）",
    "")
  nr <- 0L
  if (length(tree) > 0) for (ct in tree) {
    if (ct$rule_count == 0) next
    lines <- c(lines, sprintf("### 分类：%s", ct$name), "")
    for (r in ct$rules) {
      if (nr >= max_rules) break
      nr <- nr + 1L
      ver <- if (r$version_no > 0) sprintf("v%d · %d 条修订", r$version_no, r$revisions) else "原始版本"
      lines <- c(lines, sprintf("- **%s %s**（%s，当前生效）", r$rule_no, r$title, ver))
      body <- gsub("\r", "", hf_or(r$current, ""))
      body <- paste(strsplit(body, "\n")[[1]], collapse = "\n  ")
      lines <- c(lines, paste0("  ", body), "")
    }
  }
  if (nr == 0) lines <- c(lines, "（本次任务无匹配规则条目）", "")

  lines <- c(lines, "## 二、项目与 AI 记忆（跨会话长期保留）", "")
  nm <- 0L
  if (isTRUE(include_memory)) {
    mem <- hf_memory_list(limit = 500)
    if (!is.null(mem) && nrow(mem) > 0) {
      # 与任务类型相关的记忆优先（tags 命中），其余按时间倒序补齐
      if (nzchar(tt)) {
        rel <- grepl(tt, hf_or(mem$tags, ""), ignore.case = TRUE)
        mem <- rbind(mem[rel, , drop = FALSE], mem[!rel, , drop = FALSE])
      }
      keep <- head(mem, max_memory)
      for (i in seq_len(nrow(keep))) {
        nm <- nm + 1L
        lines <- c(lines, sprintf("- **[%s] %s**（%s）",
          hf_or(keep$category[i], "项目事实"), hf_or(keep$topic[i], ""),
          hf_or(keep$created_at[i], "")))
        lines <- c(lines, paste0("  ", gsub("\n", "\n  ", hf_or(keep$content[i], ""))), "")
      }
    }
  }
  if (nm == 0) lines <- c(lines, "（暂无记忆条目）", "")

  lines <- c(lines,
    "## 三、执行纪律",
    "",
    "- 只改与本次任务直接相关的文件；不删除、不重写无关模块。",
    "- 新增/修改后必须做语法与回归验证，再写五段式开发日志。",
    "- 五段式：【需求】【分析】【方案】【结果】【记忆】；结果段需含 Token 等消耗数据。",
    "- 规则与记忆有冲突时，以「当前生效」的最新修订为准。",
    "")
  list(text = paste(lines, collapse = "\n"), categories = cats,
       rules_n = nr, memory_n = nm, task_type = tt)
}

hf_ai_context_dir <- function() {
  d <- file.path(hf_home(), "ai_context")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

hf_ai_context_write <- function(text, run_id) {
  p <- file.path(hf_ai_context_dir(), paste0("ctx_", run_id, ".md"))
  tryCatch({
    writeLines(text, p, useBytes = TRUE)
    p
  }, error = function(e) NULL)
}

# ══════════════════════════════════════════════════════════════════
# 七、导出（移植包 / CLI 静态兜底）
# ══════════════════════════════════════════════════════════════════

hf_ai_export_dir <- function() {
  d <- file.path(hf_home(), ".codebuddy", "rules")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# 每个分类导出一个 hf-gen-<序号>.md；alwaysApply=false + description，供 CLI 按需读取；
# 另导出 alwaysApply=true 的索引文件，保证任何会话都能看到「去哪读规则」。
hf_rules_export_md <- function() {
  dir <- hf_ai_export_dir()
  tree <- hf_rule_tree()
  files <- character(0)
  for (old in list.files(dir, pattern = "^hf-gen-.*\\.md$", full.names = TRUE)) {
    try(unlink(old), silent = TRUE)
  }
  if (length(tree) > 0) for (i in seq_along(tree)) {
    ct <- tree[[i]]
    body <- c(
      "---",
      sprintf("description: HungFo 自动生成规则分类「%s」，适用任务类型：%s", ct$name,
              if (nzchar(ct$applies_to)) ct$applies_to else "通用"),
      "alwaysApply: false",
      "enabled: true",
      "---",
      "",
      sprintf("# 规则分类：%s", ct$name),
      "",
      "> 本文件由 HungFo 规则模块导出，请勿手改；修订请在 Web 工作台的规则子 Tab 中追加。",
      "")
    if (ct$rule_count > 0) for (r in ct$rules) {
      body <- c(body, sprintf("## %s %s", r$rule_no, r$title), "",
                hf_or(r$current, ""), "")
    } else body <- c(body, "（该分类下暂无规则）", "")
    f <- file.path(dir, sprintf("hf-gen-%02d.md", i))
    writeLines(paste(body, collapse = "\n"), f, useBytes = TRUE)
    files <- c(files, f)
  }
  idx <- c(
    "---",
    "description: HungFo 规则索引与执行纪律，始终生效",
    "alwaysApply: true",
    "enabled: true",
    "---",
    "",
    "# HungFo AI 规则索引",
    "",
    "- 规则与记忆的**唯一数据源**是 HungFo 的 SQLite 库（`DB/HF_HungFo.DB`）中的 `hf_rules` / `hf_rule_items` / `hf_memory` / `hf_rules_categories`。",
    "- 工作台提交任务时会按任务类型自动装配相关分类，通过 `--system-prompt-file` 精准注入，本目录下的 `hf-gen-*.md` 是静态兜底与移植快照。",
    "- 需要完整规则时，在 Web 工作台「规则 → 导出规则」重新导出；不要手工编辑生成文件。",
    "- 任何功能变更后必须写五段式开发日志：【需求】【分析】【方案】【结果】【记忆】，结果段含 Token 等消耗数据。",
    "")
  fi <- file.path(dir, "hf-gen-index.md")
  writeLines(paste(idx, collapse = "\n"), fi, useBytes = TRUE)
  list(success = TRUE, dir = dir, files = c(fi, files), categories = length(tree))
}

# 记忆导出为便携 Markdown，拷到新项目即完成移植
hf_memory_export_md <- function() {
  f <- file.path(hf_home(), "MEMORY.md")
  mem <- hf_memory_list(limit = 10000)
  body <- c("# HungFo 项目与 AI 记忆", "",
            sprintf("> 导出时间：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            "> 由 HungFo 规则模块「记忆」子 Tab 导出；移植时把本文件与 RULES.md 一起拷入新项目即可。", "")
  if (!is.null(mem) && nrow(mem) > 0) {
    for (cat in HF_MEMORY_CATEGORIES) {
      one <- mem[mem$category == cat, , drop = FALSE]
      if (nrow(one) == 0) next
      body <- c(body, sprintf("## %s", cat), "")
      for (i in seq_len(nrow(one))) {
        body <- c(body, sprintf("### %s（%s）", hf_or(one$topic[i], ""),
                                hf_or(one$created_at[i], "")), "",
                  hf_or(one$content[i], ""), "")
      }
    }
  } else body <- c(body, "（暂无记忆条目）", "")
  writeLines(paste(body, collapse = "\n"), f, useBytes = TRUE)
  list(success = TRUE, file = f, items = if (is.null(mem)) 0 else nrow(mem))
}

hf_ai_export_all <- function() {
  r <- hf_rules_export_md()
  m <- hf_memory_export_md()
  # 同时导出一份规则全文快照，便于移植
  f <- file.path(hf_home(), "RULES.md")
  tree <- hf_rule_tree()
  body <- c("# HungFo 规则全文快照", "", sprintf("> 导出时间：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "")
  if (length(tree) > 0) for (ct in tree) {
    body <- c(body, sprintf("## %s", ct$name), "")
    if (ct$rule_count > 0) for (rl in ct$rules) {
      body <- c(body, sprintf("### %s %s", rl$rule_no, rl$title), "", hf_or(rl$current, ""), "")
    }
  }
  writeLines(paste(body, collapse = "\n"), f, useBytes = TRUE)
  list(success = TRUE, rules = r, memory = m, snapshot = f)
}
