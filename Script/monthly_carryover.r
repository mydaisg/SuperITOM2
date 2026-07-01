##################
# 数据结转模块 — 月度数据结转（记事先行）
##################
source("Script/log_user.r")

# 正则匹配标题中的 (YYYY年M月) 或（YYYY年MM月）模式（兼容半角/全角括号）
.CARRYOVER_PATTERN <- "[（(](\\d{4})年(\\d{1,2})月[）)]"

# 从标题提取年月字符串，如 "2026-06"
carryover_extract_ym <- function(title) {
  m <- regmatches(title, regexec(.CARRYOVER_PATTERN, title))[[1]]
  if (length(m) < 3) return(NULL)
  yr  <- as.integer(m[2])
  mo  <- as.integer(m[3])
  if (is.na(yr) || is.na(mo) || mo < 1 || mo > 12) return(NULL)
  sprintf("%04d-%02d", yr, mo)
}

# 从标题中的年月替换为新的年月（title 中的 (2026年6月) → (2026年7月)）
carryover_replace_ym <- function(title, new_ym) {
  parts <- strsplit(new_ym, "-")[[1]]
  new_yr  <- as.integer(parts[1])
  new_mo  <- as.integer(parts[2])
  # 统一替换为半角括号，月份保持两位
  gsub(.CARRYOVER_PATTERN, sprintf("(%d年%02d月)", new_yr, new_mo), title, perl = TRUE)
}

# 列出所有匹配标题规则的记事（可选按年月筛选）
carryover_list_notes <- function(year_month = NULL) {
  con <- db_connect()
  tryCatch({
    notes <- dbGetQuery(con, "SELECT id, note_no, title, status, importance, 
      reminder_at, due_at, created_at, created_by, reported_to_daily
      FROM notes WHERE title IS NOT NULL AND title != '' ORDER BY created_at DESC")
    # 筛选标题匹配的（统一转为字符向量，避免 sapply 返回列表导致 DT 列名异常）
    notes$ym <- sapply(notes$title, function(t) {
      res <- carryover_extract_ym(t)
      if (is.null(res)) NA_character_ else res
    })
    notes <- notes[!is.na(notes$ym), , drop = FALSE]
    if (!is.null(year_month)) {
      notes <- notes[notes$ym == year_month, , drop = FALSE]
    }
    notes
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取上个月且未完成的记事清单（用于结账确认）
carryover_prev_month_pending <- function() {
  today <- Sys.Date()
  prev_month <- as.integer(format(today, "%m")) - 1L
  prev_year  <- as.integer(format(today, "%Y"))
  if (prev_month == 0L) { prev_month <- 12L; prev_year <- prev_year - 1L }
  ym <- sprintf("%04d-%02d", prev_year, prev_month)
  notes <- carryover_list_notes(ym)
  notes[notes$status != "completed", , drop = FALSE]
}

# 执行结账：将选中的记事状态改为 completed（附加评论记录操作）
carryover_close_notes <- function(note_ids, operator) {
  if (length(note_ids) == 0) return(list(success = FALSE, message = "未选择任何记事"))
  con <- db_connect()
  tryCatch({
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    for (nid in note_ids) {
      dbExecute(con, sprintf(
        "UPDATE notes SET status='completed', updated_at='%s' WHERE id=%d",
        now, as.integer(nid)))
      # 追加一条评论记录结转操作
      dbExecute(con, sprintf(
        "INSERT INTO note_comments (note_id, content, created_by, created_at, status, completed_at)
         VALUES (%d, '【数据结转】已由 [%s] 自动结转为已完成', %d, '%s', 'completed', '%s')",
        as.integer(nid), operator$username[1] %||% "系统",
        operator$id[1] %||% 1, now, now))
    }
    log_user_operation("数据结转-记事结账", sprintf("%d 条记事", length(note_ids)), operator$username[1] %||% "系统")
    list(success = TRUE, message = sprintf("已结转 %d 条记事为已完成", length(note_ids)))
  }, error = function(e) list(success = FALSE, message = paste("结转失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 获取可用于生成下月的模板（取上月的记事，按标题去重）
carryover_current_month_templates <- function() {
  today <- Sys.Date()
  prev_month <- as.integer(format(today, "%m")) - 1L
  prev_year  <- as.integer(format(today, "%Y"))
  if (prev_month == 0L) { prev_month <- 12L; prev_year <- prev_year - 1L }
  ym <- sprintf("%04d-%02d", prev_year, prev_month)
  notes <- carryover_list_notes(ym)
  # 取最近每类标题的第一条（去重）
  notes[!duplicated(notes$title), , drop = FALSE]
}

# 计算下月的首日、末日、提醒日（基于 from_ym，如 from_ym='2026-06' 生成 2026-07）
carryover_next_month_dates <- function(from_ym = NULL) {
  if (is.null(from_ym)) {
    today <- Sys.Date()
    m <- as.integer(format(today, "%m"))
    y <- as.integer(format(today, "%Y"))
  } else {
    parts <- strsplit(from_ym, "-")[[1]]
    y <- as.integer(parts[1])
    m <- as.integer(parts[2])
  }
  m <- m + 1L
  if (m == 13L) { m <- 1L; y <- y + 1L }
  first_day <- as.Date(sprintf("%04d-%02d-01", y, m))
  # 当月最后一天
  last_day <- seq(first_day, by = "month", length.out = 2)[2] - 1
  # 提醒日 25 号
  remind_day <- as.Date(sprintf("%04d-%02d-25", y, m))
  list(
    year = y, month = m,
    created_at = sprintf("%04d-%02d-01 08:00:00", y, m),
    due_at      = sprintf("%s 17:00:00", format(last_day, "%Y-%m-%d")),
    reminder_at = sprintf("%04d-%02d-25 08:01:00", y, m)
  )
}

# 生成下月记事（基于选中模板，from_ym 为模板原月份）
carryover_generate_next_month <- function(note_ids, operator, from_ym = NULL) {
  if (length(note_ids) == 0) return(list(success = FALSE, message = "未选择模板记事"))
  dates <- carryover_next_month_dates(from_ym)
  con <- db_connect()
  tryCatch({
    created <- 0
    for (nid in note_ids) {
      nid <- as.integer(nid)
      orig <- dbGetQuery(con, sprintf(
        "SELECT title, content, created_by FROM notes WHERE id=%d", nid))
      if (nrow(orig) == 0) next
      new_title <- carryover_replace_ym(orig$title[1], sprintf("%04d-%02d", dates$year, dates$month))
      new_no <- note_generate_number()
      dbExecute(con, sprintf(
        "INSERT INTO notes (note_no, title, content, status, importance,
         reminder_at, due_at, created_by, created_at)
         VALUES ('%s', '%s', '%s', 'pending', 0,
         '%s', '%s', %d, '%s')",
        new_no,
        gsub("'","''",new_title),
        gsub("'","''",orig$content[1] %||% ""),
        dates$reminder_at, dates$due_at,
        orig$created_by[1] %||% 1,
        dates$created_at))
      created <- created + 1
    }
    note_fill_missing_no()
    log_user_operation("数据结转-生成下月记事",
      sprintf("%d 条 → %d 条", length(note_ids), created),
      operator$username[1] %||% "系统")
    list(success = TRUE, message = sprintf(
      "已生成 %d 条下月记事（%d年%02d月）", created, dates$year, dates$month))
  }, error = function(e) list(success = FALSE, message = paste("生成失败:", e$message)),
  finally = { db_disconnect(con) })
}

# 一键：结账上月 + 生成下月
carryover_execute <- function(close_ids, gen_ids, operator, from_ym = NULL) {
  r1 <- carryover_close_notes(close_ids, operator)
  r2 <- carryover_generate_next_month(gen_ids, operator, from_ym)
  list(
    success = r1$success && r2$success,
    close_msg = r1$message,
    gen_msg  = r2$message
  )
}

# 换行 helper（用于 DT 表格行内展示多字段）
carryover_join_lines <- function(...) {
  paste(Filter(nchar, c(...)), collapse = "<br>")
}
