source("global.R")
source("Script/daily_report.r")
source("Script/requirement_management.r")
source("Script/monthly_carryover.r")

d <- Sys.Date()
y <- as.integer(format(d, "%Y"))
dates <- seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by="day")

# 模拟"本年度"月报模式数据
work_orders <- do.call(rbind, lapply(dates, daily_report_get_work_orders))
tasks <- do.call(rbind, lapply(dates, daily_report_get_tasks))
task_logs <- do.call(rbind, lapply(dates, daily_report_get_task_logs))
note_comments <- do.call(rbind, lapply(dates, daily_report_get_note_comments))
if (!is.null(work_orders) && nrow(work_orders) > 0) work_orders <- work_orders[!duplicated(work_orders$id), ]
if (!is.null(tasks) && nrow(tasks) > 0) tasks <- tasks[!duplicated(tasks$id), ]
if (!is.null(note_comments) && nrow(note_comments) > 0) note_comments <- note_comments[!duplicated(note_comments$id), ]

data <- list(
  date = d, month_label = sprintf("%d年", y), month_dates = dates, month_mode = TRUE,
  work_orders = work_orders %||% data.frame(),
  tasks = tasks %||% data.frame(),
  task_logs = task_logs %||% data.frame(),
  note_comments = note_comments %||% data.frame(),
  users = daily_report_get_users()
)

cat("work_orders:", nrow(data$work_orders), " tasks:", nrow(data$tasks),
    " task_logs:", nrow(data$task_logs), " note_comments:", nrow(data$note_comments), "\n")

users <- data$users
work_orders <- data$work_orders
tasks <- data$tasks
task_logs <- data$task_logs
note_comments <- data$note_comments

cards_html <- ""
text_report <- ""

for (ui in 1:nrow(users)) {
  u <- users[ui, ]
  uid <- u$id
  uname <- ifelse(is.na(u$display_name) || u$display_name == "", u$username, u$display_name)
  initial <- substr(uname, 1, 1)

  user_wo <- data.frame()
  if (nrow(work_orders) > 0) {
    user_wo <- work_orders[(!is.na(work_orders$assigned_to) & work_orders$assigned_to == uid) |
      (!is.na(work_orders$handled_by) & work_orders$handled_by == uid) |
      (!is.na(work_orders$created_by) & work_orders$created_by == uid), , drop = FALSE]
  }
  user_tasks <- data.frame()
  if (nrow(tasks) > 0) {
    user_tasks <- tasks[(!is.na(tasks$assigned_to) & tasks$assigned_to == uid) |
      (!is.na(tasks$created_by) & tasks$created_by == uid), , drop = FALSE]
  }
  user_logs <- data.frame()
  if (nrow(task_logs) > 0) {
    u_dn <- if (is.na(u$display_name) || is.null(u$display_name)) "" else u$display_name
    u_un <- if (is.na(u$username) || is.null(u$username)) "" else u$username
    user_logs <- task_logs[!is.na(task_logs$creator_name) & (task_logs$creator_name == u_dn | task_logs$creator_name == u_un), , drop = FALSE]
  }
  user_notes <- data.frame()
  if (nrow(note_comments) > 0) {
    user_notes <- note_comments[!is.na(note_comments$created_by) & note_comments$created_by == uid &
      note_comments$note_no != "NTE20260606002", , drop = FALSE]
  }

  if (nrow(user_wo) == 0 && nrow(user_tasks) == 0 && nrow(user_logs) == 0 && nrow(user_notes) == 0) next

  wo_count <- nrow(user_wo); task_count <- nrow(user_tasks)
  log_count <- nrow(user_logs); note_count <- nrow(user_notes)

  report_label <- if (isTRUE(data$month_mode) && !is.null(data$month_label)) data$month_label else substr(as.character(data$date), 1, 10)

  # 反馈日志渲染（第 641-658 行）
  log_html <- ""
  if (log_count > 0) {
    log_items <- ""
    for (li in 1:min(5, nrow(user_logs))) {
      lg <- user_logs[li, ]
      log_type_cn <- switch(as.character(lg$log_type), "execution" = "执行", "feedback" = "反馈", "status_change" = "状态", "note" = "备注", "其他")
      cat("  [user", ui, "] li=", li, " content class=", class(lg$content), " nchar=", nchar(lg$content), "\n")
      content_short <- if (nchar(lg$content) > 60) paste0(substr(lg$content, 1, 60), "...") else lg$content
      log_items <- paste0(log_items, sprintf('<div class="dr-item"><span class="dr-badge" style="background:#5bc0de;">%s</span> [%s] %s</div>', log_type_cn, ifelse(is.na(lg$task_name), "-", lg$task_name), content_short))
    }
    log_html <- sprintf('<div class="dr-section"><div class="dr-section-title">反馈记录 (%d)</div>%s</div>', log_count, log_items)
  }
}

cat("渲染完成，无报错\n")
