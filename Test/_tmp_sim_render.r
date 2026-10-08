source("global.R")
source("Script/daily_report.r")
source("Script/requirement_management.r")
source("Script/monthly_carryover.r")

d <- Sys.Date()
data <- list(
  date = d - 1, month_mode = FALSE,
  work_orders = daily_report_get_work_orders(d-1),
  tasks = daily_report_get_tasks(d-1),
  task_logs = daily_report_get_task_logs(d-1),
  note_comments = daily_report_get_note_comments(d-1),
  users = daily_report_get_users()
)

note_comments <- data$note_comments
cat("note_comments 行数:", nrow(note_comments), "\n")

# 模拟渲染层对 note_comments 的处理
users <- data$users
# 取一个实际有评论的用户（假设 admin 看全部，这里直接对全部 note_comments 按 note_no 分组测试）
user_notes <- note_comments[note_comments$note_no != "NTE20260606002", , drop = FALSE]
cat("过滤后 user_notes 行数:", nrow(user_notes), "\n")

note_by_no <- split(user_notes, user_notes$note_no)
rainbow <- requirement_rainbow_colors()
today_str <- format(Sys.Date(), "%Y-%m-%d")

note_status_label <- function(st) {
  if (!is.null(st) && !is.na(st) && st == "completed") return(list(txt = "已完成", col = "#059669"))
  list(txt = "进行中", col = "#2563eb")
}

.clean_lines <- function(txt) { trimws(gsub("\n\\s*\n", "\n", txt)) }

# 递归渲染
render_item <- function(c, num_label, color, level) {
  sct <- .clean_lines(c$content %||% "")
  st <- note_status_label(c$status)
  date_str <- c$created_at %||% ""
  is_today <- (substr(date_str, 1, 10) == today_str)
  bg_color <- if (is_today) "#e8f5e9" else "#fafafa"
  indent <- if (level > 0) sprintf("margin-left:%dpx;", level * 28) else ""
  today_mark <- if (is_today) "" else ""
  seq_html <- ""
  content_html <- if (sct != "") "" else ""
  time_str <- if (nchar(date_str) >= 16) substr(date_str, 6, 16) else date_str
  time_html <- ""
  status_html <- ""
  subs_html <- ""
  # 递归子评论 —— 这里需要 reps，但 render_item 里没有 reps 参数！
  # 注意原代码 render_item 内部引用了 reps（外层作用域变量）
  sprintf("<div>%s%s</div>", content_html, subs_html)
}

gi <- 0
for (gn in names(note_by_no)) {
  gi <- gi + 1
  grp <- note_by_no[[gn]]
  tops <- grp[is.na(grp$parent_id) | grp$parent_id == 0, , drop = FALSE]
  reps <- grp[!(is.na(grp$parent_id) | grp$parent_id == 0), , drop = FALSE]
  cat("组", gi, gn, "顶层:", nrow(tops), "子评论:", nrow(reps), "\n")
}
cat("完成，无报错\n")
