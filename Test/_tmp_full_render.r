source("global.R")
source("Script/daily_report.r")
source("Script/requirement_management.r")
source("Script/monthly_carryover.r")

d <- Sys.Date()
# 模拟"昨天"的完整渲染逻辑（照抄 dr_report_content 里的 note 部分）
data <- list(
  date = d - 1, month_mode = FALSE,
  work_orders = daily_report_get_work_orders(d-1),
  tasks = daily_report_get_tasks(d-1),
  task_logs = daily_report_get_task_logs(d-1),
  note_comments = daily_report_get_note_comments(d-1),
  users = daily_report_get_users()
)

users <- data$users
note_comments <- data$note_comments
cat("users:", nrow(users), " note_comments:", nrow(note_comments), "\n")

# 取第一个有 note 评论的用户
# 简化：直接对所有 note_comments 渲染（跳过用户维度，专注 note 渲染是否报错）
user_notes <- note_comments[note_comments$note_no != "NTE20260606002", , drop = FALSE]
cat("user_notes:", nrow(user_notes), "\n")

note_by_no <- split(user_notes, user_notes$note_no)
rainbow <- requirement_rainbow_colors()
today_str <- format(Sys.Date(), "%Y-%m-%d")
note_status_label <- function(st) {
  if (!is.null(st) && !is.na(st) && st == "completed") return(list(txt="已完成", col="#059669"))
  list(txt="进行中", col="#2563eb")
}
.clean_lines <- function(txt) { trimws(gsub("\n\\s*\n", "\n", txt)) }

note_items <- ""
gi <- 0
for (gn in names(note_by_no)) {
  gi <- gi + 1; grp <- note_by_no[[gn]]
  gn_title <- grp$note_title[1] %||% gn
  tops <- grp[is.na(grp$parent_id) | grp$parent_id == 0, , drop = FALSE]
  reps <- grp[!(is.na(grp$parent_id) | grp$parent_id == 0), , drop = FALSE]
  col <- rainbow[((gi - 1) %% length(rainbow)) + 1]

  render_item <- function(c, num_label, color, level) {
    sct <- .clean_lines(c$content %||% "")
    st <- note_status_label(c$status)
    date_str <- c$created_at %||% ""
    is_today <- (substr(date_str, 1, 10) == today_str)
    bg_color <- if (is_today) "#e8f5e9" else "#fafafa"
    indent <- if (level > 0) sprintf("margin-left:%dpx;", level * 28) else ""
    today_mark <- if (is_today) '<span>today</span>' else ""
    seq_html <- sprintf('<span>%s</span>', num_label)
    content_html <- if (sct != "") sprintf('<span>%s</span>', sct) else ""
    time_str <- if (nchar(date_str) >= 16) substr(date_str, 6, 16) else date_str
    time_html <- if (time_str != "") sprintf('<span>%s</span>', time_str) else ""
    status_html <- sprintf('<span>%s</span>', st$txt)
    subs_html <- ""
    if (nrow(reps) > 0) {
      sub <- reps[reps$parent_id == c$id, , drop = FALSE]
      if (nrow(sub) > 0) {
        sub_parts <- c()
        for (si in seq_len(nrow(sub))) {
          sub_parts <- c(sub_parts, render_item(sub[si, ], sprintf("%s.%d", num_label, si), color, level + 1))
        }
        subs_html <- sprintf('<div>%s</div>', paste(sub_parts, collapse=""))
      }
    }
    sprintf('<div>%s%s%s%s%s%s</div>', bg_color, indent, today_mark, seq_html, content_html, subs_html)
  }

  note_items <- paste0(note_items, sprintf('<div>%s、%s %s (%d条)</div>', requirement_num_to_cn(gi), gn, gn_title, nrow(grp)))
  for (ti in seq_len(nrow(tops))) {
    tc <- tops[ti, ]
    note_items <- paste0(note_items, render_item(tc, as.character(ti), col, 0))
  }
}
cat("note_items 长度:", nchar(note_items), "\n")
cat("渲染完成，无报错\n")

# 再测 text_report 的递归
text_report <- ""
note_by_no2 <- split(user_notes, user_notes$note_no)
gi <- 0
for (gn in names(note_by_no2)) {
  gi <- gi + 1
  grp <- note_by_no2[[gn]]
  tops <- grp[is.na(grp$parent_id) | grp$parent_id == 0, , drop = FALSE]
  reps <- grp[!(is.na(grp$parent_id) | grp$parent_id == 0), , drop = FALSE]
  text_report <- paste0(text_report, sprintf("%s、 %s\n", dr_cn_number(gi), grp$note_title[1] %||% ""))
  txt_render_replies <- function(pid, depth, parent_num = NULL) {
    sub <- reps[reps$parent_id == pid, , drop = FALSE]
    if (nrow(sub) == 0) return(list(txt="", has_children=FALSE))
    indent <- paste(rep("  ", depth), collapse="")
    txt <- ""
    for (si in seq_len(nrow(sub))) {
      sc <- sub[si, ]
      sc_content <- gsub("\n", "\n    ", sc$content %||% "")
      if (depth >= 2) {
        lead <- paste(rep("+", depth), collapse="")
        txt <- paste0(txt, sprintf("%s%s- %s\n", indent, lead, sc_content))
      } else {
        num <- if (!is.null(parent_num)) sprintf("%d.%d", parent_num, si) else sprintf("%d", si)
        txt <- paste0(txt, sprintf("%s%s %s\n", indent, num, sc_content))
      }
      child_result <- txt_render_replies(sc$id, depth + 1)
      txt <- paste0(txt, child_result$txt)
    }
    list(txt=txt, has_children=nrow(sub)>0)
  }
  for (ti in seq_len(nrow(tops))) {
    tc <- tops[ti, ]
    text_report <- paste0(text_report, sprintf("%d、 %s\n", ti, tc$content))
    has_replies <- nrow(reps[reps$parent_id == tc$id, , drop=FALSE]) > 0
    if (has_replies) {
      child <- txt_render_replies(tc$id, 1, parent_num = ti)
      if (nchar(child$txt) > 0) text_report <- paste0(text_report, child$txt)
      text_report <- paste0(text_report, "\n")
    }
  }
  text_report <- paste0(text_report, "\n")
}
text_report <- gsub("\n{3,}", "\n\n", text_report)
text_report <- gsub("（\\d{1,2}月\\d{1,2}日）", "", text_report)
cat("text_report 长度:", nchar(text_report), "\n")
cat("text_report 前300字:\n", substr(text_report, 1, 300), "\n")
cat("全部完成，无报错\n")
