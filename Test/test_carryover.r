source('global.R')
source('Script/log_user.r')
source('Script/note_management.r')
source('Script/monthly_carryover.r')

cat('=== 正则测试 ===\n')
pattern <- "\\((\\d{4})年(\\d{1,2})月\\)"
tests <- c(
  'IT服务-IT问题管理（2026年6月）',
  '日常事务管理-IT部(2026年6月)',
  'IT服务-系统权限开通审批(2026年6月)',
  'IT服务-IT事件管理（2026年6月）'
)
for(t in tests) {
  m <- regmatches(t, regexec(pattern, t))[[1]]
  cat(sprintf('  %s -> match: %s\n', substr(t,1,30), paste(m, collapse='|')))
}

cat('\n=== carryover_list_notes() ===\n')
ns <- carryover_list_notes()
cat('总计匹配:', nrow(ns), '\n')
print(ns[, c('title','ym','status')])

cat('\n=== prev_month_pending ===\n')
p <- carryover_prev_month_pending()
cat('上月未完成:', nrow(p), '\n')
print(p[, c('title','ym','status')])

cat('\n=== 查原始数据 ===\n')
con <- db_connect()
all_notes <- dbGetQuery(con, "SELECT title, status FROM notes WHERE title IS NOT NULL AND title != '' ORDER BY created_at DESC")
dbDisconnect(con)
# 手动 grep
matches <- grepl('\\(\\d{4}年\\d{1,2}月\\)', all_notes$title)
cat('grep matches:', sum(matches), '\n')
print(all_notes[matches, ])
