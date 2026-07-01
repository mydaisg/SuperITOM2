source('global.R')
source('Script/note_management.r')
source('Script/monthly_carryover.r')

con <- db_connect()
tryCatch({
  # 找标题带月份的记事（2026-07）但 content 中仍含旧月份的
  notes <- dbGetQuery(con, "SELECT id, title, content FROM notes WHERE title LIKE '%2026年7月%' OR title LIKE '%2026年07月%'")
  cat('找到', nrow(notes), '条标题为2026年7月的记事\n')
  
  fixed <- 0
  for (i in 1:nrow(notes)) {
    id <- notes$id[i]
    title <- notes$title[i]
    content <- notes$content[i]
    
    # 从标题提取目标月份
    target_ym <- carryover_extract_ym(title)
    if (is.null(target_ym)) next
    
    # 替换 content 中的月份
    new_content <- carryover_replace_ym(content, target_ym)
    if (!is.null(content) && new_content != content) {
      dbExecute(con, sprintf("UPDATE notes SET content = '%s', updated_at = '%s' WHERE id = %d",
        gsub("'", "''", new_content),
        format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
        id))
      cat('  修复 id=', id, ':', substr(title, 1, 30), '\n')
      fixed <- fixed + 1
    }
  }
  cat('共修复', fixed, '条\n')
}, error = function(e) {
  cat('错误:', e$message, '\n')
}, finally = {
  db_disconnect(con)
})
