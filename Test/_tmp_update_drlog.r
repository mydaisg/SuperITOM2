source("global.R")
con <- db_connect()
tryCatch({
  result_text <- paste0(
    "已修复。引入显式状态 dr_selected_date + dr_trigger，统一 dr_apply() 封装各按钮逻辑，",
    "observeEvent 改依赖 dr_trigger() 而非 input$dr_date，彻底解耦隐式触发；",
    "另加 observeEvent(input$dr_date) 同步手动改日期（带 identical 防循环）。",
    "testServer 验证：点'昨天'后日期从 2026-10-08 正确变 2026-10-07 且 trigger 递增，刷新正常。",
    "R 语法校验通过。待浏览器实际验证各筛选按键。"
  )
  dbExecute(con, sprintf(
    "UPDATE dev_logs SET result = '%s' WHERE log_no = 'DL20261008002'",
    gsub("'", "''", result_text)))
  r <- dbGetQuery(con, "SELECT log_no, title, result FROM dev_logs WHERE log_no = 'DL20261008002'")
  cat("编号:", r$log_no[1], "\n")
  cat("标题:", r$title[1], "\n")
  cat("结果:", r$result[1], "\n")
}, finally = { db_disconnect(con) })
