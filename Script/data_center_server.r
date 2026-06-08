# 数据中心模块 — 稳定版（HTML可视化，无 plotly）
data_center_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  navigate_to_tab <- function(tab_name) { session$sendCustomMessage("navigateToTab", tab_name) }

  output$proj_viz <- renderUI({
    s <- project_get_stats()
    total <- s$total[1]; active <- s$active[1]+s$planning[1]; done <- s$completed[1]
    pct <- if(total>0) round(done/total*100) else 0
    HTML(sprintf('<div class="dc-big-num" style="color:#2563eb;">%d</div>
      <div class="dc-bar-track"><div class="dc-bar-fill" style="width:%d%%;background:#2563eb;"></div></div>
      <div style="display:flex;justify-content:space-between;font-size:10px;margin-top:3px;">
        <span><span class="dc-dot" style="background:#2563eb;"></span>进行中 %d</span>
        <span><span class="dc-dot" style="background:#6ee7b7;"></span>已完成 %d</span></div>
      <div style="font-size:10px;color:#8899aa;margin-top:4px;">总项目 %d</div>', total, pct, active, done, total))
  })

  output$wo_viz <- renderUI({
    s <- work_order_get_stats()
    total <- s$total[1]; pending <- s$pending[1]+s$assigned[1]+s$processing[1]; done <- s$completed[1]+s$closed[1]
    pct <- if(total>0) round(done/total*100) else 0
    HTML(sprintf('<div class="dc-big-num" style="color:#e53e3e;">%d</div>
      <div class="dc-bar-track">
        <div style="display:flex;height:100%%;">
          <div style="width:%d%%;background:#f0ad4e;"></div>
          <div style="width:%d%%;background:#5bc0de;"></div>
          <div style="width:%d%%;background:#ff9800;"></div>
        </div>
      </div>
      <div style="display:flex;justify-content:space-between;font-size:10px;margin-top:3px;">
        <span><span class="dc-dot" style="background:#f0ad4e;"></span>待处理 %d</span>
        <span><span class="dc-dot" style="background:#0d7d3a;"></span>已完 %d</span></div>',
      total, round(max(s$pending[1],1)/max(total,1)*100), round(max(s$assigned[1],0.5)/max(total,1)*100),
      round(max(s$processing[1],0.5)/max(total,1)*100), s$pending[1]+s$assigned[1]+s$processing[1], done))
  })

  output$insp_viz <- renderUI({
    s <- inspection_get_stats()
    plans <- s$total_plans[1]; active <- s$active_plans[1]; issues <- s$pending_issues[1]
    HTML(sprintf('<div class="dc-big-num" style="color:#0d7d3a;">%d</div>
      <div style="font-size:10px;color:#8899aa;margin-bottom:4px;">巡检计划</div>
      <div style="display:flex;gap:6px;font-size:10px;">
        <span class="dc-tag" style="background:#d1fae5;color:#065f46;">执行中 %d</span>
        <span class="dc-tag" style="background:%s;color:%s;">异常 %d</span></div>',
      plans, active, if(issues>0)"#fecaca"else"#d1fae5", if(issues>0)"#991b1b"else"#065f46", issues))
  })

  output$nt_viz <- renderUI({
    log_dir <- file.path(getwd(), "Log", "network_test")
    cnt <- 0; last <- "无"
    if (dir.exists(log_dir)) {
      files <- list.files(log_dir, pattern="\\.log$")
      cnt <- length(files)
      if (cnt>0) { fi <- file.info(file.path(log_dir,files)); fi <- fi[order(fi$mtime,decreasing=TRUE),]; last <- format(fi$mtime[1],"%m-%d %H:%M") }
    }
    HTML(sprintf('<div class="dc-big-num" style="color:#7c3aed;">%d</div>
      <div style="font-size:10px;color:#8899aa;">测试记录 · 最近: %s</div>', cnt, last))
  })

  output$dr_viz <- renderUI({
    s <- daily_report_get_stats()
    HTML(sprintf('<div class="dc-big-num" style="color:#d97706;">%d</div>
      <div style="font-size:10px;color:#8899aa;margin-bottom:4px;">活跃用户</div>
      <div style="display:flex;gap:6px;font-size:10px;">
        <span class="dc-tag" style="background:#fef3c7;color:#92400e;">本月工单 %d</span>
        <span class="dc-tag" style="background:#fed7aa;color:#9a3412;">今日活动 %d</span></div>', s$total_users, s$wo_month, s$wo_today+s$task_today))
  })

  output$ast_viz <- renderUI({
    items <- tryCatch(asset_get_all(), error=function(e) data.frame())
    total <- nrow(items); active <- sum(items$status=="active",na.rm=TRUE); maint <- sum(items$status=="maintenance",na.rm=TRUE)
    pct <- if(total>0) round(active/total*100) else 0
    HTML(sprintf('<div class="dc-big-num" style="color:#0891b2;">%d</div>
      <div class="dc-bar-track"><div class="dc-bar-fill" style="width:%d%%;background:#0891b2;"></div></div>
      <div style="display:flex;justify-content:space-between;font-size:10px;margin-top:3px;">
        <span><span class="dc-dot" style="background:#0891b2;"></span>使用中 %d</span>
        <span><span class="dc-dot" style="background:#f59e0b;"></span>维护 %d</span></div>', total, pct, active, maint))
  })

  output$note_viz <- renderUI({
    items <- tryCatch(note_get_all(), error=function(e) data.frame())
    total<-nrow(items); pn<-sum(items$status=="pending",na.rm=TRUE)
    pg<-sum(items$status=="in_progress",na.rm=TRUE); dn<-sum(items$status=="completed",na.rm=TRUE)
    HTML(sprintf('<div class="dc-big-num" style="color:#6c3bbf;">%d</div>
      <div class="dc-grid-3" style="margin-top:6px;">
        <div><div class="dc-gn" style="color:#6c3bbf;">%d</div><div class="dc-gl">待处理</div></div>
        <div><div class="dc-gn" style="color:#2563eb;">%d</div><div class="dc-gl">进行中</div></div>
        <div><div class="dc-gn" style="color:#0d7d3a;">%d</div><div class="dc-gl">已完成</div></div></div>', total,pn,pg,dn))
  })

  output$duty_viz <- renderUI({
    pos <- tryCatch(nrow(duty_position_get_all()),error=function(e)0)
    stf <- tryCatch(nrow(duty_staff_get_all()),error=function(e)0)
    itm <- tryCatch(nrow(duty_item_get_all()),error=function(e)0)
    HTML(sprintf('<div class="dc-grid-3" style="margin-top:10px;">
      <div><div class="dc-gn" style="color:#ea580c;">%d</div><div class="dc-gl">岗位</div></div>
      <div><div class="dc-gn" style="color:#2563eb;">%d</div><div class="dc-gl">人员</div></div>
      <div><div class="dc-gn" style="color:#0d7d3a;">%d</div><div class="dc-gl">职责项</div></div></div>', pos,stf,itm))
  })

  output$perf_viz <- renderUI({
    sheets <- tryCatch(nrow(perf_sheet_list()),error=function(e)0)
    cur <- format(Sys.Date(),"%Y-%m"); emps <- tryCatch(nrow(perf_active_employees(cur)),error=function(e)0)
    inds <- length(perf_indicators())
    HTML(sprintf('<div class="dc-big-num" style="color:#dc2626;">%d</div>
      <div style="font-size:10px;color:#8899aa;margin-bottom:3px;">月绩效表</div>
      <div style="display:flex;gap:6px;font-size:10px;">
        <span class="dc-tag" style="background:#fee2e2;color:#991b1b;">当月 %d 人</span>
        <span class="dc-tag" style="background:#fef3c7;color:#92400e;">%d 指标</span></div>', sheets, emps, inds))
  })

  daily_report_get_stats <- function() {
    con <- db_connect()
    tryCatch({
      today <- format(Sys.Date(),"%Y-%m-%d"); ms <- format(Sys.Date(),"%Y-%m")
      total_users <- dbGetQuery(con,"SELECT COUNT(*) as cnt FROM users WHERE active=1")$cnt[1]
      wo_today <- dbGetQuery(con,sprintf("SELECT COUNT(DISTINCT assigned_to) as cnt FROM work_orders WHERE DATE(created_at)='%s' OR DATE(updated_at)='%s'",today,today))$cnt[1]
      task_today <- dbGetQuery(con,sprintf("SELECT COUNT(DISTINCT assigned_to) as cnt FROM project_tasks WHERE DATE(created_at)='%s' OR DATE(updated_at)='%s'",today,today))$cnt[1]
      wo_month <- dbGetQuery(con,sprintf("SELECT COUNT(*) as cnt FROM work_orders WHERE created_at LIKE '%s%%'",ms))$cnt[1]
      data.frame(total_users=total_users,wo_today=wo_today,task_today=task_today,wo_month=wo_month)
    },error=function(e)data.frame(total_users=0,wo_today=0,task_today=0,wo_month=0),
    finally={db_disconnect(con)})
  }

  observeEvent(input$card_project,{navigate_to_tab("项目")})
  observeEvent(input$card_workorder,{navigate_to_tab("工单")})
  observeEvent(input$card_inspection,{navigate_to_tab("巡检")})
  observeEvent(input$card_network,{navigate_to_tab("测试")})
  observeEvent(input$card_daily,{navigate_to_tab("日报")})
  observeEvent(input$card_asset,{navigate_to_tab("资产")})
  observeEvent(input$card_note,{navigate_to_tab("记事")})
  observeEvent(input$card_duty,{navigate_to_tab("岗职")})
  observeEvent(input$card_perf,{navigate_to_tab("绩效")})
  })
}
