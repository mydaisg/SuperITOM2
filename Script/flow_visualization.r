# 流程数据可视化模块 — 数据层
# 功能：上传流程实例 Excel → 自动生成 ECharts HTML 看板 → 保存历史记录
# 表：flow_visualizations（每次转化的历史）

##################
# 依赖说明
##################
# - readxl / dplyr / jsonlite 已在 global.R 加载（dplyr 需在函数内显式 library 以确保可用）
# - 生成 HTML 存放到 www/flow_viz/ 目录（通过 addResourcePath("www","www") 可访问）

# 生成历史记录编号
flow_viz_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    date <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("FLV", date)
    existing <- dbGetQuery(con, sprintf(
      "SELECT COUNT(*) AS n FROM flow_visualizations WHERE record_no LIKE '%s%%'", prefix))
    n <- existing$n[1]
    sprintf("%s%03d", prefix, n + 1)
  }, error = function(e) sprintf("FLV%s%03d", format(Sys.Date(), "%Y%m%d"), 1),
  finally = { db_disconnect(con) })
}

# 获取可视化 HTML 输出目录（绝对路径）
flow_viz_output_dir <- function() {
  file.path(getwd(), "www", "flow_viz")
}

# 确保输出目录存在
flow_viz_ensure_dir <- function() {
  d <- flow_viz_output_dir()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# 从流程名称提取"流程类型"：去掉末尾的单号 token（如 CGSQ202608017 / ITS202608005）
# 规则：按空格拆分，若最后一个 token 形如「2-6位大写字母 + 5位以上数字」则视为单号剔除，
#       其余内容完整保留（不再按 "-" 截断，避免把 1X-A、IT服务工单-xxx 之类的名称截碎）。
flow_viz_extract_type <- function(name) {
  name <- as.character(name)
  vapply(name, function(s) {
    s <- trimws(s)
    if (is.na(s) || nchar(s) == 0) return("")
    parts <- strsplit(s, "\\s+")[[1]]
    # 末尾单号：字母前缀 + 数字流水，如 ITS202608005 / CGSQ202608017 / MRBPS202608030
    last <- parts[length(parts)]
    if (grepl("^[A-Za-z]{2,6}[0-9]{5,}$", last)) {
      parts <- parts[-length(parts)]
    }
    paste(parts, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# 核心生成函数：读取 Excel → 聚合 → 生成 HTML
# 参数：
#   src_path  : 上传的 Excel 临时文件路径
#   src_name  : 原始文件名（用于命名输出 HTML）
#   operator  : 操作人（用户名或显示名）
# 返回：
#   list(success, message, html_path, out_name, stats, html_content)
flow_viz_generate <- function(src_path, src_name, operator = "系统") {
  if (!requireNamespace("readxl", quietly = TRUE))
    return(list(success = FALSE, message = "缺少 readxl 包，请先 install.packages('readxl')"))

  tryCatch({
    df <- readxl::read_excel(src_path)
    res <- flow_viz_aggregate(df)

    # ---- 输出文件命名 ----
    base <- tools::file_path_sans_ext(src_name)
    safe_base <- gsub("[^0-9A-Za-z\u4e00-\u9fa5_-]+", "_", base)
    out_name <- paste0(safe_base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    out_dir <- flow_viz_ensure_dir()
    out_path <- file.path(out_dir, out_name)

    con <- file(out_path, "wb")
    writeBin(charToRaw(res$html), con)
    close(con)

    list(success = TRUE, message = "生成成功",
         html_path = out_path, out_name = out_name,
         stats = res$stats, html_content = res$html)

  }, error = function(e) {
    list(success = FALSE, message = paste("生成失败:", e$message))
  })
}

# 聚合统计 + 生成 HTML（Excel 与 DB 两条路径共用）
# 输入：df（含 流程名称/当前节点/发起人/发起时间 四列）
# 输出：list(stats, html, dailyData_json, flowTypeData_json, ...)
flow_viz_aggregate <- function(df) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("缺少 jsonlite 包")

  need <- c("流程名称", "当前节点", "发起人", "发起时间")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0)
    stop(paste("数据缺少必需列:", paste(miss, collapse = ", ")))

  # ---- 预处理 ----
  df$is_done <- grepl("结束", df$当前节点)           # 完成标志：当前节点含"结束"
  # 流程类型：优先使用「所属工作流」列（流程本体）；缺失时回退到从流程名称提取
  if ("所属工作流" %in% names(df)) {
    raw_type <- df$所属工作流
  } else {
    raw_type <- flow_viz_extract_type(df$流程名称)
  }
  raw_type <- trimws(raw_type)
  raw_type[is.na(raw_type) | raw_type == ""] <- "未分类"

  # 拆解「流程本体」与「版本」：所属工作流形如 售后服务工单【历史版本V2】
  # 本体 = 去掉【...版本Vx】后缀；版本 = 方括号内的 活动/历史版本Vn
  df$type    <- sub("【[^】]*】$", "", raw_type)       # 流程本体（合并多版本）
  df$type    <- trimws(df$type)
  df$type[df$type == ""] <- "未分类"
  df$version <- ifelse(grepl("【[^】]*】$", raw_type),
                       sub("^.*【([^】]*)】$", "\\1", raw_type),
                       "")                            # 版本名（无版本为空）
  df$date    <- substr(df$发起时间, 1, 10)           # 发起日期

  total_flows     <- nrow(df)
  completed_flows <- sum(df$is_done)
  active_flows    <- total_flows - completed_flows
  completion_rate <- round(completed_flows / total_flows * 100, 1)

  dates    <- sort(unique(df$date))
  date_min <- min(dates)
  date_max <- max(dates)
  n_days   <- length(dates)

  daily_avg_total <- round(total_flows / n_days)
  daily_avg_done  <- round(completed_flows / n_days)

  type_count       <- length(unique(df$type))
  initiator_count  <- length(unique(df$发起人))

  # ---- 每日趋势 ----
  all_dates <- format(seq(as.Date(date_min), as.Date(date_max), by = "day"), "%Y-%m-%d")
  daily <- as.data.frame(
    table(factor(df$date, levels = all_dates)), stringsAsFactors = FALSE)
  names(daily) <- c("date", "total")
  daily$completed <- sapply(all_dates, function(d) sum(df$is_done[df$date == d]))
  daily$active    <- sapply(all_dates, function(d) sum(!df$is_done[df$date == d]))
  dailyData <- list(
    dates     = as.character(daily$date),
    total     = as.integer(daily$total),
    completed = as.integer(daily$completed),
    active    = as.integer(daily$active))

  # ---- 流程类型分布 ----
  type_tbl <- as.data.frame(table(df$type), stringsAsFactors = FALSE)
  names(type_tbl) <- c("name", "total")
  type_tbl$completed <- sapply(type_tbl$name, function(t) sum(df$is_done[df$type == t]))
  type_tbl$active    <- sapply(type_tbl$name, function(t) sum(!df$is_done[df$type == t]))
  type_tbl <- type_tbl[order(-type_tbl$total, -type_tbl$completed), ]
  rownames(type_tbl) <- NULL

  top5_pct <- round(sum(head(type_tbl$total, 5)) / total_flows * 100, 1)

  # ---- 版本分项（同一流程本体下的各版本下级数据） ----
  # 仅统计存在多版本的本体；单版本本体无下级分项
  version_tbl <- data.frame()
  has_version <- df$version != ""
  if (any(has_version)) {
    vdf <- df[has_version, c("type", "version", "is_done")]
    # 统计每个本体的「不同版本数量」，只保留存在多个版本的本体
    uniq_version_count <- tapply(vdf$version, vdf$type, function(x) length(unique(x)))
    multi <- names(uniq_version_count)[uniq_version_count > 1]
    if (length(multi) > 0) {
      vdf <- vdf[vdf$type %in% multi, ]
      # 按本体+版本聚合
      vt <- as.data.frame(table(vdf$type, vdf$version), stringsAsFactors = FALSE)
      names(vt) <- c("name", "version", "total")
      vt$completed <- mapply(function(nm, vn) sum(vdf$is_done[vdf$type == nm & vdf$version == vn]),
                             vt$name, vt$version)
      vt$active <- vt$total - vt$completed
      vt <- vt[vt$total > 0, ]
      vt <- vt[order(-vt$total, vt$name, vt$version), ]
      rownames(vt) <- NULL
      version_tbl <- vt
    }
  }

  # ---- 流程实例清单（所有实例明细，标注状态） ----
  instance_data <- data.frame(
    name      = df$流程名称,
    version   = df$version,
    initiator = df$发起人,
    time      = as.character(df$发起时间),
    node      = df$当前节点,
    status    = ifelse(df$is_done, "已完成", "进行中"),
    stringsAsFactors = FALSE
  )
  # 按发起时间倒序（最新在前）
  instance_data <- instance_data[order(instance_data$time, decreasing = TRUE), ]
  rownames(instance_data) <- NULL

  # ---- 阻塞节点（进行中流程的当前节点） ----
  active_df <- df[!df$is_done, ]
  blocking <- as.data.frame(table(active_df$当前节点), stringsAsFactors = FALSE)
  names(blocking) <- c("name", "count")
  blocking <- blocking[order(-blocking$count), ]
  rownames(blocking) <- NULL
  blocking$types <- sapply(blocking$name, function(node) {
    tt <- sort(table(active_df$type[active_df$当前节点 == node]), decreasing = TRUE)
    paste(head(paste0(names(tt), "(", tt, ")"), 5), collapse = " ")
  })

  # ---- 发起人排名 Top 15 ----
  init_tbl <- as.data.frame(table(df$发起人), stringsAsFactors = FALSE)
  names(init_tbl) <- c("name", "total")
  init_tbl$completed <- sapply(init_tbl$name, function(p) sum(df$is_done[df$发起人 == p]))
  init_tbl$active    <- sapply(init_tbl$name, function(p) sum(!df$is_done[df$发起人 == p]))
  init_tbl <- init_tbl[order(-init_tbl$total), ]
  init_tbl <- head(init_tbl, 15)
  rownames(init_tbl) <- NULL

  # ---- 每日各类型堆叠（Top 12 + 其他） ----
  top12 <- head(type_tbl$name, 12)
  type_names_full <- c(top12, "其他")
  dailyTypeData <- list()
  for (t in top12) {
    dailyTypeData[[t]] <- as.integer(sapply(all_dates, function(d) sum(df$date == d & df$type == t)))
  }
  dailyTypeData[["其他"]] <- as.integer(sapply(all_dates, function(d) {
    sum(df$date == d & !(df$type %in% top12))
  }))

  # ---- 各类型完成率（Top 12） ----
  typeCompletionData <- head(type_tbl, 12)

  # ---- 序列化 JSON ----
  to_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
  dailyData_json       <- to_json(dailyData)
  flowTypeData_json    <- to_json(type_tbl[, c("name", "total", "completed", "active")])
  blockingNodes_json   <- to_json(blocking[, c("name", "count", "types")])
  initiatorData_json   <- to_json(init_tbl[, c("name", "total", "completed", "active")])
  dailyTypeData_json   <- to_json(dailyTypeData)
  typeNames_json       <- to_json(type_names_full)
  typeCompletionData_json <- to_json(typeCompletionData[, c("name", "total", "completed", "active")])
  if (nrow(version_tbl) > 0) {
    versionData_json <- to_json(version_tbl[, c("name", "version", "total", "completed", "active")])
  } else {
    versionData_json <- "[]"
  }
  instanceData_json  <- to_json(instance_data[, c("name", "version", "initiator", "time", "node", "status")])

  # ---- 生成 HTML ----
  html <- flow_viz_build_html(
    dailyData_json, flowTypeData_json, blockingNodes_json, initiatorData_json,
    dailyTypeData_json, typeNames_json, typeCompletionData_json, versionData_json,
    instanceData_json,
    total_flows, completed_flows, active_flows, completion_rate,
    date_min, date_max, n_days, daily_avg_total, daily_avg_done,
    type_count, initiator_count, top5_pct)

  stats <- list(
    total = total_flows, completed = completed_flows, active = active_flows,
    completion_rate = completion_rate, date_min = date_min, date_max = date_max,
    n_days = n_days, type_count = type_count, initiator_count = initiator_count)

  list(stats = stats, html = html)
}

# HTML 模板（ECharts 交互式看板）
flow_viz_build_html <- function(dailyData_json, flowTypeData_json, blockingNodes_json,
                                initiatorData_json, dailyTypeData_json, typeNames_json,
                                typeCompletionData_json, versionData_json,
                                instanceData_json,
                                total_flows, completed_flows,
                                active_flows, completion_rate, date_min, date_max, n_days,
                                daily_avg_total, daily_avg_done, type_count,
                                initiator_count, top5_pct) {
  fmt_date <- function(s) paste0(format(as.Date(s), "%Y年%m月%d日"))
  date_min_fmt <- fmt_date(date_min)
  date_max_fmt <- fmt_date(date_max)

  paste0('<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>流程实例数据看板</title>
    <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif; background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%); min-height: 100vh; color: #fff; padding: 20px; }
        .header { text-align: center; padding: 30px 0; margin-bottom: 20px; }
        .header h1 { font-size: 36px; font-weight: 700; background: linear-gradient(90deg, #00d4ff, #7b2cbf); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text; margin-bottom: 10px; }
        .header .subtitle { color: #8892b0; font-size: 16px; }
        .kpi-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }
        .kpi-card { background: rgba(255,255,255,0.05); border-radius: 16px; padding: 24px; border: 1px solid rgba(255,255,255,0.1); transition: all 0.3s ease; }
        .kpi-card:hover { transform: translateY(-5px); border-color: rgba(0,212,255,0.3); box-shadow: 0 10px 40px rgba(0,212,255,0.1); }
        .kpi-label { color: #8892b0; font-size: 14px; margin-bottom: 8px; }
        .kpi-value { font-size: 32px; font-weight: 700; color: #fff; }
        .kpi-value.warn { color: #ffd700; } .kpi-value.success { color: #00e676; }
        .kpi-change { font-size: 14px; margin-top: 8px; color:#8892b0; }
        .chart-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 20px; margin-bottom: 30px; }
        .chart-card { background: rgba(255,255,255,0.05); border-radius: 16px; padding: 24px; border: 1px solid rgba(255,255,255,0.1); }
        .chart-card.full-width { grid-column: 1 / -1; }
        .chart-title { font-size: 18px; font-weight: 600; margin-bottom: 20px; color: #fff; }
        .chart-container { width: 100%; height: 350px; }
        .chart-container-tall { width: 100%; height: 450px; }
        .ranking-table { width: 100%; border-collapse: collapse; margin-top: 10px; }
        .ranking-table th, .ranking-table td { padding: 10px 12px; text-align: left; border-bottom: 1px solid rgba(255,255,255,0.1); }
        .ranking-table th { color: #8892b0; font-weight: 500; font-size: 12px; text-transform: uppercase; }
        .ranking-table td { font-size: 14px; }
        .ranking-table tr:hover { background: rgba(255,255,255,0.05); }
        .rank-badge { display: inline-block; width: 28px; height: 28px; line-height: 28px; text-align: center; border-radius: 50%; font-weight: 600; font-size: 12px; }
        .rank-badge.gold { background: linear-gradient(135deg,#ffd700,#ffb700); color:#000; }
        .rank-badge.silver { background: linear-gradient(135deg,#c0c0c0,#a0a0a0); color:#000; }
        .rank-badge.bronze { background: linear-gradient(135deg,#cd7f32,#b87333); color:#000; }
        .rank-badge.default { background: rgba(255,255,255,0.1); color:#8892b0; }
        .status-dot { display: inline-block; width: 8px; height: 8px; border-radius: 50%; margin-right: 6px; }
        .status-dot.active { background: #ffd700; }
        .progress-bar-bg { width: 100%; height: 6px; background: rgba(255,255,255,0.1); border-radius: 3px; margin-top: 4px; }
        .progress-bar-fill { height: 6px; border-radius: 3px; transition: width 0.5s; }
        .type-main-row { cursor: pointer; }
        .type-main-row:hover { background: rgba(255,255,255,0.08); }
        .type-expand-icon { display: inline-block; width: 16px; text-align: center; color: #00d4ff; transition: transform 0.2s; margin-right: 4px; }
        .type-main-row.open .type-expand-icon { transform: rotate(90deg); }
        .version-row { display: none; background: rgba(0,0,0,0.2); }
        .version-row.show { display: table-row; }
        .version-row td { padding: 6px 12px; font-size: 13px; color: #c0c8dd; }
        .version-tag { display: inline-block; padding: 1px 8px; border-radius: 10px; font-size: 11px; margin-left: 20px; }
        .status-badge { display: inline-block; padding: 2px 10px; border-radius: 10px; font-size: 12px; font-weight: 600; }
        .status-badge.done { background: rgba(0,230,118,0.15); color: #00e676; }
        .status-badge.active { background: rgba(255,215,0,0.15); color: #ffd700; }
        .footer { text-align: center; padding: 30px; color: #8892b0; font-size: 14px; }
        @media (max-width: 1200px) { .chart-grid { grid-template-columns: 1fr; } }
        @media (max-width: 768px) { .kpi-grid { grid-template-columns: 1fr; } .header h1 { font-size: 24px; } .kpi-value { font-size: 24px; } }
    </style>
</head>
<body>
    <div class="header">
        <h1>流程实例数据看板</h1>
        <div class="subtitle">数据周期：', date_min_fmt, ' - ', date_max_fmt, ' | 总流程数：', total_flows, '</div>
    </div>
    <div class="kpi-grid">
        <div class="kpi-card"><div class="kpi-label">流程总数</div><div class="kpi-value">', total_flows, '</div><div class="kpi-change">', n_days, '天监控周期</div></div>
        <div class="kpi-card"><div class="kpi-label">已完成流程</div><div class="kpi-value success">', completed_flows, '</div><div class="kpi-change">完成率 ', completion_rate, '%</div></div>
        <div class="kpi-card"><div class="kpi-label">进行中流程</div><div class="kpi-value warn">', active_flows, '</div><div class="kpi-change">待处理占比 ', round(active_flows/total_flows*100,1), '%</div></div>
        <div class="kpi-card"><div class="kpi-label">日均发起量</div><div class="kpi-value">', daily_avg_total, '</div><div class="kpi-change">日均完成 ', daily_avg_done, '</div></div>
        <div class="kpi-card"><div class="kpi-label">流程类型数</div><div class="kpi-value">', type_count, '</div><div class="kpi-change">TOP5占比 ', top5_pct, '%</div></div>
        <div class="kpi-card"><div class="kpi-label">参与人数</div><div class="kpi-value">', initiator_count, '</div><div class="kpi-change">发起人总数</div></div>
    </div>
    <div class="chart-grid"><div class="chart-card full-width"><div class="chart-title">每日流程发起与完成趋势</div><div id="dailyTrend" class="chart-container-tall"></div></div></div>
    <div class="chart-grid"><div class="chart-card full-width"><div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:10px;"><div class="chart-title" style="margin-bottom:0;">流程类型分布清单 (共', type_count, '种)</div><div style="display:flex;align-items:center;gap:8px;flex-wrap:wrap;"><select id="flowTypeShowCount" style="background:#1e2a44;color:#fff;border:1px solid #2d3748;border-radius:6px;padding:4px 8px;font-size:12px;"><option value="20">显示 20 行</option><option value="50">显示 50 行</option><option value="all">全部显示</option></select><button type="button" id="ftExpandAll" style="background:#1e2a44;color:#00d4ff;border:1px solid #2d3748;border-radius:6px;padding:4px 10px;font-size:12px;cursor:pointer;">全部展开</button><button type="button" id="ftCollapseAll" style="background:#1e2a44;color:#8892b0;border:1px solid #2d3748;border-radius:6px;padding:4px 10px;font-size:12px;cursor:pointer;">全部收缩</button></div></div><div><table class="ranking-table" id="flowTypeTable"><thead><tr><th style="width:50px;">排名</th><th>流程类型</th><th style="width:70px;">总数</th><th style="width:70px;">已完成</th><th style="width:70px;">进行中</th><th style="width:80px;">完成率</th><th>完成进度</th></tr></thead><tbody></tbody></table></div></div></div>
    <div class="chart-grid">
        <div class="chart-card"><div class="chart-title">流程状态仪表盘</div><div id="statusGauge" class="chart-container"></div></div>
        <div class="chart-card"><div class="chart-title">流程类型分布饼图 (Top 12 + 其他)</div><div id="flowTypePie" class="chart-container"></div></div>
    </div>
    <div class="chart-grid"><div class="chart-card full-width"><div class="chart-title">阻塞节点分析 (进行中流程当前所在节点)</div><div id="blockingNodes" class="chart-container-tall"></div></div></div>
    <div class="chart-grid">
        <div class="chart-card"><div class="chart-title">发起人流程数量排名 (Top 15)</div><div id="initiatorRank" class="chart-container-tall"></div></div>
        <div class="chart-card"><div class="chart-title">各类型流程完成率</div><div id="typeCompletionRate" class="chart-container-tall"></div></div>
    </div>
    <div class="chart-grid"><div class="chart-card full-width"><div class="chart-title">每日各流程类型发起量堆叠</div><div id="dailyTypeStack" class="chart-container-tall"></div></div></div>
    <div class="chart-grid"><div class="chart-card full-width"><div class="chart-title">进行中流程阻塞节点详情 (Top 20)</div><table class="ranking-table" id="blockingTable"><thead><tr><th>排名</th><th>阻塞节点</th><th>卡住流程数</th><th>涉及流程类型</th><th>占比</th></tr></thead><tbody></tbody></table></div></div>
    <div class="chart-grid"><div class="chart-card full-width"><div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:10px;"><div class="chart-title" style="margin-bottom:0;">流程实例清单 (全部实例明细)</div><div style="display:flex;align-items:center;gap:8px;flex-wrap:wrap;"><input type="text" id="instSearch" placeholder="搜索流程名称/发起人" style="background:#1e2a44;color:#fff;border:1px solid #2d3748;border-radius:6px;padding:4px 10px;font-size:12px;width:180px;"><select id="instStatusFilter" style="background:#1e2a44;color:#fff;border:1px solid #2d3748;border-radius:6px;padding:4px 8px;font-size:12px;"><option value="all">全部状态</option><option value="进行中">进行中</option><option value="已完成">已完成</option></select><select id="instShowCount" style="background:#1e2a44;color:#fff;border:1px solid #2d3748;border-radius:6px;padding:4px 8px;font-size:12px;"><option value="50">显示 50 行</option><option value="100">显示 100 行</option><option value="all">全部显示</option></select></div></div><div><table class="ranking-table" id="instanceTable"><thead><tr><th style="width:50px;">#</th><th>流程名称</th><th style="width:90px;">发起人</th><th style="width:160px;">发起时间</th><th style="width:140px;">当前节点</th><th style="width:80px;">状态</th></tr></thead><tbody></tbody></table></div></div></div>
    <div class="footer">数据来源：流程实例导出数据 | 报表生成：LVCC ITOM | 更新日期：', format(Sys.Date(), "%Y年%m月%d日"), '</div>
    <script>
        const dailyData = ', dailyData_json, ';
        const flowTypeData = ', flowTypeData_json, ';
        const blockingNodes = ', blockingNodes_json, ';
        const initiatorData = ', initiatorData_json, ';
        const dailyTypeData = ', dailyTypeData_json, ';
        const typeNames = ', typeNames_json, ';
        const typeCompletionData = ', typeCompletionData_json, ';
        const versionData = ', versionData_json, ';
        const instanceData = ', instanceData_json, ';
        const totalFlows = ', total_flows, ';
        const activeFlows = ', active_flows, ';
        const completionRate = ', completion_rate, ';

        const dailyTrendChart = echarts.init(document.getElementById("dailyTrend"));
        dailyTrendChart.setOption({
            tooltip: { trigger: "axis", backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#00d4ff", textStyle: { color: "#fff" }, formatter: function(params) { let result = "<div style=\\"font-weight:600;margin-bottom:8px;\\">" + params[0].axisValue + "</div>"; params.forEach(p => { result += "<div style=\\"display:flex;align-items:center;margin:4px 0;\\"><span style=\\"display:inline-block;width:10px;height:10px;background:" + p.color + ";border-radius:50%;margin-right:8px;\\"></span>" + p.seriesName + ": <strong>" + p.value + "</strong></div>"; }); return result; } },
            legend: { data: ["发起总数", "已完成", "进行中"], textStyle: { color: "#8892b0" }, top: 0 },
            grid: { left: "3%", right: "4%", bottom: "3%", containLabel: true },
            xAxis: { type: "category", data: dailyData.dates, axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0" } },
            yAxis: { type: "value", name: "流程数", axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0" }, splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } } },
            series: [
                { name: "发起总数", type: "line", data: dailyData.total, smooth: true, itemStyle: { color: "#00d4ff" }, lineStyle: { width: 3 }, areaStyle: { color: new echarts.graphic.LinearGradient(0,0,0,1,[{offset:0,color:"rgba(0,212,255,0.3)"},{offset:1,color:"rgba(0,212,255,0)"}]) } },
                { name: "已完成", type: "bar", data: dailyData.completed, itemStyle: { color: "rgba(0,230,118,0.7)" }, barWidth: "25%" },
                { name: "进行中", type: "bar", data: dailyData.active, itemStyle: { color: "rgba(255,215,0,0.7)" }, barWidth: "25%" }
            ]
        });

        const flowTypePieChart = echarts.init(document.getElementById("flowTypePie"));
        const top12 = flowTypeData.slice(0, 12);
        const otherTotal = flowTypeData.slice(12).reduce((s, f) => s + f.total, 0);
        const otherActive = flowTypeData.slice(12).reduce((s, f) => s + f.active, 0);
        const pieColors = ["#00d4ff","#7b2cbf","#00e676","#ffd700","#ff6b6b","#ff9f43","#a55eea","#54a0ff","#5f27cd","#01a3a4","#f368e0","#ff6348"];
        const pieData = top12.map((f,i) => ({ name: f.name, value: f.total, active: f.active, itemStyle: { color: pieColors[i % 12] } }));
        pieData.push({ name: "其他(" + (flowTypeData.length - 12) + "种)", value: otherTotal, active: otherActive, itemStyle: { color: "#8892b0" } });
        flowTypePieChart.setOption({
            tooltip: { trigger: "item", backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#00d4ff", textStyle: { color: "#fff" }, formatter: function(p) { return "<div style=\\"font-weight:600;\\">" + p.name + "</div><div>总数: " + p.value + "</div><div>进行中: " + (p.data.active||0) + "</div><div>占比: " + p.percent.toFixed(1) + "%</div>"; } },
            legend: { orient: "vertical", right: "3%", top: "center", textStyle: { color: "#8892b0", fontSize: 10 }, formatter: function(n) { return n.length > 8 ? n.substring(0,8) + ".." : n; } },
            series: [{ type: "pie", radius: ["35%","65%"], center: ["35%","50%"], avoidLabelOverlap: false, itemStyle: { borderRadius: 8, borderColor: "#1a1a2e", borderWidth: 2 }, label: { show: false }, emphasis: { label: { show: true, fontSize: 14, fontWeight: "bold", color: "#fff" }, itemStyle: { shadowBlur: 20, shadowColor: "rgba(0,212,255,0.5)" } }, data: pieData }]
        });

        const statusGaugeChart = echarts.init(document.getElementById("statusGauge"));
        statusGaugeChart.setOption({
            tooltip: { formatter: "完成率: {c}%", backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#00d4ff", textStyle: { color: "#fff" } },
            series: [{ type: "gauge", startAngle: 210, endAngle: -30, center: ["50%","55%"], radius: "85%", min: 0, max: 100, splitNumber: 10, axisLine: { lineStyle: { width: 20, color: [[0.3,"#ff5252"],[0.6,"#ffd700"],[1,"#00e676"]] } }, pointer: { icon: "path://M12.8,0.7l12,40.1H0.7L12.8,0.7z", length: "60%", width: 8, itemStyle: { color: "auto" } }, axisTick: { length: 10, lineStyle: { color: "auto", width: 2 } }, splitLine: { length: 20, lineStyle: { color: "auto", width: 4 } }, axisLabel: { color: "#8892b0", fontSize: 12, distance: 25, formatter: "{value}%" }, title: { offsetCenter: [0,"80%"], fontSize: 16, color: "#8892b0" }, detail: { fontSize: 36, offsetCenter: [0,"50%"], valueAnimation: true, formatter: "{value}%", color: "#fff", fontWeight: "bold" }, data: [{ value: completionRate, name: "流程完成率" }] }]
        });

        const blockingNodesChart = echarts.init(document.getElementById("blockingNodes"));
        const bnReversed = blockingNodes.slice().reverse();
        blockingNodesChart.setOption({
            tooltip: { trigger: "axis", axisPointer: { type: "shadow" }, backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#ffd700", textStyle: { color: "#fff" }, formatter: function(params) { const d = blockingNodes[params[0].dataIndex]; return "<div style=\\"font-weight:600;\\">" + d.name + "</div><div>卡住流程数: <strong>" + d.count + "</strong></div><div>涉及类型: " + d.types + "</div>"; } },
            grid: { left: "3%", right: "15%", bottom: "3%", containLabel: true },
            xAxis: { type: "value", name: "卡住流程数", axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0" }, splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } } },
            yAxis: { type: "category", data: bnReversed.map(n => n.name.length > 10 ? n.name.substring(0,10) + ".." : n.name), axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0", fontSize: 11 } },
            series: [{ type: "bar", data: bnReversed.map((n,i) => ({ value: n.count, itemStyle: { color: new echarts.graphic.LinearGradient(0,0,1,0,[{offset:0,color:i<3?"#ff5252":i<6?"#ffd700":"#ff9f43"},{offset:1,color:i<3?"#cc0000":i<6?"#cc9900":"#cc6600"}]) } })), barWidth: "60%", label: { show: true, position: "right", color: "#8892b0", fontSize: 12, formatter: "{c}" } }]
        });

        const initiatorRankChart = echarts.init(document.getElementById("initiatorRank"));
        initiatorRankChart.setOption({
            tooltip: { trigger: "axis", axisPointer: { type: "shadow" }, backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#00d4ff", textStyle: { color: "#fff" }, formatter: function(params) { const d = initiatorData[params[0].dataIndex]; return "<div style=\\"font-weight:600;\\">" + d.name + "</div><div>总流程: " + d.total + "</div><div>进行中: <span style=\\"color:#ffd700\\">" + d.active + "</span></div><div>已完成: <span style=\\"color:#00e676\\">" + d.completed + "</span></div>"; } },
            legend: { data: ["已完成","进行中"], textStyle: { color: "#8892b0" }, top: 0 },
            grid: { left: "3%", right: "10%", bottom: "3%", containLabel: true },
            xAxis: { type: "value", axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0" }, splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } } },
            yAxis: { type: "category", data: initiatorData.slice().reverse().map(o => o.name), axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0", fontSize: 12 } },
            series: [
                { name: "已完成", type: "bar", stack: "total", data: initiatorData.slice().reverse().map(o => o.completed), itemStyle: { color: "rgba(0,230,118,0.7)" }, barWidth: "50%" },
                { name: "进行中", type: "bar", stack: "total", data: initiatorData.slice().reverse().map(o => o.active), itemStyle: { color: "rgba(255,215,0,0.7)" }, barWidth: "50%" }
            ]
        });

        const typeCompletionRateChart = echarts.init(document.getElementById("typeCompletionRate"));
        const tcData = typeCompletionData.slice().reverse();
        const avgRate = completionRate;
        typeCompletionRateChart.setOption({
            tooltip: { trigger: "axis", axisPointer: { type: "shadow" }, backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#00d4ff", textStyle: { color: "#fff" }, formatter: function(params) { const d = tcData[params[0].dataIndex]; const rate = (d.completed/d.total*100).toFixed(1); return "<div style=\\"font-weight:600;\\">" + d.name + "</div><div>完成率: <strong>" + rate + "%</strong></div><div>已完成: " + d.completed + " / 总数: " + d.total + "</div>"; } },
            grid: { left: "3%", right: "10%", bottom: "3%", containLabel: true },
            xAxis: { type: "value", max: 100, axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0", formatter: "{value}%" }, splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } } },
            yAxis: { type: "category", data: tcData.map(f => f.name.length > 10 ? f.name.substring(0,10) + ".." : f.name), axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0", fontSize: 10 } },
            series: [{ type: "bar", data: tcData.map(f => ({ value: (f.completed/f.total*100).toFixed(1), itemStyle: { color: (f.completed/f.total) >= 0.8 ? "#00e676" : (f.completed/f.total) >= 0.5 ? "#ffd700" : "#ff5252" } })), barWidth: "60%", label: { show: true, position: "right", color: "#8892b0", fontSize: 11, formatter: "{c}%" }, markLine: { silent: true, data: [{ xAxis: avgRate, label: { formatter: "平均 " + avgRate + "%", color: "#8892b0" }, lineStyle: { color: "#00d4ff", type: "dashed" } }] } }]
        });

        const dailyTypeStackChart = echarts.init(document.getElementById("dailyTypeStack"));
        const stackColors = ["#00d4ff","#7b2cbf","#00e676","#ffd700","#ff6b6b","#ff9f43","#a55eea","#54a0ff","#5f27cd","#01a3a4","#f368e0","#ff6348","#8892b0"];
        dailyTypeStackChart.setOption({
            tooltip: { trigger: "axis", backgroundColor: "rgba(0,0,0,0.8)", borderColor: "#00d4ff", textStyle: { color: "#fff" } },
            legend: { data: typeNames, textStyle: { color: "#8892b0", fontSize: 10 }, top: 0, type: "scroll" },
            grid: { left: "3%", right: "4%", bottom: "3%", top: "15%", containLabel: true },
            xAxis: { type: "category", data: dailyData.dates, axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0" } },
            yAxis: { type: "value", name: "流程数", axisLine: { lineStyle: { color: "#2d3748" } }, axisLabel: { color: "#8892b0" }, splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } } },
            series: typeNames.map((name, i) => ({ name: name, type: "bar", stack: "total", data: dailyTypeData[name], emphasis: { focus: "series" }, itemStyle: { color: stackColors[i % 13] } }))
        });

        const ftTableBody = document.querySelector("#flowTypeTable tbody");
        let ftShowCount = 20;                        // 默认显示前 20 个本体
        const ftExpanded = new Set();                // 已展开（下级）的本体名集合

        // 根据当前显示数量 + 展开状态重绘流程类型清单
        function renderFlowTypeTable() {
            ftTableBody.innerHTML = "";
            const limit = ftShowCount === "all" ? flowTypeData.length : Math.min(ftShowCount, flowTypeData.length);
            flowTypeData.slice(0, limit).forEach((ft, index) => {
                const row = document.createElement("tr");
                row.className = "type-main-row";
                const rankBadge = index < 3 ? "<span class=\\"rank-badge " + ["gold","silver","bronze"][index] + "\\">" + (index+1) + "</span>" : "<span class=\\"rank-badge default\\">" + (index+1) + "</span>";
                const rate = (ft.completed/ft.total*100).toFixed(1);
                const barColor = rate >= 80 ? "#00e676" : rate >= 50 ? "#ffd700" : "#ff5252";
                const subs = versionData.filter(v => v.name === ft.name);
                const hasSub = subs.length > 0;
                const isOpen = ftExpanded.has(ft.name);
                if (isOpen) row.classList.add("open");
                const expandIcon = hasSub ? "<span class=\\"type-expand-icon\\">&#9654;</span>" : "";
                const nameCell = expandIcon + ft.name + (hasSub ? "<span style=\\"font-size:11px;color:#8892b0;\\">(" + subs.length + "版本)</span>" : "");
                row.innerHTML = "<td>" + rankBadge + "</td><td>" + nameCell + "</td><td style=\\"font-weight:600;\\">" + ft.total + "</td><td style=\\"color:#00e676;\\">" + ft.completed + "</td><td style=\\"color:#ffd700;\\">" + ft.active + "</td><td style=\\"color:" + barColor + ";font-weight:600;\\">" + rate + "%</td><td><div style=\\"display:flex;align-items:center;gap:8px;\\"><div class=\\"progress-bar-bg\\" style=\\"flex:1;\\"><div class=\\"progress-bar-fill\\" style=\\"width:" + rate + "%;background:" + barColor + ";\\"></div></div></div></td>";
                ftTableBody.appendChild(row);

                if (hasSub) {
                    subs.forEach(v => {
                        const vrow = document.createElement("tr");
                        vrow.className = "version-row" + (isOpen ? " show" : "");
                        const vrate = (v.completed/v.total*100).toFixed(1);
                        const vbarColor = vrate >= 80 ? "#00e676" : vrate >= 50 ? "#ffd700" : "#ff5252";
                        const verColor = v.version.indexOf("活动") === 0 ? "#00d4ff" : "#8892b0";
                        vrow.innerHTML = "<td></td><td><span class=\\"version-tag\\" style=\\"background:" + (v.version.indexOf("活动") === 0 ? "rgba(0,212,255,0.15)" : "rgba(136,146,176,0.15)") + ";color:" + verColor + ";\\">" + v.version + "</span></td><td style=\\"font-weight:600;\\">" + v.total + "</td><td style=\\"color:#00e676;\\">" + v.completed + "</td><td style=\\"color:#ffd700;\\">" + v.active + "</td><td style=\\"color:" + vbarColor + ";font-weight:600;\\">" + vrate + "%</td><td><div style=\\"display:flex;align-items:center;gap:8px;\\"><div class=\\"progress-bar-bg\\" style=\\"flex:1;\\"><div class=\\"progress-bar-fill\\" style=\\"width:" + vrate + "%;background:" + vbarColor + ";\\"></div></div></div></td>";
                        ftTableBody.appendChild(vrow);
                    });
                    row.addEventListener("click", function() {
                        if (ftExpanded.has(ft.name)) ftExpanded.delete(ft.name);
                        else ftExpanded.add(ft.name);
                        renderFlowTypeTable();
                    });
                }
            });
        }

        // 初始渲染
        renderFlowTypeTable();

        // 显示行数切换
        const ftShowSel = document.getElementById("flowTypeShowCount");
        ftShowSel.addEventListener("change", function() {
            ftShowCount = this.value === "all" ? "all" : parseInt(this.value, 10);
            renderFlowTypeTable();
        });

        // 全部展开 / 全部收缩
        document.getElementById("ftExpandAll").addEventListener("click", function() {
            const limit = ftShowCount === "all" ? flowTypeData.length : Math.min(ftShowCount, flowTypeData.length);
            flowTypeData.slice(0, limit).forEach(ft => {
                if (versionData.some(v => v.name === ft.name)) ftExpanded.add(ft.name);
            });
            renderFlowTypeTable();
        });
        document.getElementById("ftCollapseAll").addEventListener("click", function() {
            ftExpanded.clear();
            renderFlowTypeTable();
        });

        const tableBody = document.querySelector("#blockingTable tbody");
        blockingNodes.slice(0, 20).forEach((node, index) => {
            const row = document.createElement("tr");
            const rankBadge = index < 3 ? "<span class=\\"rank-badge " + ["gold","silver","bronze"][index] + "\\">" + (index+1) + "</span>" : "<span class=\\"rank-badge default\\">" + (index+1) + "</span>";
            const pct = (node.count/activeFlows*100).toFixed(1);
            const barColor = index < 3 ? "#ff5252" : index < 6 ? "#ffd700" : "#ff9f43";
            row.innerHTML = "<td>" + rankBadge + "</td><td><span class=\\"status-dot active\\"></span>" + node.name + "</td><td style=\\"color:" + barColor + ";font-weight:600;\\">" + node.count + "</td><td style=\\"font-size:12px;color:#8892b0;\\">" + node.types + "</td><td><div style=\\"display:flex;align-items:center;gap:8px;\\"><span>" + pct + "%</span><div class=\\"progress-bar-bg\\" style=\\"flex:1;\\"><div class=\\"progress-bar-fill\\" style=\\"width:" + pct + "%;background:" + barColor + ";\\"></div></div></div></td>";
            tableBody.appendChild(row);
        });

        // ---- 流程实例清单（所有实例明细 + 状态标注） ----
        const instTableBody = document.querySelector("#instanceTable tbody");
        let instShowCount = 50;                 // 默认显示前 50 条
        let instKeyword = "";
        let instStatus = "all";

        function renderInstanceTable() {
            instTableBody.innerHTML = "";
            const kw = instKeyword.trim().toLowerCase();
            const filtered = instanceData.filter(it => {
                if (instStatus !== "all" && it.status !== instStatus) return false;
                if (kw && it.name.toLowerCase().indexOf(kw) < 0 && it.initiator.toLowerCase().indexOf(kw) < 0) return false;
                return true;
            });
            const limit = instShowCount === "all" ? filtered.length : Math.min(instShowCount, filtered.length);
            filtered.slice(0, limit).forEach((it, i) => {
                const row = document.createElement("tr");
                const isDone = it.status === "已完成";
                const badge = isDone
                    ? "<span class=\\"status-badge done\\">已完成</span>"
                    : "<span class=\\"status-badge active\\">进行中</span>";
                const nodeColor = isDone ? "#00e676" : "#ffd700";
                row.innerHTML = "<td style=\\"color:#8892b0;\\">" + (i+1) + "</td><td>" + it.name + "</td><td>" + it.initiator + "</td><td style=\\"color:#8892b0;\\">" + it.time + "</td><td style=\\"color:" + nodeColor + ";\\">" + it.node + "</td><td>" + badge + "</td>";
                instTableBody.appendChild(row);
            });
        }
        renderInstanceTable();

        document.getElementById("instSearch").addEventListener("input", function() {
            instKeyword = this.value;
            renderInstanceTable();
        });
        document.getElementById("instStatusFilter").addEventListener("change", function() {
            instStatus = this.value;
            renderInstanceTable();
        });
        document.getElementById("instShowCount").addEventListener("change", function() {
            instShowCount = this.value === "all" ? "all" : parseInt(this.value, 10);
            renderInstanceTable();
        });

        window.addEventListener("resize", function() {
            dailyTrendChart.resize(); flowTypePieChart.resize(); statusGaugeChart.resize();
            blockingNodesChart.resize(); initiatorRankChart.resize();
            typeCompletionRateChart.resize(); dailyTypeStackChart.resize();
        });
    </script>
</body>
</html>
')
}

# 保存历史记录（html_content 可选：存 HTML 内容到 DB 供移植/重新导出）
# kind: 'viz'=流程数据看板（默认），'log'=流程日志效率图
flow_viz_add_record <- function(record_no, src_name, out_name, stats, operator, html_content = NULL, kind = "viz") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO flow_visualizations (record_no, src_name, out_name, total_flows, completed_flows, active_flows, completion_rate, date_min, date_max, created_by, html_content, kind, created_at)
       VALUES ('%s','%s','%s',%d,%d,%d,%s,'%s','%s','%s',%s,'%s',datetime('now','localtime'))",
      record_no,
      gsub("'","''", src_name),
      gsub("'","''", out_name),
      as.integer(stats$total), as.integer(stats$completed), as.integer(stats$active),
      stats$completion_rate,
      stats$date_min, stats$date_max,
      gsub("'","''", operator),
      if (is.null(html_content)) "NULL" else paste0("'", gsub("'", "''", html_content), "'"),
      kind))
    TRUE
  }, error = function(e) FALSE, finally = { db_disconnect(con) })
}

# 保存流程日志效率图历史记录（kind='log'，字段语义映射为节点统计）
flow_log_add_record <- function(record_no, src_name, out_name, stats, operator, html_content = NULL) {
  con <- db_connect()
  tryCatch({
    # 映射：total_flows→节点数, completed_flows→已完成节点, active_flows→未完成节点
    node_count <- as.integer(stats$node_count)
    done_nodes <- as.integer(stats$done_nodes)
    undone_nodes <- as.integer(stats$undone_nodes)
    rate <- if (node_count > 0) round(done_nodes / node_count * 100, 1) else 0
    dbExecute(con, sprintf(
      "INSERT INTO flow_visualizations (record_no, src_name, out_name, total_flows, completed_flows, active_flows, completion_rate, date_min, date_max, created_by, html_content, kind, created_at)
       VALUES ('%s','%s','%s',%d,%d,%d,%s,'%s','%s','%s',%s,'log',datetime('now','localtime'))",
      record_no,
      gsub("'","''", src_name),
      gsub("'","''", out_name),
      node_count, done_nodes, undone_nodes, rate,
      stats$flow_start, stats$flow_end,
      gsub("'","''", operator),
      if (is.null(html_content)) "NULL" else paste0("'", gsub("'", "''", html_content), "'")))
    TRUE
  }, error = function(e) FALSE, finally = { db_disconnect(con) })
}

# 获取历史记录（不含 html_content，避免大字段拖慢列表）
flow_viz_get_history <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT id, record_no, src_name, out_name, total_flows, completed_flows, active_flows, completion_rate, date_min, date_max, created_by, kind, created_at FROM flow_visualizations ORDER BY id DESC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 从 DB 读取某条记录的 HTML 内容
flow_viz_get_html <- function(record_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT html_content FROM flow_visualizations WHERE id=%d", as.integer(record_id)))
    if (nrow(r) == 0 || is.na(r$html_content[1])) NULL else r$html_content[1]
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# 从 DB 重新导出 HTML 到 www/flow_viz/（移植后重建可用文件）
# 返回：list(success, out_path)
flow_viz_export_html <- function(record_id) {
  html <- flow_viz_get_html(record_id)
  if (is.null(html)) return(list(success = FALSE, out_path = NULL))
  con <- db_connect()
  out_name <- tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT out_name FROM flow_visualizations WHERE id=%d", as.integer(record_id)))
    if (nrow(r) == 0) paste0("flow_viz_", record_id, ".html") else r$out_name[1]
  }, finally = db_disconnect(con))

  out_dir <- flow_viz_ensure_dir()
  out_path <- file.path(out_dir, out_name)
  con <- file(out_path, "wb")
  writeBin(charToRaw(html), con)
  close(con)
  list(success = TRUE, out_path = out_path)
}

##################
# 流程实例数据表（需求3：Excel 明细存 SQLite，可在流程板块重新生成看板）
##################

# 导入 Excel 明细到数据库（幂等：同一批次删除后重导）
# 返回：list(success, batch_no, count)
flow_monitor_import_excel <- function(src_path, src_name, operator = "系统") {
  if (!requireNamespace("readxl", quietly = TRUE))
    return(list(success = FALSE, message = "缺少 readxl 包"))

  tryCatch({
    df <- readxl::read_excel(src_path)
    need <- c("流程名称", "当前节点", "发起人", "发起时间")
    miss <- setdiff(need, names(df))
    if (length(miss) > 0)
      return(list(success = FALSE, message = paste("Excel 缺少必需列:", paste(miss, collapse = ", "))))

    # 「所属工作流」为流程本体（类型）列，若 Excel 未提供则留空
    has_workflow <- "所属工作流" %in% names(df)
    if (has_workflow) {
      workflow_col <- as.character(df$所属工作流)
    } else {
      workflow_col <- rep("", nrow(df))
    }

    con <- db_connect()
    tryCatch({
      # 生成批次号
      batch_no <- sprintf("FM%s%03d", format(Sys.Date(), "%Y%m%d"),
        dbGetQuery(con, "SELECT COUNT(*) AS n FROM flow_monitor_batches")$n[1] + 1)

      dbExecute(con, sprintf(
        "INSERT INTO flow_monitor_batches (batch_no, src_name, total, created_by, created_at)
         VALUES ('%s','%s',%d,'%s',datetime('now','localtime'))",
        batch_no, gsub("'","''", src_name), nrow(df), gsub("'","''", operator)))
      batch_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]

      # 批量插入明细
      is_done <- as.integer(grepl("结束", df$当前节点))
      n <- nrow(df)
      for (i in seq_len(n)) {
        dbExecute(con, sprintf(
          "INSERT INTO flow_monitor_records (batch_id, flow_name, current_node, initiator, start_time, is_done, flow_type, workflow)
           VALUES (%d,'%s','%s','%s','%s',%d,'%s','%s')",
          batch_id,
          gsub("'","''", df$流程名称[i]),
          gsub("'","''", df$当前节点[i]),
          gsub("'","''", df$发起人[i]),
          gsub("'","''", as.character(df$发起时间[i])),
          is_done[i],
          gsub("'","''", flow_viz_extract_type(df$流程名称)[i]),
          gsub("'","''", workflow_col[i])))
      }
      list(success = TRUE, batch_no = batch_no, batch_id = batch_id, count = n)
    }, finally = { db_disconnect(con) })

  }, error = function(e) list(success = FALSE, message = paste("导入失败:", e$message)))
}

# 获取所有批次
flow_monitor_get_batches <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM flow_monitor_batches ORDER BY id DESC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取某批次的明细数据（还原为聚合所需的 4 列 data.frame）
flow_monitor_get_data <- function(batch_id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf(
      "SELECT flow_name AS 流程名称, current_node AS 当前节点, initiator AS 发起人, start_time AS 发起时间, workflow AS 所属工作流
       FROM flow_monitor_records WHERE batch_id=%d ORDER BY id", as.integer(batch_id)))
    r
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 从 DB 明细重新生成看板 HTML 并写入 www/flow_viz/
# 返回：list(success, html_path, out_name, stats)
flow_monitor_generate_html <- function(batch_id, out_name = NULL) {
  df <- flow_monitor_get_data(batch_id)
  if (nrow(df) == 0) return(list(success = FALSE, message = "该批次无数据"))

  res <- flow_viz_aggregate(df)

  if (is.null(out_name)) {
    out_name <- paste0("flow_monitor_batch_", batch_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  }
  out_dir <- flow_viz_ensure_dir()
  out_path <- file.path(out_dir, out_name)

  con <- file(out_path, "wb")
  writeBin(charToRaw(res$html), con)
  close(con)

  list(success = TRUE, html_path = out_path, out_name = out_name, stats = res$stats)
}

##################
# 流程日志效率图（需求：单个流程实例的状态日志 → 效率图 HTML）
##################

# Excel 日期序列号 → POSIXct（Windows Excel 基准 1899-12-30）
flow_log_conv_time <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0 || is.na(x)) return(NA)
  as.POSIXct(x * 86400, origin = "1899-12-30", tz = "Asia/Shanghai")
}

flow_log_fmt <- function(t) {
  if (length(t) == 0 || is.na(t)) return("")
  format(t, "%Y-%m-%d %H:%M:%S")
}

# 耗时文本 → 小时数（如 "1天8小时3分钟19秒" → 32.06）
flow_log_dur_hours <- function(s) {
  if (is.null(s) || is.na(s) || s == "") return(NA)
  d <- h <- m <- sec <- 0
  if (grepl("天", s)) d <- as.numeric(sub(".*?([0-9]+)天.*", "\\1", s))
  if (grepl("小时", s)) h <- as.numeric(sub(".*?([0-9]+)小时.*", "\\1", s))
  if (grepl("分钟", s)) m <- as.numeric(sub(".*?([0-9]+)分钟.*", "\\1", s))
  if (grepl("秒", s)) sec <- as.numeric(sub(".*?([0-9]+)秒.*", "\\1", s))
  d * 24 + h + m / 60 + sec / 3600
}

# 解析流程日志 Excel（无表头、9 列）
# 返回：list(meta, timeline)
#   meta：流程元信息（标题、总耗时、未完成节点、人次统计等）
#   timeline：每个节点 list(seq, name, recv, op_time, dur, dur_hours, is_done, ops)
flow_log_parse_excel <- function(src_path) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("缺少 readxl 包")

  df <- readxl::read_excel(src_path, col_names = FALSE)
  if (nrow(df) < 26) stop("Excel 内容不完整，缺少流程节点数据")

  getstr <- function(col, row) {
    v <- df[[col]][row]
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  }

  # 流程元信息（第1-22行，第一列）
  # 提取纯数字（如 "36总人次" → "36"）
  extract_num <- function(s) {
    m <- regmatches(s, regexec("([0-9]+)", s))[[1]]
    if (length(m) >= 2) m[2] else s
  }
  meta <- list(
    title       = getstr(1, 1),
    est_done    = flow_log_fmt(flow_log_conv_time(getstr(1, 9))),
    total_dur   = getstr(1, 11),
    undone_nodes = extract_num(getstr(1, 13)),
    total_person = extract_num(getstr(1, 14)),
    done_person = extract_num(getstr(1, 15)),
    undone_person = extract_num(getstr(1, 16)),
    viewed_person = extract_num(getstr(1, 17)),
    unviewed_person = extract_num(getstr(1, 18))
  )

  # 解析节点和操作者（从第26行开始）
  nodes <- list()
  cur_idx <- 0
  for (i in 26:nrow(df)) {
    c2 <- getstr(2, i); c3 <- getstr(3, i); c4 <- getstr(4, i)

    # 节点行：c2 纯数字 + c3 节点名 + c4 含"操作者总计"
    if (grepl("^[0-9]+$", trimws(c2)) && nchar(trimws(c3)) > 0 && grepl("操作者总计", c4)) {
      getn <- function(s, pat) {
        m <- regmatches(s, regexec(pat, s))[[1]]
        if (length(m) >= 2) as.integer(m[2]) else 0
      }
      nodes[[length(nodes) + 1]] <- list(
        seq = as.integer(trimws(c2)),
        name = trimws(c3),
        total = getn(c4, "总计:(\\d+)"),
        done = getn(getstr(5, i), "已操作:.(\\d+)"),
        viewed = getn(getstr(6, i), "已查看:.(\\d+)"),
        undone = getn(getstr(7, i), "未操作:.(\\d+)"),
        unviewed = getn(getstr(8, i), "未查看:.(\\d+)"),
        ops = list()
      )
      cur_idx <- length(nodes)
      next
    }

    # 操作者行：c3 姓名 + c4 状态
    if (cur_idx > 0 && nchar(trimws(c3)) > 0 && !grepl("^[0-9]+$", trimws(c2))) {
      dur <- trimws(getstr(8, i))
      nodes[[cur_idx]]$ops[[length(nodes[[cur_idx]]$ops) + 1]] <- list(
        name = trimws(c3),
        status = trimws(c4),
        recv = flow_log_fmt(flow_log_conv_time(getstr(5, i))),
        view = flow_log_fmt(flow_log_conv_time(getstr(6, i))),
        op_time = flow_log_fmt(flow_log_conv_time(getstr(7, i))),
        dur = if (dur == "NA") "" else dur,
        src = trimws(getstr(9, i))
      )
    }
  }

  if (length(nodes) == 0) stop("未能解析出流程节点")

  # 构建时间线（关键操作 = 批准/提交/转办）
  timeline <- lapply(nodes, function(n) {
    key_ops <- Filter(function(o) grepl("批准|提交|转办", o$status), n$ops)
    is_done <- length(key_ops) > 0
    if (is_done) {
      recv <- min(sapply(key_ops, function(o) o$recv))
      op_time <- max(sapply(key_ops, function(o) o$op_time))
      key_dur <- key_ops[[1]]$dur
      dur_h <- flow_log_dur_hours(key_dur)
    } else {
      recv_times <- sapply(n$ops, function(o) if (o$recv != "") o$recv else o$view)
      recv_times <- recv_times[recv_times != "" & !is.na(recv_times)]
      recv <- if (length(recv_times) > 0) min(recv_times) else ""
      op_time <- ""; key_dur <- ""; dur_h <- NA
    }
    list(seq = n$seq, name = n$name, recv = recv, op_time = op_time,
         dur = key_dur, dur_hours = ifelse(is.na(dur_h), 0, round(dur_h, 2)),
         is_done = is_done, total = n$total, done = n$done, viewed = n$viewed,
         ops = n$ops)
  })

  flow_start <- min(sapply(timeline, function(t) t$recv))
  flow_end <- max(sapply(timeline, function(t) if (t$op_time != "") t$op_time else t$recv))

  list(meta = meta, timeline = timeline, start = flow_start, end = flow_end)
}

# 生成流程效率图 HTML
flow_log_build_html <- function(parsed) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("缺少 jsonlite 包")

  meta <- parsed$meta
  timeline <- parsed$timeline
  flow_start <- parsed$start
  flow_end <- parsed$end

  to_ms <- function(s) {
    if (is.na(s) || s == "") return(NA)
    as.numeric(as.POSIXct(s, tz = "Asia/Shanghai")) * 1000
  }

  gantt <- lapply(timeline, function(t) {
    end_str <- if (t$op_time != "") t$op_time else "2026-08-19 09:45:00"
    list(name = t$name, seq = t$seq, start = t$recv, end = end_str,
         start_ms = to_ms(t$recv), end_ms = to_ms(end_str),
         dur = t$dur, dur_hours = t$dur_hours, is_done = t$is_done)
  })

  done_nodes <- Filter(function(t) t$is_done, timeline)
  bar_data <- lapply(done_nodes, function(t) list(name = t$name, value = t$dur_hours, dur = t$dur))

  detail <- lapply(timeline, function(t) {
    lapply(t$ops, function(o) {
      list(node = t$name, name = o$name, status = o$status, recv = o$recv,
           view = o$view, op_time = o$op_time, dur = o$dur, src = o$src)
    })
  })

  to_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)
  gantt_json <- to_json(gantt)
  bar_json <- to_json(bar_data)
  detail_json <- to_json(detail)

  paste0('<!DOCTYPE html>
<html lang="zh-CN">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>流程效率图 - ', meta$title, '</title>
<script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif; background:linear-gradient(135deg,#1a1a2e 0%,#16213e 100%); min-height:100vh; color:#fff; padding:20px; }
.header { text-align:center; padding:30px 0; margin-bottom:20px; }
.header h1 { font-size:30px; font-weight:700; background:linear-gradient(90deg,#00d4ff,#7b2cbf); -webkit-background-clip:text; -webkit-text-fill-color:transparent; background-clip:text; margin-bottom:10px; }
.header .subtitle { color:#8892b0; font-size:15px; }
.kpi-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(160px,1fr)); gap:16px; margin-bottom:30px; }
.kpi-card { background:rgba(255,255,255,0.05); border-radius:14px; padding:20px; border:1px solid rgba(255,255,255,0.1); }
.kpi-label { color:#8892b0; font-size:13px; margin-bottom:6px; }
.kpi-value { font-size:26px; font-weight:700; color:#fff; }
.kpi-value.warn { color:#ffd700; } .kpi-value.success { color:#00e676; }
.kpi-change { font-size:12px; margin-top:4px; color:#8892b0; }
.chart-card { background:rgba(255,255,255,0.05); border-radius:16px; padding:24px; border:1px solid rgba(255,255,255,0.1); margin-bottom:24px; }
.chart-title { font-size:18px; font-weight:600; margin-bottom:20px; color:#fff; }
.chart-container { width:100%; height:420px; }
.detail-table { width:100%; border-collapse:collapse; margin-top:10px; }
.detail-table th, .detail-table td { padding:8px 10px; text-align:left; border-bottom:1px solid rgba(255,255,255,0.1); font-size:13px; }
.detail-table th { color:#8892b0; font-weight:500; font-size:12px; }
.status-badge { display:inline-block; padding:2px 10px; border-radius:10px; font-size:12px; font-weight:600; }
.status-done { background:rgba(0,230,118,0.15); color:#00e676; }
.status-active { background:rgba(255,215,0,0.15); color:#ffd700; }
.status-view { background:rgba(0,212,255,0.15); color:#00d4ff; }
.node-name { color:#00d4ff; font-weight:600; }
.footer { text-align:center; padding:30px; color:#8892b0; font-size:13px; }
</style>
</head>
<body>
<div class="header">
  <h1>流程效率图</h1>
  <div class="subtitle">', meta$title, '</div>
</div>

<div class="kpi-grid">
  <div class="kpi-card"><div class="kpi-label">预计总耗时</div><div class="kpi-value warn">', meta$total_dur, '</div><div class="kpi-change">预计完成 ', meta$est_done, '</div></div>
  <div class="kpi-card"><div class="kpi-label">未完成节点</div><div class="kpi-value warn">', meta$undone_nodes, '</div><div class="kpi-change">共 ', length(timeline), ' 个节点</div></div>
  <div class="kpi-card"><div class="kpi-label">总人次</div><div class="kpi-value">', meta$total_person, '</div><div class="kpi-change">已操作 ', meta$done_person, ' · 未操作 ', meta$undone_person, '</div></div>
  <div class="kpi-card"><div class="kpi-label">已查看 / 未查看</div><div class="kpi-value">', meta$viewed_person, ' / ', meta$unviewed_person, '</div><div class="kpi-change">查看进度</div></div>
  <div class="kpi-card"><div class="kpi-label">流程周期</div><div class="kpi-value" style="font-size:20px;">', flow_start, '</div><div class="kpi-change">至 ', flow_end, '</div></div>
</div>

<div class="chart-card">
  <div class="chart-title">流程时间线（各节点处理时段）</div>
  <div id="ganttChart" class="chart-container"></div>
</div>

<div class="chart-card">
  <div class="chart-title">节点耗时排行（瓶颈分析）</div>
  <div id="barChart" class="chart-container"></div>
</div>

<div class="chart-card">
  <div class="chart-title">节点操作明细</div>
  <div id="detailTable"></div>
</div>

<div class="footer">数据来源：流程状态日志 | 报表生成：LVCC ITOM | 更新日期：', format(Sys.Date(), "%Y年%m月%d日"), '</div>

<script>
const ganttData = ', gantt_json, ';
const barData = ', bar_json, ';
const detailData = ', detail_json, ';

const ganttChart = echarts.init(document.getElementById("ganttChart"));
const ganttSeries = ganttData.map((g, i) => ({
  name: g.name,
  value: [i, g.start_ms, g.end_ms, g.dur_hours],
  itemStyle: { color: g.is_done ? "#00d4ff" : "#ffd700" }
}));
ganttChart.setOption({
  tooltip: { backgroundColor:"rgba(0,0,0,0.8)", borderColor:"#00d4ff", textStyle:{color:"#fff"},
    formatter: function(p) { const g = ganttData[p.dataIndex]; return "<b>" + g.name + "</b><br/>开始: " + g.start + "<br/>结束: " + (g.is_done ? g.end : "进行中") + "<br/>耗时: " + (g.dur || "—"); } },
  grid: { left: "3%", right: "5%", bottom: "5%", top: "5%", containLabel: true },
  xAxis: { type: "time", axisLine:{lineStyle:{color:"#2d3748"}}, axisLabel:{color:"#8892b0"}, splitLine:{lineStyle:{color:"rgba(255,255,255,0.05)"}} },
  yAxis: { type: "category", data: ganttData.map(g => g.seq + ". " + g.name), axisLine:{lineStyle:{color:"#2d3748"}}, axisLabel:{color:"#c0c8dd", fontSize:11} },
  series: [{ type: "custom", renderItem: function(params, api) {
    const catIndex = api.value(0);
    const start = api.coord([api.value(1), catIndex]);
    const end = api.coord([api.value(2), catIndex]);
    const height = api.size([0, 1])[1] * 0.5;
    return { type: "rect", shape: { x: start[0], y: start[1] - height/2, width: Math.max(end[0] - start[0], 2), height: height },
      style: api.style() };
  }, encode: { x: [1, 2], y: 0 }, data: ganttSeries }]
});

const barChart = echarts.init(document.getElementById("barChart"));
barChart.setOption({
  tooltip: { backgroundColor:"rgba(0,0,0,0.8)", borderColor:"#00d4ff", textStyle:{color:"#fff"},
    formatter: function(p) { const b = barData[p.dataIndex]; return "<b>" + b.name + "</b><br/>耗时: " + b.dur; } },
  grid: { left: "3%", right: "8%", bottom: "3%", containLabel: true },
  xAxis: { type: "value", name: "耗时(小时)", axisLine:{lineStyle:{color:"#2d3748"}}, axisLabel:{color:"#8892b0"}, splitLine:{lineStyle:{color:"rgba(255,255,255,0.05)"}} },
  yAxis: { type: "category", data: barData.map(b => b.name).reverse(), axisLine:{lineStyle:{color:"#2d3748"}}, axisLabel:{color:"#c0c8dd", fontSize:11} },
  series: [{ type: "bar", data: barData.map(b => b.value).reverse(), barWidth: "55%",
    itemStyle: { color: function(p){ return new echarts.graphic.LinearGradient(0,0,1,0,[{offset:0,color:"#7b2cbf"},{offset:1,color:"#00d4ff"}]); } },
    label: { show:true, position:"right", color:"#8892b0", formatter: function(p){ return barData[barData.length-1-p.dataIndex].dur; } } }]
});

const detailHtml = detailData.map(nodeOps => {
  const nodeName = nodeOps[0].node;
  const rows = nodeOps.map(o => {
    const badge = o.status.indexOf("批准") >= 0 || o.status === "提交" ? "status-done" :
                  o.status.indexOf("查看") >= 0 ? "status-view" : "status-active";
    return "<tr><td>" + o.name + "</td><td><span class=\\"status-badge " + badge + "\\">" + o.status + "</span></td><td>" + o.recv + "</td><td>" + o.view + "</td><td>" + o.op_time + "</td><td>" + o.dur + "</td><td>" + o.src + "</td></tr>";
  }).join("");
  return "<div style=\\"margin-bottom:18px;\\"><div class=\\"node-name\\">◆ " + nodeName + "</div><table class=\\"detail-table\\"><thead><tr><th>操作人</th><th>状态</th><th>接收时间</th><th>查看时间</th><th>操作时间</th><th>操作耗时</th><th>来源</th></tr></thead><tbody>" + rows + "</tbody></table></div>";
}).join("");
document.getElementById("detailTable").innerHTML = detailHtml;

window.addEventListener("resize", function() { ganttChart.resize(); barChart.resize(); });
</script>
</body>
</html>')
}

# 主入口：解析流程日志 Excel → 生成效率图 HTML
# 返回：list(success, message, html_path, out_name, stats)
flow_log_generate <- function(src_path, src_name, operator = "系统") {
  if (!requireNamespace("readxl", quietly = TRUE))
    return(list(success = FALSE, message = "缺少 readxl 包，请先 install.packages('readxl')"))

  tryCatch({
    parsed <- flow_log_parse_excel(src_path)
    html <- flow_log_build_html(parsed)

    base <- tools::file_path_sans_ext(src_name)
    safe_base <- gsub("[^0-9A-Za-z\u4e00-\u9fa5_-]+", "_", base)
    # 若文件名已含"流程日志"前缀则不重复添加
    out_name <- paste0(safe_base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    out_dir <- flow_viz_ensure_dir()
    out_path <- file.path(out_dir, out_name)

    con <- file(out_path, "wb")
    writeBin(charToRaw(html), con)
    close(con)

    stats <- list(
      title = parsed$meta$title,
      node_count = length(parsed$timeline),
      done_nodes = sum(sapply(parsed$timeline, function(t) t$is_done)),
      undone_nodes = parsed$meta$undone_nodes,
      total_person = parsed$meta$total_person,
      flow_start = parsed$start,
      flow_end = parsed$end
    )

    list(success = TRUE, message = "生成成功",
         html_path = out_path, out_name = out_name, stats = stats, html_content = html)
  }, error = function(e) {
    list(success = FALSE, message = paste("生成失败:", e$message))
  })
}
