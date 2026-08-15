# 流程数据可视化模块 — 数据层
# 功能：上传流程监控 Excel → 自动生成 ECharts HTML 看板 → 保存历史记录
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
  df$type    <- sub("-.*", "", df$流程名称)          # 流程类型：第一个 "-" 之前
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

  # ---- 生成 HTML ----
  html <- flow_viz_build_html(
    dailyData_json, flowTypeData_json, blockingNodes_json, initiatorData_json,
    dailyTypeData_json, typeNames_json, typeCompletionData_json,
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
                                typeCompletionData_json, total_flows, completed_flows,
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
    <title>流程监控数据看板</title>
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
        .footer { text-align: center; padding: 30px; color: #8892b0; font-size: 14px; }
        @media (max-width: 1200px) { .chart-grid { grid-template-columns: 1fr; } }
        @media (max-width: 768px) { .kpi-grid { grid-template-columns: 1fr; } .header h1 { font-size: 24px; } .kpi-value { font-size: 24px; } }
    </style>
</head>
<body>
    <div class="header">
        <h1>流程监控数据看板</h1>
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
    <div class="chart-grid"><div class="chart-card full-width"><div class="chart-title">流程类型分布清单 (全部', type_count, '种)</div><div style="max-height:600px;overflow-y:auto;"><table class="ranking-table" id="flowTypeTable"><thead><tr><th style="width:50px;">排名</th><th>流程类型</th><th style="width:70px;">总数</th><th style="width:70px;">已完成</th><th style="width:70px;">进行中</th><th style="width:80px;">完成率</th><th>完成进度</th></tr></thead><tbody></tbody></table></div></div></div>
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
    <div class="footer">数据来源：流程监控导出数据 | 报表生成：LVCC ITOM | 更新日期：', format(Sys.Date(), "%Y年%m月%d日"), '</div>
    <script>
        const dailyData = ', dailyData_json, ';
        const flowTypeData = ', flowTypeData_json, ';
        const blockingNodes = ', blockingNodes_json, ';
        const initiatorData = ', initiatorData_json, ';
        const dailyTypeData = ', dailyTypeData_json, ';
        const typeNames = ', typeNames_json, ';
        const typeCompletionData = ', typeCompletionData_json, ';
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
        flowTypeData.forEach((ft, index) => {
            const row = document.createElement("tr");
            const rankBadge = index < 3 ? "<span class=\\"rank-badge " + ["gold","silver","bronze"][index] + "\\">" + (index+1) + "</span>" : "<span class=\\"rank-badge default\\">" + (index+1) + "</span>";
            const rate = (ft.completed/ft.total*100).toFixed(1);
            const barColor = rate >= 80 ? "#00e676" : rate >= 50 ? "#ffd700" : "#ff5252";
            row.innerHTML = "<td>" + rankBadge + "</td><td>" + ft.name + "</td><td style=\\"font-weight:600;\\">" + ft.total + "</td><td style=\\"color:#00e676;\\">" + ft.completed + "</td><td style=\\"color:#ffd700;\\">" + ft.active + "</td><td style=\\"color:" + barColor + ";font-weight:600;\\">" + rate + "%</td><td><div style=\\"display:flex;align-items:center;gap:8px;\\"><div class=\\"progress-bar-bg\\" style=\\"flex:1;\\"><div class=\\"progress-bar-fill\\" style=\\"width:" + rate + "%;background:" + barColor + ";\\"></div></div></div></td>";
            ftTableBody.appendChild(row);
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
flow_viz_add_record <- function(record_no, src_name, out_name, stats, operator, html_content = NULL) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO flow_visualizations (record_no, src_name, out_name, total_flows, completed_flows, active_flows, completion_rate, date_min, date_max, created_by, html_content, created_at)
       VALUES ('%s','%s','%s',%d,%d,%d,%s,'%s','%s','%s',%s,datetime('now','localtime'))",
      record_no,
      gsub("'","''", src_name),
      gsub("'","''", out_name),
      as.integer(stats$total), as.integer(stats$completed), as.integer(stats$active),
      stats$completion_rate,
      stats$date_min, stats$date_max,
      gsub("'","''", operator),
      if (is.null(html_content)) "NULL" else paste0("'", gsub("'", "''", html_content), "'")))
    TRUE
  }, error = function(e) FALSE, finally = { db_disconnect(con) })
}

# 获取历史记录（不含 html_content，避免大字段拖慢列表）
flow_viz_get_history <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT id, record_no, src_name, out_name, total_flows, completed_flows, active_flows, completion_rate, date_min, date_max, created_by, created_at FROM flow_visualizations ORDER BY id DESC")
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
# 流程监控数据表（需求3：Excel 明细存 SQLite，可在流程板块重新生成看板）
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
      flow_type <- sub("-.*", "", df$流程名称)
      n <- nrow(df)
      for (i in seq_len(n)) {
        dbExecute(con, sprintf(
          "INSERT INTO flow_monitor_records (batch_id, flow_name, current_node, initiator, start_time, is_done, flow_type)
           VALUES (%d,'%s','%s','%s','%s',%d,'%s')",
          batch_id,
          gsub("'","''", df$流程名称[i]),
          gsub("'","''", df$当前节点[i]),
          gsub("'","''", df$发起人[i]),
          gsub("'","''", as.character(df$发起时间[i])),
          is_done[i],
          gsub("'","''", flow_type[i])))
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
      "SELECT flow_name AS 流程名称, current_node AS 当前节点, initiator AS 发起人, start_time AS 发起时间
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
