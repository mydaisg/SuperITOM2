# 生成最终看板 HTML
library(jsonlite)

load(file.path("Test", "dashboard_data.RData"))

data_dir <- "D:/Tai_LVCC_2026/Tai_60_ProjectManagement/01.LVCC_ECS_试运行/流程量数据"
out_file <- file.path(data_dir, "流程实例数据看板_20260814.html")

# 日期显示格式化
fmt_date <- function(s) {
  d <- as.Date(s)
  paste0(format(d, "%Y年%m月%d日"))
}

date_min_fmt <- fmt_date(date_min)
date_max_fmt <- fmt_date(date_max)

html <- paste0('<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>流程实例数据看板</title>
    <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
            min-height: 100vh; color: #fff; padding: 20px;
        }
        .header { text-align: center; padding: 30px 0; margin-bottom: 20px; }
        .header h1 {
            font-size: 36px; font-weight: 700;
            background: linear-gradient(90deg, #00d4ff, #7b2cbf);
            -webkit-background-clip: text; -webkit-text-fill-color: transparent;
            background-clip: text; margin-bottom: 10px;
        }
        .header .subtitle { color: #8892b0; font-size: 16px; }
        .kpi-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }
        .kpi-card {
            background: rgba(255, 255, 255, 0.05); border-radius: 16px; padding: 24px;
            border: 1px solid rgba(255, 255, 255, 0.1); transition: all 0.3s ease;
        }
        .kpi-card:hover { transform: translateY(-5px); border-color: rgba(0, 212, 255, 0.3); box-shadow: 0 10px 40px rgba(0, 212, 255, 0.1); }
        .kpi-label { color: #8892b0; font-size: 14px; margin-bottom: 8px; }
        .kpi-value { font-size: 32px; font-weight: 700; color: #fff; }
        .kpi-value.warn { color: #ffd700; }
        .kpi-value.danger { color: #ff5252; }
        .kpi-value.success { color: #00e676; }
        .kpi-change { font-size: 14px; margin-top: 8px; }
        .kpi-change.up { color: #ff5252; }
        .kpi-change.down { color: #00e676; }
        .chart-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 20px; margin-bottom: 30px; }
        .chart-card {
            background: rgba(255, 255, 255, 0.05); border-radius: 16px; padding: 24px;
            border: 1px solid rgba(255, 255, 255, 0.1);
        }
        .chart-card.full-width { grid-column: 1 / -1; }
        .chart-title { font-size: 18px; font-weight: 600; margin-bottom: 20px; color: #fff; }
        .chart-container { width: 100%; height: 350px; }
        .chart-container-tall { width: 100%; height: 450px; }
        .ranking-table { width: 100%; border-collapse: collapse; margin-top: 10px; }
        .ranking-table th, .ranking-table td { padding: 10px 12px; text-align: left; border-bottom: 1px solid rgba(255, 255, 255, 0.1); }
        .ranking-table th { color: #8892b0; font-weight: 500; font-size: 12px; text-transform: uppercase; }
        .ranking-table td { font-size: 14px; }
        .ranking-table tr:hover { background: rgba(255, 255, 255, 0.05); }
        .rank-badge {
            display: inline-block; width: 28px; height: 28px; line-height: 28px;
            text-align: center; border-radius: 50%; font-weight: 600; font-size: 12px;
        }
        .rank-badge.gold { background: linear-gradient(135deg, #ffd700, #ffb700); color: #000; }
        .rank-badge.silver { background: linear-gradient(135deg, #c0c0c0, #a0a0a0); color: #000; }
        .rank-badge.bronze { background: linear-gradient(135deg, #cd7f32, #b87333); color: #000; }
        .rank-badge.default { background: rgba(255, 255, 255, 0.1); color: #8892b0; }
        .status-dot { display: inline-block; width: 8px; height: 8px; border-radius: 50%; margin-right: 6px; }
        .status-dot.done { background: #00e676; }
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
        <h1>流程实例数据看板</h1>
        <div class="subtitle">数据周期：', date_min_fmt, ' - ', date_max_fmt, ' | 总流程数：', total_flows, '</div>
    </div>

    <div class="kpi-grid">
        <div class="kpi-card">
            <div class="kpi-label">流程总数</div>
            <div class="kpi-value">', total_flows, '</div>
            <div class="kpi-change">', n_days, '天监控周期</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">已完成流程</div>
            <div class="kpi-value success">', completed_flows, '</div>
            <div class="kpi-change down">完成率 ', completion_rate, '%</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">进行中流程</div>
            <div class="kpi-value warn">', active_flows, '</div>
            <div class="kpi-change up">待处理占比 ', round(active_flows/total_flows*100, 1), '%</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">日均发起量</div>
            <div class="kpi-value">', daily_avg_total, '</div>
            <div class="kpi-change">日均完成 ', daily_avg_done, '</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">流程类型数</div>
            <div class="kpi-value">', type_count, '</div>
            <div class="kpi-change">TOP5占比 ', top5_pct, '%</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">参与人数</div>
            <div class="kpi-value">', initiator_count, '</div>
            <div class="kpi-change">发起人总数</div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card full-width">
            <div class="chart-title">每日流程发起与完成趋势</div>
            <div id="dailyTrend" class="chart-container-tall"></div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card full-width">
            <div class="chart-title">流程类型分布清单 (全部', type_count, '种)</div>
            <div style="max-height:600px;overflow-y:auto;">
                <table class="ranking-table" id="flowTypeTable">
                    <thead>
                        <tr><th style="width:50px;">排名</th><th>流程类型</th><th style="width:70px;">总数</th><th style="width:70px;">已完成</th><th style="width:70px;">进行中</th><th style="width:80px;">完成率</th><th>完成进度</th></tr>
                    </thead>
                    <tbody></tbody>
                </table>
            </div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card">
            <div class="chart-title">流程状态仪表盘</div>
            <div id="statusGauge" class="chart-container"></div>
        </div>
        <div class="chart-card">
            <div class="chart-title">流程类型分布饼图 (Top 12 + 其他)</div>
            <div id="flowTypePie" class="chart-container"></div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card full-width">
            <div class="chart-title">阻塞节点分析 (进行中流程当前所在节点)</div>
            <div id="blockingNodes" class="chart-container-tall"></div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card">
            <div class="chart-title">发起人流程数量排名 (Top 15)</div>
            <div id="initiatorRank" class="chart-container-tall"></div>
        </div>
        <div class="chart-card">
            <div class="chart-title">各类型流程完成率</div>
            <div id="typeCompletionRate" class="chart-container-tall"></div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card full-width">
            <div class="chart-title">每日各流程类型发起量堆叠</div>
            <div id="dailyTypeStack" class="chart-container-tall"></div>
        </div>
    </div>

    <div class="chart-grid">
        <div class="chart-card full-width">
            <div class="chart-title">进行中流程阻塞节点详情 (Top 20)</div>
            <table class="ranking-table" id="blockingTable">
                <thead>
                    <tr><th>排名</th><th>阻塞节点</th><th>卡住流程数</th><th>涉及流程类型</th><th>占比</th></tr>
                </thead>
                <tbody></tbody>
            </table>
        </div>
    </div>

    <div class="footer">
        数据来源：流程实例导出数据 | 报表生成：Tai_WorkBuddy | 更新日期：', format(Sys.Date(), "%Y年%m月%d日"), '
    </div>

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

        function formatPct(value, total) {
            return (value / total * 100).toFixed(1) + "%";
        }

        // ==================== 每日趋势图 ====================
        const dailyTrendChart = echarts.init(document.getElementById("dailyTrend"));
        dailyTrendChart.setOption({
            tooltip: {
                trigger: "axis",
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#00d4ff",
                textStyle: { color: "#fff" },
                formatter: function(params) {
                    let result = "<div style=\\"font-weight:600;margin-bottom:8px;\\">" + params[0].axisValue + "</div>";
                    params.forEach(p => {
                        result += "<div style=\\"display:flex;align-items:center;margin:4px 0;\\"><span style=\\"display:inline-block;width:10px;height:10px;background:" + p.color + ";border-radius:50%;margin-right:8px;\\"></span>" + p.seriesName + ": <strong>" + p.value + "</strong></div>";
                    });
                    return result;
                }
            },
            legend: {
                data: ["发起总数", "已完成", "进行中"],
                textStyle: { color: "#8892b0" },
                top: 0
            },
            grid: { left: "3%", right: "4%", bottom: "3%", containLabel: true },
            xAxis: {
                type: "category",
                data: dailyData.dates,
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0" }
            },
            yAxis: {
                type: "value",
                name: "流程数",
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0" },
                splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } }
            },
            series: [
                {
                    name: "发起总数",
                    type: "line",
                    data: dailyData.total,
                    smooth: true,
                    itemStyle: { color: "#00d4ff" },
                    lineStyle: { width: 3 },
                    areaStyle: {
                        color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                            { offset: 0, color: "rgba(0, 212, 255, 0.3)" },
                            { offset: 1, color: "rgba(0, 212, 255, 0)" }
                        ])
                    }
                },
                {
                    name: "已完成",
                    type: "bar",
                    data: dailyData.completed,
                    itemStyle: { color: "rgba(0, 230, 118, 0.7)" },
                    barWidth: "25%"
                },
                {
                    name: "进行中",
                    type: "bar",
                    data: dailyData.active,
                    itemStyle: { color: "rgba(255, 215, 0, 0.7)" },
                    barWidth: "25%"
                }
            ]
        });

        // ==================== 流程类型饼图 ====================
        const flowTypePieChart = echarts.init(document.getElementById("flowTypePie"));
        const top12 = flowTypeData.slice(0, 12);
        const otherTotal = flowTypeData.slice(12).reduce((sum, f) => sum + f.total, 0);
        const otherActive = flowTypeData.slice(12).reduce((sum, f) => sum + f.active, 0);
        const pieColors = ["#00d4ff", "#7b2cbf", "#00e676", "#ffd700", "#ff6b6b", "#ff9f43", "#a55eea", "#54a0ff", "#5f27cd", "#01a3a4", "#f368e0", "#ff6348"];
        const pieData = top12.map((f, i) => ({
            name: f.name, value: f.total, active: f.active,
            itemStyle: { color: pieColors[i % 12] }
        }));
        pieData.push({ name: "其他(" + (flowTypeData.length - 12) + "种)", value: otherTotal, active: otherActive, itemStyle: { color: "#8892b0" } });
        flowTypePieChart.setOption({
            tooltip: {
                trigger: "item",
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#00d4ff",
                textStyle: { color: "#fff" },
                formatter: function(params) {
                    return "<div style=\\"font-weight:600;\\">" + params.name + "</div>" +
                        "<div>总数: " + params.value + "</div>" +
                        "<div>进行中: " + (params.data.active || 0) + "</div>" +
                        "<div>占比: " + params.percent.toFixed(1) + "%</div>";
                }
            },
            legend: {
                orient: "vertical",
                right: "3%",
                top: "center",
                textStyle: { color: "#8892b0", fontSize: 10 },
                formatter: function(name) { return name.length > 8 ? name.substring(0, 8) + ".." : name; }
            },
            series: [{
                type: "pie",
                radius: ["35%", "65%"],
                center: ["35%", "50%"],
                avoidLabelOverlap: false,
                itemStyle: { borderRadius: 8, borderColor: "#1a1a2e", borderWidth: 2 },
                label: { show: false },
                emphasis: {
                    label: { show: true, fontSize: 14, fontWeight: "bold", color: "#fff" },
                    itemStyle: { shadowBlur: 20, shadowColor: "rgba(0, 212, 255, 0.5)" }
                },
                data: pieData
            }]
        });

        // ==================== 状态仪表盘 ====================
        const statusGaugeChart = echarts.init(document.getElementById("statusGauge"));
        statusGaugeChart.setOption({
            tooltip: {
                formatter: "完成率: {c}%",
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#00d4ff",
                textStyle: { color: "#fff" }
            },
            series: [{
                type: "gauge",
                startAngle: 210,
                endAngle: -30,
                center: ["50%", "55%"],
                radius: "85%",
                min: 0,
                max: 100,
                splitNumber: 10,
                axisLine: {
                    show: true,
                    lineStyle: {
                        width: 20,
                        color: [
                            [0.3, "#ff5252"],
                            [0.6, "#ffd700"],
                            [1, "#00e676"]
                        ]
                    }
                },
                pointer: {
                    icon: "path://M12.8,0.7l12,40.1H0.7L12.8,0.7z",
                    length: "60%",
                    width: 8,
                    itemStyle: { color: "auto" }
                },
                axisTick: { length: 10, lineStyle: { color: "auto", width: 2 } },
                splitLine: { length: 20, lineStyle: { color: "auto", width: 4 } },
                axisLabel: { color: "#8892b0", fontSize: 12, distance: 25, formatter: "{value}%" },
                title: { offsetCenter: [0, "80%"], fontSize: 16, color: "#8892b0" },
                detail: {
                    fontSize: 36, offsetCenter: [0, "50%"],
                    valueAnimation: true, formatter: "{value}%",
                    color: "#fff", fontWeight: "bold"
                },
                data: [{ value: completionRate, name: "流程完成率" }]
            }]
        });

        // ==================== 阻塞节点分析 ====================
        const blockingNodesChart = echarts.init(document.getElementById("blockingNodes"));
        const bnReversed = blockingNodes.slice().reverse();
        blockingNodesChart.setOption({
            tooltip: {
                trigger: "axis",
                axisPointer: { type: "shadow" },
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#ffd700",
                textStyle: { color: "#fff" },
                formatter: function(params) {
                    const data = blockingNodes[params[0].dataIndex];
                    return "<div style=\\"font-weight:600;\\">" + data.name + "</div>" +
                        "<div>卡住流程数: <strong>" + data.count + "</strong></div>" +
                        "<div>涉及类型: " + data.types + "</div>";
                }
            },
            grid: { left: "3%", right: "15%", bottom: "3%", containLabel: true },
            xAxis: {
                type: "value",
                name: "卡住流程数",
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0" },
                splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } }
            },
            yAxis: {
                type: "category",
                data: bnReversed.map(n => n.name.length > 10 ? n.name.substring(0, 10) + ".." : n.name),
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0", fontSize: 11 }
            },
            series: [{
                type: "bar",
                data: bnReversed.map((n, i) => ({
                    value: n.count,
                    itemStyle: {
                        color: new echarts.graphic.LinearGradient(0, 0, 1, 0, [
                            { offset: 0, color: i < 3 ? "#ff5252" : i < 6 ? "#ffd700" : "#ff9f43" },
                            { offset: 1, color: i < 3 ? "#cc0000" : i < 6 ? "#cc9900" : "#cc6600" }
                        ])
                    }
                })),
                barWidth: "60%",
                label: { show: true, position: "right", color: "#8892b0", fontSize: 12, formatter: "{c}" }
            }]
        });

        // ==================== 发起人排名 ====================
        const initiatorRankChart = echarts.init(document.getElementById("initiatorRank"));
        initiatorRankChart.setOption({
            tooltip: {
                trigger: "axis",
                axisPointer: { type: "shadow" },
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#00d4ff",
                textStyle: { color: "#fff" },
                formatter: function(params) {
                    const data = initiatorData[params[0].dataIndex];
                    return "<div style=\\"font-weight:600;\\">" + data.name + "</div>" +
                        "<div>总流程: " + data.total + "</div>" +
                        "<div>进行中: <span style=\\"color:#ffd700\\">" + data.active + "</span></div>" +
                        "<div>已完成: <span style=\\"color:#00e676\\">" + data.completed + "</span></div>";
                }
            },
            legend: {
                data: ["已完成", "进行中"],
                textStyle: { color: "#8892b0" },
                top: 0
            },
            grid: { left: "3%", right: "10%", bottom: "3%", containLabel: true },
            xAxis: {
                type: "value",
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0" },
                splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } }
            },
            yAxis: {
                type: "category",
                data: initiatorData.slice().reverse().map(o => o.name),
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0", fontSize: 12 }
            },
            series: [
                {
                    name: "已完成",
                    type: "bar",
                    stack: "total",
                    data: initiatorData.slice().reverse().map(o => o.completed),
                    itemStyle: { color: "rgba(0, 230, 118, 0.7)" },
                    barWidth: "50%"
                },
                {
                    name: "进行中",
                    type: "bar",
                    stack: "total",
                    data: initiatorData.slice().reverse().map(o => o.active),
                    itemStyle: { color: "rgba(255, 215, 0, 0.7)" },
                    barWidth: "50%"
                }
            ]
        });

        // ==================== 各类型完成率 ====================
        const typeCompletionRateChart = echarts.init(document.getElementById("typeCompletionRate"));
        const tcData = typeCompletionData.slice().reverse();
        const avgRate = completionRate;
        typeCompletionRateChart.setOption({
            tooltip: {
                trigger: "axis",
                axisPointer: { type: "shadow" },
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#00d4ff",
                textStyle: { color: "#fff" },
                formatter: function(params) {
                    const data = tcData[params[0].dataIndex];
                    const rate = (data.completed / data.total * 100).toFixed(1);
                    return "<div style=\\"font-weight:600;\\">" + data.name + "</div>" +
                        "<div>完成率: <strong>" + rate + "%</strong></div>" +
                        "<div>已完成: " + data.completed + " / 总数: " + data.total + "</div>";
                }
            },
            grid: { left: "3%", right: "10%", bottom: "3%", containLabel: true },
            xAxis: {
                type: "value",
                max: 100,
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0", formatter: "{value}%" },
                splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } }
            },
            yAxis: {
                type: "category",
                data: tcData.map(f => f.name.length > 10 ? f.name.substring(0, 10) + ".." : f.name),
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0", fontSize: 10 }
            },
            series: [{
                type: "bar",
                data: tcData.map(f => ({
                    value: (f.completed / f.total * 100).toFixed(1),
                    itemStyle: {
                        color: (f.completed / f.total) >= 0.8 ? "#00e676" :
                               (f.completed / f.total) >= 0.5 ? "#ffd700" : "#ff5252"
                    }
                })),
                barWidth: "60%",
                label: { show: true, position: "right", color: "#8892b0", fontSize: 11, formatter: "{c}%" },
                markLine: {
                    silent: true,
                    data: [{ xAxis: avgRate, label: { formatter: "平均 " + avgRate + "%", color: "#8892b0" }, lineStyle: { color: "#00d4ff", type: "dashed" } }]
                }
            }]
        });

        // ==================== 每日各类型堆叠图 ====================
        const dailyTypeStackChart = echarts.init(document.getElementById("dailyTypeStack"));
        const stackColors = ["#00d4ff", "#7b2cbf", "#00e676", "#ffd700", "#ff6b6b", "#ff9f43", "#a55eea", "#54a0ff", "#5f27cd", "#01a3a4", "#f368e0", "#ff6348", "#8892b0"];
        dailyTypeStackChart.setOption({
            tooltip: {
                trigger: "axis",
                backgroundColor: "rgba(0,0,0,0.8)",
                borderColor: "#00d4ff",
                textStyle: { color: "#fff" }
            },
            legend: {
                data: typeNames,
                textStyle: { color: "#8892b0", fontSize: 10 },
                top: 0,
                type: "scroll"
            },
            grid: { left: "3%", right: "4%", bottom: "3%", top: "15%", containLabel: true },
            xAxis: {
                type: "category",
                data: dailyData.dates,
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0" }
            },
            yAxis: {
                type: "value",
                name: "流程数",
                axisLine: { lineStyle: { color: "#2d3748" } },
                axisLabel: { color: "#8892b0" },
                splitLine: { lineStyle: { color: "rgba(255,255,255,0.05)" } }
            },
            series: typeNames.map((name, i) => ({
                name: name,
                type: "bar",
                stack: "total",
                data: dailyTypeData[name],
                emphasis: { focus: "series" },
                itemStyle: { color: stackColors[i % 13] }
            }))
        });

        // ==================== 流程类型完整清单表格 ====================
        const ftTableBody = document.querySelector("#flowTypeTable tbody");
        flowTypeData.forEach((ft, index) => {
            const row = document.createElement("tr");
            const rankBadge = index < 3
                ? "<span class=\\"rank-badge " + ["gold", "silver", "bronze"][index] + "\\">" + (index + 1) + "</span>"
                : "<span class=\\"rank-badge default\\">" + (index + 1) + "</span>";
            const rate = (ft.completed / ft.total * 100).toFixed(1);
            const barColor = rate >= 80 ? "#00e676" : rate >= 50 ? "#ffd700" : "#ff5252";
            row.innerHTML = "<td>" + rankBadge + "</td>" +
                "<td>" + ft.name + "</td>" +
                "<td style=\\"font-weight:600;\\">" + ft.total + "</td>" +
                "<td style=\\"color:#00e676;\\">" + ft.completed + "</td>" +
                "<td style=\\"color:#ffd700;\\">" + ft.active + "</td>" +
                "<td style=\\"color:" + barColor + ";font-weight:600;\\">" + rate + "%</td>" +
                "<td>" +
                    "<div style=\\"display:flex;align-items:center;gap:8px;\\">" +
                        "<div class=\\"progress-bar-bg\\" style=\\"flex:1;\\"><div class=\\"progress-bar-fill\\" style=\\"width:" + rate + "%;background:" + barColor + ";\\"></div></div>" +
                    "</div>" +
                "</td>";
            ftTableBody.appendChild(row);
        });

        // ==================== 阻塞节点表格 ====================
        const tableBody = document.querySelector("#blockingTable tbody");
        blockingNodes.slice(0, 20).forEach((node, index) => {
            const row = document.createElement("tr");
            const rankBadge = index < 3
                ? "<span class=\\"rank-badge " + ["gold", "silver", "bronze"][index] + "\\">" + (index + 1) + "</span>"
                : "<span class=\\"rank-badge default\\">" + (index + 1) + "</span>";
            const pct = (node.count / activeFlows * 100).toFixed(1);
            const barColor = index < 3 ? "#ff5252" : index < 6 ? "#ffd700" : "#ff9f43";
            row.innerHTML = "<td>" + rankBadge + "</td>" +
                "<td><span class=\\"status-dot active\\"></span>" + node.name + "</td>" +
                "<td style=\\"color: " + barColor + ";font-weight:600;\\">" + node.count + "</td>" +
                "<td style=\\"font-size:12px;color:#8892b0;\\">" + node.types + "</td>" +
                "<td>" +
                    "<div style=\\"display:flex;align-items:center;gap:8px;\\">" +
                        "<span>" + pct + "%</span>" +
                        "<div class=\\"progress-bar-bg\\" style=\\"flex:1;\\"><div class=\\"progress-bar-fill\\" style=\\"width:" + pct + "%;background:" + barColor + ";\\"></div></div>" +
                    "</div>" +
                "</td>";
            tableBody.appendChild(row);
        });

        // ==================== 响应式 ====================
        window.addEventListener("resize", function() {
            dailyTrendChart.resize();
            flowTypePieChart.resize();
            statusGaugeChart.resize();
            blockingNodesChart.resize();
            initiatorRankChart.resize();
            typeCompletionRateChart.resize();
            dailyTypeStackChart.resize();
        });
    </script>
</body>
</html>
')

# 直接按 UTF-8 字节写入，避免编码转换
con <- file(out_file, "wb")
writeBin(charToRaw(html), con)
close(con)
cat("已生成看板:", out_file, "\n")
cat("文件大小:", file.info(out_file)$size, "字节\n")
