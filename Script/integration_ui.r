# 集成模块 UI v3

integration_ui <- function() {
  tagList(
    tags$style(HTML("
      .int-card { background:white; border-radius:8px; padding:10px 12px; margin-bottom:6px; border:1px solid #e8ecf1; cursor:pointer; transition:all 0.15s; }
      .int-card:hover { border-color:#0891b2; box-shadow:0 1px 6px rgba(8,145,178,0.12); }
      .int-card.active { border-color:#0891b2; background:#f0fdfa; border-left:3px solid #0891b2; }
      .int-card .int-name { font-size:13px; font-weight:600; color:#1a2236; }
      .int-card .int-desc { font-size:10px; color:#8899aa; margin-top:2px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
      .int-result { background:#1e1e1e; color:#d4d4d4; border-radius:6px; padding:12px; font-family:Consolas,monospace; font-size:12px; max-height:400px; overflow-y:auto; white-space:pre-wrap; word-break:break-all; }
      .int-status-ok { color:#4caf50; font-weight:bold; }
      .int-status-err { color:#e53e3e; font-weight:bold; }
      .int-url-bar { font-size:11px; color:#666; padding:6px 10px; background:#f5f5f5; border-radius:4px; margin-bottom:8px; word-break:break-all; }
      /* 大屏卡片样式 */
      .bigscreen-grid { display:grid; grid-template-columns:repeat(auto-fill, minmax(200px, 1fr)); gap:12px; margin-bottom:12px; }
      .bigscreen-card { background:linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius:12px; padding:18px 16px; color:white; box-shadow:0 4px 15px rgba(0,0,0,0.15); transition:transform 0.2s; }
      .bigscreen-card:hover { transform:translateY(-2px); }
      .bigscreen-card.c1 { background:linear-gradient(135deg, #667eea 0%, #764ba2 100%); }
      .bigscreen-card.c2 { background:linear-gradient(135deg, #f093fb 0%, #f5576c 100%); }
      .bigscreen-card.c3 { background:linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); }
      .bigscreen-card.c4 { background:linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); color:#1a2236; }
      .bigscreen-card.c5 { background:linear-gradient(135deg, #fa709a 0%, #fee140 100%); color:#1a2236; }
      .bigscreen-card.c6 { background:linear-gradient(135deg, #a18cd1 0%, #fbc2eb 100%); color:#1a2236; }
      .bigscreen-card.c7 { background:linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%); color:#1a2236; }
      .bigscreen-card.c8 { background:linear-gradient(135deg, #89f7fe 0%, #66a6ff 100%); }
      .bigscreen-card.c9 { background:linear-gradient(135deg, #fddb92 0%, #d1fdff 100%); color:#1a2236; }
      .bigscreen-card.c10 { background:linear-gradient(135deg, #a1c4fd 0%, #c2e9fb 100%); color:#1a2236; }
      .bigscreen-card.c11 { background:linear-gradient(135deg, #d4fc79 0%, #96e6a1 100%); color:#1a2236; }
      .bigscreen-card .bs-value { font-size:28px; font-weight:800; letter-spacing:1px; }
      .bigscreen-card .bs-label { font-size:12px; opacity:0.85; margin-top:6px; }
      .bigscreen-card .bs-sub { font-size:10px; opacity:0.7; margin-top:2px; }
      .bigscreen-header { display:flex; justify-content:space-between; align-items:center; margin-bottom:10px; }
      .bigscreen-header h5 { margin:0; font-size:15px; }
      .bigscreen-refresh { font-size:10px; color:#999; }
    ")),
    # 数据推送到独立大屏页 + 响应大屏页的数据请求
    tags$script(HTML("
      var bigscreen_cached_data = null;

      // Shiny 刷新数据时缓存并推送
      Shiny.addCustomMessageHandler('bigscreen_push', function(data) {
        bigscreen_cached_data = data;
        // 发给本页内的 iframe
        var iframe = document.querySelector('iframe[src*=\"bigscreen\"]');
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage({ type: 'bigscreen_push', payload: data }, '*');
        }
      });

      // 监听大屏页的数据请求（来自 iframe 或独立窗口）
      window.addEventListener('message', function(e) {
        if (e.data && e.data.type === 'bigscreen_fetch') {
          if (bigscreen_cached_data) {
            e.source.postMessage({ type: 'bigscreen_data', payload: bigscreen_cached_data }, '*');
          } else {
            // 没有缓存数据，触发一次 Shiny 刷新
            Shiny.setInputValue('integ_bigscreen_refresh', Math.random(), {priority: 'event'});
            // 等1秒后重试
            setTimeout(function() {
              if (bigscreen_cached_data) {
                e.source.postMessage({ type: 'bigscreen_data', payload: bigscreen_cached_data }, '*');
              }
            }, 1500);
          }
        }
      });
    ")),
    fluidPage(
      h4(icon("plug"), " 集成管理", style="margin-bottom:10px;"),
      fluidRow(
        column(3,
          wellPanel(
            tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
              h5("配置列表", style="margin:0;"),
              actionButton("integ_add_btn", NULL, icon=icon("plus"), class="btn-xs btn-primary")
            ),
            div(style="max-height:500px; overflow-y:auto;", uiOutput("integ_config_list"))
          )
        ),
        column(9,
          # 大屏：始终可见
          tags$div(class="bigscreen-header",
            h5(icon("tachometer-alt"), " 实时数据大屏"),
            tags$span(class="bigscreen-refresh",
              actionButton("integ_bigscreen_refresh", "刷新", icon=icon("sync"), class="btn-xs btn-info"),
              textOutput("integ_bigscreen_time", inline=TRUE),
              tags$a(href="www/bigscreen.html", target="_blank",
                class="btn btn-xs btn-warning", style="margin-left:8px;",
                icon("external-link"), " 独立大屏")
            )
          ),
          uiOutput("integ_bigscreen_cards"),
          # K线图
          wellPanel(
            h5(icon("chart-line"), " 历史趋势 (K线图)", style="margin-top:0;"),
            plotlyOutput("integ_bigscreen_chart", height = "350px")
          ),
          # 历史明细数据表
          wellPanel(
            h5(icon("table"), " 历史明细", style="margin-top:0;"),
            DT::dataTableOutput("integ_bigscreen_table")
          ),
          tags$hr(),
          # 调试面板：选了配置才显示
          conditionalPanel("output.integ_selected == '1'",
                wellPanel(
                  tags$div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
                    h5("请求", style="margin:0;"),
                    tags$div(
                      actionButton("integ_edit_btn", NULL, icon=icon("edit"), class="btn-xs btn-warning", title="编辑"),
                      actionButton("integ_del_btn", NULL, icon=icon("trash"), class="btn-xs btn-danger", title="删除")
                    )
                  ),
                  tags$div(class="int-url-bar",
                    tags$b(textOutput("integ_method", inline=TRUE)), "  ",
                    textOutput("integ_url", inline=TRUE)),
                  textAreaInput("integ_json_input", NULL, rows=6, width="100%",
                    placeholder='{"key":"value"}'),
                  div(style="text-align:right; margin-bottom:8px;",
                    actionButton("integ_exec_btn", "▶ 执行", icon=icon("play"), class="btn-success btn-sm"))
                ),
                wellPanel(
                  h5("响应", style="margin-top:0;"),
                  tags$div(style="font-size:11px;margin-bottom:4px;",
                    "状态: ", textOutput("integ_resp_status", inline=TRUE), " | ",
                    "耗时: ", textOutput("integ_resp_time", inline=TRUE)),
                  tags$div(class="int-result", textOutput("integ_resp_body"))
                )
          ),
          conditionalPanel("output.integ_selected != '1'",
            wellPanel(
              p(style="color:#999; text-align:center; padding:60px 0;",
                icon("plug"), " 选择左侧配置开始调试")
            )
          )
        )
      )
    )
  )
}
