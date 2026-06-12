# 集成模块 UI v2

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
