# 工具下拉菜单模块 UI
# 合并「巡检、测试、性能、工具」4 个模块为一个 navbarMenu「工具」下拉菜单
# 共 14 个子页：巡检、测试、性能 + 原工具 11 个 tab
# 供 main_ui.r 调用，放在「可视化」后、「管理」前

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("navbarMenu", "tabPanel", "fluidPage", "sidebarLayout", "sidebarPanel",
                          "mainPanel", "textInput", "selectInput", "actionButton", "icon", "tagList",
                          "tags", "div", "h4", "h5", "titlePanel", "fluidRow", "column", "hidden",
                          "textAreaInput", "wellPanel", "br", "strong", "conditionalPanel", "checkboxInput",
                          "dateInput", "radioButtons", "numericInput", "textOutput", "verbatimTextOutput", "htmlOutput", "uiOutput",
                          "DTOutput", "HTML", "network_test_ui", "sysmon_ui", "integration_ui",
                          "std_ui", "ai_ui", "flow_viz_ui", "img2pdf_ui", "data_import_ui"))
}

tools_menu_ui <- function(can_access = function(x) TRUE) {
  navbarMenu(
    "工具",
    icon = icon("wrench"),

    # ── 1. 巡检 ──
    if (can_access("巡检")) tabPanel(
      "巡检",
      icon = icon("clipboard-check"),
      fluidPage(
        # 巡检统计数据
        fluidRow(
          column(12,
            div(style = "margin-bottom: 10px;",
              fluidRow(
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0;",
                  div(style = "font-size: 11px; color: #666; font-weight: 500;", "巡检计划"),
                  div(style = "font-size: 18px; font-weight: bold; color: #333;", textOutput("insp_stat_plans"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #5cb85c; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "进行中"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_active_plans"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #f0ad4e; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "待执行"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_pending_tasks"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #5bc0de; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "已完成"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_completed_tasks"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #d9534f; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "异常"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_abnormal_tasks"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #9370db; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "待整改"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_issues"))
                ))
              )
            )
          )
        ),
        # 巡检标签页内容
        tabsetPanel(
          # 我的任务
          tabPanel("我的任务",
            br(),
            fluidRow(
              column(3, selectInput("insp_my_status_filter", "任务状态", choices = NULL)),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_my_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("insp_my_task_table")
          ),
          # 巡检计划
          tabPanel("巡检计划",
            br(),
            fluidRow(
              column(2, selectInput("insp_plan_status_filter", "计划状态", choices = NULL)),
              column(3, div(style = "margin-top: 20px;", actionButton("insp_create_plan", "创建计划", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;"))),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_plan_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("inspection_plan_table"),
            br(),
            wellPanel(
              h4("生成巡检任务"),
              fluidRow(
                column(3, selectInput("insp_task_inspector", "检查人", choices = NULL)),
                column(3, dateInput("insp_task_date", "计划日期", value = Sys.Date(), format = "yyyy-mm-dd")),
                column(3, div(style = "margin-top: 20px;", actionButton("insp_generate_tasks", "生成任务", class = "btn-success", style = "padding: 4px 10px; font-size: 12px;")))
              )
            ),
            br(),
            fluidRow(
              column(12,
                h4("该计划下的巡检任务"),
                selectInput("insp_task_status_filter", "任务状态筛选", choices = NULL),
                DTOutput("inspection_task_table")
              )
            )
          ),
          # 巡检记录
          tabPanel("巡检记录",
            br(),
            fluidRow(
              column(3, selectInput("insp_record_status_filter", "任务状态", choices = NULL)),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_record_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("insp_record_table")
          ),
          # 巡检异常
          tabPanel("巡检异常",
            br(),
            fluidRow(
              column(3, selectInput("insp_issue_status_filter", "异常状态", choices = NULL)),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_issue_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("insp_issue_table")
          ),
          # 已删除记录（Admin专属）
          tabPanel("已删除记录",
            br(),
            # Admin 可见内容
            conditionalPanel(
              condition = "input.isAdminInspectionUser == true",
              fluidRow(
                column(12,
                  div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
                    strong("提示："), "此页面仅Admin可见，显示已删除的巡检计划和记录，可用于审计追溯。"
                  )
                )
              ),
              fluidRow(
                column(6,
                  wellPanel(
                    h4("已删除的巡检计划", style = "color: #d9534f;"),
                    DTOutput("insp_deleted_plans_table")
                  )
                ),
                column(6,
                  wellPanel(
                    h4("已删除的巡检记录", style = "color: #d9534f;"),
                    DTOutput("insp_deleted_records_table")
                  )
                )
              )
            ),
            # 非Admin提示
            conditionalPanel(
              condition = "input.isAdminInspectionUser != true",
              div(class = "alert alert-warning",
                icon("exclamation-triangle"), " 您没有权限查看已删除记录"
              )
            )
          )
        )
      )
    ),

    # ── 2. 测试（网络巡检）──
    if (can_access("测试")) tabPanel(
      "测试",
      icon = icon("network-wired"),
      network_test_ui()
    ),

    # ── 3. 性能监控 ──
    if (can_access("性能")) tabPanel(
      "性能",
      icon = icon("heartbeat"),
      sysmon_ui()
    ),

    # ── 4. 文本格式化 ──
    tabPanel("文本格式化",
      icon = icon("align-left"),
      fluidRow(
        column(6,
          h5("输入"),
          textAreaInput("tool_text_in", NULL, width = "100%", rows = 15,
            placeholder = "每行一条文本..."),
          div(style = "display:flex; gap:8px; align-items:center;",
            selectInput("tool_sep", "分隔符", choices = c("逗号 ," = ",", "分号 ;" = ";", "空格" = " ", "逗号+空格" = ", ", "分号+空格" = "; "), width = "160px"),
            checkboxInput("tool_quote", "加引号", FALSE),
            actionButton("tool_format_btn", "格式化 →", icon = icon("arrow-right"), class = "btn-primary"),
            actionButton("tool_clear_btn", "清空", icon = icon("trash"), class = "btn-sm btn-default"),
            actionButton("tool_reverse_btn", "反向(行转列)", icon = icon("exchange"), class = "btn-sm btn-info"),
            actionButton("tool_addnum_btn", "加序号", icon = icon("list-ol"), class = "btn-sm btn-success"),
            actionButton("tool_delnum_btn", "去序号", icon = icon("list-ul"), class = "btn-sm btn-default"),
            actionButton("tool_nospc_btn", "去空格", icon = icon("compress"), class = "btn-sm btn-default"),
            actionButton("tool_prefix_btn", "加前缀", icon = icon("plus-square"), class = "btn-sm btn-default"),
            actionButton("tool_merge_btn", "奇偶合并", icon = icon("object-group"), class = "btn-sm btn-info"),
            actionButton("tool_dot2plus_btn", "● → +", icon = icon("circle"), class = "btn-sm btn-warning"),
            actionButton("tool_num2plus_btn", "1、 → +", icon = icon("hashtag"), class = "btn-sm btn-warning")
          )
        ),
        column(6,
          h5("输出"),
          tags$div(style = "position:relative;",
            verbatimTextOutput("tool_text_out"),
            tags$button(class = "btn btn-xs", style = "position:absolute; top:4px; right:8px;",
              onclick = "var t=document.getElementById('tool_text_out'); if(t){navigator.clipboard.writeText(t.innerText); this.textContent='已复制'; setTimeout(function(){this.textContent='复制'}.bind(this),1500)}", "复制")
          )
        )
      )
    ),

    # ── 5. 拼音 ──
    tabPanel("拼音",
      icon = icon("font"),
      tags$script(HTML("
        window.__loadPinyinPro = function(cb) {
          if (window.pinyinPro) { if (cb) cb(); return; }
          var s = document.createElement('script');
          s.src = 'https://unpkg.com/pinyin-pro@3';
          s.async = true;
          s.onload = function() { if (cb) cb(); };
          s.onerror = function() { if (cb) cb(); };
          document.head.appendChild(s);
        };
      ")),
      tags$style(HTML("
        .py-above-wrap { font-family: 'Microsoft YaHei', 'PingFang SC', sans-serif; }
        .py-above-line { display: flex; flex-wrap: wrap; gap: 2px 4px; margin-bottom: 4px; }
        .py-item { display: inline-flex; flex-direction: column; align-items: center; justify-content: flex-end; min-width: 1.2em; padding: 2px 3px; border-radius: 3px; background: #f8f9fa; }
        .py-pinyin { font-size: 13px; color: #0d47a1; line-height: 1.4; white-space: nowrap; }
        .py-char { font-size: 16px; color: #222; line-height: 1.4; margin-top: 2px; }
        #tool_py_out { background: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 8px; min-height: 260px; font-size: 14px; }
        #tool_py_out .py-block { margin-bottom: 2px; }
        #tool_py_out pre { margin: 0 !important; padding: 0 !important; background: transparent !important; border: none !important; }
      ")),
      tags$script(HTML("
        function doPinyinCore(mode) {
          var inp = document.getElementById('tool_py_in');
          if (!inp || typeof pinyinPro === 'undefined') return;
          var txt = inp.value;
          var result = '';
          if (mode === 'above') {
            var lines = txt.split('\\n');
            var out = [];
            for (var li = 0; li < lines.length; li++) {
              var line = lines[li];
              var py3 = pinyinPro.pinyin(line, { toneType: 'symbol', type: 'array' });
              var chars3 = line.split('');
              var items = [];
              for (var i = 0; i < chars3.length; i++) {
                var c = chars3[i];
                var p = (i < py3.length && /[\\u4e00-\\u9fff]/.test(c)) ? py3[i] : (c.trim() ? c : ' ');
                items.push('<div class=\"py-item\"><div class=\"py-pinyin\">' + p + '</div><div class=\"py-char\">' + c + '</div></div>');
              }
              out.push('<div class=\"py-above-line\">' + items.join('') + '</div>');
            }
            result = '<div class=\"py-above-wrap\">' + out.join('') + '</div>';
          } else if (mode === 'pure') {
            var lines = txt.split('\\n');
            result = lines.map(function(line) {
              return pinyinPro.pinyin(line, { toneType: 'symbol', type: 'array' }).join(' ');
            }).join('\\n');
          } else if (mode === 'num') {
            var lines = txt.split('\\n');
            result = lines.map(function(line) {
              return pinyinPro.pinyin(line, { toneType: 'num', type: 'array' }).join(' ');
            }).join('\\n');
          } else if (mode === 'char') {
            var lines = txt.split('\\n');
            result = lines.map(function(line) {
              var py = pinyinPro.pinyin(line, { toneType: 'symbol', type: 'array' });
              var chars = line.split('');
              return chars.map(function(c,i) { return i < py.length ? py[i] : c; }).join(' ');
            }).join('\\n');
          } else if (mode === 'mixed') {
            var lines = txt.split('\\n');
            result = lines.map(function(line) {
              var py2 = pinyinPro.pinyin(line, { toneType: 'symbol', type: 'array' });
              var chars2 = line.split('');
              return chars2.map(function(c,i) {
                if (/[\\u4e00-\\u9fff]/.test(c) && i < py2.length) return c + '(' + py2[i] + ')';
                return c;
              }).join('');
            }).join('\\n');
          }
          Shiny.setInputValue('tool_py_result', {mode: mode, html: result}, {priority: 'event'});
        }
        Shiny.addCustomMessageHandler('doPinyin', function(mode) {
          window.__loadPinyinPro(function() { doPinyinCore(mode); });
        });
      ")),
      fluidRow(
        column(6,
          h5("输入中文"),
          textAreaInput("tool_py_in", NULL, width = "100%", rows = 15,
            placeholder = "输入中文文本..."),
          div(style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-top:8px;",
            actionButton("tool_py_above", "上部拼音", icon = icon("text-height"), class = "btn-primary btn-sm"),
            actionButton("tool_py_pure", "纯拼音 sān bǎi", icon = icon("font"), class = "btn-info btn-sm"),
            actionButton("tool_py_num", "数字调 san1 bai3", icon = icon("sort-numeric-down"), class = "btn-info btn-sm"),
            actionButton("tool_py_char", "逐字", icon = icon("align-justify"), class = "btn-success btn-sm"),
            actionButton("tool_py_mixed", "混合 三(sān)", icon = icon("italic"), class = "btn-warning btn-sm")
          ),
          div(style = "display:flex; gap:4px; margin-top:6px;",
            actionButton("tool_py_clear_in", "清空输入", icon = icon("eraser"), class = "btn-sm btn-default"),
            actionButton("tool_py_clear_out", "清空输出", icon = icon("trash"), class = "btn-sm btn-default")
          )
        ),
        column(6,
          h5("输出"),
          tags$div(style = "position:relative;",
            htmlOutput("tool_py_out"),
            tags$button(class = "btn btn-xs", style = "position:absolute; top:4px; right:8px;",
              onclick = "var t=document.getElementById('tool_py_out'); if(t){navigator.clipboard.writeText(t.innerText); this.textContent='已复制'; setTimeout(function(){this.textContent='复制'}.bind(this),1500)}", "复制")
          )
        )
      )
    ),

    # ── 6. 收集器 ──
    tabPanel("收集器",
      icon = icon("download"),
      fluidPage(
        titlePanel(""),
        sidebarLayout(
          sidebarPanel(
            textInput("collector_name", "收集器名称"),
            selectInput("collector_type", "收集器类型", choices = c("系统信息", "网络信息", "应用信息", "数据库信息")),
            textAreaInput("collector_config", "收集器配置"),
            tags$button(id="add_collector", type="button", class="btn btn-primary action-button", disabled=NA, "添加收集器"),
            br(), br(),
            actionButton("refresh_collectors", "刷新收集器", class = "btn-info")
          ),
          mainPanel(
            DTOutput("collector_table")
          )
        )
      )
    ),

    # ── 7. 集成 ──
    tabPanel("集成",
      icon = icon("plug"),
      integration_ui()
    ),

    # ── 8. 标准化 ──
    tabPanel("标准化",
      icon = icon("cogs"),
      std_ui()
    ),

    # ── 9. AI ──
    tabPanel("AI",
      icon = icon("robot"),
      ai_ui()
    ),

    # ── 10. 记算 ──
    tabPanel("记算",
      icon = icon("calculator"),
      fluidRow(
        column(6,
          h5("输入数据（每行一组数字，支持负数、加减乘除）"),
          textAreaInput("tool_calc_in", NULL, width = "100%", rows = 8,
            placeholder = "每行输入数字，空格或换行分隔\n如：\n100 200 300\n50 -20\n5*8+10"),
          div(style = "display:flex; gap:4px; flex-wrap:wrap;",
            actionButton("tool_calc_sum", "求和 Σ", icon = icon("plus"), class = "btn-primary btn-sm"),
            actionButton("tool_calc_avg", "平均", icon = icon("chart-bar"), class = "btn-info btn-sm"),
            actionButton("tool_calc_mul", "连乘 ∏", icon = icon("times"), class = "btn-warning btn-sm"),
            actionButton("tool_calc_max", "最大值", class = "btn-default btn-sm"),
            actionButton("tool_calc_min", "最小值", class = "btn-default btn-sm"),
            actionButton("tool_calc_count", "计数", class = "btn-default btn-sm"),
            actionButton("tool_calc_direct", "直接算", icon = icon("equals"), class = "btn-success btn-sm"),
            actionButton("tool_calc_clear", "清空", icon = icon("trash"), class = "btn-danger btn-sm")
          )
        ),
        column(6,
          h5("计算结果"),
          verbatimTextOutput("tool_calc_out"),
          tags$hr(style = "margin:6px 0;"),
          div(style = "display:flex; gap:4px; align-items:center;",
            tags$b("参与下次计算：", style = "font-size:12px;"),
            actionButton("tool_calc_reuse", "继续加", icon = icon("redo"), class = "btn-success btn-xs"),
            actionButton("tool_calc_reset", "重置", class = "btn-default btn-xs")
          ),
          tags$hr(style = "margin:6px 0;"),
          h5("计算历史", style = "margin-top:4px;"),
          uiOutput("tool_calc_history")
        )
      )
    ),

    # ── 11. 日期 ──
    tabPanel("日期",
      icon = icon("calendar"),
      fluidRow(
        column(6,
          h5("日期计算"),
          # 模式选择
          radioButtons("tool_date_mode", "计算模式", inline = TRUE,
            choices = c("到今天" = "to_today", "两日期差" = "diff", "日期加减" = "addsub")),
          # 单日期（到今天）
          conditionalPanel(
            condition = "input.tool_date_mode == 'to_today'",
            dateInput("tool_date_single", "日期", value = Sys.Date(), width = "100%")
          ),
          # 两个日期差
          conditionalPanel(
            condition = "input.tool_date_mode == 'diff'",
            dateInput("tool_date_start", "开始日期", value = Sys.Date(), width = "100%"),
            dateInput("tool_date_end", "结束日期", value = Sys.Date(), width = "100%"),
            checkboxInput("tool_date_inclusive", "含首尾（闭区间）", value = FALSE)
          ),
          # 日期加减
          conditionalPanel(
            condition = "input.tool_date_mode == 'addsub'",
            dateInput("tool_date_base", "基准日期", value = Sys.Date(), width = "100%"),
            numericInput("tool_date_days", "天数（可为负）", value = 30, width = "100%")
          ),
          br(),
          actionButton("tool_date_calc", "计算", icon = icon("calculator"), class = "btn-primary", style = "width:100%;"),
          br(), br(),
          # 日期信息
          h5("日期信息"),
          actionButton("tool_date_info", "查询星期/天数信息", icon = icon("info-circle"), class = "btn-info btn-sm", style = "width:100%;")
        ),
        column(6,
          h5("结果"),
          tags$div(style = "position:relative;",
            verbatimTextOutput("tool_date_out"),
            tags$button(class = "btn btn-xs", style = "position:absolute; top:4px; right:8px;",
              onclick = "var t=document.getElementById('tool_date_out'); if(t){navigator.clipboard.writeText(t.innerText); this.textContent='已复制'; setTimeout(function(){this.textContent='复制'}.bind(this),1500)}", "复制")
          )
        )
      )
    ),

    # ── 12. 流程数据可视化 ──
    tabPanel("流程数据可视化",
      icon = icon("chart-area"),
      flow_viz_ui()
    ),

    # ── 13. 图片合并PDF ──
    tabPanel("图片合并PDF",
      icon = icon("file-pdf"),
      img2pdf_ui()
    ),

    # ── 14. 通用数据导入 ──
    tabPanel("通用数据导入",
      icon = icon("file-import"),
      data_import_ui()
    )
  )
}
