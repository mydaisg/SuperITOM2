# 工具模块 UI
tools_ui <- function() {
  tagList(
    tags$script(src = "https://unpkg.com/pinyin-pro@3"),
    fluidPage(
      titlePanel(""),
      tabsetPanel(id = "tools_tabs",
        tabPanel("文本格式化",
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
        tabPanel("拼音",
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
            Shiny.addCustomMessageHandler('doPinyin', function(mode) {
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
        tabPanel("收集器",
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
        tabPanel("集成",
          integration_ui()
        ),
        tabPanel("标准化",
          std_ui()
        ),
        tabPanel("AI",
          ai_ui()
        )
      )
    )
  )
}
