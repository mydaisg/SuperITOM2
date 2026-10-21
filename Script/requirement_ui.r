# 需求模块 UI — 3 个子标签：需求列表 / 进度 / 甘特图
requirement_ui <- function() {
  tagList(
    tags$script(HTML("
      // 需求进度：明细收缩/展开（参照记事评论）
      $(document).on('click', '.reqprog-collapse-btn', function(e){
        e.stopPropagation();
        var id = $(this).data('id');
        var $body = $('#reqprog-body-' + id);
        if ($body.is(':visible')) {
          $body.hide();
          $(this).text('▾ 展开');
        } else {
          $body.show();
          $(this).text('▴ 收缩');
        }
      });
      // 全部展开 / 全部收缩
      window.reqprogToggleAll = function(btn){
        var $bodies = $('.reqprog-body');
        var anyHidden = false;
        $bodies.each(function(){ if(!$(this).is(':visible')) anyHidden = true; });
        if (anyHidden) {
          $bodies.show();
          $('.reqprog-collapse-btn').text('▴ 收缩');
          if (btn) btn.textContent = '全部收缩';
        } else {
          $bodies.hide();
          $('.reqprog-collapse-btn').text('▾ 展开');
          if (btn) btn.textContent = '全部展开';
        }
      };
      // 复制进度文本（参照总结模块）
      $(document).on('click', '#req_prog_copy_btn', function() {
        var text = $('#req_prog_text_content').val();
        if (!text) return;
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function() {
            Shiny.setInputValue('req_copy_done', Math.random(), {priority:'event'});
          });
        } else {
          // 降级：非安全上下文（HTTP/IP访问）用 execCommand
          var ta = document.createElement('textarea');
          ta.value = text; document.body.appendChild(ta); ta.select();
          try { document.execCommand('copy'); } catch(e) {}
          document.body.removeChild(ta);
          Shiny.setInputValue('req_copy_done', Math.random(), {priority:'event'});
        }
      });
      Shiny.addCustomMessageHandler('req_update_text', function(message) {
        $('#req_prog_text_content').val(message);
      });
    ")),
    tags$style(HTML("
      .req-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .req-cap h3 { margin:0; color:#fff; }
      .req-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
      .req-card { background:#fff; border:1px solid #e0e0e0; border-radius:8px; padding:14px; margin-bottom:10px; cursor:pointer; transition:all .15s; }
      .req-card:hover { border-color:#337ab7; box-shadow:0 2px 8px rgba(0,0,0,.08); }
      .req-card.active { border-color:#337ab7; background:#f5f9ff; }
      .req-badge { display:inline-block; padding:1px 10px; border-radius:10px; font-size:11px; color:#fff; margin-left:8px; }
      .prog-dept { font-weight:700; font-size:14px; color:#0f2b5c; }
      .prog-person { color:#555; font-size:13px; }
      .prog-status { display:inline-block; padding:1px 10px; border-radius:10px; font-size:11px; color:#fff; margin-left:6px; }
      .prog-date { font-size:11px; color:#999; margin-left:6px; }
      .prog-content { font-size:13px; color:#444; margin-top:4px; padding-left:16px; }
      /* 甘特图（简单版·时长数据条） */
      .gt-wrap{background:#fff;padding:6px;}
      .gt-title{text-align:center;margin-bottom:2px;}
      .gt-title h2{font-size:17px;font-weight:700;color:#0f2b5c;margin:0;}
      .gt-legend{text-align:center;font-size:10.5px;color:#556;margin-bottom:6px;}
      .gt-legend .li{display:inline-block;margin:0 6px;}
      .gt-legend .dot{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:3px;vertical-align:-1px;}
      .sb-table{border:1px solid #e2e8f0;border-radius:6px;overflow:hidden;background:#fff;}
      .sb-row{display:flex;align-items:center;border-bottom:1px solid #f0f4f8;min-height:20px;}
      .sb-name{width:300px;min-width:300px;padding:2px 10px;font-size:11px;color:#334155;box-sizing:border-box;white-space:nowrap;overflow:hidden;}
      .sb-range{width:96px;min-width:96px;font-family:Consolas,monospace;font-size:10px;color:#7b8794;box-sizing:border-box;padding:2px 6px;text-align:center;}
      .sb-barwrap{flex:1;display:flex;align-items:center;gap:6px;padding-right:10px;min-width:0;}
      .sb-bar{height:9px;border-radius:2px;opacity:.88;min-width:2px;}
      .sb-phasebar{height:12px;opacity:.95;border-radius:3px;}
      .sb-days{font-size:10px;color:#7b8794;white-space:nowrap;}
      .sb-phase{font-size:11.5px;}
      .sb-phase .sb-name{font-weight:700;}
      .ph-name-block{display:inline-block;padding:1px 8px;border-radius:6px;font-size:11.5px;font-weight:700;color:#fff;white-space:nowrap;}
      .sb-range-tag{display:inline-block;margin-left:6px;font-size:10px;font-weight:400;color:#9aa3af;}
      .sb-day-block{display:inline-block;margin-left:8px;padding:1px 8px;border-radius:8px;font-size:10px;font-weight:600;color:#fff;white-space:nowrap;}
      /* ── 进度时间轴（参照总结模块时间流水） ── */
      .reqtl-timeline { max-width: 900px; margin: 0 auto; padding: 20px 0 40px; position: relative; }
      .reqtl-timeline::before {
        content: ''; position: absolute; left: 50%; top: 0; bottom: 0;
        width: 4px; background: linear-gradient(180deg, #e0e7ff, #c7d2fe 30%, #a5b4fc 70%, #e0e7ff);
        transform: translateX(-50%); border-radius: 2px;
      }
      .reqtl-header { text-align: center; font-size: 15px; font-weight: bold; color: #4f46e5; margin-bottom: 24px; }
      .reqtl-day-label { text-align: center; margin: 16px 0 8px; position: relative; z-index: 2; }
      .reqtl-day-label span { display: inline-block; background: #eef2ff; border: 1px solid #a5b4fc; color: #4338ca; font-size: 13px; font-weight: 600; border-radius: 12px; padding: 2px 14px; }
      .reqtl-item {
        position: relative; width: 50%; padding: 8px 40px 8px 0; box-sizing: border-box;
        min-height: 60px;
      }
      .reqtl-item:nth-child(even) {
        margin-left: 50%; padding: 8px 0 8px 40px;
      }
      .reqtl-item::before {
        content: ''; position: absolute; right: -8px; top: 22px;
        width: 16px; height: 16px; border-radius: 50%; background: #fff;
        border: 4px solid #6366f1; box-shadow: 0 0 0 2px #e0e7ff; z-index: 3;
      }
      .reqtl-item:nth-child(even)::before { left: -8px; right: auto; }
      .reqtl-item::after {
        content: ''; position: absolute; right: 0; top: 28px; width: 32px; height: 2px; background: #c7d2fe; z-index: 1;
      }
      .reqtl-item:nth-child(even)::after { left: 0; right: auto; }
      .reqtl-card {
        background: #fff; border: 1px solid #e0e7ff; border-radius: 10px;
        padding: 12px 14px; box-shadow: 0 2px 6px rgba(99,102,241,0.08);
        position: relative; transition: all .15s;
      }
      .reqtl-card:hover { transform: translateY(-2px); box-shadow: 0 6px 16px rgba(99,102,241,0.15); border-color: #a5b4fc; }
      .reqtl-card::before {
        content: ''; position: absolute; top: 20px; width: 10px; height: 10px; background: #fff; border: 1px solid #e0e7ff;
        transform: rotate(45deg); border-right-color: #fff; border-top-color: #fff;
      }
      .reqtl-item:nth-child(odd) .reqtl-card::before { right: -6px; }
      .reqtl-item:nth-child(even) .reqtl-card::before { left: -6px; transform: rotate(45deg); border-left-color: #fff; border-bottom-color: #fff; border-right-color: #e0e7ff; border-top-color: #e0e7ff; }
      .reqtl-card-hd { display: flex; align-items: center; justify-content: space-between; margin-bottom: 6px; }
      .reqtl-card-hd-left { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
      .reqtl-card-hd b { font-size: 13px; color: #4338ca; }
      .reqtl-time-badge { font-size: 10px; background: #ede9fe; color: #5b21b6; border-radius: 6px; padding: 1px 8px; font-family: Consolas, monospace; white-space: nowrap; }
      .reqtl-tag { font-size: 10px; background: #e0f2fe; color: #0369a1; border-radius: 10px; padding: 1px 8px; white-space: nowrap; max-width: 180px; overflow: hidden; text-overflow: ellipsis; display: inline-block; }
      .reqtl-status { font-size: 10px; color: #fff; border-radius: 10px; padding: 1px 8px; white-space: nowrap; display: inline-block; }
      .reqtl-card-bd { font-size: 12px; color: #475569; line-height: 1.6; white-space: pre-wrap; }
    ")),
    fluidRow(
      column(12,
        div(class = "req-cap", style = "background:linear-gradient(135deg,#0f2b5c,#2563eb);",
          h3(icon("clipboard-list"), " 需求管理"),
          p("需求收集 · 进度跟踪 · 甘特图（时长数据条）")
        )
      )
    ),
    tabsetPanel(
      id = "req_tabs", type = "pills",

      # ── Tab1：需求列表 ──
      tabPanel("需求", icon = icon("list"),
        br(),
        fluidRow(
          column(3, selectInput("req_filter_status", "状态",
            choices = c("全部状态" = "", requirement_status_choices()), width = "100%")),
          column(4, textInput("req_search", NULL, width = "100%", placeholder = "搜索标题/描述...")),
          column(2, actionButton("req_search_btn", "搜索", icon = icon("search"), class = "btn-primary btn-sm")),
          column(3, actionButton("req_add_btn", "新建需求", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;"))
        ),
        br(),
        uiOutput("req_list")
      ),

      # ── Tab2：进度 ──
      tabPanel("进度", icon = icon("tasks"),
        br(),
        uiOutput("req_progress_ui")
      ),

      # ── Tab3：甘特图 ──
      tabPanel("甘特图", icon = icon("chart-gantt"),
        br(),
        fluidRow(
          column(4, uiOutput("req_gantt_filter_ui")),
          column(2, actionButton("req_gantt_refresh", "刷新", class = "btn-info btn-sm", style = "margin-top:2px;"))
        ),
        br(),
        uiOutput("req_gantt_chart_ui")
      )
    ),
    # 隐藏 textarea：存放进度纯文本（复制用）
    tags$textarea(id = "req_prog_text_content", style = "display:none;")
  )
}
