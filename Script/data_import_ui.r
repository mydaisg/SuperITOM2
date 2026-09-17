# 通用数据导入模块 - UI

data_import_ui <- function() {
  tagList(
    tags$script(HTML("
      // 字段映射：收集所有源列→目标字段名，同步到隐藏输入
      function diCollectFieldMap() {
        var map = {};
        $('.di-field-row').each(function() {
          var src = $(this).find('.di-field-input').attr('data-src');
          var val = $(this).find('.di-field-input').val();
          map[src] = val;
        });
        var json = JSON.stringify(map);
        var inp = $('#di_field_map_json');
        if (inp.length) { inp.val(json).trigger('change'); }
      }
      // 输入框失焦时收集
      $(document).on('change blur', '.di-field-input', function() { diCollectFieldMap(); });
      $(document).on('input', '.di-field-input', function() { diCollectFieldMap(); });

      // 复制引用代码到剪贴板（供数据集列表的「复制」按钮调用）
      window.copyText = function(text) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function() {
            Shiny.setInputValue('di_copy_done', Math.random(), {priority: 'event'});
          });
        } else {
          // 降级：使用隐藏 textarea
          var ta = document.createElement('textarea');
          ta.value = text; document.body.appendChild(ta); ta.select();
          try { document.execCommand('copy'); } catch(e) {}
          document.body.removeChild(ta);
          Shiny.setInputValue('di_copy_done', Math.random(), {priority: 'event'});
        }
      };
    ")),
    tags$style(HTML("
      .di-step { display:inline-block; padding:4px 10px; border-radius:20px; font-size:12px; margin-right:8px; }
      .di-step.active { background:#337ab7; color:#fff; font-weight:600; }
      .di-step.done { background:#5cb85c; color:#fff; }
      .di-step.todo { background:#eee; color:#999; }
      .di-preview-table { max-height:300px; overflow:auto; }
      .di-field-row { display:flex; align-items:center; gap:8px; padding:4px 0; border-bottom:1px solid #f0f0f0; }
      .di-field-row .src { width:200px; color:#555; font-size:12px; }
      .di-field-row .arrow { color:#999; }
      .di-field-row input { flex:1; }
    ")),
    fluidPage(
      h4(icon("file-import"), " 通用数据导入", style = "margin-bottom: 10px;"),
      p(style = "color:#666; font-size:12px;",
        "上传 Excel 文件，选择 Sheet，配置字段映射后导入系统数据库，生成可被其它模块引用的数据列表。"),

      tabsetPanel(
        # ── 导入 ──
        tabPanel("导入数据", icon = icon("upload"),
          br(),
          # 步骤指示
          div(style = "margin-bottom: 12px;",
            tags$span(class = "di-step active", "① 上传 Excel"),
            tags$span(class = "di-step todo", "② 选择 Sheet 与字段"),
            tags$span(class = "di-step todo", "③ 导入并生成列表")
          ),
          wellPanel(
            h5("1. 上传 Excel 文件", style = "margin-top:0;"),
            fileInput("di_file", NULL, width = "100%",
              accept = c(".xlsx", ".xls", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
            # Sheet 选择（上传后显示）
            uiOutput("di_sheet_ui"),
            hr(),
            h5("2. 配置字段映射"),
            uiOutput("di_field_map_ui"),
            hr(),
            h5("3. 导入"),
            fluidRow(
              column(3, radioButtons("di_import_mode", "导入方式",
                choices = c("新建数据集" = "create", "追加到现有数据集" = "append"),
                selected = "create", inline = TRUE)),
              column(3, uiOutput("di_dataset_name_ui")),
              column(6, div(style = "margin-top: 20px;",
                uiOutput("di_import_btn")))
            )
          )
        ),
        # ── 数据列表 ──
        tabPanel("数据列表", icon = icon("list"),
          br(),
          fluidRow(
            column(12,
              DTOutput("di_dataset_table"),
              br(),
              h5("数据明细", style = "color:#337ab7;"),
              uiOutput("di_record_header"),
              DTOutput("di_record_table")
            )
          )
        )
      )
    )
  )
}
