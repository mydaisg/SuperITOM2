# 流程实例清单模块 — UI（作为「流程」标签页下的子标签）
# 样式参照「流程全量清单」：暗色主题 + 按 flow_catalog 分类分组 + 可折叠

flow_instance_list_ui <- function() {
  tagList(
    tags$style(HTML("
      .fil-wrap { background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%); border-radius: 12px; padding: 18px; min-height: 60vh; color: #fff; }
      .fil-toolbar { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; margin-bottom: 14px; }
      .fil-toolbar select, .fil-toolbar input { background: #1e2a44; color: #fff; border: 1px solid #2d3748; border-radius: 6px; padding: 4px 8px; font-size: 12px; }
      .fil-cat-row { cursor: pointer; background: rgba(0,212,255,0.08); }
      .fil-cat-row td { font-weight: 600; color: #00d4ff; padding: 8px 12px; }
      .fil-cat-toggle { display: inline-block; width: 14px; text-align: center; }
      .fil-item-row td { padding: 7px 12px; font-size: 13px; color: #c0c8dd; border-bottom: 1px solid rgba(255,255,255,0.06); }
      .fil-item-row:hover { background: rgba(255,255,255,0.05); }
      .fil-table { width: 100%; border-collapse: collapse; }
      .fil-table th { color: #8892b0; font-weight: 500; font-size: 12px; padding: 8px 12px; text-align: left; border-bottom: 1px solid rgba(255,255,255,0.1); }
      .fil-badge { display: inline-block; padding: 1px 9px; border-radius: 10px; font-size: 11px; font-weight: 600; }
      .fil-badge.done { background: rgba(0,230,118,0.15); color: #00e676; }
      .fil-badge.active { background: rgba(255,215,0,0.15); color: #ffd700; }
      .fil-summary { color: #8892b0; font-size: 12px; margin-bottom: 10px; }
      .fil-empty { color: #8892b0; text-align: center; padding: 40px 0; }
    ")),
    tags$script(HTML("
      $(document).on('click', '.fil-cat-row', function() {
        var cat = $(this).data('cat');
        var collapsed = $(this).data('collapsed') === '1';
        var items = $('.fil-item-row[data-cat=\"' + cat + '\"]');
        items.css('display', collapsed ? '' : 'none');
        $(this).data('collapsed', collapsed ? '0' : '1');
        $(this).find('.fil-cat-toggle').html(collapsed ? '&#9660;' : '&#9654;');
      });
      $(document).on('click', '#fil_expand_all', function() {
        $('.fil-item-row').css('display', '');
        $('.fil-cat-row').data('collapsed', '0').find('.fil-cat-toggle').html('&#9660;');
      });
      $(document).on('click', '#fil_collapse_all', function() {
        $('.fil-item-row').css('display', 'none');
        $('.fil-cat-row').data('collapsed', '1').find('.fil-cat-toggle').html('&#9654;');
      });
    ")),
    fluidRow(
      column(12,
        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:12px;",
          h4(icon("list-alt"), " 流程实例清单", style = "margin:0;"),
          actionButton("fil_refresh", "刷新", icon = icon("sync"), class = "btn-xs btn-default")
        )
      )
    ),
    div(class = "fil-wrap",
      div(class = "fil-toolbar",
        tags$select(id = "fil_status_filter",
          tags$option(value = "all", "全部状态"),
          tags$option(value = "进行中", "进行中"),
          tags$option(value = "已完成", "已完成")),
        tags$input(id = "fil_search", type = "text", placeholder = "搜索流程名称/发起人",
          style = "width:200px;"),
        tags$span(style = "flex:1;"),
        tags$button(type = "button", id = "fil_expand_all",
          class = "btn btn-xs btn-default",
          style = "background:#1e2a44;color:#00d4ff;border:1px solid #2d3748;", "全部展开"),
        tags$button(type = "button", id = "fil_collapse_all",
          class = "btn btn-xs btn-default",
          style = "background:#1e2a44;color:#8892b0;border:1px solid #2d3748;", "全部收缩")
      ),
      div(class = "fil-summary", uiOutput("fil_summary")),
      uiOutput("fil_table")
    )
  )
}
