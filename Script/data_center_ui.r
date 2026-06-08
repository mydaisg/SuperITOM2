# 数据中心模块 UI
# 数据归集模块：集成项目、工单、巡检、测试、日报等模块的数据概览

data_center_ui <- function() {
  ns <- NS("data_center")
  
  tagList(
    tags$head(
      tags$style(HTML("
        .data-module-card {
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 15px;
          background: linear-gradient(135deg, #f5f7fa 0%, #e4e8ec 100%);
          transition: all 0.3s ease;
          cursor: pointer;
        }
        .data-module-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        }
        .data-module-card .module-icon {
          font-size: 42px;
          margin-bottom: 10px;
        }
        .data-module-card .module-title {
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 8px;
          color: #333;
        }
        .data-module-card .module-stats {
          font-size: 13px;
          color: #666;
        }
        .data-module-card .module-stats .stat-item {
          margin: 3px 0;
        }
        .data-module-card .stat-value {
          font-weight: bold;
          color: #2c3e50;
        }
        .data-module-card .view-detail {
          margin-top: 12px;
          font-size: 12px;
          color: #3498db;
        }
        /* 项目模块配色 */
        .module-project { border-left: 4px solid #3498db; }
        .module-project .module-icon { color: #3498db; }
        
        /* 工单模块配色 */
        .module-workorder { border-left: 4px solid #e74c3c; }
        .module-workorder .module-icon { color: #e74c3c; }
        
        /* 巡检模块配色 */
        .module-inspection { border-left: 4px solid #27ae60; }
        .module-inspection .module-icon { color: #27ae60; }
        
        /* 测试模块配色 */
        .module-network { border-left: 4px solid #9b59b6; }
        .module-network .module-icon { color: #9b59b6; }
        
        /* 日报模块配色 */
        .module-daily { border-left: 4px solid #f39c12; }
        .module-daily .module-icon { color: #f39c12; }
        
        /* 资产模块配色 */
        .module-asset { border-left: 4px solid #1abc9c; }
        .module-asset .module-icon { color: #1abc9c; }
        
        /* 记事模块配色 */
        .module-note { border-left: 4px solid #6c3bbf; }
        .module-note .module-icon { color: #6c3bbf; }
        
        /* 岗职模块配色 */
        .module-duty { border-left: 4px solid #e67e22; }
        .module-duty .module-icon { color: #e67e22; }
        
        /* 绩效模块配色 */
        .module-perf { border-left: 4px solid #e74c3c; }
        .module-perf .module-icon { color: #e74c3c; }
        
        /* 明细表格样式 */
        .detail-section {
          margin-top: 20px;
          padding: 15px;
          background: #fff;
          border-radius: 8px;
          border: 1px solid #e0e0e0;
        }
        .detail-section h4 {
          margin-bottom: 15px;
          color: #2c3e50;
          border-bottom: 2px solid #3498db;
          padding-bottom: 8px;
        }
        .detail-section .close-btn {
          float: right;
          margin-top: -35px;
        }
      "))
    ),

    # 卡片点击事件处理（div 需要用 JS 触发 Shiny input）
    tags$script(HTML("
      $(document).on('click', '.data-module-card', function(e) {
        var id = $(this).attr('id');
        if (id) {
          Shiny.setInputValue(id, {click: Date.now()}, {priority: 'event'});
        }
      });
    ")),

    fluidPage(
      # 页面标题
      div(style = "text-align: center; margin: 20px 0;",
        h2(icon("database"), " 数据中心"),
        p("集成展示项目、工单、巡检、测试、日报等模块的数据概览", style = "color: #7f8c8d;")
      ),
      
      # 模块概览卡片区
      fluidRow(
        # 项目模块卡片
        column(4,
          div(class = "data-module-card module-project",
            id = ns("card_project"),
            div(class = "module-icon", icon("folder-open")),
            div(class = "module-title", "项目管理"),
            div(class = "module-stats",
              div(class = "stat-item", "总项目数：", span(class = "stat-value", textOutput(ns("proj_total"), inline = TRUE))),
              div(class = "stat-item", "进行中：", span(class = "stat-value", textOutput(ns("proj_active"), inline = TRUE))),
              div(class = "stat-item", "已完成：", span(class = "stat-value", textOutput(ns("proj_completed"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        ),
        
        # 工单模块卡片
        column(4,
          div(class = "data-module-card module-workorder",
            id = ns("card_workorder"),
            div(class = "module-icon", icon("clipboard-list")),
            div(class = "module-title", "工单管理"),
            div(class = "module-stats",
              div(class = "stat-item", "总工单数：", span(class = "stat-value", textOutput(ns("wo_total"), inline = TRUE))),
              div(class = "stat-item", "待处理：", span(class = "stat-value", textOutput(ns("wo_pending"), inline = TRUE))),
              div(class = "stat-item", "已完成：", span(class = "stat-value", textOutput(ns("wo_completed"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        ),
        
        # 巡检模块卡片
        column(4,
          div(class = "data-module-card module-inspection",
            id = ns("card_inspection"),
            div(class = "module-icon", icon("clipboard-check")),
            div(class = "module-title", "巡检管理"),
            div(class = "module-stats",
              div(class = "stat-item", "总计划数：", span(class = "stat-value", textOutput(ns("insp_plans"), inline = TRUE))),
              div(class = "stat-item", "执行中：", span(class = "stat-value", textOutput(ns("insp_tasks"), inline = TRUE))),
              div(class = "stat-item", "异常数：", span(class = "stat-value", textOutput(ns("insp_issues"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        )
      ),
      
      fluidRow(
        # 测试模块卡片
        column(4,
          div(class = "data-module-card module-network",
            id = ns("card_network"),
            div(class = "module-icon", icon("wifi")),
            div(class = "module-title", "网络测试"),
            div(class = "module-stats",
              div(class = "stat-item", "测试记录：", span(class = "stat-value", textOutput(ns("nt_logs"), inline = TRUE))),
              div(class = "stat-item", "最近测试：", span(class = "stat-value", textOutput(ns("nt_last"), inline = TRUE))),
              div(class = "stat-item", "记录目录：", span(class = "stat-value", textOutput(ns("nt_dir"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        ),
        
        # 日报模块卡片
        column(4,
          div(class = "data-module-card module-daily",
            id = ns("card_daily"),
            div(class = "module-icon", icon("calendar-alt")),
            div(class = "module-title", "工作日报"),
            div(class = "module-stats",
              div(class = "stat-item", "总日报数：", span(class = "stat-value", textOutput(ns("dr_total"), inline = TRUE))),
              div(class = "stat-item", "本月日报：", span(class = "stat-value", textOutput(ns("dr_month"), inline = TRUE))),
              div(class = "stat-item", "今日日报：", span(class = "stat-value", textOutput(ns("dr_today"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        ),
        
        # 资产模块卡片 ★新增
        column(4,
          div(class = "data-module-card module-asset",
            id = ns("card_asset"),
            div(class = "module-icon", icon("laptop")),
            div(class = "module-title", "资产管理"),
            div(class = "module-stats",
              div(class = "stat-item", "总资产数：", span(class = "stat-value", textOutput(ns("ast_total"), inline = TRUE))),
              div(class = "stat-item", "使用中：", span(class = "stat-value", textOutput(ns("ast_active"), inline = TRUE))),
              div(class = "stat-item", "维护中：", span(class = "stat-value", textOutput(ns("ast_maint"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        )
      ),
      
      fluidRow(
        # 记事模块卡片 ★新增
        column(4,
          div(class = "data-module-card module-note",
            id = ns("card_note"),
            div(class = "module-icon", icon("sticky-note")),
            div(class = "module-title", "记事管理"),
            div(class = "module-stats",
              div(class = "stat-item", "总记事数：", span(class = "stat-value", textOutput(ns("note_total"), inline = TRUE))),
              div(class = "stat-item", "待处理：", span(class = "stat-value", textOutput(ns("note_pending"), inline = TRUE))),
              div(class = "stat-item", "进行中：", span(class = "stat-value", textOutput(ns("note_progress"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        ),
        
        # 岗职模块卡片 ★新增
        column(4,
          div(class = "data-module-card module-duty",
            id = ns("card_duty"),
            div(class = "module-icon", icon("sitemap")),
            div(class = "module-title", "岗职矩阵"),
            div(class = "module-stats",
              div(class = "stat-item", "岗位数：", span(class = "stat-value", textOutput(ns("duty_pos"), inline = TRUE))),
              div(class = "stat-item", "人员数：", span(class = "stat-value", textOutput(ns("duty_staff"), inline = TRUE))),
              div(class = "stat-item", "职责项：", span(class = "stat-value", textOutput(ns("duty_items"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        ),
        
        # 绩效模块卡片 ★新增
        column(4,
          div(class = "data-module-card module-perf",
            id = ns("card_perf"),
            div(class = "module-icon", icon("chart-bar")),
            div(class = "module-title", "绩效管理"),
            div(class = "module-stats",
              div(class = "stat-item", "总月表数：", span(class = "stat-value", textOutput(ns("perf_sheets"), inline = TRUE))),
              div(class = "stat-item", "当月员工：", span(class = "stat-value", textOutput(ns("perf_emps"), inline = TRUE))),
              div(class = "stat-item", "指标总数：", span(class = "stat-value", textOutput(ns("perf_inds"), inline = TRUE)))
            ),
            div(class = "view-detail", icon("chevron-right"), " 点击查看明细 →")
          )
        )
      ),
      
      # 明细展示区（点击卡片后显示）
      uiOutput(ns("detail_container"))
    )
  )
}
