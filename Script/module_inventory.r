# 模块清单 — 网站所有模块/功能/源代码/数据表映射

MODULE_INVENTORY <- list(
  list(
    module = "首页",
    frontend = "首页标签页 (navbarPage tabPanel)",
    source = "server.R (home_my_projects / home_my_work_orders / home_my_tasks)",
    tables = "projects, work_orders, project_tasks",
    perms = list(list(code="home_view", name="查看首页")),
    key_funcs = "work_order_get_stats(), project_get_all() — 统计数据"
  ),
  list(
    module = "项目",
    frontend = "项目标签页 → project_ui()",
    source = "Script/project_management.r / project_server.r / project_ui.r",
    tables = "projects, project_phases, project_work_packages, project_tasks, project_task_logs",
    perms = list(
      list(code="proj_view",   name="查看项目"),
      list(code="proj_create", name="创建项目"),
      list(code="proj_edit",   name="编辑项目"),
      list(code="proj_delete", name="删除项目"),
      list(code="proj_manage", name="管理项目")
    ),
    key_funcs = c(
      "project_get_all() / project_add() — 项目CRUD",
      "phase_get_by_project() / phase_add() — 阶段CRUD",
      "wp_get_by_phase() / wp_add() — 工作包CRUD",
      "task_get_by_wp() / task_add() — 任务CRUD",
      "task_toggle_favorite() / task_set_importance()",
      "task_convert_to_work_order() — 转工单",
      "project_generate_number() → PRJ+日期+流水"
    )
  ),
  list(
    module = "巡检",
    frontend = "巡检标签页 (5个子标签)",
    source = "Script/inspection_management.r / inspection_server.r / main_ui.r",
    tables = "inspection_plans, inspection_items, inspection_item_templates, inspection_tasks, inspection_records, inspection_issues, inspection_plan_comments",
    perms = list(
      list(code="insp_view",    name="查看巡检"),
      list(code="insp_create",  name="创建巡检"),
      list(code="insp_execute", name="执行巡检"),
      list(code="insp_manage",  name="管理巡检")
    ),
    key_funcs = c(
      "inspection_plan_get_all() / inspection_plan_add() — 计划CRUD",
      "inspection_item_get_by_plan() / inspection_item_add() — 检查项",
      "inspection_task_get_by_plan() / inspection_task_generate() — 任务",
      "inspection_record_submit() — 提交记录(含拍照)",
      "inspection_issue_create() — 创建异常→生成整改工单",
      "inspection_plan_generate_number() → INS-PLAN-日期-流水",
      "inspection_task_generate_number() → INS-TSK-日期-流水"
    )
  ),
  list(
    module = "工单",
    frontend = "工单标签页 (列表/派发/处理/批量/快速工单)",
    source = "Script/work_order.r + server.R (~600行内联)",
    tables = "work_orders, work_order_comments",
    perms = list(
      list(code="wo_view",   name="查看工单"),
      list(code="wo_create", name="创建工单"),
      list(code="wo_edit",   name="编辑工单"),
      list(code="wo_assign", name="派发工单"),
      list(code="wo_delete", name="删除工单")
    ),
    key_funcs = c(
      "work_order_get_all() / work_order_add() — 工单CRUD",
      "work_order_assign() / work_order_start_handle()",
      "work_order_complete() / work_order_close()",
      "work_order_add_comment() / work_order_get_comments()",
      "work_order_parse_quick_text() — 快速工单解析",
      "work_order_batch_parse() / work_order_batch_create() — 批量补工单",
      "work_order_generate_number() → ITS+日期+3位流水",
      "work_order_status_color() / work_order_status_label()"
    )
  ),
  list(
    module = "资产",
    frontend = "资产标签页 → asset_ui()",
    source = "Script/asset_management.r / asset_server.r / asset_ui.r",
    tables = "assets",
    perms = list(
      list(code="asset_view",   name="查看资产"),
      list(code="asset_manage", name="管理资产")
    ),
    key_funcs = c(
      "asset_get_all() / asset_add() / asset_update() / asset_delete()",
      "asset_get_stats() — 统计卡片数据"
    )
  ),
  list(
    module = "记事",
    frontend = "记事标签页 → note_ui()",
    source = "Script/note_management.r / note_server.r / note_ui.r",
    tables = "notes, note_comments",
    perms = list(
      list(code="note_view",     name="查看记事"),
      list(code="note_create",   name="创建记事"),
      list(code="note_edit",     name="编辑记事"),
      list(code="note_delete",   name="删除记事"),
      list(code="note_dispatch", name="派发记事")
    ),
    key_funcs = c(
      "note_get_all() / note_add() — 记事CRUD",
      "note_comment_add() / note_comment_get_by_note()",
      "note_comment_mark_status() — 标记完成",
      "note_generate_number() → NTE+日期+流水"
    )
  ),
  list(
    module = "标准化",
    frontend = "标准化标签页 → std_ui()",
    source = "Script/std_computer.r",
    tables = "std_hosts",
    perms = list(
      list(code="std_view",   name="查看标准化"),
      list(code="std_manage", name="管理标准化")
    ),
    key_funcs = c(
      "std_hosts_data() / std_host_add() — 主机管理",
      "std_ping_test() / std_execute_script() — 远程执行",
      "PowerShell脚本: STD/1_hostinfo.ps1 ~ 4_LocalAdmin.ps1"
    )
  ),
  list(
    module = "测试",
    frontend = "测试标签页 → network_test_ui()",
    source = "Script/network_test.r",
    tables = "- (读取config/init.json)",
    perms = list(
      list(code="ntest_view", name="查看测试"),
      list(code="ntest_run",  name="运行测试")
    ),
    key_funcs = c(
      "network_test_ping() / network_test_nslookup()",
      "network_test_tracert() / network_test_curl()",
      "network_test_port() — TCP端口测试",
      "network_test_file_server() — 文件服务器连通性",
      "iconv(result, from='GBK', to='UTF-8') — 编码转换"
    )
  ),
  list(
    module = "性能",
    frontend = "性能标签页 → sysmon_ui()",
    source = "Script/sysmon_management.r / sysmon_server.r / sysmon_ui.r",
    tables = "system_monitors",
    perms = list(
      list(code="sysmon_view",   name="查看性能监控"),
      list(code="sysmon_manage", name="管理性能监控")
    ),
    key_funcs = c(
      "sysmon_get_all() / sysmon_add() — 监控项CRUD",
      "sysmon_collect() — 采集数据"
    )
  ),
  list(
    module = "日报",
    frontend = "日报标签页 → daily_report_ui()",
    source = "Script/daily_report.r",
    tables = "- (从work_orders/project_tasks聚合)",
    perms = list(
      list(code="dr_view", name="查看日报")
    ),
    key_funcs = c(
      "daily_report_get_by_date() — 按日期提取",
      "daily_report_get_by_person() — 按人提取",
      "daily_report_copy_text() — 复制文本日报",
      "自动聚合: 工单操作记录 + 项目任务反馈日志"
    )
  ),
  list(
    module = "收集器",
    frontend = "收集器标签页 (sidebarLayout)",
    source = "Script/information_collector.r + server.R",
    tables = "information_collectors",
    perms = list(
      list(code="collector_view",   name="查看收集器"),
      list(code="collector_create", name="创建收集器"),
      list(code="collector_manage", name="管理收集器")
    ),
    key_funcs = c(
      "info_collector_get_all() / info_collector_add()"
    )
  ),
  list(
    module = "集成",
    frontend = "集成标签页 → integration_ui()",
    source = "Script/integration_management.r / integration_server.r / integration_ui.r",
    tables = "integrations",
    perms = list(
      list(code="integration_view",   name="查看集成"),
      list(code="integration_manage", name="管理集成")
    ),
    key_funcs = c(
      "integration_get_all() / integration_add() / integration_test()"
    )
  ),
  list(
    module = "数据",
    frontend = "数据标签页 → data_center_ui()",
    source = "Script/data_center_server.r / data_center_ui.r",
    tables = "- (数据归集/聚合展示)",
    perms = list(
      list(code="dc_view", name="查看数据中心")
    ),
    key_funcs = c(
      "data_center_get_stats() — 各模块统计聚合",
      "data_center_get_detail() — 明细穿透查询"
    )
  ),
  list(
    module = "岗职",
    frontend = "岗职标签页 → duty_matrix_ui()",
    source = "Script/duty_matrix_management.r / duty_matrix_server.r / duty_matrix_ui.r",
    tables = "duty_positions, duty_staff, duty_items, duty_matrix, duty_sub_items, duty_sub_matrix",
    perms = list(
      list(code="duty_view",   name="查看岗职"),
      list(code="duty_manage", name="管理岗职")
    ),
    key_funcs = c(
      "duty_position_get_all() / duty_position_add() — 岗位CRUD",
      "duty_staff_get_all() / duty_staff_add() — 人员CRUD",
      "duty_item_get_all() / duty_item_add() — 职责项CRUD",
      "duty_matrix_get() / duty_matrix_set() — 矩阵赋值",
      "duty_sub_item_get_all() / duty_sub_item_add() — 二级任务",
      "duty_sub_matrix_get() / duty_sub_matrix_set() — 二级矩阵"
    )
  ),
  list(
    module = "绩效",
    frontend = "绩效标签页 → performance_ui()",
    source = "Script/performance_management.r / performance_server.r / performance_ui.r",
    tables = "performance_records",
    perms = list(
      list(code="perf_view",   name="查看绩效"),
      list(code="perf_create", name="添加绩效"),
      list(code="perf_manage", name="管理绩效")
    ),
    key_funcs = c(
      "performance_get_all() / performance_add() — 绩效CRUD",
      "performance_get_stats() — 绩效统计"
    )
  ),
  list(
    module = "模型",
    frontend = "模型标签页 (sidebarLayout)",
    source = "Script/model_training.r + server.R",
    tables = "models",
    perms = list(
      list(code="model_view",   name="查看模型"),
      list(code="model_create", name="创建模型"),
      list(code="model_manage", name="管理模型")
    ),
    key_funcs = c(
      "model_get_all() / model_add() / model_train()"
    )
  ),
  list(
    module = "可视化",
    frontend = "可视化标签页 (sidebarLayout + plotly)",
    source = "Script/visualization.r + server.R",
    tables = "- (生成图表, 无持久化表)",
    perms = list(
      list(code="viz_view", name="查看可视化")
    ),
    key_funcs = c(
      "viz_generate() — 生成plotly图表",
      "server.R: output$viz_plot <- renderPlotly({ viz_generate(...) })"
    )
  ),
  list(
    module = "管理",
    frontend = "管理 dropdown菜单 (navbarMenu)",
    source = "Script/user_management.r / system_settings.r / rbac_management.r / github_autosubmit.r + server.R",
    tables = "users, system_config, config_options, rbac_roles, rbac_permissions, rbac_role_permissions, rbac_user_roles",
    perms = list(
      list(code="admin_users",   name="用户管理"),
      list(code="admin_system",  name="系统设置"),
      list(code="admin_options", name="选项配置"),
      list(code="admin_github",  name="GitHub操作"),
      list(code="admin_rbac",    name="授权管理")
    ),
    key_funcs = c(
      "user_get_all() / user_add() / user_update() / user_toggle_active()",
      "config_get_all() / config_add() / config_get_value()",
      "config_option_choices() / config_option_color() / config_option_label()",
      "rbac_check() / rbac_get_user_modules() — 权限检查核心",
      "rbac_role_get_all() / rbac_role_perms_set() / rbac_user_roles_set()",
      "github_autosubmit() / github_check_status() / github_pull()"
    )
  )
)

# 渲染模块清单 UI
module_inventory_ui <- function() {
  tags$div(
    tags$style(HTML("
      .mi-mod-hdr { background:#e8f0fe; padding:6px 12px; margin:6px 0 2px; cursor:pointer; border-radius:4px; font-weight:700; font-size:15px; user-select:none; border:1px solid #c8daf5; }
      .mi-mod-hdr:hover { background:#d4e4fc; }
      .mi-l2-hdr { background:#f5f5f5; padding:4px 10px; margin:2px 0; cursor:pointer; border-radius:3px; font-weight:600; font-size:13px; user-select:none; }
      .mi-l2-hdr:hover { background:#e8e8e8; }
      .mi-info { padding:2px 16px; font-size:13px; color:#555; line-height:1.7; }
      .mi-code { background:#fff; border:1px solid #ddd; border-radius:3px; padding:1px 6px; font-family:monospace; font-size:12px; margin:0 2px; }
      .mi-func { font-family:monospace; font-size:12px; color:#337ab7; }
    ")),
    tags$h3(icon("sitemap"), " 模块清单 — 全站模块/功能/源代码/数据表映射"),
    tags$p(style="color:#7f8c8d; font-size:13px;", "展开各模块查看: 前端名称 / 源码文件 / 数据库表 / 权限码 / 关键函数"),
    hr(),
    lapply(seq_along(MODULE_INVENTORY), function(i) {
      m <- MODULE_INVENTORY[[i]]
      mod_id <- paste0("mi-mod-", i)
      perms_str <- paste(sapply(m$perms, function(p) sprintf('<span class="mi-code">[%s] %s</span>', p$code, p$name)), collapse = " ")

      tags$div(style="margin-bottom:4px; border:1px solid #d0d0d0; border-radius:4px; overflow:hidden;",
        # 模块头
        tags$div(class="mi-mod-hdr",
          onclick = sprintf("var el=document.getElementById('%s');el.style.display=el.style.display==='none'?'block':'none';", mod_id),
          sprintf("📁 %s", m$module)
        ),
        # 二级内容
        tags$div(id=mod_id, style="display:none; padding:4px 12px 8px;",
          # 基本信息
          tags$div(class="mi-l2-hdr", onclick=sprintf("var el=document.getElementById('%s-info');el.style.display=el.style.display==='none'?'block':'none';", mod_id), "▸ 基本信息"),
          tags$div(id=paste0(mod_id,"-info"), class="mi-info", style="display:none;",
            tags$div(tags$b("前端名称: "), m$frontend),
            tags$div(tags$b("源码文件: "), tags$code(m$source)),
            tags$div(tags$b("数据表: "), tags$code(m$tables))
          ),
          # 权限码
          tags$div(class="mi-l2-hdr", onclick=sprintf("var el=document.getElementById('%s-perms');el.style.display=el.style.display==='none'?'block':'none';", mod_id), "▸ 权限项"),
          tags$div(id=paste0(mod_id,"-perms"), class="mi-info", style="display:none;",
            HTML(perms_str)
          ),
          # 关键函数
          tags$div(class="mi-l2-hdr", onclick=sprintf("var el=document.getElementById('%s-funcs');el.style.display=el.style.display==='none'?'block':'none';", mod_id), "▸ 关键函数"),
          tags$div(id=paste0(mod_id,"-funcs"), class="mi-info", style="display:none;",
            if (is.character(m$key_funcs) && length(m$key_funcs) == 1) {
              tags$div(tags$span(class="mi-func", m$key_funcs))
            } else {
              lapply(m$key_funcs, function(f) tags$div(tags$span(class="mi-func", f)))
            }
          )
        )
      )
    })
  )
}
