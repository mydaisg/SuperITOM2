# 系统架构可视化 — 纯 HTML/CSS 色块 + 表格 + 图标

# 模块颜色映射
ARCH_COLORS <- list(
  "UI" = "#3b82f6", "Service" = "#10b981", "DB" = "#ef4444",
  "数据" = "#f59e0b", "流程" = "#8b5cf6", "工具" = "#6b7280"
)

# 模块列表（按导航栏顺序）
ARCH_MODULES <- list(
  list(icon="home",             name="首页",     color="#4caf50"),
  list(icon="project-diagram",  name="项目",     color="#2196f3"),
  list(icon="clipboard-check",  name="巡检",     color="#ff9800"),
  list(icon="clipboard-list",   name="工单",     color="#e91e63"),
  list(icon="laptop",           name="资产",     color="#9c27b0"),
  list(icon="sticky-note",      name="记事",     color="#fbc02d"),
  list(icon="cogs",             name="标准化",   color="#009688"),
  list(icon="network-wired",    name="测试",     color="#3f51b5"),
  list(icon="heartbeat",        name="性能",     color="#ff5722"),
  list(icon="calendar-day",     name="日报",     color="#0288d1"),
  list(icon="download",         name="收集器",   color="#689f38"),
  list(icon="plug",             name="集成",     color="#5e35b1"),
  list(icon="database",         name="数据",     color="#ff8f00"),
  list(icon="sitemap",          name="岗职",     color="#00bcd4"),
  list(icon="chart-bar",        name="绩效",     color="#cddc39"),
  list(icon="cogs",             name="模型",     color="#607d8b"),
  list(icon="chart-line",       name="可视化",   color="#66bb6a"),
  list(icon="tools",            name="管理",     color="#9e9e9e")
)

# 数据表映射
ARCH_TABLES <- list(
  projects    = c("projects","project_phases","project_work_packages","project_tasks","project_task_logs"),
  inspection  = c("inspection_plans","inspection_items","inspection_tasks","inspection_records","inspection_issues","inspection_plan_comments"),
  work_orders = c("work_orders","work_order_comments"),
  assets      = c("assets"),
  notes       = c("notes","note_comments"),
  duty        = c("duty_positions","duty_staff","duty_items","duty_matrix","duty_sub_items","duty_sub_matrix"),
  performance = c("performance_records"),
  system      = c("users","system_config","config_options","rbac_roles","rbac_permissions","rbac_role_permissions","rbac_user_roles"),
  other       = c("std_hosts","itom_data","models","information_collectors","integrations","system_monitors")
)

# 工单状态流转
ARCH_WO_FLOW <- list(
  c("pending","待处理","#f0ad4e"),
  c("assigned","已派发","#5bc0de"),
  c("processing","处理中","#337ab7"),
  c("completed","已完成","#5cb85c"),
  c("closed","已关闭","#777")
)

# RBAC 关系
ARCH_RBAC <- data.frame(
  from = c("rbac_roles","rbac_roles","rbac_permissions","users"),
  to   = c("rbac_role_permissions","rbac_user_roles","rbac_role_permissions","rbac_user_roles"),
  type = c("1:N","1:N","1:N","1:N"),
  stringsAsFactors = FALSE
)

##################
# UI
##################
system_architecture_ui <- function() {
  tags$div(style="padding-bottom:80px;font-size:13px;",
    tags$style(HTML("
      .arch-block { border-radius:8px; padding:12px 16px; margin-bottom:16px; border:1px solid #e0e0e0; }
      .arch-block h4 { margin:0 0 10px; font-size:16px; }
      .arch-tag { display:inline-block; padding:3px 12px; border-radius:12px; font-size:13px; color:white; margin:1px 3px; white-space:nowrap; }
      .arch-tag-w { padding:2px 8px; border-radius:4px; font-size:12px; margin:0 2px; display:inline-block; white-space:nowrap; }
      .arch-row { display:flex; flex-wrap:wrap; gap:8px; margin:4px 0; align-items:flex-start; }
      .arch-cell { border-radius:6px; padding:8px 12px; min-width:80px; text-align:center; }
      .arch-arrow { font-size:20px; color:#999; margin:0 6px; }
      .arch-table { width:100%; border-collapse:collapse; font-size:13px; }
      .arch-table td,.arch-table th { border:1px solid #e0e0e0; padding:6px 10px; vertical-align:top; }
      .arch-table th { background:#f5f5f5; font-weight:600; white-space:nowrap; font-size:14px; }
      .arch-table td { font-size:13px; }
      .flow-box { display:inline-block; padding:6px 14px; border-radius:6px; color:white; font-weight:bold; font-size:13px; margin:4px 2px; }
      .flow-arrow { display:inline-block; margin:0 4px; font-size:18px; color:#999; }
    ")),

    tags$h3(icon("project-diagram"), " 系统架构总览"),
    tags$p(style="color:#7f8c8d;", "HTML/CSS 色块 + 表格，本地渲染，无任何外部依赖"),
    tags$hr(),

    # ========== 1. 模块对应表 — 色块箭头 ==========
    tags$div(class="arch-block", style="background:#f0f7ff;",
      tags$h4(icon("layer-group"), " 1. 模块对应 — UI → Service → Database"),
      tags$div(
        lapply(list(
          list(icon="home",             name="首页",   color="#4caf50", svc=c("server.R"),                            db="projects, work_orders, project_tasks"),
          list(icon="project-diagram",  name="项目",   color="#2196f3", svc=c("project_management.r","project_server.r"), db="projects, phases, work_packages, tasks, task_logs"),
          list(icon="clipboard-check",  name="巡检",   color="#ff9800", svc=c("inspection_management.r","inspection_server.r"), db="inspection_plans, items, tasks, records, issues"),
          list(icon="clipboard-list",   name="工单",   color="#e91e63", svc=c("work_order.r","server.R"),               db="work_orders, work_order_comments"),
          list(icon="laptop",           name="资产",   color="#9c27b0", svc=c("asset_management.r","asset_server.r"),   db="assets"),
          list(icon="sticky-note",      name="记事",   color="#fbc02d", svc=c("note_management.r","note_server.r"),     db="notes, note_comments, note_dispatches"),
          list(icon="cogs",             name="标准化", color="#009688", svc=c("std_computer.r"),                         db="std_hosts"),
          list(icon="network-wired",    name="测试",   color="#3f51b5", svc=c("network_test.r"),                        db="config/init.json"),
          list(icon="heartbeat",        name="性能",   color="#ff5722", svc=c("sysmon_management.r","sysmon_server.r"), db="system_monitors"),
          list(icon="calendar-day",     name="日报",   color="#0288d1", svc=c("daily_report.r"),                        db="(聚合查询)"),
          list(icon="download",         name="收集器", color="#689f38", svc=c("information_collector.r"),                db="information_collectors"),
          list(icon="plug",             name="集成",   color="#5e35b1", svc=c("integration_management.r","integration_server.r"), db="integrations"),
          list(icon="database",         name="数据",   color="#ff8f00", svc=c("data_center_server.r"),                  db="(跨模块聚合)"),
          list(icon="sitemap",          name="岗职",   color="#00bcd4", svc=c("duty_matrix_management.r","duty_matrix_server.r"), db="duty_positions, staff, items, matrix, sub_*"),
          list(icon="chart-bar",        name="绩效",   color="#cddc39", svc=c("performance_management.r","performance_server.r"), db="performance_records"),
          list(icon="cogs",             name="模型",   color="#607d8b", svc=c("model_training.r"),                      db="models"),
          list(icon="chart-line",       name="可视化", color="#66bb6a", svc=c("visualization.r"),                       db="(无持久化表)"),
          list(icon="tools",            name="管理",   color="#9e9e9e", svc=c("user_management.r","system_settings.r","rbac_management.r","github_autosubmit.r"), db="users, system_config, config_options, rbac_*")
        ), function(m) {
          tags$div(class="arch-row", style="align-items:center;padding:4px 0;border-bottom:1px solid #e8e8e8;",
            tags$span(style=sprintf("padding:4px 12px;border-radius:6px;background:%s;color:white;font-weight:bold;font-size:13px;min-width:90px;text-align:center;", m$color),
              icon(m$icon), " ", m$name),
            tags$span(class="arch-arrow", "→"),
            tags$span(style="background:#e8f5e9;padding:3px 8px;border-radius:4px;font-size:12px;",
              paste(m$svc, collapse=" + ")),
            tags$span(class="arch-arrow", "→"),
            tags$span(style="background:#ffebee;padding:3px 8px;border-radius:4px;font-size:12px;color:#c62828;",
              tags$code(m$db))
          )
        })
      )
    ),

    # ========== 2. 导航结构 ==========
    tags$div(class="arch-block", style="background:#fafafa;",
      tags$h4(icon("bars"), " 2. 导航栏结构"),
      tags$div(class="arch-row",
        lapply(ARCH_MODULES, function(m) {
          tags$span(style=sprintf("border-left:3px solid %s; padding-left:6px; margin-right:10px; font-size:12px;", m$color),
            icon(m$icon), " ", m$name)
        })
      ),
      tags$div(style="margin-top:10px; font-size:12px; color:#666;",
        tags$b("管理子菜单："), "用户管理 | 系统设置 | 选项配置 | 授权管理 | GitHub | 模块清单 | 系统架构 | 个人信息"
      )
    ),

    # ========== 3. 工单状态流转 ==========
    tags$div(class="arch-block", style="background:#fffbeb;",
      tags$h4(icon("exchange-alt"), " 3. 工单状态流转"),
      tags$div(style="display:flex;flex-wrap:wrap;align-items:center;gap:0;",
        lapply(seq_along(ARCH_WO_FLOW), function(i) {
          s <- ARCH_WO_FLOW[[i]]
          tagList(
            tags$span(class="flow-box", style=sprintf("background:%s;", s[3]), s[2]),
            if (i < length(ARCH_WO_FLOW)) tags$span(class="flow-arrow", "→") else ""
          )
        })
      ),
      tags$div(style="margin-top:8px; font-size:11px; color:#999;",
        "pending(创建) → assigned(派发) → processing(处理) → completed(完成) → closed(关闭)",
        tags$br(), "可跳过: pending→processing, 任意状态→closed, closed→pending(激活)"
      )
    ),

    # ========== 4. 项目管理层级 ==========
    tags$div(class="arch-block", style="background:#f0fdf4;",
      tags$h4(icon("project-diagram"), " 4. 项目管理层级"),
      tags$div(style="display:flex;flex-wrap:wrap;gap:0;align-items:center;",
        tags$span(class="flow-box", style="background:#2196f3;", "Project 项目"),
        tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#4caf50;", "Phase 阶段"),
        tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#ff9800;", "Work Package 工作包"),
        tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#9c27b0;", "Task 任务"),
        tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#607d8b;", "Task Log 反馈"),
        tags$br(), tags$br(),
        tags$span(class="flow-box", style="background:#9c27b0;", "Task"), tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#e91e63;", "Work Order 工单(转换)")
      )
    ),

    # ========== 5. 巡检流程 ==========
    tags$div(class="arch-block", style="background:#fffbeb;",
      tags$h4(icon("clipboard-check"), " 5. 巡检管理流程"),
      tags$div(style="display:flex;flex-wrap:wrap;gap:0;align-items:center;",
        tags$span(class="flow-box", style="background:#1565c0;", "巡检计划 INS-PLAN"),
        tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#ff8f00;", "巡检任务 INS-TSK"),
        tags$span(class="flow-arrow", "→"),
        tags$span(class="flow-box", style="background:#2e7d32;", "执行巡检"),
        tags$span(class="flow-arrow", "↓")
      ),
      tags$div(style="display:flex;gap:20px;margin-top:8px;",
        tags$span(class="flow-box", style="background:#4caf50;", "巡检记录 (拍照/评分)"),
        tags$span(class="flow-box", style="background:#e53935;", "巡检异常 → 整改工单")
      )
    ),

    # ========== 6. RBAC 模型 ==========
    tags$div(class="arch-block", style="background:#f5f0ff;",
      tags$h4(icon("shield-alt"), " 6. RBAC 权限模型"),
      tags$table(class="arch-table",
        tags$tr(
          tags$th("实体"), tags$th("字段/关系"), tags$th("说明")
        ),
        tags$tr(tags$td(tags$b("rbac_roles")), tags$td("id, name, description → rbac_role_permissions (1:N), rbac_user_roles (1:N)"), tags$td("角色定义")),
        tags$tr(tags$td(tags$b("rbac_permissions")), tags$td("id, module, component, code, name → rbac_role_permissions (1:N)"), tags$td("权限码(18模块, 49条)")),
        tags$tr(tags$td(tags$b("rbac_role_permissions")), tags$td("role_id FK, permission_id FK"), tags$td("角色-权限关联表")),
        tags$tr(tags$td(tags$b("rbac_user_roles")), tags$td("user_id FK, role_id FK"), tags$td("用户-角色分配表")),
        tags$tr(tags$td(tags$b("users")), tags$td("id, username, password, display_name, role, active"), tags$td("用户表"))
      ),
      tags$div(style="margin-top:8px; font-size:12px; color:#666;",
        tags$b("核心函数: "), tags$code("rbac_check(user, code)"), " — 检查单权限(admin直接通过)",
        tags$br(), tags$b("模块级: "), tags$code("rbac_get_user_modules(user)"), " → 返回可访问模块列表 → main_ui can_access()"
      )
    ),

    # ========== 7. 数据表字段 ==========
    tags$div(class="arch-block", style="background:#f0f7ff;",
      tags$h4(icon("database"), " 7. 核心数据表 & 字段"),
      tags$table(class="arch-table",
        tags$tr(tags$th("表名"), tags$th("字段"), tags$th("说明")),
        tags$tr(tags$td(tags$code("users")), tags$td("id, username, password, display_name, role, active, created_at, updated_at"), tags$td("用户表，role支持 admin/user/it_desk/it_engineer/sys_engineer")),
        tags$tr(tags$td(tags$code("system_config")), tags$td("id, config_key, config_value, description, updated_at"), tags$td("键值对配置 (table_font_size等)")),
        tags$tr(tags$td(tags$code("config_options")), tags$td("id, category, value, label, color, sort_order, is_default, active"), tags$td("下拉选项配置 (状态/优先级/分类)")),
        tags$tr(tags$td(tags$code("rbac_roles")), tags$td("id, name, description"), tags$td("RBAC角色定义")),
        tags$tr(tags$td(tags$code("rbac_permissions")), tags$td("id, module, component, code, name"), tags$td("权限码清单 (18模块49条)")),
        tags$tr(tags$td(tags$code("rbac_role_permissions")), tags$td("id, role_id FK, permission_id FK"), tags$td("角色-权限关联")),
        tags$tr(tags$td(tags$code("rbac_user_roles")), tags$td("id, user_id FK, role_id FK"), tags$td("用户-角色分配")),
        tags$tr(tags$td(tags$code("work_orders")), tags$td("id, order_no, title, description, category, subcategory, status, priority, created_by, request_user, request_user_name, assigned_to, assigned_at, handled_by, handled_at, completed_at, resolution, created_at, updated_at"), tags$td("工单主表, ITS+日期+流水")),
        tags$tr(tags$td(tags$code("work_order_comments")), tags$td("id, work_order_id FK, comment, created_by, created_at"), tags$td("工单评论/备注")),
        tags$tr(tags$td(tags$code("projects")), tags$td("id, project_no, name, description, status, priority, start_date, end_date, created_by, created_at, updated_at"), tags$td("项目主表, PRJ+日期+流水")),
        tags$tr(tags$td(tags$code("project_phases")), tags$td("id, project_id FK, name, description, status, sort_order, start_date, end_date, created_at, updated_at"), tags$td("项目阶段")),
        tags$tr(tags$td(tags$code("project_work_packages")), tags$td("id, phase_id FK, project_id FK, name, description, status, assigned_to, sort_order, start_date, end_date, created_at, updated_at"), tags$td("工作包")),
        tags$tr(tags$td(tags$code("project_tasks")), tags$td("id, task_no, project_id FK, phase_id FK, wp_id FK, name, description, status, priority, assigned_to, is_favorite, importance, created_by, created_at, updated_at"), tags$td("任务, TSK+日期+流水")),
        tags$tr(tags$td(tags$code("project_task_logs")), tags$td("id, task_id FK, content, creator_name, created_at"), tags$td("任务执行反馈日志")),
        tags$tr(tags$td(tags$code("inspection_plans")), tags$td("id, plan_no, name, category, cycle_type, start_date, end_date, description, is_deleted, created_by, created_at, updated_at"), tags$td("巡检计划, INS-PLAN+日期+流水")),
        tags$tr(tags$td(tags$code("inspection_items")), tags$td("id, plan_id FK, item_name, check_standard, scoring_type, max_score, sort_order"), tags$td("巡检检查项")),
        tags$tr(tags$td(tags$code("inspection_item_templates")), tags$td("id, category, item_name, check_standard, scoring_type, max_score"), tags$td("检查项模板 (含33项数据中心等)")),
        tags$tr(tags$td(tags$code("inspection_tasks")), tags$td("id, task_no, plan_id FK, inspector, scheduled_date, status, is_deleted, created_at"), tags$td("巡检任务, INS-TSK+日期+流水")),
        tags$tr(tags$td(tags$code("inspection_records")), tags$td("id, task_id FK, item_id FK, result_type, score, remark, photos, is_deleted, created_by, created_at"), tags$td("巡检记录 (含拍照)")),
        tags$tr(tags$td(tags$code("inspection_issues")), tags$td("id, task_id FK, record_id FK, issue_type, description, severity, related_work_order_id, status, created_at"), tags$td("巡检异常 → 整改工单")),
        tags$tr(tags$td(tags$code("inspection_plan_comments")), tags$td("id, plan_id FK, comment, created_by, created_at"), tags$td("巡检计划评论")),
        tags$tr(tags$td(tags$code("notes")), tags$td("id, note_no, title, content, status, importance, is_pinned, reminder_at, due_date, created_by, created_at, updated_at"), tags$td("记事, NTE+日期+流水")),
        tags$tr(tags$td(tags$code("note_comments")), tags$td("id, note_id FK, content, status, created_by, created_at"), tags$td("记事评论 (status支持 completed)")),
        tags$tr(tags$td(tags$code("note_dispatches")), tags$td("id, note_id FK, user_id FK, created_at"), tags$td("记事派发目标用户")),
        tags$tr(tags$td(tags$code("assets")), tags$td("id, asset_no, hostname, ip, type, status, location, description, created_at, updated_at"), tags$td("资产管理")),
        tags$tr(tags$td(tags$code("duty_positions")), tags$td("id, name, description, created_at"), tags$td("岗职-岗位")),
        tags$tr(tags$td(tags$code("duty_staff")), tags$td("id, name, department, email, user_id FK, position_id FK"), tags$td("岗职-人员")),
        tags$tr(tags$td(tags$code("duty_items")), tags$td("id, name, category, description, sort_order, created_at"), tags$td("岗职-职责项")),
        tags$tr(tags$td(tags$code("duty_sub_items")), tags$td("id, item_id FK, name, category, description, sort_order"), tags$td("岗职-二级任务")),
        tags$tr(tags$td(tags$code("duty_matrix")), tags$td("id, position_id FK, staff_id FK, item_id FK, rbac_level"), tags$td("岗职矩阵 (负责人/执行/知晓)")),
        tags$tr(tags$td(tags$code("duty_sub_matrix")), tags$td("id, staff_id FK, sub_item_id FK, rbac_level"), tags$td("岗职二级矩阵")),
        tags$tr(tags$td(tags$code("std_hosts")), tags$td("id, hostname, ip, os, description, created_at"), tags$td("标准化主机列表")),
        tags$tr(tags$td(tags$code("performance_records")), tags$td("id, user_id FK, metric, value, period, created_at"), tags$td("绩效记录")),
        tags$tr(tags$td(tags$code("其他表")), tags$td(tags$code("itom_data, models, information_collectors, integrations, system_monitors")), tags$td("基础数据/模型/收集器/集成/性能"))
      )
    ),

    # ========== 8. 项目模块 ==========
    tags$div(class="arch-block", style="background:#e3f2fd;",
      tags$h4(icon("project-diagram"), " 8. 项目模块 — PRJ+日期+流水"),
      tags$div(style="font-size:13px;",
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 层级: "),
          tags$span(class="flow-box", style="background:#2196f3;", "Project 项目"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#4caf50;", "Phase 阶段"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#ff9800;", "WP 工作包"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#9c27b0;", "Task 任务"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#607d8b;", "Task Log 日志"), tags$br(),
          tags$span(class="flow-box", style="background:#9c27b0;", "Task"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#e91e63;", "Work Order 工单")
        ),
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("项目"), tags$td(tags$code("project_get_all/add/update/delete/get_stats")), tags$td("项目CRUD + 级联删除 + 统计")),
          tags$tr(tags$td("阶段"), tags$td(tags$code("phase_get_by_project/add/update/delete")), tags$td("阶段CRUD, sort_order排序")),
          tags$tr(tags$td("工作包"), tags$td(tags$code("wp_get_by_phase/add/update/delete")), tags$td("工作包CRUD, 可分配assigned_to")),
          tags$tr(tags$td("任务"), tags$td(tags$code("task_add/update/delete/update_status")), tags$td("TSK+日期+流水, is_favorite, importance 0~5")),
          tags$tr(tags$td("收藏"), tags$td(tags$code("task_toggle_favorite()")), tags$td("★ 收藏/取消")),
          tags$tr(tags$td("重要性"), tags$td(tags$code("task_set_importance(0~5)")), tags$td("红旗 🚩 数量")),
          tags$tr(tags$td("转工单"), tags$td(tags$code("task_convert_to_work_order()")), tags$td("任务 → 工单")),
          tags$tr(tags$td("日志"), tags$td(tags$code("task_log_add/get_by_task")), tags$td("任务执行反馈")),
          tags$tr(tags$td("分配"), tags$td(tags$code("project_get_assignable_users()")), tags$td("可分配用户列表")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#2196f3;", "proj_view"), " ",
          tags$span(class="arch-tag", style="background:#2196f3;", "proj_create"), " ",
          tags$span(class="arch-tag", style="background:#2196f3;", "proj_edit"), " ",
          tags$span(class="arch-tag", style="background:#2196f3;", "proj_delete"), " ",
          tags$span(class="arch-tag", style="background:#2196f3;", "proj_manage"))
      )
    ),

    # ========== 9. 巡检模块 ==========
    tags$div(class="arch-block", style="background:#fff3e0;",
      tags$h4(icon("clipboard-check"), " 9. 巡检模块 — INS-PLAN/INS-TSK+日期+流水"),
      tags$div(style="font-size:13px;",
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 流程: "),
          tags$span(class="flow-box", style="background:#1565c0;", "模板"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#ff8f00;", "计划"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#e65100;", "任务"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#2e7d32;", "执行"), tags$span(class="flow-arrow", "↓"), tags$br(),
          tags$span(class="flow-box", style="background:#4caf50;", "记录"), " + ",
          tags$span(class="flow-box", style="background:#e53935;", "异常"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#b71c1c;", "整改工单")
        ),
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("计划"), tags$td(tags$code("inspection_plan_get_all/add/update/delete")), tags$td("INS-PLAN, 软删除, 级联")),
          tags$tr(tags$td("检查项"), tags$td(tags$code("inspection_item_get_by_plan/add/delete")), tags$td("绑定计划, 含评分标准")),
          tags$tr(tags$td("模板"), tags$td(tags$code("inspection_template_get_by_category")), tags$td("33项数据中心等种子数据")),
          tags$tr(tags$td("任务"), tags$td(tags$code("inspection_task_generate_from_plan/get_mine")), tags$td("INS-TSK, 我的任务过滤")),
          tags$tr(tags$td("记录"), tags$td(tags$code("inspection_record_add/add_batch/get_by_task")), tags$td("含拍照(photos), 批量提交")),
          tags$tr(tags$td("异常"), tags$td(tags$code("inspection_issue_add/create_work_order")), tags$td("异常→整改工单")),
          tags$tr(tags$td("统计"), tags$td(tags$code("inspection_get_stats/get_inspectors")), tags$td("统计 + 人员选择")),
          tags$tr(tags$td("评论"), tags$td(tags$code("inspection_plan_add_comment/get_comments")), tags$td("计划评论")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#ff9800;", "insp_view"), " ",
          tags$span(class="arch-tag", style="background:#ff9800;", "insp_create"), " ",
          tags$span(class="arch-tag", style="background:#ff9800;", "insp_execute"), " ",
          tags$span(class="arch-tag", style="background:#ff9800;", "insp_manage"))
      )
    ),

    # ========== 10. 工单模块 ==========
    tags$div(class="arch-block", style="background:#fce4ec;",
      tags$h4(icon("clipboard-list"), " 10. 工单模块 — ITS+日期+流水"),
      tags$div(style="font-size:13px;",
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 实体: "),
          tags$span(class="flow-box", style="background:#e91e63;", "work_orders"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#f06292;", "work_order_comments")
        ),
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("创建"), tags$td(tags$code("work_order_add(title, desc, ...)")), tags$td("ITS+日期+3位流水, 防并发重试")),
          tags$tr(tags$td("批量"), tags$td(tags$code("work_order_batch_parse/create/delete/reopen/close")), tags$td("日报文本→工单, 批量操作Admin")),
          tags$tr(tags$td("快速"), tags$td(tags$code("work_order_parse_quick_text()")), tags$td("格式化工单文本自动解析")),
          tags$tr(tags$td("派发"), tags$td(tags$code("work_order_assign(order_id, assignee_id)")), tags$td("指派处理人")),
          tags$tr(tags$td("状态"), tags$td(tags$code("start_handle/complete/close/update_status")), tags$td("pending→assigned→processing→completed→closed")),
          tags$tr(tags$td("编辑"), tags$td(tags$code("work_order_edit()")), tags$td("编辑全部字段")),
          tags$tr(tags$td("评论"), tags$td(tags$code("work_order_add_comment/get_comments")), tags$td("评论历史")),
          tags$tr(tags$td("统计"), tags$td(tags$code("work_order_get_stats()")), tags$td("总量/待处理/已派发/处理中/已完成/已关闭")),
          tags$tr(tags$td("查找"), tags$td(tags$code("work_order_find_user_by_name()")), tags$td("姓名模糊匹配")),
          tags$tr(tags$td("配置"), tags$td(tags$code("work_order_status_choices/color/label")), tags$td("从config_options读取")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#e91e63;", "wo_view"), " ",
          tags$span(class="arch-tag", style="background:#e91e63;", "wo_create"), " ",
          tags$span(class="arch-tag", style="background:#e91e63;", "wo_edit"), " ",
          tags$span(class="arch-tag", style="background:#e91e63;", "wo_assign"), " ",
          tags$span(class="arch-tag", style="background:#e91e63;", "wo_delete"))
      )
    ),

    # ========== 11. 资产模块 ==========
    tags$div(class="arch-block", style="background:#f3e5f5;",
      tags$h4(icon("laptop"), " 11. 资产模块"),
      tags$div(style="font-size:13px;",
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("查询"), tags$td(tags$code("asset_get_all/get_by_id")), tags$td("列表+详情")),
          tags$tr(tags$td("添加"), tags$td(tags$code("asset_add(hostname, ip, type, ...)")), tags$td("弹窗添加")),
          tags$tr(tags$td("编辑"), tags$td(tags$code("asset_update()")), tags$td("弹窗编辑")),
          tags$tr(tags$td("删除"), tags$td(tags$code("asset_delete()")), tags$td("弹窗确认删除")),
          tags$tr(tags$td("扫描"), tags$td(tags$code("asset_scan()")), tags$td("从目标主机system2采集信息")),
          tags$tr(tags$td("编号"), tags$td(tags$code("asset_generate_number()")), tags$td("编号生成")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#9c27b0;", "asset_view"), " ",
          tags$span(class="arch-tag", style="background:#9c27b0;", "asset_manage"))
      )
    ),

    # ========== 12. 记事模块 ==========
    tags$div(class="arch-block", style="background:#fffde7;",
      tags$h4(icon("sticky-note"), " 12. 记事模块 — 数据模型 & 业务逻辑"),
      tags$div(style="font-size:13px;",
        # 实体
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 实体关系："),
          tags$span(class="flow-box", style="background:#fbc02d;", "notes 记事"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#4caf50;", "note_comments 评论"), tags$br(),
          tags$span(class="flow-box", style="background:#fbc02d;", "notes"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#9c27b0;", "note_dispatches 派发"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#e91e63;", "users 被派发人")
        ),
        # 编号规则
        tags$div(style="margin-bottom:10px;",
          tags$b("🔢 编号: "), tags$code("NTE + YYYYMMDD + 3位流水"), " (note_generate_number)"
        ),
        # 核心功能
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("创建"), tags$td(tags$code("note_add(text)")), tags$td("首行自动提取为 title，其余为 content")),
          tags$tr(tags$td("查看"), tags$td(tags$code("note_get_all() / note_get_by_id() / note_search()")), tags$td("列表按更新时间倒序；支持标题+评论内容搜索")),
          tags$tr(tags$td("编辑"), tags$td(tags$code("note_update() / note_patch()")), tags$td("弹窗编辑全部字段 / 快速更新状态/重要性/提醒/到期")),
          tags$tr(tags$td("评论"), tags$td(tags$code("note_comment_add() / mark_status()")), tags$td("评论支持 status=completed 标记完成")),
          tags$tr(tags$td("派发"), tags$td(tags$code("note_dispatch_set(id, user_ids)")), tags$td("Admin → 多个用户，被派发人查看记事")),
          tags$tr(tags$td("置顶"), tags$td(tags$code("note_toggle_pin()")), tags$td("最多5条同时置顶")),
          tags$tr(tags$td("提醒"), tags$td(tags$code("note_cancel_reminder() / extend_due()")), tags$td("取消提醒 / 延长到期日")),
          tags$tr(tags$td("转工单"), tags$td(tags$code("note_convert_to_work_order()")), tags$td("记事 → 工单 work_order_add")),
          tags$tr(tags$td("删除"), tags$td(tags$code("note_delete() / comment_delete()")), tags$td("软删除 (硬删除)")),
          tags$tr(tags$td("日报"), tags$td(tags$code("note_get_today()")), tags$td("今日记事供日报聚合"))
        ),
        # 权限 & 可见性
        tags$div(style="margin-top:10px;",
          tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#3f51b5;", "note_view"), " ",
          tags$span(class="arch-tag", style="background:#3f51b5;", "note_create"), " ",
          tags$span(class="arch-tag", style="background:#3f51b5;", "note_edit"), " ",
          tags$span(class="arch-tag", style="background:#3f51b5;", "note_delete"), " ",
          tags$span(class="arch-tag", style="background:#3f51b5;", "note_dispatch"),
          tags$br(),
          tags$code("note_visible_user_id()"), " 数据隔离：非admin只看到自己的记事和被派发的记事"
        ),
        # COO 优化元模式
        tags$div(style="margin-top:10px; padding:8px 12px; background:#e8f5e9; border-radius:6px;",
          tags$b("🔧 特殊机制 — 元任务模式: "),
          "记事 NTE20260606002 用于跟踪 SuperITOM2 优化需求。评论中的任务标记为 completed 后自动跳过。",
          tags$br(),
          tags$code("note_comment_mark_status()"), " → status='completed' → 下次查询 WHERE status != 'completed'"
        )
      )
    ),

    # ========== 13. 标准化模块 ==========
    tags$div(class="arch-block", style="background:#e0f2f1;",
      tags$h4(icon("cogs"), " 13. 标准化模块"),
      tags$div(style="font-size:13px;",
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("主机"), tags$td(tags$code("std_hosts_data() / std_host_add()")), tags$td("主机列表管理")),
          tags$tr(tags$td("测试"), tags$td(tags$code("std_ping_test()")), tags$td("Ping连通性测试")),
          tags$tr(tags$td("执行"), tags$td(tags$code("std_execute_script()")), tags$td("远程执行PS脚本")),
          tags$tr(tags$td("脚本"), tags$td(tags$code("STD/1~4_*.ps1")), tags$td("WinRM/主机信息/重命名/加域/本地管理")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#009688;", "std_view"), " ",
          tags$span(class="arch-tag", style="background:#009688;", "std_manage"))
      )
    ),

    # ========== 14. 测试模块 ==========
    tags$div(class="arch-block", style="background:#e8eaf6;",
      tags$h4(icon("network-wired"), " 14. 测试/网络巡检模块"),
      tags$div(style="font-size:13px;",
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("Ping"), tags$td(tags$code("network_test_ping()")), tags$td("ping nt_target")),
          tags$tr(tags$td("DNS"), tags$td(tags$code("network_test_nslookup()")), tags$td("nslookup nt_target")),
          tags$tr(tags$td("路由"), tags$td(tags$code("network_test_tracert()")), tags$td("tracert nt_target")),
          tags$tr(tags$td("域控"), tags$td(tags$code("network_test_nltest()")), tags$td("nltest nt_domain")),
          tags$tr(tags$td("HTTP"), tags$td(tags$code("network_test_curl()")), tags$td("curl nt_http_target")),
          tags$tr(tags$td("端口"), tags$td(tags$code("network_test_port()")), tags$td("TCP socketConnection")),
          tags$tr(tags$td("网卡"), tags$td(tags$code("network_test_ipconfig()")), tags$td("本机网络配置")),
          tags$tr(tags$td("文件服务器"), tags$td(tags$code("network_test_file_server()")), tags$td("10.10.50.50/10.10.50.150")),
          tags$tr(tags$td("综合"), tags$td(tags$code("network_test_all()")), tags$td("全部项目一次测试")),
          tags$tr(tags$td("应用系统"), tags$td(tags$code("nt_run_app_ecs / nt_run_app_custom")), tags$td("Test-NetConnection 端口测试 + Ping + DNS, 支持自定义参数")),
          tags$tr(tags$td("编码"), tags$td(tags$code("iconv(result, GBK→UTF-8)")), tags$td("Windows命令输出编码转换")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#3f51b5;", "ntest_view"), " ",
          tags$span(class="arch-tag", style="background:#3f51b5;", "ntest_run"))
      )
    ),

    # ========== 15. 性能模块 ==========
    tags$div(class="arch-block", style="background:#fbe9e7;",
      tags$h4(icon("heartbeat"), " 15. 性能监控模块"),
      tags$div(style="font-size:13px;",
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("查询"), tags$td(tags$code("sysmon_get_all()")), tags$td("监控项列表")),
          tags$tr(tags$td("添加"), tags$td(tags$code("sysmon_add()")), tags$td("添加监控项")),
          tags$tr(tags$td("更新"), tags$td(tags$code("sysmon_update()")), tags$td("更新监控项")),
          tags$tr(tags$td("删除"), tags$td(tags$code("sysmon_delete()")), tags$td("删除监控项")),
          tags$tr(tags$td("采集"), tags$td(tags$code("sysmon_collect()")), tags$td("采集性能数据")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#ff5722;", "sysmon_view"), " ",
          tags$span(class="arch-tag", style="background:#ff5722;", "sysmon_manage"))
      )
    ),

    # ========== 16. 日报模块 ==========
    tags$div(class="arch-block", style="background:#e1f5fe;",
      tags$h4(icon("calendar-day"), " 16. 日报模块"),
      tags$div(style="font-size:13px;",
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 数据源: "),
          tags$span(class="arch-tag", style="background:#e91e63;", "work_orders"), " + ",
          tags$span(class="arch-tag", style="background:#9c27b0;", "project_tasks"), " + ",
          tags$span(class="arch-tag", style="background:#fbc02d;", "notes"), " + ",
          tags$span(class="arch-tag", style="background:#607d8b;", "task_logs")
        ),
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("工单"), tags$td(tags$code("daily_report_get_work_orders()")), tags$td("按日期提取工单(创建/处理/完成)")),
          tags$tr(tags$td("任务"), tags$td(tags$code("daily_report_get_tasks()")), tags$td("按日期提取任务")),
          tags$tr(tags$td("日志"), tags$td(tags$code("daily_report_get_task_logs()")), tags$td("任务反馈日志")),
          tags$tr(tags$td("记事"), tags$td(tags$code("daily_report_get_note_comments()")), tags$td("记事评论按用户聚合")),
          tags$tr(tags$td("复制"), tags$td(tags$code("dr_copy_text")), tags$td("复制纯文本日报: 工作日志 日期 (N条) 姓名")),
          tags$tr(tags$td("格式"), tags$td(tags$code("dr_cn_number()")), tags$td("中文序号: 一、二、三、...")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#0288d1;", "dr_view"))
      )
    ),

    # ========== 17. 岗职模块 ==========
    tags$div(class="arch-block", style="background:#e0f7fa;",
      tags$h4(icon("sitemap"), " 17. 岗职矩阵模块"),
      tags$div(style="font-size:13px;",
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 实体: "),
          tags$span(class="flow-box", style="background:#00bcd4;", "positions 岗位"), " × ",
          tags$span(class="flow-box", style="background:#26c6da;", "staff 人员"), " × ",
          tags$span(class="flow-box", style="background:#4dd0e1;", "items 职责"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#80deea;", "matrix RBAC级别"), tags$br(),
          tags$span(class="flow-box", style="background:#00bcd4;", "sub_items 二级任务"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#80deea;", "sub_matrix 二级矩阵")
        ),
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("岗位"), tags$td(tags$code("duty_position_get_all/add/update/delete")), tags$td("岗位CRUD")),
          tags$tr(tags$td("人员"), tags$td(tags$code("duty_staff_get_all/add/update/delete")), tags$td("从系统用户选择")),
          tags$tr(tags$td("职责"), tags$td(tags$code("duty_item_get_all/add/update/delete")), tags$td("职责项CRUD, 含排序")),
          tags$tr(tags$td("二级"), tags$td(tags$code("duty_sub_item_add/update/delete")), tags$td("二级任务CRUD")),
          tags$tr(tags$td("矩阵"), tags$td(tags$code("duty_matrix_get/set/delete")), tags$td("岗位×人员×职责, RBAC级别")),
          tags$tr(tags$td("二级矩阵"), tags$td(tags$code("duty_sub_matrix_get/set/delete")), tags$td("二级任务矩阵")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#00bcd4;", "duty_view"), " ",
          tags$span(class="arch-tag", style="background:#00bcd4;", "duty_manage"))
      )
    ),

    # ========== 18. 绩效模块 ==========
    tags$div(class="arch-block", style="background:#f9fbe7;",
      tags$h4(icon("chart-bar"), " 18. 绩效模块 — 指标×员工×月度计分"),
      tags$div(style="font-size:13px;",
        tags$div(style="margin-bottom:10px;",
          tags$b("📦 实体: "),
          tags$span(class="flow-box", style="background:#cddc39;", "perf_sheets 月表"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#afb42b;", "perf_work_items 工作项"), tags$br(),
          tags$span(class="flow-box", style="background:#cddc39;", "perf_sheets"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#827717;", "perf_results 评定"), tags$br(),
          tags$span(class="flow-box", style="background:#cddc39;", "perf_sheets"), tags$span(class="flow-arrow", "→"),
          tags$span(class="flow-box", style="background:#9e9d24;", "sheet_employees 表内员工")
        ),
        tags$div(style="margin-bottom:10px;",
          tags$b("📊 指标体系 (PERF_INDICATORS): "),
          tags$code("A1 用户投诉"), " (扣分制, L1普通1分/L2严重3分/L3重大5分) | ",
          tags$code("A2 工作质量"), " | ",
          tags$code("B1 任务响应"), " | ",
          tags$code("B2 工作完成率"), " | ",
          tags$code("C1 主动运维"), " | ",
          tags$code("C2 文档信息")
        ),
        tags$table(class="arch-table",
          tags$tr(tags$th("操作"), tags$th("函数"), tags$th("说明")),
          tags$tr(tags$td("月表"), tags$td(tags$code("perf_sheet_create/get/list/by_month")), tags$td("按 year_month 创建/查询绩效月表")),
          tags$tr(tags$td("员工"), tags$td(tags$code("perf_sheet_employee_add/list/remove")), tags$td("月表内员工管理（独立于系统用户）")),
          tags$tr(tags$td("加载"), tags$td(tags$code("perf_load_work_sources()")), tags$td("自动从工单/任务/日志/记事加载本月工作")),
          tags$tr(tags$td("工作项"), tags$td(tags$code("perf_work_item_add/update/remove")), tags$td("手动添加/编辑/删除工作项, 可关联来源")),
          tags$tr(tags$td("计分"), tags$td(tags$code("perf_calculate(sheet_id, employees)")), tags$td("按指标逐人计分, 展示A/B/C类得分明细")),
          tags$tr(tags$td("评定"), tags$td(tags$code("perf_result_get/set()")), tags$td("结果评定+标杆设定"))
        ),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#cddc39;", "perf_view"), " ",
          tags$span(class="arch-tag", style="background:#cddc39;", "perf_manage"))
      )
    ),

    # ========== 19. 管理模块 ==========
    tags$div(class="arch-block", style="background:#fafafa;",
      tags$h4(icon("tools"), " 19. 管理模块 — 认证 / 用户 / 系统 / RBAC / GitHub"),
      tags$div(style="font-size:13px;",
        tags$table(class="arch-table",
          tags$tr(tags$th("子模块"), tags$th("源文件"), tags$th("核心函数"), tags$th("说明")),
          tags$tr(tags$td(tags$b("认证")), tags$td(tags$code("auth.r")),
            tags$td(tags$code("auth_login/logout/login_by_id/check_user")), tags$td("用户名+密码登录, localStorage自动登录")),
          tags$tr(tags$td(tags$b("登录UI")), tags$td(tags$code("login_ui.r")),
            tags$td(tags$code("login_ui()")), tags$td("登录界面, 支持保存状态")),
          tags$tr(tags$td(tags$b("用户管理")), tags$td(tags$code("user_management.r")),
            tags$td(tags$code("user_get_all/add/update/toggle_active/update_password")), tags$td("Admin专属, 禁用/启用, 改密")),
          tags$tr(tags$td(tags$b("系统设置")), tags$td(tags$code("system_settings.r")),
            tags$td(tags$code("config_get_all/add/get_value")), tags$td("字体大小/系统配置 键值对")),
          tags$tr(tags$td(tags$b("选项配置")), tags$td(tags$code("project_management.r (config_option_*)")),
            tags$td(tags$code("config_option_choices/color/label/add/update/delete")), tags$td("状态下拉/优先级/分类等")),
          tags$tr(tags$td(tags$b("RBAC")), tags$td(tags$code("rbac_management.r")),
            tags$td(tags$code("rbac_check/get_user_modules/role_perms_set")), tags$td("18模块49权限, 模块级可见性")),
          tags$tr(tags$td(tags$b("GitHub")), tags$td(tags$code("github_autosubmit.r")),
            tags$td(tags$code("github_autosubmit/check_status/pull")), tags$td("git操作: 提交/状态/拉取")),
          tags$tr(tags$td(tags$b("清单")), tags$td(tags$code("module_inventory.r / system_architecture.r")),
            tags$td(tags$code("module_inventory_ui / system_architecture_ui")), tags$td("模块清单+架构总览(本页)")
        )),
        tags$div(style="margin-top:8px;", tags$b("🔒 权限: "),
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_users"), " ",
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_system"), " ",
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_options"), " ",
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_github"), " ",
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_rbac"), " ",
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_inventory"), " ",
          tags$span(class="arch-tag", style="background:#9e9e9e;", "admin_architecture"))
      )
    ),

    tags$hr(),
    tags$p(style="color:#999; font-size:11px; text-align:center;", "SuperITOM2 系统架构 · HTML/CSS 渲染 · 和模块清单同步更新")
  )
}
