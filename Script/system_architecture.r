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
      .arch-block h4 { margin:0 0 10px; font-size:15px; }
      .arch-tag { display:inline-block; padding:2px 10px; border-radius:12px; font-size:11px; color:white; margin:1px 2px; white-space:nowrap; }
      .arch-tag-w { padding:1px 6px; border-radius:3px; font-size:10px; margin:0 2px; display:inline-block; white-space:nowrap; }
      .arch-row { display:flex; flex-wrap:wrap; gap:6px; margin:4px 0; align-items:flex-start; }
      .arch-cell { border-radius:6px; padding:8px 12px; min-width:80px; text-align:center; }
      .arch-arrow { font-size:18px; color:#999; margin:0 4px; }
      .arch-table { width:100%; border-collapse:collapse; font-size:12px; }
      .arch-table td,.arch-table th { border:1px solid #e0e0e0; padding:4px 8px; vertical-align:top; }
      .arch-table th { background:#f5f5f5; font-weight:600; white-space:nowrap; }
      .flow-box { display:inline-block; padding:6px 14px; border-radius:6px; color:white; font-weight:bold; font-size:12px; margin:4px 2px; }
      .flow-arrow { display:inline-block; margin:0 4px; font-size:16px; color:#999; }
    ")),

    tags$h3(icon("project-diagram"), " 系统架构总览"),
    tags$p(style="color:#7f8c8d;", "HTML/CSS 色块 + 表格，本地渲染，无任何外部依赖"),
    tags$hr(),

    # ========== 1. 三层架构 ==========
    tags$div(class="arch-block", style="background:#f0f7ff;",
      tags$h4(icon("layer-group"), " 1. 三层架构"),
      tags$div(class="arch-row",
        # UI Layer
        tags$div(class="arch-cell", style="background:#e3f2fd; flex:1;",
          tags$b(style="color:#1565c0;", "UI Layer (navbarPage)"), tags$br(),
          lapply(ARCH_MODULES, function(m) {
            tags$span(class="arch-tag", style=sprintf("background:%s;", m$color),
              icon(m$icon), " ", m$name)
          })
        ),
        tags$span(class="arch-arrow", "→"),
        # Service Layer
        tags$div(class="arch-cell", style="background:#e8f5e9; flex:1;",
          tags$b(style="color:#2e7d32;", "Service Layer (R)"), tags$br(),
          lapply(c("auth.r","work_order.r","project_management.r","inspection_management.r",
            "asset_management.r","note_management.r","duty_matrix_management.r",
            "performance_management.r","rbac_management.r","user_management.r",
            "system_settings.r","std_computer.r","network_test.r","daily_report.r","model_training.r",
            "visualization.r","information_collector.r","integration_management.r",
            "sysmon_management.r","data_center_server.r"),
            function(f) tags$span(class="arch-tag", style="background:#43a047;", f))
        ),
        tags$span(class="arch-arrow", "→"),
        # DB Layer
        tags$div(class="arch-cell", style="background:#ffebee; flex:1;",
          tags$b(style="color:#c62828;", "Data Layer (SQLite)"), tags$br(),
          tags$b("GH_ITOM.db — 27 tables"), tags$br(),
          lapply(names(ARCH_TABLES), function(k) {
            tagList(
              tags$div(style="font-weight:600;margin-top:4px;", k),
              tags$div(lapply(ARCH_TABLES[[k]], function(t) tags$span(class="arch-tag", style="background:#e53935;", t)))
            )
          })
        )
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

    # ========== 7. 数据表导出 ==========
    tags$div(class="arch-block", style="background:#f0f7ff;",
      tags$h4(icon("database"), " 7. 核心数据表 (27张)"),
      tags$table(class="arch-table",
        tags$tr(tags$th("模块"), tags$th("表名"), tags$th("说明")),
        tags$tr(tags$td(tags$b("用户/系统")), tags$td(tags$code("users, system_config, config_options")), tags$td("RBAC基础: rbac_roles, rbac_permissions, rbac_role_permissions, rbac_user_roles")),
        tags$tr(tags$td(tags$b("工单")), tags$td(tags$code("work_orders, work_order_comments")), tags$td("ITS+日期+流水号")),
        tags$tr(tags$td(tags$b("项目")), tags$td(tags$code("projects, project_phases, project_work_packages, project_tasks, project_task_logs")), tags$td("PRJ/TSK+日期+流水号, 4级层级")),
        tags$tr(tags$td(tags$b("巡检")), tags$td(tags$code("inspection_plans, inspection_items, inspection_item_templates, inspection_tasks, inspection_records, inspection_issues, inspection_plan_comments")), tags$td("INS-PLAN/INS-TSK+日期+流水号")),
        tags$tr(tags$td(tags$b("资产")), tags$td(tags$code("assets")), tags$td("asset_generate_number()")),
        tags$tr(tags$td(tags$b("记事")), tags$td(tags$code("notes, note_comments")), tags$td("NTE+日期+流水号, 派发表 note_dispatches")),
        tags$tr(tags$td(tags$b("岗职")), tags$td(tags$code("duty_positions, duty_staff, duty_items, duty_matrix, duty_sub_items, duty_sub_matrix")), tags$td("三级矩阵: 岗位/人员/职责/二级")),
        tags$tr(tags$td(tags$b("其他")), tags$td(tags$code("std_hosts, itom_data, models, information_collectors, integrations, system_monitors, performance_records")), tags$td("标准化/数据/模型/收集器/集成/性能/绩效"))
      )
    ),

    tags$hr(),
    tags$p(style="color:#999; font-size:11px; text-align:center;", "SuperITOM2 系统架构 · HTML/CSS 渲染 · 和模块清单同步更新")
  )
}
