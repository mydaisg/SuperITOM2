# 模块清单 — 网站所有模块/功能/源代码/数据表映射

MODULE_INVENTORY <- list(
  list(
    module = "首页", icon = "home",
    frontend = "首页标签页 (navbarPage tabPanel)",
    source = c("server.R"),
    tables = "projects, work_orders, project_tasks (聚合查询)",
    perms = list(list(code="home_view", name="查看首页")),
    key_funcs = c(
      "work_order_get_stats() — 工单统计卡片数据",
      "project_get_all() — 我的项目列表(排除已完成/关闭)"
    )
  ),
  list(
    module = "项目", icon = "project-diagram",
    frontend = "项目标签页 → project_ui() (含甘特图)",
    source = c("Script/project_management.r", "Script/project_server.r", "Script/project_ui.r"),
    tables = "projects, project_phases, project_work_packages, project_tasks, project_task_logs",
    perms = list(
      list(code="proj_view",   name="查看项目"),
      list(code="proj_create", name="创建项目"),
      list(code="proj_edit",   name="编辑项目"),
      list(code="proj_delete", name="删除项目"),
      list(code="proj_manage", name="管理项目")
    ),
    key_funcs = c(
      "proj_visible_user_id() — 非admin返回用户ID用于数据过滤",
      "project_get_all() / project_get_by_id() — 项目查询",
      "project_generate_number() → PRJ+YYYYMMDD+3位流水",
      "project_add() / project_update() / project_delete() — 项目CRUD(级联删除)",
      "project_get_stats() — 项目统计",
      "project_get_assignable_users() — 可分配用户列表",
      "phase_get_by_project() / phase_add() — 阶段CRUD",
      "phase_update() / phase_delete() — 阶段更新/删除",
      "wp_get_by_phase() / wp_add() — 工作包CRUD",
      "wp_get_by_project() / wp_update() / wp_delete()",
      "task_get_by_wp() / task_get_by_project() / task_get_by_id() — 任务查询",
      "task_generate_number() → TSK+YYYYMMDD+3位流水",
      "task_add() / task_update() / task_delete() — 任务CRUD",
      "task_toggle_favorite() — 切换收藏(★)",
      "task_set_importance() — 设置重要性(0~5红旗)",
      "task_update_status() — 更新任务状态",
      "task_convert_to_work_order() — 任务转工单",
      "task_log_add() / task_log_get_by_task() — 任务反馈日志",
      "phase_get_all() — 全局阶段(含项目名)",
      "task_get_all_global() — 全局任务(含项目/阶段/工作包名)"
    )
  ),
  list(
    module = "巡检", icon = "clipboard-check",
    frontend = "巡检标签页 (5个子标签: 我的任务/计划/记录/异常/已删除)",
    source = c("Script/inspection_management.r", "Script/inspection_server.r", "Script/main_ui.r"),
    tables = "inspection_plans, inspection_items, inspection_item_templates, inspection_tasks, inspection_records, inspection_issues, inspection_plan_comments",
    perms = list(
      list(code="insp_view",    name="查看巡检"),
      list(code="insp_create",  name="创建巡检"),
      list(code="insp_execute", name="执行巡检"),
      list(code="insp_manage",  name="管理巡检")
    ),
    key_funcs = c(
      "insp_visible_user_id() — 非admin返回用户ID用于过滤",
      "generate_plan_name_from_items() — 从检查项自动生成计划名称",
      "inspection_plan_get_all() / get_by_id() — 计划查询",
      "inspection_plan_generate_no() → INS-PLAN-YYYYMMDD-XXX",
      "inspection_plan_add() / update() / delete() — 计划CRUD(软删除)",
      "inspection_template_get_by_category() — 检查项模板",
      "inspection_category_get_all() — 巡检分类列表",
      "inspection_item_get_by_plan() / add() / delete() — 检查项CRUD",
      "inspection_task_generate_no() → INS-TSK-YYYYMMDD-XXX",
      "inspection_task_generate_from_plan() — 从计划生成任务",
      "inspection_task_get_mine() — 我的待执行任务",
      "inspection_task_get_all() / get_by_id() — 任务查询",
      "inspection_task_update_status() / delete() — 任务状态/删除(软删除)",
      "inspection_record_add() — 提交单条巡检记录",
      "inspection_record_add_batch() — 批量提交(含拍照)",
      "inspection_record_get_by_task() / get_all() — 记录查询",
      "inspection_record_delete() — 记录删除(软删除)",
      "inspection_issue_add() — 记录巡检异常",
      "inspection_issue_create_work_order() — 单个异常→整改工单",
      "inspection_task_create_work_order() — 任务所有异常→整改工单",
      "inspection_issue_get_all() / get_grouped() — 异常查询(分组)",
      "inspection_issue_get_by_task() / update_status() — 异常详情/状态",
      "inspection_get_stats() — 巡检统计(排除已删除)",
      "inspection_get_inspectors() — 可派检查人员",
      "inspection_get_responsibles() — 可派负责人",
      "inspection_plan_get_deleted() — 已删除计划(Admin审计)",
      "inspection_record_get_deleted() — 已删除记录(Admin审计)",
      "inspection_plan_add_comment() / get_comments() — 计划评论"
    )
  ),
  list(
    module = "工单", icon = "clipboard-list",
    frontend = "工单标签页 (列表/派发/处理/批量/快速工单/新建弹窗)",
    source = c("Script/work_order.r", "server.R"),
    tables = "work_orders, work_order_comments",
    perms = list(
      list(code="wo_view",   name="查看工单"),
      list(code="wo_create", name="创建工单"),
      list(code="wo_edit",   name="编辑工单"),
      list(code="wo_assign", name="派发工单"),
      list(code="wo_delete", name="删除工单")
    ),
    key_funcs = c(
      "wo_visible_user_id() — 非admin返回其ID过滤数据",
      "work_order_get_all() — 获取工单列表(含状态筛选+描述截断)",
      "work_order_get_by_id() — 获取工单详情",
      "work_order_generate_number() → ITS+YYYYMMDD+3位流水(防并发)",
      "work_order_add() — 创建工单",
      "work_order_assign() — 派发工单指派处理人",
      "work_order_start_handle() — 开始处理(→processing)",
      "work_order_complete() — 完成工单(→completed+解决方案)",
      "work_order_close() — 关闭工单(任意状态下+关闭原因)",
      "work_order_update_status() — 强制更新状态",
      "work_order_delete() — 删除工单",
      "work_order_edit() — 编辑所有字段(Admin专用)",
      "work_order_get_stats() — 工单统计(总量/待处理/已派发/处理中/已完成/已关闭)",
      "work_order_get_assignable_users() — 可派发用户列表",
      "work_order_add_comment() — 添加工单评论/备注",
      "work_order_get_comments() — 获取评论历史",
      "work_order_parse_quick_text() — 快速工单文本解析",
      "work_order_find_user_by_name() — 按姓名模糊查找用户ID",
      "work_order_batch_parse() — 批量补工单:日报文本解析",
      "work_order_batch_create() — 批量补工单:直接插入closed工单",
      "work_order_batch_delete() — 批量删除工单",
      "work_order_batch_reopen() — 批量激活(→pending)",
      "work_order_batch_close() — 批量关闭(→closed)",
      "init_work_order_config_options() — 工单配置选项初始化",
      "work_order_status_choices() — 状态下拉选项(含全部)",
      "work_order_status_color() — 状态→颜色(#f0ad4e等)",
      "work_order_status_label() — 状态→中文(待处理/已派发等)"
    )
  ),
  list(
    module = "资产", icon = "laptop",
    frontend = "资产标签页 → tabsetPanel(资产列表, 工位图) → asset_ui()",
    source = c("Script/asset_management.r", "Script/asset_server.r", "Script/asset_ui.r",
               "Script/seat_map_management.r", "Script/seat_map_server.r"),
    tables = "assets, seat_buildings, seat_floors, seat_zones, seats",
    perms = list(
      list(code="asset_view",      name="查看资产"),
      list(code="asset_manage",    name="管理资产"),
      list(code="seat_map_view",   name="查看工位图"),
      list(code="seat_map_manage", name="管理工位")
    ),
    key_funcs = c(
      "asset_generate_number() — 资产编号生成",
      "asset_get_all/add/update/delete() — 资产CRUD",
      "building_get_all/add/update/delete() — 楼栋CRUD",
      "floor_get_all/add/update/delete() — 楼层CRUD",
      "zone_get_all/add/update/delete() — 区域CRUD",
      "seat_get_all/add/update/delete() — 工位CRUD",
      "seat_batch_generate() — 批量生成工位",
      "seat_floor_snapshot() — 楼层快照(工位图渲染)"
    )
  ),
  list(
    module = "记事", icon = "sticky-note",
    frontend = "记事标签页 → note_ui()",
    source = c("Script/note_management.r", "Script/note_server.r", "Script/note_ui.r"),
    tables = "notes, note_comments",
    perms = list(
      list(code="note_view",     name="查看记事"),
      list(code="note_create",   name="创建记事"),
      list(code="note_edit",     name="编辑记事"),
      list(code="note_delete",   name="删除记事"),
      list(code="note_dispatch", name="派发记事")
    ),
    key_funcs = c(
      "note_visible_user_id() — 非admin返回其ID用于过滤",
      "note_check_ownership() — 检查所有权(非admin只能操作自己的)",
      "note_generate_number() → NTE+YYYYMMDD+3位流水",
      "note_get_all() — 获取所有记事(按更新时间排序)",
      "note_search() — 搜索记事(标题+评论内容)",
      "note_search_get_matching_comments() — 搜索时获取匹配评论",
      "note_get_by_id() — 获取单条记事",
      "note_add() — 新增记事(首行自动提取为title)",
      "note_fill_missing_no() — 补充旧数据缺失的note_no",
      "note_update() — 更新记事(编辑弹窗)",
      "note_patch() — 快速更新字段(状态/重要性/提醒/到期)",
      "note_cancel_reminder() — 取消提醒",
      "note_extend_due() — 延长到期日期",
      "note_toggle_pin() — 置顶切换(最多5条)",
      "note_delete() — 删除记事",
      "note_convert_to_work_order() — 转工单",
      "note_comment_add() — 添加评论",
      "note_comment_mark_status() — 评论状态标记(completed等)",
      "note_comment_get_by_id() / get_last() / get_all() — 评论查询",
      "note_comment_update() / update_time() / delete() — 评论CRUD",
      "note_get_today() — 今日记事(供日报用)",
      "note_get_top_keywords() — Top N关键字提取(快速筛选)",
      "note_dispatch_set() — 记事派发(admin→多个user)",
      "note_dispatch_get_users() — 获取派发目标用户",
      "note_is_dispatched_user() — 检查是否被派发者"
    )
  ),
  list(
    module = "标准化", icon = "cogs",
    frontend = "标准化标签页 → std_ui()",
    source = c("Script/std_computer.r"),
    tables = "std_hosts",
    perms = list(
      list(code="std_view",   name="查看标准化"),
      list(code="std_manage", name="管理标准化")
    ),
    key_funcs = c(
      "std_hosts_data() — 主机列表数据(reactiveVal)",
      "std_host_add() — 添加主机",
      "std_ping_test() — Ping连通性测试",
      "std_execute_script() — 执行PowerShell脚本",
      "std_server() — 标准化模块服务端入口",
      "std_ui() — 标准化模块UI",
      "PowerShell: STD/0_winrm.ps1 — WinRM配置",
      "PowerShell: STD/1_hostinfo.ps1 — 主机信息采集",
      "PowerShell: STD/2_rename_host.ps1 — 主机重命名",
      "PowerShell: STD/3_JoinDomain_LVCC.ps1 — 加入LVCC域",
      "PowerShell: STD/4_LocalAdmin.ps1 — 本地管理员管理"
    )
  ),
  list(
    module = "测试", icon = "network-wired",
    frontend = "测试标签页 → network_test_ui()",
    source = c("Script/network_test.r"),
    tables = "- (读取config/init.json配置)",
    perms = list(
      list(code="ntest_view", name="查看测试"),
      list(code="ntest_run",  name="运行测试")
    ),
    key_funcs = c(
      "network_test_ping() — Ping测试(含统计)",
      "network_test_nslookup() — DNS解析测试",
      "network_test_tracert() — 路由追踪",
      "network_test_nltest() — 域控/信任关系测试",
      "network_test_curl() — HTTP连通性测试",
      "network_test_ipconfig() — 本机网络配置",
      "network_test_port() — TCP端口连通性测试(socketConnection)",
      "network_test_file_server() — 文件服务器连通性(10.10.50.50/10.10.50.150)",
      "network_test_all() — 综合测试(全部项目)",
      "network_test_server() — 测试模块服务端",
      "iconv(result, from='GBK', to='UTF-8') — Windows编码转换"
    )
  ),
  list(
    module = "性能", icon = "heartbeat",
    frontend = "性能标签页 → sysmon_ui()",
    source = c("Script/sysmon_management.r", "Script/sysmon_server.r", "Script/sysmon_ui.r"),
    tables = "system_monitors",
    perms = list(
      list(code="sysmon_view",   name="查看性能监控"),
      list(code="sysmon_manage", name="管理性能监控")
    ),
    key_funcs = c(
      "sysmon_get_all() — 监控项列表",
      "sysmon_add() — 添加监控项",
      "sysmon_update() — 更新监控项",
      "sysmon_delete() — 删除监控项",
      "sysmon_collect() — 采集性能数据",
      "sysmon_server() — 性能模块服务端",
      "sysmon_ui() — 性能模块UI"
    )
  ),
  list(
    module = "日报", icon = "calendar-day",
    frontend = "日报标签页 → daily_report_ui()",
    source = c("Script/daily_report.r"),
    tables = "- (从work_orders + project_tasks + notes聚合)",
    perms = list(
      list(code="dr_view", name="查看日报")
    ),
    key_funcs = c(
      "daily_report_get_by_date() — 按日期提取日报",
      "daily_report_get_by_person() — 按人提取工作记录",
      "daily_report_copy_text() — 复制文本格式日报",
      "daily_report_server() — 日报模块服务端",
      "daily_report_ui() — 日报模块UI",
      "自动聚合源: 工单操作记录 + 项目任务反馈日志 + 今日记事"
    )
  ),
  list(
    module = "收集器", icon = "download",
    frontend = "收集器标签页 (sidebarLayout)",
    source = c("Script/information_collector.r", "server.R"),
    tables = "information_collectors",
    perms = list(
      list(code="collector_view",   name="查看收集器"),
      list(code="collector_create", name="创建收集器"),
      list(code="collector_manage", name="管理收集器")
    ),
    key_funcs = c(
      "info_collector_get_all() — 获取所有收集器",
      "info_collector_add() — 添加收集器"
    )
  ),
  list(
    module = "集成", icon = "plug",
    frontend = "集成标签页 → integration_ui()",
    source = c("Script/integration_management.r", "Script/integration_server.r", "Script/integration_ui.r"),
    tables = "integrations",
    perms = list(
      list(code="integration_view",   name="查看集成"),
      list(code="integration_manage", name="管理集成")
    ),
    key_funcs = c(
      "integration_get_all() — 获取所有集成",
      "integration_add() — 添加集成",
      "integration_update() — 更新集成",
      "integration_delete() — 删除集成",
      "integration_test() — 测试集成连通性",
      "integration_server() — 集成模块服务端",
      "integration_ui() — 集成模块UI"
    )
  ),
  list(
    module = "数据", icon = "database",
    frontend = "数据标签页 → data_center_ui()",
    source = c("Script/data_center_server.r", "Script/data_center_ui.r"),
    tables = "- (跨模块数据归集聚合展示)",
    perms = list(
      list(code="dc_view", name="查看数据中心")
    ),
    key_funcs = c(
      "data_center_get_stats() — 各模块统计聚合(工单/项目/巡检/测试/日报)",
      "data_center_get_detail() — 明细穿透查询",
      "data_center_server() — 数据中心服务端(moduleServer)",
      "data_center_ui() — 数据中心UI(5个卡片+明细区域)"
    )
  ),
  list(
    module = "岗职", icon = "sitemap",
    frontend = "岗职标签页 → duty_matrix_ui()",
    source = c("Script/duty_matrix_management.r", "Script/duty_matrix_server.r", "Script/duty_matrix_ui.r"),
    tables = "duty_positions, duty_staff, duty_items, duty_matrix, duty_sub_items, duty_sub_matrix",
    perms = list(
      list(code="duty_view",   name="查看岗职"),
      list(code="duty_manage", name="管理岗职")
    ),
    key_funcs = c(
      "duty_position_get_all() — 岗位列表",
      "duty_position_add() / update() / delete() — 岗位CRUD",
      "duty_staff_get_all() — 人员列表",
      "duty_staff_add() / update() / delete() — 人员CRUD",
      "duty_item_get_all() — 职责项列表",
      "duty_item_add() / update() / delete() — 职责项CRUD",
      "duty_sub_item_get_by_item() — 按上级获取二级任务",
      "duty_sub_item_get_all_with_parent() — 全部二级任务(含上级信息)",
      "duty_sub_item_add() / update() / delete() — 二级任务CRUD",
      "duty_matrix_get() — 获取岗位×人员×职责矩阵",
      "duty_matrix_set() — 设置矩阵(RBAC级别: 负责人/执行/知晓)",
      "duty_matrix_delete() — 删除矩阵条目",
      "duty_sub_matrix_get() — 获取二级矩阵",
      "duty_sub_matrix_set() — 设置二级矩阵",
      "duty_sub_matrix_delete() — 删除二级矩阵条目",
      "duty_matrix_server() — 岗职模块服务端",
      "duty_matrix_ui() — 岗职模块UI(矩阵+卡片清单)"
    )
  ),
  list(
    module = "绩效", icon = "chart-bar",
    frontend = "绩效标签页 → performance_ui()",
    source = c("Script/performance_management.r", "Script/performance_server.r", "Script/performance_ui.r"),
    tables = "performance_records",
    perms = list(
      list(code="perf_view",   name="查看绩效"),
      list(code="perf_create", name="添加绩效"),
      list(code="perf_manage", name="管理绩效")
    ),
    key_funcs = c(
      "performance_get_all() — 绩效记录列表",
      "performance_add() — 添加绩效记录",
      "performance_update() — 更新绩效记录",
      "performance_delete() — 删除绩效记录",
      "performance_get_stats() — 绩效统计数据",
      "performance_server() — 绩效模块服务端",
      "performance_ui() — 绩效模块UI"
    )
  ),
  list(
    module = "模型", icon = "cogs",
    frontend = "模型标签页 (sidebarLayout)",
    source = c("Script/model_training.r", "server.R"),
    tables = "models",
    perms = list(
      list(code="model_view",   name="查看模型"),
      list(code="model_create", name="创建模型"),
      list(code="model_manage", name="管理模型")
    ),
    key_funcs = c(
      "model_get_all() — 获取所有模型",
      "model_add() — 添加模型记录",
      "model_train() — 训练模型(含进度条+准确率)"
    )
  ),
  list(
    module = "可视化", icon = "chart-line",
    frontend = "可视化标签页 (sidebarLayout + plotly图表)",
    source = c("Script/visualization.r", "server.R"),
    tables = "- (生成图表，无持久化表)",
    perms = list(
      list(code="viz_view", name="查看可视化")
    ),
    key_funcs = c(
      "viz_generate() — 生成plotly交互式图表(支持折线/柱状/散点/饼图/热力图)",
      "renderPlotly({ viz_generate(...) }) — Shiny渲染层",
      "server.R: output$viz_mtr_complete_rate/timeout_rate/avg_duration — 流程监控指标卡片"
    )
  ),
  list(
    module = "管理", icon = "tools",
    frontend = "管理 dropdown菜单 (navbarMenu: 用户/系统/选项/授权/GitHub/模块清单/个人信息)",
    source = c("Script/user_management.r", "Script/system_settings.r", "Script/rbac_management.r", "Script/github_autosubmit.r", "Script/module_inventory.r", "server.R"),
    tables = "users, system_config, config_options, rbac_roles, rbac_permissions, rbac_role_permissions, rbac_user_roles",
    perms = list(
      list(code="admin_users",   name="用户管理"),
      list(code="admin_system",  name="系统设置"),
      list(code="admin_options", name="选项配置"),
      list(code="admin_github",  name="GitHub操作"),
      list(code="admin_rbac",    name="授权管理"),
      list(code="admin_inventory", name="模块清单"),
      list(code="admin_architecture", name="系统架构")
    ),
    key_funcs = c(
      "user_get_all() / user_add() — 用户列表/添加",
      "user_update() / user_delete() — 用户更新/删除",
      "user_toggle_active() — 禁用/启用用户",
      "user_update_password() — 修改密码",
      "config_get_all() / config_add() — 系统配置CRUD",
      "config_get_value() — 获取配置值(含默认值)",
      "config_option_get() / choices() / label() / color() / default() — 选项配置查询",
      "config_option_add() / update() / delete() — 选项配置CRUD(含激活开关)",
      "config_option_categories() — 所有配置类别",
      "rbac_permission_get_all() — 权限清单(38条)",
      "rbac_role_get_all() / add() / update() / delete() — 角色CRUD",
      "rbac_role_perms_get() / set() — 角色权限查询/设置",
      "rbac_user_roles_get() / set() — 用户角色分配",
      "rbac_user_get_all() — 用户列表(含角色分配)",
      "rbac_check() — 单权限项检查(admin直接通过)",
      "rbac_get_user_modules() — 获取用户可访问模块列表",
      "rbac_all_modules() — 获取全部RBAC注册模块(18个)",
      "github_autosubmit() — Git提交所有更改",
      "github_check_status() — 查看Git状态",
      "github_pull() — 拉取最新代码",
      "module_inventory_ui() — 模块清单页面(本页)"
    )
  )
)

# 导航栏图标映射
NAV_ICONS <- list(
  "首页" = "home", "项目" = "project-diagram", "巡检" = "clipboard-check",
  "工单" = "clipboard-list", "资产" = "laptop", "记事" = "sticky-note",
  "标准化" = "cogs", "测试" = "network-wired", "性能" = "heartbeat",
  "日报" = "calendar-day", "收集器" = "download", "集成" = "plug",
  "数据" = "database", "岗职" = "sitemap", "绩效" = "chart-bar",
  "模型" = "cogs", "可视化" = "chart-line", "管理" = "tools"
)

# 读取文件时间戳
source_file_info <- function(filepath) {
  full <- file.path(getwd(), filepath)
  if (file.exists(full)) {
    fi <- file.info(full)
    sprintf("<span style='color:#337ab7;'>%s</span> <span style='font-size:11px;color:#999;'>创建 %s · 修改 %s</span>",
      filepath,
      format(fi$ctime, "%Y-%m-%d"),
      format(fi$mtime, "%Y-%m-%d"))
  } else {
    sprintf("<span style='color:#337ab7;'>%s</span> <span style='font-size:11px;color:#ccc;'>(文件未找到)</span>", filepath)
  }
}

# 渲染模块清单 UI
module_inventory_ui <- function() {
  tags$div(style = "padding-bottom: 80px;",
    tags$style(HTML("
      .mi-col { column-count:2; column-gap:10px; }
      .mi-card { break-inside:avoid; display:block; margin-bottom:4px; border:1px solid #d0d0d0; border-radius:4px; overflow:hidden; }
      .mi-mod-hdr { background:#e8f0fe; padding:6px 12px; margin:0; cursor:pointer; border-radius:0; font-weight:700; font-size:15px; user-select:none; border:none; }
      .mi-mod-hdr:hover { background:#d4e4fc; }
      .mi-l2-hdr { background:#f5f5f5; padding:4px 10px; margin:2px 0; cursor:pointer; border-radius:3px; font-weight:600; font-size:13px; user-select:none; }
      .mi-l2-hdr:hover { background:#e8e8e8; }
      .mi-info { padding:2px 16px; font-size:13px; color:#555; line-height:1.8; }
      .mi-code { background:#fff; border:1px solid #ddd; border-radius:3px; padding:1px 6px; font-family:monospace; font-size:12px; margin:0 2px; display:inline-block; }
      .mi-func { font-family:monospace; font-size:12px; color:#337ab7; display:block; }
    ")),
    tags$div(style="display:flex; align-items:center; gap:12px;",
      tags$h3(icon("sitemap"), " 模块清单 — 全站模块/功能/源代码/数据表映射", style="margin:0;"),
      actionButton("mi_refresh_btn", "刷新清单", icon=icon("sync"), class="btn-info btn-sm")
    ),
    tags$p(style="color:#7f8c8d; font-size:13px;", "点模块名展开全部子项 · 按导航栏顺序排列 · 共18个模块 · 每次刷新重新读取文件时间戳"),
    hr(),
    tags$div(class="mi-col",
      lapply(seq_along(MODULE_INVENTORY), function(i) {
        m <- MODULE_INVENTORY[[i]]
        mod_id <- paste0("mi-mod-", i)
        icon_name <- m$icon %||% "cube"
        perms_str <- paste(sapply(m$perms, function(p) sprintf('<span class="mi-code">[%s] %s</span>', p$code, p$name)), collapse = " ")

        tags$div(class="mi-card",
          # 模块头 — 点击展开/折叠全部子项
          tags$div(class="mi-mod-hdr",
            onclick = sprintf("
              var c=document.getElementById('%s');
              var showing=c.style.display!=='block';
              c.style.display=showing?'block':'none';
              var subs=c.querySelectorAll('.mi-info');
              for(var i=0;i<subs.length;i++) subs[i].style.display=showing?'block':'none';
            ", mod_id),
            icon(icon_name), " ", m$module
          ),
            tags$div(id=mod_id, style="display:none; padding:4px 12px 8px;",
              tags$div(class="mi-l2-hdr", onclick=sprintf("var el=document.getElementById('%s-info');el.style.display=el.style.display==='none'?'block':'none';", mod_id), "▸ 基本信息"),
              tags$div(id=paste0(mod_id,"-info"), class="mi-info", style="display:none;",
                tags$div(tags$b("前端名称: "), m$frontend),
                tags$div(tags$b("源码文件: "), lapply(m$source, function(f) tags$div(style="padding-left:8px;", HTML(source_file_info(f))))),
                tags$div(tags$b("数据表: "), tags$code(m$tables))
              ),
              tags$div(class="mi-l2-hdr", onclick=sprintf("var el=document.getElementById('%s-perms');el.style.display=el.style.display==='none'?'block':'none';", mod_id), "▸ 权限项"),
              tags$div(id=paste0(mod_id,"-perms"), class="mi-info", style="display:none;",
                HTML(perms_str)
              ),
              tags$div(class="mi-l2-hdr", onclick=sprintf("var el=document.getElementById('%s-funcs');el.style.display=el.style.display==='none'?'block':'none';", mod_id),
                sprintf("▸ 关键函数 (%d个)", length(m$key_funcs))),
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
  )
}
