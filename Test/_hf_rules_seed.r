# 临时脚本：将 SuperITOM2 项目的规则/约定/记忆 分类写入 HungFo 规则环节
# 目标表：hf_records，module_key='rules'（每条 = 一个分类，title=分类名，payload=规则内容）
suppressMessages({ library(RSQLite); library(DBI) })

con <- dbConnect(RSQLite::SQLite(), "d:/GitHub/SuperITOM2/DB/GH_ITOM.db")
on.exit(dbDisconnect(con), add = TRUE)

rules <- list(
  list(no = "001", title = "项目概述", payload = paste0(
    "名称：SuperITOM2（IT运维管理系统 V2）\n",
    "技术栈：R + Shiny（Web 框架），R 4.6.x\n",
    "数据库：SQLite（GH_ITOM.db），路径从 config/init.json 读取\n",
    "主界面：navbarPage + 动态 UI（ui.R 三行 uiOutput(\"app_ui\")，实际 UI 由 server.R 的 renderUI 根据登录状态动态生成）\n",
    "导航栏结构：13 项（首页/项目/巡检/工单/标准化/测试/日报/收集器/数据/模型/可视化/管理/退出）")),

  list(no = "002", title = "目录结构", payload = paste0(
    "run_app.R 启动入口（端口 80，勿改）\n",
    "start_app.bat 批处理启动（端口 3838）\n",
    "global.R 全局配置 + 数据库迁移（勿轻易改 migrate_database）\n",
    "ui.R / server.R 入口（ui.R 核心 3 行结构勿改）\n",
    "config/ init.json 主配置 + config_loader.r 配置加载器（勿改）\n",
    "Script/ R 模块脚本（约 140 个 .r）\n",
    "STD/ PowerShell 运维脚本；DB/ 数据库文件\n",
    "docs/ Test/ Log/ Logs/ 文档与测试日志\n",
    "SuperITOM_TS/ TypeScript 重构子项目；SuperITOM_RS/ Rust 子项目\n",
    "约束：不在根目录创建新 .r（统一放 Script/）；不创建空目录；不修改 run_app.R / ui.R / config_loader.r")),

  list(no = "003", title = "启动与运行", payload = paste0(
    "启动：run_app.R（端口 80）或 start_app.bat（端口 3838）\n",
    "配置文件 config/init.json 关键段：app / server / database / features / security\n",
    "  例如 server.port、database.relative_path=DB、features.auto_migrate_db=true\n",
    "快捷函数：get_db_path() / get_logs_dir() / get_std_dir() / get_git_cmd() / get_ping_cmd() / get_powershell_cmd() / get_os()")),

  list(no = "004", title = "包依赖与模块加载", payload = paste0(
    "核心包：shiny / shinythemes(cosmo) / DT / RSQLite / DBI / ggplot2 / plotly / jsonlite / DiagrammeR\n",
    "server.R 按顺序 source 18 个核心文件（auth → data_management → model_training → visualization → user_management → system_settings → work_order → project_management → project_server → information_collector → inspection_patrol → inspection_management → inspection_server → login_ui → main_ui → github_autosubmit → std_computer → data_center_server）\n",
    "main_ui.r 内部额外 source：std_computer / network_test / project_ui / daily_report / data_center_ui\n",
    "HungFo 子项目：server.R 末尾 source Script/hungfo/hf_*.r（12 环节骨架）")),

  list(no = "005", title = "数据库表结构", payload = paste0(
    "由 global.R migrate_database() 自动创建/迁移\n",
    "用户与系统：users / system_config / config_options\n",
    "工单：work_orders / work_order_comments\n",
    "项目（4 层）：projects / project_phases / project_work_packages / project_tasks / project_task_logs\n",
    "巡检：inspection_plans / inspection_items / inspection_item_templates / inspection_tasks / inspection_records / inspection_issues / inspection_plan_comments\n",
    "记事：notes / note_comments（含 status/completed_at/parent_id）/ note_dispatches / note_kw_stats\n",
    "知识：dev_logs / ideas / idea_versions / solutions\n",
    "其他：std_hosts / itom_data / models / information_collectors\n",
    "HungFo 自包含表：hf_modules / hf_records")),

  list(no = "006", title = "关键设计原则", payload = paste0(
    "动态 UI：ui.R 仅 uiOutput(\"app_ui\")，server.R 用 renderUI 切换 login_ui()/main_ui()\n",
    "响应式刷新：reactiveVal 触发器模式（rv$xxx <- rv$xxx + 1），在 renderDT/renderUI 中依赖触发器刷新\n",
    "数据库连接规范：con <- db_connect() → tryCatch{...} finally{db_disconnect(con)}（必须断开）\n",
    "日志：log_user_operation(operation, target, operator, details) 写入 Logs/user_operations.log\n",
    "编码处理：Windows 命令输出 iconv(result, from=\"GBK\", to=\"UTF-8\", sub=\"\")\n",
    "条件渲染：output$xxx <- reactive({...}) + outputOptions(output, \"xxx\", suspendWhenHidden=FALSE)\n",
    "按钮灰化：tags$button(disabled=NA) + observe({ toggle_btn(...) })\n",
    "禁止在 renderUI 中修改 rv（会触发响应式循环）")),

  list(no = "007", title = "编号生成规范", payload = paste0(
    "工单号：ITS+YYYYMMDD+3位流水（防并发重试）\n",
    "项目号：PRJ+YYYYMMDD+3位流水\n",
    "任务号：TSK+YYYYMMDD+3位流水\n",
    "巡检计划号：INS-PLAN-YYYYMMDD-3位流水\n",
    "巡检任务号：INS-TSK-YYYYMMDD-3位流水\n",
    "记事编号：NTE+YYYYMMDD+3位流水\n",
    "开发日志编号：DL+YYYYMMDD+3位流水（今日已有记录则递增）")),

  list(no = "008", title = "用户角色与权限", payload = paste0(
    "admin - 管理员（全部权限）\n",
    "it_desk - IT 服务台（可派发工单）\n",
    "it_engineer - IT 工程师（可接收工单）\n",
    "sys_engineer - 系统工程师\n",
    "user - 普通用户\n",
    "Admin 权限检查：is_admin <- !is.null(rv$current_user) && nrow(rv$current_user)>0 && rv$current_user$role[1]==\"admin\"\n",
    "Admin 菜单默认隐藏（.admin-menu-item display:none），JS 通过 session$sendCustomMessage 控制显示")),

  list(no = "009", title = "UI 风格与交互", payload = paste0(
    "主题：shinytheme(\"cosmo\")\n",
    "导航栏选中态：蓝色背景 #337ab7\n",
    "网络测试结果：暗色背景 #1e1e1e，成功绿色加粗、失败红色加粗\n",
    "状态标签：圆角彩色 badge 样式\n",
    "工单状态颜色：待处理#f0ad4e → 已派发#5bc0de → 处理中#ff9800 → 已完成#5cb85c → 已关闭#d9534f\n",
    "记事评论：彩虹色分级（二级/三级不一色，避免单调）+ 收缩/展开交互")),

  list(no = "010", title = "编程禁区", payload = paste0(
    "1. server.R 的 output$app_ui <- renderUI() 登录/主界面切换逻辑（不可改）\n",
    "2. ui.R 核心结构（fluidPage + uiOutput，不可改）\n",
    "3. config/config_loader.r 配置加载器（不可改）\n",
    "4. global.R 的 migrate_database() 自动建表逻辑（不可改）\n",
    "5. 不允许在 renderUI 中修改 rv（触发响应式循环）\n",
    "另：run_app.R 启动入口不可修改")),

  list(no = "011", title = "新增功能步骤", payload = paste0(
    "1. Script/ 下创建 *_ui.r 和 *_server.r\n",
    "2. server.R 末尾添加 source() 加载新文件\n",
    "3. main_ui.r 对应位置加 if (can_access(\"模块名\")) tabPanel(...)\n",
    "4. 同步更新 Script/module_inventory.r（基本信息/权限码/关键函数）\n",
    "5. 在 rbac_permissions 表 INSERT 新权限码\n",
    "6. 用 reactiveVal 触发器控制刷新\n",
    "7. 保存/添加按钮用灰化模式 tags$button(disabled=NA) + observe({toggle_btn(...)})\n",
    "8. 所有变更必须写开发日志 dev_log_add(...)")),

  list(no = "012", title = "开发日志规范", payload = paste0(
    "任何代码变更（新增/修复/重构）必须写开发日志，缺一不可\n",
    "函数：dev_log_add(module, title, requirement, solution, result, requirement_en, solution_en, result_en, commit_msg, files_changed, code_snippet=NULL, operator=NULL, source_text=NULL)\n",
    "编号：DL+YYYYMMDD+3位流水（自动生成）\n",
    "英文字段规范（参照 DL20260702007）：\n",
    "  requirement_en 1-2 句英文重述需求\n",
    "  solution_en 1-2 句英文方案\n",
    "  result_en ~6 词总结\n",
    "  commit_msg ~50 字符，带 feat:/fix: 前缀（用于 git commit）\n",
    "多条元任务各自独立写一条，不合并\n",
    "需求/方案/结果中多条有序号内容要换行显示")),

  list(no = "013", title = "元任务与补录流程", payload = paste0(
    "元任务机制：查询跟踪记事（如 NTE20260606002）未完成评论（status != 'completed'），逐条分析、执行、标记完成\n",
    "处理步骤：\n",
    "  1. 查询未完成评论\n",
    "  2. 分析每条评论判断是优化（实现）还是排障（排查）\n",
    "  3. 执行任务\n",
    "  4. 标记完成 UPDATE note_comments SET status='completed', completed_at=... WHERE id=?\n",
    "  5. 写开发日志（含中英文描述和 commit_msg）\n",
    "Idea 补录：评论以「Idea」开头时，解析标题/原始日期/版本(v1,v2...)/正文，调用 idea_add / idea_add_version 写入 Idea 模块（ideas + idea_versions，保留原始日期），写开发日志（module=行业），标记评论 completed")),

  list(no = "014", title = "快速工单解析格式", payload = paste0(
    "示例格式：\n",
    "IT服务请求 20260512 1110：\n",
    "用户：谢芳材-供应链中心-副总经理\n",
    "内容：两栋楼的\"监控角度需要修正\"...\n",
    "@韩荣昌-IT部-IT工程师(Sky)\n",
    "说明：用于快速从一段文本解析出工单的标题/用户/内容/指派人等字段"))
)

# 幂等：先清除旧的 rules 记录（避免重复写入）
dbExecute(con, "DELETE FROM hf_records WHERE module_key='rules'")

for (r in rules) {
  dbExecute(con,
    "INSERT INTO hf_records (module_key, rec_no, title, payload, status, created_by, created_at, updated_at) VALUES ('rules', ?, ?, ?, '正式', 2, datetime('now','localtime'), datetime('now','localtime'))",
    params = list(paste0("HF-RULES-", r$no), r$title, r$payload))
}

n <- dbGetQuery(con, "SELECT COUNT(*) n FROM hf_records WHERE module_key='rules'")$n[1]
cat("DONE inserted", n, "rules records\n")
