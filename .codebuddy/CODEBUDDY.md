# SuperITOM2 项目记忆

## 项目概述
- **名称**：SuperITOM2（IT运维管理系统 V2）
- **技术栈**：R + Shiny（Web框架），R 4.6.0
- **数据库**：SQLite（GH_ITOM.db），路径从 config/init.json 读取
- **配置文件**：config/init.json（含端口、数据库、平台命令、功能开关等）
- **启动方式**：run_app.R（端口 80）或 start_app.bat（端口 3838）
- **主界面**：navbarPage + 动态UI（ui.R三行代码 `uiOutput("app_ui")`，实际UI由server.R的renderUI根据登录状态动态生成）

## 目录结构
```
SuperITOM2/
├── run_app.R              # 启动入口（不要修改）
├── start_app.bat          # Windows批处理启动（端口3838）
├── global.R               # 全局配置+数据库迁移初始化（不要轻易修改）
├── ui.R                   # 动态UI入口（3行代码，不要修改核心逻辑）
├── server.R               # 服务端入口（18个source加载各模块）
├── config/                # 配置目录
│   ├── init.json          # 主配置文件
│   └── config_loader.r    # 配置加载器（不要修改）
├── Script/                # R模块脚本（25个 .r 文件）
├── STD/                   # PowerShell运维脚本（6个文件）
├── DB/                    # 数据库文件（GH_ITOM.db + init_database.sql）
├── docs/                  # 文档目录（install_packages.R）
├── Test/                  # 测试脚本（4个 .r 文件）
├── Log/                   # 运行时日志（含 network_test/ 子目录）
├── Logs/                  # 用户操作日志
├── SuperITOM_TS/          # TypeScript重构项目（独立开发）
└── .gitignore             # R项目标准忽略规则
```

### 重要约束
- **不要**在根目录创建新 .r 文件（统一放 Script/）
- **不要**创建空目录
- **不要**修改 run_app.R、ui.R、config_loader.r
- **不要**轻易修改 global.R 的核心迁移逻辑

## 包依赖
```r
library(shiny)         # Web框架
library(shinythemes)   # UI主题（cosmo）
library(DT)            # 数据表格渲染
library(RSQLite)       # SQLite数据库驱动
library(DBI)           # 数据库接口
library(ggplot2)       # 静态图表
library(plotly)        # 交互式图表
library(jsonlite)      # JSON解析（config/config_loader.r 中）
```

## 文件清单（Script/ 目录 25 个 .r 文件）

### 核心层
| 文件 | 大小 | 说明 |
|------|------|------|
| `auth.r` | 2.23 KB | 认证层：login/logout/register/auto-login |
| `login_ui.r` | 1.34 KB | 登录界面UI定义 |
| `main_ui.r` | 36.03 KB | 主界面定义（13个导航项 + URL路由JS） |
| `log_user.r` | 2.28 KB | 用户操作日志记录（文件日志） |

### 工单模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `work_order.r` | 24.73 KB | 工单数据层CRUD + 快速工单解析 |

工单服务端逻辑直接写在 `server.R` 中（约600行），无独立的 work_order_server.r 文件。

### 项目管理模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `project_management.r` | 34.09 KB | 项目/阶段/工作包/任务CRUD + 配置选项CRUD |
| `project_server.r` | 71.79 KB | 项目服务端（钻入导航、甘特图、弹窗交互） |
| `project_ui.r` | 5.9 KB | 项目UI定义（3个子标签 + 甘特图） |

### 巡检管理模块 ★核心模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `inspection_management.r` | 55.03 KB | 巡检数据层（计划/任务/记录/异常/模板/评论） |
| `inspection_server.r` | 72.83 KB | 巡检服务端（4个子标签 + 执行弹窗 + Admin管理） |
| `inspection_patrol.r` | 2.43 KB | 旧版巡检兼容（inspection_patrols表CRUD） |

流程：计划(Plan) → 任务(Task) → 执行(Execute) → 记录(Record) / 异常(Issue) → 整改工单

### 网络测试模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `network_test.r` | 19.47 KB | 网络巡检：ping/nslookup/tracert/nltest/curl/端口测试/文件服务器测试 |

### 日报模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `daily_report.r` | 15.74 KB | 自动从工单和项目任务按人提取工作记录 |

### 数据中心模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `data_center_ui.r` | 7.25 KB | 数据归集UI（5个模块卡片 + 明细区域） |
| `data_center_server.r` | 17.36 KB | 数据中心服务端（统计渲染 + 穿透链接） |

### 标准化模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `std_computer.r` | 10.69 KB | 标准化：主机管理/脚本选择/远程执行/语法高亮 |

### 系统管理模块
| 文件 | 大小 | 说明 |
|------|------|------|
| `user_management.r` | 9.04 KB | 用户CRUD（Admin专属） |
| `system_settings.r` | 2.28 KB | 系统配置（system_config表CRUD） |
| `github_autosubmit.r` | 17.02 KB | GitHub自动提交/拉取/分支管理 |
| `high_light.r` | 2.26 KB | PowerShell语法高亮显示 |

### 数据/模型/可视化模块（基础功能）
| 文件 | 大小 | 说明 |
|------|------|------|
| `data_management.r` | 1.71 KB | 基础数据管理（itom_data表CRUD） |
| `information_collector.r` | 2.39 KB | 信息收集器（information_collectors表CRUD） |
| `model_training.r` | 1.56 KB | 模型训练/管理（模拟数据） |
| `visualization.r` | 4.1 KB | 数据可视化（模拟数据 + ggplot2 + plotly） |

### 工具脚本
| 文件 | 大小 | 说明 |
|------|------|------|
| `update_datacenter_templates.r` | 6 KB | 数据中心巡检模板更新脚本（手动运行） |

## 模块加载顺序（server.R，18个 source）

```r
source("Script/auth.r")               # 1. 身份验证
source("Script/data_management.r")     # 2. 数据管理
source("Script/model_training.r")      # 3. 模型训练
source("Script/visualization.r")       # 4. 数据可视化
source("Script/user_management.r")     # 5. 用户管理
source("Script/system_settings.r")     # 6. 系统设置
source("Script/work_order.r")          # 7. 工单数据层
source("Script/project_management.r")  # 8. 项目数据层
source("Script/project_server.r")      # 9. 项目服务端
source("Script/information_collector.r") # 10. 信息收集器
source("Script/inspection_patrol.r")   # 11. 旧版巡检
source("Script/inspection_management.r")# 12. 巡检数据层
source("Script/inspection_server.r")   # 13. 巡检服务端
source("Script/login_ui.r")           # 14. 登录界面
source("Script/main_ui.r")            # 15. 主界面（内部再source子模块）
source("Script/github_autosubmit.r")  # 16. GitHub自动提交
source("Script/std_computer.r")       # 17. 标准化模块
source("Script/data_center_server.r") # 18. 数据中心
```

main_ui.r 内部还会额外 source：
```r
source("Script/std_computer.r")    # 标准化（重复source，safe）
source("Script/network_test.r")    # 网络测试
source("Script/project_ui.r")      # 项目UI
source("Script/daily_report.r")    # 日报
source("Script/data_center_ui.r")  # 数据中心UI
```

## 导航栏结构（13项）

| 序号 | 标签 | 类型 | 内容来源 |
|------|------|------|----------|
| 1 | 首页 | tabPanel | 内联：我的项目/我的工单/我的任务 |
| 2 | 项目 | tabPanel | project_ui() → 4个子标签（列表/详情/任务/甘特图）|
| 3 | 巡检 | tabPanel | 内联：5个子标签（我的任务/计划/记录/异常/已删除）|
| 4 | 工单 | tabPanel | 内联：3个子标签（列表/派发/处理）+ 编辑/创建/快速工单 |
| 5 | 标准化 | tabPanel | std_ui() |
| 6 | 测试 | tabPanel | network_test_ui() |
| 7 | 日报 | tabPanel | daily_report_ui() |
| 8 | 收集器 | tabPanel | 内联：sidebarLayout |
| 9 | 数据 | tabPanel | data_center_ui() |
| 10 | 模型 | tabPanel | 内联：sidebarLayout |
| 11 | 可视化 | tabPanel | 内联：sidebarLayout |
| 12 | 管理 | navbarMenu | 用户管理/系统设置/选项配置/GitHub（Admin专用，JS隐藏）|
| 13 | 退出 | tabPanel | 内联：确认退出按钮 |

## 数据库表（由 global.R migrate_database() 自动创建/迁移）

### 用户与系统
| 表名 | 用途 |
|------|------|
| `users` | 用户表（username, password, display_name, role, active） |
| `system_config` | 键值对配置（table_font_size, input_font_size等） |
| `config_options` | 下拉选项配置（项目/工单的状态、优先级、分类等） |

### 工单
| 表名 | 用途 |
|------|------|
| `work_orders` | 工单表（order_no, title, description, status, category, priority, request_user等）|
| `work_order_comments` | 工单评论 |

### 项目管理（4层结构）
| 表名 | 用途 |
|------|------|
| `projects` | 项目（project_no, name, status, priority, start_date, end_date）|
| `project_phases` | 阶段（project_id, name, status, sort_order）|
| `project_work_packages` | 工作包（phase_id, project_id, name, assigned_to）|
| `project_tasks` | 任务（task_no, name, priority, assigned_to, is_favorite, importance）|
| `project_task_logs` | 任务执行反馈日志 |

### 巡检管理
| 表名 | 用途 |
|------|------|
| `inspection_plans` | 巡检计划（plan_no, name, category, cycle_type, start_date, end_date, is_deleted）|
| `inspection_items` | 检查项（plan_id, item_name, check_standard, scoring_type, max_score）|
| `inspection_item_templates` | 检查项模板（含33项数据中心巡检等种子数据）|
| `inspection_tasks` | 巡检任务（task_no, plan_id, inspector, scheduled_date, status, is_deleted）|
| `inspection_records` | 巡检记录（task_id, result_type, score, remark, photos, is_deleted）|
| `inspection_issues` | 异常记录（related_work_order_id, issue_type, severity）|
| `inspection_plan_comments` | 计划评论 |
| `inspection_patrols` | 旧版巡检兼容表 |

### 其他
| 表名 | 用途 |
|------|------|
| `std_hosts` | 标准主机列表 |
| `itom_data` | 基础数据存储 |
| `models` | 模型记录 |
| `information_collectors` | 信息收集器 |

## 配置中心化

### config/init.json 关键配置项
```json
{
  "app": { "name": "SuperITOM2", "version": "1.0.0", "env": "development" },
  "server": { "port": 8080, "host": "0.0.0.0", "launch_browser": true },
  "database": { "type": "sqlite", "name": "GH_ITOM.db", "relative_path": "DB" },
  "features": { "github_autosubmit": true, "std_computer": true, "auto_migrate_db": true },
  "security": { "session_timeout_minutes": 30, "enable_local_storage_auto_login": true }
}
```

### 快捷函数
```r
get_db_path()       # 数据库完整路径
get_logs_dir()      # 日志目录
get_std_dir()       # STD脚本目录
get_git_cmd()       # git命令
get_ping_cmd()      # ping命令
get_powershell_cmd()# powershell命令
get_os()            # 操作系统检测
```

## 关键设计原则

### 动态UI架构
- ui.R 只有3行：`uiOutput("app_ui")`
- server.R 中 `renderUI` 根据 `rv$logged_in` 切换 `login_ui()` 和 `main_ui()`
- 登录后通过 `session$sendCustomMessage("toggleAdminMenu")` 控制Admin菜单显示

### 响应式刷新机制
- **reactiveVal 触发器模式**：
  - `rv$work_order_refresh_trigger`
  - `rv$proj_data_refresh`
  - `rv$inspection_refresh_trigger`
- 触发方式：`rv$xxx <- rv$xxx + 1`
- 在 renderDT/renderUI 中依赖触发器来刷新数据

### 数据库连接规范
```r
con <- db_connect()
tryCatch({
  # DB操作
}, error = function(e) {
  log_user_error(...)
  return(list(success = FALSE, message = e$message))
}, finally = {
  db_disconnect(con)  # 必须断开！
})
```

### 日志记录
- `log_user_operation(operation, target, operator, details)` - 写入 Logs/user_operations.log
- 日志格式：`[timestamp] 操作: xxx, 用户: xxx, 操作者: xxx, 详情: xxx`

### 编码处理
- Windows命令输出：`iconv(result, from = "GBK", to = "UTF-8", sub = "")`

### 工单号/项目号/任务号/巡检号格式
- 工单号：`ITS+YYYYMMDD+3位流水`（防并发重试机制）
- 项目号：`PRJ+YYYYMMDD+3位流水`
- 任务号：`TSK+YYYYMMDD+3位流水`
- 巡检计划号：`INS-PLAN-YYYYMMDD-3位流水`
- 巡检任务号：`INS-TSK-YYYYMMDD-3位流水`

### 用户角色体系
- `admin` - 管理员（全部权限）
- `it_desk` - IT服务台（可派发工单）
- `it_engineer` - IT工程师（可接收工单）
- `sys_engineer` - 系统工程师
- `user` - 普通用户

### Admin权限检查
```r
is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin"
```

### 条件渲染模式
```r
# output条件渲染（前端 conditionalPanel 使用）
output$work_order_selected <- reactive({ !is.null(rv$selected_work_order_id) })
outputOptions(output, "work_order_selected", suspendWhenHidden = FALSE)
```

### UI风格
- 主题：shinytheme("cosmo")
- 导航栏选中态：蓝色背景 `#337ab7`
- Admin菜单：默认隐藏 `.admin-menu-item { display: none }`，JS控制 `.admin-user` 切换
- 网络测试结果：暗色背景 `#1e1e1e`，成功绿色加粗，失败红色加粗
- 状态标签：圆角彩色badge样式
- 工单状态颜色：待处理(#f0ad4e) → 已派发(#5bc0de) → 处理中(#ff9800) → 已完成(#5cb85c) → 已关闭(#d9534f)

## 快速工单解析格式
```
IT服务请求 20260512 1110：
用户：谢芳材-供应链中心-副总经理
内容：两栋楼的"监控角度需要修正"...
@韩荣昌-IT部-IT工程师(Sky)
```

## 编程禁区（不可修改）
1. server.R 的 `output$app_ui <- renderUI()` 登录/主界面切换逻辑
2. ui.R 的核心结构（fluidePage + uiOutput）
3. config/config_loader.r 配置加载器
4. global.R 的 `migrate_database()` 自动建表逻辑
5. 不允许在 renderUI 中修改 rv（会触发响应式循环）

## 新增功能步骤
1. Script/ 目录下创建 `*_ui.r` 和 `*_server.r` 文件
2. 在 server.R 末尾添加 `source()` 加载新文件
3. 在 main_ui.r 对应位置添加 tabPanel/navbarMenu
4. 使用 reactiveVal 触发器控制刷新
5. 使用 `module_server(id, rv)` 模式（目前仅 data_center 使用 moduleServer）

## STD PowerShell脚本
| 文件 | 说明 |
|------|------|
| `0_winrm.ps1` | WinRM远程管理配置 |
| `1_hostinfo.ps1` | 主机信息采集 |
| `2_rename_host.ps1` | 主机重命名 |
| `3_JoinDomain_LVCC.ps1` | 加入LVCC域 |
| `4_LocalAdmin.ps1` | 本地管理员管理 |
| `hosts_new.csv` | 主机批量导入模板 |
