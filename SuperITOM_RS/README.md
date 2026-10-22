# SuperITOM_RS — SuperITOM2 本地桌面客户端（Rust）

用 Rust 编写的本地桌面客户端，**直连现有 SQLite 数据库**（`SuperITOM2/DB/GH_ITOM.db`），无需启动任何后端服务。
已实现功能：**登录 + 首页工作台 + 工单模块 + 记事 + 开发日志 + 方案**，数据与 R 版共享同一份数据库、零迁移。

## 技术栈

| 项 | 技术 |
|----|------|
| 语言 | Rust（edition 2021） |
| GUI | `egui` / `eframe`（immediate mode，纯 Rust，跨平台） |
| 数据库 | `rusqlite`（`bundled` 特性，内置 SQLite，无需系统 sqlite3） |
| 数据 | 直接复用 `SuperITOM2/DB/GH_ITOM.db`，与 R 版共享同一份数据，零迁移 |

> 登录逻辑与现有 `Script/auth.r` 完全一致（`users` 表明文密码比对 + `active` 禁用检查），现有账号可直接登录。

## 目录结构

```
SuperITOM_RS/
├── Cargo.toml
└── src/
    ├── main.rs        # 入口 + 窗口配置
    ├── app.rs         # 应用状态 + 顶部导航 + 主事件循环（登录/各页面切换）
    ├── models.rs      # 数据结构
    ├── db.rs          # rusqlite 数据访问（登录/工作台/工单/记事/开发日志/方案）
    ├── fonts.rs       # 系统中文字体加载（避免中文显示方块）
    ├── login.rs       # 登录界面
    ├── dashboard.rs   # 首页工作台界面
    ├── workorders.rs  # 工单模块（列表/详情/创建/评论/派发/处理）
    ├── notes.rs       # 记事模块（左列表 + 右详情 + 评论）
    ├── devlogs.rs     # 开发日志模块（按日期分组列表 + 内联编辑）
    └── solutions.rs   # 方案模块（卡片列表 + 展开详情，HTML 转纯文本显示）
```

## 环境要求

1. **Rust 工具链**：通过 [rustup](https://rustup.rs/) 安装。
2. **MSVC 链接器**：`rusqlite` 的 `bundled` 特性需要 C 编译器。Windows 上需要安装
   [Visual Studio Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools/)（勾选「使用 C++ 的桌面开发」）。
   - 若使用 GNU 工具链（`x86_64-pc-windows-gnu`），则需 MinGW-w64 的 `gcc`。

## 构建与运行

```bash
cd SuperITOM_RS
cargo run                 # 开发运行（首次编译较慢，需拉取并编译 egui 等依赖）
cargo build --release     # 发布构建（生成 target/release/superitom-rs.exe）
```

> 首次编译 egui 依赖较多（数百个 crate），可能需要数分钟，属正常现象。

## 数据库路径

程序按以下优先级定位数据库：

1. 环境变量 `SUPERITOM_DB_PATH`（可指向任意 SQLite 文件）
2. 相对路径 `DB/GH_ITOM.db` → `../DB/GH_ITOM.db` → `../../DB/GH_ITOM.db`

登录界面底部会显示当前使用的数据库路径，便于确认。默认（在项目根运行 `cargo run`）会定位到 `../DB/GH_ITOM.db`，即 `SuperITOM2/DB/GH_ITOM.db`。

## 登录账号

直接使用现有 `users` 表中的账号（与 R 版同库同密码），例如 `admin`。

## 界面说明

- **登录页**：用户名 / 密码，回车或点击登录；底部显示数据库路径。
- **顶部导航**：登录后顶部常驻导航栏（工作台 / 工单 / 记事 / 开发日志 / 方案）+ 右侧退出。
- **工作台**：顶部「你好，{显示名}」+ 刷新；6 个工单统计卡片；我的项目 / 我的工单 / 我的任务三个折叠面板（状态 badge 颜色与 R 版一致，任务重要性用 🚩 标识）。
- **记事**：左列表（按状态筛选）+ 右详情双栏；新建（首行即标题）、编辑、删除、置顶（最多 5 条）、重要性 🚩、状态流转（待处理/进行中/已完成/挂起）、评论（完成标记/删除）。
- **开发日志**：顶部统计 + 模块筛选 + 搜索；按日期分组的卡片列表（编号/模块/commit 信息/需求/方案/结果/变更文件），内联新建/编辑表单（含中英文字段）。
- **方案**：搜索 + 分类筛选 + 卡片列表（点击展开正文，HTML 自动转纯文本显示）；内联新建/编辑/删除。
- **中文字体**：自动加载 `C:\Windows\Fonts` 下的微软雅黑 / 黑体 / 宋体 / 等线，无需额外配置。

## 与其他版本的关系

| 项目 | 说明 |
|------|------|
| `SuperITOM2`（R Shiny） | 原版 Web 应用 |
| `SuperITOM_TS` | TypeScript 全栈重构（Web） |
| `SuperITOM_MP` | 微信小程序版（Node 后端 + 原生小程序） |
| `SuperITOM_RS`（本项目） | **本地桌面客户端**（直连 SQLite，独立运行） |

## 后续扩展方向

- 项目 / 任务 / 巡检模块
- 记事派发（admin → 多人）、提醒/到期时间
- 记住登录账号、配置化数据库路径（设置页）
