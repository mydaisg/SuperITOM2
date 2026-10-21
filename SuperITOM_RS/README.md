# SuperITOM_RS — SuperITOM2 本地桌面客户端（Rust）

用 Rust 编写的本地桌面客户端，**直连现有 SQLite 数据库**（`SuperITOM2/DB/GH_ITOM.db`），无需启动任何后端服务。
首期功能：**登录 + 首页工作台**（工单统计卡片 + 我的项目 / 我的工单 / 我的任务）。

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
    ├── app.rs         # 应用状态 + 主事件循环（登录/工作台切换）
    ├── models.rs      # 数据结构
    ├── db.rs          # rusqlite 数据访问（登录 + 工作台查询）
    ├── fonts.rs       # 系统中文字体加载（避免中文显示方块）
    ├── login.rs       # 登录界面
    └── dashboard.rs   # 首页工作台界面
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
- **工作台**：顶部「你好，{显示名}」+ 刷新 / 退出；6 个工单统计卡片；我的项目 / 我的工单 / 我的任务三个折叠面板（状态 badge 颜色与 R 版一致，任务重要性用 🚩 标识）。
- **中文字体**：自动加载 `C:\Windows\Fonts` 下的微软雅黑 / 黑体 / 宋体 / 等线，无需额外配置。

## 与其他版本的关系

| 项目 | 说明 |
|------|------|
| `SuperITOM2`（R Shiny） | 原版 Web 应用 |
| `SuperITOM_TS` | TypeScript 全栈重构（Web） |
| `SuperITOM_MP` | 微信小程序版（Node 后端 + 原生小程序） |
| `SuperITOM_RS`（本项目） | **本地桌面客户端**（直连 SQLite，独立运行） |

## 后续扩展方向

- 工单列表 / 详情 / 评论
- 项目 / 任务 / 巡检模块
- 记住登录账号、配置化数据库路径（设置页）
