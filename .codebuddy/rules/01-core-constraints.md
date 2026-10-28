---
description: SuperITOM2 核心约束与编程禁区，始终生效，所有改动前必读
alwaysApply: true
enabled: true
---

# 核心约束与编程禁区（SuperITOM2）

> 本规则始终注入。**任何改动前，先对照本清单自查**：你要动的东西是否在禁区内？

## 一、绝对禁区（碰了就是事故，禁止修改）

| 文件/区域 | 为什么禁止 | 例外 |
|-----------|-----------|------|
| `run_app.R` | 启动入口 | 无 |
| `ui.R` 的核心结构（`fluidPage` + `uiOutput("app_ui")`） | 动态 UI 骨架 | 无 |
| `config/config_loader.r` | 配置加载器 | 无 |
| `global.R` 的 `migrate_database()` 自动建表逻辑 | 迁移逻辑牵一发动全身 | 仅加新表字段时，在末尾追加迁移语句 |
| `server.R` 的 `output$app_ui <- renderUI()` 登录/主界面切换逻辑 | 认证入口 | 无 |

## 二、强约束（可以改，但必须遵守规则）

1. **文件位置**：新 `.r` 文件一律放 `Script/` 目录，禁止在根目录创建。
2. **禁止**在 `renderUI` 中修改 `rv`（会触发响应式循环死锁）。
3. **禁止**创建空目录。
4. **禁止**擅自重构：未经用户明确要求，不得重命名函数、合并文件、调整模块边界。
5. **禁止**擅自加依赖：引入新 R 包前必须先征得用户同意，并在 `CODEBUDDY.md` 的包依赖清单登记。

## 三、响应式规范（R/Shiny 高频事故点）

- **触发器模式**：数据变更用 `rv$xxx_trigger <- rv$xxx_trigger + 1` 触发刷新，`renderDT`/`renderUI` 内依赖该触发器。
- **DB 连接必须释放**：`db_connect()` 后必须 `tryCatch(... finally = db_disconnect(con))`，禁止裸写连接导致 DB 锁死。
- **Windows 命令输出编码**：`iconv(result, from = "GBK", to = "UTF-8", sub = "")`。

## 四、改动前强制自查（内部过一遍，不必输出）

- [ ] 是否触碰了绝对禁区？
- [ ] 是否遵守响应式规范（触发器 + finally 断开连接）？
- [ ] 是否引入了新依赖/新文件位置？
- [ ] 是否在 `renderUI` 里改了 `rv`？

**一句话原则**：能局部改，就不全局动；能追加，就不重写。
