---
description: SuperITOM2 开发工作流与交付纪律，含 dev_log 强制规范，始终生效
alwaysApply: true
enabled: true
---

# 开发工作流与交付纪律（SuperITOM2）

> 本规则始终注入。**任何"新增功能/修复/重构"完成后，必须写开发日志**，否则视为未完成。

## 一、新增功能的固定步骤（按序执行）

1. 在 `Script/` 下创建 `*_ui.r` 和 `*_server.r`。
2. 在 `server.R` 末尾追加 `source()` 加载新文件。
3. 在 `main_ui.r` 对应位置添加 `if (can_access("模块名")) tabPanel(...)`。
4. ★ 同步更新 `Script/module_inventory.r`（模块基本信息、权限码、关键函数）。
5. ★ 在 `rbac_permissions` 表 `INSERT` 新权限码。
6. 用 reactiveVal 触发器控制刷新。
7. 保存/添加按钮用灰化模式：`tags$button(disabled=NA)` + `observe({ toggle_btn(...) })`。
8. ★★ 写开发日志（见下）。

## 二、开发日志（dev_log）强制规范

每次完成变更，必须调用：

```r
dev_log_add(module, title, requirement, solution, result,
            requirement_en, solution_en, result_en, commit_msg, files_changed)
```

字段要求（参照 DL20260702007）：

| 字段 | 要求 |
|------|------|
| `requirement_en` | 1-2 句 |
| `solution_en` | 1-2 句 |
| `result_en` | 约 6 词 |
| `commit_msg` | 约 50 字符，带 `feat:` / `fix:` 前缀 |

- 编号自动生成：`DL + YYYYMMDD + 3位流水`。
- **写完日志后，必须在回复中报告 `log_no`**，否则视为流程未闭环。

## 三、交付纪律（小步快跑）

- **写 → 验 → 报**：每完成一个功能点，立即做语法/构建/冒烟验证，一句话汇报结果。
- **不堆积未验证代码**：一次改多处时，先验证再继续。
- **失败即停**：同一问题连续 3 次修不好，停下来向用户说明已尝试的方案，请求协助，禁止反复盲试。

## 四、完成前自检

- [ ] 是否更新了 `module_inventory.r`？
- [ ] 是否 `INSERT` 了权限码？
- [ ] 是否写了 dev_log 并在回复中报告 log_no？
- [ ] 是否做了语法/构建验证？
