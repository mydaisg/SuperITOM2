---
description: HungFo 元任务触发规则已迁移至独立仓库（本文件仅作指针）
alwaysApply: false
enabled: true
---

# HungFo 元任务触发规则（已迁出）

2026-11-06 拆仓后，本规则的唯一有效副本在：

`D:/GitHub/HungFo/.codebuddy/rules/05-metatask-trigger.md`

命令统一为：

```bash
cd /d/GitHub/HungFo && Rscript hf_metatask_runner.r peek
```

SuperITOM2 内嵌的 `Script/hungfo/` 挂载已移除，本仓库不再包含 HungFo 代码或数据库。
对话中出现「元任务 / 元 / metatask」触发词时，仍按用户级长期记忆里的约定立即手工执行，只是工作目录换成 `D:/GitHub/HungFo`。
