---
description: HungFo 规则索引与执行纪律，始终生效
alwaysApply: true
enabled: true
---

# HungFo AI 规则索引

- 规则与记忆的**唯一数据源**是 HungFo 的 SQLite 库（`DB/HF_HungFo.DB`）中的 `hf_rules` / `hf_rule_items` / `hf_memory` / `hf_rules_categories`。
- 工作台提交任务时会按任务类型自动装配相关分类，通过 `--system-prompt-file` 精准注入，本目录下的 `hf-gen-*.md` 是静态兜底与移植快照。
- 需要完整规则时，在 Web 工作台「规则 → 导出规则」重新导出；不要手工编辑生成文件。
- 任何功能变更后必须写五段式开发日志：【需求】【分析】【方案】【结果】【记忆】，结果段含 Token 等消耗数据。

