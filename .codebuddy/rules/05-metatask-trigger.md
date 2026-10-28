---
description: HungFo 元任务手工触发规则——对话收到触发词即立即执行元任务队列，不等 1 小时定时任务
alwaysApply: true
enabled: true
---

# HungFo 元任务手工触发（SuperITOM2）

> 本规则始终注入。用于绕开「1 小时定时任务」的延迟，让用户在对话里一句话立即驱动元任务执行。

## 一、触发词

当用户的**最新消息**中包含以下任一关键词时，立即进入「手工执行元任务」流程：

- `元任务`
- `执行元任务`
- `元`
- `metatask`（不区分大小写）

> 注意：仅凭触发词，无需用户再补充「做什么」。元任务内容以队列里的任务描述为准。

## 二、执行流程（严格按序）

1. **读队列**（Bash）：
   ```bash
   cd /d/GitHub/SuperITOM2 && Rscript Script/hungfo/hf_metatask_runner.r peek 2>&1 | grep -v "During startup\|LC_\|Setting\|^\[1\]"
   ```
   解析输出 JSON 的 `due` 数组（每项含 `id`/`title`/`payload`/`cron`）。

2. **判断**：
   - `due` 为空 → 直接告诉用户「当前无待执行元任务」，并附上队列里已存在任务的状态快照（`SELECT id,title,status,cron FROM hf_metatask_queue`），结束。
   - `due` 非空 → 逐个执行（见第 3 步）。

3. **子代理隔离执行**（每个任务一个独立 sub-agent，`subagent_type="general-purpose"`）：
   - 记录开始时间、生成会话 ID `mt<id>-<时间戳>`。
   - 用 Agent 工具 spawn 子代理，prompt 自包含任务 title + payload 全文，要求直接完成，并返回三段式结果（【需求】【分析】【方案】【结果】）。
   - 记录结束时间，估算 `duration_ms`；LLM 名尽力填（无法得知填 `auto`）；Token 尽力填（无法获得填 0）。

4. **写开发日志**：
   ```bash
   Rscript Script/hungfo/hf_metatask_runner.r log "<任务标题>" "<三段式结果文本>"
   ```

5. **标记完成 + 上报元数据**：
   ```bash
   Rscript Script/hungfo/hf_metatask_runner.r done <任务id> "<结果摘要>" --llm "<LLM名>" --tin <输入token> --tout <输出token> --dur <耗时ms> --sid "<会话ID>"
   ```

6. **汇报**：执行了几个任务、各自结果、元数据上报情况。

## 三、关键约束

- **只在 `due` 非空时才动手**，否则静默汇报「无待执行任务」即可。
- 每个任务必须走独立子代理，禁止在同一上下文连续手写执行（保证上下文隔离、便于追踪）。
- 写数据库一律通过 `Rscript` 脚本，禁止直接用 sqlite3 手写 SQL（避免中文编码问题）。
- 执行前先读懂任务描述，不要臆造改动；保持项目现有代码风格与模块化结构。

## 四、环境

- 数据库：`D:/GitHub/SuperITOM2/DB/GH_ITOM.db`
- 命令行工具：`Script/hungfo/hf_metatask_runner.r`（命令：`peek` / `done` / `log` / `export` / `sessions` / `cron`）
