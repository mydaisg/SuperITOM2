---
description: HungFo 元任务手工触发规则——对话收到触发词即立即执行元任务队列，不等 1 小时定时任务
alwaysApply: true
enabled: true
---

# HungFo 元任务手工触发（SuperITOM2）

> 本规则始终注入。用于绕开「1 小时定时任务」的延迟，让用户在对话里一句话立即驱动元任务执行。
> 规则与记忆的**唯一数据源**是 HungFo 工作台的规则 / 记忆子 Tab（SQLite），本文件只约定执行流程。

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
   解析输出 JSON 的 `due` 数组（每项含 `id` / `title` / `payload` / `cron` / `model` / `task_type` / `source`）。

2. **判断**：
   - `due` 为空 → 直接告诉用户「当前无待执行元任务」，并附上队列里已存在任务的状态快照，结束。
   - `due` 非空 → 逐个执行（见第 3 步）。

3. **按分类装配规则与记忆**（每个任务执行前必做）：
   ```bash
   Rscript Script/hungfo/hf_metatask_runner.r context "<task_type>"
   ```
   返回 JSON 中的 `path` 即 system prompt 文件（已按分类挑出相关规则、当前生效修订与记忆）。
   需要让 CLI 执行时加 `--system-prompt-file "<path>"`；`--model` 用 `due[].model`（为空则用账号默认）。
   这样「新增模块 / 修复功能 / 优化 UI」只会读到各自相关的规则，不浪费 Token 读全量规则。

4. **子代理隔离执行**（每个任务一个独立 sub-agent，`subagent_type="general-purpose"`）：
   - 记录开始时间、生成会话 ID `mt<id>-<时间戳>`。
   - 用 Agent 工具 spawn 子代理，prompt 自包含任务 title + payload 全文 + 注入的上下文要点，要求直接完成。
   - 返回**五段式**结果：`【需求】【分析】【方案】【结果】【记忆】`。
     `【结果】`必须包含消耗 Token 等真实数据（可从 stream-json 的 `result` 事件 / usage 字段读取，无法获得要如实写明"未取得"）；
     `【记忆】`记录本次产生的、需要跨会话长期保留的事实、决策或经验（没有则写「无新增记忆」）。
   - 记录结束时间，估算 `duration_ms`；Token 尽力填（无法获得填 0，不得编造）。

5. **写开发日志**（五段式，缺段由脚本补齐并明确标注）：
   ```bash
   Rscript Script/hungfo/hf_metatask_runner.r log5 "<任务标题>" "<需求>" "<分析>" "<方案>" "<结果>" "<记忆>"
   ```

6. **标记完成 + 上报元数据**：
   ```bash
   Rscript Script/hungfo/hf_metatask_runner.r done <任务id> "<结果摘要>" --llm "<LLM名>" --model "<指定模型>" --tin <输入token> --tout <输出token> --dur <耗时ms> --sid "<会话ID>"
   ```

7. **汇报**：执行了几个任务、各自结果、模型与 Token 元数据上报情况。

## 三、统一入口

Web 工作台、微信通道、定时自动任务、CB 会话产生的需求一律走同一张队列表，用 `source` 区分来源：

```bash
Rscript Script/hungfo/hf_metatask_runner.r enqueue "<标题>" "<内容>" --source wecom --model fast-model --tasktype ui
```

`source` 取值：`web` / `wecom` / `cron` / `cb`。`--model` 留空表示用账号默认（较贵）；简单任务应指定 `fast-model` 或本地 `custom-local:*` 模型。

## 四、关键约束

- **只在 `due` 非空时才动手**，否则静默汇报「无待执行任务」即可。
- 每个任务必须走独立子代理，禁止在同一上下文连续手写执行（保证上下文隔离、便于追踪）。
- 写数据库一律通过 `Rscript` 脚本，禁止直接用 sqlite3 手写 SQL（避免中文编码问题）。
- 执行前先读懂任务描述与注入的规则/记忆，不要臆造改动；保持项目现有代码风格与模块化结构。
- 开发日志一律**五段式**；【结果】的 Token 等消耗数据必须真实可溯源。

## 五、环境

- 数据库：`D:/GitHub/SuperITOM2/Script/hungfo/DB/HF_HungFo.DB`（可用 `HUNGFO_DB` 覆盖；不是父项目 `DB/GH_ITOM.db`）
- 命令行工具：`Script/hungfo/hf_metatask_runner.r`
  命令：`peek` / `done` / `log` / `log5` / `enqueue` / `context` / `export` / `sessions` / `usage` / `token` / `cron`
- 规则与记忆导出：`Script/hungfo/.codebuddy/rules/hf-gen-*.md`、`Script/hungfo/MEMORY.md`、`Script/hungfo/RULES.md`
