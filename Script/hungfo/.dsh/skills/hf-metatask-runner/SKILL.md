---
name: hf-metatask-runner
description: HungFo 元任务队列的读取与回写：peek 到期任务、done 标记完成并上报元数据、log 写开发日志、export 同步镜像。当需要执行或登记元任务、写开发日志、刷新 MetaTasks.json 与监控页数据时使用。
whenToUse: 用户提到"元任务""metatask"，或需要写开发日志、同步任务镜像时。
---

# HungFo 元任务执行器（DSH 版）

## 工具

```powershell
$ScriptsDir = 'D:\GitHub\SuperITOM2\Script\hungfo'
$Rscript    = 'D:\Tai_ProgramFiles\R\R-4.6.1\bin\x64\Rscript.exe'
$runner     = "$ScriptsDir\hf_metatask_runner.r"
$env:HUNGFO_DB = 'D:\GitHub\SuperITOM2\Script\hungfo\DB\HF_HungFo_DSH.DB'  # 务必指向开发库
```

## 命令

| 命令 | 作用 |
|---|---|
| `peek` | 输出到期待执行任务 JSON（`{now, due:[{id,title,payload,cron}]}`） |
| `done <id> "<结果>" [--llm --agent --tin --tout --dur --sid]` | 标记完成 + 写 `hf_task_sessions` + 重导镜像（有 cron 回 pending 并推进 next_run_at） |
| `log "<标题>" "<内容>"` | 写一条开发日志到 `hf_records`（module_key='devlog'） |
| `export` | 重导 `MetaTasks.json` |
| `sessions` | 重导 `www/hungfo_sessions.js` |
| `usage` | 采集用量统计（原 CodeBuddy 口径，DSH 下不可用，见下） |
| `cron "<expr>"` | 测试 cron 表达式下次触发时间 |

## 标准执行流程（替代原 CodeBuddy 每小时定时任务）

```powershell
# 1) 读到期任务
& $Rscript $runner peek 2>&1 | Where-Object { $_ -notmatch 'During startup|LC_|^\[1\]' }
# 解析 JSON 的 due 数组

# 2) 每个任务：用 subagent 隔离执行（上下文不堆积，等价原 sub-agent）

# 3) 写开发日志（四段式：需求 / 分析 / 方案 / 结果）
& $Rscript $runner log "<任务标题>" "<四段式内容>"

# 4) 标记完成 + 上报元数据
& $Rscript $runner done <id> "<结果摘要>" --llm deepseek-flash --agent DSH --dur <毫秒> --sid <会话ID>
```

## 关键注意

- **`due` 为空时不要动手**，直接汇报"当前无待执行元任务"。
- 拆 `due` 的判断：`cron` 为空 = 一次性；非空 = 循环任务，完成后自动回 pending。
- 输出解析前**必须过滤 R 启动警告**（`During startup` / `LC_` / `[1] "LC_COLLATE...`），否则 JSON 解析失败。
- 写库一律经 `Rscript`，**不要用 sqlite3 手写 SQL**（中文编码 + 镜像不同步双重风险）。

## 与旧环境的差异（重要）

| 项 | 原 CodeBuddy/WorkBuddy | 现在 DSH |
|---|---|---|
| 调度 | 每小时 automation | DSH 会话内按需 / 计划任务 |
| 执行 | sub-agent | `subagent` 工具 |
| `--agent` | WorkBuddy / CodeBuddy | 填 `DSH` |
| `--llm` | DeepSeek-V4-Pro 等 | 填当前 DSH 模型名 |
| `--tin/--tout` | 真实 token | DSH 不暴露，**填 0，不要伪造** |
| `usage` 命令 | 采集 CodeBuddy 私有库 | **不可用**，历史数据保留只读 |

## 触发方式变更

旧规则 `05-metatask-trigger.md` 靠"元任务"触发词自动执行。在 DSH 里**改为显式指令**：用户明确说"执行元任务"时才走上述流程，避免误触发。
