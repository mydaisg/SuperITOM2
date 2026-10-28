# HungFo 开发日志 · Agent Workflow Monitor 迭代

> 本日志记录 Agent Workflow Monitor 可视化监控面板的完整开发过程、结果、配置方法与正式环境调试方法。
> 路径：`D:\GitHub\SuperITOM2\Script\hungfo\`

---

## 一、背景与目标

HungFo 是 SuperITOM2 下的独立子项目（R + Shiny + SQLite），包含 12 个环节（规则/爬取/数据/算法/展示/自动任务/回测/元任务/开发日志/自动架构/对外集成/权限体系），彼此独立、通过注册表/总线（`hf_bus`）互相提供接口。

本次迭代围绕「多智能体工作流可视化监控面板」展开，核心诉求：

1. 可视化多 Agent（17 个）之间的任务流转，深色科技风、霓虹流光。
2. 按「输入 → 处理 → 输出」左右分区布局。
3. 每个任务独立一条线，看到节点间的任务数量（通过线的数量突出任务规模）。
4. 柔和曲线 + 残影 + 风筝线感（稀疏、慢速、轻晃）。
5. 所有可自定义参数配置化。

---

## 二、技术选型与架构决策

| 决策点 | 结论 | 原因 |
|--------|------|------|
| 渲染技术 | 纯前端 HTML + SVG + Canvas | visNetwork 做不出霓虹流光 / 残影 / 晃动效果 |
| 嵌入方式 | `tags$iframe` 嵌入 Shiny Tab（后改为新标签页独立页面） | 静态资源自包含 |
| 静态资源 | `addResourcePath("hungfo", "Script/hungfo/www")` | 子项目自包含，不依赖主应用 www |
| 逻辑解耦 | 三个独立循环：`trafficLoop`（每帧渲染视觉）/ `taskPump`（任务推进）/ `logPump`（日志产生） | 闪烁/流光不绑定日志频率 |
| 视觉参数 | `VISUAL` 配置对象集中管理 | 所有魔法数字抽离，调参只改一处 |
| 任务来源 | 自动任务模块 + SQLite 表 `hf_agent_tasks` | 示例/正式任务统一编辑、下达 |

---

## 三、核心文件清单

| 文件 | 职责 |
|------|------|
| `Script/hungfo/www/hungfo_agent_monitor.html` | 监控面板主文件（深色科技风，约 1100 行） |
| `Script/hungfo/www/hungfo_tasks.js` | 任务数据源（由自动任务模块导出，监控页引入） |
| `Script/hungfo/hf_ui.r` | 聚合 UI + Server（自动任务专属页、Agent Workflow Monitor Tab） |
| `Script/hungfo/hf_core.r` | 核心层（注册表 + 数据库 + Agent 任务数据层） |
| `Script/hungfo/hf_mod_autotask.r` | 环节06 自动任务（任务来源 + 接口） |

---

## 四、关键实现

### 4.1 视觉配置对象（VISUAL）

```js
const VISUAL = {
  lineWidth: 2.6,        // 丝线宽度(px)
  lineOpacity: 0.5,      // 丝线透明度
  dotWidth: 5,           // 光点宽度
  dotOpacity: 0.95,      // 光点透明度
  dotDashRatio: 0.012,   // 光点占线段比例
  dotSpeed: 0.3,         // 光点游走速度
  wobbleAmp: 20,         // 晃动幅度(px)
  wobbleSpeed: 0.6,      // 晃动频率
  glowBlur: 7,           // 发光模糊半径
  ghostWidth: 1.6,       // 残影线宽
  ghostOpacity: 0.16,    // 残影初始透明度
  ghostLife: 3200,       // 残影寿命(ms)
  ghostMax: 200,         // 残影数量上限
};
```

### 4.2 模拟引擎配置（cfg）

```js
const cfg = {
  concurrency: 12,   // 并发路数
  rate: 12,          // 日志频率(条/秒)
  taskRate: 6,       // 任务推进节奏(步/秒，与日志解耦)
  blockProb: 0.08,   // 拦截概率
  total: 1284,       // 任务总量(累计计数上限，到达后归档滚动)
  paused: false,
};
```

### 4.3 双定时器解耦（核心修复）

任务推进与日志产生彻底解耦：

```js
function restartTaskPump() {
  if (taskTimer) clearInterval(taskTimer);
  taskTimer = setInterval(taskPump, Math.max(60, Math.round(1000 / cfg.taskRate)));
}
function restartLogPump() {
  if (logTimer) clearInterval(logTimer);
  logTimer = setInterval(logPump, Math.max(40, Math.round(1000 / cfg.rate)));
}
```

### 4.4 任务活动连线渲染（每任务独立一条线）

`renderTraffic()` 每帧清空重建 traffic-canvas，画两层：
- **残影层**：任务走过后残留的边，二次衰减渐隐（`(1-age)^2`）。
- **活动线层**：每个任务 = 一根丝线 + 一个稀疏游走光点，`ghostPath()` 生成带 sin 波晃动的贝塞尔曲线。

### 4.5 任务名称列表（左侧 Input 下）

- 固定比例、无下拉条（`overflow:hidden`）。
- 任务少时显示空行占位（`opacity:0` 的空 `.tl-item`）。
- 任务多时缓慢流动（CSS `@keyframes tl-scroll` marquee，双份内容 `translateY(-50%)` 无缝循环，流速约 18px/s）。

---

## 五、Bug 排查与修复记录

### Bug 1：SMIL 动画被重建打断
`renderTraffic` 每 tick 清空重建，`animateMotion` 动画跑不起来 → 改用 JS 驱动的 `stroke-dashoffset`。

### Bug 2：节点层级问题
traffic-canvas z-index 覆盖节点 → 给 `.node` 加 `z-index:2`。

### Bug 3：任务"齐步走"
所有任务同时推进导致集中在路径末尾 → 每个任务按独立概率推进。

### Bug 4：blockProb 重复绑定
两个 input listener 冲突 → 删除重复的 `bindRange`。

### Bug 5：线太细看不见
只加倍宽度没加透明度 → 宽 + 亮同时提升。

### Bug 6（本次）：长时间运行后任务只剩一条线、无动态，日志仍在
**根因**：`spawnTask()` 用 `completed + blocked + activeTasks.length >= cfg.total` 作为停止生成条件。`cfg.total` 既是展示的总量，又被当作累计完成上限。运行一段时间后 `completed + blocked` 累计到 `total`，`activeTasks` 不再补充 → 线消失、无任务动态；但日志因有兜底心跳池，仍在继续刷。

**修复**：改为「持续滚动模拟」——`total` 仅作累计计数上限，到达后自动归档重置（`completed/blocked/taskSeq` 归零，任务序列继续），保证活跃任务永远流转：

```js
function spawnTask() {
  if (completed + blocked >= cfg.total) {
    completed = 0; blocked = 0; taskSeq = 0;   // 归档重置，重新滚动
  }
  // ... 从任务池选一个生成
}
```

---

## 六、配置方法（全部参数化）

配置面板含 **3 个分组、14 个滑块**：

| 分组 | 滑块 |
|------|------|
| 模拟引擎 | 并发路数、日志频率、任务推进节奏、拦截概率、任务总量 |
| 视觉·线体 | 丝线宽度、丝线透明度、光点宽度、光点速度、晃动幅度、发光柔和度 |
| 视觉·残影 | 残影透明度、残影寿命、残影数量上限 |

通用绑定函数 `bindRange(id, valId, obj, key, fmt, onchange)` 支持绑定到 `cfg` 或 `VISUAL` 任意字段。

### 任务来源配置

- **示例任务 / 正式任务**：通过自动任务模块页编辑，存储在 `hf_agent_tasks` 表（`task_type` 字段区分）。
- **切换显示**：监控面板顶部的「示例任务 / 正式任务」切换按钮。
- **数据导出**：R 端 `hf_write_tasks_js()` 把任务导出为 `www/hungfo_tasks.js`，监控页通过 `<script src="hungfo_tasks.js">` 引入，读取 `window.HF_TASKS`。

---

## 七、正式环境调试方法

1. **启动应用**：运行 SuperITOM2 的 Shiny 应用，登录后进入 HungFo（工具菜单 → HungFo → 进入独立界面）。

2. **访问监控面板**：HungFo 导航栏 → 权限体系之后 → 「Agent Workflow Monitor」→ 点击按钮在新标签页打开独立 HTML 页面。

3. **打开浏览器开发者工具**（F12）：
   - **Console 面板**：查看 JS 报错（TDZ、undefined 等）。
   - **Network 面板**：确认 `hungfo_tasks.js` 是否 200 加载（若 404，说明 `hf_write_tasks_js()` 未执行或路径不对）。

4. **验证任务数据**：
   - 确认 `window.HF_TASKS` 已加载：Console 输入 `window.HF_TASKS`，应返回任务数组。
   - 若为空/undefined，检查自动任务模块页是否已播种任务（登录后 `hf_task_seed_sample()` 自动播种 3 条示例任务）。

5. **R 侧语法检查**（Windows 下注意 GBK/UTF-8）：
   ```r
   Sys.setlocale("LC_ALL", "en_US.UTF-8")
   parse("Script/hungfo/hf_ui.r")   # 应无报错
   ```

6. **数据库验证**：
   ```sql
   SELECT * FROM hf_agent_tasks;   -- 查看任务是否已写入
   ```

7. **热重载注意**：`global.R` 已设置 `options(shiny.autoreload.pattern = ".*\\.(r|R)$")`，只监视 R 源码，避免写 `hungfo_tasks.js` 触发页面刷新。

---

## 八、本次迭代变更清单

| 需求 | 变更 | 涉及文件 |
|------|------|----------|
| 1 | Input 下方添加任务名称列表（固定比例、无下拉条、少则空行多则流动） | `hungfo_agent_monitor.html` |
| 2 | 修复长时间运行后任务枯竭（只剩一条线）Bug | `hungfo_agent_monitor.html` |
| 3 | 新增 Agent Workflow Monitor 模块，放权限体系后，独立页面新标签页打开 | `hf_ui.r` |
| 4 | 自动任务模块作为任务来源，编辑示例/正式任务，切换显示 | `hf_core.r` / `hf_mod_autotask.r` / `hf_ui.r` / `hungfo_tasks.js` / `hungfo_agent_monitor.html` |
| 5 | 本开发日志 | `Script/hungfo/开发日志_Agent_Workflow_Monitor.md` |

---

## 九、后续方向

- 接真实心跳接口（当前为模拟引擎）。
- `hf_agent_tasks` 表扩展：优先级、依赖、定时调度字段，配合 `schedule()` 接口做真实任务下发。
- 监控页支持按 Agent 维度过滤任务、显示每个节点的实时负载。

---

# 附：元任务闭环（WorkBuddy 定时任务自动执行）

> 需求：以后不再与 WorkBuddy 对话，改在 HungFo「元任务」里添加任务记录，WorkBuddy 定时任务自动读取并执行，把执行过程（分析/方案/结果）写入开发日志。

## 一、整体架构

```
用户 → HungFo「元任务」页添加任务记录
        ↓ (R 端 hf_metatask_add)
   hf_metatask_queue 表（SQLite）
        ↓ (hf_metatask_export 导出镜像)
   Script/hungfo/MetaTasks.json（本地文本镜像）
        ↓ (WorkBuddy 定时任务读取，每 1 小时)
   Agent 直接执行任务（改文件/写文档）
        ↓ (hf_metatask_runner.r)
   写开发日志(devlog) + 标记任务完成 + 更新镜像
```

## 二、核心文件

| 文件 | 职责 |
|------|------|
| `Script/hungfo/hf_core.r` | 元任务数据层 + cron 解析 + `hf_metatask_export` 导出镜像 |
| `Script/hungfo/hf_mod_metatask.r` | 元任务环节接口（list/add/dispatch） |
| `Script/hungfo/hf_ui.r` | 元任务专属页（任务记录队列）+ 开发日志查看界面 |
| `Script/hungfo/hf_metatask_runner.r` | 命令行工具（peek/done/log/export/cron），供定时任务调用 |
| `Script/hungfo/MetaTasks.json` | 任务队列本地镜像（定时任务读取） |

## 三、数据表 `hf_metatask_queue`

| 字段 | 说明 |
|------|------|
| id | 主键 |
| title / payload | 任务标题 / 详细内容 |
| cron | cron 表达式（空=只跑一次；非空=循环） |
| status | pending(待执行) / running(执行中) / done(已完成) |
| last_run_at / next_run_at | 上次执行 / 下次执行时间 |
| result | 执行结果摘要 |

## 四、状态机

```
pending ──(到期: next_run_at <= now)──> 执行中 ──(完成)──┬─ 有 cron → pending(推进 next_run_at)
                                                        └─ 无 cron → done(一次性)
```

## 五、命令行工具用法

```bash
cd /d/GitHub/SuperITOM2
# 查看当前到期待执行任务（输出 JSON）
Rscript Script/hungfo/hf_metatask_runner.r peek

# 标记任务完成（有 cron 自动循环，无 cron 标记 done）
Rscript Script/hungfo/hf_metatask_runner.r done <任务id> "<结果摘要>"

# 写一条开发日志
Rscript Script/hungfo/hf_metatask_runner.r log "<标题>" "<内容>"

# 重新导出镜像 / 测试 cron
Rscript Script/hungfo/hf_metatask_runner.r export
Rscript Script/hungfo/hf_metatask_runner.r cron "*/5 * * * *"
```

## 六、cron 表达式

支持 5 字段（分 时 日 月 周），`*`、`*/n`、单值、逗号列表、区间：
- `*/5 * * * *` — 每 5 分钟
- `0 */1 * * *` — 每小时
- `0 9 * * 1-5` — 工作日 9 点

## 七、正式环境调试方法

1. **手动触发一次执行**：`Rscript Script/hungfo/hf_metatask_runner.r peek`，看 `due` 是否返回待执行任务。
2. **测试 cron**：`Rscript Script/hungfo/hf_metatask_runner.r cron "*/5 * * * *"`，验证返回的下次触发时间是否正确。
3. **验证镜像**：`cat Script/hungfo/MetaTasks.json`，确认任务列表与数据库一致。
4. **验证数据库**：`SELECT * FROM hf_metatask_queue;` 查看状态流转。
5. **验证开发日志**：`SELECT * FROM hf_records WHERE module_key='devlog';` 确认执行过程已写入。
6. **注意**：R 启动会打印 `During startup` / `LC_` 警告，解析 JSON 时需过滤这些行（定时任务 prompt 已说明）。

## 八、定时任务

- **名称**：HungFo 元任务自动执行器
- **周期**：每 1 小时（WorkBuddy 定时任务平台最小间隔为 1 小时，不支持分钟级）
- **行为**：读 MetaTasks.json 到期任务 → **子代理隔离执行** → 写 devlog → 标记完成 + 上报元数据 → 更新镜像

## 九、会话追踪与调用元数据（v2 新增）

### 9.1 目标
1. **每个元任务独立会话**：定时任务对每个到期任务 spawn 一个独立 sub-agent 执行，上下文互不堆积，也便于按任务追踪可视化。
2. **调用元数据可视化**：把每次执行的 LLM 名称、Token 消耗（输入/输出）、总耗时记录到数据库，并在监控页展示。

### 9.2 数据层（新增）
- `hf_metatask_queue` 表扩展字段：`session_id` / `llm_name` / `token_in` / `token_out` / `duration_ms` / `started_at` / `finished_at`（记录最近一次执行的元数据）。
- 新增 `hf_task_sessions` 表：每次执行 = 一行明细（task_id / session_id / llm_name / token_in / token_out / duration_ms / status / result / started_at / finished_at）。
- 新增镜像 `Script/hungfo/www/hungfo_sessions.js`：导出 `window.HF_SESSIONS = { summary: {total, today}, sessions: [...] }`，供监控页前端读取。

### 9.3 命令行工具（hf_metatask_runner.r）新增/扩展
- `done` 命令支持元数据参数：`done <id> "<result>" --llm <名> --tin <n> --tout <n> --dur <ms> --sid <会话ID>`
- 新增 `sessions` 命令：重新导出 hungfo_sessions.js
- `done` 执行时会自动写入 hf_task_sessions 并导出 sessions 镜像

### 9.4 监控页展示（hungfo_agent_monitor.html）
- 左侧 `IN-FLIGHT` / `AGENTS` 指标下方新增 **TOKEN 指标**：当天 TOKEN 总数 + 总 TOKEN（含会话数）。
- `LIVE TASKS` 下方新增 **Agent Sessions 面板**：展示真实元任务执行记录（LLM 名 / Token / 耗时 / 会话ID），多则缓慢流动，少则空行。
- 数据源：页面 `<head>` 引入 `hungfo_sessions.js`，JS 读取 `window.HF_SESSIONS`。

### 9.5 元数据来源说明
- **LLM 名称**：定时任务执行时尽力上报当前模型名，无法得知时填 `"auto"`。
- **Token**：尽力而为，无法获得时填 0（WorkBuddy 定时任务环境未必暴露真实 token 用量）。
- **耗时**：定时任务记录子代理开始/结束时间，估算 duration_ms。

### 9.6 调试方法（新增）
1. 手动上报测试：`Rscript Script/hungfo/hf_metatask_runner.r done <id> "结果" --llm "DeepSeek-V4-Pro" --tin 100 --tout 50 --dur 3000 --sid "sess-x"`
2. 查看会话明细：`SELECT * FROM hf_task_sessions ORDER BY id DESC;`
3. 验证镜像：`cat Script/hungfo/www/hungfo_sessions.js`
4. 重新导出：`Rscript Script/hungfo/hf_metatask_runner.r sessions`


## 十、Bug 修复：正式任务界面显示示例数据（2026-10-28）

### 现象
切到「正式任务」后，监控页仍在跑示例任务的数据。

### 根因（两层）
1. **数据库缺表**：`hf_agent_tasks` 表从没被真正创建——上一轮只做了 R 语法检查，`hf_db_init()` 未在真实 DB 执行；`hungfo_tasks.js` 是手写的临时文件，从未从数据库生成。
2. **前端静默回退**：`currentPool()` 在「正式任务」池为空时，无条件回退到内置 `TASK_PATHS`（research/codegen/data，与示例同名同路径），导致看起来「还是示例数据」。

### 修复
1. 实际执行建表 + 播种 3 条示例任务 + 从 DB 导出 `hungfo_tasks.js`（不再手写）。
2. 前端 `buildTaskPool`/`currentPool`：切到某类型无任务时返回空数组，**不再静默回退示例数据**。
3. `spawnTask` 空池返回 null；`taskPump` 的 while 补并发循环加 `break`（防死循环）。
4. 切换类型时：无任务则日志提示「⚠ 暂无正式任务，请先在自动任务模块添加」。
5. 补充一条正式任务「生产环境发布巡检」（formal）作为正式任务界面的真实数据。

### 结论
「正式任务界面跑示例数据」的真相是：正式任务池里根本没有正式任务，前端回退逻辑又掩盖了这个空态。修复后，无正式任务会明确提示，有正式任务则正确显示。

### 正确使用方式
- 自动任务模块页 → 任务类型选「正式任务」→ 添加任务 → 保存（自动 `hf_write_tasks_js` 导出）→ 监控页切「正式任务」即可看到真实正式任务流转。
