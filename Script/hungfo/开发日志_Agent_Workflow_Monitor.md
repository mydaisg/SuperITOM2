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
