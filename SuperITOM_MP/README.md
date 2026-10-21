# SuperITOM_MP — SuperITOM2 微信小程序版

将现有 R Shiny 版 `SuperITOM2` 发布到**微信小程序**的最小可运行 MVP。
首期范围：**登录 + 首页工作台**（工单统计卡片 + 我的项目 / 我的工单 / 我的任务）。

## 技术栈

| 层 | 技术 |
|----|------|
| 小程序前端 | 微信原生小程序 + TypeScript（WXML/WXSS/TS） |
| 后端 | Node.js + TypeScript + Express + `node:sqlite` + JWT |
| 数据 | 直接复用现有 `SuperITOM2/DB/GH_ITOM.db`（SQLite，与 R 端共享同一份数据，零迁移） |

> 后端使用 Node 内置的 `node:sqlite`（Node ≥ 22.5），无需编译原生模块。
> 登录逻辑与现有 `Script/auth.r` 完全一致（`users` 表明文密码比对 + `active` 禁用检查），现有账号可直接登录。

## 目录结构

```
SuperITOM_MP/
├── backend/                 # Node/TS 后端
│   └── src/
│       ├── config.ts        # 端口 / DB路径 / JWT 配置
│       ├── db.ts            # node:sqlite 连接封装
│       ├── middleware/auth.ts  # JWT 鉴权中间件
│       ├── routes/auth.ts   # POST /api/auth/login
│       ├── routes/home.ts   # GET  /api/home/dashboard
│       ├── routes/workorders.ts  # 工单模块（列表/详情/评论/创建/状态流转）
│       ├── app.ts           # Express 应用组装
│       └── server.ts        # 启动入口
└── miniprogram/             # 微信小程序前端（微信开发者工具导入此目录）
    ├── app.ts / app.json / app.wxss
    ├── utils/               # request 封装 / 登录态 / 状态映射 / 配置
    └── pages/
        ├── login/           # 登录页
        ├── index/           # 首页工作台
        └── workorders/      # 工单模块
            ├── list/        # 工单列表（状态 tab + 分页 + 新建）
            ├── detail/      # 工单详情（字段 + 评论 + 派发/处理/完成/关闭）
            └── create/      # 创建工单
```

## 一、后端启动

```bash
cd backend
npm install
npm run dev          # 开发模式（tsx watch，默认 http://localhost:3000）
```

验证：

```bash
curl http://localhost:3000/health
```

> 启动时会看到一行 `SQLite is an experimental feature` 警告，这是 Node 对 `node:sqlite` 的提示，无害，可忽略。

### 后端配置（环境变量，可选）

默认会自动定位数据库到 `SuperITOM2/DB/GH_ITOM.db`。如需覆盖，复制 `.env.example` 为 `.env` 修改：

| 变量 | 默认值 | 说明 |
|------|--------|------|
| `PORT` | `3000` | 服务端口 |
| `DB_PATH` | 自动定位 | SQLite 数据库完整路径 |
| `JWT_SECRET` | `superitom-mp-secret-change-me` | **生产必须修改** |
| `JWT_EXPIRES_IN` | `24h` | Token 有效期 |

## 二、小程序前端运行

1. 安装类型依赖：`cd miniprogram && npm install`
2. 打开**微信开发者工具** → 导入项目 → 选择 `SuperITOM_MP/miniprogram` 目录
3. AppID 使用自己的（或先选「测试号」；`project.config.json` 里默认 `touristappid`）
4. 本地调试：在「详情 → 本地设置」勾选 **「不校验合法域名」**，使本地 `http://127.0.0.1:3000` 可访问
5. 后端地址在 `miniprogram/utils/config.ts` 的 `BASE_URL` 中配置

### 登录账号

直接使用现有 `users` 表中的账号（与 R 版同库同密码），例如 `admin`。

## 三、接口一览

| 方法 | 路径 | 鉴权 | 说明 |
|------|------|------|------|
| GET | `/health` | 无 | 健康检查 |
| POST | `/api/auth/login` | 无 | 登录，返回 `{ token, user }` |
| GET | `/api/home/dashboard` | Bearer Token | 首页工作台聚合数据 |
| GET | `/api/workorders` | Bearer Token | 工单列表（`?status=&page=&pageSize=`） |
| GET | `/api/workorders/assignable-users` | Bearer Token | 可派发用户（active=1） |
| GET | `/api/workorders/:id` | Bearer Token | 工单详情（含名称合并） |
| GET | `/api/workorders/:id/comments` | Bearer Token | 工单评论列表 |
| POST | `/api/workorders` | Bearer Token | 创建工单（自动生成 ITS 工单号） |
| POST | `/api/workorders/:id/comments` | Bearer Token | 添加评论 |
| POST | `/api/workorders/:id/assign` | Bearer Token | 派发（需 admin/it_desk） |
| POST | `/api/workorders/:id/start` | Bearer Token | 开始处理 |
| POST | `/api/workorders/:id/complete` | Bearer Token | 完成（含解决方案，同步巡检异常） |
| POST | `/api/workorders/:id/close` | Bearer Token | 关闭（同步巡检异常） |

> 权限：非 admin 用户仅能查看/操作与自己相关（`created_by`/`assigned_to`/`handled_by`）的工单；派发需 `admin` 或 `it_desk` 角色。

`/api/home/dashboard` 返回结构：

```json
{
  "success": true,
  "data": {
    "stats": { "total": 0, "pending": 0, "assigned": 0, "processing": 0, "completed": 0, "closed": 0 },
    "projects":   [ { "id": 1, "project_no": "PRJ...", "name": "...", "status": "active", ... } ],
    "workOrders": [ { "id": 1, "order_no": "ITS...", "title": "...", "status": "pending", ... } ],
    "tasks":      [ { "id": 1, "task_no": "TSK...", "name": "...", "status": "pending", "importance": 1, ... } ]
  }
}
```

## 四、生产发布注意事项

微信小程序正式发布前需完成：

1. **后端部署到公网 HTTPS**：小程序 `wx.request` 只允许 HTTPS 域名，需将后端部署到有域名的服务器（建议 Nginx 反代 + SSL 证书）。
2. **配置 request 合法域名**：在「小程序管理后台 → 开发管理 → 开发设置 → 服务器域名」添加后端 HTTPS 域名。
3. **AppID**：替换 `project.config.json` 中的 `touristappid`，并使用已认证的主体 AppID。
4. **安全**：务必修改 `JWT_SECRET`；当前登录为明文密码比对（为兼容现有数据），后续建议改造为哈希 + 微信 `wx.login` 绑定 openid 的一键登录。
5. **数据库并发**：`node:sqlite` 为同步单连接，适合 MVP / 内部小规模使用；并发量上来后可改为 `better-sqlite3`（同库文件）或迁移到 PostgreSQL。

## 五、后续扩展方向

- 微信 `wx.login` → openid 绑定现有账号，实现一键登录
- 工单列表 / 详情 / 评论（下一个 MVP 模块）
- 巡检任务执行（拍照上传）
- 企业微信小程序（面向内部员工，通讯录打通）
