# SuperITOM_TS 项目部署手册

## 1. 环境准备

### 1.1 服务器信息
- **操作系统**: CentOS 10
- **服务器名称**: CS7
- **部署目录**: /opt/github/SuperITOM_TS
- **网络要求**: 开放端口 3000（后端）、3001（前端）、5432（PostgreSQL）、6379（Redis）、5672（RabbitMQ）

### 1.2 系统更新
```bash
# 更新系统包
sudo dnf update -y

# 安装必要的系统工具
sudo dnf install -y git curl wget unzip make gcc-c++
```

## 2. 依赖安装

### 2.1 Node.js 安装
```bash
# 安装 Node.js 18
curl -fsSL https://rpm.nodesource.com/setup_18.x | sudo bash -
sudo dnf install -y nodejs

# 验证安装
node -v
npm -v

# 安装 yarn（可选）
npm install -g yarn
```

### 2.2 PostgreSQL 安装
```bash
# 安装 PostgreSQL
sudo dnf install -y postgresql-server postgresql-contrib

# 初始化数据库
sudo postgresql-setup --initdb

# 启动服务并设置开机自启
sudo systemctl start postgresql
sudo systemctl enable postgresql

# 创建数据库和用户
sudo -u postgres psql -c "CREATE USER admin WITH PASSWORD 'password';"
sudo -u postgres psql -c "CREATE DATABASE superitom OWNER admin;"
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE superitom TO admin;"
```

### 2.3 Redis 安装
```bash
# 安装 Redis
sudo dnf install -y redis

# 启动服务并设置开机自启
sudo systemctl start redis
sudo systemctl enable redis

# 验证安装
redis-cli ping
```

### 2.4 RabbitMQ 安装
```bash
# 安装 RabbitMQ 依赖
sudo dnf install -y epel-release
sudo dnf install -y erlang

# 安装 RabbitMQ
sudo rpm --import https://github.com/rabbitmq/signing-keys/releases/download/2.0/rabbitmq-release-signing-key.asc
sudo dnf install -y https://github.com/rabbitmq/rabbitmq-server/releases/download/v3.12.0/rabbitmq-server-3.12.0-1.el9.noarch.rpm

# 启动服务并设置开机自启
sudo systemctl start rabbitmq-server
sudo systemctl enable rabbitmq-server

# 启用管理插件
sudo rabbitmq-plugins enable rabbitmq_management

# 创建用户（可选）
sudo rabbitmqctl add_user admin password
sudo rabbitmqctl set_user_tags admin administrator
sudo rabbitmqctl set_permissions -p / admin ".*" ".*" ".*"
```

## 3. 项目部署

### 3.1 代码克隆
```bash
# 创建部署目录
sudo mkdir -p /opt/github
cd /opt/github

# 克隆项目代码
sudo git clone https://github.com/yourusername/SuperITOM_TS.git

# 设置目录权限
sudo chown -R $USER:$USER /opt/github/SuperITOM_TS
cd /opt/github/SuperITOM_TS
```

### 3.2 配置文件设置

#### 3.2.1 后端配置
```bash
# 创建 .env 文件
cd backend
touch .env

# 编辑 .env 文件
cat > .env << EOF
# 应用配置
PORT=3000
NODE_ENV=production

# 数据库配置
DATABASE_HOST=localhost
DATABASE_PORT=5432
DATABASE_USERNAME=admin
DATABASE_PASSWORD=password
DATABASE_NAME=superitom

# Redis 配置
REDIS_HOST=localhost
REDIS_PORT=6379

# RabbitMQ 配置
RABBITMQ_HOST=localhost
RABBITMQ_PORT=5672

# JWT 配置
JWT_SECRET=superitom_secret_key
JWT_EXPIRES_IN=24h

# 日志配置
LOG_LEVEL=info
EOF
```

#### 3.2.2 前端配置
```bash
# 创建 .env 文件
cd ../frontend
touch .env

# 编辑 .env 文件
cat > .env << EOF
VITE_API_URL=http://localhost:3000
EOF
```

### 3.3 依赖安装

#### 3.3.1 后端依赖
```bash
cd ../backend
npm install --production
```

#### 3.3.2 前端依赖
```bash
cd ../frontend
npm install --production
```

### 3.4 项目构建

#### 3.4.1 后端构建
```bash
cd ../backend
npm run build
```

#### 3.4.2 前端构建
```bash
cd ../frontend
npm run build
```

## 4. 服务启动

### 4.1 使用 systemd 管理服务

#### 4.1.1 后端服务配置
```bash
# 创建后端服务文件
sudo cat > /etc/systemd/system/superitom-backend.service << EOF
[Unit]
Description=SuperITOM Backend Service
After=network.target postgresql.service redis.service rabbitmq-server.service

[Service]
Type=simple
User=$USER
WorkingDirectory=/opt/github/SuperITOM_TS/backend
ExecStart=/usr/bin/npm run start:prod
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF
```

#### 4.1.2 前端服务配置
```bash
# 创建前端服务文件
sudo cat > /etc/systemd/system/superitom-frontend.service << EOF
[Unit]
Description=SuperITOM Frontend Service
After=network.target superitom-backend.service

[Service]
Type=simple
User=$USER
WorkingDirectory=/opt/github/SuperITOM_TS/frontend
ExecStart=/usr/bin/npm run preview
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF
```

### 4.2 启动服务
```bash
# 重新加载 systemd 配置
sudo systemctl daemon-reload

# 启动后端服务
sudo systemctl start superitom-backend
sudo systemctl enable superitom-backend

# 启动前端服务
sudo systemctl start superitom-frontend
sudo systemctl enable superitom-frontend

# 查看服务状态
sudo systemctl status superitom-backend
sudo systemctl status superitom-frontend
```

### 4.3 验证服务
```bash
# 检查端口是否开放
ss -tulpn | grep -E '3000|3001'

# 测试后端 API
curl http://localhost:3000/auth/login -X POST -H "Content-Type: application/json" -d '{"username":"admin","password":"admin123"}'

# 测试前端访问
curl -I http://localhost:3001
```

## 5. 功能说明

### 5.1 核心功能

#### 5.1.1 用户认证
- **登录**: POST /auth/login - 用户名密码登录
- **注册**: POST /auth/register - 注册新用户
- **获取个人信息**: GET /auth/profile - 获取当前用户信息
- **退出登录**: GET /auth/logout - 退出登录

#### 5.1.2 自动化管理
- **获取脚本列表**: GET /automation/scripts - 获取可用的 PowerShell 脚本
- **执行脚本**: POST /automation/execute - 执行指定的脚本
- **终止脚本**: POST /automation/terminate/:id - 终止正在执行的脚本
- **获取脚本状态**: GET /automation/status/:id - 获取脚本执行状态

#### 5.1.3 数据管理
- **获取数据列表**: GET /data/list - 获取数据列表
- **上传数据**: POST /data/upload - 上传数据文件
- **下载数据**: GET /data/download/:id - 下载数据文件
- **删除数据**: DELETE /data/:id - 删除数据

#### 5.1.4 GitHub 集成
- **获取仓库列表**: GET /github/repos - 获取 GitHub 仓库列表
- **添加仓库**: POST /github/repos - 添加 GitHub 仓库
- **删除仓库**: DELETE /github/repos/:id - 删除 GitHub 仓库
- **获取 Webhook 列表**: GET /github/webhooks - 获取 Webhook 列表

### 5.2 使用方法

#### 5.2.1 访问系统
- **前端地址**: http://CS7:3001
- **默认登录凭据**:
  - 用户名: admin
  - 密码: admin123（首次登录后请修改）

#### 5.2.2 执行自动化脚本
1. 登录系统后，进入「自动化管理」页面
2. 选择要执行的 PowerShell 脚本
3. 输入脚本参数（可选）
4. 点击「执行脚本」按钮
5. 查看脚本执行输出和状态

#### 5.2.3 管理数据
1. 进入「数据管理」页面
2. 可以查看、上传、下载和删除数据
3. 点击「添加数据」按钮添加新数据

#### 5.2.4 GitHub 集成
1. 进入「GitHub 集成」页面
2. 添加 GitHub 仓库 URL
3. 同步仓库信息
4. 配置 Webhook

## 6. 代码结构

### 6.1 项目架构
```
SuperITOM_TS/
├── backend/                # 后端 NestJS 应用
│   ├── src/
│   │   ├── modules/        # 业务模块
│   │   │   ├── auth/       # 认证模块
│   │   │   ├── config/     # 配置模块
│   │   │   ├── data/       # 数据管理模块
│   │   │   ├── automation/ # 自动化模块
│   │   │   └── github/     # GitHub 集成模块
│   │   ├── common/         # 公共组件
│   │   ├── main.ts         # 应用入口
│   │   └── app.module.ts   # 根模块
│   ├── package.json        # 依赖配置
│   ├── tsconfig.json       # TypeScript 配置
│   └── Dockerfile          # 容器化配置
├── frontend/               # 前端 React 应用
│   ├── src/
│   │   ├── components/     # 通用组件
│   │   ├── pages/          # 页面组件
│   │   ├── main.tsx        # 应用入口
│   │   └── App.tsx         # 根组件
│   ├── package.json        # 依赖配置
│   ├── tsconfig.json       # TypeScript 配置
│   ├── vite.config.ts      # Vite 配置
│   └── Dockerfile          # 容器化配置
├── docker-compose.yml      # 容器编排配置
├── README.md               # 项目说明文档
├── ARCHITECTURE.md         # 架构设计文档
└── DEPLOYMENT.md           # 部署手册
```

### 6.2 核心模块说明

#### 6.2.1 认证模块（backend/src/modules/auth/）
- **功能**: 处理用户认证和授权
- **主要文件**:
  - `auth.service.ts`: 实现认证业务逻辑
  - `auth.controller.ts`: 处理 HTTP 请求
  - `user.entity.ts`: 用户数据模型
  - `jwt.strategy.ts`: JWT 认证策略

#### 6.2.2 自动化模块（backend/src/modules/automation/）
- **功能**: 执行和管理 PowerShell 脚本
- **主要文件**:
  - `automation.service.ts`: 实现脚本执行逻辑
  - `automation.controller.ts`: 处理 HTTP 请求
  - `automation.gateway.ts`: WebSocket 实时通信

#### 6.2.3 数据模块（backend/src/modules/data/）
- **功能**: 管理系统数据
- **主要文件**:
  - `data.service.ts`: 实现数据管理逻辑
  - `data.controller.ts`: 处理 HTTP 请求
  - `data.entity.ts`: 数据模型

#### 6.2.4 GitHub 模块（backend/src/modules/github/）
- **功能**: 集成 GitHub 仓库
- **主要文件**:
  - `github.service.ts`: 实现 GitHub 集成逻辑
  - `github.controller.ts`: 处理 HTTP 请求

### 6.3 前端模块说明

#### 6.3.1 页面组件（frontend/src/pages/）
- `Login.tsx`: 登录页面
- `Dashboard.tsx`: 仪表板页面
- `Automation.tsx`: 自动化管理页面
- `DataManagement.tsx`: 数据管理页面
- `GitHubIntegration.tsx`: GitHub 集成页面
- `Settings.tsx`: 系统设置页面

#### 6.3.2 通用组件（frontend/src/components/）
- `Header.tsx`: 页面头部组件
- `Sidebar.tsx`: 侧边栏导航组件
- `ProtectedRoute.tsx`: 路由保护组件

### 6.4 开发范式

SuperITOM_TS 采用模块化开发和函数式开发范式，确保代码的可维护性、可测试性和可扩展性。

#### 6.4.1 模块化开发

- **后端模块化**：基于业务功能划分为独立模块，每个模块包含控制器、服务、数据模型等组件
- **前端模块化**：UI 组件按功能划分为通用组件和页面组件，状态管理按业务领域划分
- **模块边界**：每个模块有清晰的职责边界，避免跨模块耦合
- **模块通信**：通过依赖注入和服务引用实现模块间通信

#### 6.4.2 函数式开发

- **纯函数**：业务逻辑实现为纯函数，相同输入总是产生相同输出，无副作用
- **不可变性**：数据一旦创建，不能被修改，状态变化通过创建新数据实现
- **函数组合**：通过组合小函数构建复杂功能，提高代码复用性
- **高阶函数**：使用高阶函数处理数据转换和业务逻辑
- **错误处理**：使用 Either 类型或 Result 类型处理错误，避免异常传递

**函数式编程优势**：
- 代码可测试性高，纯函数易于编写单元测试
- 代码可维护性好，函数职责单一，逻辑清晰
- 并行处理能力强，纯函数适合并行执行
- 状态预测性好，不可变性使状态变化可预测

#### 6.4.3 开发建议

1. **遵循模块化原则**：新功能应添加到相应的模块中，或创建新的模块
2. **使用函数式编程**：业务逻辑应实现为纯函数，避免副作用
3. **保持代码简洁**：函数应短小精悍，职责单一
4. **使用 TypeScript 类型**：充分利用 TypeScript 的类型系统，提高代码安全性
5. **编写单元测试**：为纯函数编写单元测试，确保代码质量

## 7. 开发指南

### 7.1 环境设置

#### 7.1.1 开发依赖安装
```bash
# 后端开发依赖
cd /opt/github/SuperITOM_TS/backend
npm install

# 前端开发依赖
cd ../frontend
npm install
```

### 7.2 开发模式启动

#### 7.2.1 后端开发模式
```bash
cd /opt/github/SuperITOM_TS/backend
npm run start:dev
```

#### 7.2.2 前端开发模式
```bash
cd /opt/github/SuperITOM_TS/frontend
npm run dev
```

### 7.3 代码调试

#### 7.3.1 后端调试
- **VS Code 调试配置**:
  ```json
  {
    "version": "0.2.0",
    "configurations": [
      {
        "type": "node",
        "request": "attach",
        "name": "Attach to Backend",
        "port": 9229,
        "restart": true,
        "localRoot": "${workspaceFolder}/backend",
        "remoteRoot": "/opt/github/SuperITOM_TS/backend"
      }
    ]
  }
  ```

#### 7.3.2 前端调试
- 使用 Chrome DevTools 进行调试
- 访问 http://CS7:3001 并打开开发者工具

### 7.4 代码质量

#### 7.4.1 后端代码检查
```bash
cd /opt/github/SuperITOM_TS/backend
npm run lint
npm run test
```

#### 7.4.2 前端代码检查
```bash
cd /opt/github/SuperITOM_TS/frontend
npm run lint
npm run test
```

### 7.5 部署流程

#### 7.5.1 代码更新
```bash
cd /opt/github/SuperITOM_TS
git pull

# 更新后端
cd backend
npm install
npm run build
sudo systemctl restart superitom-backend

# 更新前端
cd ../frontend
npm install
npm run build
sudo systemctl restart superitom-frontend
```

#### 7.5.2 数据库迁移
```bash
cd /opt/github/SuperITOM_TS/backend
npm run migration:run
```

## 8. 故障排查

### 8.1 常见问题

#### 8.1.1 服务启动失败
- **检查日志**:
  ```bash
  sudo journalctl -u superitom-backend
  sudo journalctl -u superitom-frontend
  ```

- **检查依赖服务**:
  ```bash
  sudo systemctl status postgresql redis rabbitmq-server
  ```

#### 8.1.2 数据库连接失败
- **检查数据库状态**:
  ```bash
  sudo systemctl status postgresql
  sudo -u postgres psql -c "\l"
  ```

- **检查数据库配置**:
  ```bash
  cat /opt/github/SuperITOM_TS/backend/.env
  ```

#### 8.1.3 前端无法访问后端
- **检查网络连接**:
  ```bash
  ping localhost
  curl http://localhost:3000
  ```

- **检查 CORS 配置**:
  ```bash
  grep -A 10 "enableCors" /opt/github/SuperITOM_TS/backend/src/main.ts
  ```

### 8.2 日志管理

#### 8.2.1 后端日志
- **应用日志**:
  ```bash
  sudo journalctl -u superitom-backend -f
  ```

#### 8.2.2 前端日志
- **浏览器控制台**:
  - 打开浏览器开发者工具
  - 查看控制台日志

## 9. 维护计划

### 9.1 定期维护
- **系统更新**: 每月执行 `sudo dnf update -y`
- **数据库备份**: 每周执行数据库备份
- **日志清理**: 每月清理日志文件

### 9.2 性能监控
- **系统监控**: 使用 Prometheus + Grafana 监控系统性能
- **应用监控**: 集成应用性能监控工具

### 9.3 安全加固
- **定期更新依赖**: 每月执行 `npm audit` 检查依赖安全
- **密码策略**: 实施强密码策略
- **访问控制**: 限制敏感接口的访问权限

## 10. 联系方式

### 10.1 技术支持
- **维护人员**: IT 运维团队
- **联系方式**: it-support@company.com
- **紧急联系电话**: 123-456-7890

### 10.2 文档更新
- **文档版本**: 1.0
- **更新日期**: 2026-01-28
- **更新人员**: 系统管理员

---

本部署手册涵盖了 SuperITOM_TS 项目在 CentOS 10 环境下的完整部署流程，包括环境准备、依赖安装、项目部署、服务启动、功能说明、代码结构和开发指南。通过本手册，Trae 可以快速理解项目架构和部署步骤，进行环境部署和后续开发工作。
