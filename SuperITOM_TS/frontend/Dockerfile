# 构建阶段
FROM node:18-alpine as build

WORKDIR /app

# 复制依赖文件
COPY package*.json ./

# 安装依赖
RUN npm install

# 复制源代码
COPY . .

# 构建项目
RUN npm run build

# 运行阶段
FROM node:18-alpine as runtime

WORKDIR /app

# 复制构建产物
COPY --from=build /app/dist ./dist
COPY --from=build /app/package*.json ./

# 安装生产依赖
RUN npm install --production

# 暴露端口
EXPOSE 3001

# 启动应用
CMD ["npm", "run", "preview"]
