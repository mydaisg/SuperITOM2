# 研发中心采访专栏 - 独立流媒体网站

完全独立、零依赖、可移植的流媒体服务（不依赖 SuperITOM2 / R / Shiny）。

## 目录结构
```
rd_interview/
├── server.js            # Node 单文件服务（静态托管 + Range 流媒体 + 访问统计）
├── RD_Interview.html    # 采访专栏页面（禁止回拉 + 时长上报）
├── stats.html           # 统计查看页面（访问量/时长/明细图表）
├── interview_column.js  # 视频清单配置
├── videos/              # 视频文件（mp4）
├── access_log.csv       # 访问统计（自动生成，Excel 可直接打开）
├── start.bat            # Windows 一键启动
└── README.md
```

## 运行（仅需 Node.js，无需安装任何包）
```bash
node server.js            # 默认端口 8899
node server.js 8080       # 指定端口
```
Windows 也可双击 `start.bat`。

访问：
- 播放页面：`http://<主机IP>:<端口>/RD_Interview.html`
- 统计页面：`http://<主机IP>:<端口>/stats.html`

## 移植到其它主机
1. 将整个 `rd_interview/` 文件夹拷贝到目标主机
2. 目标主机安装 Node.js（任意较新版本即可）
3. 运行 `node server.js` 或双击 `start.bat`

无需数据库、无需 R、无需任何 npm install。

## 功能说明
- **流媒体**：HTTP Range 支持，边播边缓冲，可拖进度条
- **访问统计**：记录访客 IP、User-Agent、观看的视频、开始/结束时间、实际观看时长（秒），写入 `access_log.csv`
- **统计查看页**：`/stats.html` 展示总访问量、独立访客、累计/人均观看时长、每日趋势图、各视频访问量、最近访问明细，每 30 秒自动刷新
- **禁止回拉**：用户不能把进度条拖回已看过的位置（可向前快进、可暂停）

## 新增一期采访
编辑 `interview_column.js`，在 `INTERVIEW_LIST` 数组末尾追加对象：
```js
{
  id: "ai-2026-02",
  title: "研发中心采访 · 某某部门（2026年第2期专访）",
  video: "videos/interview_xxx_2026_ep2.mp4",
  cover: "",
  dept: "某某部门",
  issue: "2026年 第2期",
  date: "2026-09-01",
  desc: "简介...",
  duration: ""
}
```
然后将视频文件放入 `videos/` 目录即可。
