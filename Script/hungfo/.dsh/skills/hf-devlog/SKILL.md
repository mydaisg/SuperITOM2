---
name: hf-devlog
description: 查询、搜索、筛选 HungFo 开发日志。用户说"开发日志""看日志""查开发记录"时使用。支持按关键词、日期范围、状态筛选。
whenToUse: 需要查看、检索、筛选 HungFo 开发日志时。
---

# HungFo 开发日志查询

## 常量

```powershell
$sq = 'D:\GitHub\SuperITOM2\Script\hungfo\DB\sqlite3.exe'
$db = 'D:\GitHub\SuperITOM2\Script\hungfo\DB\HF_HungFo_DSH.DB'
```

## 表结构 `hf_records`

`id, module_key, rec_no, title, payload, status, created_by, created_at, updated_at`

- `module_key` 值：`devlog`（开发日志）、`rules`（规则）
- `status` 值：`draft`、`正式`

## 常用查询

```powershell
# 最近 20 条
& $sq $db ".separator ' | '" "SELECT id, rec_no, substr(title,1,40), substr(created_at,1,16) FROM hf_records ORDER BY id DESC LIMIT 20;"

# 关键词搜索（标题+正文）
& $sq $db ".separator ' | '" "SELECT id, rec_no, substr(title,1,40), substr(created_at,1,16) FROM hf_records WHERE (title LIKE '%KW%' OR payload LIKE '%KW%') ORDER BY id DESC LIMIT 50;"

# 日期范围
& $sq $db ".separator ' | '" "SELECT id, rec_no, substr(title,1,40), substr(created_at,1,16) FROM hf_records WHERE date(created_at) BETWEEN '2026-10-28' AND '2026-11-02' ORDER BY id DESC;"

# 按月统计
& $sq $db ".separator ' | '" "SELECT substr(created_at,1,7) AS 月份, COUNT(*) AS 数量 FROM hf_records GROUP BY 1 ORDER BY 1 DESC;"

# 看详情
& $sq $db ".separator ' | '" "SELECT title, payload FROM hf_records WHERE id = 48;"
```

## 输出

Markdown 表格：编号 / 标题 / 日期。默认 20 条，标题截断 40 字。

## 注意

- 只读，绝不写库
- 时间列是 `created_at`；编号列是 `rec_no`
- 数据在**开发库** `HF_HungFo_DSH.DB`，不是正式库

---

# 网页版（可搜索筛选）

生成带搜索/筛选的只读页面：

```powershell
& 'D:\Tai_ProgramFiles\R\R-4.6.1\bin\x64\Rscript.exe' 'D:\GitHub\SuperITOM2\Script\hungfo\hf_devlog_page.r'
```

输出：`Script\hungfo\www\dsh_devlog.html`，双击即可打开，无需服务。
支持：关键词搜索（标题+正文）、日期范围、按月分组、点行看详情。
