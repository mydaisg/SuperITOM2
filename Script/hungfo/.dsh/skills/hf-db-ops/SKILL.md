---
name: hf-db-ops
description: HungFo 数据库操作规范：查表、改数、迁移、备份，以及 Windows 中文编码坑。当需要查询或修改 HF_HungFo_DSH.DB、写 SQL 迁移脚本、核对数据时使用。
whenToUse: 需要查询数据库内容、执行 INSERT/UPDATE、写迁移脚本、或遇到中文乱码时。
---

# HungFo 数据库操作（DSH 开发库）

## 库位置与工具

| 项 | 值 |
|---|---|
| 开发库 | `D:\GitHub\SuperITOM2\Script\hungfo\DB\HF_HungFo_DSH.DB` |
| 正式库 | `...\DB\HF_HungFo.DB`（**只读，禁止写**） |
| sqlite3 | `...\DB\sqlite3.exe`（不在 PATH，用全路径） |
| 日志模式 | `delete`（非 WAL），可直接文件复制当备份 |
| R 包 | shiny / shinythemes / DT / DBI / RSQLite / jsonlite 等 206 个已装 |

## 读数据

```powershell
$sq  = 'D:\GitHub\SuperITOM2\Script\hungfo\DB\sqlite3.exe'
$db  = 'D:\GitHub\SuperITOM2\Script\hungfo\DB\HF_HungFo_DSH.DB'
& $sq $db ".tables"
& $sq $db "SELECT COUNT(*) FROM hf_metatask_queue;"
```

**注意**：沙箱内的探测对 `file:...?mode=ro` URI 支持不稳定，直接传路径即可；正式库若要读，先 `Copy-Item` 到临时再查，避免误写。

## 写数据（两种方式）

1. **简单 SQL**：`sqlite3.exe` 直连开发库。
2. **有业务逻辑**（触发导出、更新镜像 js）：**必须走 R 数据层**，不要手写 SQL，否则 `MetaTasks.json` / `hungfo_*.js` 镜像会不同步。

```powershell
# 例：通过 R 层跑元任务相关操作
& 'D:\Tai_ProgramFiles\R\R-4.6.1\bin\x64\Rscript.exe' Script/hungfo/hf_metatask_runner.r export
```

## 编码铁律（Windows 中文乱码根因）

- R 侧统一 `$env:R_ENCODING='UTF-8'`
- 外部命令输出转码：`iconv(result, from = "GBK", to = "UTF-8", sub = "")`
- R 启动会打印 `During startup` / `LC_` / `[1] "LC_COLLATE=..."` 警告，**解析输出时要过滤掉这些行**
- 中文写库一律经 R（`hf_esc()` 做转义），不要用 PowerShell 拼 SQL

## 备份（改动前必做）

```powershell
$db='D:\GitHub\SuperITOM2\Script\hungfo\DB\HF_HungFo_DSH.DB'
Copy-Item $db "$db.bak_$(Get-Date -Format yyyyMMdd_HHmmss)" -Force
& 'D:\GitHub\SuperITOM2\Script\hungfo\DB\sqlite3.exe' $db "PRAGMA integrity_check;"
```

## 迁移脚本

放 `DB/` 下，命名 `<序号>_<说明>.sql`（沿袭现有 `init_hungfo_db.sql`、`migrate_notes_from_parent.sql`）。改结构后要同步更新 `hf_core.r` 里的 `hf_db_init()`，否则新环境建库会缺表。

## 从正式库同步（只允许单向）

```powershell
# 仅在你确认要重置开发数据时执行；会覆盖开发库
Copy-Item '...\DB\HF_HungFo.DB' '...\DB\HF_HungFo_DSH.DB' -Force
```
**永远反向操作**（开发库 → 正式库）**禁止**。
