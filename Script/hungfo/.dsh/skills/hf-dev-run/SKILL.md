---
name: hf-dev-run
description: 启动、停止、验证 HungFo 的 DSH 开发实例（端口 9999，开发库 HF_HungFo_DSH.DB）。当需要运行应用、重启、排查启动失败、查看运行日志时使用。
whenToUse: 需要起停 HungFo 服务、确认端口/数据库是否正确、或排查 Shiny 启动报错时。
---

# HungFo 开发实例起停（DSH）

## 环境约定（不可更改）

| 项 | 开发（本技能） | 正式（不要碰） |
|---|---|---|
| 端口 | **9999** | 8999 |
| 数据库 | `DB/HF_HungFo_DSH.DB` | `DB/HF_HungFo.DB` |
| 入口脚本 | `dsh_dev.R` | `app.R` |
| 日志 | `Log/DSH/` | — |

`dsh_dev.R` 内置两道安全闸：端口等于 8999 直接 `stop()`；目标库等于正式库直接 `stop()`。**不要绕过**。

## 启动

```powershell
$hungfo = 'D:\GitHub\SuperITOM2\Script\hungfo'
$rscript = 'D:\Tai_ProgramFiles\R\R-4.6.1\bin\x64\Rscript.exe'
New-Item -ItemType Directory -Force -Path "$hungfo\Log\DSH" | Out-Null
Start-Process -FilePath $rscript -ArgumentList 'dsh_dev.R' -WorkingDirectory $hungfo `
  -RedirectStandardOutput "$hungfo\Log\DSH\out.log" `
  -RedirectStandardError  "$hungfo\Log\DSH\err.log" `
  -WindowStyle Hidden -PassThru
```

## 验证（必须同时满足）

```powershell
Start-Sleep -Seconds 12
# 1) HTTP 可达且是 HungFo 页面
(Invoke-WebRequest 'http://127.0.0.1:9999' -TimeoutSec 10 -UseBasicParsing).Content.Length
# 2) 启动日志里应有 Listening on ...9999 与 HUNGFO_DB 指向 _DSH.DB
Get-Content "$hungfo\Log\DSH\err.log" -Tail 5
Get-Content "$hungfo\Log\DSH\out.log" -Tail 10
```

健康输出示例：
```
ERR: Listening on http://127.0.0.1:9999
OUT: [dsh_dev] HUNGFO_DB = ...\DB\HF_HungFo_DSH.DB
OUT: [dsh_dev] 端口 = 9999
```

## 停止

```powershell
netstat -ano | Select-String ':9999.*LISTENING'
Stop-Process -Id <PID> -Force
```

注意：`Get-NetTCPConnection` 在 DSH 沙箱下可能返回空，**改用 `netstat -ano`**。

## 常见故障

| 现象 | 原因 | 处理 |
|---|---|---|
| `address already in use` | 9999 已被占用（多为上次未停） | netstat 找 PID 后结束，再启动 |
| 启动即退出、日志为空 | 安全闸触发（端口/库被设成正式值） | 检查是否设了 `HUNGFO_PORT`/`HUNGFO_DB` 环境变量 |
| `there is no package called 'xxx'` | 缺 R 包 | 见 `hf-db-ops` 技能的包清单，先征得同意再装 |
| 页面 200 但一直转圈 | 首次登录需 `hf_db_init()` 建表/播种 | 看 `Log/DSH/out.log` 有无报错 |

## 铁律

- 绝不对 **8999 / `HF_HungFo.DB` / `app.R`** 做任何写操作或重启。
- 起停只在 DSH 里用 9999；正式实例由你自己管理。
