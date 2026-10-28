---
description: SuperITOM2 R/Shiny 编码规范与数据访问约定，写 R 代码时生效
alwaysApply: true
enabled: true
---

# 编码与数据访问规范（SuperITOM2）

> 本规则始终注入。**写任何 R 代码、碰任何数据库表前，先读本规范。**

## 一、数据库访问（🔴 硬性）

```r
con <- db_connect()
tryCatch({
  # DB 操作
}, error = function(e) {
  log_user_error(...)
  return(list(success = FALSE, message = e$message))
}, finally = {
  db_disconnect(con)  # 必须断开！
})
```

- 查询/SQL 过滤**必须下推到 SQL**（`WHERE` 子句），禁止全表捞出后在 R 里过滤。
- 参数**必须绑定**，禁止字符串拼接 SQL（防注入）。
- 改查询前，先打开底层数据层文件核对实际字段名，不要凭记忆写列名。

## 二、编号规则（🔴 硬性，勿改）

| 类型 | 格式 |
|------|------|
| 工单号 | `ITS + YYYYMMDD + 3位流水` |
| 项目号 | `PRJ + YYYYMMDD + 3位流水` |
| 任务号 | `TSK + YYYYMMDD + 3位流水` |
| 巡检计划号 | `INS-PLAN-YYYYMMDD-3位流水` |
| 巡检任务号 | `INS-TSK-YYYYMMDD-3位流水` |
| 开发日志号 | `DL + YYYYMMDD + 3位流水` |

- 编号生成需**防并发重试机制**。

## 三、编码/日志（🟡 默认）

- Windows 命令输出：`iconv(result, from = "GBK", to = "UTF-8", sub = "")`。
- 用户操作日志：`log_user_operation(operation, target, operator, details)` → 写入 `Logs/user_operations.log`。
- 日志格式：`[timestamp] 操作: xxx, 用户: xxx, 操作者: xxx, 详情: xxx`。

## 四、用户角色（🔴 硬性，勿改）

- `admin`（全部权限）、`it_desk`（派发工单）、`it_engineer`（接收工单）、`sys_engineer`（系统工程师）、`user`（普通用户）。

## 五、权限检查（🔴 硬性）

```r
is_admin <- !is.null(rv$current_user) && nrow(rv$current_user) > 0 &&
            rv$current_user$role[1] == "admin"
```

- 权限判断用 `can_access("模块名")`，与 `rbac_permissions` 表对齐。

## 六、响应式（🟡 默认，见 01 号规则）

- 数据刷新用 `rv$xxx_trigger` 触发器。
- 条件渲染：`outputOptions(output, "xxx", suspendWhenHidden = FALSE)`。
