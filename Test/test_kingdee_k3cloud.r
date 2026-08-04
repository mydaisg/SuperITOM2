# ================================================
# 金蝶云星空 连接测试 & 数据回传
# 方案：SOL20260731001 ERP集成开发
# ================================================

cat("\n═══════════════════════════════════════════\n")
cat("  金蝶云星空 WebAPI 连接测试\n")
cat("  方案：SOL20260731001 ERP集成开发\n")
cat("═══════════════════════════════════════════\n\n")

# 加载依赖
library(httr)
library(jsonlite)
library(openssl)

# 加载集成模块
source("Script/kingdee_k3cloud.r")

# ── 测试 1：登录认证 ──
cat("[1/4] 登录认证 (LoginByAppSecret)...\n")
login <- k3_login()
if (login$success) {
  cat("  ✓ 登录成功\n")
  cat("  SessionID:", substr(login$session_id, 1, 20), "...\n")
  cat("  消息:", login$message, "\n")
} else {
  cat("  ✗ 登录失败:", login$message, "\n")
  cat("  HTTP状态:", login$status_code, "\n")
  if (!is.null(login$raw_response)) {
    cat("  原始响应:\n", substr(login$raw_response, 1, 500), "\n")
  }
}

# ── 测试 2：查询客户 ──
cat("\n[2/4] 查询基础资料：客户 (BD_Customer)...\n")
if (login$success) {
  sid <- login$session_id
  customers <- k3_query("BD_Customer", "FNumber,FName", "", sid)
  if (customers$success) {
    cat("  ✓ 查询成功:", customers$message, "\n")
    if (is.data.frame(customers$data) && nrow(customers$data) > 0) {
      print(head(customers$data, 10))
    } else {
      cat("  (数据为空，测试账套可能无客户数据)\n")
    }
  } else {
    cat("  ✗ 查询失败:", customers$message, "\n")
  }
}

# ── 测试 3：查询部门 ──
cat("\n[3/4] 查询基础资料：部门 (BD_Department)...\n")
if (login$success) {
  departments <- k3_query("BD_Department", "FNumber,FName", "", sid)
  if (departments$success) {
    cat("  ✓ 查询成功:", departments$message, "\n")
    if (is.data.frame(departments$data) && nrow(departments$data) > 0) {
      print(head(departments$data, 10))
    } else {
      cat("  (数据为空)\n")
    }
  } else {
    cat("  ✗ 查询失败:", departments$message, "\n")
  }
}

# ── 测试 4：查询物料 ──
cat("\n[4/4] 查询基础资料：物料 (BD_Material)...\n")
if (login$success) {
  materials <- k3_query("BD_Material", "FNumber,FName,FSpecification", "", sid)
  if (materials$success) {
    cat("  ✓ 查询成功:", materials$message, "\n")
    if (is.data.frame(materials$data) && nrow(materials$data) > 0) {
      print(head(materials$data, 10))
    } else {
      cat("  (数据为空)\n")
    }
  } else {
    cat("  ✗ 查询失败:", materials$message, "\n")
  }
}

# ── 生成 JSON 测试报告 ──
cat("\n═══════════════════════════════════════════\n")
cat("  生成测试报告...\n")

report <- k3_test_connection()

# 写入 JSON 文件，供 HTML 看板读取
output_path <- "Test/k3cloud_test_report.json"
# 将 data.frame 转为可序列化的格式
report_json <- report
if (report_json$login$success && !is.null(report_json$login$session_id)) {
  # 脱敏 session_id
  report_json$login$session_id <- paste0(substr(report_json$login$session_id, 1, 8), "***")
}

# 处理 queries 中的 data.frame
for (name in names(report_json$queries)) {
  q <- report_json$queries[[name]]
  if (is.list(q) && is.data.frame(q$data)) {
    q$data <- lapply(1:min(nrow(q$data), 20), function(i) as.list(q$data[i, ]))
    q$total_rows <- nrow(report$queries[[name]]$data)
    report_json$queries[[name]] <- q
  }
}

jsonlite::write_json(report_json, output_path, pretty = TRUE, auto_unbox = TRUE)
cat("  报告已保存到:", output_path, "\n")
cat("═══════════════════════════════════════════\n")
