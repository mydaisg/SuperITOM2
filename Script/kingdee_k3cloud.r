# ================================================
# 金蝶云星空 WebAPI 集成模块
# 方案：SOL20260731001 ERP集成开发
# 实现：LoginByAppSecret 认证 + 基础资料查询/单据保存
# ================================================

# ── 配置常量 ─────────────────────────────────────
K3_CONFIG <- list(
  server_url    = "http://39.108.127.76/k3cloud",
  acct_id       = "693bff83061bf1",
  app_id        = "331221_R6av3Zjs2oD/28TsS7wMQwSL4uRdQqsK",
  app_sec       = "692b1f15a0d34af7b1dbb1f375afbf4e",
  user_name     = "办公协同",
  password      = "Lvcc@260803",
  lc_id         = 2052,           # 简体中文
  product_code  = "QJDG-DMOG-OPNZ-CLUQ-AJKT"
)

# ── 内部工具函数 ──────────────────────────────────

# 生成签名（HMAC-SHA256，与金蝶 Java SDK 一致）
.k3_sign <- function(app_id, app_sec, timestamp) {
  raw_str <- paste0(app_id, timestamp)
  # 优先使用 openssl 包
  if (requireNamespace("openssl", quietly = TRUE)) {
    sig <- openssl::sha256(raw_str, key = app_sec)
    return(as.character(sig))
  }
  # fallback: 用 digest 包
  if (requireNamespace("digest", quietly = TRUE)) {
    sig_raw <- digest::hmac(app_sec, raw_str, algo = "sha256", serialize = FALSE)
    return(sig_raw)
  }
  stop("需要安装 openssl 或 digest 包用于 HMAC-SHA256 签名")
}

# 生成 Cookie 字符串（ASP.NET_SessionId + kdservice-sessionid）
.k3_cookie_header <- function(session_id) {
  # 金蝶的 Cookie 需要拼接 ASP.NET 和 kd 两段
  paste0(
    "ASP.NET_SessionId=", session_id,
    "; kdservice-sessionid=", session_id
  )
}

# ── 核心API函数 ───────────────────────────────────

#' 登录金蝶云星空（LoginByAppSecret）
#' @return list(success, session_id, message, raw_response)
k3_login <- function() {
  if (!requireNamespace("httr", quietly = TRUE)) stop("请安装 httr 包")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("请安装 jsonlite 包")
  if (!requireNamespace("openssl", quietly = TRUE)) stop("请安装 openssl 包")
  
  url <- paste0(K3_CONFIG$server_url, "/Kingdee.BOS.WebApi.ServicesStub.AuthService.LoginByAppSecret.common.kdsvc")
  
  timestamp <- as.character(floor(as.numeric(Sys.time())))
  signature <- .k3_sign(K3_CONFIG$app_id, K3_CONFIG$app_sec, timestamp)
  
  body <- jsonlite::toJSON(list(
    acctID    = K3_CONFIG$acct_id,
    appId     = K3_CONFIG$app_id,
    appSecret = K3_CONFIG$app_sec,
    timestamp = timestamp,
    signature = signature,
    lcId      = K3_CONFIG$lc_id
  ), auto_unbox = TRUE)
  
  tryCatch({
    resp <- httr::POST(url,
      body = body,
      httr::content_type("application/json"),
      httr::add_headers(
        "AppKey"     = K3_CONFIG$app_id,
        "Timestamp"  = timestamp,
        "Signature"  = signature
      ),
      httr::timeout(30)
    )
    
    status <- httr::status_code(resp)
    resp_body <- httr::content(resp, "text", encoding = "UTF-8")
    resp_parsed <- tryCatch(jsonlite::fromJSON(resp_body, simplifyVector = FALSE), error = function(e) NULL)
    
    # 提取 Cookie（ASP.NET_SessionId）
    cookies <- httr::cookies(resp)
    session_id <- NULL
    if (nrow(cookies) > 0) {
      asp_row <- cookies[cookies$name == "ASP.NET_SessionId", ]
      if (nrow(asp_row) > 0) session_id <- asp_row$value[1]
    }
    
    if (status == 200 && !is.null(resp_parsed) && isTRUE(resp_parsed$LoginResultType == 1)) {
      # 提取 kdservice-sessionid
      if (is.null(session_id) && !is.null(resp_parsed$KDSVCSessionId)) {
        session_id <- resp_parsed$KDSVCSessionId
      }
      list(
        success      = TRUE,
        session_id   = session_id,
        message      = resp_parsed$Message %||% "登录成功",
        raw_response = resp_body,
        status_code  = status
      )
    } else {
      msg <- if (!is.null(resp_parsed)) resp_parsed$Message %||% "登录失败" else paste("HTTP", status)
      list(success = FALSE, session_id = NULL, message = msg, raw_response = resp_body, status_code = status)
    }
  }, error = function(e) {
    list(success = FALSE, session_id = NULL, message = paste("请求异常:", e$message), raw_response = NULL, status_code = NA)
  })
}

#' 查看基础资料（View）
#' @param form_id 表单标识，如 "BD_Customer"（客户）、"BD_Department"（部门）
#' @param data_id 数据ID
#' @param session_id 登录后获取的会话ID
#' @return list(success, data, message)
k3_view <- function(form_id, data_id, session_id) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("请安装 httr 包")
  
  url <- paste0(K3_CONFIG$server_url, "/Kingdee.BOS.WebApi.ServicesStub.DynamicFormService.View.common.kdsvc")
  
  body <- jsonlite::toJSON(list(
    formId = form_id,
    data   = list(Id = as.integer(data_id))
  ), auto_unbox = TRUE)
  
  tryCatch({
    resp <- httr::POST(url,
      body = body,
      httr::content_type("application/json"),
      httr::set_cookies(
        "ASP.NET_SessionId" = session_id,
        "kdservice-sessionid" = session_id
      ),
      httr::timeout(30)
    )
    status <- httr::status_code(resp)
    resp_body <- httr::content(resp, "text", encoding = "UTF-8")
    resp_parsed <- tryCatch(jsonlite::fromJSON(resp_body, simplifyVector = FALSE), error = function(e) NULL)
    
    if (status == 200 && !is.null(resp_parsed)) {
      list(success = TRUE, data = resp_parsed, message = "查询成功", status_code = status)
    } else {
      msg <- if (!is.null(resp_parsed)) resp_parsed$Message %||% "查询失败" else paste("HTTP", status)
      list(success = FALSE, data = NULL, message = msg, status_code = status)
    }
  }, error = function(e) {
    list(success = FALSE, data = NULL, message = paste("请求异常:", e$message), status_code = NA)
  })
}

#' 执行单据查询（ExecuteBillQuery）
#' @param form_id 表单标识
#' @param field_keys 字段列表，如 "FNumber,FName"
#' @param filter 过滤条件，如 "FNumber='CUST001'"
#' @param session_id 会话ID
#' @return list(success, data, message)
k3_query <- function(form_id, field_keys = "FNumber,FName", filter = "", session_id = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("请安装 httr 包")
  
  url <- paste0(K3_CONFIG$server_url, "/Kingdee.BOS.WebApi.ServicesStub.DynamicFormService.ExecuteBillQuery.common.kdsvc")
  
  query_data <- list(
    formId    = form_id,
    fieldKeys = field_keys,
    filterString = filter,
    orderString  = "",
    startRow     = 0,
    limit        = 100,
    topRowCount  = 0
  )
  
  body <- jsonlite::toJSON(query_data, auto_unbox = TRUE)
  
  tryCatch({
    resp <- httr::POST(url,
      body = body,
      httr::content_type("application/json"),
      if (!is.null(session_id)) httr::set_cookies(
        "ASP.NET_SessionId" = session_id,
        "kdservice-sessionid" = session_id
      ),
      httr::timeout(30)
    )
    status <- httr::status_code(resp)
    resp_body <- httr::content(resp, "text", encoding = "UTF-8")
    
    # ExecuteBillQuery 返回的是二维数组 JSON（无字段名，只有值）
    # 解析为表格
    parsed <- tryCatch(jsonlite::fromJSON(resp_body, simplifyVector = FALSE), error = function(e) NULL)
    
    if (status == 200 && !is.null(parsed) && length(parsed) > 0) {
      # 第一行可能是字段名，后续行是数据
      field_names <- strsplit(field_keys, ",")[[1]]
      rows <- parsed
      if (length(rows) > 0 && is.character(rows[[1]])) {
        # 解析为 data.frame
        df <- do.call(rbind, lapply(rows, function(r) {
          if (length(r) == length(field_names)) {
            setNames(as.data.frame(as.list(r), stringsAsFactors = FALSE), field_names)
          } else {
            NULL
          }
        }))
        if (is.null(df)) df <- data.frame()
        list(success = TRUE, data = df, message = paste("查询到", nrow(df), "条记录"), status_code = status)
      } else {
        list(success = TRUE, data = parsed, message = "查询成功（原始数据）", status_code = status)
      }
    } else {
      msg <- if (!is.null(parsed)) {
        if (is.list(parsed) && !is.null(parsed$Message)) parsed$Message else "查询失败"
      } else {
        paste("HTTP", status)
      }
      list(success = FALSE, data = NULL, message = msg, status_code = status)
    }
  }, error = function(e) {
    list(success = FALSE, data = NULL, message = paste("请求异常:", e$message), status_code = NA)
  })
}

#' 保存单据（Save）
#' @param form_id 表单标识
#' @param model 业务对象数据（R list）
#' @param session_id 会话ID
#' @return list(success, id, number, message)
k3_save <- function(form_id, model, session_id) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("请安装 httr 包")
  
  url <- paste0(K3_CONFIG$server_url, "/Kingdee.BOS.WebApi.ServicesStub.DynamicFormService.Save.common.kdsvc")
  
  body <- jsonlite::toJSON(list(
    formId = form_id,
    model  = model,
    needUpDateFields = list(),
    needReturnFields = list()
  ), auto_unbox = TRUE)
  
  tryCatch({
    resp <- httr::POST(url,
      body = body,
      httr::content_type("application/json"),
      httr::set_cookies(
        "ASP.NET_SessionId" = session_id,
        "kdservice-sessionid" = session_id
      ),
      httr::timeout(30)
    )
    status <- httr::status_code(resp)
    resp_body <- httr::content(resp, "text", encoding = "UTF-8")
    resp_parsed <- tryCatch(jsonlite::fromJSON(resp_body, simplifyVector = FALSE), error = function(e) NULL)
    
    if (status == 200 && !is.null(resp_parsed) && isTRUE(resp_parsed$Result$ResponseStatus$IsSuccess)) {
      list(
        success = TRUE,
        id      = resp_parsed$Result$Id %||% NA,
        number  = resp_parsed$Result$Number %||% NA,
        message = "保存成功",
        status_code = status
      )
    } else {
      msg <- if (!is.null(resp_parsed)) {
        resp_parsed$Result$ResponseStatus$Errors[[1]]$Message %||%
          resp_parsed$Message %||% "保存失败"
      } else paste("HTTP", status)
      list(success = FALSE, id = NULL, number = NULL, message = msg, status_code = status)
    }
  }, error = function(e) {
    list(success = FALSE, id = NULL, number = NULL, message = paste("请求异常:", e$message), status_code = NA)
  })
}

#' 测试连接：登录 + 查询基础资料
#' @return list(success, login, queries, diagnostics)
k3_test_connection <- function() {
  results <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    config = list(
      server = K3_CONFIG$server_url,
      acct   = K3_CONFIG$acct_id,
      user   = K3_CONFIG$user_name,
      lcId   = K3_CONFIG$lc_id
    ),
    login   = NULL,
    queries = list(),
    diagnostics = list()
  )
  
  # Step 1: 签名测试
  ts <- as.character(floor(as.numeric(Sys.time())))
  results$diagnostics$timestamp_raw <- ts
  results$diagnostics$app_id_preview <- paste0(substr(K3_CONFIG$app_id, 1, 8), "...")
  
  # Step 2: 登录
  login_result <- k3_login()
  results$login <- login_result
  
  if (!login_result$success) {
    results$diagnostics$error <- login_result$message
    return(results)
  }
  
  sid <- login_result$session_id
  
  # Step 3: 查询基础资料
  # 查询客户
  cust_result <- k3_query("BD_Customer", "FNumber,FName", "", sid)
  results$queries$customers <- cust_result
  
  # 查询部门
  dept_result <- k3_query("BD_Department", "FNumber,FName", "", sid)
  results$queries$departments <- dept_result
  
  # 查询物料（如果有）
  mat_result <- k3_query("BD_Material", "FNumber,FName,FSpecification", "FUseStatus=1", sid)
  results$queries$materials <- mat_result
  
  results
}
