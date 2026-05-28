# 性能监控 - 数据层（无代理监控，类似Zabbix简单版）

##################
# 主机管理
##################
sysmon_host_list <- function() {
  con <- db_connect()
  tryCatch({ dbGetQuery(con, "SELECT * FROM sysmon_hosts WHERE is_active=1 ORDER BY hostname") },
  finally={ db_disconnect(con) })
}

sysmon_host_get <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM sysmon_hosts WHERE id=%d",as.integer(id)))
    if (nrow(r)==0) NULL else r
  }, finally={ db_disconnect(con) })
}

sysmon_host_add <- function(hostname, ip, port=0, os_type="windows", credential_id=NULL, remark="") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO sysmon_hosts (hostname,ip,port,os_type,credential_id,remark) VALUES ('%s','%s',%d,'%s',%s,'%s')",
      gsub("'","''",hostname),ip,as.integer(port),os_type,
      ifelse(is.null(credential_id),"NULL",as.character(as.integer(credential_id))),
      gsub("'","''",remark)))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    list(success=TRUE, id=id, message=sprintf("主机 %s(%s) 已添加",hostname,ip))
  }, error=function(e) list(success=FALSE, message=e$message),
  finally={ db_disconnect(con) })
}

sysmon_host_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf("UPDATE sysmon_hosts SET is_active=0 WHERE id=%d",as.integer(id)))
  }, finally={ db_disconnect(con) })
}

sysmon_host_update_status <- function(id, status, response_time_ms=0) {
  con <- db_connect()
  tryCatch({
    now <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
    if (status=="online") {
      dbExecute(con, sprintf("UPDATE sysmon_hosts SET status='%s',last_check='%s',last_online='%s',response_time_ms=%d WHERE id=%d",
        status,now,now,as.integer(response_time_ms),as.integer(id)))
    } else {
      dbExecute(con, sprintf("UPDATE sysmon_hosts SET status='%s',last_check='%s',response_time_ms=%d WHERE id=%d",
        status,now,as.integer(response_time_ms),as.integer(id)))
    }
  }, finally={ db_disconnect(con) })
}

##################
# 检查记录
##################
sysmon_check_log <- function(host_id, check_type, status, response_time_ms=0, detail="") {
  con <- db_connect()
  tryCatch({
    dbExecute(con, sprintf(
      "INSERT INTO sysmon_checks (host_id,check_type,status,response_time_ms,detail) VALUES (%d,'%s','%s',%d,'%s')",
      as.integer(host_id),check_type,status,as.integer(response_time_ms),gsub("'","''",detail)))
  }, finally={ db_disconnect(con) })
}

sysmon_check_history <- function(host_id, limit=20) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM sysmon_checks WHERE host_id=%d ORDER BY checked_at DESC LIMIT %d",as.integer(host_id),as.integer(limit)))
  }, finally={ db_disconnect(con) })
}

##################
# 连通性检测
##################
sysmon_ping_check <- function(ip) {
  start <- Sys.time()
  result <- tryCatch({
    # 用 system2 替代 system，避免 ignore.stderr 在 Windows 上的不可靠行为
    out <- system2("ping", c("-n", "1", "-w", "3000", ip), stdout=TRUE, stderr=TRUE)
    out <- iconv(out, from="GBK", to="UTF-8", sub="")
    elapsed <- as.integer(as.numeric(difftime(Sys.time(), start, units="secs")) * 1000)
    output_text <- paste(out, collapse=" ")
    # 成功：匹配英文/中文的 Ping 成功标识
    if (grepl("TTL=|Reply from|来自|回复|bytes=|time[<=>]|time=", output_text, ignore.case=TRUE)) {
      list(success=TRUE, ms=elapsed, detail="Ping OK")
    } else if (grepl("无法访问|超时|timed out|unreachable|could not find|找不到主机|请求超时", output_text, ignore.case=TRUE)) {
      list(success=FALSE, ms=elapsed, detail="Ping 目标不可达")
    } else {
      list(success=FALSE, ms=elapsed, detail="Ping 无响应")
    }
  }, error=function(e) list(success=FALSE, ms=0, detail=paste("Ping失败:", e$message)))
  result
}

sysmon_port_check <- function(ip, port) {
  start <- Sys.time()
  result <- tryCatch({
    con <- suppressWarnings(socketConnection(host=ip, port=as.integer(port), open="r+b", blocking=TRUE, timeout=3))
    close(con)
    elapsed <- as.integer(as.numeric(difftime(Sys.time(), start, units="secs")) * 1000)
    list(success=TRUE, ms=elapsed, detail=sprintf("端口 %d 开放", port))
  }, error=function(e) {
    elapsed <- as.integer(as.numeric(difftime(Sys.time(), start, units="secs")) * 1000)
    list(success=FALSE, ms=elapsed, detail=sprintf("端口 %d 关闭", port))
  })
  result
}

##################
# 扫描网络（实时回调模式）
##################
sysmon_scan_subnet <- function(subnet, start_ip=1, end_ip=254, progress_callback=NULL, stop_flag=NULL) {
  # subnet: "192.168.1"
  # progress_callback: function(ip, success, ms, detail) 每次检测后回调
  # stop_flag: reactiveVal, 当为TRUE时停止扫描
  hosts <- list()
  for (i in start_ip:end_ip) {
    if (!is.null(stop_flag) && isTRUE(stop_flag())) break
    ip <- sprintf("%s.%d", subnet, i)
    result <- sysmon_ping_check(ip)
    entry <- list(ip=ip, hostname=ip, ms=result$ms)
    if (result$success) {
      tryCatch({
        name <- system(sprintf("nslookup %s", ip), intern=TRUE, ignore.stderr=TRUE)
        name <- iconv(name, from="GBK", to="UTF-8", sub="")
        name_line <- name[grepl("name\\s*=|名称", name, ignore.case=TRUE)]
        if (length(name_line)>0) entry$hostname <- trimws(gsub(".*name\\s*=\\s*|.*名称\\s*:\\s*", "", name_line[1], ignore.case=TRUE))
      }, error=function(e) {})
      hosts[[length(hosts)+1]] <- entry
    }
    if (!is.null(progress_callback)) progress_callback(entry$ip, result$success, result$ms, ifelse(result$success,"存活","无响应"))
  }
  hosts
}

##################
# 本机IP获取
##################
sysmon_get_local_ip <- function() {
  tryCatch({
    result <- system("ipconfig", intern=TRUE, ignore.stderr=TRUE)
    result <- iconv(result, from="GBK", to="UTF-8", sub="")
    # 查找 IPv4 地址（排除 127.0.0.1）
    ip_lines <- result[grepl("IPv4|IP[^C]|192\\.|10\\.|172\\.", result, ignore.case=TRUE)]
    for (line in ip_lines) {
      ips <- regmatches(line, gregexpr("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", line))[[1]]
      ips <- ips[ips != "127.0.0.1"]
      if (length(ips) > 0) return(ips[1])
    }
    # 备用: 用 nslookup 本机名
    hostname <- Sys.info()["nodename"]
    nslookup <- system(sprintf("nslookup %s", hostname), intern=TRUE, ignore.stderr=TRUE)
    nslookup <- iconv(nslookup, from="GBK", to="UTF-8", sub="")
    addr_lines <- nslookup[grepl("Address", nslookup, ignore.case=TRUE)]
    for (line in addr_lines) {
      ips <- regmatches(line, gregexpr("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", line))[[1]]
      ips <- ips[ips != "127.0.0.1"]
      if (length(ips) > 0) return(ips[1])
    }
    "192.168.1"
  }, error=function(e) "192.168.1")
}

sysmon_get_subnet <- function(ip) {
  if (is.null(ip) || ip=="" || ip=="127.0.0.1") return("192.168.1")
  parts <- strsplit(ip, "\\.")[[1]]
  if (length(parts)==4) paste(parts[1], parts[2], parts[3], sep=".") else "192.168.1"
}

##################
# 统计
##################
sysmon_stats <- function() {
  con <- db_connect()
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) as c FROM sysmon_hosts WHERE is_active=1")$c[1]
    online <- dbGetQuery(con, "SELECT COUNT(*) as c FROM sysmon_hosts WHERE is_active=1 AND status='online'")$c[1]
    offline <- dbGetQuery(con, "SELECT COUNT(*) as c FROM sysmon_hosts WHERE is_active=1 AND status='offline'")$c[1]
    unknown <- dbGetQuery(con, "SELECT COUNT(*) as c FROM sysmon_hosts WHERE is_active=1 AND status='unknown'")$c[1]
    list(total=total, online=online, offline=offline, unknown=unknown)
  }, finally={ db_disconnect(con) })
}
