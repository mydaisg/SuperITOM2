#!/usr/bin/env Rscript
##################
# HungFo 通用数据命令行工具（hf_gh_bash.r）
# 纯函数式：每条命令 = 一个只读纯函数，函数式括号语法调用。
# 与 hf_metatask_runner.r 平级，独立于 Shiny，Rscript 直接调用。
#
# 用法：
#   Rscript hf_gh_bash.r "gh-get(kb,100,22)"            # 查 kb 表最新 100 条（字段 22 个，SELECT * 全量返回）
#   Rscript hf_gh_bash.r "gh-get(kb)"                    # 默认最新 100 条、全字段
#   Rscript hf_gh_bash.r "gh-list()"                     # 列出所有表 + 记录数
#   Rscript hf_gh_bash.r "gh-schema(kb)"                 # 表结构（字段/类型/主键）
#   Rscript hf_gh_bash.r "gh-count(kb)"                  # 记录总数
#   Rscript hf_gh_bash.r "gh-find(kb, status='done')"    # 条件查询（过滤下推 SQL）
#   Rscript hf_gh_bash.r "gh-help()"                     # 帮助
#   全局 flag：--fmt json|table|csv（默认 json）
#   Rscript hf_gh_bash.r "gh-get(kb,100,22)" --fmt table
#
# 说明：
#   - 数据源：HungFo 独立库 DB/HF_HungFo.DB（环境变量 HUNGFO_DB 可覆盖）
#   - “最新”口径：ORDER BY 主键/rowid DESC + LIMIT n
#   - 表名 / 字段名一律白名单校验（sqlite_master / PRAGMA table_info），防注入
##################

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressMessages({ library(DBI); library(RSQLite); library(jsonlite) })

# ── 根目录 / 数据库路径（与 hf_metatask_runner.r 一致）──
gh_home <- function() {
  env <- Sys.getenv("HUNGFO_HOME")
  if (nzchar(env)) return(env)
  if (file.exists(file.path(getwd(), "hf_core.r"))) return(getwd())
  if (file.exists(file.path(getwd(), "Script", "hungfo", "hf_core.r"))) return(file.path(getwd(), "Script", "hungfo"))
  getwd()
}
DB_PATH <- Sys.getenv("HUNGFO_DB", unset = "")
if (!nzchar(DB_PATH)) DB_PATH <- file.path(gh_home(), "DB", "HF_HungFo.DB")

# ══════════════════════════════════════════════════════════════════
# 数据访问层
# ══════════════════════════════════════════════════════════════════
gh_db <- function() {
  dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
  con <- dbConnect(SQLite(), DB_PATH)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}
gh_esc <- function(x) gsub("'", "''", as.character(x))

# 表名白名单（从 sqlite_master 取真实表，杜绝注入）
gh_table_names <- function(con) {
  tryCatch(
    dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")$name,
    error = function(e) character(0))
}
# 表字段（PRAGMA table_info：cid/name/type/notnull/dflt_value/pk）
gh_columns <- function(con, table) {
  tryCatch(dbGetQuery(con, sprintf("PRAGMA table_info(%s)", table)), error = function(e) data.frame())
}
# “最新”排序列：优先主键 id，否则主键列，最后 rowid
gh_order_col <- function(cols) {
  if (is.null(cols) || nrow(cols) == 0) return("rowid")
  if ("id" %in% cols$name) return("id")
  pk <- cols$name[cols$pk == 1]
  if (length(pk) > 0) return(pk[1])
  "rowid"
}

# ══════════════════════════════════════════════════════════════════
# 解析层：函数式括号语法 "gh-get(kb,100,22)" → list(cmd, args)
# ══════════════════════════════════════════════════════════════════
gh_parse <- function(s) {
  s <- trimws(s)
  if (is.na(s) || !nzchar(s)) return(list(cmd = "", args = list()))
  p <- regexpr("(", s, fixed = TRUE)[1]
  if (p < 0) return(list(cmd = s, args = list()))
  cmd <- trimws(substr(s, 1, p - 1))
  inner <- substr(s, p + 1, nchar(s))
  if (nchar(inner) > 0 && endsWith(inner, ")")) inner <- substr(inner, 1, nchar(inner) - 1)
  args <- gh_split_args(inner)
  list(cmd = cmd, args = lapply(args, gh_cast))
}
# 逗号拆分（尊重单/双引号内的逗号，引号字符本身保留，供 where 的 SQL 字面量使用）
gh_split_args <- function(inner) {
  inner <- trimws(inner)
  if (!nzchar(inner)) return(character(0))
  chars <- strsplit(inner, "", fixed = TRUE)[[1]]
  res <- character(0); cur <- ""; quote <- ""
  for (ch in chars) {
    if (quote == "") {
      if (ch == "'" || ch == '"') { quote <- ch; cur <- paste0(cur, ch) }
      else if (ch == ",") { res <- c(res, trimws(cur)); cur <- "" }
      else cur <- paste0(cur, ch)
    } else {
      cur <- paste0(cur, ch)
      if (ch == quote) quote <- ""
    }
  }
  c(res, trimws(cur))
}
# 参数整体被引号包裹时去外层引号；纯数字字符串 → numeric；其余原样（内部引号保留）
gh_cast <- function(x) {
  x <- trimws(x)
  if (nchar(x) >= 2) {
    first <- substr(x, 1, 1); last <- substr(x, nchar(x), nchar(x))
    if ((first == "'" && last == "'") || (first == '"' && last == '"')) {
      x <- substr(x, 2, nchar(x) - 1)
    }
  }
  if (grepl("^[+-]?[0-9]+$", x)) as.numeric(x) else x
}

# ══════════════════════════════════════════════════════════════════
# 命令层：纯函数（只读，零副作用）
# ══════════════════════════════════════════════════════════════════

# gh-get(<table>[, <limit=100>][, <fields>])：查最新 N 条，SELECT * 全量返回
gh_get <- function(table, limit = 100, fields = NULL) {
  con <- gh_db(); on.exit(dbDisconnect(con))
  if (is.null(table) || !nzchar(as.character(table))) {
    return(list(success = FALSE, message = "缺少表名"))
  }
  table <- as.character(table)
  tabs <- gh_table_names(con)
  if (!(table %in% tabs)) {
    return(list(success = FALSE, message = paste0("表不存在: ", table), tables = tabs))
  }
  cols <- gh_columns(con, table)
  order_col <- gh_order_col(cols)
  n <- suppressWarnings(as.integer(limit))
  if (is.na(n) || n <= 0) n <- 100
  df <- dbGetQuery(con, sprintf("SELECT * FROM %s ORDER BY %s DESC LIMIT %d", table, order_col, n))
  actual <- ncol(df)
  res <- list(
    success = TRUE, table = table, rows = nrow(df), limit = n,
    fields = actual, field_names = names(df), order_by = order_col,
    data = df)
  exp_fields <- if (is.null(fields)) NULL else suppressWarnings(as.integer(fields))
  if (!is.null(exp_fields) && !is.na(exp_fields)) {
    res$expected_fields <- exp_fields
    if (exp_fields != actual) {
      res$note <- sprintf("字段数不匹配：期望 %d，实际 %d", exp_fields, actual)
    }
  }
  res
}

# gh-list()：列出所有表 + 记录数
gh_list <- function() {
  con <- gh_db(); on.exit(dbDisconnect(con))
  tabs <- gh_table_names(con)
  if (length(tabs) == 0) return(list(success = TRUE, count = 0, tables = data.frame()))
  rows <- vapply(tabs, function(t) {
    tryCatch(as.integer(dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t))$n[1]),
             error = function(e) NA_integer_)
  }, integer(1))
  df <- data.frame(table = unname(tabs), rows = unname(rows), stringsAsFactors = FALSE)
  list(success = TRUE, count = nrow(df), tables = df)
}

# gh-schema(<table>)：表结构（字段/类型/主键）
gh_schema <- function(table) {
  con <- gh_db(); on.exit(dbDisconnect(con))
  if (is.null(table) || !nzchar(as.character(table))) {
    return(list(success = FALSE, message = "缺少表名"))
  }
  table <- as.character(table)
  tabs <- gh_table_names(con)
  if (!(table %in% tabs)) {
    return(list(success = FALSE, message = paste0("表不存在: ", table)))
  }
  cols <- gh_columns(con, table)
  if (nrow(cols) == 0) {
    return(list(success = FALSE, message = paste0("无法读取表结构: ", table)))
  }
  df <- data.frame(
    cid = cols$cid, name = cols$name, type = cols$type,
    notnull = cols$notnull, pk = cols$pk,
    dflt = ifelse(is.na(cols$dflt_value), "", as.character(cols$dflt_value)),
    stringsAsFactors = FALSE)
  list(success = TRUE, table = table, fields = nrow(df), schema = df)
}

# gh-count(<table>)：记录总数
gh_count <- function(table) {
  con <- gh_db(); on.exit(dbDisconnect(con))
  if (is.null(table) || !nzchar(as.character(table))) {
    return(list(success = FALSE, message = "缺少表名"))
  }
  table <- as.character(table)
  tabs <- gh_table_names(con)
  if (!(table %in% tabs)) {
    return(list(success = FALSE, message = paste0("表不存在: ", table)))
  }
  n <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", table))$n[1]
  list(success = TRUE, table = table, count = as.integer(n))
}

# gh-find(<table>, <where>)：条件查询（过滤下推 SQL，只读）
gh_find <- function(table, where = NULL) {
  con <- gh_db(); on.exit(dbDisconnect(con))
  if (is.null(table) || !nzchar(as.character(table))) {
    return(list(success = FALSE, message = "缺少表名"))
  }
  table <- as.character(table)
  tabs <- gh_table_names(con)
  if (!(table %in% tabs)) {
    return(list(success = FALSE, message = paste0("表不存在: ", table)))
  }
  whr <- if (is.null(where) || !nzchar(as.character(where))) "1=1" else as.character(where)
  if (grepl(";", whr, fixed = TRUE)) {
    return(list(success = FALSE, message = "where 条件不允许包含分号"))
  }
  cols <- gh_columns(con, table)
  order_col <- gh_order_col(cols)
  df <- dbGetQuery(con, sprintf(
    "SELECT * FROM %s WHERE %s ORDER BY %s DESC LIMIT 500", table, whr, order_col))
  list(success = TRUE, table = table, rows = nrow(df), where = whr, data = df)
}

# gh-help()：帮助
gh_help <- function() {
  list(success = TRUE, commands = c(
    "gh-get(<table>[, <limit=100>][, <fields>])  — 查最新 N 条（SELECT * 全量返回）",
    "gh-list()                                     — 列出所有表 + 记录数",
    "gh-schema(<table>)                            — 表结构（字段/类型/主键）",
    "gh-count(<table>)                             — 记录总数",
    "gh-find(<table>, <where>)                     — 条件查询（下推 SQL）",
    "gh-help()                                     — 本帮助"),
    note = "全局 flag：--fmt sql|json|csv（默认 sql；table 为 sql 别名）")
}

# 命令注册表（命令名 → 纯函数）
GH_COMMANDS <- list(
  "gh-get"    = gh_get,
  "gh-list"   = gh_list,
  "gh-schema" = gh_schema,
  "gh-count"  = gh_count,
  "gh-find"   = gh_find,
  "gh-help"   = gh_help
)

# ══════════════════════════════════════════════════════════════════
# 输出层：sql（默认，传统 SQL 边框表格）/ json / csv
# ══════════════════════════════════════════════════════════════════
gh_find_df <- function(res) {
  for (nm in c("data", "schema", "tables")) {
    if (nm %in% names(res) && is.data.frame(res[[nm]])) return(list(name = nm, df = res[[nm]]))
  }
  NULL
}

# 显示宽度（中文等宽字符按 2 列，保证终端对齐）
gh_w <- function(s) {
  s <- as.character(s)
  if (length(s) == 0 || is.na(s)) s <- ""
  nchar(s, type = "width")
}
# 右侧补空格到显示宽度 w
gh_pad <- function(s, w) {
  s <- as.character(s)
  if (is.na(s)) s <- ""
  paste0(s, strrep(" ", max(0, w - nchar(s, type = "width"))))
}
# 数据框 → 传统 SQL 边框表格（MySQL +---+ 风格）
gh_sql_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("(empty)")
  m <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  hdr <- colnames(df)
  widths <- integer(ncol(m))
  for (j in seq_len(ncol(m))) {
    widths[j] <- max(c(gh_w(hdr[j]), vapply(m[[j]], gh_w, integer(1)), 0L))
  }
  border <- paste0("+", paste(vapply(widths, function(w) strrep("-", w + 2), character(1)), collapse = "+"), "+")
  line <- function(vals) paste0("| ", paste(mapply(gh_pad, as.character(vals), widths), collapse = " | "), " |")
  out <- c(border, line(hdr), border)
  for (i in seq_len(nrow(m))) out <- c(out, line(m[i, ]))
  c(out, border)
}

# SQL 样式渲染：优先打印 data.frame 边框表格，否则 key: value 逐行
gh_render_sql <- function(res) {
  found <- gh_find_df(res)
  if (!is.null(found)) {
    cat(paste(gh_sql_table(found$df), collapse = "\n"), "\n")
    return(invisible())
  }
  for (nm in names(res)) {
    v <- res[[nm]]
    if (is.data.frame(v)) next
    if (is.list(v)) {
      cat(sprintf("%-14s: %s\n", nm, toJSON(v, auto_unbox = TRUE, null = "null")))
    } else if (length(v) > 1) {
      cat(sprintf("%s:\n", nm))
      for (x in v) cat(sprintf("  - %s\n", x))
    } else {
      cat(sprintf("%-14s: %s\n", nm, paste(v, collapse = ", ")))
    }
  }
}

gh_render <- function(res, fmt = "sql") {
  fmt <- tolower(fmt)
  if (fmt == "json") {
    cat(toJSON(res, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), "\n")
  } else if (fmt == "csv") {
    found <- gh_find_df(res)
    if (!is.null(found)) {
      write.csv(found$df, row.names = FALSE, file = stdout())
    } else {
      cat(toJSON(res, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), "\n")
    }
  } else {
    # sql（默认）与 table 别名：传统 SQL 边框表格
    gh_render_sql(res)
  }
}

# ══════════════════════════════════════════════════════════════════
# 主入口
# ══════════════════════════════════════════════════════════════════
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  gh_render(gh_help(), "sql")
  quit(status = 0)
}

# 剥离全局 flag（--fmt <v>），其余合并为函数式命令字符串
fmt <- "sql"
cmd_input <- NULL
i <- 1
while (i <= length(args)) {
  if (args[i] == "--fmt" && i < length(args)) {
    fmt <- args[i + 1]; i <- i + 2
  } else if (is.null(cmd_input)) {
    cmd_input <- args[i]; i <- i + 1
  } else {
    cmd_input <- paste(cmd_input, args[i]); i <- i + 1
  }
}

p <- gh_parse(cmd_input)
if (p$cmd == "" || is.null(GH_COMMANDS[[p$cmd]])) {
  gh_render(gh_help(), fmt)
  cat(sprintf("\n[未知命令] %s\n", p$cmd))
  quit(status = 1)
}
res <- do.call(GH_COMMANDS[[p$cmd]], p$args)
gh_render(res, fmt)
quit(status = 0)
