# 通用数据导入模块 - 数据层
# 功能：读取 Excel → Sheet 选择 → 字段选择/编辑 → 导入 SQLite → 生成可被引用的数据列表
# 数据表：
#   import_datasets  — 数据集（导入批次元数据 + 字段映射 JSON）
#   import_records   — 导入的数据记录（列名→值 JSON，供其它模块引用）

##################
# 工具函数
##################

# 生成数据集编号
data_import_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    prefix <- sprintf("IMP%s", format(Sys.time(), "%Y%m%d"))
    r <- dbGetQuery(con, sprintf(
      "SELECT dataset_no FROM import_datasets WHERE dataset_no LIKE '%s%%' ORDER BY dataset_no DESC LIMIT 1", prefix))
    if (nrow(r) == 0) {
      sprintf("%s%03d", prefix, 1L)
    } else {
      last_seq <- suppressWarnings(as.integer(substr(r$dataset_no[1], nchar(prefix) + 1, nchar(r$dataset_no[1]))))
      if (is.na(last_seq)) last_seq <- 0L
      sprintf("%s%03d", prefix, last_seq + 1L)
    }
  }, error = function(e) {
    sprintf("IMP%s%03d", format(Sys.time(), "%Y%m%d"), 1L)
  }, finally = { db_disconnect(con) })
}

# 读取 Excel 的所有 Sheet 名称
data_import_list_sheets <- function(file_path) {
  if (!requireNamespace("readxl", quietly = TRUE))
    return(character(0))
  tryCatch(readxl::excel_sheets(file_path), error = function(e) character(0))
}

# 读取指定 Sheet 的前 N 行预览（用于字段选择/编辑）
data_import_preview <- function(file_path, sheet, n = 10) {
  if (!requireNamespace("readxl", quietly = TRUE))
    return(data.frame())
  tryCatch({
    df <- readxl::read_excel(file_path, sheet = sheet, n_max = n)
    # 统一列名为字符串（处理空列名）
    if (!is.null(df)) {
      names(df) <- make.unique(ifelse(is.na(names(df)) | names(df) == "", "列", names(df)))
    }
    df
  }, error = function(e) data.frame())
}

# 读取完整 Sheet 数据
data_import_read_full <- function(file_path, sheet) {
  if (!requireNamespace("readxl", quietly = TRUE))
    return(list(success = FALSE, message = "缺少 readxl 包"))
  tryCatch({
    df <- readxl::read_excel(file_path, sheet = sheet)
    if (!is.null(df)) {
      names(df) <- make.unique(ifelse(is.na(names(df)) | names(df) == "", "列", names(df)))
    }
    list(success = TRUE, data = df)
  }, error = function(e) list(success = FALSE, message = paste("读取失败:", e$message)))
}

##################
# 数据集 CRUD
##################

# 获取所有数据集
data_import_get_datasets <- function() {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, "SELECT * FROM import_datasets ORDER BY id DESC")
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取单个数据集
data_import_get_dataset <- function(id) {
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf("SELECT * FROM import_datasets WHERE id = %d", as.integer(id)))
    if (nrow(r) == 0) NULL else r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# 按数据集名称获取数据集（精确匹配，返回最新的那条）
data_import_get_dataset_by_name <- function(name) {
  if (is.null(name) || nchar(trimws(name)) == 0) return(NULL)
  con <- db_connect()
  tryCatch({
    r <- dbGetQuery(con, sprintf(
      "SELECT * FROM import_datasets WHERE name = '%s' ORDER BY id DESC LIMIT 1",
      gsub("'", "''", trimws(name))))
    if (nrow(r) == 0) NULL else r
  }, error = function(e) NULL, finally = { db_disconnect(con) })
}

# 按数据集名称模糊搜索（返回 data.frame）
data_import_search_datasets <- function(keyword) {
  con <- db_connect()
  tryCatch({
    if (is.null(keyword) || nchar(trimws(keyword)) == 0) {
      dbGetQuery(con, "SELECT * FROM import_datasets ORDER BY id DESC")
    } else {
      dbGetQuery(con, sprintf(
        "SELECT * FROM import_datasets WHERE name LIKE '%%%s%%' OR dataset_no LIKE '%%%s%%' ORDER BY id DESC",
        gsub("'", "''", trimws(keyword)), gsub("'", "''", trimws(keyword))))
    }
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 内部工具：将 df 的每条记录序列化为「列名→值 JSON」并批量插入
# 参数：con(已建立事务的连接)、dataset_id、df
# 返回插入的行数（在事务内，由调用方 commit/rollback）
data_import_insert_records <- function(con, dataset_id, df) {
  n <- nrow(df)
  if (n == 0) return(0L)
  rows <- character(n)
  for (i in seq_len(n)) {
    rec <- as.list(df[i, , drop = FALSE])
    rec <- lapply(rec, function(v) {
      v <- v[1]
      if (length(v) == 0 || is.na(v)) return(NULL)
      as.character(v)
    })
    rows[i] <- sprintf("(%d, '%s')", dataset_id,
      gsub("'", "''", jsonlite::toJSON(rec, auto_unbox = TRUE)))
  }
  batch_size <- 500L
  for (s in seq(1, n, by = batch_size)) {
    e <- min(s + batch_size - 1, n)
    query <- sprintf("INSERT INTO import_records (dataset_id, data_json) VALUES %s",
      paste(rows[s:e], collapse = ","))
    dbExecute(con, query)
  }
  n
}

# 创建数据集并导入记录
# 参数：
#   name      数据集名称
#   source    来源文件名
#   sheet     Sheet 名称
#   df        要导入的 data.frame
#   field_map 字段映射 list：list(源列名 = 目标字段名)（可选，NULL 则用原始列名）
#   operator  操作人
# 返回 list(success, dataset_no, dataset_id, count)
data_import_create <- function(name, source, sheet, df, field_map = NULL, operator = "系统") {
  if (is.null(df) || nrow(df) == 0)
    return(list(success = FALSE, message = "没有可导入的数据"))

  name <- trimws(name)
  if (is.null(name) || nchar(name) == 0)
    return(list(success = FALSE, message = "数据集名称不能为空"))

  # 字段映射：NULL 则保持原始列名
  src_cols <- names(df)
  if (is.null(field_map)) {
    field_map <- setNames(as.list(src_cols), src_cols)
  } else {
    # 只保留实际存在的源列
    field_map <- field_map[names(field_map) %in% src_cols]
    if (length(field_map) == 0)
      return(list(success = FALSE, message = "字段映射为空"))
  }

  dataset_no <- data_import_generate_no()
  field_map_json <- jsonlite::toJSON(field_map, auto_unbox = TRUE)

  con <- db_connect()
  tryCatch({
    dbBegin(con)
    # 1. 创建数据集
    dbExecute(con, sprintf(
      "INSERT INTO import_datasets (dataset_no, name, source, sheet, field_map, created_by, created_at) VALUES ('%s','%s','%s','%s','%s','%s', datetime('now','localtime'))",
      dataset_no,
      gsub("'", "''", name),
      gsub("'", "''", source),
      gsub("'", "''", sheet),
      gsub("'", "''", field_map_json),
      gsub("'", "''", operator)))
    dataset_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]

    # 2. 批量插入记录
    n <- data_import_insert_records(con, dataset_id, df)

    dbCommit(con)
    list(success = TRUE, dataset_no = dataset_no, dataset_id = dataset_id, count = n,
         message = sprintf("成功导入 %d 条记录", n))
  }, error = function(e) {
    tryCatch(dbRollback(con), error = function(e2) NULL)
    list(success = FALSE, message = paste("导入失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 追加记录到已有数据集（不新建数据集，直接往现有 dataset 下加记录）
# 参数：
#   dataset_id  目标数据集 id（或通过 data_import_get_id_by_name(name) 获取）
#   df          要追加的 data.frame（列名应与目标数据集一致，不一致的列会以各自列名存储）
# 返回 list(success, dataset_id, count, total)
data_import_append <- function(dataset_id, df) {
  if (is.null(df) || nrow(df) == 0)
    return(list(success = FALSE, message = "没有可追加的数据"))
  dataset_id <- as.integer(dataset_id)
  if (is.na(dataset_id) || dataset_id <= 0)
    return(list(success = FALSE, message = "无效的数据集 id"))

  # 确认数据集存在
  ds <- data_import_get_dataset(dataset_id)
  if (is.null(ds))
    return(list(success = FALSE, message = "数据集不存在"))

  con <- db_connect()
  tryCatch({
    dbBegin(con)
    n <- data_import_insert_records(con, dataset_id, df)
    dbCommit(con)
    total <- dbGetQuery(con, sprintf(
      "SELECT COUNT(*) AS c FROM import_records WHERE dataset_id = %d", dataset_id))$c[1]
    list(success = TRUE, dataset_id = dataset_id, count = n, total = total,
         message = sprintf("成功追加 %d 条记录（数据集现有 %d 条）", n, total))
  }, error = function(e) {
    tryCatch(dbRollback(con), error = function(e2) NULL)
    list(success = FALSE, message = paste("追加失败:", e$message))
  }, finally = { db_disconnect(con) })
}

# 删除数据集（级联删除记录）
data_import_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    dbBegin(con)
    dbExecute(con, sprintf("DELETE FROM import_records WHERE dataset_id = %d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM import_datasets WHERE id = %d", as.integer(id)))
    dbCommit(con)
    list(success = TRUE, message = "已删除")
  }, error = function(e) {
    tryCatch(dbRollback(con), error = function(e2) NULL)
    list(success = FALSE, message = paste("删除失败:", e$message))
  }, finally = { db_disconnect(con) })
}

##################
# 数据记录查询（供其它模块引用）
##################

# 获取某数据集下的所有记录（还原为 data.frame）
data_import_get_records <- function(dataset_id) {
  con <- db_connect()
  tryCatch({
    rows <- dbGetQuery(con, sprintf(
      "SELECT id, data_json FROM import_records WHERE dataset_id = %d ORDER BY id", as.integer(dataset_id)))
    if (nrow(rows) == 0) return(data.frame())
    # 解析 JSON 为 data.frame
    parsed <- lapply(rows$data_json, function(j) {
      tryCatch(jsonlite::fromJSON(j), error = function(e) list())
    })
    # 统一所有字段名
    all_keys <- unique(unlist(lapply(parsed, names)))
    if (length(all_keys) == 0) return(data.frame())
    mat <- matrix(NA_character_, nrow = length(parsed), ncol = length(all_keys),
                  dimnames = list(NULL, all_keys))
    for (i in seq_along(parsed)) {
      p <- parsed[[i]]
      if (length(p) > 0) {
        # 每个字段值强制转标量字符串：
        # 标量/字符串 → 直接转；空对象{} / 空数组[] → ""；嵌套对象/数组 → JSON 文本
        vals <- vapply(names(p), function(k) {
          v <- p[[k]]
          if (is.null(v) || length(v) == 0) return("")
          if (length(v) == 1 && !is.list(v)) return(as.character(v))
          # 嵌套结构（list/对象/数组）→ 序列化为 JSON 文本
          tryCatch(jsonlite::toJSON(v, auto_unbox = TRUE), error = function(e) "")
        }, character(1), USE.NAMES = FALSE)
        mat[i, names(p)] <- vals
      }
    }
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    cbind(row_id = rows$id, df, stringsAsFactors = FALSE)
  }, error = function(e) data.frame(), finally = { db_disconnect(con) })
}

# 获取数据集的字段映射（还原为命名 list）
data_import_get_field_map <- function(dataset_id) {
  ds <- data_import_get_dataset(dataset_id)
  if (is.null(ds) || is.na(ds$field_map[1]) || nchar(ds$field_map[1]) == 0)
    return(NULL)
  tryCatch(jsonlite::fromJSON(ds$field_map[1]), error = function(e) NULL)
}

# 按数据集名称直接获取记录（供其它模块引用的便捷入口）
# 示例：data_import_get_records_by_name("流程实例0917")
data_import_get_records_by_name <- function(name) {
  ds <- data_import_get_dataset_by_name(name)
  if (is.null(ds)) return(data.frame())
  data_import_get_records(ds$id[1])
}

# 按数据集名称获取 dataset_id（供其它模块引用）
# 示例：did <- data_import_get_id_by_name("流程实例0917")
data_import_get_id_by_name <- function(name) {
  ds <- data_import_get_dataset_by_name(name)
  if (is.null(ds)) return(NULL)
  ds$id[1]
}
