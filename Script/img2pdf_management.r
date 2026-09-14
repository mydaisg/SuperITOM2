##################
# 图片合并 PDF 工具 — 数据层
# 功能：多张图片合并为一个 PDF，支持水印（内容/颜色/字号/位置）
# 记录：生成前的图片存储地址 + 生成后的 PDF 文件路径
##################

# 图片合并 PDF 的输出目录（存 DB 下，避免写 www 目录触发 devmode 热重载回首页）
img2pdf_output_dir <- function() {
  file.path(getwd(), "DB", "img2pdf")
}

# 图片上传持久化目录（生成前的文件存储地址）
img2pdf_upload_dir <- function() {
  file.path(img2pdf_output_dir(), "uploads")
}

# 确保目录存在
img2pdf_ensure_dirs <- function() {
  dir.create(img2pdf_output_dir(), showWarnings = FALSE, recursive = TRUE)
  dir.create(img2pdf_upload_dir(), showWarnings = FALSE, recursive = TRUE)
}

# 生成记录号：I2P + YYYYMMDD + 3位流水
img2pdf_generate_no <- function() {
  con <- db_connect()
  tryCatch({
    date_part <- format(Sys.Date(), "%Y%m%d")
    prefix <- paste0("I2P", date_part)
    q <- sprintf("SELECT record_no FROM img2pdf_records WHERE record_no LIKE '%s%%' ORDER BY record_no DESC LIMIT 1", prefix)
    last <- dbGetQuery(con, q)$record_no
    if (length(last) == 0 || is.na(last[1])) {
      seq <- 1L
    } else {
      seq <- as.integer(substr(last[1], nchar(prefix) + 1, nchar(last[1]))) + 1L
    }
    sprintf("%s%03d", prefix, seq)
  }, error = function(e) {
    sprintf("I2P%s%03d", format(Sys.Date(), "%Y%m%d"), as.integer(runif(1, 1, 999)))
  }, finally = {
    db_disconnect(con)
  })
}

# 将 HEX 颜色（如 "#CCCCCC"）解析为 RGB 分量（0~1）
img2pdf_hex_to_rgb <- function(hex) {
  hex <- gsub("#", "", hex, fixed = TRUE)
  if (nchar(hex) != 6) hex <- "CCCCCC"
  r <- as.integer(paste0("0x", substr(hex, 1, 2))) / 255
  g <- as.integer(paste0("0x", substr(hex, 3, 4))) / 255
  b <- as.integer(paste0("0x", substr(hex, 5, 6))) / 255
  list(r = r, g = g, b = b)
}

# 生成水印透明 PNG（用 grDevices 渲染文字，避免 magick 的 fontconfig 缺失问题）
# 支持：多条水印文字网格平铺（tile）、多个角度（srt 旋转）、透明色（alpha）、数量/间距
# 参数：
#   lines     字符向量：水印文字（多条时在网格中循环排列）
#   width, height  透明画布尺寸（与图片一致）
#   color     水印 HEX 颜色
#   alpha     透明度 0~1（0 全透明，1 不透明）
#   size      字号
#   angles    数字向量：水印旋转角度（度），网格中循环使用
#   cols      水平方向水印数量（列数）
#   rows      垂直方向水印数量（行数）
#   spacing   间距系数 0~1（0=紧密，越大水印间距越稀疏）
img2pdf_build_watermark <- function(lines, width, height, color = "#CCCCCC",
                                    alpha = 0.3, size = 40, angles = 0,
                                    cols = 3, rows = 3, spacing = 0.2) {
  tmp <- tempfile(fileext = ".png")
  rgbc <- img2pdf_hex_to_rgb(color)
  png(tmp, width = width, height = height, bg = "transparent")
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  col <- rgb(rgbc$r, rgbc$g, rgbc$b, alpha)
  cex <- max(0.5, size / 20)
  nlines <- length(lines)
  if (nlines == 0) {
    dev.off(); return(tmp)
  }
  cols <- max(1L, as.integer(cols))
  rows <- max(1L, as.integer(rows))
  spacing <- max(0, min(0.8, as.numeric(spacing)))
  # 网格平铺：每个网格点绘制一条水印（lines 循环、angles 循环）
  # x 方向列坐标：间距系数让相邻水印之间留空隙
  x_positions <- seq(0, 1, length.out = cols)
  y_positions <- seq(0, 1, length.out = rows)
  # 若 cols/rows == 1，则居中
  if (cols == 1) x_positions <- 0.5
  if (rows == 1) y_positions <- 0.5
  k <- 0
  for (r in seq_len(rows)) {
    for (c in seq_len(cols)) {
      k <- k + 1
      txt <- lines[((k - 1) %% nlines) + 1]
      ang <- angles[((k - 1) %% length(angles)) + 1]
      text(x_positions[c], y_positions[r], txt,
           cex = cex, srt = ang, col = col)
    }
  }
  dev.off()
  tmp
}

# 核心：多张图片合并为【一页】PDF
# 流程：读取图片 → 横向/纵向拼接成一页大图 → 平铺水印（多条多角度透明）→ 写 PDF
# 参数：
#   img_paths     字符向量：图片文件绝对路径（按顺序）
#   out_path      输出 PDF 路径
#   watermark     字符向量：水印文字（NULL/空则不加，可多条）
#   wm_color      水印颜色（如 "#CCCCCC" 浅色）
#   wm_alpha      水印透明度 0~1
#   wm_size       水印字号
#   wm_angles     数字向量：水印旋转角度
#   wm_cols       水印水平数量（列数）
#   wm_rows       水印垂直数量（行数）
#   wm_spacing    水印间距系数 0~1
#   stack         拼接方向：FALSE=横向并排，TRUE=纵向堆叠
img2pdf_merge <- function(img_paths, out_path, watermark = NULL,
                          wm_color = "#CCCCCC", wm_alpha = 0.3, wm_size = 40,
                          wm_angles = 0, wm_cols = 3, wm_rows = 3, wm_spacing = 0.2,
                          stack = FALSE) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    return(list(success = FALSE, message = "缺少 magick 包，请先安装：install.packages('magick')"))
  }
  if (length(img_paths) == 0) {
    return(list(success = FALSE, message = "没有可处理的图片"))
  }
  if (is.null(watermark) || length(watermark) == 0 ||
      all(nchar(trimws(watermark)) == 0)) {
    watermark <- NULL
  } else {
    watermark <- trimws(watermark)
    watermark <- watermark[nchar(watermark) > 0]
  }
  if (length(wm_angles) == 0) wm_angles <- 0

  tryCatch({
    imgs <- lapply(img_paths, function(p) magick::image_read(p))
    # 拼接到一页（横向或纵向）
    combined <- magick::image_append(magick::image_join(imgs), stack = isTRUE(stack))
    # 平铺水印
    if (!is.null(watermark)) {
      info <- magick::image_info(combined)
      w <- info$width[1]; h <- info$height[1]
      wm_file <- img2pdf_build_watermark(watermark, w, h,
                                         color = wm_color, alpha = wm_alpha,
                                         size = wm_size, angles = wm_angles,
                                         cols = wm_cols, rows = wm_rows,
                                         spacing = wm_spacing)
      on.exit(unlink(wm_file), add = TRUE)
      wm_img <- magick::image_read(wm_file)
      combined <- magick::image_composite(combined, wm_img, operator = "over")
    }
    magick::image_write(combined, path = out_path, format = "pdf")
    list(success = TRUE, message = "PDF 生成成功",
         page_count = 1, out_path = out_path)
  }, error = function(e) {
    list(success = FALSE, message = paste("生成失败:", e$message))
  })
}

# 保存一条合并记录（含生成前图片地址 + 生成后 PDF 路径）
img2pdf_add_record <- function(source_paths, output_path, watermark_text, watermark_color, operator) {
  con <- db_connect()
  tryCatch({
    record_no <- img2pdf_generate_no()
    src <- paste(source_paths, collapse = "\n")
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(con, sprintf(
      "INSERT INTO img2pdf_records (record_no, source_paths, output_path, watermark_text, watermark_color, page_count, created_by, created_at)
       VALUES ('%s', '%s', '%s', '%s', '%s', %d, '%s', '%s')",
      record_no,
      gsub("'", "''", src),
      gsub("'", "''", output_path),
      gsub("'", "''", watermark_text %||% ""),
      gsub("'", "''", watermark_color %||% ""),
      length(source_paths),
      gsub("'", "''", operator %||% "系统"),
      now))
    tryCatch(log_user_operation("图片合并PDF", record_no, operator %||% "系统"),
             error = function(e) NULL)
    list(success = TRUE, message = paste("已记录", record_no), record_no = record_no)
  }, error = function(e) {
    list(success = FALSE, message = paste("记录失败:", e$message))
  }, finally = {
    db_disconnect(con)
  })
}

# 获取历史记录
img2pdf_get_records <- function(limit = 50) {
  con <- db_connect()
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT id, record_no, source_paths, output_path, watermark_text, watermark_color, page_count, created_by, created_at
       FROM img2pdf_records ORDER BY id DESC LIMIT %d", as.integer(limit)))
  }, error = function(e) data.frame(), finally = {
    db_disconnect(con)
  })
}
