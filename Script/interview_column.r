# 研发中心采访专栏 - 数据层
# 功能：从 UNC 网络路径复制采访视频到本地 www/rd_interview/videos/，并自动更新采访清单 interview_column.js
# 独立流媒体网站：www/rd_interview/（含 server.js + RD_Interview.html，脱离 Shiny 主界面）

##################
# 目录辅助
##################
# 采访视频本地目录（绝对路径）
interview_video_dir <- function() {
  file.path(getwd(), "www", "rd_interview", "videos")
}

# 采访清单配置文件路径
interview_js_path <- function() {
  file.path(getwd(), "www", "rd_interview", "interview_column.js")
}

# 确保视频目录存在
interview_ensure_dir <- function() {
  d <- interview_video_dir()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

##################
# 视频复制
##################
# 从 UNC 网络路径复制视频到本地 www/videos/
# 参数：
#   src_path : UNC 源路径（如 \\10.10.50.50\研发中心\...\xxx.mp4）
#   dest_name : 目标文件名（建议 ASCII，如 interview_ai_2026_ep1.mp4）
# 返回：list(success, message, dest_path)
interview_copy_video <- function(src_path, dest_name = NULL) {
  interview_ensure_dir()
  if (is.null(src_path) || nchar(trimws(src_path)) == 0) {
    return(list(success = FALSE, message = "源路径不能为空"))
  }
  if (!file.exists(src_path)) {
    return(list(success = FALSE, message = paste("源文件不存在：", src_path)))
  }
  if (is.null(dest_name) || nchar(trimws(dest_name)) == 0) {
    dest_name <- basename(src_path)
  }
  dest_path <- file.path(interview_video_dir(), dest_name)
  ok <- tryCatch({
    file.copy(src_path, dest_path, overwrite = TRUE)
  }, error = function(e) {
    return(list(success = FALSE, message = paste("复制失败：", e$message)))
  })
  if (is.list(ok)) return(ok)
  if (!ok) {
    return(list(success = FALSE, message = "复制失败（file.copy 返回 FALSE）"))
  }
  mb <- round(file.info(dest_path)$size / 1024 / 1024, 2)
  list(success = TRUE,
       message = sprintf("已复制视频（%s MB）：%s", mb, dest_path),
       dest_path = dest_path)
}

##################
# 清单生成
##################
# 将采访视频元数据写入 interview_column.js
# 参数 items：data.frame 或 list，每项含 id/title/video/cover/dept/issue/date/desc/duration
interview_write_js <- function(items) {
  js_path <- interview_js_path()
  entries <- vapply(seq_len(nrow(items)), function(i) {
    it <- items[i, ]
    j <- function(k, default = "") {
      v <- it[[k]]
      if (is.null(v) || is.na(v)) default else as.character(v)
    }
    # JSON 转义
    esc <- function(s) {
      s <- gsub("\\\\", "\\\\\\\\", s)
      s <- gsub("\"", "\\\\\"", s)
      s <- gsub("\n", "\\\\n", s)
      s
    }
    sprintf(
      paste0("  {\n",
             "    id: \"%s\",\n",
             "    title: \"%s\",\n",
             "    video: \"%s\",\n",
             "    cover: \"%s\",\n",
             "    dept: \"%s\",\n",
             "    issue: \"%s\",\n",
             "    date: \"%s\",\n",
             "    desc: \"%s\",\n",
             "    duration: \"%s\"\n",
             "  }"),
      esc(j("id")), esc(j("title")), esc(j("video")), esc(j("cover")),
      esc(j("dept")), esc(j("issue")), esc(j("date")), esc(j("desc")),
      esc(j("duration"))
    )
  }, character(1))
  body <- paste(entries, collapse = ",\n")

  header <- paste0(
    "// 研发中心采访专栏 - 视频清单配置\n",
    "// 新增一期采访：在 INTERVIEW_LIST 数组末尾追加一个对象即可\n",
    "// 字段说明：id 唯一标识 / title 标题 / video 视频相对路径 / cover 封面（可选）\n",
    "//          dept 部门 / issue 期号 / date 发布日期 / desc 简介 / duration 时长（可选）\n\n",
    "var INTERVIEW_LIST = [\n"
  )
  footer <- "\n];\n"

  writeLines(paste0(header, body, footer), js_path, useBytes = TRUE)
  list(success = TRUE, message = paste("已更新清单：", js_path), js_path = js_path)
}

# 快捷示例：添加一期采访（复制视频 + 追加清单）
# 用法（R 控制台）：
#   source("Script/interview_column.r")
#   interview_add_episode(
#     src   = "\\\\10.10.50.50\\研发中心\\...\\xxx.mp4",
#     id    = "ai-2026-02",
#     title = "研发中心采访 · 某某部门（2026年第2期专访）",
#     dept  = "某某部门",
#     issue = "2026年 第2期",
#     date  = "2026-09-01",
#     desc  = "简介...",
#     dest  = "interview_xxx_2026_ep2.mp4"
#   )
interview_add_episode <- function(src, id, title, dept, issue, date, desc = "", dest = NULL, duration = "") {
  cp <- interview_copy_video(src, dest)
  if (!cp$success) return(cp)

  items <- data.frame(
    id = id, title = title,
    video = paste0("videos/", basename(cp$dest_path)),
    cover = "", dept = dept, issue = issue, date = date,
    desc = desc, duration = duration,
    stringsAsFactors = FALSE
  )
  interview_write_js(items)
}
