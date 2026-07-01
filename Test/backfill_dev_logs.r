source('global.R')
source('Script/log_user.r')
source('Script/dev_log_management.r')

# 写入开发日志的快捷函数
dl <- function(module, title, requirement, solution, result, code_snippet = NULL, files_changed = NULL) {
  r <- dev_log_add(module, title, requirement, solution, result, code_snippet, files_changed)
  cat(if(r$success) "✓" else "✗", title, "\n")
}

dl("数据结转",
  "新增月度数据结转模块（记事先行）",
  "每次月初需要将上月未完成的记事批量结账（标记已完成），并生成下月模板副本。",
  "创建 Script/monthly_carryover.r，实现 carryover_list_notes / carryover_prev_month_pending / carryover_close_notes / carryover_current_month_templates / carryover_generate_next_month 全套接口，正则匹配标题中 (YYYY年M月) 模式来提取/替换月份。",
  "已上线运行，上月(6月)17条模板成功生成7月副本。",
  NULL,
  "Script/monthly_carryover.r, server.R, Script/main_ui.r"
)

dl("数据结转",
  "修复结转表格字段名异常（sapply 返回列表导致列名展开）",
  "DT 表格表头出现“月份_标题（2026年6月）”等异常列名。",
  "carryover_list_notes 中 sapply 返回带 names 的列表 → 改为统一返回 NA_character_ 后用 is.na 过滤，确保 ym 为字符向量。server.R 表格改用英文列名+colnames 重命名。",
  "表格恢复正常 6 列(ID/记事号/标题/状态/创建时间/月份)。",
  NULL,
  "Script/monthly_carryover.r, server.R"
)

dl("数据结转",
  "结转生成的目标月份从系统日期驱动改为模板原月份+1",
  "当前日期 7月1日，系统“下月”算 8月，但结转应基于模板的 6月生成 7月副本。",
  "carryover_next_month_dates() 增加 from_ym 参数；carryover_generate_next_month() 透传 from_ym；server.R 弹窗从选中模板提取 ym 计算目标月。",
  "6月模板正确生成 7月副本。",
  NULL,
  "Script/monthly_carryover.r, server.R"
)

dl("数据结转",
  "已完成记事按标题月份归类，而非 updated_at",
  "结转后已完成记事的 updated_at 变为当前日期(7月)，导致“已完成”视图错误归类。",
  "note_server.r 中 build_done_col 函数改为优先从标题提取月份，无标题月份才回退到 updated_at。",
  "NTE20260609003 正确归到“记事-已完成-2026年06月”。",
  "subset$month <- sapply(subset$title, function(t) { m <- carryover_extract_ym(t); if(is.null(m)) NA_character_ else m })",
  "Script/note_server.r"
)

dl("数据结转",
  "结转生成新记事时同步替换 content 首行标题月份",
  "新记事标题是 7月，但内容(content)首行仍是 6月，窗口内外不一致。",
  "carryover_generate_next_month 中新增 new_content <- carryover_replace_ym(orig$content[1] %||% '', target_ym)。",
  "新生成的 7月记事标题和内容首行月份一致。",
  NULL,
  "Script/monthly_carryover.r"
)

dl("数据结转",
  "结转月份判定改为数据驱动（carryover_get_prev_ym）",
  "跨月后系统“上月”会偏移，已完成记事可能被错误归类。",
  "新增 carryover_get_prev_ym() 扫描所有带标题月份的未完成记事，取最小的 ym 作为需要处理的月份。",
  "无论系统日期如何变化，都优先处理最早未结账月份。",
  "carryover_get_prev_ym <- function() { notes <- carryover_list_notes(); pending <- notes[notes$status != 'completed',]; if(nrow(pending)==0) return(NULL); sort(unique(pending$ym))[1] }",
  "Script/monthly_carryover.r"
)

dl("记事模块",
  "统计栏增加今日新增计数 + 评论总数/今日新增",
  "统计栏只有各类总数，看不出今日活跃度；缺少评论统计。",
  "server.R 统计栏增加 today_prefix 计算每状态今日新增数；新增 note_comment_count_today() 返回评论总数和今日新增；每个 stat-box 右下角显示“+n”。",
  "统计栏显示：全部/待处理/进行中/已完成/评论，各带今日新增数。",
  "note_comment_count_today <- function() { ... list(total=..., today=...) }",
  "Script/note_server.r, Script/note_management.r"
)

dl("记事模块",
  "搜索框优化：移到分类标签同一行、加宽到20汉字、防Chrome自动填充",
  "搜索框独占一行太高；宽度不够 20 汉字；Chrome 自动填入登录名。",
  "搜索框移至分类行最左侧，width 290px；CSS 用 flex 居中图标；ui.R 新增 guardNoteSearchInput handler 每 600ms 检查并清除自动填充；加回车触发搜索。",
  "搜索框不撑大页面，图标垂直居中。Chrome 3秒内 5 次检查清除。",
  NULL,
  "Script/note_server.r, ui.R"
)

dl("日报模块",
  "日报增加本月/上月月报模式",
  "日报只支持单日查看，无法快速回顾整月工作。",
  "新增 dr_month_mode reactiveVal；本月/上月按钮；月报模式下逐日查询整月数据合并去重；标题显示“2026年06月”。",
  "点击“本月”查看全月工作日报汇总。",
  NULL,
  "Script/daily_report.r"
)

dl("全局",
  "新增开发日志模块",
  "需要自动记录每次开发的需求、方案、结果和关键代码。",
  "新建 Script/dev_log_management.r (数据层) + Script/dev_log_server.r (服务端)；global.R 添加 dev_logs 表迁移；server.R 和 main_ui.r 集成。",
  "管理→开发日志可查看历史记录，支持按模块筛选。",
  "CREATE TABLE dev_logs (id INTEGER PRIMARY KEY AUTOINCREMENT, module TEXT, title TEXT, requirement TEXT, solution TEXT, result TEXT, code_snippet TEXT, files_changed TEXT, created_by TEXT, created_at TEXT)",
  "Script/dev_log_management.r, Script/dev_log_server.r, global.R, server.R, Script/main_ui.r"
)

cat('\n开发日志回填完成\n')
