##################
# HungFo 独立启动入口（R + Shiny + SQLite）
# 把整个 hungfo 目录拷出后，进入本目录运行：
#   1) Rscript app.R                        # 直接启动
#   2) shiny::runApp("<hungfo 目录>")        # 或 RStudio 打开本目录点 Run App
# 依赖 R 包：shiny / shinythemes / DT / RSQLite / DBI / jsonlite
# 独立数据库：默认 DB/HF_HungFo.DB（可用环境变量 HUNGFO_DB 覆盖；根目录可用 HUNGFO_HOME 覆盖）
# 专用端口：默认 8999（可用环境变量 HUNGFO_PORT 覆盖）
##################

# ── 兼容多语言环境：确保 R 使用支持 UTF-8 多字节的 locale ──
# 某些部署/沙箱环境默认 locale 损坏（如 C.UTF-8 设置失败），会导致中文标识符/字符串
# 在 source() 时解析失败（unexpected invalid token）。此处按优先级尝试可用 locale，
# 任一成功即生效，全部失败则静默保留环境默认，不影响已有正常环境。
tryCatch({
  for (.hf_loc in c("Chinese (Simplified)_China.65001", "zh_CN.UTF-8",
                   "zh_CN.utf8", "en_US.UTF-8", "C.UTF-8", "UTF-8")) {
    if (!is.na(Sys.setlocale("LC_ALL", .hf_loc))) break
  }
}, error = function(e) NULL)

# 定位本文件目录并切换（保证 source 相对路径 / 资源路径一致）
.hf_script_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- args[startsWith(args, "--file=")]
  if (length(f) > 0) dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/", mustWork = FALSE))
  else getwd()
})()
if (file.exists(file.path(.hf_script_dir, "hf_core.r"))) setwd(.hf_script_dir)

options(shiny.autoreload = FALSE)
# 专用 WEB 端口 8999（独立服务，可用环境变量 HUNGFO_PORT 覆盖）
options(shiny.port = as.integer(Sys.getenv("HUNGFO_PORT", unset = "8999")))

# 1) 核心层（注册表 + 根目录解析 + 数据层）
source("hf_core.r")
# 1.1) AI 上下文层（规则修订 / 记忆 / 模型路由 / 按分类装配，只提供函数，不注册模块）
source("hf_ai_context.r")
# 2) 独立配置层（依赖库 + DB 访问层）
source("hf_config.r")
# 3) 12 个环节模块（按文件名排序加载）
for (f in sort(list.files(".", pattern = "^hf_mod_.*\\.r$"))) source(f)
# 4) 聚合 UI + Server
source("hf_ui.r")

# 独立运行：默认未登录，由登录遮罩 + hungfo_server 的登录/登出逻辑完成鉴权
rv <- reactiveValues(logged_in = FALSE, current_user = NULL, app_view = "hungfo")

# 静态 UI 对象；show_back=FALSE 去掉「回 ITOM」页签（独立运行无 ITOM 可回）
ui <- hungfo_standalone_ui(show_back = FALSE)

server <- function(input, output, session) {
  hungfo_server(input, output, session, rv)
}

shinyApp(ui, server)
