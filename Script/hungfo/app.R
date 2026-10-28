##################
# HungFo 独立启动入口（R + Shiny + SQLite）
# 把整个 hungfo 目录拷出后，进入本目录运行：
#   1) Rscript app.R                        # 直接启动
#   2) shiny::runApp("<hungfo 目录>")        # 或 RStudio 打开本目录点 Run App
# 依赖 R 包：shiny / shinythemes / DT / RSQLite / DBI / jsonlite
# 独立数据库：默认 DB/hungfo.db（可用环境变量 HUNGFO_DB 覆盖；根目录可用 HUNGFO_HOME 覆盖）
##################

# 定位本文件目录并切换（保证 source 相对路径 / 资源路径一致）
.hf_script_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- args[startsWith(args, "--file=")]
  if (length(f) > 0) dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/", mustWork = FALSE))
  else getwd()
})()
if (file.exists(file.path(.hf_script_dir, "hf_core.r"))) setwd(.hf_script_dir)

options(shiny.autoreload = FALSE)

# 1) 核心层（注册表 + 根目录解析 + 数据层）
source("hf_core.r")
# 2) 独立配置层（依赖库 + DB 访问层）
source("hf_config.r")
# 3) 12 个环节模块（按文件名排序加载）
for (f in sort(list.files(".", pattern = "^hf_mod_.*\\.r$"))) source(f)
# 4) 聚合 UI + Server
source("hf_ui.r")

# 独立运行无需登录：直接视为已登录
rv <- reactiveValues(logged_in = TRUE, current_user = NULL, app_view = "hungfo")

ui <- hungfo_standalone_ui(show_back = FALSE)

server <- function(input, output, session) {
  hungfo_server(input, output, session, rv)
}

shinyApp(ui, server)
