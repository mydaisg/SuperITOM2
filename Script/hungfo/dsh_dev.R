##################
# HungFo — DSH 开发环境启动脚本
# 用途：在 DeepSeek Harness 里开发 HungFo 时使用，与本地正式实例完全隔离。
#   端口：9999          （正式实例 8999，本脚本绝不占用）
#   数据库：DB/HF_HungFo_DSH.DB（正式库 HF_HungFo.DB，本脚本绝不写）
#   日志：Log/DSH/      （与正式运行日志分开）
#
# 用法（在 Script/hungfo 目录下，或任意目录——脚本会自己定位）：
#   Rscript dsh_dev.R
# 或在 DSH 里后台启动（Windows）：
#   Start-Process Rscript -ArgumentList 'dsh_dev.R' -WorkingDirectory '<hungfo>' `
#     -RedirectStandardOutput 'Log\DSH\out.log' -RedirectStandardError 'Log\DSH\err.log' -WindowStyle Hidden
#
# 环境变量可覆盖（不设即取下列默认值）：
#   HUNGFO_PORT / HUNGFO_DB / HUNGFO_HOME
##################

# ── 1. 定位本文件所在目录（hungfo 根），保证相对路径一致 ──
.hf_script_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- args[startsWith(args, "--file=")]
  if (length(f) > 0) {
    dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
})()

# 只有当本目录确实是 hungfo 根（含 hf_core.r）时才切换工作目录
if (file.exists(file.path(.hf_script_dir, "hf_core.r"))) {
  setwd(.hf_script_dir)
}

# ── 2. 开发环境默认值（仅在未显式设置时生效，便于人工覆盖）──
if (!nzchar(Sys.getenv("HUNGFO_PORT"))) Sys.setenv(HUNGFO_PORT = "9999")
if (!nzchar(Sys.getenv("HUNGFO_DB"))) {
  Sys.setenv(HUNGFO_DB = file.path(getwd(), "DB", "HF_HungFo_DSH.DB"))
}

# ── 3. 安全闸：拒绝占用正式端口、拒绝写正式库 ──
.hf_port <- as.integer(Sys.getenv("HUNGFO_PORT"))
.hf_db   <- Sys.getenv("HUNGFO_DB")
.hf_prod_db <- normalizePath(file.path(getwd(), "DB", "HF_HungFo.DB"), winslash = "/", mustWork = FALSE)

if (identical(.hf_port, 8999L)) {
  stop("dsh_dev.R 不允许使用正式端口 8999，请用 9999 或其它端口（HUNGFO_PORT）。")
}
if (normalizePath(.hf_db, winslash = "/", mustWork = FALSE) == .hf_prod_db) {
  stop("dsh_dev.R 不允许写正式库 HF_HungFo.DB，请使用 HF_HungFo_DSH.DB（HUNGFO_DB）。")
}

# ── 4. 开发库首次使用时从正式库复制（只复制一次，不覆盖已有开发数据）──
if (!file.exists(.hf_db)) {
  if (file.exists(.hf_prod_db)) {
    file.copy(.hf_prod_db, .hf_db, overwrite = FALSE)
    cat(sprintf("[dsh_dev] 已从正式库初始化开发库：%s\n", basename(.hf_db)))
  } else {
    cat("[dsh_dev] 正式库不存在，将由 hf_db_init() 建新库\n")
  }
}

# ── 5. 启动信息 ──
options(shiny.autoreload = FALSE)
options(shiny.port = .hf_port)
if (!nzchar(Sys.getenv("HUNGFO_HOME"))) Sys.setenv(HUNGFO_HOME = getwd())

cat(sprintf("[dsh_dev] HUNGFO_HOME = %s\n", Sys.getenv("HUNGFO_HOME")))
cat(sprintf("[dsh_dev] HUNGFO_DB   = %s\n", .hf_db))
cat(sprintf("[dsh_dev] 端口         = %d\n", .hf_port))
cat(sprintf("[dsh_dev] 访问地址     = http://127.0.0.1:%d\n", .hf_port))

# ── 6. 加载与正式入口一致的四层 ──
source("hf_core.r")                     # 核心层（注册表 + 根目录 + 数据层）
source("hf_config.r")                   # 独立配置层（依赖库 + DB 访问层）
for (f in sort(list.files(".", pattern = "^hf_mod_.*\\.r$"))) source(f)  # 12 个环节
source("hf_ui.r")                       # 聚合 UI + Server

rv <- reactiveValues(logged_in = FALSE, current_user = NULL, app_view = "hungfo")

ui <- hungfo_standalone_ui(show_back = FALSE)

server <- function(input, output, session) {
  hungfo_server(input, output, session, rv)
}

shinyApp(ui, server)
