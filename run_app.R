# ★ 2026-10-13 Fix(DL20261013004)：Shiny 1.14 在交互式 R（CodeBuddy 终端）中 autoreload 默认开启
# （autoreload 默认值 = interactive()），未安装 watcher 包时回退 legacy 文件监视，
# 扫描 www/(3104文件) 致主进程单核 100% 死循环、HTTP 全部超时。
# 必须显式关闭 autoreload（此行在交互式/非交互下均生效）。
options(shiny.autoreload = FALSE)
setwd("D:/GitHub/SuperITOM2")
source("global.R")  # 加载全局配置和库
# shiny::runApp(".", port=80, host="127.0.0.1", launch.browser=FALSE)
shiny::runApp(".", port=8899, host="0.0.0.0", launch.browser=FALSE)
# shiny::runApp(".", port=8000, host="0.0.0.0", launch.browser=FALSE)
                                                                                                    