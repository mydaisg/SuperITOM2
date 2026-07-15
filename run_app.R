shiny::devmode(TRUE)  # 保存.r文件后自动热重载
setwd("D:/GitHub/SuperITOM2")
source("global.R")  # 加载全局配置和库
# shiny::runApp(".", port=80, host="127.0.0.1", launch.browser=FALSE)
shiny::runApp(".", port=3838, host="0.0.0.0", launch.browser=FALSE)
# shiny::runApp(".", port=8000, host="0.0.0.0", launch.browser=FALSE)
                                                                                                    