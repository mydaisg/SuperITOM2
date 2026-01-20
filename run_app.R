library(shiny)

options(shiny.port = 8080)
options(shiny.host = "0.0.0.0")

cat("正在启动 SuperITOM2 Shiny 应用...\n")
cat("应用将在浏览器中打开: http://localhost:8080\n")
cat("按 Ctrl+C 停止应用\n\n")

shiny::runApp(launch.browser = TRUE)
