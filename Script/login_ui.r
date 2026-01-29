
# 全局声明Shiny UI函数以消除lint警告
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("tagList", "div", "h2", "textInput", "passwordInput", "actionButton", "p"))
}

login_ui <- function() {
  tagList(
    div(
      class = "login-container",
      style = "display: flex; justify-content: center; align-items: center; height: 100vh; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);",
      div(
        class = "login-box",
        style = "background: white; padding: 40px; border-radius: 10px; box-shadow: 0 10px 25px rgba(0,0,0,0.2); width: 400px;",
        h2("SuperITOM2 登录", style = "text-align: center; color: #667eea; margin-bottom: 30px;"),
        textInput("login_username", "用户名", placeholder = "请输入用户名"),
        passwordInput("login_password", "密码", placeholder = "请输入密码"),
        br(), br(),
        actionButton("login_btn", "登录", class = "btn-primary btn-block", style = "width: 100%;"),
        br(), br(),
        p("默认管理员账号: admin / admin123", style = "text-align: center; color: #666; font-size: 12px;")
      )
    )
  )
}
