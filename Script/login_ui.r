
# 全局声明Shiny UI函数以消除lint警告
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("tagList", "div", "h2", "textInput", "passwordInput", "actionButton", "p"))
}

login_ui <- function() {
  # ★ 半透明遮罩：background 不透明度 0.85，防止 Shiny 判定底层输出为不可见
  tagList(
    tags$script(HTML("
      $(document).on('keypress', '#login_username, #login_password', function(e) {
        if (e.which == 13) {
          e.preventDefault();
          $('#login_btn').click();
        }
      });
    ")),
    div(
      style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 10000;
               display: flex; justify-content: center; align-items: center;
               background: rgba(102, 126, 234, 0.85);",
      div(
        style = "background: white; padding: 40px; border-radius: 10px; box-shadow: 0 10px 25px rgba(0,0,0,0.3); width: 400px;",
        h2("LVCC ITOM 登录", style = "text-align: center; color: #667eea; margin-bottom: 30px;"),
        textInput("login_username", "用户名", placeholder = "请输入用户名"),
        passwordInput("login_password", "密码", placeholder = "请输入密码"),
        br(), br(),
        actionButton("login_btn", "登录", class = "btn-primary btn-block", style = "width: 100%;"),
        br(), br()
      )
    )
  )
}
