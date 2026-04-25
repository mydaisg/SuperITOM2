ui <- fluidPage(
  # 页面刷新后自动恢复登录状态的JS（放在ui.R确保始终加载）
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        var savedUserId = localStorage.getItem('itom2_user_id');
        if (savedUserId) {
          Shiny.setInputValue('auto_login_user_id', savedUserId, {priority: 'event'});
        }
      });
      Shiny.addCustomMessageHandler('saveLoginState', function(message) {
        if (message.user_id) {
          localStorage.setItem('itom2_user_id', message.user_id);
        }
      });
      Shiny.addCustomMessageHandler('clearLoginState', function(message) {
        localStorage.removeItem('itom2_user_id');
      });
    "))
  ),
  uiOutput("app_ui")
)
