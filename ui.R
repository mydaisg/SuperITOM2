# OLD 架构恢复 + 系统设置预加载
source("Script/system_settings.r")

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        var savedUserId = localStorage.getItem('itom2_user_id');
        if (savedUserId) {
          Shiny.setInputValue('auto_login_user_id', savedUserId, {priority: 'event'});
        }
      });
      Shiny.addCustomMessageHandler('saveLoginState', function(m) {
        if (m.user_id) localStorage.setItem('itom2_user_id', m.user_id);
      });
      Shiny.addCustomMessageHandler('clearLoginState', function(m) {
        localStorage.removeItem('itom2_user_id');
      });
    "))
  ),
  uiOutput("app_ui")
)
