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
      // 通用按钮启用/禁用（必须放在静态 head 中，renderUI 内的 shiny:connected 会错过时机）
      Shiny.addCustomMessageHandler('toggleBtn', function(msg) {
        var b = document.getElementById(msg.id);
        if (b) {
          b.disabled = msg.disabled;
          b.style.opacity = msg.disabled ? '0.45' : '';
          b.style.cursor = msg.disabled ? 'not-allowed' : '';
        }
      });
      Shiny.addCustomMessageHandler('runjs', function(msg) {
        if (window[msg]) window[msg]();
      });
      // admin 菜单 JS 控制（兼容 server.R 中的 toggleAdminMenu 调用）
      Shiny.addCustomMessageHandler('toggleAdminMenu', function(message) {
        if (message.show) {
          document.body.classList.add('admin-user');
        } else {
          document.body.classList.remove('admin-user');
        }
      });
    "))
  ),
  uiOutput("app_ui")
)
