window.switchToDevLogTab = function() {
  $('[data-toggle=\"tab\"][data-value=\"管理\"]').tab('show');
  setTimeout(function() {
    $('[data-toggle=\"tab\"][data-value=\"开发日志\"]').tab('show');
  }, 300);
};
