/* HungFo 导航菜单「名称全称」悬停提示
   hf_nav_tab() 把全称写入 tab-pane 的 data-hf-nav-full 属性；本脚本把该值同步到
   对应导航 <a> 标签的 title（浏览器原生 tooltip）。
   注意：不修改 tabPanel 自带的 title 属性（它是导航标签文本）。 */
(function () {
  var cached = null;
  function collectTips() {
    if (cached) return cached;
    var byValue = {}, byName = {}, found = false;
    var panes = document.querySelectorAll('.tab-pane[data-hf-nav-full]');
    for (var i = 0; i < panes.length; i++) {
      var pane = panes[i], tip = pane.getAttribute('data-hf-nav-full');
      if (!tip) continue;
      found = true;
      var v = pane.getAttribute('data-value'), n = pane.getAttribute('data-hf-nav-name');
      if (v) byValue[v] = tip;
      if (n) byName[n] = tip;
    }
    cached = { byValue: byValue, byName: byName, found: found };
    return cached;
  }
  function applyNavTips() {
    var tips = collectTips();
    if (!tips.found) return;
    var links = document.querySelectorAll('a[data-value], a[data-toggle="tab"], a[data-bs-toggle="tab"]');
    for (var j = 0; j < links.length; j++) {
      var a = links[j];
      var key = a.getAttribute('data-value') || '';
      var txt = (a.textContent || '').replace(/\s+/g, ' ').trim();
      var full = tips.byValue[key] || tips.byName[txt] || tips.byName[key];
      if (full && !a.getAttribute('title')) a.setAttribute('title', full);
    }
  }
  function schedule() { applyNavTips(); setTimeout(applyNavTips, 200); setTimeout(applyNavTips, 800); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', schedule);
  else schedule();
  document.addEventListener('shiny:connected', schedule);
  document.addEventListener('shiny:sessioninitialized', schedule);
})();
