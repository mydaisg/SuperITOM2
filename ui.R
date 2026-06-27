# OLD 架构恢复 + 系统设置预加载
source("Script/system_settings.r")

ui <- fluidPage(
  tags$head(
    tags$script(src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"),

    # ========== 全站 Selectize 美化样式 ==========
    tags$style(HTML("
      /* ── 导航栏选中态 ── */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #337ab7 !important;
        color: #fff !important;
        font-weight: bold;
        border: none;
        box-shadow: 0 -3px 0 #1a5276 inset;
        border-radius: 4px 4px 0 0;
      }
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        background-color: #d6e9f8 !important;
      }

      /* ── Material Design 下划线输入框 ── */
      .selectize-control {
        min-height: 36px;
      }
      .selectize-control .selectize-input,
      .form-control.selectize-input {
        border: none !important;
        border-bottom: 2px solid #cfd8dc !important;
        border-radius: 0 !important;
        padding: 6px 4px !important;
        font-size: 14px !important;
        min-height: 36px !important;
        background: transparent !important;
        box-shadow: none !important;
        transition: border-color 0.25s cubic-bezier(0.4, 0, 0.2, 1);
      }
      .selectize-control .selectize-input.focus,
      .form-control.selectize-input.focus {
        border-bottom-color: #4f8ef7 !important;
        border-bottom-width: 2px !important;
        box-shadow: 0 1px 0 0 #4f8ef7 !important;
      }
      .selectize-control .selectize-input:hover:not(.focus):not(.disabled) {
        border-bottom-color: #90a4ae !important;
      }

      /* ── 下拉面板（卡片+标签云） ── */
      .selectize-control .selectize-dropdown {
        border: none !important;
        border-radius: 12px !important;
        box-shadow: 0 8px 32px rgba(0,0,0,0.14), 0 2px 8px rgba(0,0,0,0.08) !important;
        margin-top: 6px !important;
        overflow: hidden !important;
        font-size: 14px !important;
        background: #fff !important;
      }
      .selectize-control .selectize-dropdown-content {
        max-height: 320px !important;
        overflow-y: auto !important;
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 8px !important;
        padding: 14px !important;
        align-content: flex-start;
      }
      .selectize-control .selectize-dropdown .optgroup-header {
        width: 100% !important;
        flex-basis: 100% !important;
        padding: 8px 4px !important;
        font-weight: 700 !important;
        font-size: 11px !important;
        text-transform: uppercase !important;
        color: #78909c !important;
        letter-spacing: 0.5px;
      }

      /* ── 选项：彩色药丸卡片 ── */
      .selectize-control .selectize-dropdown [data-selectable] {
        border-radius: 20px !important;
        padding: 5px 14px !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        display: inline-flex !important;
        align-items: center !important;
        gap: 6px !important;
        border: 1px solid #e8ecf0;
        background: #f8f9fb;
        color: #455a64 !important;
        white-space: nowrap !important;
        flex: none !important;
        transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1) !important;
        cursor: pointer !important;
        user-select: none !important;
      }
      .selectize-control .selectize-dropdown [data-selectable]:last-child {
        border-bottom: 1px solid #e8ecf0 !important;
      }
      .selectize-control .selectize-dropdown .active {
        transform: translateY(-2px) !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.12) !important;
        z-index: 1;
      }
      .selectize-control .selectize-dropdown .selected {
        font-weight: 600 !important;
        box-shadow: 0 0 0 2px rgba(76,175,80,0.4) !important;
      }
      .selectize-control .selectize-dropdown .selected::after {
        content: ' ✓';
        color: #4caf50;
        font-weight: bold;
        font-size: 14px;
      }

      /* ── 选中标签/徽章（Material chip） ── */
      .selectize-control .selectize-input .item {
        display: inline-flex !important;
        align-items: center !important;
        gap: 4px;
        padding: 3px 10px !important;
        margin: 2px 3px 2px 0 !important;
        border-radius: 16px !important;
        font-size: 12px !important;
        font-weight: 500 !important;
        line-height: 1.4 !important;
        background: #eceff1 !important;
        color: #37474f !important;
        border: 1px solid #cfd8dc !important;
        box-shadow: none !important;
        transition: background 0.2s;
      }
      .selectize-control .selectize-input .item:hover {
        background: #e0e4e7 !important;
      }
      .selectize-control .selectize-input .item .remove {
        font-size: 15px !important;
        color: #78909c !important;
        border-left: 1px solid #cfd8dc;
        padding-left: 5px;
        margin-left: 3px;
        line-height: 1;
        cursor: pointer;
      }
      .selectize-control .selectize-input .item .remove:hover {
        color: #c62828 !important;
        border-color: #ef9a9a;
      }

      /* ── 搜索输入框样式 ── */
      .selectize-control .selectize-input input[type='text'] {
        font-size: 14px !important;
        color: #333 !important;
        min-height: 24px;
      }
      .selectize-control .selectize-input input[type='text']::placeholder {
        color: #90a4ae !important;
        font-style: italic;
      }

      /* ── 单选箭头 ── */
      .selectize-control.single .selectize-input::after {
        content: '▾';
        font-size: 12px;
        color: #90a4ae;
        margin-left: 4px;
        right: 4px !important;
      }
      .selectize-control.single .selectize-input.input-active::after {
        content: '▴';
      }

      /* ── 未选择/满载状态 ── */
      .selectize-control .selectize-input.has-items {
        padding: 4px 10px !important;
      }
      .selectize-control .selectize-input.full {
        background: transparent !important;
      }

      /* ── 禁用状态 ── */
      .selectize-control .selectize-input.disabled {
        opacity: 0.45;
        cursor: not-allowed;
        border-bottom-style: dashed !important;
      }

      /* ── 弹窗拖拽光标 ── */
      .modal .modal-header { cursor: grab !important; user-select: none !important; }
      .modal .modal-header.modal-dragging { cursor: grabbing !important; }

      /* ── 通知居中显示 ── */
      #shiny-notification-panel {
        top: 50% !important;
        left: 50% !important;
        transform: translate(-50%, -50%) !important;
        right: auto !important;
        bottom: auto !important;
        width: auto !important;
        max-width: 520px;
      }
      .shiny-notification {
        font-size: 15px !important;
        padding: 16px 24px !important;
        border-radius: 10px !important;
        box-shadow: 0 8px 32px rgba(0,0,0,0.18) !important;
        animation: shiny-notif-in 0.35s cubic-bezier(0.21, 1.02, 0.73, 1);
      }
      @keyframes shiny-notif-in {
        from { opacity: 0; transform: translateY(20px); }
        to   { opacity: 1; transform: translateY(0); }
      }
    ")),

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

      // ========== 全站弹窗拖拽（拖标题栏即可移动） ==========
      (function initGlobalModalDrag(){
        var $dlg = null, dragging = false, offX = 0, offY = 0;
        $(function(){
          $(document).on('mousedown','.modal-dialog .modal-header',function(e){
            $dlg = $(this).closest('.modal-dialog');
            if (!$dlg.length) return;
            dragging = true;
            var rect = $dlg[0].getBoundingClientRect();
            offX = e.clientX - rect.left;
            offY = e.clientY - rect.top;
            $dlg.css({position:'fixed',left:rect.left+'px',top:rect.top+'px',margin:'0',transform:'none'});
            $(this).addClass('modal-dragging');
            e.preventDefault();
          });
          $(document).on('mousemove',function(e){
            if (!dragging || !$dlg) return;
            $dlg.css({left:(e.clientX-offX)+'px', top:(e.clientY-offY)+'px'});
          });
          $(document).on('mouseup',function(){
            if ($dlg) $dlg.find('.modal-header').removeClass('modal-dragging');
            dragging = false; $dlg = null;
          });
        });
      })();

      // ========== 下拉选项彩色药丸 + 色圆点 ==========
      var selPalette = [
        { dot: '#4CAF50', bg: '#e8f5e9', border: '#a5d6a7' },
        { dot: '#FF9800', bg: '#fff3e0', border: '#ffcc80' },
        { dot: '#2196F3', bg: '#e3f2fd', border: '#90caf9' },
        { dot: '#9C27B0', bg: '#f3e5f5', border: '#ce93d8' },
        { dot: '#F44336', bg: '#ffebee', border: '#ef9a9a' },
        { dot: '#00BCD4', bg: '#e0f7fa', border: '#80deea' },
        { dot: '#FF5722', bg: '#fbe9e7', border: '#ffab91' },
        { dot: '#607D8B', bg: '#eceff1', border: '#b0bec5' }
      ];

      function selColor(text) {
        var h = 0, i;
        for (i = 0; i < text.length; i++) {
          h = ((h << 5) - h) + text.charCodeAt(i);
          h |= 0;
        }
        return selPalette[Math.abs(h) % selPalette.length];
      }

      function selDecorate(opt) {
        // 跳过已被自定义 render 装饰过的选项（有内联背景色）
        if (opt.querySelector('.sel-dot')) return;
        if (opt.style.background && opt.style.background !== '' && opt.style.background !== '#f8f9fb') return;
        var c = selColor(opt.textContent.replace(/[✓✔\\s]+$/g, '').trim());
        var dot = document.createElement('span');
        dot.className = 'sel-dot';
        dot.style.cssText = 'display:inline-block;width:9px;height:9px;border-radius:50%;flex-shrink:0;background:' + c.dot + ';';
        opt.style.display = 'inline-flex';
        opt.style.alignItems = 'center';
        opt.style.gap = '6px';
        opt.style.background = c.bg;
        opt.style.borderColor = c.border;
        opt.insertBefore(dot, opt.firstChild);
      }

      function selScanDropdown(dd) {
        dd.querySelectorAll('[data-selectable]').forEach(selDecorate);
      }

      $(document).ready(function() {
        new MutationObserver(function(muts) {
          muts.forEach(function(m) {
            m.addedNodes.forEach(function(n) {
              if (n.nodeType === 1 && n.classList && n.classList.contains('selectize-dropdown')) {
                selScanDropdown(n);
                // 继续监听子节点变化（搜索过滤/动态更新选项）
                new MutationObserver(function() {
                  selScanDropdown(n);
                }).observe(n, { childList: true, subtree: true });
              }
            });
          });
        }).observe(document.body, { childList: true, subtree: true });
      });
    "))
  ),
  uiOutput("app_ui")
)
