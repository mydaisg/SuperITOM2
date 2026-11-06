// 用 CDP 连到运行中的 HungFo Shiny，真实填写并点击登录，抓取状态与 console 错误
const CDP_PORT = 9222;
const APP = 'http://127.0.0.1:8999/';
const sleep = ms => new Promise(r => setTimeout(r, ms));

class CDP {
  constructor(ws) {
    this.ws = ws; this.id = 0; this.pending = new Map(); this.listeners = [];
    ws.addEventListener('message', ev => {
      let m; try { m = JSON.parse(ev.data); } catch (e) { return; }
      if (m.id && this.pending.has(m.id)) {
        const { resolve, reject } = this.pending.get(m.id); this.pending.delete(m.id);
        m.error ? reject(new Error(JSON.stringify(m.error))) : resolve(m.result);
      } else if (m.method) {
        for (const f of this.listeners) f(m.method, m.params);
      }
    });
  }
  send(method, params = {}) {
    const id = ++this.id;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }
  on(f) { this.listeners.push(f); }
  async eval(expr) {
    const r = await this.send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails) return { error: r.exceptionDetails.text + ' ' + (r.exceptionDetails.exception?.description || '') };
    return r.result?.value;
  }
}

(async () => {
  // 创建一个新 page target
  const v = await (await fetch(`http://127.0.0.1:${CDP_PORT}/json/version`)).json();
  console.log('browser:', v.Browser);
  const browser = new CDP(new WebSocket(v.webSocketDebuggerUrl));
  await new Promise(r => browser.ws.addEventListener('open', r));
  const t = await browser.send('Target.createTarget', { url: 'about:blank' });
  const pages = await (await fetch(`http://127.0.0.1:${CDP_PORT}/json/list`)).json();
  const page = pages.find(p => p.id === t.targetId);
  const c = new CDP(new WebSocket(page.webSocketDebuggerUrl));
  await new Promise(r => c.ws.addEventListener('open', r));

  const errors = [], consoles = [], failed = [];
  c.on((method, params) => {
    if (method === 'Runtime.exceptionThrown') errors.push(params.exceptionDetails?.text + ' ' + (params.exceptionDetails?.exception?.description || ''));
    if (method === 'Runtime.consoleAPICalled' && ['error', 'warning'].includes(params.type))
      consoles.push(params.type + ': ' + params.args.map(a => a.value ?? a.description).join(' '));
    if (method === 'Log.entryAdded' && params.entry.level === 'error') failed.push(params.entry.text);
    if (method === 'Network.loadingFailed') failed.push('LOADFAIL ' + (params.errorText || ''));
  });
  await c.send('Page.enable'); await c.send('Runtime.enable');
  await c.send('Log.enable'); await c.send('Network.enable');

  await c.send('Page.navigate', { url: APP });
  await sleep(6000);

  const snap = async label => {
    const s = await c.eval(`(function(){
      var ov = document.getElementById('hf_login_overlay');
      var btn = document.getElementById('hf_login_btn');
      var un  = document.getElementById('hf_login_username');
      var pw  = document.getElementById('hf_login_password');
      var flag= document.getElementById('hf_login_flag');
      var nav = document.getElementById('hf_navbar');
      function vis(e){ if(!e) return 'MISSING'; var r=e.getBoundingClientRect(); return r.width>0&&r.height>0?'VISIBLE':'HIDDEN'; }
      return {
        ready: document.readyState,
        shiny: typeof Shiny,
        connected: (typeof Shiny!=='undefined' && Shiny.shinyapp) ? !!Shiny.shinyapp.isConnected() : null,
        overlay: vis(ov), btn: vis(btn), navbar: vis(nav),
        flagVal: flag ? flag.value : 'MISSING',
        un: un ? un.value : 'MISSING', pw: pw ? (pw.value?'***':'') : 'MISSING',
        shinyInputFlag: (typeof Shiny!=='undefined') ? Shiny.getInputValue('hf_login_flag') : null,
        shinyInputBtn: (typeof Shiny!=='undefined') ? Shiny.getInputValue('hf_login_btn') : null,
        notifications: Array.from(document.querySelectorAll('.shiny-notification')).map(function(n){return n.textContent.trim();}),
        bodyText: (document.body.innerText||'').slice(0,160)
      };
    })()`);
    console.log('\n--- ' + label + ' ---');
    console.log(JSON.stringify(s, null, 2));
    return s;
  };

  const before = await snap('BEFORE (登录后页面状态)');

  // 真实填写（模拟用户输入）+ 点击
  console.log('\n>>> filling & clicking');
  await c.eval(`(function(){
    function setVal(id, v){
      var el=document.getElementById(id); if(!el) return 'NO '+id;
      el.value=v; el.dispatchEvent(new Event('input',{bubbles:true}));
      el.dispatchEvent(new Event('change',{bubbles:true}));
      el.dispatchEvent(new KeyboardEvent('keyup',{bubbles:true,key:'a'}));
      return 'ok';
    }
    window.__r1=setVal('hf_login_username','DaiSG');
    window.__r2=setVal('hf_login_password','wrongpwd');
    return window.__r1+' / '+window.__r2;
  })()`);
  await sleep(1200);
  const mid = await snap('AFTER FILL (点击前)');
  await c.eval(`(function(){ var b=document.getElementById('hf_login_btn'); if(!b) return 'NO BTN'; b.click(); return 'clicked'; })()`);
  await sleep(4000);
  const after = await snap('AFTER CLICK');

  console.log('\n=== JS ERRORS ==='); console.log(errors.length ? errors.join('\n') : '(none)');
  console.log('=== CONSOLE error/warning ==='); console.log(consoles.length ? consoles.join('\n') : '(none)');
  console.log('=== LOG / NETWORK errors ==='); console.log(failed.length ? failed.join('\n') : '(none)');

  const verdict = {
    pageLoaded: before && before.ready === 'complete',
    shinyPresent: before && before.shiny === 'object',
    shinyConnected: before && before.connected === true,
    overlayVisibleBefore: before && before.overlay === 'VISIBLE',
    btnClickRegistered: after && mid && (Number(after.shinyInputBtn) > Number(mid.shinyInputBtn)),
    flagChangedAfterClick: after && before && String(after.shinyInputFlag) !== String(before.shinyInputFlag),
    notificationShown: after && after.notifications && after.notifications.length > 0
  };
  console.log('\n=== VERDICT ==='); console.log(JSON.stringify(verdict, null, 2));
  process.exit(0);
})().catch(e => { console.error('CDP FAIL:', e); process.exit(1); });
