// 真实浏览器验证登录后 UI 切换（HungFo 登录无响应修复回归）
const PORT = Number(process.env.HF_PORT || 9060);
const APP = `http://127.0.0.1:${PORT}/`;
const sleep = ms => new Promise(r => setTimeout(r, ms));

class CDP {
  constructor(ws) {
    this.ws = ws; this.id = 0; this.pending = new Map(); this.events = [];
    ws.addEventListener('message', ev => {
      let m; try { m = JSON.parse(ev.data); } catch (e) { return; }
      if (m.id && this.pending.has(m.id)) {
        const { resolve, reject } = this.pending.get(m.id); this.pending.delete(m.id);
        m.error ? reject(new Error(JSON.stringify(m.error))) : resolve(m.result);
      } else if (m.method) this.events.push(m);
    });
  }
  send(method, params = {}) {
    const id = ++this.id;
    return Promise.race([
      new Promise((resolve, reject) => { this.pending.set(id, { resolve, reject }); this.ws.send(JSON.stringify({ id, method, params })); }),
      new Promise((_, reject) => setTimeout(() => reject(new Error('TIMEOUT ' + method)), 8000))
    ]);
  }
  async eval(expr) {
    const r = await this.send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails) return { error: r.exceptionDetails.exception?.description || r.exceptionDetails.text };
    return r.result?.value;
  }
}

(async () => {
  const v = await (await fetch('http://127.0.0.1:9222/json/version')).json();
  const b = new CDP(new WebSocket(v.webSocketDebuggerUrl));
  await new Promise(r => b.ws.addEventListener('open', r));
  const t = await b.send('Target.createTarget', { url: 'about:blank' });
  const pages = await (await fetch('http://127.0.0.1:9222/json/list')).json();
  const pg = pages.find(p => p.id === t.targetId);
  const c = new CDP(new WebSocket(pg.webSocketDebuggerUrl));
  await new Promise(r => c.ws.addEventListener('open', r));

  const jsErrors = [];
  c.events.push = function(m) { if (m.method === 'Runtime.exceptionThrown') jsErrors.push(m.params.exceptionDetails?.exception?.description || m.params.exceptionDetails?.text); return Array.prototype.push.call(this, m); };
  await c.send('Runtime.enable'); await c.send('Page.enable'); await c.send('Log.enable');
  await c.send('Page.navigate', { url: APP });
  await sleep(5000);

  const snap = async () => c.eval(`(function(){
    var ov=document.getElementById('hf_login_overlay');
    var nav=document.getElementById('hf_navbar');
    var f=document.getElementById('hf_login_flag');
    function vis(e){
      if(!e) return 'MISSING';
      var n=e, hidden=false;
      while(n && n!==document.body){ var cs=getComputedStyle(n); if(cs.display==='none'||cs.visibility==='hidden'){hidden=true;break;} n=n.parentElement; }
      if(hidden) return 'HIDDEN';
      var r=e.getBoundingClientRect(); return r.width>0&&r.height>0?'VISIBLE':'HIDDEN';
    }
    return {
      ready: document.readyState,
      shiny: typeof Shiny,
      connected: (typeof Shiny!=='undefined'&&Shiny.shinyapp)?!!Shiny.shinyapp.isConnected():null,
      overlay: vis(ov), navbar: vis(nav),
      shownPanel: (function(){
        var all=document.querySelectorAll('[data-display-if]');
        for(var k=0;k<all.length;k++){
          var cond=all[k].getAttribute('data-display-if')||'';
          if(cond.indexOf('==')>=0) return getComputedStyle(all[k]).display;
        }
        return 'MISSING';
      })(),
      tabCount: document.querySelectorAll('.nav-tabs li').length,
      flag: f ? f.value : 'MISSING',
      btn: (typeof Shiny!=='undefined' && Shiny.shinyapp) ? Shiny.shinyapp.$inputValues?.hf_login_btn : null,
      notifications: Array.from(document.querySelectorAll('.shiny-notification')).map(n=>n.textContent.trim())
    };
  })()`);

  const before = await snap();
  console.log('BEFORE:', JSON.stringify(before));

  console.log('>>> fill DaiSG / falwpa');
  await c.eval(`(function(){
    function setv(id,val){var el=document.getElementById(id);if(!el)return;el.value=val;el.dispatchEvent(new Event('input',{bubbles:true}));el.dispatchEvent(new Event('change',{bubbles:true}));}
    setv('hf_login_username','DaiSG'); setv('hf_login_password','falwpa'); return 'ok';
  })()`);
  await sleep(1000);
  console.log('>>> click 登录');
  await c.eval(`(function(){var b=document.getElementById('hf_login_btn');if(!b)return 'NOBTN';b.click();return 'clicked';})()`);
  await sleep(3500);
  const after = await snap();
  console.log('AFTER :', JSON.stringify(after));

  const checks = {
    pageReady: before?.ready === 'complete',
    shinyConnected: before?.connected === true,
    overlayVisibleBefore: before?.overlay === 'VISIBLE',
    clickRegistered: (after?.notifications||[]).length > (before?.notifications||[]).length,
    flagBecame1: String(after?.flag) === '1',
    overlayHiddenAfter: after?.overlay === 'HIDDEN',
    navbarVisibleAfter: after?.shownPanel === 'block' && Number(after?.tabCount) > 0,
    welcomeNotification: (after?.notifications || []).some(x => x.includes('欢迎'))
  };
  console.log('\n=== CHECKS ===');
  for (const [k, val] of Object.entries(checks)) console.log(`  ${val ? 'PASS' : 'FAIL'}  ${k}`);
  console.log('JS_ERRORS:', jsErrors.length ? jsErrors.join(' | ').slice(0, 400) : '(none)');
  const fail = Object.values(checks).filter(x => !x).length;
  console.log(`\n===== LOGIN CDP: PASS ${Object.keys(checks).length - fail} / FAIL ${fail} =====`);
  await b.send('Target.closeTarget', { targetId: t.targetId }).catch(() => {});
  process.exit(fail ? 1 : 0);
})().catch(e => { console.error('CDP FAIL:', e.message); process.exit(1); });
