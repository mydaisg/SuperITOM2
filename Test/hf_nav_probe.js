const PORT = 9061;
(async () => {
  const v = await (await fetch('http://127.0.0.1:9222/json/version')).json();
  const b = new WebSocket(v.webSocketDebuggerUrl); await new Promise(r => b.addEventListener('open', r));
  let id=0; const pend=new Map();
  b.addEventListener('message', e=>{const m=JSON.parse(e.data); if(m.id&&pend.has(m.id)){pend.get(m.id)(m);pend.delete(m.id);}});
  const bs=(method,params={})=>new Promise(r=>{const i=++id;pend.set(i,r);b.send(JSON.stringify({id:i,method,params}));});
  const t=await bs('Target.createTarget',{url:'http://127.0.0.1:'+PORT+'/'});
  const pages=await (await fetch('http://127.0.0.1:9222/json/list')).json();
  const pg=pages.find(p=>p.id===t.result.targetId);
  const ws=new WebSocket(pg.webSocketDebuggerUrl); await new Promise(r=>ws.addEventListener('open',r));
  let j=0; const p2=new Map();
  ws.addEventListener('message',e=>{const m=JSON.parse(e.data); if(m.id&&p2.has(m.id)){p2.get(m.id)(m);p2.delete(m.id);}});
  const s=(method,params={})=>new Promise(r=>{const i=++j;p2.set(i,r);ws.send(JSON.stringify({id:i,method,params}));});
  const ev=async expr=>{const r=await s('Runtime.evaluate',{expression:expr,returnByValue:true});return r.result?.result?.value ?? r.result?.exceptionDetails;};
  await new Promise(r=>setTimeout(r,5000));
  // 该会话是全新的，rv 全局已被前一会话置为已登录 → 直接检视已登录 DOM
  console.log(JSON.stringify(await ev(`(function(){
    var panels=Array.from(document.querySelectorAll('[data-display-if]')).map(function(d){
      return {cond:d.getAttribute('data-display-if'), display:getComputedStyle(d).display, cls:d.className, htmlLen:d.innerHTML.length};
    });
    var nb=document.getElementById('hf_navbar');
    var ov=document.getElementById('hf_login_overlay');
    return {
      flag: document.getElementById('hf_login_flag')?.value,
      panels: panels,
      navbar: nb ? {tag:nb.tagName, cls:nb.className, display:getComputedStyle(nb).display, rect:nb.getBoundingClientRect().width+'x'+nb.getBoundingClientRect().height} : 'MISSING',
      overlay: ov ? {display:getComputedStyle(ov).display, rect:ov.getBoundingClientRect().width+'x'+ov.getBoundingClientRect().height} : 'MISSING',
      tabCount: document.querySelectorAll('.nav-tabs li').length,
      tabNames: Array.from(document.querySelectorAll('.nav-tabs li a')).slice(0,5).map(a=>a.textContent.trim())
    };
  })()`), null, 2));
  await bs('Target.closeTarget',{targetId:t.result.targetId});
  process.exit(0);
})().catch(e=>{console.error('FAIL',e);process.exit(1)});
