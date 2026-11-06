const sleep = ms => new Promise(r => setTimeout(r, ms));
function conn(wsUrl) {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(wsUrl);
    ws.addEventListener('open', () => resolve(ws));
    ws.addEventListener('error', e => reject(new Error('ws error')));
  });
}
(async () => {
  const pages = await (await fetch('http://127.0.0.1:9222/json/list')).json();
  const p = pages.find(x => x.url && x.url.includes('8999'));
  console.log('target:', p && p.url, p && p.id);
  const ws = await conn(p.webSocketDebuggerUrl);
  let id = 0; const pend = new Map();
  ws.addEventListener('message', ev => {
    const m = JSON.parse(ev.data);
    if (m.id && pend.has(m.id)) { pend.get(m.id)(m); pend.delete(m.id); }
  });
  const send = (method, params) => new Promise(res => {
    const i = ++id; pend.set(i, res); ws.send(JSON.stringify({ id: i, method, params }));
  });
  const withTimeout = (pr, ms, label) => Promise.race([
    pr, new Promise(r => setTimeout(() => r({ TIMEOUT: label }), ms))
  ]);
  console.log('Runtime.enable:', JSON.stringify(await withTimeout(send('Runtime.enable', {}), 4000, 'enable')));
  const r = await withTimeout(send('Runtime.evaluate', { expression: '1+1', returnByValue: true }), 8000, 'eval 1+1');
  console.log('eval 1+1 =>', JSON.stringify(r));
  const r2 = await withTimeout(send('Runtime.evaluate', { expression: 'document.readyState + " | " + (typeof Shiny) + " | " + document.title', returnByValue: true }), 8000, 'eval readyState');
  console.log('readyState =>', JSON.stringify(r2));
  process.exit(0);
})().catch(e => { console.error('FAIL', e); process.exit(1); });
