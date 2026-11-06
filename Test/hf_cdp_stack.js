(async () => {
  const pages = await (await fetch('http://127.0.0.1:9222/json/list')).json();
  const p = pages.find(x => x.url && x.url.includes('8999'));
  const ws = new WebSocket(p.webSocketDebuggerUrl);
  await new Promise(r => ws.addEventListener('open', r));
  let id = 0; const pend = new Map();
  ws.addEventListener('message', ev => {
    const m = JSON.parse(ev.data);
    if (m.id && pend.has(m.id)) { pend.get(m.id)(m); pend.delete(m.id); }
    if (m.method === 'Debugger.paused') {
      console.log('\n=== PAUSED CALL STACK ===');
      (m.params.callFrames || []).slice(0, 25).forEach((f, i) => {
        console.log(`#${i} ${f.functionName || '(anonymous)'} @ ${f.url || '(inline)'}:${f.location.lineNumber + 1}:${f.location.columnNumber}`);
      });
      console.log('reason:', m.params.reason);
      process.exit(0);
    }
    if (m.method === 'Debugger.scriptParsed' && m.params.url && /hungfo|shiny/i.test(m.params.url))
      console.log('script:', m.params.url);
  });
  const send = (method, params = {}) => new Promise(res => {
    const i = ++id; pend.set(i, res); ws.send(JSON.stringify({ id: i, method, params }));
  });
  await Promise.race([send('Debugger.enable'), new Promise(r => setTimeout(r, 4000))]);
  console.log('Debugger.enable sent');
  await Promise.race([send('Debugger.pause'), new Promise(r => setTimeout(r, 4000))]);
  console.log('Debugger.pause sent, waiting 8s ...');
  await new Promise(r => setTimeout(r, 8000));
  console.log('NO PAUSE EVENT (main thread may be uninterruptible)');
  process.exit(0);
})().catch(e => { console.error('FAIL', e); process.exit(1); });
