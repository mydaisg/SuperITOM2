// 极简 CORS 代理 - 给大屏页用
// 启动: node bigscreen-proxy.js
// 然后大屏页 fetch http://127.0.0.1:3839/api 即可

const http = require('http');
const https = require('https');

const PORT = 3839;
const TARGET = 'https://lvcchong.com/factoryBi/charge/0/realTimeData';

http.createServer((req, res) => {
  if (req.url === '/api') {
    https.get(TARGET, { headers: { 'Accept': 'application/json' } }, upstream => {
      let body = '';
      upstream.on('data', chunk => body += chunk);
      upstream.on('end', () => {
        res.writeHead(200, {
          'Content-Type': 'application/json',
          'Access-Control-Allow-Origin': '*'
        });
        res.end(body);
      });
    }).on('error', () => {
      res.writeHead(502, { 'Access-Control-Allow-Origin': '*' });
      res.end('{"error":"upstream error"}');
    });
  } else {
    res.writeHead(404);
    res.end();
  }
}).listen(PORT, () => console.log('CORS proxy on port ' + PORT));
