/*
 * 研发中心采访专栏 - 独立流媒体服务（单文件，零依赖，可移植）
 *
 * 功能：
 *   1. 静态文件托管（HTML/JS/视频），支持 HTTP Range 流媒体（边播边缓冲）
 *   2. 访问统计：记录访客 IP、开始/结束时间、观看时长，写入 access_log.csv
 *   3. 完全独立于 SuperITOM2，拷贝本目录到任意装了 Node.js 的主机即可运行
 *
 * 用法：
 *   node server.js            # 默认端口 3838
 *   node server.js 8080       # 指定端口
 *
 * 访问：http://<主机IP>:<端口>/RD_Interview.html
 */

'use strict';

const http = require('http');
const fs = require('fs');
const path = require('path');
const { URL } = require('url');

const ROOT = __dirname;                       // 网站根目录（本文件所在目录）
const PORT = parseInt(process.argv[2], 10) || 3838;
const CSV_PATH = path.join(ROOT, 'access_log.csv');

// ---------- MIME ----------
const MIME = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'application/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.mp4': 'video/mp4',
  '.webm': 'video/webm',
  '.ogg': 'video/ogg',
  '.m4v': 'video/mp4',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.gif': 'image/svg+xml',
  '.svg': 'image/svg+xml',
  '.ico': 'image/x-icon',
  '.csv': 'text/csv; charset=utf-8',
  '.txt': 'text/plain; charset=utf-8',
};

// ---------- 工具 ----------
function getClientIp(req) {
  // 支持反向代理（X-Forwarded-For）
  const xff = req.headers['x-forwarded-for'];
  if (xff) return xff.split(',')[0].trim();
  return req.socket.remoteAddress || 'unknown';
}

function csvEscape(v) {
  const s = String(v == null ? '' : v);
  if (/[",\r\n]/.test(s)) {
    return '"' + s.replace(/"/g, '""') + '"';
  }
  return s;
}

function nowStr() {
  const d = new Date();
  const p = (n) => String(n).padStart(2, '0');
  return `${d.getFullYear()}-${p(d.getMonth() + 1)}-${p(d.getDate())} ${p(d.getHours())}:${p(d.getMinutes())}:${p(d.getSeconds())}`;
}

function ensureCsv() {
  if (!fs.existsSync(CSV_PATH)) {
    fs.writeFileSync(CSV_PATH, '\uFEFFip,user_agent,video,start_time,end_time,duration_seconds\n', 'utf8');
  }
}

function appendCsv(row) {
  ensureCsv();
  const line = [
    csvEscape(row.ip),
    csvEscape(row.user_agent),
    csvEscape(row.video),
    csvEscape(row.start_time),
    csvEscape(row.end_time),
    csvEscape(row.duration_seconds),
  ].join(',') + '\n';
  fs.appendFileSync(CSV_PATH, line, 'utf8');
}

// ---------- 静态文件 + Range 流媒体 ----------
function serveStatic(req, res, pathname) {
  // 防路径穿越
  let filePath = path.normalize(path.join(ROOT, decodeURIComponent(pathname)));
  if (!filePath.startsWith(ROOT)) {
    res.writeHead(403); res.end('Forbidden'); return;
  }
  if (pathname === '/' || pathname === '') filePath = path.join(ROOT, 'RD_Interview.html');

  fs.stat(filePath, (err, stat) => {
    if (err || !stat.isFile()) {
      res.writeHead(404, { 'Content-Type': 'text/plain; charset=utf-8' });
      res.end('404 Not Found');
      return;
    }

    const ext = path.extname(filePath).toLowerCase();
    const mime = MIME[ext] || 'application/octet-stream';
    const total = stat.size;

    // 处理 Range 请求（流媒体关键）
    const range = req.headers.range;
    if (range) {
      const m = /bytes=(\d*)-(\d*)/.exec(range);
      let start = m && m[1] ? parseInt(m[1], 10) : 0;
      let end = m && m[2] ? parseInt(m[2], 10) : total - 1;
      if (isNaN(start) || start < 0) start = 0;
      if (isNaN(end) || end >= total) end = total - 1;
      if (start > end) {
        res.writeHead(416, { 'Content-Range': `bytes */${total}` });
        res.end(); return;
      }
      res.writeHead(206, {
        'Content-Type': mime,
        'Content-Range': `bytes ${start}-${end}/${total}`,
        'Accept-Ranges': 'bytes',
        'Content-Length': end - start + 1,
        'Cache-Control': 'public, max-age=3600',
      });
      const stream = fs.createReadStream(filePath, { start, end });
      stream.on('error', () => res.end());
      stream.pipe(res);
    } else {
      res.writeHead(200, {
        'Content-Type': mime,
        'Accept-Ranges': 'bytes',
        'Content-Length': total,
        'Cache-Control': 'public, max-age=3600',
      });
      const stream = fs.createReadStream(filePath);
      stream.on('error', () => res.end());
      stream.pipe(res);
    }
  });
}

// ---------- CSV 解析 ----------
function parseCsv(text) {
  // 去除开头的所有 BOM（可能因多次写入叠加多个）
  while (text.charCodeAt(0) === 0xFEFF) text = text.slice(1);
  const lines = text.split(/\r?\n/).filter((l) => l.trim() !== '');
  if (lines.length === 0) return { headers: [], rows: [] };
  const parseLine = (line) => {
    const out = [];
    let cur = '';
    let inQ = false;
    for (let i = 0; i < line.length; i++) {
      const c = line[i];
      if (inQ) {
        if (c === '"') {
          if (line[i + 1] === '"') { cur += '"'; i++; }
          else inQ = false;
        } else cur += c;
      } else {
        if (c === '"') inQ = true;
        else if (c === ',') { out.push(cur); cur = ''; }
        else cur += c;
      }
    }
    out.push(cur);
    return out;
  };
  const headers = parseLine(lines[0]);
  const rows = lines.slice(1).map(parseLine).filter((r) => r.length >= headers.length || r.some((x) => x !== ''));
  return { headers, rows };
}

// 读取并聚合统计
function getStats() {
  if (!fs.existsSync(CSV_PATH)) {
    return { total: 0, uniqueIps: 0, totalDuration: 0, byVideo: [], byDay: [], recent: [] };
  }
  const text = fs.readFileSync(CSV_PATH, 'utf8');
  const { headers, rows } = parseCsv(text);
  const idx = {};
  headers.forEach((h, i) => { idx[h] = i; });

  const total = rows.length;
  const ipSet = new Set();
  let totalDuration = 0;
  const videoMap = {};   // video -> { count, duration }
  const dayMap = {};     // date -> { count, duration }

  rows.forEach((r) => {
    const ip = (r[idx.ip] || '').trim();
    const video = (r[idx.video] || '').trim() || '(未记录视频)';
    const dur = Math.max(0, Number(r[idx.duration_seconds] || 0));
    const st = (r[idx.start_time] || '').trim();
    const date = st.slice(0, 10) || '(未知日期)';

    if (ip) ipSet.add(ip);
    totalDuration += dur;

    if (!videoMap[video]) videoMap[video] = { count: 0, duration: 0 };
    videoMap[video].count++;
    videoMap[video].duration += dur;

    if (!dayMap[date]) dayMap[date] = { count: 0, duration: 0 };
    dayMap[date].count++;
    dayMap[date].duration += dur;
  });

  const byVideo = Object.keys(videoMap).map((v) => ({
    video: v, count: videoMap[v].count, duration: videoMap[v].duration,
  })).sort((a, b) => b.count - a.count);

  const byDay = Object.keys(dayMap).map((d) => ({
    date: d, count: dayMap[d].count, duration: dayMap[d].duration,
  })).sort((a, b) => a.date.localeCompare(b.date));

  // 最近 20 条明细
  const recent = rows.slice(-20).reverse().map((r) => ({
    ip: (r[idx.ip] || '').trim(),
    user_agent: (r[idx.user_agent] || '').trim(),
    video: (r[idx.video] || '').trim(),
    start_time: (r[idx.start_time] || '').trim(),
    duration_seconds: Math.max(0, Number(r[idx.duration_seconds] || 0)),
  }));

  return {
    total,
    uniqueIps: ipSet.size,
    totalDuration,
    byVideo,
    byDay,
    recent,
  };
}

// ---------- 访问统计 API ----------
function handleApi(req, res, pathname, body) {
  if (pathname === '/api/stats' && req.method === 'GET') {
    try {
      const stats = getStats();
      res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' });
      res.end(JSON.stringify({ ok: true, data: stats }));
    } catch (e) {
      res.writeHead(500, { 'Content-Type': 'application/json; charset=utf-8' });
      res.end(JSON.stringify({ ok: false, error: e.message }));
    }
    return;
  }

  if (pathname === '/api/visit' && req.method === 'POST') {
    let data = {};
    try { data = JSON.parse(body || '{}'); } catch (e) { data = {}; }

    const row = {
      ip: getClientIp(req),
      user_agent: (req.headers['user-agent'] || '').slice(0, 255),
      video: String(data.video || '').slice(0, 255),
      start_time: nowStr(),
      end_time: nowStr(),
      duration_seconds: Math.max(0, Math.round(Number(data.duration_seconds) || 0)),
    };

    try {
      appendCsv(row);
      res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' });
      res.end(JSON.stringify({ ok: true, ip: row.ip }));
    } catch (e) {
      res.writeHead(500, { 'Content-Type': 'application/json; charset=utf-8' });
      res.end(JSON.stringify({ ok: false, error: e.message }));
    }
    return;
  }

  res.writeHead(404, { 'Content-Type': 'application/json; charset=utf-8' });
  res.end(JSON.stringify({ ok: false, error: 'unknown api' }));
}

// ---------- 主服务器 ----------
const server = http.createServer((req, res) => {
  const u = new URL(req.url, `http://${req.headers.host || 'localhost'}`);
  let pathname = u.pathname;

  // 兼容已发布的旧链接：/www/xxx → /xxx
  // 例如 http://10.3.3.131:3838/www/RD_Interview.html → /RD_Interview.html
  if (pathname.startsWith('/www/')) {
    pathname = pathname.slice('/www'.length) || '/';
  }

  if (pathname.startsWith('/api/')) {
    // 收集 body（POST）
    let body = '';
    req.on('data', (c) => { body += c; if (body.length > 1e6) req.destroy(); });
    req.on('end', () => handleApi(req, res, pathname, body));
    return;
  }

  serveStatic(req, res, pathname);
});

server.listen(PORT, '0.0.0.0', () => {
  console.log('==================================================');
  console.log('  研发中心采访专栏 - 流媒体服务已启动');
  console.log(`  播放页面:  http://<本机IP>:${PORT}/RD_Interview.html`);
  console.log(`  统计页面:  http://<本机IP>:${PORT}/stats.html`);
  console.log(`  统计文件:  ${CSV_PATH}`);
  console.log('  (Ctrl+C 停止服务)');
  console.log('==================================================');
});
