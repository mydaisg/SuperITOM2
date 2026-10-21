import { Router } from 'express';
import { queryOne, queryAll, execute, lastInsertRowId } from '../db.js';
import { authRequired } from '../middleware/auth.js';

const router = Router();
router.use(authRequired);

// 本地日期 YYYYMMDD（用于工单号流水）
function localYmd(d: Date = new Date()): string {
  const y = d.getFullYear();
  const m = String(d.getMonth() + 1).padStart(2, '0');
  const day = String(d.getDate()).padStart(2, '0');
  return `${y}${m}${day}`;
}

// 工单号：ITS + YYYYMMDD + 3位流水（沿用 R 端规则）
function generateOrderNo(): string {
  const prefix = `ITS${localYmd()}`;
  const row = queryOne<{ order_no: string }>(
    "SELECT order_no FROM work_orders WHERE order_no GLOB ? ORDER BY order_no DESC LIMIT 1",
    `${prefix}???`,
  );
  let seq = 1;
  if (row && row.order_no) {
    const n = parseInt(row.order_no.slice(prefix.length), 10);
    if (!Number.isNaN(n)) seq = n + 1;
  }
  return `${prefix}${String(seq).padStart(3, '0')}`;
}

function canAssign(role: string): boolean {
  return role === 'admin' || role === 'it_desk';
}

// 列表：GET /api/workorders?status=&page=&pageSize=
router.get('/', (req, res) => {
  const uid = req.user!.sub;
  const role = req.user!.role;
  const status = (req.query.status as string) || '';
  const page = Math.max(1, parseInt((req.query.page as string) || '1', 10) || 1);
  const pageSize = Math.min(50, Math.max(1, parseInt((req.query.pageSize as string) || '20', 10) || 20));

  const conds: string[] = [];
  const params: (number | string)[] = [];

  // 非 admin 只看与自己相关的工单
  if (role !== 'admin') {
    conds.push('(wo.created_by = ? OR wo.assigned_to = ? OR wo.handled_by = ?)');
    params.push(uid, uid, uid);
  }
  if (status && status !== 'all') {
    conds.push('wo.status = ?');
    params.push(status);
  }
  const where = conds.length ? `WHERE ${conds.join(' AND ')}` : '';

  const total =
    queryOne<{ cnt: number }>(`SELECT COUNT(*) AS cnt FROM work_orders wo ${where}`, ...params)?.cnt ?? 0;

  const list = queryAll(
    `SELECT wo.id, wo.order_no, wo.title, wo.priority, wo.status, wo.category, wo.subcategory,
            wo.request_user,
            COALESCE(wo.created_at,'') AS created_at,
            COALESCE(wo.updated_at,'') AS updated_at,
            COALESCE(NULLIF(c.display_name,''), c.username, '') AS creator_name,
            COALESCE(NULLIF(a.display_name,''), a.username, '') AS assignee_name,
            COALESCE(NULLIF(h.display_name,''), h.username, '') AS handler_name
     FROM work_orders wo
     LEFT JOIN users c ON wo.created_by = c.id
     LEFT JOIN users a ON wo.assigned_to = a.id
     LEFT JOIN users h ON wo.handled_by = h.id
     ${where}
     ORDER BY wo.updated_at DESC
     LIMIT ? OFFSET ?`,
    ...params,
    pageSize,
    (page - 1) * pageSize,
  );

  res.json({ success: true, data: { list, total, page, pageSize } });
});

// 可派发用户（active=1）—— 注意：必须注册在 /:id 之前
router.get('/assignable-users', (req, res) => {
  const users = queryAll(
    `SELECT id, username, COALESCE(NULLIF(display_name,''), username) AS display_name, role
     FROM users WHERE active = 1 ORDER BY role, username`,
  );
  res.json({ success: true, data: users });
});

// 详情：GET /api/workorders/:id
router.get('/:id', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const uid = req.user!.sub;
  const role = req.user!.role;

  const wo = queryOne<any>(
    `SELECT wo.*,
            COALESCE(NULLIF(c.display_name,''), c.username, '') AS creator_name,
            COALESCE(NULLIF(a.display_name,''), a.username, '') AS assignee_name,
            COALESCE(NULLIF(ab.display_name,''), ab.username, '') AS assigner_name,
            COALESCE(NULLIF(h.display_name,''), h.username, '') AS handler_name
     FROM work_orders wo
     LEFT JOIN users c ON wo.created_by = c.id
     LEFT JOIN users a ON wo.assigned_to = a.id
     LEFT JOIN users ab ON wo.assigned_by = ab.id
     LEFT JOIN users h ON wo.handled_by = h.id
     WHERE wo.id = ?`,
    id,
  );

  if (!wo) {
    res.status(404).json({ success: false, message: '工单不存在' });
    return;
  }
  // 非 admin 只能看与自己相关的工单
  if (role !== 'admin' && wo.created_by !== uid && wo.assigned_to !== uid && wo.handled_by !== uid) {
    res.status(403).json({ success: false, message: '无权查看该工单' });
    return;
  }

  const commentCount =
    queryOne<{ cnt: number }>('SELECT COUNT(*) AS cnt FROM work_order_comments WHERE work_order_id = ?', id)?.cnt ?? 0;

  res.json({ success: true, data: { ...wo, comment_count: commentCount } });
});

// 评论列表：GET /api/workorders/:id/comments
router.get('/:id/comments', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const comments = queryAll(
    `SELECT c.id, c.comment, COALESCE(c.created_at,'') AS created_at,
            COALESCE(NULLIF(u.display_name,''), u.username, '') AS creator_name
     FROM work_order_comments c
     LEFT JOIN users u ON c.created_by = u.id
     WHERE c.work_order_id = ?
     ORDER BY c.created_at ASC, c.id ASC`,
    id,
  );
  res.json({ success: true, data: comments });
});

// 添加评论：POST /api/workorders/:id/comments
router.post('/:id/comments', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const uid = req.user!.sub;
  const { comment } = (req.body || {}) as { comment?: string };
  if (!comment || !comment.trim()) {
    res.status(400).json({ success: false, message: '评论内容不能为空' });
    return;
  }
  execute(
    "INSERT INTO work_order_comments (work_order_id, comment, created_by, created_at) VALUES (?, ?, ?, datetime('now','localtime'))",
    id,
    comment.trim(),
    uid,
  );
  res.json({ success: true, message: '评论添加成功' });
});

// 创建工单：POST /api/workorders
router.post('/', (req, res) => {
  const uid = req.user!.sub;
  const { title, description, priority, category, subcategory, request_user } = (req.body || {}) as {
    title?: string;
    description?: string;
    priority?: string;
    category?: string;
    subcategory?: string;
    request_user?: string;
  };
  if (!title || !title.trim()) {
    res.status(400).json({ success: false, message: '标题不能为空' });
    return;
  }
  const orderNo = generateOrderNo();
  execute(
    `INSERT INTO work_orders
       (order_no, title, description, priority, status, category, subcategory, request_user, created_by, created_at, updated_at)
     VALUES (?, ?, ?, ?, 'pending', ?, ?, ?, ?, datetime('now','localtime'), datetime('now','localtime'))`,
    orderNo,
    title.trim(),
    description || '',
    priority || '中',
    category || '一般',
    subcategory || '',
    request_user || null,
    uid,
  );
  res.json({ success: true, message: '工单创建成功', data: { id: lastInsertRowId(), order_no: orderNo } });
});

// 派发：POST /api/workorders/:id/assign
router.post('/:id/assign', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const uid = req.user!.sub;
  const role = req.user!.role;
  if (!canAssign(role)) {
    res.status(403).json({ success: false, message: '无派发权限' });
    return;
  }
  const { assigned_to } = (req.body || {}) as { assigned_to?: number };
  if (!assigned_to) {
    res.status(400).json({ success: false, message: '请选择派发对象' });
    return;
  }
  execute(
    `UPDATE work_orders
     SET assigned_to = ?, assigned_by = ?, assigned_at = datetime('now','localtime'),
         status = 'assigned', updated_at = datetime('now','localtime')
     WHERE id = ?`,
    assigned_to,
    uid,
    id,
  );
  res.json({ success: true, message: '工单派发成功' });
});

// 开始处理：POST /api/workorders/:id/start
router.post('/:id/start', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const uid = req.user!.sub;
  execute(
    `UPDATE work_orders
     SET handled_by = ?, handled_at = datetime('now','localtime'),
         status = 'processing', updated_at = datetime('now','localtime')
     WHERE id = ?`,
    uid,
    id,
  );
  res.json({ success: true, message: '开始处理工单' });
});

// 完成：POST /api/workorders/:id/complete
router.post('/:id/complete', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const { resolution } = (req.body || {}) as { resolution?: string };
  execute(
    `UPDATE work_orders
     SET status = 'completed', resolution = ?,
         completed_at = datetime('now','localtime'), updated_at = datetime('now','localtime')
     WHERE id = ?`,
    resolution || '',
    id,
  );
  // 同步关联巡检异常为已解决
  execute(
    "UPDATE inspection_issues SET status = 'resolved', updated_at = datetime('now','localtime') WHERE related_work_order_id = ? AND status IN ('pending','processing')",
    id,
  );
  res.json({ success: true, message: '工单已完成' });
});

// 关闭：POST /api/workorders/:id/close
router.post('/:id/close', (req, res) => {
  const id = parseInt(req.params.id, 10);
  const { close_reason } = (req.body || {}) as { close_reason?: string };
  const reason = (close_reason || '').trim();
  if (reason) {
    execute(
      `UPDATE work_orders
       SET status = 'closed', resolution = ?,
           completed_at = datetime('now','localtime'), updated_at = datetime('now','localtime')
       WHERE id = ?`,
      `[已关闭] ${reason}`,
      id,
    );
  } else {
    execute(`UPDATE work_orders SET status = 'closed', updated_at = datetime('now','localtime') WHERE id = ?`, id);
  }
  // 同步关联巡检异常状态
  const targetStatus = reason === '已处理和交付' ? 'resolved' : 'closed';
  execute(
    "UPDATE inspection_issues SET status = ?, updated_at = datetime('now','localtime') WHERE related_work_order_id = ? AND status IN ('pending','processing')",
    targetStatus,
    id,
  );
  res.json({ success: true, message: '工单已关闭' });
});

export default router;
