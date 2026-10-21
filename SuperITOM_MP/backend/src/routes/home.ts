import { Router } from 'express';
import { queryAll, queryOne } from '../db.js';
import { authRequired } from '../middleware/auth.js';

const router = Router();
router.use(authRequired);

interface WoStats {
  total: number;
  pending: number;
  assigned: number;
  processing: number;
  completed: number;
  closed: number;
}

interface ProjectRow {
  id: number;
  project_no: string;
  name: string;
  status: string;
  priority: string;
  start_date: string | null;
  end_date: string | null;
}

interface WorkOrderRow {
  id: number;
  order_no: string | null;
  title: string;
  status: string;
  priority: string;
  category: string | null;
  created_at: string | null;
}

interface TaskRow {
  id: number;
  task_no: string | null;
  name: string;
  status: string;
  priority: string;
  importance: number | null;
  project_name: string | null;
}

// 生成工单号兜底（与 R 端一致：ITS + YYYYMMDD + 3位id）
function orderNoFallback(id: number, createdAt: string | null): string {
  const d = createdAt ? createdAt.slice(0, 10).replace(/-/g, '') : '';
  return `ITS${d}${String(id).padStart(3, '0')}`;
}

router.get('/dashboard', (req, res) => {
  const uid = req.user!.sub;

  const stats = queryOne<WoStats>(
    `SELECT COUNT(*) AS total,
            COALESCE(SUM(CASE WHEN status='pending' THEN 1 ELSE 0 END), 0) AS pending,
            COALESCE(SUM(CASE WHEN status='assigned' THEN 1 ELSE 0 END), 0) AS assigned,
            COALESCE(SUM(CASE WHEN status='processing' THEN 1 ELSE 0 END), 0) AS processing,
            COALESCE(SUM(CASE WHEN status='completed' THEN 1 ELSE 0 END), 0) AS completed,
            COALESCE(SUM(CASE WHEN status='closed' THEN 1 ELSE 0 END), 0) AS closed
     FROM work_orders
     WHERE created_by = ? OR assigned_to = ? OR handled_by = ?`,
    uid,
    uid,
    uid,
  );

  const projects = queryAll<ProjectRow>(
    `SELECT id, project_no, name, status, priority, start_date, end_date
     FROM projects
     WHERE created_by = ? AND status NOT IN ('completed', 'closed')
     ORDER BY updated_at DESC LIMIT 5`,
    uid,
  );

  const workOrders = queryAll<WorkOrderRow>(
    `SELECT id, order_no, title, status, priority, category, created_at
     FROM work_orders
     WHERE (assigned_to = ? OR handled_by = ? OR created_by = ?)
       AND status NOT IN ('completed', 'closed')
     ORDER BY updated_at DESC LIMIT 5`,
    uid,
    uid,
    uid,
  ).map((w) => ({
    ...w,
    order_no: w.order_no && w.order_no.trim() ? w.order_no : orderNoFallback(w.id, w.created_at),
  }));

  const tasks = queryAll<TaskRow>(
    `SELECT t.id, t.task_no, t.name, t.status, t.priority, t.importance,
            p.name AS project_name
     FROM project_tasks t
     LEFT JOIN projects p ON t.project_id = p.id
     WHERE t.assigned_to = ? AND t.status NOT IN ('completed')
     ORDER BY COALESCE(t.importance, 0) DESC, t.updated_at DESC LIMIT 15`,
    uid,
  );

  res.json({ success: true, data: { stats, projects, workOrders, tasks } });
});

export default router;
