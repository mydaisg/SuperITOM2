//! 数据访问层：rusqlite 直连现有 SuperITOM2 的 SQLite 数据库
//! 查询逻辑与 R 端（Script/auth.r、server.R 首页）保持一致

use std::path::Path;

use rusqlite::{params, Connection, OptionalExtension};

use crate::models::{
    AssignableUser, Project, Task, User, WoComment, WoDetail, WoListItem, WoStats, WorkOrder,
};

/// 定位数据库路径：优先环境变量，其次向上层目录查找 DB/GH_ITOM.db
pub fn default_db_path() -> String {
    if let Ok(p) = std::env::var("SUPERITOM_DB_PATH") {
        if !p.trim().is_empty() {
            return p;
        }
    }
    let candidates = [
        "DB/GH_ITOM.db",
        "../DB/GH_ITOM.db",
        "../../DB/GH_ITOM.db",
        "../../../DB/GH_ITOM.db",
    ];
    for c in candidates {
        if Path::new(c).exists() {
            return c.to_string();
        }
    }
    // 兜底：返回相对路径（不存在时登录会提示错误）
    "../DB/GH_ITOM.db".to_string()
}

pub fn open(path: &str) -> Result<Connection, String> {
    Connection::open(path).map_err(|e| format!("无法打开数据库 {}: {}", path, e))
}

/// 登录：沿用 auth.r 的明文密码比对 + active 禁用检查
pub fn login(conn: &Connection, username: &str, password: &str) -> Result<User, String> {
    let mut stmt = conn
        .prepare(
            "SELECT id, username, COALESCE(display_name,''), role, active
             FROM users WHERE username = ?1 AND password = ?2",
        )
        .map_err(|e| e.to_string())?;

    let user = stmt
        .query_row(params![username, password], |row| {
            Ok(User {
                id: row.get(0)?,
                username: row.get(1)?,
                display_name: row.get(2)?,
                role: row.get(3)?,
                active: row.get(4)?,
            })
        })
        .optional()
        .map_err(|e| e.to_string())?;

    match user {
        None => Err("用户名或密码错误".to_string()),
        Some(u) if u.active == 0 => Err("您的账号已被禁用，请联系管理员".to_string()),
        Some(u) => Ok(u),
    }
}

/// 工单统计（我的相关工单）
pub fn get_stats(conn: &Connection, uid: i64) -> Result<WoStats, String> {
    let mut stmt = conn
        .prepare(
            "SELECT COUNT(*) AS total,
                    COALESCE(SUM(CASE WHEN status='pending' THEN 1 ELSE 0 END),0) AS pending,
                    COALESCE(SUM(CASE WHEN status='assigned' THEN 1 ELSE 0 END),0) AS assigned,
                    COALESCE(SUM(CASE WHEN status='processing' THEN 1 ELSE 0 END),0) AS processing,
                    COALESCE(SUM(CASE WHEN status='completed' THEN 1 ELSE 0 END),0) AS completed,
                    COALESCE(SUM(CASE WHEN status='closed' THEN 1 ELSE 0 END),0) AS closed
             FROM work_orders
             WHERE created_by = ?1 OR assigned_to = ?1 OR handled_by = ?1",
        )
        .map_err(|e| e.to_string())?;

    stmt.query_row(params![uid], |row| {
        Ok(WoStats {
            total: row.get(0)?,
            pending: row.get(1)?,
            assigned: row.get(2)?,
            processing: row.get(3)?,
            completed: row.get(4)?,
            closed: row.get(5)?,
        })
    })
    .map_err(|e| e.to_string())
}

/// 我的项目（排除已完成/已关闭）
pub fn get_projects(conn: &Connection, uid: i64) -> Result<Vec<Project>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT id, project_no, name, status, priority
             FROM projects
             WHERE created_by = ?1 AND status NOT IN ('completed','closed')
             ORDER BY updated_at DESC LIMIT 5",
        )
        .map_err(|e| e.to_string())?;

    let rows = stmt
        .query_map(params![uid], |row| {
            Ok(Project {
                id: row.get(0)?,
                project_no: row.get(1)?,
                name: row.get(2)?,
                status: row.get(3)?,
                priority: row.get(4)?,
            })
        })
        .map_err(|e| e.to_string())?;

    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 我的工单（排除已完成/已关闭）
pub fn get_work_orders(conn: &Connection, uid: i64) -> Result<Vec<WorkOrder>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT id, COALESCE(order_no,''), title, status, priority,
                    COALESCE(category,''), COALESCE(created_at,'')
             FROM work_orders
             WHERE (assigned_to = ?1 OR handled_by = ?1 OR created_by = ?1)
               AND status NOT IN ('completed','closed')
             ORDER BY updated_at DESC LIMIT 5",
        )
        .map_err(|e| e.to_string())?;

    let rows = stmt
        .query_map(params![uid], |row| {
            let id: i64 = row.get(0)?;
            let order_no: String = row.get(1)?;
            let created_at: String = row.get(6)?;
            // 工单号为空时生成兜底：ITS + YYYYMMDD + 3位id（与 R 端一致）
            let order_no = if order_no.trim().is_empty() {
                let d = created_at.chars().take(10).collect::<String>().replace('-', "");
                format!("ITS{}{:03}", d, id)
            } else {
                order_no
            };
            Ok(WorkOrder {
                id,
                order_no,
                title: row.get(2)?,
                status: row.get(3)?,
                priority: row.get(4)?,
                category: row.get(5)?,
            })
        })
        .map_err(|e| e.to_string())?;

    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 我的任务（排除已完成，按重要性排序）
pub fn get_tasks(conn: &Connection, uid: i64) -> Result<Vec<Task>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT t.id, COALESCE(t.task_no,''), t.name, t.status, t.priority,
                    COALESCE(t.importance,0), COALESCE(p.name,'')
             FROM project_tasks t
             LEFT JOIN projects p ON t.project_id = p.id
             WHERE t.assigned_to = ?1 AND t.status NOT IN ('completed')
             ORDER BY COALESCE(t.importance,0) DESC, t.updated_at DESC LIMIT 15",
        )
        .map_err(|e| e.to_string())?;

    let rows = stmt
        .query_map(params![uid], |row| {
            Ok(Task {
                id: row.get(0)?,
                task_no: row.get(1)?,
                name: row.get(2)?,
                status: row.get(3)?,
                priority: row.get(4)?,
                importance: row.get(5)?,
                project_name: row.get(6)?,
            })
        })
        .map_err(|e| e.to_string())?;

    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

// ───────────────────── 工单模块 ─────────────────────

/// 工单号：ITS + YYYYMMDD + 3位流水
fn generate_order_no(conn: &Connection) -> Result<String, String> {
    let today: String = conn
        .query_row("SELECT strftime('%Y%m%d','now','localtime')", [], |r| r.get(0))
        .map_err(|e| e.to_string())?;
    let prefix = format!("ITS{}", today);

    let last: Option<String> = conn
        .query_row(
            "SELECT order_no FROM work_orders WHERE order_no GLOB ?1 ORDER BY order_no DESC LIMIT 1",
            params![format!("{}???", prefix)],
            |r| r.get(0),
        )
        .optional()
        .map_err(|e| e.to_string())?;

    let seq = match last {
        Some(s) if s.len() > prefix.len() => s[prefix.len()..].parse::<i64>().unwrap_or(0) + 1,
        _ => 1,
    };
    Ok(format!("{}{:03}", prefix, seq))
}

/// 工单列表（非 admin 只看与自己相关的）
pub fn get_work_order_list(
    conn: &Connection,
    uid: i64,
    role: &str,
    status_filter: &str,
) -> Result<Vec<WoListItem>, String> {
    let mut where_clause = String::new();
    if role != "admin" {
        where_clause.push_str(&format!(
            "WHERE (wo.created_by = {} OR wo.assigned_to = {} OR wo.handled_by = {})",
            uid, uid, uid
        ));
    }
    if !status_filter.is_empty() && status_filter != "all" {
        if where_clause.is_empty() {
            where_clause.push_str(&format!("WHERE wo.status = '{}'", status_filter));
        } else {
            where_clause.push_str(&format!(" AND wo.status = '{}'", status_filter));
        }
    }

    let sql = format!(
        "SELECT wo.id, COALESCE(wo.order_no,''), wo.title, COALESCE(wo.priority,''),
                wo.status, COALESCE(wo.category,''),
                COALESCE(NULLIF(c.display_name,''), c.username, '') AS creator_name,
                COALESCE(NULLIF(a.display_name,''), a.username, '') AS assignee_name,
                COALESCE(wo.created_at,'') AS created_at
         FROM work_orders wo
         LEFT JOIN users c ON wo.created_by = c.id
         LEFT JOIN users a ON wo.assigned_to = a.id
         {} ORDER BY wo.updated_at DESC",
        where_clause
    );

    let mut stmt = conn.prepare(&sql).map_err(|e| e.to_string())?;
    let rows = stmt
        .query_map([], |row| {
            Ok(WoListItem {
                id: row.get(0)?,
                order_no: row.get(1)?,
                title: row.get(2)?,
                priority: row.get(3)?,
                status: row.get(4)?,
                category: row.get(5)?,
                creator_name: row.get(6)?,
                assignee_name: row.get(7)?,
                created_at: row.get(8)?,
            })
        })
        .map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 工单详情
pub fn get_work_order_detail(conn: &Connection, id: i64) -> Result<Option<WoDetail>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT wo.id, COALESCE(wo.order_no,''), wo.title, COALESCE(wo.description,''),
                    COALESCE(wo.priority,''), wo.status, COALESCE(wo.category,''), COALESCE(wo.subcategory,''),
                    COALESCE(wo.request_user,''), COALESCE(wo.resolution,''),
                    COALESCE(NULLIF(c.display_name,''), c.username, '') AS creator_name,
                    COALESCE(NULLIF(a.display_name,''), a.username, '') AS assignee_name,
                    COALESCE(NULLIF(h.display_name,''), h.username, '') AS handler_name,
                    COALESCE(wo.created_at,''), COALESCE(wo.assigned_at,''), COALESCE(wo.completed_at,''),
                    COALESCE(wo.assigned_to,0), COALESCE(wo.handled_by,0)
             FROM work_orders wo
             LEFT JOIN users c ON wo.created_by = c.id
             LEFT JOIN users a ON wo.assigned_to = a.id
             LEFT JOIN users h ON wo.handled_by = h.id
             WHERE wo.id = ?1",
        )
        .map_err(|e| e.to_string())?;

    let detail = stmt
        .query_row(params![id], |row| {
            Ok(WoDetail {
                id: row.get(0)?,
                order_no: row.get(1)?,
                title: row.get(2)?,
                description: row.get(3)?,
                priority: row.get(4)?,
                status: row.get(5)?,
                category: row.get(6)?,
                subcategory: row.get(7)?,
                request_user: row.get(8)?,
                resolution: row.get(9)?,
                creator_name: row.get(10)?,
                assignee_name: row.get(11)?,
                handler_name: row.get(12)?,
                created_at: row.get(13)?,
                assigned_at: row.get(14)?,
                completed_at: row.get(15)?,
                assigned_to: row.get(16)?,
                handled_by: row.get(17)?,
                comment_count: 0,
            })
        })
        .optional()
        .map_err(|e| e.to_string())?;

    match detail {
        Some(mut d) => {
            d.comment_count = conn
                .query_row(
                    "SELECT COUNT(*) FROM work_order_comments WHERE work_order_id = ?1",
                    params![id],
                    |r| r.get(0),
                )
                .unwrap_or(0);
            Ok(Some(d))
        }
        None => Ok(None),
    }
}

/// 工单评论
pub fn get_work_order_comments(conn: &Connection, id: i64) -> Result<Vec<WoComment>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT c.id, c.comment, COALESCE(c.created_at,''),
                    COALESCE(NULLIF(u.display_name,''), u.username, '') AS creator_name
             FROM work_order_comments c
             LEFT JOIN users u ON c.created_by = u.id
             WHERE c.work_order_id = ?1
             ORDER BY c.created_at ASC, c.id ASC",
        )
        .map_err(|e| e.to_string())?;
    let rows = stmt
        .query_map(params![id], |row| {
            Ok(WoComment {
                id: row.get(0)?,
                comment: row.get(1)?,
                created_at: row.get(2)?,
                creator_name: row.get(3)?,
            })
        })
        .map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 添加评论
pub fn add_work_order_comment(conn: &Connection, id: i64, comment: &str, uid: i64) -> Result<(), String> {
    conn.execute(
        "INSERT INTO work_order_comments (work_order_id, comment, created_by, created_at)
         VALUES (?1, ?2, ?3, datetime('now','localtime'))",
        params![id, comment, uid],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 创建工单
#[allow(clippy::too_many_arguments)]
pub fn create_work_order(
    conn: &Connection,
    title: &str,
    description: &str,
    priority: &str,
    category: &str,
    subcategory: &str,
    request_user: Option<&str>,
    uid: i64,
) -> Result<i64, String> {
    let order_no = generate_order_no(conn)?;
    conn.execute(
        "INSERT INTO work_orders
           (order_no, title, description, priority, status, category, subcategory, request_user, created_by, created_at, updated_at)
         VALUES (?1, ?2, ?3, ?4, 'pending', ?5, ?6, ?7, ?8, datetime('now','localtime'), datetime('now','localtime'))",
        params![order_no, title, description, priority, category, subcategory, request_user, uid],
    )
    .map_err(|e| e.to_string())?;
    Ok(conn.last_insert_rowid())
}

/// 派发
pub fn assign_work_order(conn: &Connection, id: i64, assigned_to: i64, uid: i64) -> Result<(), String> {
    conn.execute(
        "UPDATE work_orders SET assigned_to = ?1, assigned_by = ?2,
                assigned_at = datetime('now','localtime'), status = 'assigned',
                updated_at = datetime('now','localtime') WHERE id = ?3",
        params![assigned_to, uid, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 开始处理
pub fn start_work_order(conn: &Connection, id: i64, uid: i64) -> Result<(), String> {
    conn.execute(
        "UPDATE work_orders SET handled_by = ?1, handled_at = datetime('now','localtime'),
                status = 'processing', updated_at = datetime('now','localtime') WHERE id = ?2",
        params![uid, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 完成
pub fn complete_work_order(conn: &Connection, id: i64, resolution: &str) -> Result<(), String> {
    conn.execute(
        "UPDATE work_orders SET status = 'completed', resolution = ?1,
                completed_at = datetime('now','localtime'), updated_at = datetime('now','localtime')
         WHERE id = ?2",
        params![resolution, id],
    )
    .map_err(|e| e.to_string())?;
    conn.execute(
        "UPDATE inspection_issues SET status = 'resolved', updated_at = datetime('now','localtime')
         WHERE related_work_order_id = ?1 AND status IN ('pending','processing')",
        params![id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 关闭
pub fn close_work_order(conn: &Connection, id: i64, close_reason: &str) -> Result<(), String> {
    if close_reason.is_empty() {
        conn.execute(
            "UPDATE work_orders SET status = 'closed', updated_at = datetime('now','localtime') WHERE id = ?1",
            params![id],
        )
        .map_err(|e| e.to_string())?;
    } else {
        conn.execute(
            "UPDATE work_orders SET status = 'closed', resolution = ?1,
                    completed_at = datetime('now','localtime'), updated_at = datetime('now','localtime')
             WHERE id = ?2",
            params![format!("[已关闭] {}", close_reason), id],
        )
        .map_err(|e| e.to_string())?;
    }
    let target = if close_reason == "已处理和交付" { "resolved" } else { "closed" };
    conn.execute(
        "UPDATE inspection_issues SET status = ?1, updated_at = datetime('now','localtime')
         WHERE related_work_order_id = ?2 AND status IN ('pending','processing')",
        params![target, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 可派发用户
pub fn get_assignable_users(conn: &Connection) -> Result<Vec<AssignableUser>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT id, username, COALESCE(NULLIF(display_name,''), username) AS display_name, role
             FROM users WHERE active = 1 ORDER BY role, username",
        )
        .map_err(|e| e.to_string())?;
    let rows = stmt
        .query_map([], |row| {
            Ok(AssignableUser {
                id: row.get(0)?,
                username: row.get(1)?,
                display_name: row.get(2)?,
                role: row.get(3)?,
            })
        })
        .map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}
