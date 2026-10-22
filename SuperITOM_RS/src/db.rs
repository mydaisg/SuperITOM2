//! 数据访问层：rusqlite 直连现有 SuperITOM2 的 SQLite 数据库
//! 查询逻辑与 R 端（Script/auth.r、server.R 首页）保持一致

use std::path::Path;

use rusqlite::{params, Connection, OptionalExtension};

use crate::models::{
    AssignableUser, DevLog, Note, NoteComment, Project, Solution, Task, User, WoComment, WoDetail,
    WoListItem, WoStats, WorkOrder,
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

// ───────────────────── 记事模块 ─────────────────────

const NOTE_SELECT: &str = "SELECT n.id, COALESCE(n.note_no,''), n.title, COALESCE(n.content,''),
        COALESCE(n.status,'pending'), COALESCE(n.importance,0), COALESCE(n.pinned,0),
        COALESCE(n.reminder_at,''), COALESCE(n.due_at,''), COALESCE(n.created_by,0),
        COALESCE(n.created_at,''), COALESCE(n.updated_at,''),
        COALESCE(NULLIF(u.display_name,''), u.username, '') AS creator_name,
        (SELECT COUNT(*) FROM note_comments c WHERE c.note_id = n.id) AS comment_count
        FROM notes n LEFT JOIN users u ON n.created_by = u.id";

fn map_note(row: &rusqlite::Row) -> rusqlite::Result<Note> {
    Ok(Note {
        id: row.get(0)?,
        note_no: row.get(1)?,
        title: row.get(2)?,
        content: row.get(3)?,
        status: row.get(4)?,
        importance: row.get(5)?,
        pinned: row.get(6)?,
        reminder_at: row.get(7)?,
        due_at: row.get(8)?,
        created_by: row.get(9)?,
        created_at: row.get(10)?,
        updated_at: row.get(11)?,
        creator_name: row.get(12)?,
        comment_count: row.get(13)?,
    })
}

/// 记事编号：NTE + YYYYMMDD + 3位流水
fn generate_note_no(conn: &Connection) -> Result<String, String> {
    let today: String = conn
        .query_row("SELECT strftime('%Y%m%d','now','localtime')", [], |r| r.get(0))
        .map_err(|e| e.to_string())?;
    let prefix = format!("NTE{}", today);
    let last: Option<String> = conn
        .query_row(
            "SELECT note_no FROM notes WHERE note_no LIKE ?1 ORDER BY note_no DESC LIMIT 1",
            params![format!("{}%", prefix)],
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

/// 记事列表：admin 看全部，其他人看自己创建的
pub fn note_list(conn: &Connection, uid: i64, role: &str) -> Result<Vec<Note>, String> {
    let sql = if role == "admin" {
        format!("{} ORDER BY n.pinned DESC, n.updated_at DESC", NOTE_SELECT)
    } else {
        format!(
            "{} WHERE n.created_by = ?1 ORDER BY n.pinned DESC, n.updated_at DESC",
            NOTE_SELECT
        )
    };
    let mut stmt = conn.prepare(&sql).map_err(|e| e.to_string())?;
    if role == "admin" {
        let rows = stmt.query_map([], map_note).map_err(|e| e.to_string())?;
        rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
    } else {
        let rows = stmt.query_map(params![uid], map_note).map_err(|e| e.to_string())?;
        rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
    }
}

/// 单条记事
pub fn note_get(conn: &Connection, id: i64) -> Result<Option<Note>, String> {
    let sql = format!("{} WHERE n.id = ?1", NOTE_SELECT);
    let mut stmt = conn.prepare(&sql).map_err(|e| e.to_string())?;
    stmt.query_row(params![id], map_note)
        .optional()
        .map_err(|e| e.to_string())
}

/// 新建记事：首行作为标题，全文作为正文
pub fn note_create(conn: &Connection, text: &str, uid: i64) -> Result<i64, String> {
    let text = text.trim();
    if text.is_empty() {
        return Err("内容不能为空".to_string());
    }
    let first_line = text.lines().next().unwrap_or("未命名记事").trim();
    let title = if first_line.is_empty() { "未命名记事" } else { first_line };
    let note_no = generate_note_no(conn)?;
    conn.execute(
        "INSERT INTO notes (note_no, title, content, status, importance, pinned, created_by, created_at, updated_at)
         VALUES (?1, ?2, ?3, 'pending', 0, 0, ?4, datetime('now','localtime'), datetime('now','localtime'))",
        params![note_no, title, text, uid],
    )
    .map_err(|e| e.to_string())?;
    Ok(conn.last_insert_rowid())
}

/// 更新记事标题与正文
pub fn note_update(conn: &Connection, id: i64, title: &str, content: &str) -> Result<(), String> {
    conn.execute(
        "UPDATE notes SET title = ?1, content = ?2, updated_at = datetime('now','localtime') WHERE id = ?3",
        params![title, content, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 更新状态
pub fn note_patch_status(conn: &Connection, id: i64, status: &str) -> Result<(), String> {
    conn.execute(
        "UPDATE notes SET status = ?1, updated_at = datetime('now','localtime') WHERE id = ?2",
        params![status, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 更新重要性
pub fn note_patch_importance(conn: &Connection, id: i64, importance: i64) -> Result<(), String> {
    conn.execute(
        "UPDATE notes SET importance = ?1, updated_at = datetime('now','localtime') WHERE id = ?2",
        params![importance, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 置顶切换（最多5条）
pub fn note_toggle_pin(conn: &Connection, id: i64) -> Result<(), String> {
    let pinned: i64 = conn
        .query_row("SELECT COALESCE(pinned,0) FROM notes WHERE id = ?1", params![id], |r| r.get(0))
        .map_err(|e| e.to_string())?;
    if pinned == 0 {
        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM notes WHERE pinned = 1", [], |r| r.get(0))
            .map_err(|e| e.to_string())?;
        if count >= 5 {
            return Err("最多置顶5条".to_string());
        }
        conn.execute(
            "UPDATE notes SET pinned = 1, updated_at = datetime('now','localtime') WHERE id = ?1",
            params![id],
        )
        .map_err(|e| e.to_string())?;
    } else {
        conn.execute("UPDATE notes SET pinned = 0 WHERE id = ?1", params![id])
            .map_err(|e| e.to_string())?;
    }
    Ok(())
}

/// 删除记事（连带评论、派发）
pub fn note_delete(conn: &Connection, id: i64) -> Result<(), String> {
    conn.execute("DELETE FROM note_comments WHERE note_id = ?1", params![id])
        .map_err(|e| e.to_string())?;
    conn.execute("DELETE FROM note_dispatches WHERE note_id = ?1", params![id])
        .map_err(|e| e.to_string())?;
    conn.execute("DELETE FROM notes WHERE id = ?1", params![id])
        .map_err(|e| e.to_string())?;
    Ok(())
}

/// 记事评论列表
pub fn note_comment_list(conn: &Connection, note_id: i64) -> Result<Vec<NoteComment>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT c.id, c.note_id, COALESCE(c.content,''), COALESCE(c.created_by,0),
                    COALESCE(c.created_at,''), COALESCE(c.status,''), COALESCE(c.completed_at,''),
                    COALESCE(c.parent_id,0), COALESCE(NULLIF(u.display_name,''), u.username,'') AS creator_name
             FROM note_comments c LEFT JOIN users u ON c.created_by = u.id
             WHERE c.note_id = ?1 ORDER BY c.created_at ASC, c.id ASC",
        )
        .map_err(|e| e.to_string())?;
    let rows = stmt
        .query_map(params![note_id], |row| {
            Ok(NoteComment {
                id: row.get(0)?,
                note_id: row.get(1)?,
                content: row.get(2)?,
                created_by: row.get(3)?,
                created_at: row.get(4)?,
                status: row.get(5)?,
                completed_at: row.get(6)?,
                parent_id: row.get(7)?,
                creator_name: row.get(8)?,
            })
        })
        .map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 新增评论
pub fn note_comment_add(conn: &Connection, note_id: i64, content: &str, uid: i64) -> Result<i64, String> {
    conn.execute(
        "INSERT INTO note_comments (note_id, content, created_by, created_at)
         VALUES (?1, ?2, ?3, datetime('now','localtime'))",
        params![note_id, content, uid],
    )
    .map_err(|e| e.to_string())?;
    conn.execute(
        "UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = ?1",
        params![note_id],
    )
    .map_err(|e| e.to_string())?;
    Ok(conn.last_insert_rowid())
}

/// 标记评论完成 / 取消完成（status 为 Some("completed") 时完成）
pub fn note_comment_mark(conn: &Connection, comment_id: i64, completed: bool) -> Result<(), String> {
    if completed {
        conn.execute(
            "UPDATE note_comments SET status = 'completed', completed_at = datetime('now','localtime') WHERE id = ?1",
            params![comment_id],
        )
        .map_err(|e| e.to_string())?;
    } else {
        conn.execute(
            "UPDATE note_comments SET status = NULL, completed_at = NULL WHERE id = ?1",
            params![comment_id],
        )
        .map_err(|e| e.to_string())?;
    }
    conn.execute(
        "UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = (SELECT note_id FROM note_comments WHERE id = ?1)",
        params![comment_id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 删除评论
pub fn note_comment_delete(conn: &Connection, comment_id: i64) -> Result<(), String> {
    conn.execute(
        "UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = (SELECT note_id FROM note_comments WHERE id = ?1)",
        params![comment_id],
    )
    .map_err(|e| e.to_string())?;
    conn.execute("DELETE FROM note_comments WHERE id = ?1", params![comment_id])
        .map_err(|e| e.to_string())?;
    Ok(())
}

// ───────────────────── 开发日志模块 ─────────────────────

fn map_devlog(row: &rusqlite::Row) -> rusqlite::Result<DevLog> {
    Ok(DevLog {
        id: row.get(0)?,
        log_no: row.get(1)?,
        module: row.get(2)?,
        title: row.get(3)?,
        requirement: row.get(4)?,
        solution: row.get(5)?,
        result: row.get(6)?,
        requirement_en: row.get(7)?,
        solution_en: row.get(8)?,
        result_en: row.get(9)?,
        commit_msg: row.get(10)?,
        files_changed: row.get(11)?,
        created_by: row.get(12)?,
        created_at: row.get(13)?,
        status: row.get(14)?,
    })
}

/// 开发日志编号：DL + YYYYMMDD + 3位流水
fn generate_devlog_no(conn: &Connection) -> Result<String, String> {
    let today: String = conn
        .query_row("SELECT strftime('%Y%m%d','now','localtime')", [], |r| r.get(0))
        .map_err(|e| e.to_string())?;
    let prefix = format!("DL{}", today);
    let last: Option<String> = conn
        .query_row(
            "SELECT log_no FROM dev_logs WHERE log_no LIKE ?1 ORDER BY log_no DESC LIMIT 1",
            params![format!("{}%", prefix)],
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

/// 开发日志列表（可选模块/搜索过滤）
pub fn devlog_list(
    conn: &Connection,
    module: Option<&str>,
    search: Option<&str>,
) -> Result<Vec<DevLog>, String> {
    let mut sql = String::from(
        "SELECT id, COALESCE(log_no,''), COALESCE(module,''), COALESCE(title,''),
                COALESCE(requirement,''), COALESCE(solution,''), COALESCE(result,''),
                COALESCE(requirement_en,''), COALESCE(solution_en,''), COALESCE(result_en,''),
                COALESCE(commit_msg,''), COALESCE(files_changed,''), COALESCE(created_by,''),
                COALESCE(created_at,''), COALESCE(status,'正式')
         FROM dev_logs WHERE 1=1",
    );
    let mut params: Vec<String> = Vec::new();
    if let Some(m) = module {
        if !m.trim().is_empty() {
            sql.push_str(" AND module = ?");
            params.push(m.to_string());
        }
    }
    if let Some(s) = search {
        let s = s.trim();
        if !s.is_empty() {
            sql.push_str(
                " AND (title LIKE ? OR module LIKE ? OR requirement LIKE ? OR solution LIKE ? OR result LIKE ? OR log_no LIKE ?)",
            );
            let like = format!("%{}%", s);
            for _ in 0..6 {
                params.push(like.clone());
            }
        }
    }
    sql.push_str(" ORDER BY created_at DESC");

    let mut stmt = conn.prepare(&sql).map_err(|e| e.to_string())?;
    let param_refs: Vec<&dyn rusqlite::ToSql> =
        params.iter().map(|s| s as &dyn rusqlite::ToSql).collect();
    let rows = stmt
        .query_map(param_refs.as_slice(), map_devlog)
        .map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 单条开发日志（编辑回填）
pub fn devlog_get(conn: &Connection, id: i64) -> Result<Option<DevLog>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT id, COALESCE(log_no,''), COALESCE(module,''), COALESCE(title,''),
                    COALESCE(requirement,''), COALESCE(solution,''), COALESCE(result,''),
                    COALESCE(requirement_en,''), COALESCE(solution_en,''), COALESCE(result_en,''),
                    COALESCE(commit_msg,''), COALESCE(files_changed,''), COALESCE(created_by,''),
                    COALESCE(created_at,''), COALESCE(status,'正式')
             FROM dev_logs WHERE id = ?1",
        )
        .map_err(|e| e.to_string())?;
    stmt.query_row(params![id], map_devlog)
        .optional()
        .map_err(|e| e.to_string())
}

/// 模块列表（筛选下拉）
pub fn devlog_modules(conn: &Connection) -> Result<Vec<String>, String> {
    let mut stmt = conn
        .prepare("SELECT DISTINCT module FROM dev_logs WHERE module IS NOT NULL AND module != '' ORDER BY module")
        .map_err(|e| e.to_string())?;
    let rows = stmt
        .query_map([], |row| row.get::<_, String>(0))
        .map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 统计：总数 + 今日新增
pub fn devlog_stats(conn: &Connection) -> Result<(i64, i64), String> {
    let total: i64 = conn
        .query_row("SELECT COUNT(*) FROM dev_logs", [], |r| r.get(0))
        .map_err(|e| e.to_string())?;
    let today: i64 = conn
        .query_row(
            "SELECT COUNT(*) FROM dev_logs WHERE DATE(created_at) = DATE('now','localtime')",
            [],
            |r| r.get(0),
        )
        .map_err(|e| e.to_string())?;
    Ok((total, today))
}

/// 新增开发日志
#[allow(clippy::too_many_arguments)]
pub fn devlog_create(
    conn: &Connection,
    module: &str,
    title: &str,
    requirement: &str,
    solution: &str,
    result: &str,
    requirement_en: &str,
    solution_en: &str,
    result_en: &str,
    commit_msg: &str,
    files_changed: &str,
    created_by: &str,
) -> Result<i64, String> {
    let log_no = generate_devlog_no(conn)?;
    conn.execute(
        "INSERT INTO dev_logs (log_no, module, title, requirement, solution, result,
                requirement_en, solution_en, result_en, commit_msg, files_changed, created_by, created_at, status)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, datetime('now','localtime'), '正式')",
        params![
            log_no, module, title, requirement, solution, result, requirement_en, solution_en,
            result_en, commit_msg, files_changed, created_by
        ],
    )
    .map_err(|e| e.to_string())?;
    Ok(conn.last_insert_rowid())
}

/// 更新开发日志
#[allow(clippy::too_many_arguments)]
pub fn devlog_update(
    conn: &Connection,
    id: i64,
    module: &str,
    title: &str,
    requirement: &str,
    solution: &str,
    result: &str,
    requirement_en: &str,
    solution_en: &str,
    result_en: &str,
    commit_msg: &str,
    files_changed: &str,
) -> Result<(), String> {
    conn.execute(
        "UPDATE dev_logs SET module = ?1, title = ?2, requirement = ?3, solution = ?4, result = ?5,
                requirement_en = ?6, solution_en = ?7, result_en = ?8, commit_msg = ?9, files_changed = ?10
         WHERE id = ?11",
        params![
            module, title, requirement, solution, result, requirement_en, solution_en, result_en,
            commit_msg, files_changed, id
        ],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

// ───────────────────── 方案模块 ─────────────────────

fn map_solution(row: &rusqlite::Row) -> rusqlite::Result<Solution> {
    Ok(Solution {
        id: row.get(0)?,
        sol_no: row.get(1)?,
        title: row.get(2)?,
        content: row.get(3)?,
        category: row.get(4)?,
        related_project: row.get(5)?,
        created_by: row.get(6)?,
        created_at: row.get(7)?,
        updated_at: row.get(8)?,
        creator_name: row.get(9)?,
    })
}

/// 方案编号：SOL + YYYYMMDD + 3位流水
fn generate_solution_no(conn: &Connection) -> Result<String, String> {
    let today: String = conn
        .query_row("SELECT strftime('%Y%m%d','now','localtime')", [], |r| r.get(0))
        .map_err(|e| e.to_string())?;
    let prefix = format!("SOL{}", today);
    let last: Option<String> = conn
        .query_row(
            "SELECT sol_no FROM solutions WHERE sol_no LIKE ?1 ORDER BY sol_no DESC LIMIT 1",
            params![format!("{}%", prefix)],
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

/// 方案列表（按更新时间倒序）
pub fn solution_list(conn: &Connection) -> Result<Vec<Solution>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT s.id, COALESCE(s.sol_no,''), s.title, COALESCE(s.content,''),
                    COALESCE(s.category,''), COALESCE(s.related_project,''), COALESCE(s.created_by,0),
                    COALESCE(s.created_at,''), COALESCE(s.updated_at,''),
                    COALESCE(NULLIF(u.display_name,''), u.username,'') AS creator_name
             FROM solutions s LEFT JOIN users u ON s.created_by = u.id
             ORDER BY s.updated_at DESC",
        )
        .map_err(|e| e.to_string())?;
    let rows = stmt.query_map([], map_solution).map_err(|e| e.to_string())?;
    rows.collect::<Result<Vec<_>, _>>().map_err(|e| e.to_string())
}

/// 单条方案
pub fn solution_get(conn: &Connection, id: i64) -> Result<Option<Solution>, String> {
    let mut stmt = conn
        .prepare(
            "SELECT s.id, COALESCE(s.sol_no,''), s.title, COALESCE(s.content,''),
                    COALESCE(s.category,''), COALESCE(s.related_project,''), COALESCE(s.created_by,0),
                    COALESCE(s.created_at,''), COALESCE(s.updated_at,''),
                    COALESCE(NULLIF(u.display_name,''), u.username,'') AS creator_name
             FROM solutions s LEFT JOIN users u ON s.created_by = u.id WHERE s.id = ?1",
        )
        .map_err(|e| e.to_string())?;
    stmt.query_row(params![id], map_solution)
        .optional()
        .map_err(|e| e.to_string())
}

/// 新增方案
pub fn solution_create(
    conn: &Connection,
    title: &str,
    content: &str,
    category: &str,
    related_project: &str,
    uid: i64,
) -> Result<i64, String> {
    let sol_no = generate_solution_no(conn)?;
    conn.execute(
        "INSERT INTO solutions (sol_no, title, content, category, related_project, created_by, created_at, updated_at)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, datetime('now','localtime'), datetime('now','localtime'))",
        params![sol_no, title, content, category, related_project, uid],
    )
    .map_err(|e| e.to_string())?;
    Ok(conn.last_insert_rowid())
}

/// 更新方案
pub fn solution_update(
    conn: &Connection,
    id: i64,
    title: &str,
    content: &str,
    category: &str,
    related_project: &str,
) -> Result<(), String> {
    conn.execute(
        "UPDATE solutions SET title = ?1, content = ?2, category = ?3, related_project = ?4,
                updated_at = datetime('now','localtime') WHERE id = ?5",
        params![title, content, category, related_project, id],
    )
    .map_err(|e| e.to_string())?;
    Ok(())
}

/// 删除方案
pub fn solution_delete(conn: &Connection, id: i64) -> Result<(), String> {
    conn.execute("DELETE FROM solutions WHERE id = ?1", params![id])
        .map_err(|e| e.to_string())?;
    Ok(())
}
