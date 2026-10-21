//! 数据模型（纯数据结构，不依赖 UI 库）

#[allow(dead_code)]
pub struct User {
    pub id: i64,
    pub username: String,
    pub display_name: String,
    pub role: String,
    pub active: i64,
}

pub struct WoStats {
    pub total: i64,
    pub pending: i64,
    pub assigned: i64,
    pub processing: i64,
    pub completed: i64,
    pub closed: i64,
}

#[allow(dead_code)]
pub struct Project {
    pub id: i64,
    pub project_no: String,
    pub name: String,
    pub status: String,
    pub priority: String,
}

#[allow(dead_code)]
pub struct WorkOrder {
    pub id: i64,
    pub order_no: String,
    pub title: String,
    pub status: String,
    pub priority: String,
    pub category: String,
}

#[allow(dead_code)]
pub struct Task {
    pub id: i64,
    pub task_no: String,
    pub name: String,
    pub status: String,
    pub priority: String,
    pub importance: i64,
    pub project_name: String,
}

#[allow(dead_code)]
pub struct WoListItem {
    pub id: i64,
    pub order_no: String,
    pub title: String,
    pub priority: String,
    pub status: String,
    pub category: String,
    pub creator_name: String,
    pub assignee_name: String,
    pub created_at: String,
}

#[derive(Clone)]
#[allow(dead_code)]
pub struct WoDetail {
    pub id: i64,
    pub order_no: String,
    pub title: String,
    pub description: String,
    pub priority: String,
    pub status: String,
    pub category: String,
    pub subcategory: String,
    pub request_user: String,
    pub resolution: String,
    pub creator_name: String,
    pub assignee_name: String,
    pub handler_name: String,
    pub created_at: String,
    pub assigned_at: String,
    pub completed_at: String,
    pub assigned_to: i64,
    pub handled_by: i64,
    pub comment_count: i64,
}

#[derive(Clone)]
#[allow(dead_code)]
pub struct WoComment {
    pub id: i64,
    pub comment: String,
    pub created_at: String,
    pub creator_name: String,
}

#[allow(dead_code)]
pub struct AssignableUser {
    pub id: i64,
    pub username: String,
    pub display_name: String,
    pub role: String,
}
