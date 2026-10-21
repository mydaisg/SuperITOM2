//! 应用状态与主事件循环

use eframe::egui;
use rusqlite::Connection;

use crate::models::{
    AssignableUser, Project, Task, User, WoComment, WoDetail, WoListItem, WoStats, WorkOrder,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Page {
    Dashboard,
    WorkOrderList,
    WorkOrderDetail(i64),
    WorkOrderCreate,
}

pub struct SuperItomApp {
    pub db_path: String,
    pub conn: Option<Connection>,
    pub current_user: Option<User>,
    pub page: Page,
    // 登录态
    pub username: String,
    pub password: String,
    pub login_error: Option<String>,
    // 工作台数据
    pub stats: Option<WoStats>,
    pub projects: Vec<Project>,
    pub work_orders: Vec<WorkOrder>,
    pub tasks: Vec<Task>,
    pub load_error: Option<String>,
    // 工单列表
    pub wo_list: Vec<WoListItem>,
    pub wo_status_filter: String,
    // 工单详情
    pub wo_detail: Option<WoDetail>,
    pub wo_comments: Vec<WoComment>,
    pub wo_comment_input: String,
    pub assignable_users: Vec<AssignableUser>,
    pub assign_index: usize,
    pub complete_resolution: String,
    pub close_reason: String,
    pub action_error: Option<String>,
    // 创建工单
    pub create_title: String,
    pub create_desc: String,
    pub create_priority: String,
    pub create_category: String,
    pub create_subcategory: String,
    pub create_request_user: String,
}

impl SuperItomApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        crate::fonts::install_fonts(&cc.egui_ctx);

        let db_path = crate::db::default_db_path();
        let conn = crate::db::open(&db_path).ok();

        Self {
            db_path,
            conn,
            current_user: None,
            page: Page::Dashboard,
            username: String::new(),
            password: String::new(),
            login_error: None,
            stats: None,
            projects: Vec::new(),
            work_orders: Vec::new(),
            tasks: Vec::new(),
            load_error: None,
            wo_list: Vec::new(),
            wo_status_filter: "all".to_string(),
            wo_detail: None,
            wo_comments: Vec::new(),
            wo_comment_input: String::new(),
            assignable_users: Vec::new(),
            assign_index: 0,
            complete_resolution: String::new(),
            close_reason: String::new(),
            action_error: None,
            create_title: String::new(),
            create_desc: String::new(),
            create_priority: "中".to_string(),
            create_category: "一般".to_string(),
            create_subcategory: String::new(),
            create_request_user: String::new(),
        }
    }

    pub fn current_uid(&self) -> i64 {
        self.current_user.as_ref().map(|u| u.id).unwrap_or(0)
    }

    pub fn can_assign(&self) -> bool {
        matches!(&self.current_user, Some(u) if u.role == "admin" || u.role == "it_desk")
    }

    pub fn do_login(&mut self) {
        self.login_error = None;
        let Some(conn) = &self.conn else {
            self.login_error = Some("数据库未连接，请检查数据库路径".to_string());
            return;
        };
        match crate::db::login(conn, self.username.trim(), &self.password) {
            Ok(user) => {
                self.current_user = Some(user);
                self.password.clear();
                self.page = Page::Dashboard;
                self.load_dashboard();
            }
            Err(e) => self.login_error = Some(e),
        }
    }

    pub fn load_dashboard(&mut self) {
        self.load_error = None;
        let Some(conn) = &self.conn else {
            self.load_error = Some("数据库未连接".to_string());
            return;
        };
        let uid = self.current_uid();
        if uid == 0 {
            return;
        }

        match crate::db::get_stats(conn, uid) {
            Ok(s) => self.stats = Some(s),
            Err(e) => {
                self.stats = None;
                self.load_error = Some(e);
            }
        }
        self.projects = crate::db::get_projects(conn, uid).unwrap_or_default();
        self.work_orders = crate::db::get_work_orders(conn, uid).unwrap_or_default();
        self.tasks = crate::db::get_tasks(conn, uid).unwrap_or_default();
    }

    pub fn do_logout(&mut self) {
        self.current_user = None;
        self.page = Page::Dashboard;
        self.stats = None;
        self.projects.clear();
        self.work_orders.clear();
        self.tasks.clear();
        self.load_error = None;
        self.wo_list.clear();
        self.wo_detail = None;
        self.wo_comments.clear();
        self.assignable_users.clear();
        self.action_error = None;
    }

    // ── 工单 ──

    pub fn go_dashboard(&mut self) {
        self.page = Page::Dashboard;
        self.load_dashboard();
    }

    pub fn go_workorder_list(&mut self) {
        self.page = Page::WorkOrderList;
        self.load_workorder_list();
    }

    pub fn load_workorder_list(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let role = self
            .current_user
            .as_ref()
            .map(|u| u.role.clone())
            .unwrap_or_default();
        self.wo_list =
            crate::db::get_work_order_list(conn, uid, &role, &self.wo_status_filter).unwrap_or_default();
    }

    pub fn go_workorder_detail(&mut self, id: i64) {
        self.page = Page::WorkOrderDetail(id);
        self.load_workorder_detail(id);
        if self.can_assign() {
            if let Some(conn) = &self.conn {
                self.assignable_users = crate::db::get_assignable_users(conn).unwrap_or_default();
            }
        }
    }

    pub fn load_workorder_detail(&mut self, id: i64) {
        let Some(conn) = &self.conn else { return };
        self.wo_detail = crate::db::get_work_order_detail(conn, id).unwrap_or(None);
        self.wo_comments = crate::db::get_work_order_comments(conn, id).unwrap_or_default();
        self.wo_comment_input.clear();
        self.action_error = None;
    }

    pub fn go_workorder_create(&mut self) {
        self.page = Page::WorkOrderCreate;
        self.action_error = None;
    }

    pub fn do_create_workorder(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let title = self.create_title.trim().to_string();
        if title.is_empty() {
            self.action_error = Some("标题不能为空".to_string());
            return;
        }
        let request_user = {
            let r = self.create_request_user.trim();
            if r.is_empty() {
                None
            } else {
                Some(r)
            }
        };
        match crate::db::create_work_order(
            conn,
            &title,
            self.create_desc.trim(),
            &self.create_priority,
            &self.create_category,
            self.create_subcategory.trim(),
            request_user,
            uid,
        ) {
            Ok(_) => {
                self.create_title.clear();
                self.create_desc.clear();
                self.create_subcategory.clear();
                self.create_request_user.clear();
                self.go_workorder_list();
            }
            Err(e) => self.action_error = Some(e),
        }
    }

    pub fn do_assign(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let Some(detail) = &self.wo_detail else { return };
        let Some(target) = self.assignable_users.get(self.assign_index) else {
            self.action_error = Some("请选择派发对象".to_string());
            return;
        };
        match crate::db::assign_work_order(conn, detail.id, target.id, uid) {
            Ok(()) => self.load_workorder_detail(detail.id),
            Err(e) => self.action_error = Some(e),
        }
    }

    pub fn do_start(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let Some(detail) = &self.wo_detail else { return };
        match crate::db::start_work_order(conn, detail.id, uid) {
            Ok(()) => self.load_workorder_detail(detail.id),
            Err(e) => self.action_error = Some(e),
        }
    }

    pub fn do_complete(&mut self) {
        let Some(conn) = &self.conn else { return };
        let Some(detail) = &self.wo_detail else { return };
        let resolution = self.complete_resolution.clone();
        match crate::db::complete_work_order(conn, detail.id, &resolution) {
            Ok(()) => {
                self.complete_resolution.clear();
                self.load_workorder_detail(detail.id);
            }
            Err(e) => self.action_error = Some(e),
        }
    }

    pub fn do_close(&mut self) {
        let Some(conn) = &self.conn else { return };
        let Some(detail) = &self.wo_detail else { return };
        let reason = self.close_reason.clone();
        match crate::db::close_work_order(conn, detail.id, &reason) {
            Ok(()) => {
                self.close_reason.clear();
                self.load_workorder_detail(detail.id);
            }
            Err(e) => self.action_error = Some(e),
        }
    }

    pub fn do_add_comment(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let Some(detail) = &self.wo_detail else { return };
        let comment = self.wo_comment_input.trim().to_string();
        if comment.is_empty() {
            return;
        }
        match crate::db::add_work_order_comment(conn, detail.id, &comment, uid) {
            Ok(()) => {
                self.wo_comment_input.clear();
                if let Some(conn2) = &self.conn {
                    self.wo_comments =
                        crate::db::get_work_order_comments(conn2, detail.id).unwrap_or_default();
                }
            }
            Err(e) => self.action_error = Some(e),
        }
    }
}

impl eframe::App for SuperItomApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            if self.current_user.is_none() {
                crate::login::show(self, ui);
                return;
            }
            match self.page {
                Page::Dashboard => crate::dashboard::show(self, ui),
                Page::WorkOrderList => crate::workorders::show_list(self, ui),
                Page::WorkOrderDetail(_) => crate::workorders::show_detail(self, ui),
                Page::WorkOrderCreate => crate::workorders::show_create(self, ui),
            }
        });
    }
}
