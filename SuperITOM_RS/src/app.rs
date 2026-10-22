//! 应用状态与主事件循环

use eframe::egui;
use rusqlite::Connection;

use crate::models::{
    AssignableUser, DevLog, Note, NoteComment, Project, Solution, Task, User, WoComment, WoDetail,
    WoListItem, WoStats, WorkOrder,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Page {
    Dashboard,
    WorkOrderList,
    WorkOrderDetail(i64),
    WorkOrderCreate,
    Notes,
    DevLogs,
    Solutions,
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
    // 记事模块
    pub notes: Vec<Note>,
    pub note_status_filter: String,
    pub selected_note_id: Option<i64>,
    pub note_detail: Option<Note>,
    pub note_comments: Vec<NoteComment>,
    pub note_new_input: String,
    pub note_comment_input: String,
    pub note_edit_title: String,
    pub note_edit_content: String,
    pub note_editing: bool,
    pub note_error: Option<String>,
    // 开发日志模块
    pub devlogs: Vec<DevLog>,
    pub devlog_module_filter: String,
    pub devlog_search: String,
    pub devlog_modules: Vec<String>,
    pub devlog_total: i64,
    pub devlog_today: i64,
    pub devlog_editing_id: Option<i64>,
    pub devlog_form_open: bool,
    pub dl_form_module: String,
    pub dl_form_title: String,
    pub dl_form_requirement: String,
    pub dl_form_solution: String,
    pub dl_form_result: String,
    pub dl_form_requirement_en: String,
    pub dl_form_solution_en: String,
    pub dl_form_result_en: String,
    pub dl_form_commit: String,
    pub dl_form_files: String,
    pub devlog_error: Option<String>,
    // 方案模块
    pub solutions: Vec<Solution>,
    pub solution_search: String,
    pub solution_category_filter: String,
    pub solution_categories: Vec<String>,
    pub solution_editing_id: Option<i64>,
    pub solution_form_open: bool,
    pub sol_form_title: String,
    pub sol_form_content: String,
    pub sol_form_category: String,
    pub sol_form_project: String,
    pub solution_error: Option<String>,
    pub solution_expanded: Option<i64>,
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
            notes: Vec::new(),
            note_status_filter: "all".to_string(),
            selected_note_id: None,
            note_detail: None,
            note_comments: Vec::new(),
            note_new_input: String::new(),
            note_comment_input: String::new(),
            note_edit_title: String::new(),
            note_edit_content: String::new(),
            note_editing: false,
            note_error: None,
            devlogs: Vec::new(),
            devlog_module_filter: String::new(),
            devlog_search: String::new(),
            devlog_modules: Vec::new(),
            devlog_total: 0,
            devlog_today: 0,
            devlog_editing_id: None,
            devlog_form_open: false,
            dl_form_module: String::new(),
            dl_form_title: String::new(),
            dl_form_requirement: String::new(),
            dl_form_solution: String::new(),
            dl_form_result: String::new(),
            dl_form_requirement_en: String::new(),
            dl_form_solution_en: String::new(),
            dl_form_result_en: String::new(),
            dl_form_commit: String::new(),
            dl_form_files: String::new(),
            devlog_error: None,
            solutions: Vec::new(),
            solution_search: String::new(),
            solution_category_filter: String::new(),
            solution_categories: Vec::new(),
            solution_editing_id: None,
            solution_form_open: false,
            sol_form_title: String::new(),
            sol_form_content: String::new(),
            sol_form_category: String::new(),
            sol_form_project: String::new(),
            solution_error: None,
            solution_expanded: None,
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

    // ── 导航 ──

    fn page_is(&self, kind: &str) -> bool {
        match self.page {
            Page::Dashboard => kind == "dashboard",
            Page::WorkOrderList | Page::WorkOrderDetail(_) | Page::WorkOrderCreate => {
                kind == "workorders"
            }
            Page::Notes => kind == "notes",
            Page::DevLogs => kind == "devlogs",
            Page::Solutions => kind == "solutions",
        }
    }

    pub fn show_top_nav(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            for (key, label) in [
                ("dashboard", "工作台"),
                ("workorders", "工单"),
                ("notes", "记事"),
                ("devlogs", "开发日志"),
                ("solutions", "方案"),
            ] {
                let active = self.page_is(key);
                if ui.selectable_label(active, label).clicked() {
                    match key {
                        "dashboard" => self.go_dashboard(),
                        "workorders" => self.go_workorder_list(),
                        "notes" => self.go_notes(),
                        "devlogs" => self.go_devlogs(),
                        "solutions" => self.go_solutions(),
                        _ => {}
                    }
                }
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                if ui.button("退出").clicked() {
                    self.do_logout();
                }
            });
        });
    }

    // ── 记事模块 ──

    pub fn go_notes(&mut self) {
        self.page = Page::Notes;
        self.load_notes();
    }

    pub fn load_notes(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let role = self
            .current_user
            .as_ref()
            .map(|u| u.role.clone())
            .unwrap_or_default();
        self.notes = crate::db::note_list(conn, uid, &role).unwrap_or_default();
        if let Some(sel) = self.selected_note_id {
            if self.notes.iter().any(|n| n.id == sel) {
                if self.note_detail.is_none() {
                    self.load_note_detail(sel);
                }
            } else {
                self.selected_note_id = None;
                self.note_detail = None;
                self.note_comments.clear();
            }
        }
    }

    pub fn load_note_detail(&mut self, id: i64) {
        let Some(conn) = &self.conn else { return };
        self.note_detail = crate::db::note_get(conn, id).unwrap_or(None);
        self.note_comments = crate::db::note_comment_list(conn, id).unwrap_or_default();
        self.note_error = None;
    }

    pub fn select_note(&mut self, id: i64) {
        self.selected_note_id = Some(id);
        self.note_editing = false;
        self.load_note_detail(id);
    }

    pub fn do_note_create(&mut self) {
        let Some(conn) = &self.conn else { return };
        let uid = self.current_uid();
        let text = self.note_new_input.trim().to_string();
        match crate::db::note_create(conn, &text, uid) {
            Ok(id) => {
                self.note_new_input.clear();
                self.note_error = None;
                self.load_notes();
                self.selected_note_id = Some(id);
                self.load_note_detail(id);
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_update(&mut self) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        match crate::db::note_update(conn, id, self.note_edit_title.trim(), self.note_edit_content.trim()) {
            Ok(()) => {
                self.note_editing = false;
                self.note_error = None;
                self.load_notes();
                self.load_note_detail(id);
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_patch_status(&mut self, status: &str) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        match crate::db::note_patch_status(conn, id, status) {
            Ok(()) => {
                self.note_error = None;
                self.load_notes();
                self.load_note_detail(id);
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_importance(&mut self, delta: i64) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        let cur = self.note_detail.as_ref().map(|d| d.importance).unwrap_or(0);
        let next = (cur + delta).clamp(0, 5);
        match crate::db::note_patch_importance(conn, id, next) {
            Ok(()) => {
                self.note_error = None;
                self.load_notes();
                self.load_note_detail(id);
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_toggle_pin(&mut self) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        match crate::db::note_toggle_pin(conn, id) {
            Ok(()) => {
                self.note_error = None;
                self.load_notes();
                self.load_note_detail(id);
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_delete(&mut self) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        match crate::db::note_delete(conn, id) {
            Ok(()) => {
                self.note_error = None;
                self.selected_note_id = None;
                self.note_detail = None;
                self.note_comments.clear();
                self.load_notes();
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_add_comment(&mut self) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        let uid = self.current_uid();
        let content = self.note_comment_input.trim().to_string();
        if content.is_empty() {
            return;
        }
        match crate::db::note_comment_add(conn, id, &content, uid) {
            Ok(_) => {
                self.note_comment_input.clear();
                self.note_error = None;
                self.note_comments = crate::db::note_comment_list(conn, id).unwrap_or_default();
                self.load_notes();
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_comment_mark(&mut self, comment_id: i64) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        let completed = self
            .note_comments
            .iter()
            .find(|c| c.id == comment_id)
            .map(|c| c.status != "completed")
            .unwrap_or(true);
        match crate::db::note_comment_mark(conn, comment_id, completed) {
            Ok(()) => {
                self.note_error = None;
                self.note_comments = crate::db::note_comment_list(conn, id).unwrap_or_default();
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    pub fn do_note_comment_delete(&mut self, comment_id: i64) {
        let Some(conn) = &self.conn else { return };
        let Some(id) = self.selected_note_id else { return };
        match crate::db::note_comment_delete(conn, comment_id) {
            Ok(()) => {
                self.note_error = None;
                self.note_comments = crate::db::note_comment_list(conn, id).unwrap_or_default();
                self.load_notes();
            }
            Err(e) => self.note_error = Some(e),
        }
    }

    // ── 开发日志模块 ──

    pub fn go_devlogs(&mut self) {
        self.page = Page::DevLogs;
        self.load_devlogs();
    }

    pub fn load_devlogs(&mut self) {
        let Some(conn) = &self.conn else { return };
        let mf = if self.devlog_module_filter.is_empty() {
            None
        } else {
            Some(self.devlog_module_filter.as_str())
        };
        let sk = if self.devlog_search.trim().is_empty() {
            None
        } else {
            Some(self.devlog_search.as_str())
        };
        self.devlogs = crate::db::devlog_list(conn, mf, sk).unwrap_or_default();
        self.devlog_modules = crate::db::devlog_modules(conn).unwrap_or_default();
        if let Ok((t, d)) = crate::db::devlog_stats(conn) {
            self.devlog_total = t;
            self.devlog_today = d;
        }
        self.devlog_error = None;
    }

    pub fn devlog_open_create(&mut self) {
        self.devlog_editing_id = None;
        self.devlog_form_open = true;
        self.dl_form_module.clear();
        self.dl_form_title.clear();
        self.dl_form_requirement.clear();
        self.dl_form_solution.clear();
        self.dl_form_result.clear();
        self.dl_form_requirement_en.clear();
        self.dl_form_solution_en.clear();
        self.dl_form_result_en.clear();
        self.dl_form_commit.clear();
        self.dl_form_files.clear();
        self.devlog_error = None;
    }

    pub fn devlog_open_edit(&mut self, id: i64) {
        let Some(conn) = &self.conn else { return };
        if let Ok(Some(d)) = crate::db::devlog_get(conn, id) {
            self.devlog_editing_id = Some(id);
            self.devlog_form_open = true;
            self.dl_form_module = d.module;
            self.dl_form_title = d.title;
            self.dl_form_requirement = d.requirement;
            self.dl_form_solution = d.solution;
            self.dl_form_result = d.result;
            self.dl_form_requirement_en = d.requirement_en;
            self.dl_form_solution_en = d.solution_en;
            self.dl_form_result_en = d.result_en;
            self.dl_form_commit = d.commit_msg;
            self.dl_form_files = d.files_changed;
            self.devlog_error = None;
        }
    }

    pub fn devlog_save(&mut self) {
        let Some(conn) = &self.conn else { return };
        if self.dl_form_title.trim().is_empty() {
            self.devlog_error = Some("标题不能为空".to_string());
            return;
        }
        let module = if self.dl_form_module.trim().is_empty() {
            "快速开发".to_string()
        } else {
            self.dl_form_module.trim().to_string()
        };
        let result: Result<(), String> = match self.devlog_editing_id {
            Some(id) => crate::db::devlog_update(
                conn, id, &module, self.dl_form_title.trim(), self.dl_form_requirement.trim(),
                self.dl_form_solution.trim(), self.dl_form_result.trim(),
                self.dl_form_requirement_en.trim(), self.dl_form_solution_en.trim(),
                self.dl_form_result_en.trim(), self.dl_form_commit.trim(), self.dl_form_files.trim(),
            ),
            None => {
                let username = self
                    .current_user
                    .as_ref()
                    .map(|u| u.username.clone())
                    .unwrap_or_else(|| "系统".to_string());
                crate::db::devlog_create(
                    conn, &module, self.dl_form_title.trim(), self.dl_form_requirement.trim(),
                    self.dl_form_solution.trim(), self.dl_form_result.trim(),
                    self.dl_form_requirement_en.trim(), self.dl_form_solution_en.trim(),
                    self.dl_form_result_en.trim(), self.dl_form_commit.trim(), self.dl_form_files.trim(),
                    &username,
                )
                .map(|_| ())
            }
        };
        match result {
            Ok(()) => {
                self.devlog_editing_id = None;
                self.devlog_form_open = false;
                self.devlog_error = None;
                self.load_devlogs();
            }
            Err(e) => self.devlog_error = Some(e),
        }
    }

    pub fn devlog_cancel(&mut self) {
        self.devlog_form_open = false;
        self.devlog_error = None;
    }

    // ── 方案模块 ──

    pub fn go_solutions(&mut self) {
        self.page = Page::Solutions;
        self.load_solutions();
    }

    pub fn load_solutions(&mut self) {
        let Some(conn) = &self.conn else { return };
        self.solutions = crate::db::solution_list(conn).unwrap_or_default();
        let mut cats: Vec<String> = self
            .solutions
            .iter()
            .filter_map(|s| {
                if s.category.trim().is_empty() {
                    None
                } else {
                    Some(s.category.clone())
                }
            })
            .collect();
        cats.sort();
        cats.dedup();
        self.solution_categories = cats;
        self.solution_error = None;
    }

    pub fn solution_open_create(&mut self) {
        self.solution_editing_id = None;
        self.solution_form_open = true;
        self.sol_form_title.clear();
        self.sol_form_content.clear();
        self.sol_form_category.clear();
        self.sol_form_project.clear();
        self.solution_error = None;
    }

    pub fn solution_open_edit(&mut self, id: i64) {
        let Some(conn) = &self.conn else { return };
        if let Ok(Some(s)) = crate::db::solution_get(conn, id) {
            self.solution_editing_id = Some(id);
            self.solution_form_open = true;
            self.sol_form_title = s.title;
            self.sol_form_content = s.content;
            self.sol_form_category = s.category;
            self.sol_form_project = s.related_project;
            self.solution_error = None;
        }
    }

    pub fn solution_save(&mut self) {
        let Some(conn) = &self.conn else { return };
        if self.sol_form_title.trim().is_empty() {
            self.solution_error = Some("标题不能为空".to_string());
            return;
        }
        let uid = self.current_uid();
        let result: Result<(), String> = match self.solution_editing_id {
            Some(id) => crate::db::solution_update(
                conn, id, self.sol_form_title.trim(), self.sol_form_content.trim(),
                self.sol_form_category.trim(), self.sol_form_project.trim(),
            ),
            None => crate::db::solution_create(
                conn, self.sol_form_title.trim(), self.sol_form_content.trim(),
                self.sol_form_category.trim(), self.sol_form_project.trim(), uid,
            )
            .map(|_| ()),
        };
        match result {
            Ok(()) => {
                self.solution_editing_id = None;
                self.solution_form_open = false;
                self.solution_error = None;
                self.load_solutions();
            }
            Err(e) => self.solution_error = Some(e),
        }
    }

    pub fn solution_cancel(&mut self) {
        self.solution_form_open = false;
        self.solution_error = None;
    }

    pub fn solution_delete(&mut self, id: i64) {
        let Some(conn) = &self.conn else { return };
        match crate::db::solution_delete(conn, id) {
            Ok(()) => {
                self.solution_error = None;
                if self.solution_expanded == Some(id) {
                    self.solution_expanded = None;
                }
                self.load_solutions();
            }
            Err(e) => self.solution_error = Some(e),
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
            self.show_top_nav(ui);
            ui.separator();
            match self.page {
                Page::Dashboard => crate::dashboard::show(self, ui),
                Page::WorkOrderList => crate::workorders::show_list(self, ui),
                Page::WorkOrderDetail(_) => crate::workorders::show_detail(self, ui),
                Page::WorkOrderCreate => crate::workorders::show_create(self, ui),
                Page::Notes => crate::notes::show(self, ui),
                Page::DevLogs => crate::devlogs::show(self, ui),
                Page::Solutions => crate::solutions::show(self, ui),
            }
        });
    }
}
