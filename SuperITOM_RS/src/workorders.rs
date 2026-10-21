//! 工单模块界面：列表 / 详情 / 创建

use eframe::egui;
use egui::{Color32, RichText};

use crate::app::SuperItomApp;

const STATUS_FILTERS: &[(&str, &str)] = &[
    ("all", "全部"),
    ("pending", "待处理"),
    ("assigned", "已派发"),
    ("processing", "处理中"),
    ("completed", "已完成"),
    ("closed", "已关闭"),
];

const PRIORITIES: &[&str] = &["低", "中", "高", "紧急"];
const CATEGORIES: &[&str] = &["一般", "故障", "请求", "咨询", "变更", "其他"];

fn wo_status(s: &str) -> (&str, Color32) {
    match s {
        "pending" => ("待处理", Color32::from_rgb(240, 173, 78)),
        "assigned" => ("已派发", Color32::from_rgb(91, 192, 222)),
        "processing" => ("处理中", Color32::from_rgb(51, 122, 183)),
        "completed" => ("已完成", Color32::from_rgb(92, 184, 92)),
        "closed" => ("已关闭", Color32::from_rgb(217, 83, 79)),
        _ => (s, Color32::GRAY),
    }
}

fn priority_color(p: &str) -> Color32 {
    match p {
        "紧急" => Color32::from_rgb(217, 83, 79),
        "高" => Color32::from_rgb(240, 173, 78),
        "中" => Color32::from_rgb(91, 192, 222),
        _ => Color32::from_rgb(153, 153, 153),
    }
}

fn gray(text: &str) -> RichText {
    RichText::new(text).color(Color32::GRAY)
}

// ───────── 列表 ─────────

pub fn show_list(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    ui.horizontal(|ui| {
        if ui.button("← 工作台").clicked() {
            app.go_dashboard();
        }
        ui.heading("工单");
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.button("＋ 新建工单").clicked() {
                app.go_workorder_create();
            }
            if ui.button("刷新").clicked() {
                app.load_workorder_list();
            }
        });
    });
    ui.separator();

    // 状态筛选
    ui.horizontal(|ui| {
        for (key, label) in STATUS_FILTERS {
            let selected = app.wo_status_filter == *key;
            if ui.selectable_label(selected, *label).clicked() {
                app.wo_status_filter = (*key).to_string();
                app.load_workorder_list();
            }
        }
    });
    ui.separator();

    // 列表
    let mut clicked_id: Option<i64> = None;
    egui::ScrollArea::vertical().show(ui, |ui| {
        if app.wo_list.is_empty() {
            ui.label(gray("暂无工单"));
            return;
        }
        for w in &app.wo_list {
            let (status_text, status_color) = wo_status(&w.status);
            let resp = egui::Frame::group(ui.style())
                .show(ui, |ui| {
                    ui.set_width(ui.available_width());
                    ui.horizontal(|ui| {
                        ui.label(RichText::new(&w.order_no).small().color(Color32::GRAY));
                        ui.add_space(8.0);
                        ui.label(
                            RichText::new(status_text)
                                .color(Color32::WHITE)
                                .background_color(status_color)
                                .size(11.0),
                        );
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.label(
                                RichText::new(&w.priority)
                                    .color(priority_color(&w.priority))
                                    .small(),
                            );
                        });
                    });
                    ui.add_space(4.0);
                    ui.label(RichText::new(&w.title).strong());
                    let meta = format!("{} · {}", w.creator_name, w.created_at);
                    ui.label(RichText::new(meta).small().color(Color32::GRAY));
                })
                .response;
            if resp.clicked() {
                clicked_id = Some(w.id);
            }
            ui.add_space(4.0);
        }
    });

    if let Some(id) = clicked_id {
        app.go_workorder_detail(id);
    }
}

// ───────── 详情 ─────────

pub fn show_detail(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    if ui.button("← 返回列表").clicked() {
        app.go_workorder_list();
        return;
    }
    ui.separator();

    let Some(detail) = app.wo_detail.clone() else {
        ui.label(gray("工单不存在"));
        return;
    };
    let comments = app.wo_comments.clone();
    let assignable_names: Vec<String> = app
        .assignable_users
        .iter()
        .map(|u| u.display_name.clone())
        .collect();
    let uid = app.current_uid();

    let (status_text, status_color) = wo_status(&detail.status);
    let is_assignee = detail.assigned_to == uid;
    let is_handler = detail.handled_by == uid;
    let is_terminal = detail.status == "completed" || detail.status == "closed";

    egui::ScrollArea::vertical().show(ui, |ui| {
        // 头部
        ui.heading(&detail.title);
        ui.horizontal(|ui| {
            ui.label(RichText::new(&detail.order_no).small().color(Color32::GRAY));
            ui.label(
                RichText::new(status_text)
                    .color(Color32::WHITE)
                    .background_color(status_color)
                    .size(11.0),
            );
            ui.label(RichText::new(&detail.priority).color(priority_color(&detail.priority)).small());
        });
        ui.separator();

        // 描述
        if !detail.description.is_empty() {
            ui.label(RichText::new("描述").strong());
            ui.label(&detail.description);
            ui.add_space(8.0);
        }

        // 字段
        ui.label(RichText::new("信息").strong());
        field_row(ui, "创建人", &detail.creator_name);
        let assignee = if detail.assignee_name.is_empty() {
            "未分配".to_string()
        } else {
            detail.assignee_name.clone()
        };
        field_row(ui, "指派给", &assignee);
        if !detail.handler_name.is_empty() {
            field_row(ui, "处理人", &detail.handler_name);
        }
        if !detail.request_user.is_empty() {
            field_row(ui, "请求用户", &detail.request_user);
        }
        let cat = if detail.subcategory.is_empty() {
            detail.category.clone()
        } else {
            format!("{} / {}", detail.category, detail.subcategory)
        };
        field_row(ui, "分类", &cat);
        field_row(ui, "创建时间", &detail.created_at);
        if !detail.completed_at.is_empty() {
            field_row(ui, "完成时间", &detail.completed_at);
        }
        if !detail.resolution.is_empty() {
            field_row(ui, "解决方案", &detail.resolution);
        }
        ui.separator();

        // 操作区
        if !is_terminal {
            ui.label(RichText::new("操作").strong());
            ui.add_space(4.0);

            if detail.status == "pending" && app.can_assign() {
                ui.horizontal(|ui| {
                    egui::ComboBox::from_label("派发给")
                        .selected_text(
                            assignable_names
                                .get(app.assign_index)
                                .cloned()
                                .unwrap_or_else(|| "选择用户".to_string()),
                        )
                        .show_ui(ui, |ui| {
                            for (i, name) in assignable_names.iter().enumerate() {
                                ui.selectable_value(&mut app.assign_index, i, name);
                            }
                        });
                    if ui.button("确认派发").clicked() {
                        app.do_assign();
                    }
                });
            }

            if detail.status == "assigned" && is_assignee {
                if ui.button("开始处理").clicked() {
                    app.do_start();
                }
            }

            if detail.status == "processing" && is_handler {
                ui.horizontal(|ui| {
                    ui.label("解决方案");
                    ui.text_edit_singleline(&mut app.complete_resolution);
                    if ui.button("完成").clicked() {
                        app.do_complete();
                    }
                });
            }

            ui.horizontal(|ui| {
                ui.label("关闭原因");
                ui.text_edit_singleline(&mut app.close_reason);
                if ui.button("关闭").clicked() {
                    app.do_close();
                }
            });
            ui.add_space(8.0);
        }

        if let Some(err) = &app.action_error {
            ui.colored_label(Color32::RED, err);
        }
        ui.separator();

        // 评论
        ui.label(RichText::new(format!("评论 ({})", comments.len())).strong());
        if comments.is_empty() {
            ui.label(gray("暂无评论"));
        } else {
            for c in &comments {
                ui.horizontal(|ui| {
                    ui.label(RichText::new(&c.creator_name).strong().size(12.0));
                    ui.label(RichText::new(&c.created_at).small().color(Color32::GRAY));
                });
                ui.label(&c.comment);
                ui.add_space(6.0);
            }
        }
        ui.add_space(8.0);

        // 评论输入
        ui.horizontal(|ui| {
            ui.add(
                egui::TextEdit::singleline(&mut app.wo_comment_input)
                    .hint_text("输入评论…")
                    .desired_width(ui.available_width() - 70.0),
            );
            if ui.button("发送").clicked() {
                app.do_add_comment();
            }
        });
    });
}

// ───────── 创建 ─────────

pub fn show_create(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    if ui.button("← 返回列表").clicked() {
        app.go_workorder_list();
        return;
    }
    ui.separator();
    ui.heading("创建工单");
    ui.add_space(8.0);

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.label("标题 *");
        ui.add(
            egui::TextEdit::singleline(&mut app.create_title)
                .hint_text("请输入工单标题")
                .desired_width(f32::INFINITY),
        );
        ui.add_space(8.0);

        ui.label("描述");
        ui.add(
            egui::TextEdit::multiline(&mut app.create_desc)
                .hint_text("请输入问题/需求描述")
                .desired_rows(5)
                .desired_width(f32::INFINITY),
        );
        ui.add_space(8.0);

        ui.horizontal(|ui| {
            ui.label("优先级");
            egui::ComboBox::from_id_salt("priority_combo")
                .selected_text(app.create_priority.clone())
                .show_ui(ui, |ui| {
                    for p in PRIORITIES {
                        ui.selectable_value(&mut app.create_priority, (*p).to_string(), *p);
                    }
                });
            ui.add_space(16.0);
            ui.label("分类");
            egui::ComboBox::from_id_salt("category_combo")
                .selected_text(app.create_category.clone())
                .show_ui(ui, |ui| {
                    for c in CATEGORIES {
                        ui.selectable_value(&mut app.create_category, (*c).to_string(), *c);
                    }
                });
        });
        ui.add_space(8.0);

        ui.label("子分类");
        ui.add(
            egui::TextEdit::singleline(&mut app.create_subcategory)
                .hint_text("可选")
                .desired_width(f32::INFINITY),
        );
        ui.add_space(8.0);

        ui.label("请求用户");
        ui.add(
            egui::TextEdit::singleline(&mut app.create_request_user)
                .hint_text("可选，如：谢芳材-供应链中心")
                .desired_width(f32::INFINITY),
        );
        ui.add_space(12.0);

        if let Some(err) = &app.action_error {
            ui.colored_label(Color32::RED, err);
            ui.add_space(8.0);
        }

        if ui
            .add_sized([200.0, 36.0], egui::Button::new("提交工单"))
            .clicked()
        {
            app.do_create_workorder();
        }
    });
}

fn field_row(ui: &mut egui::Ui, label: &str, value: &str) {
    ui.horizontal(|ui| {
        ui.label(RichText::new(label).color(Color32::GRAY));
        ui.add_space(8.0);
        ui.label(value);
    });
}
