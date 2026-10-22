//! 记事模块界面：左列表 + 右详情

use eframe::egui;
use egui::{Color32, RichText};

use crate::app::SuperItomApp;

const NOTE_STATUSES: &[(&str, &str)] = &[
    ("all", "全部"),
    ("pending", "待处理"),
    ("active", "进行中"),
    ("completed", "已完成"),
    ("suspended", "已挂起"),
];

fn note_status(s: &str) -> (&str, Color32) {
    match s {
        "pending" => ("待处理", Color32::from_rgb(240, 173, 78)),
        "active" => ("进行中", Color32::from_rgb(51, 122, 183)),
        "completed" => ("已完成", Color32::from_rgb(92, 184, 92)),
        "suspended" => ("已挂起", Color32::from_rgb(217, 83, 79)),
        _ => (s, Color32::GRAY),
    }
}

fn gray(text: &str) -> RichText {
    RichText::new(text).color(Color32::GRAY)
}

pub fn show(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    ui.columns(2, |cols| {
        show_list(app, &mut cols[0]);
        show_detail(app, &mut cols[1]);
    });
}

fn show_list(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    ui.label(RichText::new("新建记事").strong());
    ui.add(
        egui::TextEdit::multiline(&mut app.note_new_input)
            .hint_text("第一行作为标题…")
            .desired_rows(2)
            .desired_width(f32::INFINITY),
    );
    if ui.button("添加记事").clicked() {
        app.do_note_create();
    }
    ui.separator();

    ui.horizontal(|ui| {
        for (key, label) in NOTE_STATUSES {
            let selected = app.note_status_filter == *key;
            if ui.selectable_label(selected, *label).clicked() {
                app.note_status_filter = (*key).to_string();
            }
        }
    });
    ui.separator();

    let mut clicked_id: Option<i64> = None;
    egui::ScrollArea::vertical().show(ui, |ui| {
        let filter = app.note_status_filter.clone();
        for n in &app.notes {
            if filter != "all" && n.status != filter {
                continue;
            }
            let (status_text, status_color) = note_status(&n.status);
            let pin = if n.pinned > 0 { "📌 " } else { "" };
            let importance = if n.importance > 0 {
                "🚩".repeat(n.importance as usize)
            } else {
                String::new()
            };
            let title = format!("{pin}{importance} {}", n.title);
            let resp = egui::Frame::group(ui.style())
                .show(ui, |ui| {
                    ui.set_width(ui.available_width());
                    ui.label(RichText::new(title).strong());
                    ui.horizontal(|ui| {
                        ui.label(
                            RichText::new(status_text)
                                .color(Color32::WHITE)
                                .background_color(status_color)
                                .size(11.0),
                        );
                        ui.label(
                            RichText::new(format!("{} 评论", n.comment_count))
                                .small()
                                .color(Color32::GRAY),
                        );
                    });
                })
                .response;
            if resp.clicked() {
                clicked_id = Some(n.id);
            }
            ui.add_space(4.0);
        }
        if app.notes.is_empty() {
            ui.label(gray("暂无记事"));
        }
    });

    if let Some(id) = clicked_id {
        app.select_note(id);
    }
}

fn show_detail(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    if app.note_detail.is_none() {
        ui.add_space(40.0);
        ui.vertical_centered(|ui| {
            ui.label(gray("选择左侧记事查看详情"));
        });
        return;
    }

    let detail = app.note_detail.clone().unwrap();
    let comments = app.note_comments.clone();
    let uid = app.current_uid();
    let is_admin = app
        .current_user
        .as_ref()
        .map(|u| u.role == "admin")
        .unwrap_or(false);
    let is_owner = detail.created_by == uid;

    let (status_text, status_color) = note_status(&detail.status);

    egui::ScrollArea::vertical().show(ui, |ui| {
        if app.note_editing {
            ui.label(RichText::new("编辑记事").strong());
            ui.label("标题");
            ui.add(egui::TextEdit::singleline(&mut app.note_edit_title).desired_width(f32::INFINITY));
            ui.label("正文");
            ui.add(
                egui::TextEdit::multiline(&mut app.note_edit_content)
                    .desired_rows(8)
                    .desired_width(f32::INFINITY),
            );
            ui.horizontal(|ui| {
                if ui.button("保存").clicked() {
                    app.do_note_update();
                }
                if ui.button("取消").clicked() {
                    app.note_editing = false;
                    app.note_error = None;
                }
            });
        } else {
            ui.horizontal(|ui| {
                let pin = if detail.pinned > 0 { "📌 " } else { "" };
                ui.heading(format!("{pin}{}", detail.title));
            });
            ui.horizontal(|ui| {
                ui.label(RichText::new(&detail.note_no).small().color(Color32::GRAY));
                ui.label(
                    RichText::new(status_text)
                        .color(Color32::WHITE)
                        .background_color(status_color)
                        .size(11.0),
                );
                ui.label(
                    RichText::new(format!("创建人 {}", detail.creator_name))
                        .small()
                        .color(Color32::GRAY),
                );
                ui.label(RichText::new(&detail.created_at).small().color(Color32::GRAY));
            });

            ui.horizontal(|ui| {
                if is_owner || is_admin {
                    if ui.button("✏ 编辑").clicked() {
                        app.note_edit_title = detail.title.clone();
                        app.note_edit_content = detail.content.clone();
                        app.note_editing = true;
                    }
                    if ui.button("🗑 删除").clicked() {
                        app.do_note_delete();
                    }
                }
                let pin_label = if detail.pinned > 0 { "取消置顶" } else { "置顶" };
                if ui.button(pin_label).clicked() {
                    app.do_note_toggle_pin();
                }
                if ui.button("🚩+").clicked() {
                    app.do_note_importance(1);
                }
                if ui.button("🚩-").clicked() {
                    app.do_note_importance(-1);
                }
            });

            ui.horizontal(|ui| {
                ui.label("状态:");
                for (key, label) in [
                    ("pending", "待处理"),
                    ("active", "进行中"),
                    ("completed", "已完成"),
                    ("suspended", "挂起"),
                ] {
                    if ui.selectable_label(detail.status == key, label).clicked() {
                        app.do_note_patch_status(key);
                    }
                }
            });

            ui.separator();

            if detail.content.is_empty() {
                ui.label(gray("（无正文）"));
            } else {
                ui.label(&detail.content);
            }
        }

        if let Some(err) = &app.note_error {
            ui.colored_label(Color32::RED, err);
        }
        ui.separator();

        ui.label(RichText::new(format!("评论 ({})", comments.len())).strong());
        for c in &comments {
            ui.horizontal(|ui| {
                let done = c.status == "completed";
                let name = if done {
                    format!("{} ✅", c.creator_name)
                } else {
                    c.creator_name.clone()
                };
                ui.label(RichText::new(name).strong().size(12.0));
                ui.label(RichText::new(&c.created_at).small().color(Color32::GRAY));
            });
            ui.label(&c.content);
            ui.horizontal(|ui| {
                let done = c.status == "completed";
                if ui.small_button(if done { "取消完成" } else { "完成" }).clicked() {
                    app.do_note_comment_mark(c.id);
                }
                if ui.small_button("删除").clicked() {
                    app.do_note_comment_delete(c.id);
                }
            });
            ui.add_space(4.0);
        }

        ui.horizontal(|ui| {
            ui.add(
                egui::TextEdit::singleline(&mut app.note_comment_input)
                    .hint_text("输入评论…")
                    .desired_width(ui.available_width() - 70.0),
            );
            if ui.button("发送").clicked() {
                app.do_note_add_comment();
            }
        });
    });
}
