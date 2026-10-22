//! 开发日志模块界面：统计 + 筛选 + 按日期分组的卡片列表 + 内联编辑表单

use eframe::egui;
use egui::{Color32, RichText};

use crate::app::SuperItomApp;

const MODULE_BG: Color32 = Color32::from_rgb(139, 92, 246);
const REQ_COLOR: Color32 = Color32::from_rgb(180, 83, 9);
const SOL_COLOR: Color32 = Color32::from_rgb(46, 125, 50);
const RES_COLOR: Color32 = Color32::from_rgb(21, 101, 192);

fn gray(text: &str) -> RichText {
    RichText::new(text).color(Color32::GRAY)
}

pub fn show(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    let modules = app.devlog_modules.clone();
    let filter = app.devlog_module_filter.clone();

    ui.horizontal(|ui| {
        ui.label(RichText::new(format!("共 {} 条", app.devlog_total)).strong());
        ui.label(
            RichText::new(format!("今日 +{}", app.devlog_today))
                .color(Color32::from_rgb(13, 125, 58)),
        );
        ui.separator();

        egui::ComboBox::from_id_salt("dl_module")
            .selected_text(if filter.is_empty() { "全部模块".to_string() } else { filter.clone() })
            .show_ui(ui, |ui| {
                if ui.selectable_label(app.devlog_module_filter.is_empty(), "全部模块").clicked() {
                    app.devlog_module_filter.clear();
                    app.load_devlogs();
                }
                for m in &modules {
                    if ui.selectable_label(app.devlog_module_filter == *m, m).clicked() {
                        app.devlog_module_filter = m.clone();
                        app.load_devlogs();
                    }
                }
            });

        let search_resp = ui.add(
            egui::TextEdit::singleline(&mut app.devlog_search)
                .hint_text("搜索日志…")
                .desired_width(160.0),
        );
        let enter = search_resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter));
        if ui.button("搜索").clicked() || enter {
            app.load_devlogs();
        }
        if ui.button("刷新").clicked() {
            app.load_devlogs();
        }
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.button("＋ 新建日志").clicked() {
                app.devlog_open_create();
            }
        });
    });
    ui.separator();

    egui::ScrollArea::vertical().show(ui, |ui| {
        if app.devlog_form_open {
            show_form(app, ui);
            ui.separator();
        }
        show_list(app, ui);
    });
}

fn show_form(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    let is_new = app.devlog_editing_id.is_none();
    egui::Frame::group(ui.style()).show(ui, |ui| {
        ui.set_width(ui.available_width());
        ui.label(RichText::new(if is_new { "新建开发日志" } else { "编辑开发日志" }).strong());
        ui.separator();

        ui.horizontal(|ui| {
            ui.label("模块");
            ui.add(egui::TextEdit::singleline(&mut app.dl_form_module).desired_width(150.0));
            ui.add_space(8.0);
            ui.label("标题 *");
            ui.add(egui::TextEdit::singleline(&mut app.dl_form_title).desired_width(f32::INFINITY));
        });
        ui.add_space(4.0);

        ui.label("需求");
        ui.add(
            egui::TextEdit::multiline(&mut app.dl_form_requirement)
                .desired_rows(2)
                .desired_width(f32::INFINITY),
        );
        ui.label("方案");
        ui.add(
            egui::TextEdit::multiline(&mut app.dl_form_solution)
                .desired_rows(2)
                .desired_width(f32::INFINITY),
        );
        ui.label("结果");
        ui.add(
            egui::TextEdit::multiline(&mut app.dl_form_result)
                .desired_rows(2)
                .desired_width(f32::INFINITY),
        );
        ui.add_space(4.0);

        ui.label("需求 EN");
        ui.add(egui::TextEdit::singleline(&mut app.dl_form_requirement_en).desired_width(f32::INFINITY));
        ui.label("方案 EN");
        ui.add(egui::TextEdit::singleline(&mut app.dl_form_solution_en).desired_width(f32::INFINITY));
        ui.label("结果 EN");
        ui.add(egui::TextEdit::singleline(&mut app.dl_form_result_en).desired_width(f32::INFINITY));
        ui.add_space(4.0);

        ui.horizontal(|ui| {
            ui.label("Commit");
            ui.add(egui::TextEdit::singleline(&mut app.dl_form_commit).desired_width(280.0));
        });
        ui.label("变更文件");
        ui.add(egui::TextEdit::singleline(&mut app.dl_form_files).desired_width(f32::INFINITY));
        ui.add_space(4.0);

        if let Some(err) = &app.devlog_error {
            ui.colored_label(Color32::RED, err);
        }
        ui.horizontal(|ui| {
            if ui.button("保存").clicked() {
                app.devlog_save();
            }
            if ui.button("取消").clicked() {
                app.devlog_cancel();
            }
        });
    });
}

fn show_list(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    let logs = app.devlogs.clone();
    let mut edit_id: Option<i64> = None;

    if logs.is_empty() {
        ui.label(gray("暂无开发日志"));
        return;
    }

    let mut current_date = String::new();
    for d in &logs {
        let date = d.created_at.chars().take(10).collect::<String>();
        if date != current_date {
            current_date = date.clone();
            ui.add_space(10.0);
            ui.label(RichText::new(format!("📅 {date}")).strong().size(14.0));
            ui.separator();
        }

        egui::Frame::group(ui.style()).show(ui, |ui| {
            ui.set_width(ui.available_width());

            ui.horizontal(|ui| {
                if !d.log_no.is_empty() {
                    ui.label(RichText::new(&d.log_no).small().monospace().color(Color32::GRAY));
                }
                ui.label(
                    RichText::new(&d.module)
                        .color(Color32::WHITE)
                        .background_color(MODULE_BG)
                        .size(11.0),
                );
                if d.status == "待开发" {
                    ui.label(
                        RichText::new("⏳ 待开发")
                            .color(Color32::WHITE)
                            .background_color(Color32::from_rgb(240, 173, 78))
                            .size(11.0),
                    );
                }
                if !d.commit_msg.is_empty() {
                    ui.label(
                        RichText::new(&d.commit_msg)
                            .monospace()
                            .color(Color32::from_rgb(126, 203, 126))
                            .background_color(Color32::from_rgb(30, 30, 30))
                            .size(10.0),
                    );
                }
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if ui.small_button("✏ 编辑").clicked() {
                        edit_id = Some(d.id);
                    }
                });
            });

            ui.label(RichText::new(&d.title).strong());
            ui.label(RichText::new(&d.created_at).small().color(Color32::GRAY));

            if !d.requirement.is_empty() || !d.requirement_en.is_empty() {
                ui.label(RichText::new("需求").color(REQ_COLOR).strong());
                if !d.requirement.is_empty() {
                    ui.label(&d.requirement);
                }
                if !d.requirement_en.is_empty() {
                    ui.label(RichText::new(&d.requirement_en).monospace().small().color(REQ_COLOR));
                }
            }
            if !d.solution.is_empty() || !d.solution_en.is_empty() {
                ui.label(RichText::new("方案").color(SOL_COLOR).strong());
                if !d.solution.is_empty() {
                    ui.label(&d.solution);
                }
                if !d.solution_en.is_empty() {
                    ui.label(RichText::new(&d.solution_en).monospace().small().color(SOL_COLOR));
                }
            }
            if !d.result.is_empty() || !d.result_en.is_empty() {
                ui.label(RichText::new("结果").color(RES_COLOR).strong());
                if !d.result_en.is_empty() {
                    ui.label(RichText::new(&d.result_en).monospace().small().color(RES_COLOR));
                }
                if !d.result.is_empty() {
                    ui.label(&d.result);
                }
            }
            if !d.files_changed.is_empty() {
                ui.label(RichText::new(&d.files_changed).small().color(Color32::GRAY));
            }
        });
        ui.add_space(4.0);
    }

    if let Some(id) = edit_id {
        app.devlog_open_edit(id);
    }
}
