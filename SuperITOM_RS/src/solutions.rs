//! 方案模块界面：搜索 + 分类筛选 + 卡片列表（展开详情）+ 内联编辑表单
//! content 存 HTML（与 R 版一致），显示时转纯文本

use eframe::egui;
use egui::{Color32, RichText};

use crate::app::SuperItomApp;
use crate::models::Solution;

const CAT_BG: Color32 = Color32::from_rgb(46, 125, 50);

fn gray(text: &str) -> RichText {
    RichText::new(text).color(Color32::GRAY)
}

/// 去掉 <script>/<style> 块
fn strip_block(s: &str, open: &str, close: &str) -> String {
    let lower = s.to_lowercase();
    let mut out = String::new();
    let mut rest = s;
    let mut offset = 0;
    while let Some(pos) = lower[offset..].find(open) {
        let abs = offset + pos;
        out.push_str(&rest[..pos]);
        let after_open = &rest[pos + open.len()..];
        let after_lower = &lower[abs + open.len()..];
        if let Some(close_pos) = after_lower.find(close) {
            rest = &after_open[close_pos + close.len()..];
            offset = abs + open.len() + close_pos + close.len();
        } else {
            rest = after_open;
            offset = abs + open.len();
        }
    }
    out.push_str(rest);
    out
}

/// HTML → 纯文本（去除标签、解基本实体、压缩空白）
fn html_to_text(html: &str) -> String {
    let mut s = html.to_string();
    s = strip_block(&s, "<script", "</script>");
    s = strip_block(&s, "<style", "</style>");

    let mut out = String::new();
    let mut in_tag = false;
    for c in s.chars() {
        if c == '<' {
            in_tag = true;
            continue;
        }
        if c == '>' {
            in_tag = false;
            continue;
        }
        if !in_tag {
            out.push(c);
        }
    }
    out = out
        .replace("&nbsp;", " ")
        .replace("&amp;", "&")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"");

    let mut result = String::new();
    let mut prev_space = false;
    for c in out.chars() {
        if c.is_whitespace() {
            if !prev_space {
                result.push(' ');
                prev_space = true;
            }
        } else {
            result.push(c);
            prev_space = false;
        }
    }
    result.trim().to_string()
}

pub fn show(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    let categories = app.solution_categories.clone();
    let filter = app.solution_category_filter.clone();
    let kw = app.solution_search.trim().to_string();

    ui.horizontal(|ui| {
        ui.label(RichText::new(format!("共 {} 条方案", app.solutions.len())).strong());
        ui.separator();

        egui::ComboBox::from_id_salt("sol_category")
            .selected_text(if filter.is_empty() { "全部分类".to_string() } else { filter.clone() })
            .show_ui(ui, |ui| {
                if ui.selectable_label(app.solution_category_filter.is_empty(), "全部分类").clicked() {
                    app.solution_category_filter.clear();
                }
                for c in &categories {
                    if ui.selectable_label(app.solution_category_filter == *c, c).clicked() {
                        app.solution_category_filter = c.clone();
                    }
                }
            });

        let search_resp = ui.add(
            egui::TextEdit::singleline(&mut app.solution_search)
                .hint_text("搜索方案…")
                .desired_width(160.0),
        );
        let enter = search_resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter));
        let _ = enter;

        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.button("＋ 新建方案").clicked() {
                app.solution_open_create();
            }
        });
    });
    ui.separator();

    egui::ScrollArea::vertical().show(ui, |ui| {
        if app.solution_form_open {
            show_form(app, ui);
            ui.separator();
        }
        show_list(app, ui, &kw);
    });
}

fn show_form(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    let is_new = app.solution_editing_id.is_none();
    egui::Frame::group(ui.style()).show(ui, |ui| {
        ui.set_width(ui.available_width());
        ui.label(RichText::new(if is_new { "新建方案" } else { "编辑方案" }).strong());
        ui.separator();

        ui.label("方案标题 *");
        ui.add(egui::TextEdit::singleline(&mut app.sol_form_title).desired_width(f32::INFINITY));
        ui.add_space(4.0);

        ui.horizontal(|ui| {
            ui.label("分类");
            ui.add(egui::TextEdit::singleline(&mut app.sol_form_category).desired_width(180.0));
            ui.add_space(8.0);
            ui.label("关联项目号");
            ui.add(egui::TextEdit::singleline(&mut app.sol_form_project).desired_width(200.0));
        });
        ui.add_space(4.0);

        ui.label("方案内容（支持 HTML）");
        ui.add(
            egui::TextEdit::multiline(&mut app.sol_form_content)
                .desired_rows(10)
                .desired_width(f32::INFINITY),
        );
        ui.add_space(4.0);

        if let Some(err) = &app.solution_error {
            ui.colored_label(Color32::RED, err);
        }
        ui.horizontal(|ui| {
            if ui.button("保存").clicked() {
                app.solution_save();
            }
            if ui.button("取消").clicked() {
                app.solution_cancel();
            }
        });
    });
}

fn show_list(app: &mut SuperItomApp, ui: &mut egui::Ui, kw: &str) {
    let shown: Vec<Solution> = app
        .solutions
        .iter()
        .filter(|s| {
            let ok_cat = app.solution_category_filter.is_empty()
                || s.category == app.solution_category_filter;
            let ok_kw = kw.is_empty() || s.title.contains(kw) || s.content.contains(kw);
            ok_cat && ok_kw
        })
        .cloned()
        .collect();

    if shown.is_empty() {
        ui.label(gray("暂无方案"));
        return;
    }

    let mut toggle_id: Option<i64> = None;
    let mut edit_id: Option<i64> = None;
    let mut delete_id: Option<i64> = None;

    for s in &shown {
        let expanded = app.solution_expanded == Some(s.id);
        egui::Frame::group(ui.style()).show(ui, |ui| {
            ui.set_width(ui.available_width());

            ui.horizontal(|ui| {
                if ui.selectable_label(expanded, if expanded { "▾" } else { "▸" }).clicked() {
                    toggle_id = Some(s.id);
                }
                ui.label(RichText::new(&s.sol_no).small().monospace().color(Color32::GRAY));
                ui.label(RichText::new(&s.title).strong());
                if !s.category.is_empty() {
                    ui.label(
                        RichText::new(&s.category)
                            .color(Color32::WHITE)
                            .background_color(CAT_BG)
                            .size(11.0),
                    );
                }
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if ui.small_button("✏ 编辑").clicked() {
                        edit_id = Some(s.id);
                    }
                    if ui.small_button("🗑 删除").clicked() {
                        delete_id = Some(s.id);
                    }
                });
            });

            ui.label(
                RichText::new(format!("{} · 更新 {}", s.creator_name, s.updated_at))
                    .small()
                    .color(Color32::GRAY),
            );

            if expanded {
                ui.separator();
                let text = html_to_text(&s.content);
                if text.is_empty() {
                    ui.label(gray("（无内容）"));
                } else {
                    ui.label(&text);
                }
                if !s.related_project.is_empty() {
                    ui.label(
                        RichText::new(format!("关联项目: {}", s.related_project))
                            .small()
                            .color(Color32::GRAY),
                    );
                }
            }
        });
        ui.add_space(4.0);
    }

    if let Some(id) = toggle_id {
        app.solution_expanded = if app.solution_expanded == Some(id) {
            None
        } else {
            Some(id)
        };
    }
    if let Some(id) = edit_id {
        app.solution_open_edit(id);
    }
    if let Some(id) = delete_id {
        app.solution_delete(id);
    }
}
