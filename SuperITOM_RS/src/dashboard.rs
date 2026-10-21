//! 首页工作台界面

use eframe::egui;

use crate::app::SuperItomApp;
use crate::models::WoStats;

pub fn show(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    // 顶部栏
    ui.horizontal(|ui| {
        let name = app
            .current_user
            .as_ref()
            .map(|u| u.display_name.clone())
            .unwrap_or_default();
        ui.heading(format!("你好，{name}"));
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.button("刷新").clicked() {
                app.load_dashboard();
            }
            if ui.button("退出").clicked() {
                app.do_logout();
            }
        });
    });
    ui.separator();

    if let Some(err) = &app.load_error {
        ui.colored_label(egui::Color32::RED, format!("加载失败: {err}"));
    }

    egui::ScrollArea::vertical().show(ui, |ui| {
        if let Some(stats) = &app.stats {
            show_stats(ui, stats);
            ui.add_space(8.0);
        }

        egui::CollapsingHeader::new("我的项目")
            .default_open(true)
            .show(ui, |ui| {
                if app.projects.is_empty() {
                    ui.label(gray("暂无进行中的项目"));
                } else {
                    for p in &app.projects {
                        let (text, color) = project_status(p.status.as_str());
                        item_row(ui, text, color, &p.name, &format!("[{}]", p.project_no));
                    }
                }
            });

        egui::CollapsingHeader::new("我的工单")
            .default_open(true)
            .show(ui, |ui| {
                if app.work_orders.is_empty() {
                    ui.label(gray("暂无待处理的工单"));
                } else {
                    for w in &app.work_orders {
                        let (text, color) = work_order_status(w.status.as_str());
                        let meta = if w.category.is_empty() {
                            w.order_no.clone()
                        } else {
                            format!("{} · {}", w.order_no, w.category)
                        };
                        item_row(ui, text, color, &w.title, &meta);
                    }
                }
            });

        egui::CollapsingHeader::new("我的任务")
            .default_open(true)
            .show(ui, |ui| {
                if app.tasks.is_empty() {
                    ui.label(gray("暂无待处理的任务"));
                } else {
                    for t in &app.tasks {
                        let (text, color) = task_status(t.status.as_str());
                        let name = if t.importance > 0 {
                            format!("{} {}", "🚩".repeat(t.importance as usize), t.name)
                        } else {
                            t.name.clone()
                        };
                        let task_no = if t.task_no.is_empty() {
                            "-".to_string()
                        } else {
                            t.task_no.clone()
                        };
                        let meta = if t.project_name.is_empty() {
                            task_no
                        } else {
                            format!("{task_no} · {}", t.project_name)
                        };
                        item_row(ui, text, color, &name, &meta);
                    }
                }
            });
    });
}

fn show_stats(ui: &mut egui::Ui, stats: &WoStats) {
    let cards = [
        ("全部", stats.total, egui::Color32::from_rgb(102, 126, 234)),
        ("待处理", stats.pending, egui::Color32::from_rgb(240, 173, 78)),
        ("已派发", stats.assigned, egui::Color32::from_rgb(91, 192, 222)),
        ("处理中", stats.processing, egui::Color32::from_rgb(51, 122, 183)),
        ("已完成", stats.completed, egui::Color32::from_rgb(92, 184, 92)),
        ("已关闭", stats.closed, egui::Color32::from_rgb(217, 83, 79)),
    ];

    ui.columns(6, |cols| {
        for (i, (label, value, color)) in cards.iter().enumerate() {
            egui::Frame::group(cols[i].style()).show(&mut cols[i], |ui| {
                ui.set_width(ui.available_width());
                ui.vertical_centered(|ui| {
                    ui.label(
                        egui::RichText::new(value.to_string())
                            .size(22.0)
                            .strong()
                            .color(*color),
                    );
                    ui.label(egui::RichText::new(*label).size(11.0).color(egui::Color32::GRAY));
                });
            });
        }
    });
}

fn item_row(ui: &mut egui::Ui, status_text: &str, status_color: egui::Color32, name: &str, meta: &str) {
    ui.horizontal(|ui| {
        ui.label(
            egui::RichText::new(status_text)
                .color(egui::Color32::WHITE)
                .background_color(status_color)
                .size(12.0),
        );
        ui.add_space(6.0);
        ui.label(egui::RichText::new(name).strong());
        ui.add_space(6.0);
        ui.label(egui::RichText::new(meta).color(egui::Color32::GRAY).size(12.0));
    });
    ui.add_space(2.0);
}

fn gray(text: &str) -> egui::RichText {
    egui::RichText::new(text).color(egui::Color32::GRAY)
}

fn project_status(s: &str) -> (&str, egui::Color32) {
    match s {
        "planning" => ("规划中", egui::Color32::from_rgb(91, 192, 222)),
        "active" => ("进行中", egui::Color32::from_rgb(51, 122, 183)),
        "suspended" => ("已暂停", egui::Color32::from_rgb(240, 173, 78)),
        "completed" => ("已完成", egui::Color32::from_rgb(92, 184, 92)),
        "closed" => ("已关闭", egui::Color32::from_rgb(217, 83, 79)),
        _ => (s, egui::Color32::GRAY),
    }
}

fn work_order_status(s: &str) -> (&str, egui::Color32) {
    match s {
        "pending" => ("待处理", egui::Color32::from_rgb(240, 173, 78)),
        "assigned" => ("已派发", egui::Color32::from_rgb(91, 192, 222)),
        "processing" => ("处理中", egui::Color32::from_rgb(51, 122, 183)),
        "completed" => ("已完成", egui::Color32::from_rgb(92, 184, 92)),
        "closed" => ("已关闭", egui::Color32::from_rgb(217, 83, 79)),
        _ => (s, egui::Color32::GRAY),
    }
}

fn task_status(s: &str) -> (&str, egui::Color32) {
    match s {
        "pending" => ("待处理", egui::Color32::from_rgb(240, 173, 78)),
        "in_progress" => ("进行中", egui::Color32::from_rgb(51, 122, 183)),
        "blocked" => ("已阻塞", egui::Color32::from_rgb(217, 83, 79)),
        "completed" => ("已完成", egui::Color32::from_rgb(92, 184, 92)),
        _ => (s, egui::Color32::GRAY),
    }
}
