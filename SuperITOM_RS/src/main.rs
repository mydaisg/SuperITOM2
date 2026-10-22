// 发布版隐藏控制台窗口（仅 Windows）
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app;
mod dashboard;
mod db;
mod devlogs;
mod fonts;
mod login;
mod models;
mod notes;
mod solutions;
mod workorders;

use app::SuperItomApp;
use eframe::egui;

fn main() -> eframe::Result {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([920.0, 680.0])
            .with_min_inner_size([720.0, 520.0])
            .with_title("SuperITOM 运维工作台"),
        ..Default::default()
    };

    eframe::run_native(
        "SuperITOM",
        options,
        Box::new(|cc| Ok(Box::new(SuperItomApp::new(cc)))),
    )
}
