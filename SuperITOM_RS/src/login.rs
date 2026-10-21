//! 登录界面

use eframe::egui;

use crate::app::SuperItomApp;

pub fn show(app: &mut SuperItomApp, ui: &mut egui::Ui) {
    ui.vertical_centered(|ui| {
        ui.add_space(60.0);
        ui.label(
            egui::RichText::new("SuperITOM 运维管理系统")
                .size(28.0)
                .strong(),
        );
        ui.add_space(6.0);
        ui.label(egui::RichText::new("LVCC ITOM 登录").color(egui::Color32::GRAY));
        ui.add_space(36.0);

        egui::Frame::group(ui.style()).show(ui, |ui| {
            ui.set_width(340.0);
            ui.add_space(12.0);

            ui.label("用户名");
            ui.add(
                egui::TextEdit::singleline(&mut app.username)
                    .hint_text("请输入用户名")
                    .desired_width(f32::INFINITY),
            );

            ui.add_space(10.0);
            ui.label("密码");
            let resp = ui.add(
                egui::TextEdit::singleline(&mut app.password)
                    .password(true)
                    .hint_text("请输入密码")
                    .desired_width(f32::INFINITY),
            );

            ui.add_space(16.0);
            let btn = egui::Button::new("登 录").min_size(egui::vec2(340.0, 36.0));
            let clicked = ui.add(btn).clicked();
            // 密码框按回车提交
            let enter = resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter));
            if clicked || enter {
                app.do_login();
            }

            ui.add_space(10.0);
            if let Some(err) = &app.login_error {
                ui.colored_label(egui::Color32::RED, err);
            }
            ui.add_space(12.0);
        });

        ui.add_space(16.0);
        ui.label(
            egui::RichText::new(format!("数据库: {}", app.db_path))
                .small()
                .color(egui::Color32::GRAY),
        );
    });
}
