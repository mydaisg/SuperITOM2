//! 加载系统中文字体，避免界面中文显示为方块（egui 默认字体不含 CJK）

use eframe::egui;

const FONT_CANDIDATES: &[&str] = &[
    "C:\\Windows\\Fonts\\msyh.ttc",   // 微软雅黑
    "C:\\Windows\\Fonts\\simhei.ttf", // 黑体
    "C:\\Windows\\Fonts\\simsun.ttc", // 宋体
    "C:\\Windows\\Fonts\\Deng.ttf",   // 等线
];

pub fn install_fonts(ctx: &egui::Context) {
    let mut fonts = egui::FontDefinitions::default();

    for path in FONT_CANDIDATES {
        if let Ok(bytes) = std::fs::read(path) {
            fonts
                .font_data
                .insert("chinese".to_owned(), egui::FontData::from_owned(bytes).into());
            // 插入到 Proportional 首位，作为首选字体
            if let Some(family) = fonts.families.get_mut(&egui::FontFamily::Proportional) {
                family.insert(0, "chinese".to_owned());
            }
            if let Some(family) = fonts.families.get_mut(&egui::FontFamily::Monospace) {
                family.push("chinese".to_owned());
            }
            break;
        }
    }

    ctx.set_fonts(fonts);
}
