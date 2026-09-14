# 图片合并 PDF 工具 — UI
img2pdf_ui <- function() {
  fluidRow(
    column(5,
      h5(icon("images"), "选择图片（可多选，按顺序合并）"),
      fileInput("i2p_file", NULL, width = "100%",
        accept = c("image/*", ".jpg", ".jpeg", ".png", ".webp", ".heic", ".bmp", ".gif", ".tif", ".tiff"),
        multiple = TRUE,
        buttonLabel = "选择图片", placeholder = "未选择文件"),
      tags$div(style = "font-size:12px; color:#666; margin-top:4px;",
        uiOutput("i2p_file_list")),
      hr(),
      h5(icon("stamp"), "水印设置"),
      textAreaInput("i2p_watermark", "水印内容（每行一条，留空不加水印）",
        value = "", rows = 3,
        placeholder = "例如：\n机密文件\n内部资料"),
      fluidRow(
        column(6, selectInput("i2p_wm_color", "水印颜色（浅色透明）",
          choices = c(
            "浅灰 #CCCCCC" = "#CCCCCC",
            "浅蓝 #AAC9EE" = "#AAC9EE",
            "浅绿 #B8DFC9" = "#B8DFC9",
            "浅红 #F0B9B9" = "#F0B9B9",
            "浅黄 #E8DFAE" = "#E8DFAE",
            "中灰 #999999" = "#999999"
          ), selected = "#CCCCCC")),
        column(6, numericInput("i2p_wm_size", "字号", value = 40, min = 10, max = 200, step = 5))
      ),
      fluidRow(
        column(6, sliderInput("i2p_wm_alpha", "透明度", value = 0.3, min = 0.05, max = 0.8, step = 0.05)),
        column(6, selectInput("i2p_wm_angles", "水印角度（可多选）",
          choices = c(
            "水平 0°" = "0",
            "对角 30°" = "30",
            "对角 45°" = "45",
            "对角 -45°" = "-45",
            "垂直 90°" = "90"
          ), selected = "45", multiple = TRUE))
      ),
      fluidRow(
        column(6, numericInput("i2p_wm_cols", "水平数量（列）", value = 3, min = 1, max = 10, step = 1)),
        column(6, numericInput("i2p_wm_rows", "垂直数量（行）", value = 3, min = 1, max = 10, step = 1))
      ),
      sliderInput("i2p_wm_spacing", "水印间距", value = 0.2, min = 0, max = 0.8, step = 0.05),
      selectInput("i2p_stack", "图片拼接方向",
        choices = c(
          "横向并排（适合横图）" = "horizontal",
          "纵向堆叠（适合竖图）" = "vertical"
        ), selected = "horizontal"),
      hr(),
      tags$button(id = "i2p_generate", type = "button",
        class = "btn btn-primary action-button", disabled = NA,
        style = "width:100%; padding:8px 16px; font-size:14px;",
        list(icon("file-pdf"), " 合并生成 PDF")),
      br(), br(),
      uiOutput("i2p_result")
    ),
    column(7,
      h5(icon("history"), "生成历史"),
      DTOutput("i2p_history_table")
    )
  )
}
