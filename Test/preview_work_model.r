source("global.R")
source("Script/work_model.r")

html <- as.character(work_model_mermaid("IT部"))

# 修复被转义的 Mermaid 箭头
html <- gsub("&gt;", ">", html, fixed = TRUE)
html <- gsub("&lt;", "<", html, fixed = TRUE)
html <- gsub("&amp;", "&", html, fixed = TRUE)

page <- paste0(
  '<!DOCTYPE html><html lang="zh-CN"><head><meta charset="UTF-8">',
  '<script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>',
  '<style>body{font-family:-apple-system,"Microsoft YaHei",sans-serif;background:#fff;margin:20px;padding:0}</style>',
  '</head><body>', html, '</body></html>'
)

writeLines(page, "Test/work_model_preview.html", useBytes = TRUE)
cat("Preview saved\n")
