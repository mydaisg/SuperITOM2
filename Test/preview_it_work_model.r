source("global.R")
source("Script/it_work_model.r")

html <- as.character(it_work_model_mermaid())

page <- paste0(
  '<!DOCTYPE html><html lang="zh-CN"><head><meta charset="UTF-8">',
  '<script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>',
  '<style>body{font-family:-apple-system,"Microsoft YaHei",sans-serif;background:#fff;margin:20px;padding:0}</style>',
  '</head><body>', html, '</body></html>'
)

writeLines(page, "Test/it_work_model_preview.html")
cat("Preview saved\n")
