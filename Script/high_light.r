# 语法高亮模块
# 加载必要的包
library(shiny)
# UI部分 - 提供语法高亮所需的资源
high_light_ui <- function() {
  tags$head(
    # 添加自定义语法高亮样式
    tags$style(HTML('/* GitHub风格的语法高亮样式 */
pre {
  background-color: #f6f8fa;
  border-radius: 3px;
  padding: 16px;
  overflow-x: auto;
}

code {
  font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
  font-size: 12px;
  line-height: 1.5;
}

/* PowerShell语法高亮样式 */
.keyword {
  color: #d73a49;
  font-weight: 600;
}

.string {
  color: #22863a;
  font-weight: 500;
}

.comment {
  color: #6a737d;
  font-style: italic;
}

.variable {
  color: #e36209;
}

.function {
  color: #6f42c1;
}

.number {
  color: #005cc5;
}

.bracket {
  color: #005cc5;
  font-weight: 500;
}

.square-bracket {
  color: #6f42c1;
  font-weight: 500;
}

.operator {
  font-weight: 600;
}

.redirect {
  color: #d73a49;
  font-weight: 600;
}

.command {
  color: #005cc5;
  font-weight: 600;
}'))
  )
}

# 服务器端函数 - 生成带语法高亮的代码显示
generate_highlighted_code <- function(code_content, language = "powershell") {
  # 对PowerShell代码进行简单的语法高亮处理
  if (language == "powershell") {
    # 处理PowerShell关键字
    keywords <- c('function', 'param', 'if', 'else', 'foreach', 'for', 'while', 'do', 'try', 'catch', 'finally', 'return', 'exit', 'break', 'continue', 'switch', 'case', 'default')
    for (keyword in keywords) {
      pattern <- paste0('\\b', keyword, '\\b')
      code_content <- gsub(pattern, paste0('<span class="keyword">', keyword, '</span>'), code_content)
    }
    
    # 处理PowerShell命令（使用正则表达式匹配动词-名词格式）
    # PowerShell命令通常遵循 Verb-Noun 格式，如 Get-Content, Write-Output 等
    code_content <- gsub('\\b([A-Z][a-z]+-[A-Z][a-zA-Z0-9]+)\\b', '<span class="command">\\1</span>', code_content)
    
    # 处理PowerShell变量（$开头）
    code_content <- gsub('\\$([a-zA-Z_][a-zA-Z0-9_]*)', '<span class="variable">$\\1</span>', code_content)
  }
  
  # 生成包含代码的HTML
  script_html <- paste(
    '<div id="script-container">',
    '<pre><code>',
    code_content,
    '</code></pre>',
    '</div>',
    sep = ''
  )
  
  return(HTML(script_html))
}
