# AI 模块 Server
ai_server <- function(input, output, session, rv) {

  # 全网搜索：打开多个搜索引擎
  observeEvent(input$ai_search_btn, {
    req(rv$logged_in)
    kw <- trimws(input$ai_search_kw %||% "")
    if (nchar(kw) == 0) { showNotification("请输入搜索内容", type = "warning"); return() }
    ekw <- URLencode(kw)
    session$sendCustomMessage("runjs", sprintf("
      document.getElementById('ai_google').src='https://www.google.com/search?q=%s';
      document.getElementById('ai_bing').src='https://www.bing.com/search?q=%s';
      document.getElementById('ai_baidu').src='https://www.baidu.com/s?wd=%s';
      document.getElementById('ai_ddg').src='https://duckduckgo.com/?q=%s';
    ", ekw, ekw, ekw, ekw))
  })

  # 全网AI：打开多个AI对话
  observeEvent(input$ai_chat_btn, {
    req(rv$logged_in)
    txt <- trimws(input$ai_chat_input %||% "")
    if (nchar(txt) == 0) { showNotification("请输入问题", type = "warning"); return() }
    etxt <- URLencode(txt)
    session$sendCustomMessage("runjs", sprintf("
      document.getElementById('ai_chatgpt').src='https://chat.openai.com/';
      document.getElementById('ai_claude').src='https://claude.ai/new';
      document.getElementById('ai_deepseek').src='https://chat.deepseek.com/';
    "))
    showNotification("已加载 AI 对话窗口，请在各窗口内输入问题", type = "message", duration = 5)
  })
}
