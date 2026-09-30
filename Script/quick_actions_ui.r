# 一键功能 — 首页快捷入口
# 在首页标题右侧提供可展开/收缩的「一键功能」面板
# 按模块分类列出所有单个功能，一页全面展示（无下级）
# 点击行为分两类：
#   navigate —— 跳转到对应模块/页面（填写内容型，停留在目标页面）
#   trigger  —— 直接触发（如测试的「域控」「全部测试」，先切测试页再自动点击按钮）

# 功能清单：module -> 分类图标 -> actions
# 每项 action：name(名称) / type(navigate|trigger) / tab(目标tab) / btn(触发按钮id) / focus(跳转后聚焦的输入框id)
QUICK_ACTIONS <- list(
  list(module = "工单", perm = "工单", icon = "clipboard-list", color = "#337ab7",
    actions = list(
      list(name = "新建工单",     type = "navigate", tab = "工单", focus = "work_order_title"),
      list(name = "快速创建工单", type = "navigate", tab = "工单", focus = "quick_work_order_text"),
      list(name = "批量补工单",   type = "navigate", tab = "工单", focus = "batch_work_order_text")
    )),
  list(module = "项目", perm = "项目", icon = "project-diagram", color = "#5bc0de",
    actions = list(
      list(name = "项目列表", type = "navigate", tab = "项目"),
      list(name = "任务总览", type = "navigate", tab = "项目")
    )),
  list(module = "记事", perm = "记事", icon = "sticky-note", color = "#5cb85c",
    actions = list(
      list(name = "记事列表", type = "navigate", tab = "记事")
    )),
  list(module = "巡检", perm = "巡检", icon = "clipboard-check", color = "#f0ad4e",
    actions = list(
      list(name = "我的任务", type = "navigate", tab = "巡检"),
      list(name = "巡检计划", type = "navigate", tab = "巡检")
    )),
  list(module = "测试（直接执行）", perm = "测试", icon = "network-wired", color = "#d9534f",
    actions = list(
      list(name = "全部测试",     type = "trigger", tab = "测试", btn = "nt_run_all"),
      list(name = "网卡信息",     type = "trigger", tab = "测试", btn = "nt_run_ipconfig"),
      list(name = "Ping",         type = "trigger", tab = "测试", btn = "nt_run_ping"),
      list(name = "DNS 解析",     type = "trigger", tab = "测试", btn = "nt_run_nslookup"),
      list(name = "域控",         type = "trigger", tab = "测试", btn = "nt_run_nltest"),
      list(name = "路由追踪",     type = "trigger", tab = "测试", btn = "nt_run_tracert"),
      list(name = "HTTP 测试",    type = "trigger", tab = "测试", btn = "nt_run_curl"),
      list(name = "文件服务器 50",  type = "trigger", tab = "测试", btn = "nt_run_fileserver1"),
      list(name = "文件服务器 150", type = "trigger", tab = "测试", btn = "nt_run_fileserver2"),
      list(name = "全部邮箱诊断",  type = "trigger", tab = "测试", btn = "nt_run_email_all"),
      list(name = "协同平台前端",  type = "trigger", tab = "测试", btn = "nt_run_app_ecs")
    )),
  list(module = "工具", perm = NULL, icon = "wrench", color = "#9370db",
    actions = list(
      list(name = "文本格式化", type = "navigate", tab = "工具"),
      list(name = "拼音转换",   type = "navigate", tab = "工具"),
      list(name = "记算",       type = "navigate", tab = "工具"),
      list(name = "日期计算",   type = "navigate", tab = "工具")
    )),
  list(module = "总结", perm = "总结", icon = "calendar-day", color = "#17a2b8",
    actions = list(
      list(name = "生成日报", type = "navigate", tab = "总结")
    )),
  list(module = "数据", perm = "数据", icon = "database", color = "#6c757d",
    actions = list(
      list(name = "数据中心", type = "navigate", tab = "数据")
    ))
)

# 一键功能面板 UI（首页标题右侧）
# 可展开/收缩，按模块分类，一页全面展示
quick_actions_ui <- function() {
  tags$div(id = "qa-wrap", style = "display:inline-block; margin-left:12px; vertical-align:middle;",
    # 触发按钮
    tags$button(id = "qa-toggle", type = "button",
      class = "btn btn-primary btn-sm action-button",
      style = "padding:4px 12px; font-weight:bold;",
      list(icon("bolt"), " 一键功能"))
  )
}

# 面板主体（在首页标题下方以块级元素展开，保持在主页内容区域内，宽度跟随容器）
quick_actions_panel <- function() {
  tags$div(id = "qa-panel", style = "display:none; width:100%; margin-top:10px;
    max-height:70vh; overflow-y:auto; background:#f8f9fa; border:1px solid #ddd;
    border-radius:8px; padding:14px;",
    uiOutput("qa_panel_body")
  )
}

# 渲染面板正文（按模块分组的功能网格）
quick_actions_body <- function(actions) {
  if (length(actions) == 0) {
    return(tags$div(style = "color:#999; padding:20px; text-align:center;", "暂无可用功能"))
  }
  tags$div(
    lapply(seq_along(actions), function(i) {
      m <- actions[[i]]
      # 模块分组标题
      tags$div(style = "margin-bottom:10px;",
        tags$div(style = sprintf("font-weight:700; font-size:14px; color:%s; margin-bottom:6px; border-bottom:2px solid %s; padding-bottom:4px;",
          m$color, m$color),
          icon(m$icon), " ", m$module),
        tags$div(style = "display:flex; flex-wrap:wrap; gap:6px;",
          lapply(m$actions, function(a) {
            tags$button(type = "button",
              class = "btn btn-default btn-sm qa-action",
              style = "font-size:12px; padding:4px 10px;",
              `data-type` = a$type,
              `data-tab`  = a$tab,
              `data-btn`  = if (a$type == "trigger") a$btn else "",
              `data-focus`= if (!is.null(a$focus)) a$focus else "",
              a$name)
          })
        )
      )
    })
  )
}
