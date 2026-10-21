# 小程序用户 + VIP 会员管理 - UI

# 统计卡片
mp_stat_card <- function(title, value_id, color = "#337ab7", icon_name = "user") {
  tags$div(
    style = sprintf("border:1px solid %s; border-radius:8px; padding:16px 14px; background:#fff; height:100%%;", color),
    tags$div(style = "display:flex; align-items:center; justify-content:space-between;",
      tags$div(
        tags$div(style = sprintf("color:%s; font-size:12px; font-weight:600;", color), title),
        tags$div(style = "font-size:28px; font-weight:700; color:#333; margin-top:4px;",
          textOutput(value_id, inline = TRUE))
      ),
      tags$div(style = sprintf("font-size:30px; color:%s; opacity:.4;", color), icon(icon_name))
    )
  )
}

# 小程序用户管理页
mp_users_ui <- function() {
  tagList(
    titlePanel("小程序用户管理"),
    p(style = "color:#666; font-size:13px;", "管理小程序端（微信 openid 体系）注册用户，支持增删改查、禁用/启用及 VIP 权益设置。"),
    fluidRow(
      column(3, mp_stat_card("总用户数", "mp_stat_total", "#337ab7", "users")),
      column(3, mp_stat_card("今日新增", "mp_stat_today", "#5cb85c", "user-plus")),
      column(3, mp_stat_card("VIP 用户", "mp_stat_vip", "#f0ad4e", "crown")),
      column(3, mp_stat_card("已禁用", "mp_stat_disabled", "#d9534f", "user-slash"))
    ),
    hr(),
    fluidRow(
      column(12,
        div(style = "display:flex; gap:6px; align-items:center; flex-wrap:wrap;",
          actionButton("mp_user_add", "", icon = icon("plus"), class = "btn-sm btn-success", title = "添加用户"),
          actionButton("mp_user_edit", "", icon = icon("pencil"), class = "btn-sm btn-default", title = "编辑用户"),
          actionButton("mp_user_del", "", icon = icon("trash"), class = "btn-sm btn-danger", title = "删除用户"),
          actionButton("mp_user_act", "", icon = icon("toggle-on"), class = "btn-sm btn-default", title = "禁用/启用"),
          actionButton("mp_user_vip", "", icon = icon("crown"), class = "btn-sm btn-warning", title = "开通/续费VIP"),
          actionButton("mp_refresh", "", icon = icon("sync"), class = "btn-sm btn-info", title = "刷新"),
          div(style = "width:140px;", selectInput("mp_filter_status", NULL,
            choices = c("全部状态" = "all", "正常" = "1", "已禁用" = "0"), width = "100%")),
          div(style = "width:140px;", selectInput("mp_filter_vip", NULL,
            choices = c("全部用户" = "all", "仅VIP" = "vip", "非VIP" = "novip"), width = "100%")),
          div(style = "width:200px;", textInput("mp_search", NULL, placeholder = "搜索昵称/手机/openid...", width = "100%"))
        )
      )
    ),
    br(),
    DTOutput("mp_user_table")
  )
}

# VIP 会员管理页
vip_ui <- function() {
  tagList(
    titlePanel("VIP 会员管理"),
    p(style = "color:#666; font-size:13px;", "管理小程序用户的 VIP 会员开通、续费与到期情况。"),
    fluidRow(
      column(4, mp_stat_card("有效 VIP 数", "vip_stat_total", "#f0ad4e", "crown")),
      column(4, mp_stat_card("30 天内到期", "vip_stat_expiring", "#5bc0de", "hourglass-half")),
      column(4, mp_stat_card("已过期", "vip_stat_expired", "#d9534f", "times-circle"))
    ),
    hr(),
    fluidRow(
      column(12,
        div(style = "display:flex; gap:6px; align-items:center; flex-wrap:wrap;",
          actionButton("vip_add", "", icon = icon("crown"), class = "btn-sm btn-warning", title = "开通VIP"),
          actionButton("vip_renew", "", icon = icon("calendar-plus"), class = "btn-sm btn-success", title = "续费"),
          actionButton("vip_del", "", icon = icon("trash"), class = "btn-sm btn-danger", title = "删除记录"),
          actionButton("vip_refresh", "", icon = icon("sync"), class = "btn-sm btn-info", title = "刷新"),
          div(style = "width:140px;", selectInput("vip_filter_level", NULL,
            choices = c("全部等级" = "all", "VIP1" = "1", "VIP2" = "2", "VIP3" = "3"), width = "100%")),
          div(style = "width:200px;", textInput("vip_search", NULL, placeholder = "搜索用户昵称/手机/openid...", width = "100%"))
        )
      )
    ),
    br(),
    DTOutput("vip_record_table")
  )
}
