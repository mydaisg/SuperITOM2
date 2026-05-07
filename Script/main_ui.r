# 定义主应用界面函数
# 当用户登录成功后，server.R会调用此函数生成主界面
# 这是应用的核心界面结构定义

# 全局声明Shiny UI函数以消除lint警告
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("navbarPage", "tabPanel", "fluidPage", "sidebarLayout", "sidebarPanel",
                          "mainPanel", "textInput", "passwordInput", "selectInput", "actionButton",
                          "DTOutput", "plotlyOutput", "verbatimTextOutput", "icon", "tagList",
                          "tags", "div", "h2", "h3", "h4", "p", "ul", "li", "titlePanel",
                          "fluidRow", "column", "hidden", "textAreaInput"))
}

# 显式声明passwordInput函数
passwordInput <- shiny::passwordInput

# 加载标准化模块
source("Script/std_computer.r")

# 加载测试模块（网络巡检）
source("Script/network_test.r")

# 加载项目管理模块UI
source("Script/project_ui.r")

# 加载日报模块
source("Script/daily_report.r")

main_ui <- function() {
  # 读取字体大小配置
  table_font_size <- config_get_value("table_font_size", "13")
  input_font_size <- config_get_value("input_font_size", "13")

  # 创建导航栏页面
  # navbarPage是Shiny中创建带有标签页的导航栏界面的函数
  navbarPage(
    title = "SuperITOM2",  # 应用标题
    theme = shinytheme("cosmo"),  # 使用cosmo主题，使界面更美观
    
    # 首页标签页
    tabPanel(
      "首页",  # 标签页标题
      icon = icon("home"),  # 标签页图标
      fluidPage(
        titlePanel("欢迎使用 Super ITOM 2"),
        br(),
        fluidRow(
          column(6,
            div(style = "border:1px solid #ddd; border-radius:8px; padding:16px; margin-bottom:16px;",
              h4(style = "margin-top:0; color:#337ab7; border-bottom:2px solid #337ab7; padding-bottom:8px;", "我的项目"),
              uiOutput("home_my_projects")
            )
          ),
          column(6,
            div(style = "border:1px solid #ddd; border-radius:8px; padding:16px; margin-bottom:16px;",
              h4(style = "margin-top:0; color:#5bc0de; border-bottom:2px solid #5bc0de; padding-bottom:8px;", "我的工单"),
              uiOutput("home_my_work_orders")
            )
          )
        ),
        fluidRow(
          column(12,
            div(style = "border:1px solid #ddd; border-radius:8px; padding:16px;",
              h4(style = "margin-top:0; color:#5cb85c; border-bottom:2px solid #5cb85c; padding-bottom:8px;", "我的任务"),
              uiOutput("home_my_tasks")
            )
          )
        )
      )
    ),
    
    # 项目管理标签页（放在工单前面）
    tabPanel(
      "项目",
      icon = icon("project-diagram"),
      project_ui()
    ),

    # 工单标签页
    tabPanel(
      "工单",
      icon = icon("clipboard-list"),
      fluidPage(
        # 自定义导航栏选中态样式 + admin菜单控制 + 字体大小配置
        tags$head(
          tags$style(HTML(sprintf("
            /* 全局列表字体大小配置 */
            .dataTables_wrapper table.dataTable tbody td {
              font-size: %spx !important;
            }
            .dataTables_wrapper table.dataTable thead th {
              font-size: %spx !important;
            }
            /* 输入框和选择框字体大小配置 */
            .form-control, .selectize-input, .selectize-dropdown {
              font-size: %spx !important;
            }
            .navbar-default .navbar-nav > .active > a,
            .navbar-default .navbar-nav > .active > a:hover,
            .navbar-default .navbar-nav > .active > a:focus {
              background-color: #337ab7 !important;
              color: #fff !important;
              font-weight: bold;
              border: none;
              box-shadow: 0 -3px 0 #1a5276 inset;
            }
            .navbar-default .navbar-nav > li > a:hover,
            .navbar-default .navbar-nav > li > a:focus {
              background-color: #d6e9f8 !important;
            }
            /* 默认隐藏管理菜单，JS根据角色控制显示 */
            .admin-menu-item { display: none !important; }
            body.admin-user .admin-menu-item { display: block !important; }
          ", table_font_size, table_font_size, input_font_size))),
          tags$script(HTML("
            $(document).on('shiny:connected', function(event) {
              // 给管理菜单添加标识class
              $('.navbar-nav > li.dropdown').each(function() {
                var link = $(this).find('a.dropdown-toggle');
                if (link.length && link.text().indexOf('管理') >= 0) {
                  $(this).addClass('admin-menu-item');
                }
              });
              Shiny.addCustomMessageHandler('toggleAdminMenu', function(message) {
                if (message.show) {
                  $('body').addClass('admin-user');
                } else {
                  $('body').removeClass('admin-user');
                }
              });
            });
          "))
        ),
        # 工单统计数据（小色块，一行显示）
        fluidRow(
          column(12,
            div(style = "margin-bottom: 15px;",
              fluidRow(
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 12px 8px; margin-bottom: 5px;",
                  div(style = "font-size: 14px; color: #666; font-weight: 500;", "总工单"),
                  div(style = "font-size: 26px; font-weight: bold; color: #333;", textOutput("wo_stat_total"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 12px 8px; margin-bottom: 5px; background: #f0ad4e; color: white;",
                  div(style = "font-size: 14px; font-weight: 500;", "待处理"),
                  div(style = "font-size: 26px; font-weight: bold;", textOutput("wo_stat_pending"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 12px 8px; margin-bottom: 5px; background: #5bc0de; color: white;",
                  div(style = "font-size: 14px; font-weight: 500;", "已派发"),
                  div(style = "font-size: 26px; font-weight: bold;", textOutput("wo_stat_assigned"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 12px 8px; margin-bottom: 5px; background: #337ab7; color: white;",
                  div(style = "font-size: 14px; font-weight: 500;", "处理中"),
                  div(style = "font-size: 26px; font-weight: bold;", textOutput("wo_stat_processing"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 12px 8px; margin-bottom: 5px; background: #5cb85c; color: white;",
                  div(style = "font-size: 14px; font-weight: 500;", "已完成"),
                  div(style = "font-size: 26px; font-weight: bold;", textOutput("wo_stat_completed"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 12px 8px; margin-bottom: 5px; background: #777; color: white;",
                  div(style = "font-size: 14px; font-weight: 500;", "已关闭"),
                  div(style = "font-size: 26px; font-weight: bold;", textOutput("wo_stat_closed"))
                ))
              )
            )
          )
        ),
        # 工单列表（占满宽度）
        fluidRow(
          column(12,
            tabsetPanel(
              tabPanel("工单列表",
                br(),
                fluidRow(
                  column(2, uiOutput("work_order_status_filter_ui")),
                  column(1, div(style = "margin-top: 20px;", actionButton("refresh_work_orders", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;"))),
                  column(1, div(style = "margin-top: 20px;", actionButton("show_create_work_order", "新建工单", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;")))
                ),
                DTOutput("work_order_table")
              ),
              tabPanel("工单派发",
                br(),
                conditionalPanel(
                  condition = "output.work_order_selected",
                  wellPanel(
                    h4("选中工单信息"),
                    verbatimTextOutput("selected_work_order_info"),
                    hr(),
                    h4("派发操作"),
                    selectInput("work_order_assignee", "选择处理人", choices = NULL),
                    actionButton("assign_work_order", "派发工单", class = "btn-primary")
                  )
                ),
                conditionalPanel(
                  condition = "!output.work_order_selected",
                  div(class = "alert alert-info", HTML("请先在<b>工单列表</b>中点击选择一行"))
                )
              ),
              tabPanel("工单处理",
                br(),
                conditionalPanel(
                  condition = "output.work_order_selected",
                  wellPanel(
                    h4("选中工单信息"),
                    verbatimTextOutput("selected_work_order_info2"),
                    hr(),
                    h4("处理操作"),
                    conditionalPanel(
                      condition = "output.work_order_can_edit",
                      actionButton("edit_work_order_btn", "编辑工单", class = "btn-info btn-block"),
                      br()
                    ),
                    conditionalPanel(
                      condition = "output.work_order_can_start",
                      actionButton("start_handle_work_order", "开始处理", class = "btn-primary btn-block"),
                      br()
                    ),
                    conditionalPanel(
                      condition = "output.work_order_can_complete",
                      textAreaInput("work_order_resolution", "解决方案/处理结果", rows = 4),
                      actionButton("complete_work_order", "完成工单", class = "btn-success btn-block"),
                      br()
                    ),
                    conditionalPanel(
                      condition = "output.work_order_can_close",
                      selectInput("work_order_close_reason", "关闭原因（必选）",
                                 choices = c("请选择关闭原因" = "", "已处理和交付" = "已处理和交付", "无法处理关闭" = "无法处理关闭")),
                      actionButton("close_work_order", "关闭工单", class = "btn-warning btn-block"),
                      br()
                    ),
                    actionButton("delete_work_order_btn", "删除工单", class = "btn-danger btn-block")
                  )
                ),
                conditionalPanel(
                  condition = "!output.work_order_selected",
                  div(class = "alert alert-info", HTML("请先在<b>工单列表</b>中点击选择一行"))
                ),
                br(),
                # 评论区域
                conditionalPanel(
                  condition = "output.work_order_selected",
                  wellPanel(
                    h4("工单评论/备注"),
                    textAreaInput("work_order_comment", "添加评论", rows = 2),
                    actionButton("add_work_order_comment", "发表评论", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;"),
                    hr(),
                    h5("历史评论"),
                    uiOutput("work_order_comments_ui")
                  )
                )
              )
            )
          )
        ),

        hr(),

        # 编辑工单区域（默认隐藏，点击编辑后显示）
        conditionalPanel(
          condition = "output.work_order_edit_mode",
          fluidRow(
            column(12,
              wellPanel(
                h4("编辑工单"),
                fluidRow(
                  column(4, textInput("edit_work_order_title", "工单标题")),
                  column(2, uiOutput("edit_work_order_priority_ui")),
                  column(3, uiOutput("edit_work_order_category_ui")),
                  column(3, div(style = "margin-top: 20px;",
                    actionButton("save_edit_work_order", "保存修改", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px; margin-right: 5px;"),
                    actionButton("cancel_edit_work_order", "取消", class = "btn-default", style = "padding: 4px 10px; font-size: 12px;")
                  ))
                ),
                fluidRow(
                  column(12, textAreaInput("edit_work_order_description", "工单描述", rows = 2))
                )
              )
            )
          )
        ),

        # 第四行：创建工单（放在最下面）
        fluidRow(
          column(12,
            wellPanel(
              h4("创建新工单"),
              fluidRow(
                column(4, textInput("work_order_title", "工单标题")),
                column(2, uiOutput("work_order_priority_ui")),
                column(3, uiOutput("work_order_category_ui")),
                column(1, div(style = "margin-top: 20px;", actionButton("add_work_order", "创建工单", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;")))
              ),
              fluidRow(
                column(12, textAreaInput("work_order_description", "工单描述", rows = 2))
              )
            )
          )
        )
      )
    ),

    # 日报标签页（放在工单后面）
    tabPanel(
      "日报",
      icon = icon("calendar-day"),
      daily_report_ui()
    ),

    # 收集器标签页
    tabPanel(
      "收集器",
      icon = icon("download"),  # 收集器图标
      fluidPage(
        titlePanel("信息收集器"),
        sidebarLayout(
          sidebarPanel(
            textInput("collector_name", "收集器名称"),
            selectInput("collector_type", "收集器类型", choices = c("系统信息", "网络信息", "应用信息", "数据库信息")),
            textAreaInput("collector_config", "收集器配置"),
            actionButton("add_collector", "添加收集器", class = "btn-primary"),
            br(), br(),
            actionButton("refresh_collectors", "刷新收集器", class = "btn-info")
          ),
          mainPanel(
            DTOutput("collector_table")
          )
        )
      )
    ),
    
    # 巡检标签页
    tabPanel(
      "巡检",
      icon = icon("search"),  # 巡检图标
      fluidPage(
        titlePanel("巡检管理"),
        sidebarLayout(
          sidebarPanel(
            textInput("inspection_name", "巡检名称"),
            selectInput("inspection_type", "巡检类型", choices = c("系统巡检", "网络巡检", "安全巡检", "应用巡检")),
            textAreaInput("inspection_schedule", "巡检计划"),
            actionButton("add_inspection", "创建巡检", class = "btn-primary"),
            br(), br(),
            actionButton("refresh_inspections", "刷新巡检", class = "btn-info")
          ),
          mainPanel(
            DTOutput("inspection_table")
          )
        )
      )
    ),
    
    # 测试标签页（网络巡检）
    tabPanel(
      "测试",
      icon = icon("network-wired"),
      network_test_ui()
    ),

    # 标准化标签页
    tabPanel(
      "标准化",
      icon = icon("cogs"),  # 齿轮图标
      std_ui()
    ),
    
    # 数据管理标签页
    tabPanel(
      "数据",
      icon = icon("database"),  # 数据库图标
      fluidPage(
        titlePanel("数据"),
        sidebarLayout(  # 创建侧边栏布局
          sidebarPanel(  # 侧边栏面板
            textInput("data_name", "数据名称"),  # 文本输入框
            selectInput("data_type", "数据类型", choices = c("服务器", "网络", "应用", "数据库", "其他")),  # 下拉选择框
            textAreaInput("data_value", "数据值"),  # 文本区域输入框
            actionButton("add_data", "添加数据", class = "btn-primary"),  # 主要操作按钮
            br(), br(),
            actionButton("refresh_data", "刷新数据", class = "btn-info")  # 信息类按钮
          ),
          mainPanel(  # 主面板
            DTOutput("data_table")  # 数据表格输出
          )
        )
      )
    ),
    
    # 模型训练标签页
    tabPanel(
      "模型",
      icon = icon("cogs"),  # 齿轮图标
      fluidPage(
        titlePanel("模型"),
        sidebarLayout(
          sidebarPanel(
            textInput("model_name", "模型名称"),
            selectInput("model_type", "模型类型", choices = c("线性回归", "决策树", "随机森林", "神经网络", "SVM")),
            textAreaInput("model_params", "模型参数"),
            actionButton("train_model", "训练模型", class = "btn-primary"),
            br(), br(),
            actionButton("refresh_models", "刷新模型", class = "btn-info")
          ),
          mainPanel(
            h4("模型列表"),
            DTOutput("model_table"),  # 模型表格
            br(),
            h4("训练结果"),
            verbatimTextOutput("training_result")  # 训练结果文本输出
          )
        )
      )
    ),
    
    # 数据可视化标签页
    tabPanel(
      "可视化",
      icon = icon("chart-line"),  # 图表图标
      fluidPage(
        titlePanel("数据可视化"),
        sidebarLayout(
          sidebarPanel(
            selectInput("viz_type", "图表类型", choices = c("折线图", "柱状图", "散点图", "饼图", "热力图")),
            selectInput("viz_data", "数据源", choices = c("ITOM数据", "模型数据")),
            actionButton("generate_viz", "生成图表", class = "btn-primary")
          ),
          mainPanel(
            plotlyOutput("viz_plot")  # Plotly交互式图表输出
          )
        )
      )
    ),
    
    # 管理菜单（admin专用，JS控制显示/隐藏）
    navbarMenu(
      "管理",
      icon = icon("tools"),
      tabPanel(
        "用户管理",
        icon = icon("users"),
        fluidPage(
          titlePanel("用户管理"),
          sidebarLayout(
            sidebarPanel(
              tags$div(textInput("selected_user_id", "", value = ""), style = "display: none;"),
              textInput("username", "用户名"),
              textInput("display_name", "显示名称"),
              passwordInput("password", "密码"),
              selectInput("role", "角色", choices = c("user", "admin")),
              actionButton("add_user", "添加用户", class = "btn-primary"),
              br(), br(),
              actionButton("update_user", "修改账号", class = "btn-warning"),
              br(), br(),
              actionButton("toggle_active_user", "禁用/启用用户", class = "btn-danger"),
              br(), br(),
              actionButton("refresh_users", "刷新用户", class = "btn-info")
            ),
            mainPanel(
              DTOutput("user_table")
            )
          )
        )
      ),
      tabPanel(
        "系统设置",
        icon = icon("cogs"),
        fluidPage(
          titlePanel("系统设置"),
          # 字体大小快捷配置区域
          wellPanel(
            h4("界面字体大小"),
            p(style = "color:#666; font-size:12px;", "调整列表表格和输入框的字体大小（保存后刷新页面生效）"),
            fluidRow(
              column(3, numericInput("cfg_table_font_size", "列表表格字体(px)", value = 13, min = 10, max = 20, step = 1)),
              column(3, numericInput("cfg_input_font_size", "输入框/选择框字体(px)", value = 13, min = 10, max = 20, step = 1)),
              column(3, div(style = "margin-top:25px;",
                actionButton("save_font_config", "保存字体设置", class = "btn-primary btn-sm", icon = icon("save"))))
            )
          ),
          hr(),
          # 通用配置管理
          sidebarLayout(
            sidebarPanel(
              textInput("config_key", "配置键"),
              textInput("config_value", "配置值"),
              textInput("config_desc", "描述"),
              actionButton("add_config", "添加配置", class = "btn-primary"),
              br(), br(),
              actionButton("refresh_config", "刷新配置", class = "btn-info")
            ),
            mainPanel(
              DTOutput("config_table")
            )
          )
        )
      ),
      tabPanel(
        "选项配置",
        icon = icon("sliders-h"),
        fluidPage(
          titlePanel("选项配置管理"),
          p("管理项目状态、优先级等下拉选项。修改后立即生效，所有界面自动使用最新配置。"),
          fluidRow(
            column(3, selectInput("co_category", "配置类别",
              choices = c(
                "项目管理" = list("项目状态"="project_status", "项目优先级"="project_priority",
                                  "阶段状态"="phase_status", "工作包状态"="wp_status",
                                  "任务状态"="task_status", "任务优先级"="task_priority"),
                "工单管理" = list("工单状态"="work_order_status", "工单优先级"="work_order_priority",
                                  "工单分类"="work_order_category")
              ))),
            column(2, actionButton("co_refresh", "刷新列表", class = "btn-info btn-sm", style = "margin-top:24px;"))
          ),
          DTOutput("co_option_table"),
          hr(),
          wellPanel(
            h4("添加/编辑选项"),
            fluidRow(
              column(2, textInput("co_value", "选项值")),
              column(2, textInput("co_label", "显示名称")),
              column(2, textInput("co_color", "颜色(HEX)", placeholder = "#337ab7")),
              column(1, numericInput("co_sort", "排序", value = 0, min = 0)),
              column(2, selectInput("co_default", "默认", choices = c("否"="0", "是"="1"))),
              column(1, div(style = "margin-top:20px;",
                actionButton("co_add", "添加", class = "btn-primary btn-sm"))),
              column(2, div(style = "margin-top:20px;",
                actionButton("co_save_edit", "保存修改", class = "btn-warning btn-sm")))
            )
          )
        )
      ),
      tabPanel(
        "GitHub",
        icon = icon("github"),
        fluidPage(
          titlePanel("GitHub 自动提交"),
          br(),
          sidebarLayout(
            sidebarPanel(
              h4("Git 操作"),
              br(),
              textInput("commit_message", "提交信息", value = "Commit from ITOM2"),
              br(), br(),
              actionButton("github_autosubmit", "提交所有更改", icon = icon("upload"), class = "btn-primary"),
              br(), br(),
              actionButton("github_check_status", "查看 Git 状态", icon = icon("info-circle"), class = "btn-info"),
              br(), br(),
              actionButton("github_pull", "拉取最新代码", icon = icon("download"), class = "btn-warning")
            ),
            mainPanel(
              h4("Git 状态"),
              verbatimTextOutput("git_status"),
              br(),
              h4("提交结果"),
              verbatimTextOutput("git_result")
            )
          )
        )
      )
    ),

    # 添加退出登录标签页
    tabPanel(
      "退出",
      icon = icon("sign-out-alt"),
      fluidPage(
        # titlePanel("退出登录"),
        fluidRow(
          column(12,
            div(
              style = "text-align: center; margin-top: 50px;",
              h3("您确定要退出登录吗？"),
              br(),
              actionButton("logout", "确认退出", class = "btn-danger")
            )
          )
        )
      )
    )
  )
}

# 定义admin专用标签页
admin_tabs_ui <- function() {
  tagList(
    # 用户管理标签页
    tabPanel(
      "用户管理",
      icon = icon("users"),
      fluidPage(
        titlePanel("用户管理"),
        sidebarLayout(
          sidebarPanel(
            tags$div(textInput("selected_user_id", "", value = ""), style = "display: none;"),
            textInput("username", "用户名"),
            passwordInput("password", "密码"),
            selectInput("role", "角色", choices = c("user", "admin")),
            actionButton("add_user", "添加用户", class = "btn-primary"),
            br(), br(),
            actionButton("update_user", "修改账号", class = "btn-warning"),
            br(), br(),
            actionButton("toggle_active_user", "禁用/启用用户", class = "btn-danger"),
            br(), br(),
            actionButton("refresh_users", "刷新用户", class = "btn-info")
          ),
          mainPanel(
            DTOutput("user_table")
          )
        )
      )
    ),
    
    # 系统设置标签页
    tabPanel(
      "系统设置",
      icon = icon("cogs"),
      fluidPage(
        titlePanel("系统设置"),
        sidebarLayout(
          sidebarPanel(
            textInput("config_key", "配置键"),
            textInput("config_value", "配置值"),
            textInput("config_desc", "描述"),
            actionButton("add_config", "添加配置", class = "btn-primary"),
            br(), br(),
            actionButton("refresh_config", "刷新配置", class = "btn-info")
          ),
          mainPanel(
            DTOutput("config_table")
          )
        )
      )
    ),
    
    # GitHub自动提交标签页
    tabPanel(
      "GitHub",
      icon = icon("github"),
      fluidPage(
        titlePanel("GitHub 自动提交"),
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Git 操作"),
            br(),
            textInput("commit_message", "提交信息", value = "Commit from ITOM2"),
            br(), br(),
            actionButton("github_autosubmit", "提交所有更改", icon = icon("upload"), class = "btn-primary"),
            br(), br(),
            actionButton("github_check_status", "查看 Git 状态", icon = icon("info-circle"), class = "btn-info"),
            br(), br(),
            actionButton("github_pull", "拉取最新代码", icon = icon("download"), class = "btn-warning")
          ),
          mainPanel(
            h4("Git 状态"),
            verbatimTextOutput("git_status"),
            br(),
            h4("提交结果"),
            verbatimTextOutput("git_result")
          )
        )
      )
    )
  )
}
