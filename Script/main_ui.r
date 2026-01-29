# 定义主应用界面函数
# 当用户登录成功后，server.R会调用此函数生成主界面
# 这是应用的核心界面结构定义

# 全局声明Shiny UI函数以消除lint警告
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("navbarPage", "tabPanel", "fluidPage", "sidebarLayout", "sidebarPanel",
                          "mainPanel", "textInput", "passwordInput", "selectInput", "actionButton",
                          "DTOutput", "plotlyOutput", "verbatimTextOutput", "icon", "tagList",
                          "tags", "div", "h2", "h3", "h4", "p", "ul", "li", "titlePanel",
                          "fluidRow", "column", "hidden"))
}

# 显式声明passwordInput函数
passwordInput <- shiny::passwordInput

# 加载标准化模块
source("Script/std_computer.r")

main_ui <- function() {
  # 创建导航栏页面
  # navbarPage是Shiny中创建带有标签页的导航栏界面的函数
  navbarPage(
    title = "SuperITOM2",  # 应用标题
    theme = shinytheme("cosmo"),  # 使用cosmo主题，使界面更美观
    
    # 首页标签页
    tabPanel(
      "首页",  # 标签页标题
      icon = icon("home"),  # 标签页图标
      fluidPage(  # 创建流体布局页面
        titlePanel("欢迎使用 SuperITOM2"),  # 页面标题
        br(),  # 换行
        fluidRow(  # 创建流体行
          column(12,  # 创建12列宽度的列
            h3("系统简介"),  # 三级标题
            p("SuperITOM2 是一个综合性的IT运维管理系统，提供标准化作业、远程批量自动作业、作业记录泛应用、数据管理、模型训练、可视化分析等功能。"),  # 段落文本
            br(),
            h4("主要功能："),  # 四级标题
            tags$ul(  # 创建无序列表
              tags$li("标准化：远程对新计算机进行标准化配置"), 
              tags$li("数据管理：管理IT运维数据"),  # 列表项
              tags$li("模型训练：训练预测模型"),
              tags$li("可视化：数据可视化分析"),
              tags$li("用户管理：管理系统用户"),
              tags$li("系统设置：配置系统参数"),
               tags$li("作业自动化：通过ITOM实现客户端标准化作业远程化和自动化、日常IT服务远程化和脚本化、作业记录自动按模板生成日报消息/交付消息/写入日志")
            )
          )
        )
      )
    ),
    
    # 标准化标签页
    tabPanel(
      "标准化",
      icon = icon("cogs"),  # 齿轮图标
      std_ui()
    ),
    
    # 数据管理标签页
    tabPanel(
      "数据管理",
      icon = icon("database"),  # 数据库图标
      fluidPage(
        titlePanel("数据管理"),
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
      "模型训练",
      icon = icon("cogs"),  # 齿轮图标
      fluidPage(
        titlePanel("模型训练"),
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
    
    # 用户管理标签页
    tabPanel(
      "用户管理",
      icon = icon("users"),  # 用户图标
      fluidPage(
        titlePanel("用户管理"),
        sidebarLayout(
          sidebarPanel(
            tags$div(textInput("selected_user_id", "", value = ""), style = "display: none;"),
            textInput("username", "用户名"),
            passwordInput("password", "密码"),  # 密码输入框（隐藏输入）
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
            DTOutput("user_table")  # 用户表格
          )
        )
      )
    ),
    
    # 系统设置标签页
    tabPanel(
      "系统设置",
      icon = icon("cogs"),  # 设置图标
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
            DTOutput("config_table")  # 配置表格
          )
        )
      )
    ),
    
    # GitHub自动提交标签页
    tabPanel(
      "GitHub",
      icon = icon("github"),  # GitHub图标
      fluidPage(
        titlePanel("GitHub 自动提交"),
        br(),
        sidebarLayout(
          sidebarPanel(
            h4("Git 操作"),
            br(),
            textInput("commit_message", "提交信息", value = "Commit from ITOM2"),  # 提交信息输入框
            br(), br(),
            actionButton("github_autosubmit", "提交所有更改", icon = icon("upload"), class = "btn-primary"),  # 提交按钮
            br(), br(),
            actionButton("github_check_status", "查看 Git 状态", icon = icon("info-circle"), class = "btn-info"),  # 查看状态按钮
            br(), br(),
            actionButton("github_pull", "拉取最新代码", icon = icon("download"), class = "btn-warning")  # 拉取代码按钮
          ),
          mainPanel(
            h4("操作结果"),
            verbatimTextOutput("github_output")  # Git操作结果输出
          )
        )
      )
    ),
    
    # 退出登录标签页
    # 这是一个特殊的标签页，只显示退出按钮
    tabPanel(
      "",  # 空标题，只显示图标
      icon = icon("sign-out"),  # 退出图标
      # 创建退出按钮，使用btn-link样式使其看起来像链接
      # 并通过CSS样式去除背景、边框和内边距，使其融入导航栏
      actionButton("logout", "", class = "btn-link", style = "background: none; border: none; padding: 0;")
    )
  )
}

# 界面设计原理总结：
# 1. 模块化设计：使用函数封装界面逻辑，使代码更清晰
# 2. 响应式布局：使用fluidPage、fluidRow、column等创建自适应布局
# 3. 组件化结构：
#    - navbarPage：创建顶层导航栏
#    - tabPanel：创建各个功能标签页
#    - sidebarLayout：创建侧边栏+主面板的布局
#    - 各种输入组件：textInput、selectInput等
#    - 各种输出组件：DTOutput、plotlyOutput等
# 4. 主题应用：使用shinytheme添加美观的界面主题
# 5. 图标使用：通过icon函数添加直观的图标，提升用户体验
# 6. 样式定制：通过class和style参数自定义组件样式

# 与server.R的交互原理：
# - 界面中的输入组件（如actionButton、textInput）会生成对应的input$对象
# - server.R中的observeEvent函数会监听这些input$对象的变化
# - 当用户操作界面组件时，会触发server.R中的相应逻辑
# - server.R通过output$对象将处理结果发送回界面显示
# - 这种双向通信机制是Shiny应用的核心特性
