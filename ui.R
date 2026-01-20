ui <- navbarPage(
  title = "SuperITOM2",
  theme = shinytheme("cosmo"),
  
  tabPanel(
    "首页",
    icon = icon("home"),
    fluidPage(
      titlePanel("欢迎使用 SuperITOM2"),
      br(),
      fluidRow(
        column(12,
          h3("系统简介"),
          p("SuperITOM2 是一个综合性的IT运维管理系统，提供数据管理、模型训练、可视化分析等功能。"),
          br(),
          h4("主要功能："),
          tags$ul(
            tags$li("数据管理：管理IT运维数据"),
            tags$li("模型训练：训练预测模型"),
            tags$li("可视化：数据可视化分析"),
            tags$li("用户管理：管理系统用户"),
            tags$li("系统设置：配置系统参数")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "数据管理",
    icon = icon("database"),
    fluidPage(
      titlePanel("数据管理"),
      sidebarLayout(
        sidebarPanel(
          textInput("data_name", "数据名称"),
          selectInput("data_type", "数据类型", choices = c("服务器", "网络", "应用", "数据库", "其他")),
          textAreaInput("data_value", "数据值"),
          actionButton("add_data", "添加数据", class = "btn-primary"),
          br(), br(),
          actionButton("refresh_data", "刷新数据", class = "btn-info")
        ),
        mainPanel(
          DTOutput("data_table")
        )
      )
    )
  ),
  
  tabPanel(
    "模型训练",
    icon = icon("cogs"),
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
          DTOutput("model_table"),
          br(),
          h4("训练结果"),
          verbatimTextOutput("training_result")
        )
      )
    )
  ),
  
  tabPanel(
    "可视化",
    icon = icon("chart-line"),
    fluidPage(
      titlePanel("数据可视化"),
      sidebarLayout(
        sidebarPanel(
          selectInput("viz_type", "图表类型", choices = c("折线图", "柱状图", "散点图", "饼图", "热力图")),
          selectInput("viz_data", "数据源", choices = c("ITOM数据", "模型数据")),
          actionButton("generate_viz", "生成图表", class = "btn-primary")
        ),
        mainPanel(
          plotlyOutput("viz_plot")
        )
      )
    )
  ),
  
  tabPanel(
    "用户管理",
    icon = icon("users"),
    fluidPage(
      titlePanel("用户管理"),
      sidebarLayout(
        sidebarPanel(
          textInput("username", "用户名"),
          passwordInput("password", "密码"),
          selectInput("role", "角色", choices = c("user", "admin")),
          actionButton("add_user", "添加用户", class = "btn-primary"),
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
    icon = icon("settings"),
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
  
  tabPanel(
    "GitHub",
    icon = icon("github"),
    fluidPage(
      titlePanel("GitHub"),
      br(),
      fluidRow(
        column(12,
          h3("项目信息"),
          p("项目名称: SuperITOM2"),
          p("版本: 1.0.0"),
          br(),
          h4("项目链接"),
          tags$a(href = "https://github.com", "GitHub Repository", target = "_blank")
        )
      )
    )
  )
)
