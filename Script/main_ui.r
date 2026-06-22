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

# 加载性能监控模块
source("Script/sysmon_ui.r")

# 加载项目管理模块UI
source("Script/project_ui.r")

# 加载日报模块
source("Script/daily_report.r")

# 加载数据中心模块（数据归集）
source("Script/data_center_ui.r")
source("Script/integration_ui.r")

# 加载流程模块
source("Script/process_ui.r")

# 加载绩效模块
source("Script/performance_ui.r")

# 加载记事模块
source("Script/note_ui.r")

# 加载资产模块
source("Script/asset_ui.r")

# 加载岗职模块
source("Script/duty_matrix_ui.r")

main_ui <- function(is_admin = FALSE) {
  # 读取字体大小配置
  table_font_size <- config_get_value("table_font_size", "13")
  input_font_size <- config_get_value("input_font_size", "13")

  # 创建导航栏页面
  # navbarPage是Shiny中创建带有标签页的导航栏界面的函数
  navbarPage(
    id = "main_tabs",  # 用于 updateTabsetPanel 切换标签
    title = "SuperITOM2",  # 应用标题
    theme = shinytheme("cosmo"),  # 使用cosmo主题，使界面更美观
    collapsible = TRUE,  # 移动端导航栏可折叠
    
    # URL路由同步脚本 - 放在header中避免导航容器警告
    header = tags$script(HTML("
      // 路由表：URL路径 -> 标签文本
      var routeMap = {
        '/home': '首页',
        '/project': '项目',
        '/inspection': '巡检',
        '/work_order': '工单',
        '/note': '记事',
        '/std': '标准化',
        '/network_test': '测试',
        '/monitor': '性能',
        '/daily_report': '日报',
        '/collector': '收集器',
        '/data': '数据',
        '/process': '流程',
        '/duty': '岗职',
        '/model': '模型',
        '/visualization': '可视化',
        '/admin': '管理'
      };
      
      // 查找包含指定文本的导航链接
      function findNavLink(text) {
        var $links = $('.navbar-nav li a, .navbar-nav a, ul.nav a, .nav-tabs a');
        for (var i = 0; i < $links.length; i++) {
          var $link = $links.eq(i);
          var linkText = $link.text().trim();
          // 排除图标（只保留文字部分）
          linkText = linkText.replace(/\\s*<[^>]*>\\s*/g, '').trim();
          if (linkText === text || linkText.indexOf(text) >= 0) {
            return $link;
          }
        }
        return null;
      }
      
      // 从URL hash获取目标标签
      function getTargetFromHash() {
        var hash = window.location.hash.replace('#', '');
        if (!hash) return '首页';
        // 精确匹配
        if (routeMap['/' + hash]) return routeMap['/' + hash];
        if (routeMap[hash]) return routeMap[hash];
        // 模糊匹配
        for (var path in routeMap) {
          if (hash.startsWith(path.replace('/', '')) || hash === path.slice(1)) {
            return routeMap[path];
          }
        }
        return '首页';
      }
      
      // 切换到指定标签
      function switchToTab(text) {
        var $link = findNavLink(text);
        if ($link && $link.length) {
          $link[0].click();
        }
      }
      
      // 监听 Shiny 服务端发起的导航请求（穿透链接）
      Shiny.addCustomMessageHandler('navigateToTab', function(tabName) {
        switchToTab(tabName);
      });
      
      // 点击导航链接时更新URL hash
      $(document).on('click', '.navbar-nav li a, .navbar-nav > li > a', function(e) {
        var $link = $(this);
        var text = $link.text().trim().replace(/\\s*<[^>]*>\\s*/g, '').trim();
        
        // 遍历路由映射，找到匹配的标签
        for (var path in routeMap) {
          if (routeMap[path] === text) {
            history.replaceState(null, '', '#' + path.replace('/', ''));
            break;
          }
        }
      });
      
      // 页面加载后根据URL切换标签
      $(document).on('shiny:connected', function() {
        var target = getTargetFromHash();
        // 延迟等待DOM和Shiny完全加载
        var attempts = 0;
        var maxAttempts = 20;
        var interval = setInterval(function() {
          attempts++;
          var $link = findNavLink(target);
          if ($link && $link.length && !$link.closest('li').hasClass('active')) {
            clearInterval(interval);
            $link[0].click();
          } else if (attempts >= maxAttempts) {
            clearInterval(interval);
          }
        }, 300);
      });
      
      // 监听浏览器前进/后退
      window.addEventListener('hashchange', function() {
        var target = getTargetFromHash();
        switchToTab(target);
      });
    ")),
    
    # 首页标签页
    tabPanel(
      "首页",  # 标签页标题
      icon = icon("home"),  # 标签页图标
      fluidPage(
        # 首页项目点击事件处理
        tags$script(HTML("
          $(document).on('click', '.proj-enter-btn', function(e) {
            e.stopPropagation(); e.preventDefault();
            var id = $(this).data('id');
            var name = $(this).data('name');
            Shiny.setInputValue('proj_enter_click', {id: String(id), name: name}, {priority:'event'});
          });
        ")),
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

    # 巡检标签页
    tabPanel(
      "巡检",
      icon = icon("clipboard-check"),
      fluidPage(
        # 巡检统计数据
        fluidRow(
          column(12,
            div(style = "margin-bottom: 10px;",
              fluidRow(
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0;",
                  div(style = "font-size: 11px; color: #666; font-weight: 500;", "巡检计划"),
                  div(style = "font-size: 18px; font-weight: bold; color: #333;", textOutput("insp_stat_plans"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #5cb85c; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "进行中"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_active_plans"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #f0ad4e; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "待执行"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_pending_tasks"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #5bc0de; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "已完成"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_completed_tasks"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #d9534f; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "异常"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_abnormal_tasks"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #9370db; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "待整改"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("insp_stat_issues"))
                ))
              )
            )
          )
        ),
        # 巡检标签页内容
        tabsetPanel(
          # 我的任务
          tabPanel("我的任务",
            br(),
            fluidRow(
              column(3, selectInput("insp_my_status_filter", "任务状态", choices = NULL)),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_my_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("insp_my_task_table")
          ),
          # 巡检计划
          tabPanel("巡检计划",
            br(),
            fluidRow(
              column(2, selectInput("insp_plan_status_filter", "计划状态", choices = NULL)),
              column(3, div(style = "margin-top: 20px;", actionButton("insp_create_plan", "创建计划", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;"))),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_plan_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("inspection_plan_table"),
            br(),
            wellPanel(
              h4("生成巡检任务"),
              fluidRow(
                column(3, selectInput("insp_task_inspector", "检查人", choices = NULL)),
                column(3, dateInput("insp_task_date", "计划日期", value = Sys.Date(), format = "yyyy-mm-dd")),
                column(3, div(style = "margin-top: 20px;", actionButton("insp_generate_tasks", "生成任务", class = "btn-success", style = "padding: 4px 10px; font-size: 12px;")))
              )
            ),
            br(),
            fluidRow(
              column(12,
                h4("该计划下的巡检任务"),
                selectInput("insp_task_status_filter", "任务状态筛选", choices = NULL),
                DTOutput("inspection_task_table")
              )
            )
          ),
          # 巡检记录
          tabPanel("巡检记录",
            br(),
            fluidRow(
              column(3, selectInput("insp_record_status_filter", "任务状态", choices = NULL)),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_record_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("insp_record_table")
          ),
          # 巡检异常
          tabPanel("巡检异常",
            br(),
            fluidRow(
              column(3, selectInput("insp_issue_status_filter", "异常状态", choices = NULL)),
              column(2, div(style = "margin-top: 20px;", actionButton("insp_issue_refresh", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
            ),
            DTOutput("insp_issue_table")
          ),
          # 已删除记录（Admin专属）
          tabPanel("已删除记录",
            br(),
            # Admin 可见内容
            conditionalPanel(
              condition = "input.isAdminInspectionUser == true",
              fluidRow(
                column(12,
                  div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
                    strong("提示："), "此页面仅Admin可见，显示已删除的巡检计划和记录，可用于审计追溯。"
                  )
                )
              ),
              fluidRow(
                column(6,
                  wellPanel(
                    h4("已删除的巡检计划", style = "color: #d9534f;"),
                    DTOutput("insp_deleted_plans_table")
                  )
                ),
                column(6,
                  wellPanel(
                    h4("已删除的巡检记录", style = "color: #d9534f;"),
                    DTOutput("insp_deleted_records_table")
                  )
                )
              )
            ),
            # 非Admin提示
            conditionalPanel(
              condition = "input.isAdminInspectionUser != true",
              div(class = "alert alert-warning",
                icon("exclamation-triangle"), " 您没有权限查看已删除记录"
              )
            )
          )
        )
      )
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
            /* 管理菜单用服务端 is_admin 控制显示，不再需要CSS隐藏 */
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
        # 工单统计数据（缩小一半，在一行显示）
        fluidRow(
          column(12,
            div(style = "margin-bottom: 10px;",
              fluidRow(
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0;",
                  div(style = "font-size: 11px; color: #666; font-weight: 500;", "总工单"),
                  div(style = "font-size: 18px; font-weight: bold; color: #333;", textOutput("wo_stat_total"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #f0ad4e; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "待处理"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("wo_stat_pending"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #5bc0de; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "已派发"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("wo_stat_assigned"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #337ab7; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "处理中"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("wo_stat_processing"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #5cb85c; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "已完成"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("wo_stat_completed"))
                )),
                column(2, div(class = "well well-sm", style = "text-align: center; padding: 6px 4px; margin-bottom: 0; background: #777; color: white;",
                  div(style = "font-size: 11px; font-weight: 500;", "已关闭"),
                  div(style = "font-size: 18px; font-weight: bold;", textOutput("wo_stat_closed"))
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
                  column(3, textInput("work_order_search", NULL, placeholder = "搜索工单...")),
                  column(2, div(style = "margin-top: 20px;", actionButton("show_create_work_order", "新建工单", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;"))),
                  column(2, div(style = "margin-top: 20px;", actionButton("show_quick_work_order", "快速创建", class = "btn-success", style = "padding: 4px 10px; font-size: 12px;"))),
                  column(1, div(style = "margin-top: 20px;", actionButton("refresh_work_orders", "刷新", class = "btn-info", style = "padding: 4px 10px; font-size: 12px;")))
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
                column(2, textInput("work_order_request_user", "请求用户")),
                column(2, uiOutput("work_order_category_ui")),
                column(2, div(style = "margin-top: 20px;", actionButton("add_work_order", "创建工单", class = "btn-primary", style = "padding: 4px 10px; font-size: 12px;")))
              ),
              fluidRow(
                column(12, textAreaInput("work_order_description", "工单描述", rows = 2))
              )
            )
          )
        ),

        # 快速工单（粘贴格式化工单文本）
        fluidRow(
          column(12,
            wellPanel(
              h4("快速工单"),
              p("粘贴以下格式的文本，自动解析并创建工单：", style = "color: #666; font-size: 12px;"),
              pre('IT服务请求 20260512 1110：
用户：谢芳材-供应链中心-副总经理
内容：两栋楼的"监控角度需要修正"...
@韩荣昌-IT部-IT工程师(Sky)', style = "font-size: 11px; background: #f5f5f5; padding: 8px;"),
              fluidRow(
                column(10, textAreaInput("quick_work_order_text", "粘贴格式化工单", rows = 4, placeholder = "请粘贴格式化工单内容...")),
                column(2,
                  div(style = "margin-top: 50px;",
                    actionButton("create_quick_work_order", "快速创建", class = "btn-success", style = "padding: 8px 16px; font-size: 14px;")
                  ),
                  uiOutput("quick_work_order_preview")
                )
              )
            )
          )
        )
      )
    ),

    # 资产标签页
    tabPanel(
      "资产",
      icon = icon("laptop"),
      asset_ui()
    ),

    # 记事标签页
    tabPanel(
      "记事",
      icon = icon("sticky-note"),
      note_ui()
    ),

    # 标准化标签页
    tabPanel(
      "标准化",
      icon = icon("cogs"),  # 齿轮图标
      std_ui()
    ),
    
    # 测试标签页（网络巡检）
    tabPanel(
      "测试",
      icon = icon("network-wired"),
      network_test_ui()
    ),

    # 性能监控标签页
    tabPanel(
      "性能",
      icon = icon("heartbeat"),
      sysmon_ui()
    ),

    # 日报标签页
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
    
    # 集成标签页
    tabPanel(
      "集成",
      icon = icon("plug"),
      integration_ui()
    ),

    # 数据中心标签页（数据归集）
    tabPanel(
      "数据",
      icon = icon("database"),
      data_center_ui()
    ),

    # 流程引擎标签页（暂停排查）
    # tabPanel(
    #   "流程",
    #   icon = icon("project-diagram"),
    #   process_ui()
    # ),

    # 岗职矩阵标签页（admin专用）
    if (is_admin) tabPanel(
      "岗职",
      icon = icon("sitemap"),
      duty_matrix_ui()
    ),

    # 绩效管理标签页（admin专用）
    if (is_admin) tabPanel(
      "绩效",
      icon = icon("chart-bar"),
      performance_ui()
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
    
    # 数据可视化标签页（含流程监控）
    tabPanel(
      "可视化",
      icon = icon("chart-line"),
      fluidPage(
        # 流程监控指标卡片
        fluidRow(
          column(3, div(class="well well-sm",style="text-align:center;padding:10px;margin-bottom:10px;background:#e8f5e9;",
            h3(textOutput("viz_mtr_complete_rate"),style="margin:0;color:#2e7d32;font-size:24px;"),
            p("流程完成率",style="margin:3px 0 0;font-size:12px;"))),
          column(3, div(class="well well-sm",style="text-align:center;padding:10px;margin-bottom:10px;background:#fff3e0;",
            h3(textOutput("viz_mtr_timeout_rate"),style="margin:0;color:#e65100;font-size:24px;"),
            p("超时率",style="margin:3px 0 0;font-size:12px;"))),
          column(3, div(class="well well-sm",style="text-align:center;padding:10px;margin-bottom:10px;background:#e3f2fd;",
            h3(textOutput("viz_mtr_avg_duration"),style="margin:0;color:#1565c0;font-size:24px;"),
            p("平均耗时",style="margin:3px 0 0;font-size:12px;"))),
          column(3, div(class="well well-sm",style="text-align:center;padding:10px;margin-bottom:10px;background:#f3e5f5;",
            h3(textOutput("viz_mtr_running"),style="margin:0;color:#7b1fa2;font-size:24px;"),
            p("运行中流程",style="margin:3px 0 0;font-size:12px;")))
        ),
        fluidRow(
          column(6, div(class="well well-sm",style="padding:8px;margin-bottom:10px;",
            h5("今日活动",style="margin:0 0 5px;color:#555;font-size:13px;"),
            span(textOutput("viz_mtr_today"),style="color:#337ab7;font-size:16px;font-weight:bold;"))),
          column(6, div(class="well well-sm",style="padding:8px;margin-bottom:10px;",
            h5("流程引擎",style="margin:0 0 5px;color:#555;font-size:13px;"),
            span("前往 流程模块 创建和管理流程", style="font-size:13px;color:#666;")))
        ),
        hr(),
        titlePanel("数据可视化"),
        sidebarLayout(
          sidebarPanel(
            selectInput("viz_type", "图表类型", choices = c("折线图", "柱状图", "散点图", "饼图", "热力图")),
            selectInput("viz_data", "数据源", choices = c("ITOM数据", "模型数据", "流程监控")),
            actionButton("generate_viz", "生成图表", class = "btn-primary")
          ),
          mainPanel(
            plotlyOutput("viz_plot")
          )
        )
      )
    ),
    
    # 管理菜单（admin全功能 / user仅个人信息）
    navbarMenu(
      "管理",
      icon = icon("tools"),
      # --- admin 专属 ---
      if (is_admin) tabPanel(
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
      if (is_admin) tabPanel(
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
      if (is_admin) tabPanel(
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
      if (is_admin) tabPanel(
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
      ),
      # --- 所有用户可见 ---
      tabPanel(
        "个人信息",
        icon = icon("user-circle"),
        fluidPage(
          titlePanel("个人信息"),
          fluidRow(
            column(6,
              wellPanel(
                h4("账号信息"),
                div(style = "margin-bottom:10px;",
                  tags$b("用户名："), textOutput("self_info_username", inline = TRUE)),
                div(style = "margin-bottom:10px;",
                  tags$b("显示名称："), textOutput("self_info_display_name", inline = TRUE)),
                div(style = "margin-bottom:10px;",
                  tags$b("角色："), textOutput("self_info_role", inline = TRUE))
              )
            ),
            column(6,
              wellPanel(
                h4("修改密码"),
                passwordInput("self_old_password", "旧密码"),
                passwordInput("self_new_password", "新密码"),
                passwordInput("self_new_password_confirm", "确认新密码"),
                actionButton("self_save_password", "保存密码", class = "btn-primary", icon = icon("save")),
                br(), br(),
                textOutput("self_password_msg")
              )
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
