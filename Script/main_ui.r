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

# RBAC 管理函数（权限检查需要）
source("Script/rbac_management.r")

main_ui <- function(is_admin = FALSE, user_modules = NULL, current_user = NULL) {
  # 读取字体大小配置
  table_font_size <- config_get_value("table_font_size", "13")
  input_font_size <- config_get_value("input_font_size", "13")
  
  # RBAC 模块可见性检查
  rbac_modules <- rbac_all_modules()
  can_access <- function(mod) {
    if (is_admin || is.null(user_modules)) return(TRUE)
    if (!mod %in% rbac_modules) return(TRUE)  # 模块不在RBAC管控内，开放
    return(mod %in% user_modules)
  }
  
  # RBAC 管理子页签权限检查（admin角色/拥有对应权限码）
  can_admin <- function(code) {
    is_admin || rbac_check(current_user, code)
  }

  # 创建导航栏页面
  # navbarPage是Shiny中创建带有标签页的导航栏界面的函数
  navbarPage(
    id = "main_tabs",  # 用于 updateTabsetPanel 切换标签
    title = "LVCC ITOM",  # 应用标题
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
    
    # 首页标签页（RBAC管控）
    if (can_access("首页")) tabPanel(
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
        titlePanel("欢迎使用 LVCC ITOM（Information Technology Operations Management）"),
        br(),
        fluidRow(
          column(6,
            div(style = "border:1px solid #ddd; border-radius:8px; padding:16px; margin-bottom:16px;",
              div(style = "display:flex; justify-content:space-between; align-items:center; border-bottom:2px solid #337ab7; padding-bottom:8px; margin-bottom:8px;",
                h4(style = "margin:0; color:#337ab7;", "我的项目"),
                tags$a("more \u00bb", href = "#", onclick = "Shiny.setInputValue('home_goto_proj',Math.random(),{priority:'event'});return false;", style = "font-size:12px; color:#337ab7;")
              ),
              uiOutput("home_my_projects")
            ),
            div(style = "border:1px solid #ddd; border-radius:8px; padding:16px; margin-bottom:16px;",
              h4(style = "margin-top:0; color:#5cb85c; border-bottom:2px solid #5cb85c; padding-bottom:8px;", "我的任务"),
              uiOutput("home_my_tasks")
            )
          ),
          column(6,
            div(style = "border:1px solid #ddd; border-radius:8px; padding:16px; margin-bottom:16px;",
              div(style = "display:flex; justify-content:space-between; align-items:center; border-bottom:2px solid #5bc0de; padding-bottom:8px; margin-bottom:8px;",
                h4(style = "margin:0; color:#5bc0de;", "我的工单"),
                tags$a("more \u00bb", href = "#", onclick = "Shiny.setInputValue('home_goto_wo',Math.random(),{priority:'event'});return false;", style = "font-size:12px; color:#5bc0de;")
              ),
              uiOutput("home_my_work_orders")
            )
          )
        ),
        # 快速操作（仅管理员可见）
        if (is_admin) fluidRow(
          column(4,
            div(style = "border:1px solid #f0ad4e; border-radius:8px; padding:12px; margin-top:16px; background:#fffdf5;",
              h5(style = "margin-top:0; color:#f0ad4e;", icon("code"), " 快速开发"),
              textAreaInput("quick_dev_input", NULL, width = "100%", rows = 6, placeholder = "输入开发需求/方案…"),
              div(style = "display:flex; gap:4px;",
                actionButton("quick_dev_submit", "提交", icon = icon("paper-plane"), class = "btn-warning btn-sm", style = "flex:1;"),
                actionButton("quick_dev_goto_log", "View More \u00bb", class = "btn-sm btn-link", style = "white-space:nowrap;")
              ),
              hr(style = "margin:6px 0;"),
              uiOutput("home_latest_dev_logs")
            )
          ),
          column(4,
            div(style = "border:1px solid #5bc0de; border-radius:8px; padding:12px; margin-top:16px; background:#f0f9ff;",
              h5(style = "margin-top:0; color:#5bc0de;", icon("sticky-note"), " 快速记事"),
              textAreaInput("quick_note_input", NULL, width = "100%", rows = 6, placeholder = "标题(第一行)\n内容…"),
              div(style = "display:flex; gap:4px;",
                actionButton("quick_note_submit", "创建", icon = icon("plus"), class = "btn-info btn-sm", style = "flex:1;"),
                actionButton("quick_note_viewmore", "View More \u00bb", class = "btn-sm btn-link", style = "white-space:nowrap;")
              ),
              hr(style = "margin:6px 0;"),
              uiOutput("home_recent_notes"),
              div(style = "display:flex; gap:4px; margin-top:6px;",
                textInput("home_note_search", NULL, width = "100%", placeholder = "搜索记事…"),
                actionButton("home_note_search_btn", NULL, icon = icon("search"), class = "btn-xs btn-info")
              ),
              uiOutput("home_note_search_result")
            )
          ),
          column(4,
            div(style = "border:1px solid #5cb85c; border-radius:8px; padding:12px; margin-top:16px; background:#f0fff4;",
              h5(style = "margin-top:0; color:#5cb85c;", icon("ticket-alt"), " 快速工单"),
              textAreaInput("quick_wo_input", NULL, width = "100%", rows = 6, placeholder = "IT服务请求 20260702 1000：\n用户：姓名-部门-职位\n内容：…\n@处理人-部门-职位"),
              div(style = "display:flex; gap:4px;",
                actionButton("quick_wo_submit", "创建", icon = icon("plus"), class = "btn-success btn-sm", style = "flex:1;"),
                actionButton("quick_wo_viewmore", "View More \u00bb", class = "btn-sm btn-link", style = "white-space:nowrap;")
              ),
              hr(style = "margin:6px 0;"),
              uiOutput("home_recent_wos")
            )
          )
        ),
        # 页面底部留白
        tags$div(style = "height:120px;")
      )
    ),
    
    # 项目管理标签页（放在工单前面）
    if (can_access("项目")) tabPanel(
      "项目",
      icon = icon("project-diagram"),
      project_ui()
    ),

    # 巡检标签页
    if (can_access("巡检")) tabPanel(
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
    if (can_access("工单")) tabPanel(
      "工单",
      icon = icon("clipboard-list"),
      fluidPage(
        # 字体大小动态配置 + 工单专属JS
        tags$head(
          tags$style(HTML(sprintf("
            /* 全局列表字体大小配置 */
            .dataTables_wrapper table.dataTable tbody td {
              font-size: %spx !important;
            }
            .dataTables_wrapper table.dataTable thead th {
              font-size: %spx !important;
            }
            /* 表单控件字体大小（覆盖全局默认14px，使用系统设置值） */
            .form-control, .selectize-input, .selectize-dropdown {
              font-size: %spx !important;
            }
          ", table_font_size, table_font_size, input_font_size))),
          tags$script(HTML("
            $(document).on('shiny:connected', function(event) {
              $('.navbar-nav > li.dropdown').each(function() {
                var link = $(this).find('a.dropdown-toggle');
                if (link.length && link.text().indexOf('管理') >= 0) {
                  $(this).addClass('admin-menu-item');
                }
              });
            });
            window.scrollToQuickWO = function() {
              var h4s = document.querySelectorAll('h4');
              for (var i = 0; i < h4s.length; i++) {
                if (h4s[i].innerText.indexOf('快速工单') !== -1) {
                  h4s[i].scrollIntoView({ behavior: 'smooth', block: 'center' });
                  break;
                }
              }
              setTimeout(function() {
                var ta = document.querySelector('#quick_work_order_text');
                if (ta) ta.focus();
              }, 300);
            };
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
                # ★ Admin批量操作栏
                conditionalPanel(
                  condition = "output.wo_is_admin",
                  div(style = "background:#fef3e8; padding:6px 10px; margin-bottom:4px; border-radius:4px; border:1px solid #f0c36d; display:flex; align-items:center; gap:8px;",
                    tags$b("批量操作：", style="font-size:12px; color:#e67e22;"),
                    tags$input(type="checkbox", id="wo_select_all", onclick="$('.wo-batch-cb').prop('checked',this.checked).trigger('change')", style="margin:0;"),
                    tags$label("全选", `for`="wo_select_all", style="font-size:12px; margin:0 6px 0 2px;"),
                    actionButton("wo_batch_delete", "批量删除", class = "btn-danger btn-sm", icon = icon("trash"),
                      onclick = "$('#wo_batch_ids').val($('.wo-batch-cb:checked').map(function(){return this.value}).get().join(',')).trigger('change')"),
                    actionButton("wo_batch_reopen", "批量激活", class = "btn-warning btn-sm", icon = icon("play"),
                      onclick = "$('#wo_batch_ids').val($('.wo-batch-cb:checked').map(function(){return this.value}).get().join(',')).trigger('change')"),
                    actionButton("wo_batch_close", "批量关闭", class = "btn-default btn-sm", icon = icon("stop"),
                      onclick = "$('#wo_batch_ids').val($('.wo-batch-cb:checked').map(function(){return this.value}).get().join(',')).trigger('change')"),
                    tags$span(style="font-size:11px; color:#999;", HTML("先勾选工单前的复选框，再点操作"))
                  ),
                  tags$div(style="display:none;", textInput("wo_batch_ids", NULL, value = ""))
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
            wellPanel(style = "background:#f0f7ff;",
              h4("创建新工单"),
              fluidRow(
                column(4, textInput("work_order_title", "工单标题")),
                column(2, uiOutput("work_order_priority_ui")),
                column(2, textInput("work_order_request_user", "请求用户")),
                column(2, uiOutput("work_order_category_ui")),
                column(2, div(style = "margin-top: 20px;", tags$button(id="add_work_order", type="button", class="btn btn-primary action-button", disabled=NA, style="padding:4px 10px;font-size:12px;", "创建工单")))
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
            wellPanel(style = "background:#f0faf5;",
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
                    tags$button(id="create_quick_work_order", type="button", class="btn btn-success action-button", disabled=NA, style="padding:8px 16px;font-size:14px;", "快速创建")
                  ),
                  uiOutput("quick_work_order_preview")
                )
              )
            )
          )
        ),
        # 批量补工单（粘贴日报文本，支持编辑请求人后创建）
        fluidRow(
          column(12,
            wellPanel(style = "background:#f0f9fa;",
              h4("批量补工单"),
              p("粘贴日报文本，自动拆解为多条工单。可在预览中修改「请求人」后再创建。", style = "color: #666; font-size: 12px;"),
              pre('韩荣昌 2026年6月23日 日报

1. IT支持与故障处理
● 处理蔡金萍反馈2号楼4楼打印报错问题...
● 处理吕嘉俊电脑表格复制变空白的问题...

2. 权限管理与安全
● 处理赖庆耀新更换的手机临时授权一天...', style = "font-size: 11px; background: #f5f5f5; padding: 8px;"),
              fluidRow(
                column(12, textAreaInput("batch_work_order_text", "粘贴日报文本", rows = 5, placeholder = "第一行：姓名 日期 日报\n后续：● 开头的行为工单条目..."))
              ),
              uiOutput("batch_work_order_preview"),
              div(style = "margin-top:8px;",
                tags$button(id="create_batch_work_order", type="button", class="btn btn-info action-button", disabled=NA, style="padding:8px 16px;font-size:14px;", "批量创建")
              )
            )
          )
        )
      )
    ),

    # 资产标签页（含工位图子标签）
    if (can_access("资产")) tabPanel(
      "资产",
      icon = icon("laptop"),
      asset_ui()
    ),

    # 记事标签页
    if (can_access("记事")) tabPanel(
      "记事",
      icon = icon("sticky-note"),
      note_ui()
    ),

    # 标准化标签页
    if (can_access("标准化")) tabPanel(
      "标准化",
      icon = icon("cogs"),  # 齿轮图标
      std_ui()
    ),
    
    # 测试标签页（网络巡检）
    if (can_access("测试")) tabPanel(
      "测试",
      icon = icon("network-wired"),
      network_test_ui()
    ),

    # 性能监控标签页
    if (can_access("性能")) tabPanel(
      "性能",
      icon = icon("heartbeat"),
      sysmon_ui()
    ),

    # 日报标签页
    if (can_access("日报")) tabPanel(
      "日报",
      icon = icon("calendar-day"),
      daily_report_ui()
    ),

    # 收集器标签页
    if (can_access("收集器")) tabPanel(
      "收集器",
      icon = icon("download"),  # 收集器图标
      fluidPage(
        titlePanel("信息收集器"),
        sidebarLayout(
          sidebarPanel(
            textInput("collector_name", "收集器名称"),
            selectInput("collector_type", "收集器类型", choices = c("系统信息", "网络信息", "应用信息", "数据库信息")),
            textAreaInput("collector_config", "收集器配置"),
            tags$button(id="add_collector", type="button", class="btn btn-primary action-button", disabled=NA, "添加收集器"),
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
    if (can_access("集成")) tabPanel(
      "集成",
      icon = icon("plug"),
      integration_ui()
    ),

    # 数据中心标签页（数据归集）
    if (can_access("数据")) tabPanel(
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

    # 岗职矩阵标签页
    if (can_access("岗职")) tabPanel(
      "岗职",
      icon = icon("sitemap"),
      duty_matrix_ui()
    ),

    # 绩效管理标签页
    if (can_access("绩效")) tabPanel(
      "绩效",
      icon = icon("chart-bar"),
      performance_ui()
    ),

    # 模型训练标签页
    if (can_access("模型")) tabPanel(
      "模型",
      icon = icon("cogs"),  # 齿轮图标
      fluidPage(
        titlePanel("模型"),
        sidebarLayout(
          sidebarPanel(
            textInput("model_name", "模型名称"),
            selectInput("model_type", "模型类型", choices = c("线性回归", "决策树", "随机森林", "神经网络", "SVM")),
            textAreaInput("model_params", "模型参数"),
            tags$button(id="train_model", type="button", class="btn btn-primary action-button", disabled=NA, "训练模型"),
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
    if (can_access("可视化")) tabPanel(
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
        tags$style(HTML("
          .viz-code-toggle { cursor:pointer; color:#337ab7; font-size:12px; display:inline-block; user-select:none; }
          .viz-code-toggle:hover { text-decoration:underline; }
        ")),
        sidebarLayout(
          sidebarPanel(
            selectInput("viz_type", "图表类型", choices = c("词云图", "柱状图", "折线图", "散点图", "饼图", "热力图"), selected = "词云图"),
            selectInput("viz_data", "数据源", choices = c("记事数据", "ITOM数据", "模型数据", "流程监控"), selected = "记事数据"),
            actionButton("generate_viz", "生成图表", class = "btn-primary"),
            hr(),
            tags$a("▸ 算法 / 代码", class="viz-code-toggle",
              onclick="var b=document.getElementById('viz_code_block');b.style.display=b.style.display==='none'?'block':'none';this.textContent=(b.style.display==='none'?'▸':'▾')+' 算法 / 代码';"),
            div(id="viz_code_block", style="display:none;",
              htmlOutput("viz_code", style = "margin-top:6px;")
            )
          ),
          mainPanel(
            uiOutput("viz_plot")
          )
        )
      )
    ),
    
    # 管理菜单（admin全功能 / user仅个人信息，始终可见）
    navbarMenu(
      "管理",
      icon = icon("tools"),
      # --- admin 专属 ---
      if (can_admin("admin_users")) tabPanel(
        "组织架构",
        icon = icon("sitemap"),
        fluidPage(
          titlePanel("组织架构"),
          tags$style(HTML("
            /* ── Xmind 风格思维导图容器 ── */
            .org-mindmap-wrap { width:100%; height:68vh; overflow:auto; border:1px solid #e0e0e0; border-radius:8px; background:#fafbfc; padding:16px; }
            .org-mindmap-wrap svg { max-width:none; }
            /* ── 搜索栏 ── */
            .org-search-bar { display:flex; align-items:center; max-width:360px; border:1px solid #cfd8dc; border-radius:20px; padding:0 4px 0 14px; background:#fff; transition:border-color 0.2s; margin-bottom:10px; }
            .org-search-bar:focus-within { border-color:#4f8ef7; box-shadow:0 0 0 2px rgba(79,142,247,0.15); }
            .org-search-input { border:none; outline:none; flex:1; padding:7px 4px; font-size:13px; background:transparent; min-width:0; }
            .org-search-icon, .org-search-clear { display:flex; align-items:center; justify-content:center; width:30px; height:30px; border-radius:50%; cursor:pointer; color:#90a4ae; transition:all 0.2s; font-size:13px; flex-shrink:0; }
            .org-search-icon:hover { color:#4f8ef7; background:#e3f2fd; }
            .org-search-clear:hover { color:#d9534f; background:#fde8e8; }
            /* ── 选中节点高亮 ── */
            .org-mindmap-wrap .node-highlight rect,
            .org-mindmap-wrap .node-highlight circle,
            .org-mindmap-wrap .node-highlight ellipse,
            .org-mindmap-wrap .node-highlight polygon { stroke:#4f8ef7 !important; stroke-width:3px !important; }
            /* ── 搜索高亮 ── */
            .org-mindmap-wrap .node-search-match rect,
            .org-mindmap-wrap .node-search-match circle,
            .org-mindmap-wrap .node-search-match ellipse { fill:#fff9c4 !important; stroke:#ffc107 !important; stroke-width:2px !important; }
          ")),
          div(style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-bottom:8px;",
            # 搜索框（带放大镜和X）
            tags$div(class="org-search-bar",
              tags$input(id="org_search_input", type="text", class="org-search-input",
                placeholder="搜索部门或人员...", autocomplete="off"),
              tags$span(id="org_search_btn", class="org-search-icon", title="搜索",
                tags$i(class="fa fa-search")),
              tags$span(id="org_search_clear", class="org-search-clear", style="display:none;", title="清除",
                tags$i(class="fa fa-times"))
            ),
            actionButton("org_add_dept","",icon=icon("plus"),class="btn-sm btn-success",title="添加部门"),
            actionButton("org_edit_dept","",icon=icon("building"),class="btn-sm btn-warning",title="编辑部门"),
            actionButton("org_del_dept","",icon=icon("trash"),class="btn-sm btn-danger",title="删除部门"),
            actionButton("org_add_user","",icon=icon("user-plus"),class="btn-sm btn-primary",title="添加人员"),
            actionButton("org_edit_user","",icon=icon("id-badge"),class="btn-sm btn-info",title="编辑人员"),
            actionButton("org_expand_all","",icon=icon("expand-arrows-alt"),class="btn-sm btn-default",title="全部展开"),
            actionButton("org_collapse_all","",icon=icon("compress-arrows-alt"),class="btn-sm btn-default",title="全部折叠"),
            actionButton("org_refresh","",icon=icon("sync"),class="btn-sm btn-default",title="刷新"),
            tags$span(style="margin-left:6px; font-size:13px; color:#555;", uiOutput("org_selected_info"))
          ),
          div(class="org-mindmap-wrap", id="org_mindmap_container",
            uiOutput("org_mindmap")
          )
        )
      ),
      if (can_admin("admin_system")) tabPanel(
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
                tags$button(id="save_font_config", type="button", class="btn btn-primary btn-sm action-button", disabled=NA, list(icon("save"), "保存字体设置"))))
            )
          ),
          hr(),
          # 彩虹色配置
          wellPanel(
            h4("全局彩虹色", style="margin-top:0;"),
            p(style="color:#666; font-size:12px;", "配置 20 个全局颜色（组织架构等模块调用此顺序）。点击色块可编辑，拖拽排序可调顺序。"),
            div(style="display:flex; gap:6px; align-items:center; flex-wrap:wrap; margin-bottom:8px;",
              uiOutput("cfg_rainbow_swatches"),
              div(style="margin-left:8px;",
                actionButton("cfg_rainbow_add","",icon=icon("plus"),class="btn-xs btn-success",title="追加颜色"),
                actionButton("cfg_rainbow_reset","",icon=icon("undo"),class="btn-xs btn-warning",title="恢复默认20色")
              )
            ),
            div(style="display:none;", textInput("cfg_rainbow_edit_idx", NULL), textInput("cfg_rainbow_edit_color", NULL)),
            div(id="cfg_rainbow_msg", style="font-size:12px; color:#999; margin-top:4px;")
          ),
          hr(),
          # 通用配置管理
          sidebarLayout(
            sidebarPanel(
              textInput("config_key", "配置键"),
              textInput("config_value", "配置值"),
              textInput("config_desc", "描述"),
              tags$button(id="add_config", type="button", class="btn btn-primary action-button", disabled=NA, "添加配置"),
              br(), br(),
              actionButton("refresh_config", "刷新配置", class = "btn-info")
            ),
            mainPanel(
              DTOutput("config_table")
            )
          )
        )
      ),
      if (can_admin("admin_options")) tabPanel(
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
      if (can_admin("admin_rbac")) tabPanel(
        "授权管理",
        icon = icon("shield-alt"),
        fluidPage(
          titlePanel("RBAC 授权管理"),
          tabsetPanel(
            # ── Tab 1：用户管理 ──
            tabPanel("用户管理",
              br(),
              fluidRow(
                column(3,
                  wellPanel(
                    h4("部门筛选"),
                    div(style="margin-bottom:6px;",
                      actionButton("rbac_u_refresh","",icon=icon("sync"),class="btn-xs btn-default",title="刷新"),
                      tags$small(" 选择部门查看人员")
                    ),
                    uiOutput("rbac_u_dept_tree")
                  )
                ),
                column(9,
                  div(style="display:flex; gap:4px; margin-bottom:8px; align-items:center; flex-wrap:wrap;",
                    actionButton("rbac_u_add",  "", icon=icon("plus"),         class="btn-sm btn-success", title="添加用户"),
                    actionButton("rbac_u_edit", "", icon=icon("pencil"),       class="btn-sm btn-default", title="编辑用户"),
                    actionButton("rbac_u_del",  "", icon=icon("trash"),        class="btn-sm btn-danger",  title="删除用户"),
                    actionButton("rbac_u_rpw",  "", icon=icon("key"),          class="btn-sm btn-warning", title="初始化密码"),
                    actionButton("rbac_u_act",  "", icon=icon("toggle-on"),    class="btn-sm btn-default", title="禁用/启用"),
                    div(style="width:180px;", selectizeInput("rbac_u_filter_role", NULL,
                      choices=c("全部角色"="","admin","user","it_desk","it_engineer","sys_engineer"),
                      width="100%", options=list(placeholder="角色筛选..."))),
                    div(style="width:160px;", textInput("rbac_u_search", NULL, placeholder="搜索用户名/显示名...", width="100%"))
                  ),
                  DTOutput("rbac_u_table")
                )
              )
            ),
            # ── Tab 2：角色管理 ──
            tabPanel("角色管理",
              br(),
              fluidRow(
                column(3,
                  wellPanel(
                    h4("角色列表"),
                    DTOutput("rbac_role_table"),
                    br(),
                    div(style = "display:flex; gap:6px; align-items:center; margin-bottom:6px;",
                      tags$button(id="rbac_edit_role", type="button", class="btn btn-warning btn-sm action-button", disabled=NA, list(icon("pencil"), "编辑")),
                      tags$button(id="rbac_delete_roles", type="button", class="btn btn-danger btn-sm action-button", disabled=NA, list(icon("trash"), "删除选中"))
                    ),
                    textInput("rbac_new_role_name", NULL, placeholder = "新角色名称"),
                    textInput("rbac_new_role_desc", NULL, placeholder = "描述(可选)"),
                    tags$button(id="rbac_add_role", type="button", class="btn btn-primary btn-sm action-button", disabled=NA, list(icon("plus"), "添加角色"))
                  )
                ),
                column(9,
                  wellPanel(
                    h4("权限配置"),
                    uiOutput("rbac_role_perms_ui")
                  )
                )
              )
            ),
            # ── Tab 3：权限清单 ──
            tabPanel("权限清单",
              br(),
              uiOutput("rbac_perm_table")
            ),
            # ── Tab 4：授权管理（原用户授权）──
            tabPanel("授权管理",
              br(),
              fluidRow(
                column(4,
                  wellPanel(
                    h4("用户列表"),
                    DTOutput("rbac_user_table")
                  )
                ),
                column(8,
                  wellPanel(
                    h4("角色分配"),
                    uiOutput("rbac_user_roles_ui"),
                    br(),
                    tags$button(id="rbac_save_user_roles", type="button", class="btn btn-success action-button", disabled=NA, list(icon("save"), "保存角色"))
                  )
                )
              )
            )
          )
        )
      ),
      # 数据结转（月度数据结转）
      tabPanel(
        "数据结转",
        icon = icon("calendar-check"),
        fluidPage(
          titlePanel("月度数据结转"),
          p(style="color:#666; font-size:13px;", "将月度数据（记事等）结转到下月。先确认上月未完成事项，再生成下月模板。"),
          tabsetPanel(
            # ── Tab 1：记事结转 ──
            tabPanel("记事结转",
              br(),
              h4(icon("clipboard-list"), "1. 待结账记事 — 确认结账"),
              p(style="color:#999; font-size:12px;", "自动检测最早未完成月份。勾选要结转到\"已完成\"的记事，确认后执行。"),
              fluidRow(
                column(2, actionButton("carryover_load_prev", "加载待结账清单", icon=icon("search"), class="btn-info btn-sm")),
                column(2, actionButton("carryover_select_all", "全选", class="btn-default btn-sm")),
                column(2, actionButton("carryover_deselect_all", "取消全选", class="btn-default btn-sm")),
                column(3, div(style="margin-top:4px; font-size:12px; color:#999;", uiOutput("carryover_prev_month_label")))
              ),
              br(),
              DTOutput("carryover_prev_table"),
              br(),
              div(style="display:flex; gap:8px;",
                actionButton("carryover_close_btn", "确认结账（改为已完成）", icon=icon("check-circle"), class="btn-warning"),
                tags$span(style="color:#999; font-size:12px; margin-top:6px;", "仅处理勾选的记事")
              ),
              hr(),
              h4(icon("copy"), "2. 生成本月模板记事的副本 → 下月"),
              p(style="color:#999; font-size:12px;", "从所有带 (YYYY年M月) 标题的记事中选取上月模板，生成下月副本。日期自动设为下月1日8:00、到期末天17:00、提醒25日8:01。"),
              fluidRow(
                column(2, actionButton("carryover_load_curr", "加载本月模板", icon=icon("search"), class="btn-info btn-sm")),
                column(2, actionButton("carryover_gen_sel_all", "全选", class="btn-default btn-sm")),
                column(2, actionButton("carryover_gen_desel_all", "取消全选", class="btn-default btn-sm")),
                column(3, div(style="margin-top:4px; font-size:12px; color:#999;", uiOutput("carryover_next_month_label")))
              ),
              br(),
              DTOutput("carryover_template_table"),
              br(),
              actionButton("carryover_gen_btn", "生成下月记事", icon=icon("forward"), class="btn-success")
            )
          )
        )
      ),
      if (can_admin("admin_github")) tabPanel(
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
      if (can_admin("admin_architecture")) tabPanel(
        "系统架构",
        icon = icon("project-diagram"),
        fluidPage(
          system_architecture_ui()
        )
      ),
      if (can_admin("admin_inventory")) tabPanel(
        "模块清单",
        icon = icon("sitemap"),
        fluidPage(
          uiOutput("module_inventory_ui")
        )
      ),
      if (can_admin("admin_dev_log")) tabPanel(
        "开发日志",
        icon = icon("history"),
        fluidPage(
          uiOutput("dl_head"),
          hr(),
          uiOutput("dl_list")
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
                tags$button(id="self_save_password", type="button", class="btn btn-primary action-button", disabled=NA, list(icon("save"), "保存密码")),
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
            tags$button(id="add_config", type="button", class="btn btn-primary action-button", disabled=NA, "添加配置"),
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
    ),
  )
}
