# 治理模块 UI（框架）
governance_ui <- function(is_admin = FALSE) {
  tagList(
    tags$style(HTML("
      .gov-cap { padding:16px 20px; border-radius:8px; color:#fff; margin-bottom:16px; }
      .gov-cap h3 { margin:0; color:#fff; }
      .gov-cap p { margin:4px 0 0; opacity:.9; font-size:13px; }
      .gov-tbl { width:100%; border-collapse:collapse; font-size:13px; margin-bottom:16px; }
      .gov-tbl th { background:#00695c; color:#fff; padding:8px 10px; text-align:left; border:1px solid #00695c; }
      .gov-tbl td { padding:8px 10px; border:1px solid #e0e0e0; vertical-align:top; }
      .gov-tbl tr:nth-child(even) td { background:#f5faf9; }
      .gov-sec { margin:20px 0 10px; color:#00695c; font-weight:bold; font-size:15px; }
      .gov-note { color:#666; font-size:12px; line-height:1.8; margin:6px 0 16px; }
      /* ── 组织架构（Xmind 风格思维导图）── */
      .org-mindmap-wrap { width:100%; height:68vh; overflow:auto; border:1px solid #e0e0e0; border-radius:8px; background:#fafbfc; padding:16px; }
      .org-mindmap-wrap svg { max-width:none; }
      .org-search-bar { display:flex; align-items:center; max-width:360px; border:1px solid #cfd8dc; border-radius:20px; padding:0 4px 0 14px; background:#fff; transition:border-color 0.2s; margin-bottom:10px; }
      .org-search-bar:focus-within { border-color:#4f8ef7; box-shadow:0 0 0 2px rgba(79,142,247,0.15); }
      .org-search-input { border:none; outline:none; flex:1; padding:7px 4px; font-size:13px; background:transparent; min-width:0; }
      .org-search-icon, .org-search-clear { display:flex; align-items:center; justify-content:center; width:30px; height:30px; border-radius:50%; cursor:pointer; color:#90a4ae; transition:all 0.2s; font-size:13px; flex-shrink:0; }
      .org-search-icon:hover { color:#4f8ef7; background:#e3f2fd; }
      .org-search-clear:hover { color:#d9534f; background:#fde8e8; }
      .org-mindmap-wrap .node-highlight rect,
      .org-mindmap-wrap .node-highlight circle,
      .org-mindmap-wrap .node-highlight ellipse,
      .org-mindmap-wrap .node-highlight polygon { stroke:#4f8ef7 !important; stroke-width:3px !important; }
      .org-mindmap-wrap .node-search-match rect,
      .org-mindmap-wrap .node-search-match circle,
      .org-mindmap-wrap .node-search-match ellipse { fill:#fff9c4 !important; stroke:#ffc107 !important; stroke-width:2px !important; }
    ")),
    fluidRow(
      column(12,
        div(class = "gov-cap", style = "background:linear-gradient(135deg,#00695c,#26a69a);",
          h3(icon("landmark"), " 企业治理与风险管理"),
          p("行业周期 · 治理模型 · 风险模型（PESTEL / PEST-SWOT）· 分析 · 行动 · 复盘")
        )
      )
    ),
    tabsetPanel(
      id = "governance_tabs", type = "pills",

      # ── 行业周期 ──
      tabPanel("行业周期",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("chart-line", "fa-4x"), br(), br(),
          h4("行业生命周期（框架）"),
          p("导入期 · 成长期 · 成熟期 · 衰退期，识别行业所处阶段与战略选择。待后续实现。")
        )
      ),

      # ── 治理模型 ──
      tabPanel("治理模型",
        br(),
        # 模型选择
        fluidRow(
          column(3, selectInput("gov_model_selector", "治理模型",
            choices = c("BSC 平衡计分卡" = "BSC"),
            width = "100%"))
        ),
        br(),

        # 一、BSC 平衡计分卡的四个维度
        tags$div(class = "gov-sec", icon("sitemap"), " 一、平衡计分卡的四个维度"),
        tags$table(class = "gov-tbl",
          tags$thead(
            tags$tr(tags$th("维度", style = "width:16%;"), tags$th("关键指标"))
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$b("财务维度")),
              tags$td("订单金额、销售收入、利润、现金流、回款等经营最终结果。")
            ),
            tags$tr(
              tags$td(tags$b("客户维度")),
              tags$td("市场份额、客户满意度、新增客户数、留存率、复购率、流失率、老客户转介绍、客户钱包份额。")
            ),
            tags$tr(
              tags$td(tags$b("内部运营维度")),
              tags$td("研发、生产制造、售后服务、老产品降本、一次生产合格率（直通率）、机器稼动率/OEE、销售预测准确率、DSO 应收账款周转天数、ITO 存货周转天数、超长期应收、人均效益。")
            ),
            tags$tr(
              tags$td(tags$b("学习与成长维度")),
              tags$td("招聘、核心人才到岗、继任者计划、合理化建议采纳、员工流失率、薪酬包投入占比。")
            )
          )
        ),

        # 二、四个维度的底层逻辑
        tags$div(class = "gov-sec", icon("link"), " 二、四个维度的底层逻辑"),
        tags$p(class = "gov-note",
          "财务目标来自新老客户，赢得客户靠内部运营竞争力（成本、服务、交期、品质），而运营能力又取决于团队持续的学习与成长。"
        ),

        # 三、平衡计分卡的五大平衡
        tags$div(class = "gov-sec", icon("balance-scale"), " 三、平衡计分卡的五大平衡"),
        tags$table(class = "gov-tbl",
          tags$thead(
            tags$tr(tags$th("#"), tags$th("平衡关系"), tags$th("说明"))
          ),
          tags$tbody(
            tags$tr(tags$td("1"), tags$td("财务指标与非财务指标的平衡"), tags$td("因与果")),
            tags$tr(tags$td("2"), tags$td("内部指标与外部指标的平衡"), tags$td("内外兼顾")),
            tags$tr(tags$td("3"), tags$td("结果与过程的平衡"), tags$td("既看结果，也管过程")),
            tags$tr(tags$td("4"), tags$td("定量与定性的平衡"), tags$td("可量化与可判断并重")),
            tags$tr(tags$td("5"), tags$td("长期指标与短期指标的平衡"), tags$td("增加土壤肥力与多打粮食"))
          )
        ),

        # 四、一句话记忆
        tags$div(class = "gov-sec", icon("lightbulb"), " 四、一句话记忆"),
        tags$p(class = "gov-note",
          tags$i(
            "\u201c用一棵树来比喻：学习成长是根，内部运营是树干，客户是树叶和花朵，财务是夏天结出的果实。",
            "想在果实上出成绩，就要在根上持续下功夫。\u201d"
          )
        )
      ),

      # ── 风险模型 ──
      tabPanel("风险模型",
        br(),
        # 一、PESTEL 六维度
        tags$div(class = "gov-sec", icon("binoculars"), " 一、PESTEL 六维度"),
        tags$table(class = "gov-tbl",
          tags$thead(
            tags$tr(
              tags$th("维度"), tags$th("含义"), tags$th("典型机会"), tags$th("典型风险")
            )
          ),
          tags$tbody(
            tags$tr(tags$td(tags$b("Political 政策/政治")), tags$td("政策、监管、补贴、地缘"), tags$td("产业扶持、试点资质、国产替代"), tags$td("监管收紧、反垄断、出口管制")),
            tags$tr(tags$td(tags$b("Economic 经济")), tags$td("增长、利率、汇率、通胀"), tags$td("消费升级、融资宽松、出海"), tags$td("周期下行、汇率波动、成本上升")),
            tags$tr(tags$td(tags$b("Social 社会")), tags$td("人口、文化、消费习惯"), tags$td("新需求、品牌认同"), tags$td("人口老龄化、偏好转移")),
            tags$tr(tags$td(tags$b("Technological 技术")), tags$td("AI、数字化、专利、平台"), tags$td("提效、新产品、技术壁垒"), tags$td("技术迭代、网络安全、数据隐私")),
            tags$tr(tags$td(tags$b("Environmental 环境")), tags$td("双碳、环保、气候"), tags$td("绿色补贴、ESG 融资"), tags$td("环保处罚、碳成本、供应链中断")),
            tags$tr(tags$td(tags$b("Legal 法律")), tags$td("公司法、数据法、劳动法"), tags$td("合规优势、知识产权保护"), tags$td("诉讼、罚款、合规成本"))
          )
        ),
        tags$p(class = "gov-note", "\u201c行业\u201d通常并入 经济（E），或单独加 I 变成 PESTELI；也有 STEEPLE 等扩展版。"),

        # 二、PEST-SWOT 怎么组合
        tags$div(class = "gov-sec", icon("exchange-alt"), " 二、PEST-SWOT 怎么组合"),
        tags$p(class = "gov-note", "PESTEL 负责外部扫描，输出 O（机会）和 T（威胁）；SWOT 再补上内部的 S（优势）和 W（劣势），形成四类战略："),
        tags$table(class = "gov-tbl",
          tags$thead(
            tags$tr(
              tags$th("组合"), tags$th("含义"), tags$th("治理/战略动作")
            )
          ),
          tags$tbody(
            tags$tr(tags$td(tags$b("SO")), tags$td("用优势抓机会"), tags$td("加大投资、快速扩张")),
            tags$tr(tags$td(tags$b("WO")), tags$td("补劣势抓机会"), tags$td("合作、并购、引才")),
            tags$tr(tags$td(tags$b("ST")), tags$td("用优势避威胁"), tags$td("合规护城河、差异化")),
            tags$tr(tags$td(tags$b("WT")), tags$td("劣势遇威胁"), tags$td("退出、收缩、对冲"))
          )
        ),

        # 三、在企业治理中的用法
        tags$div(class = "gov-sec", icon("landmark"), " 三、在企业治理中的用法"),
        tags$ul(class = "gov-note",
          tags$li(tags$b("战略委员会："), "每年做 PESTEL 扫描，判断政策、技术、行业变化。"),
          tags$li(tags$b("风险委员会："), "把 PESTEL 因素转成\u201c风险登记册 + 机会登记册\u201d。"),
          tags$li(tags$b("审计/合规委员会："), "重点盯法律、政策、数据安全、反垄断。"),
          tags$li(tags$b("投资决策："), "重大投资、并购、出海前做 PEST-SWOT。"),
          tags$li(tags$b("董事会报告："), "一页纸仪表盘——机会清单、风险清单、行动清单、指标。"),
          tags$li(tags$b("风险偏好："), "合规零容忍；创新允许可控失败；政策、技术设预警线。")
        )
      ),

      # ── 分析 ──
      tabPanel("分析",
        br(),
        fluidRow(
          column(3, selectInput("gov_filter_fw", "分析框架", choices = c("全部框架"="", governance_get_frameworks()), width = "100%")),
          column(4, textInput("gov_search", NULL, width = "100%", placeholder = "搜索标题...")),
          column(3, actionButton("gov_add_btn", "新增治理条目", icon = icon("plus"), class = "btn-success btn-sm", style = "width:100%;"))
        ),
        br(),
        uiOutput("gov_list")
      ),

      # ── 行动 ──
      tabPanel("行动",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("tasks", "fa-4x"), br(), br(),
          h4("治理行动（框架）"),
          p("将分析结论转化为可执行行动项，跟踪进度。待后续实现。")
        )
      ),

      # ── 复盘 ──
      tabPanel("复盘",
        div(style = "text-align:center; padding:60px; color:#999;",
          icon("clipboard-check", "fa-4x"), br(), br(),
          h4("治理复盘（框架）"),
          p("定期复盘治理成效，持续优化。待后续实现。")
        )
      ),

      # ── 组织架构（仅 admin 可见）──
      if (is_admin) tabPanel("组织架构",
        icon = icon("sitemap"),
        br(),
        div(style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-bottom:8px;",
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
      ),

      # ── 工作模型（仅 admin 可见）──
      if (is_admin) tabPanel("工作模型",
        icon = icon("cubes"),
        div(style = "width:100%; min-height:80vh; background:#fff; border-radius:12px; padding:16px;",
          h3("研发中心-IT部 工作模型", style = "text-align:center; color:#1e293b; margin-bottom:4px;"),
          p("IT运维全生命周期管理体系 · 关系架构图", style = "text-align:center; color:#94a3b8; font-size:13px; margin-bottom:16px;"),
          uiOutput("work_model_chart")
        )
      )
    )
  )
}
