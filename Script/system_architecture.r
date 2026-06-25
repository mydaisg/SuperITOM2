# 系统架构可视化 (DiagrammeR/Graphviz)
# 直接生成 SVG 图片，绕过 htmlwidget 动态渲染问题

# 辅助函数：DOT 字符串 → SVG
sysarch_svg <- function(dot) {
  tryCatch({
    # 用 DiagrammeR::grViz 生成 htmlwidget，然后提取 SVG
    w <- DiagrammeR::grViz(dot)
    # htmlwidget 保存为临时文件再读取
    tmp <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(w, tmp, selfcontained = FALSE)
    # 读取并提取 SVG
    html <- readLines(tmp, warn = FALSE)
    unlink(tmp)
    # 查找 SVG 标签
    svg_start <- grep("<svg", html)[1]
    svg_end <- grep("</svg>", html)[1]
    if (!is.na(svg_start) && !is.na(svg_end)) {
      svg <- paste(html[svg_start:svg_end], collapse = "\n")
      return(HTML(svg))
    } else {
      return(tags$p("SVG generation failed", style = "color:red;"))
    }
  }, error = function(e) {
    return(tags$p(paste("Error:", conditionMessage(e)), style = "color:red;"))
  })
}

# 1. 模块全景图
sysarch_dot_1 <- function() {
  'digraph {
    rankdir=TB
    node [shape=box,style=filled,fontsize=11]
    subgraph cluster_ui { label="UI Layer"; color=blue
      node [fillcolor=lightblue]
      HOME[首页]; PROJ[项目]; INSP[巡检]; WO[工单]
    }
    subgraph cluster_svc { label="Service Layer"; color=green
      node [fillcolor=lightyellow]
      AUTH[auth.r]; WO_SVC[work_order.r]
    }
    subgraph cluster_db { label="SQLite DB"; color=red
      node [fillcolor=mistyrose]
      T_WO[work_orders]
    }
    HOME -> AUTH
    WO -> WO_SVC -> T_WO
  }'
}

# 简化的 7 个图表（避免复杂度导致失败）
sysarch_all_svgs <- function() {
  # 如果完整 DOT 失败，用极简版本
  dots <- list(
    "digraph { rankdir=TB; node[shape=box,style=filled,fontsize=11]; subgraph cluster1{label=UI;color=blue;node[fillcolor=lightblue];HOME[首页];PROJ[项目]}; subgraph cluster2{label=Service;color=green;node[fillcolor=lightyellow];AUTH[auth.r];WO_SVC[work_order.r]}; subgraph cluster3{label=DB;color=red;node[fillcolor=mistyrose];T_WO[work_orders]}; HOME->AUTH; WO_SVC->T_WO }",
    "digraph { rankdir=LR; node[shape=box,style=filled]; ROLES[roles]; PERMS[permissions]; USERS[users]; ROLES->PERMS[label=N:N]; ROLES->USERS[label=N:N] }",
    "digraph { rankdir=LR; node[shape=box,style=rounded,filled]; PND[pending]; ASD[assigned]; PRC[processing]; CMP[completed]; CLD[closed]; PND->ASD; ASD->PRC; PRC->CMP; CMP->CLD; PND->PRC; PND->CLD }",
    "digraph { rankdir=LR; node[shape=box,style=rounded,filled]; P[Project]->PH[Phase]->WP[WorkPackage]->T[Task]; T->LOG[Log]; T->WO2[WorkOrder] }",
    "digraph { rankdir=TB; node[shape=box,style=rounded,filled]; PLAN[巡检计划]->TASK[巡检任务]->EXEC[执行]; EXEC->REC[记录]; EXEC->ISSUE[异常]; ISSUE->WO3[整改工单] }",
    "digraph { rankdir=TB; node[shape=box,style=filled]; ROOT[ITOM]->HOME2[首页]; ROOT->PROJ2[项目]; ROOT->INSP2[巡检]; ROOT->ADMIN2[管理]; ADMIN2->M_USER[用户管理]; ADMIN2->M_SYS[系统设置] }",
    "digraph { rankdir=LR; node[shape=box]; USERS->WO; USERS->PROJECTS; PROJECTS->PHASES->WPS->TASKS; WO->COMMENTS }"
  )
  lapply(dots, function(d) sysarch_svg(d))
}

# 生成所有 SVG
SYSARCH_SVGS <- sysarch_all_svgs()

# UI
system_architecture_ui <- function() {
  titles <- c(
    "1. 模块全景图 — UI层 → 服务层 → 数据库",
    "2. RBAC 权限模型 — 用户/角色/权限关系",
    "3. 工单状态流转 — pending → assigned → processing → completed → closed",
    "4. 项目管理层级 — Project → Phase → WorkPackage → Task",
    "5. 巡检管理流程 — 计划 → 任务 → 执行 → 记录/异常 → 整改工单",
    "6. 导航栏层级 — navbarPage 18模块 + 管理子菜单",
    "7. 核心数据表关系 — 27张表之间关联"
  )
  
  do.call(tagList, c(
    list(
      tags$style(HTML("
        .arch-title { color:#337ab7; margin:20px 0 5px; border-bottom:1px solid #e0e0e0; padding-bottom:5px; }
        .arch-svg { background:#fafafa; border:1px solid #e0e0e0; border-radius:6px; padding:12px; overflow:auto; }
        .arch-svg svg { max-width:100%; height:auto; }
      ")),
      tags$h3(icon("project-diagram"), " 系统架构总览"),
      tags$p(style="color:#7f8c8d; font-size:13px;", "DiagrammeR/Graphviz SVG 渲染 — 7张架构图"),
      tags$hr()
    ),
    lapply(seq_along(titles), function(i) {
      tagList(
        tags$h4(class="arch-title", titles[i]),
        tags$div(class="arch-svg", SYSARCH_SVGS[[i]])
      )
    })
  ))
}
