# 研发中心-IT部 工作模型 — Mermaid 流程图
# 在 server.R 中调用：output$itwm_chart <- renderUI({ it_work_model_mermaid() })
# 无需额外加载库，ui.R 已加载 mermaid@10 CDN

it_work_model_mermaid <- function() {
  code <- '
flowchart TB
  %% ===== 样式定义 =====
  classDef inspect fill:#3b82f6,stroke:#2563eb,color:#fff,stroke-width:2px
  classDef service fill:#ef4444,stroke:#dc2626,color:#fff,stroke-width:2px
  classDef problem fill:#f59e0b,stroke:#d97706,color:#fff,stroke-width:2px
  classDef project fill:#8b5cf6,stroke:#7c3aed,color:#fff,stroke-width:2px
  classDef launch  fill:#06b6d4,stroke:#0891b2,color:#fff,stroke-width:2px
  classDef handle  fill:#10b981,stroke:#059669,color:#fff,stroke-width:2px
  classDef node    fill:#e2e8f0,stroke:#94a3b8,color:#334155
  classDef done    fill:#22c55e,stroke:#16a34a,color:#fff,stroke-width:2px
  classDef entry   fill:#cffafe,stroke:#06b6d4,color:#0e7490

  %% ===== 巡检巡查 =====
  subgraph INSPECT ["🔍 IT巡检巡查"]
    direction TB
    INS_PLAN[制定巡检计划]:::node --> INS_EXEC[执行检查任务]:::node --> INS_FB[问题反馈]:::node
    INS_FB --> INS_DONE[完结]:::done
  end

  %% ===== 服务工单 =====
  subgraph SERVICE ["🎧 IT服务工单"]
    direction TB
    SRV_REQ[服务请求]:::entry --> SRV_CORE{工单处理中心}:::service
    SRV_EVT[IT事件]:::entry --> SRV_CORE
    SRV_DMD[IT需求]:::entry --> SRV_CORE
    SRV_CORE --> SRV_FB[作业反馈]:::node
    SRV_FB --> SRV_DELIVER[交付]:::done
  end

  %% ===== 问题管理 =====
  subgraph PROBLEM ["🔎 IT问题管理"]
    direction TB
    PRB_CORE{问题管理中心}:::problem
    PRB_CORE --> PRB_RESEARCH[调研分析]:::node --> PRB_PLAN[制定方案]:::node --> PRB_EXEC[执行验证]:::node
    PRB_EXEC --> PRB_CLOSE[结案]:::done
  end

  %% ===== 项目管理 =====
  subgraph PROJECT ["📐 IT项目管理"]
    direction TB
    PRJ_CORE{项目管理中心}:::project
    PRJ_CORE --> PRJ_SCOPE[目标范围]:::node --> PRJ_PHASE[阶段/工作包]:::node --> PRJ_TASK[任务分解]:::node --> PRJ_FB[作业反馈]:::node
    PRJ_FB --> PRJ_CLOSE[结案]:::done
  end

  %% ===== 流程·发起 =====
  subgraph LAUNCH ["📨 流程·发起"]
    direction LR
    FLW_PURCHASE[采购申请]:::node
    FLW_CONTRACT[合同]:::node
    FLW_TRAIN[培训]:::node
  end

  %% ===== 流程·处理 =====
  subgraph HANDLE ["⚙️ 流程·处理"]
    direction LR
    FLH_ACCOUNT[账号开通]:::node
    FLH_PERM[权限变更]:::node
    FLH_OPT[优化申请]:::node
  end

  %% ===== 跨模块关联 =====
  INS_FB -->|"转工单"| SRV_CORE
  INS_FB -->|"转问题"| PRB_CORE
  SRV_FB -->|"转问题"| PRB_CORE
  PRB_EXEC -->|"转项目"| PRJ_CORE
  PRJ_FB -->|"分解工单"| SRV_CORE
  SRV_CORE -.->|"驱动"| FLH_ACCOUNT

  FLW_PURCHASE -->|"审批"| FLH_ACCOUNT
  FLW_CONTRACT -->|"审批"| FLH_PERM
  FLW_TRAIN -->|"审批"| FLH_OPT
'

  tagList(
    tags$pre(class = "mermaid", id = "itwm_mermaid",
      style = "background:transparent; border:none; font-size:13px;",
      code),
    tags$script(HTML("
      setTimeout(function() {
        if (typeof mermaid !== 'undefined') {
          mermaid.run({ nodes: document.querySelectorAll('#itwm_mermaid') });
        }
      }, 100);
    "))
  )
}
