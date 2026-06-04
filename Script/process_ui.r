# 审批模块 UI（企业微信风格）

process_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.appr-start-btn',function(e){e.preventDefault();Shiny.setInputValue('appr_start_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.appr-detail-btn',function(e){e.preventDefault();Shiny.setInputValue('appr_detail_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.appr-publish-btn',function(e){e.preventDefault();Shiny.setInputValue('appr_publish_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.appr-del-tpl-btn',function(e){e.preventDefault();if(confirm('确定删除此模板？'))Shiny.setInputValue('appr_del_tpl_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.appr-withdraw-btn',function(e){e.preventDefault();if(confirm('确定撤销此审批？'))Shiny.setInputValue('appr_withdraw_click',$(this).data('id'),{priority:'event'});});
      $(document).on('click','.appr-urge-btn',function(e){e.preventDefault();Shiny.setInputValue('appr_urge_click',$(this).data('id'),{priority:'event'});});
    ")),
    div(
      div(style="text-align:center;margin:8px 0;",
        h2(icon("clipboard-check")," 审批"),
        p(style="color:#7f8c8d;font-size:12px;","模板化审批 · 逐级流转 · 全程留痕")),
      uiOutput("appr_stat_cards"),
      br(),
      tabsetPanel(
        # ===== 待审批 =====
        tabPanel("待审批", icon=icon("bell"), br(),
          DT::DTOutput("appr_pending_table")),
        # ===== 我发起的 =====
        tabPanel("我发起的", icon=icon("paper-plane"), br(),
          fluidRow(
            column(2, selectInput("appr_my_status","状态",width="100%",
              choices=c("全部"="","审批中"="pending","已通过"="approved","已驳回"="rejected","已撤销"="withdrawn"),selected="")),
            column(2, div(style="margin-top:25px;",actionButton("appr_refresh_my","刷新",class="btn-info btn-sm")))
          ), br(),
          DT::DTOutput("appr_my_table")),
        # ===== 我处理的 =====
        tabPanel("我处理的", icon=icon("check-double"), br(),
          DT::DTOutput("appr_done_table")),
        # ===== 抄送我的 =====
        tabPanel("抄送我的", icon=icon("share"), br(),
          DT::DTOutput("appr_cc_table")),
        # ===== 审批模板 =====
        tabPanel("审批模板", icon=icon("sitemap"), br(),
          fluidRow(
            column(2, actionButton("appr_new_tpl","新建模板",class="btn-primary",icon=icon("plus"))),
            column(2, actionButton("appr_create_demo","创建示例模板",class="btn-success btn-sm",icon=icon("magic")),
                   div(style="margin-top:5px;font-size:11px;color:#999;","一键创建通用审批模板")),
            column(2, div(style="margin-top:0;",actionButton("appr_refresh_tpl","刷新",class="btn-info btn-sm")))
          ), br(),
          DT::DTOutput("appr_tpl_table"))
      )
    )
  )
}
