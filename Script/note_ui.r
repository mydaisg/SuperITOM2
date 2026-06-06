# 记事模块 UI - Trello 看板风格

note_ui <- function() {
  tagList(
    tags$script(HTML("
      $(document).on('click','.note-card',function(e){
        if ($(e.target).closest('button,a,.note-flag').length) return;
        Shiny.setInputValue('note_edit_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.note-move-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('note_move_click',{id:$(this).data('id'),to:$(this).data('to')},{priority:'event'});
      });
      $(document).on('click','.note-wo-btn',function(e){
        e.stopPropagation();
        if(confirm('转为工单？')) Shiny.setInputValue('note_to_wo_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.note-del-btn',function(e){
        e.stopPropagation();
        if(confirm('删除？')) Shiny.setInputValue('note_del_click',$(this).data('id'),{priority:'event'});
      });
      $(document).on('click','.note-report-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('note_report_click',$(this).data('id'),{priority:'event'});
      });
      Shiny.addCustomMessageHandler('noteEditMode', function(msg) {
        if (msg.mode === 'edit') {
          $('#note_content_ro').hide(); $('#note_content_ed').show();
          $('#note_toggle_edit').hide(); $('#note_cancel_edit').show(); $('#note_do_save').show();
          $('.comment-actions').show();
        } else {
          $('#note_content_ro').show(); $('#note_content_ed').hide();
          $('#note_toggle_edit').show(); $('#note_cancel_edit').hide(); $('#note_do_save').hide();
          $('.comment-actions').hide();
        }
      });
      Shiny.addCustomMessageHandler('noteInjectComment', function(msg) {
        var $comments = $('.modal-body .note-comment-list');
        if ($comments.length === 0) {
          var $nocomment = $('.modal-body .note-no-comment');
          if ($nocomment.length > 0) {
            $nocomment.replaceWith('<div class=\"note-comment-list\" style=\"max-height:500px; overflow-y:auto; margin-bottom:8px;\">' + msg.html + '</div>');
          }
        } else {
          $comments.append(msg.html);
          $comments.scrollTop($comments[0].scrollHeight);
        }
      });
      // 从弹窗移除评论
      Shiny.addCustomMessageHandler('noteRemoveComment', function(msg) {
        $('#comment-' + msg.comment_id).fadeOut(300, function() { $(this).remove(); });
      });
      // 评论状态标记
      Shiny.addCustomMessageHandler('noteCommentMarkDone', function(msg) {
        var $item = $('#comment-' + msg.comment_id);
        if (msg.status === 'completed') {
          var $nameSpan = $item.find('span[style*=\"font-weight:bold\"]');
          var badge = $item.find('.comment-status-badge');
          if (badge.length === 0 && $nameSpan.length > 0) {
            $nameSpan.after(' <span class=\"comment-status-badge\" style=\"background:#5cb85c; color:white; font-size:10px; padding:1px 6px; border-radius:10px; margin-left:6px;\">✅ 已完成 ' + msg.completed_at + '</span>');
          }
          // 切换按钮
          $item.find('.comment-done-btn').replaceWith('<button class=\"btn btn-xs btn-default comment-undone-btn\" data-id=\"' + msg.comment_id + '\">🔄</button>');
        } else {
          $item.find('.comment-status-badge').remove();
          $item.find('.comment-undone-btn').replaceWith('<button class=\"btn btn-xs btn-success comment-done-btn\" data-id=\"' + msg.comment_id + '\">✅</button>');
        }
      });
      // 评论标记完成按钮（直接标记，无需确认）
      $(document).on('click','.comment-done-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('note_comment_done', $(this).data('id'), {priority:'event'});
      });
      // 评论取消完成按钮
      $(document).on('click','.comment-undone-btn',function(e){
        e.stopPropagation();
        Shiny.setInputValue('note_comment_undone', $(this).data('id'), {priority:'event'});
      });
      // 评论编辑：显示编辑区
      $(document).on('click','.comment-edit-btn',function(e){
        e.stopPropagation();
        var $item = $(this).closest('.comment-item');
        $item.find('.comment-text').hide();
        $item.find('.comment-edit-area').show();
      });
      // 评论取消编辑
      $(document).on('click','.comment-cancel-btn',function(e){
        e.stopPropagation();
        var $item = $(this).closest('.comment-item');
        $item.find('.comment-text').show();
        $item.find('.comment-edit-area').hide();
      });
      // 评论保存
      $(document).on('click','.comment-save-btn',function(e){
        e.stopPropagation();
        var id = $(this).data('id');
        var text = $(this).closest('.comment-item').find('.comment-edit-input').val();
        Shiny.setInputValue('note_comment_edit', id+':'+text, {priority:'event'});
      });
      // 评论删除
      $(document).on('click','.comment-del-btn',function(e){
        e.stopPropagation();
        if (!confirm('删除此评论？')) return;
        Shiny.setInputValue('note_comment_delete', $(this).data('id'), {priority:'event'});
      });
    ")),
    tags$style(HTML("
      .trello-board { display:flex; gap:14px; overflow-x:auto; padding:10px 0; }
      .trello-col { flex:1; min-width:320px; background:#ebecf0; border-radius:8px; padding:12px; }
      .trello-col h4 { margin:0 0 10px; font-size:14px; font-weight:600; padding:4px 8px; border-radius:4px; }
      .trello-col.pending h4 { background:#fff3cd; color:#856404; }
      .trello-col.active  h4 { background:#d1ecf1; color:#0c5460; }
      .trello-col.done    h4 { background:#d4edda; color:#155724; }
      .note-card { background:white; border-radius:8px; padding:14px; margin-bottom:10px;
                   cursor:pointer; box-shadow:0 1px 3px rgba(0,0,0,0.12); 
                   transition:box-shadow 0.15s; }
      .note-card:hover { box-shadow:0 3px 10px rgba(0,0,0,0.25); }
      .note-card .note-title { font-size:14px; font-weight:600; margin-bottom:8px; color:#172b4d; }
      .note-card .note-body { font-size:13px; color:#5e6c84; max-height:400px; overflow-y:auto; 
                               white-space:pre-wrap; line-height:1.5; }
      .note-card .note-meta { font-size:11px; color:#999; margin-top:8px; display:flex; align-items:center; gap:8px; }
      .note-card .note-actions { margin-top:6px; display:flex; gap:4px; }
      .note-card .note-actions button { font-size:10px; padding:2px 6px; }
      .note-flag { color:#ccc; font-size:16px; cursor:pointer; text-decoration:none; margin-right:2px; }
      .note-flag.active { color:#d9534f; }
      .note-importance { color:#d9534f; font-size:14px; cursor:pointer; user-select:none; }
      .note-importance:hover { opacity:0.7; }
      .note-importance-empty { color:#ccc; font-size:14px; cursor:pointer; user-select:none; }
      .note-importance-empty:hover { color:#d9534f; }
      .note-due-overdue { color:#d9534f; font-weight:bold; }
      .comment-status-badge { font-size:10px; padding:1px 6px; border-radius:10px; margin-left:6px; white-space:nowrap; }
    ")),
    fluidPage(
      div(style="text-align:center;margin:8px 0 4px;",
        h2(icon("sticky-note")," 记事"),
        p(style="color:#7f8c8d;font-size:12px;","单框输入 · 首行为标题 · 拖拽式看板 · 红旗标记 · 时间提醒")),

      # 快捷添加
      wellPanel(
        textAreaInput("note_new_text", NULL, rows = 3, 
          placeholder = "输入内容，第一行自动作为标题…\n可换行写详细描述"),
        fluidRow(
          column(3, numericInput("note_reminder_hours", "⏰ 提醒(小时后)", value = 3, min = 0, max = 168, step = 1)),
          column(3, numericInput("note_due_hour", "📅 到期(几点)", value = 18, min = 0, max = 23, step = 1)),
          column(2, div(style="margin-top:20px;", actionButton("note_add","添加记事",class="btn-primary",icon=icon("plus"))))
        )
      ),

      # 看板
      uiOutput("note_board")
    )
  )
}
