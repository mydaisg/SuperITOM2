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
      // 回复按钮：显示/隐藏回复表单
      $(document).on('click','.comment-reply-btn',function(e){
        e.stopPropagation();
        var $item = $(this).closest('.comment-item');
        $item.find('.comment-reply-form').toggle();
        $item.find('.comment-reply-input').focus();
      });
      // 回复取消
      $(document).on('click','.comment-reply-cancel',function(e){
        e.stopPropagation();
        $(this).closest('.comment-reply-form').hide();
      });
      // 回复提交
      $(document).on('click','.comment-reply-submit',function(e){
        e.stopPropagation();
        var id = $(this).data('id');
        var text = $(this).closest('.comment-item').find('.comment-reply-input').val();
        if (!text || text.trim() === '') return;
        Shiny.setInputValue('note_reply_submit', {id: id, text: text}, {priority:'event'});
      });
      // 重新打开弹窗（回复后刷新嵌套结构）
      Shiny.addCustomMessageHandler('noteReopenModal', function(msg) {
        setTimeout(function() {
          Shiny.setInputValue('note_edit_click', msg.note_id, {priority: 'event'});
        }, 150);
      });
      // 搜索框回车触发搜索
      $(document).on('keypress', '#note_search_input', function(e) {
        if (e.which === 13) {
          e.preventDefault();
          $('#note_search_btn').click();
        }
      });
    ")),
    tags$style(HTML("
      .trello-board { display:flex; gap:12px; overflow-x:auto; padding:6px 0; }
      .trello-col { flex:1; min-width:320px; border-radius:10px; padding:10px; }
      .trello-col.pending { background:#f8f4ff; }
      .trello-col.active  { background:#f0f7ff; }
      .trello-col.done    { background:#f0faf5; }
      .trello-col h4 { margin:0 0 8px; font-size:13px; font-weight:700; padding:6px 10px; border-radius:6px;
                        letter-spacing:0.5px; display:flex; align-items:center; gap:6px; }
      .trello-col.pending h4 { background:#ede2ff; color:#6c3bbf; }
      .trello-col.active  h4 { background:#d6ebff; color:#2563eb; }
      .trello-col.done    h4 { background:#c7f0d8; color:#0d7d3a; }
      .note-card { background:white; border-radius:10px; padding:12px 14px; margin-bottom:8px;
                   cursor:pointer; border:1px solid #e8ecf1; 
                   transition:all 0.15s ease; position:relative; }
      .note-card:hover { border-color:#c8d4e0; box-shadow:0 2px 8px rgba(0,0,0,0.08); transform:translateY(-1px); }
      .note-card .note-title { font-size:13px; font-weight:700; margin-bottom:6px; color:#1a2236;
                                display:flex; align-items:center; gap:4px; }
      .note-card .note-body { font-size:12px; color:#596780; max-height:400px; overflow-y:auto;
                               white-space:pre-wrap; line-height:1.55; }
      .note-card .note-meta { font-size:10px; color:#a0aec0; margin-top:6px; display:flex; align-items:center; gap:8px; }
      .note-card .note-actions { margin-top:6px; display:flex; gap:4px; }
      .note-card .note-actions button { font-size:10px; padding:2px 6px; }
      .note-flag { color:#cbd5e0; font-size:16px; cursor:pointer; text-decoration:none; margin-right:2px; }
      .note-flag.active { color:#e53e3e; }
      .note-importance { color:#e53e3e; font-size:14px; cursor:pointer; user-select:none; }
      .note-importance:hover { opacity:0.7; }
      .note-importance-empty { color:#cbd5e0; font-size:14px; cursor:pointer; user-select:none; }
      .note-importance-empty:hover { color:#e53e3e; }
      .note-due-overdue { color:#e53e3e; font-weight:bold; }
      .comment-status-badge { font-size:10px; padding:1px 6px; border-radius:10px; margin-left:6px; white-space:nowrap; }
      .note-pin-icon { font-size:14px; cursor:pointer; margin-right:4px; opacity:0.25; transition:opacity 0.2s; }
      .note-pin-icon:hover { opacity:0.7; }
      .note-pin-icon.pinned { opacity:1; }
      .note-card-pinned { border-color:#f0c929; background:#fffef5; }
      .note-stat-box { text-align:center; border-radius:8px; padding:8px 6px; background:white; border:1px solid #e8ecf1; }
      .note-stat-box:hover { box-shadow:0 1px 4px rgba(0,0,0,0.06); }
      .note-stat-box .stat-num { font-size:22px; font-weight:800; line-height:1.2; }
      .note-stat-box .stat-lbl { font-size:10px; color:#8899aa; margin-top:2px; }
    ")),
    fluidPage(
      # 看板（含统计栏 + 搜索栏 + 创建表单 + 待处理分页）
      uiOutput("note_board")
    )
  )
}
