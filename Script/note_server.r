# 记事模块 - 服务端 v4 (编号 + footer按钮 + 评论编辑/删除)

note_server <- function(input, output, session, rv) {
  
  # 补充旧数据缺失的 note_no
  tryCatch({ note_fill_missing_no() }, error = function(e) message("[NOTE-INIT] 补充编号失败: ", e$message))
  
  note_trigger <- reactiveVal(0)
  note_pending_page <- reactiveVal(1)
  note_compact_mode <- reactiveVal(FALSE)  # 简约/详细切换
  note_search_term <- reactiveVal("")      # 搜索关键词
  note_filter <- reactiveVal("")           # 筛选: "reminder"/"due"/""
  note_search_history <- reactiveVal(character(0))  # 最近搜索记录（最多15条）
  note_search_freq <- reactiveVal(list())           # 搜索频次 {keyword: count}
  note_keywords_cache <- reactiveVal(character(0))  # TOP10关键字缓存
  
  ##################
  # Trello 看板
  ##################
  output$note_board <- renderUI({
    note_trigger()
    note_pending_page()
    note_compact_mode()
    note_search_term()
    note_filter()
    req(rv$logged_in)
    kw <- note_search_term()
    items <- if (is.null(kw) || kw == "") note_get_all(rv$current_user) else note_search(kw, rv$current_user)
    # 搜索关键字词列表（用于高亮）
    search_words <- if (!is.null(kw) && kw != "") strsplit(trimws(kw), "\\s+")[[1]] else character(0)
    search_words <- search_words[search_words != ""]
    # 关键字高亮辅助函数
    .hl <- function(text, words) {
      if (length(words) == 0 || is.null(text) || is.na(text)) return(text)
      for (w in words) {
        # 使用 gsubfn 或多次替换实现不区分大小写高亮
        pos <- gregexpr(w, text, ignore.case = TRUE)[[1]]
        if (pos[1] > 0) {
          parts <- c(); last <- 1
          for (i in seq_along(pos)) {
            p <- pos[i]; l <- attr(pos, "match.length")[i]
            if (p > last) parts <- c(parts, substr(text, last, p - 1))
            parts <- c(parts, '<mark style="background:#fde68a;color:#92400e;border-radius:3px;padding:0 2px;">',
              substr(text, p, p + l - 1), '</mark>')
            last <- p + l
          }
          if (last <= nchar(text)) parts <- c(parts, substr(text, last, nchar(text)))
          text <- paste(parts, collapse = "")
        }
      }
      text
    }
    # 提取匹配片段（内容匹配时显示关键词附近文字）
    .snippet <- function(text, words, radius = 40) {
      if (length(words) == 0 || is.null(text) || is.na(text)) return(substr(text, 1, radius * 2))
      best <- Inf
      for (w in words) {
        p <- regexpr(w, text, ignore.case = TRUE)[1]
        if (p > 0 && p < best) best <- p
      }
      if (best == Inf) return(substr(text, 1, radius * 2))
      start <- max(1, best - radius)
      end <- min(nchar(text), best + radius)
      snip <- substr(text, start, end)
      if (start > 1) snip <- paste0("…", snip)
      if (end < nchar(text)) snip <- paste0(snip, "…")
      snip
    }
    # 搜索时：标题命中优先排列
    if (length(search_words) > 0 && nrow(items) > 0) {
      items$match_title <- FALSE
      for (w in search_words) {
        items$match_title <- items$match_title | grepl(w, items$title, ignore.case = TRUE)
      }
      items <- items[order(items$match_title, decreasing = TRUE), ]
    }
    # 搜索时：拉取匹配关键字的评论（供卡片展示）
    matching_comments <- data.frame()
    if (length(search_words) > 0 && nrow(items) > 0) {
      matching_comments <- note_search_get_matching_comments(items$id, kw)
    }
    # 筛选：提醒/到期
    flt <- note_filter()
    if (flt == "reminder") {
      items <- items[!is.na(items$reminder_at) & items$reminder_at != "" & as.POSIXct(items$reminder_at) <= Sys.time(), , drop = FALSE]
    } else if (flt == "due") {
      items <- items[!is.na(items$due_at) & items$due_at != "" & as.POSIXct(items$due_at) < Sys.time(), , drop = FALSE]
    }
    compact <- note_compact_mode()
    



    build_col <- function(status, label, items, page = NULL, page_size = NULL) {
      subset <- if (nrow(items) > 0) items[items$status == status, ] else items
      cards <- list()
      if (nrow(subset) > 0) {
        # 分页：仅待处理列
        if (!is.null(page) && !is.null(page_size)) {
          total <- nrow(subset)
          total_pages <- ceiling(total / page_size)
          if (page > total_pages) page <- 1
          start <- (page - 1) * page_size + 1
          end <- min(page * page_size, total)
          subset <- subset[start:end, ]
        }
        for (i in 1:nrow(subset)) {
          r <- subset[i, ]
          imp_card <- r$importance %||% 0
          if (imp_card > 0) {
            flags <- sprintf('<span class="note-importance" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击切换重要性 (当前: %d)">%s</span>',
              r$id, imp_card, paste(rep("🚩", imp_card), collapse = ""))
          } else {
            flags <- sprintf('<span class="note-importance-empty" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击添加重要性">🏳</span>', r$id)
          }
          
          created <- if (!is.na(r$created_at) && nchar(r$created_at) > 10) substr(r$created_at, 1, 16) else r$created_at
          reminder <- if (!is.na(r$reminder_at) && nchar(r$reminder_at) > 10) substr(r$reminder_at, 1, 16) else ""
          due <- if (!is.na(r$due_at) && nchar(r$due_at) > 10) substr(r$due_at, 1, 16) else ""
          note_no <- r$note_no[1] %||% ""
          
          due_cls <- ""
          rem_overdue <- FALSE
          if (!is.na(r$reminder_at) && r$reminder_at != "") {
            rem_dt <- tryCatch(as.POSIXct(r$reminder_at), error = function(e) NULL)
            if (!is.null(rem_dt) && rem_dt < Sys.time()) rem_overdue <- TRUE
          }
          
          body <- r$content %||% ""
          body_lines <- strsplit(body, "\n")[[1]]
          if (length(body_lines) > 1) body <- paste(body_lines[-1], collapse = "\n") else body <- ""
          if (length(search_words) > 0) {
            body <- .snippet(body, search_words, 80)
          } else if (nchar(body) > 400) {
            body <- paste0(substr(body, 1, 400), "...")
          }
          
          # 搜索时：显示匹配关键字的评论；否则显示最新评论
          comment_html <- ""
          if (length(search_words) > 0 && nrow(matching_comments) > 0) {
            mc <- matching_comments[matching_comments$note_id == r$id, , drop = FALSE]
            if (nrow(mc) > 0) {
              parts <- c()
              for (mi in 1:min(nrow(mc), 2)) {
                mcc <- mc[mi, ]
                cn <- mcc$creator_name[1] %||% "匿名"
                ct <- .snippet(mcc$content[1], search_words, 60)
                ct <- .hl(ct, search_words)
                parts <- c(parts, sprintf('<div style="margin-bottom:2px;"><span style="color:#999;">%s:</span> <span>%s</span></div>', cn, ct))
              }
              if (nrow(mc) > 2) parts <- c(parts, sprintf('<div style="font-size:10px; color:#999;">... 还有 %d 条匹配评论</div>', nrow(mc) - 2))
              comment_html <- as.character(tags$div(style="font-size:11px; color:#5e6c84; margin-top:6px; padding:4px 6px; background:#f4f5f7; border-radius:3px;",
                HTML(paste(parts, collapse = ""))))
            }
          } else {
            last_comment <- note_comment_get_last(r$id, rv$current_user)
            if (!is.null(last_comment) && nrow(last_comment) > 0) {
              ct <- last_comment$content[1]; cn <- last_comment$creator_name[1] %||% "匿名"
              if (isTRUE(nchar(ct) > 80)) ct <- paste0(substr(ct, 1, 80), "...")
              comment_html <- as.character(tags$div(style="font-size:11px; color:#5e6c84; margin-top:6px; padding:4px 6px; background:#f4f5f7; border-radius:3px;",
                tags$span(style="color:#999;", cn, ": "), tags$span(ct)))
            }
          }
          
          pinned <- r$pinned[1] %||% 0
          pin_html <- if (pinned > 0) {
            sprintf('<span class="note-pin-icon pinned" onclick="Shiny.setInputValue(\'note_pin_click\',%d,{priority:\'event\'});event.stopPropagation();" title="已置顶，点击取消">📌</span>', r$id)
          } else {
            sprintf('<span class="note-pin-icon" onclick="Shiny.setInputValue(\'note_pin_click\',%d,{priority:\'event\'});event.stopPropagation();" title="点击置顶">📌</span>', r$id)
          }
          # 超时判定：待处理>8h，进行中>4h，时间标红
          stale_cls <- ""
          if (status == "pending" || status == "in_progress") {
            updated_dt <- tryCatch(as.POSIXct(r$updated_at), error = function(e) NULL)
            if (!is.null(updated_dt)) {
              hours_ago <- as.numeric(difftime(Sys.time(), updated_dt, units = "hours"))
              if ((status == "pending" && hours_ago > 8) || (status == "in_progress" && hours_ago > 4)) {
                stale_cls <- "note-stale-time"
              }
            }
          }
          card_class <- if (pinned > 0) "note-card note-card-pinned" else "note-card"
          cards[[i]] <- tags$div(class = card_class, `data-id` = r$id,
            tags$div(class = "note-title",
              HTML(pin_html),
              if (isTRUE(note_no != "")) tags$span(style="color:#337ab7;font-size:11px;margin-right:6px;", note_no),
              HTML(.hl(r$title, search_words)), " ", HTML(flags)),
            if (!compact && isTRUE(nchar(body) > 0)) tags$div(class = "note-body", HTML(.hl(body, search_words))) else "",
            if (!compact) tags$div(class = "note-meta", style = "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
              tags$span(if (stale_cls != "") class = stale_cls else NULL,
                if (stale_cls != "") tags$span(style="color:#e53e3e;font-weight:bold;", "🕐", created) else tags$span("🕐", created)),
              if (isTRUE(reminder != "")) tags$span(class = if(rem_overdue) "note-due-overdue" else "", "⏰", reminder,
                HTML(sprintf('<a href="#" onclick="Shiny.setInputValue(\'note_cancel_reminder_btn\',%d,{priority:\'event\'});return false;" style="color:#999;font-size:9px;margin-left:1px;text-decoration:none;">✕</a>', r$id)),
                HTML(sprintf('<a href="#" onclick="Shiny.setInputValue(\'note_extend_due_btn\',%d,{priority:\'event\'});return false;" style="color:#2563eb;font-size:9px;margin-left:2px;text-decoration:none;">Ext 1D</a>', r$id))),
              if (isTRUE(due != "")) tags$span("📅", due)
            ),
            if (!compact) HTML(comment_html) else ""
          )
        }
      }
      do.call(tagList, cards)
    }

    # 已完成列专用：按月分组，本月展开，往月收缩
    build_done_col <- function(items) {
      subset <- items[items$status == "completed", ]
      if (nrow(subset) == 0) return(tagList())
      now_month <- format(Sys.Date(), "%Y-%m")
      subset$month <- substr(subset$updated_at, 1, 7)
      months <- unique(subset$month)
      months <- sort(months, decreasing = TRUE)

      month_groups <- lapply(months, function(mo) {
        grp <- subset[subset$month == mo, ]
        is_current <- (mo == now_month)
        grp_id <- paste0("ndone-", gsub("-","",mo))

        header <- tags$div(
          class = "note-card", style = "padding:8px 12px; background:#e8f5e9; border:1px solid #c8e6c9; cursor:pointer; margin-bottom:6px;",
          onclick = sprintf("var d=document.getElementById('%s');d.style.display=d.style.display==='none'?'block':'none';", grp_id),
          tags$div(style = "font-size:12px; font-weight:600; color:#2e7d32;",
            "📦 记事-已完成-", format(as.Date(paste0(mo,"-01")), "%Y年%m月"),
            sprintf(" (%d条)", nrow(grp)),
            if (!is_current) tags$span(" ▸", style = "float:right;") else tags$span(" ▾", style = "float:right;")
          )
        )

        body <- tags$div(id = grp_id,
          style = if (is_current) "display:block;" else "display:none;",
          lapply(1:nrow(grp), function(i) {
            r <- grp[i, ]
            imp_card <- r$importance %||% 0
            if (imp_card > 0) {
              flags <- sprintf('<span class="note-importance" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击切换重要性 (当前: %d)">%s</span>',
                r$id, imp_card, paste(rep("🚩", imp_card), collapse = ""))
            } else {
              flags <- sprintf('<span class="note-importance-empty" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击添加重要性">🏳</span>', r$id)
            }
            pinned <- r$pinned[1] %||% 0
            pin_html <- if (pinned > 0) {
              sprintf('<span class="note-pin-icon pinned" onclick="Shiny.setInputValue(\'note_pin_click\',%d,{priority:\'event\'});event.stopPropagation();" title="已置顶，点击取消">📌</span>', r$id)
            } else {
              sprintf('<span class="note-pin-icon" onclick="Shiny.setInputValue(\'note_pin_click\',%d,{priority:\'event\'});event.stopPropagation();" title="点击置顶">📌</span>', r$id)
            }
            card_class <- if (pinned > 0) "note-card note-card-pinned" else "note-card"
            note_no <- r$note_no[1] %||% ""
            body <- r$content %||% ""
            body_lines <- strsplit(body, "\n")[[1]]
            if (length(body_lines) > 1) body <- paste(body_lines[-1], collapse = "\n") else body <- ""
            if (length(search_words) > 0) {
              body <- .snippet(body, search_words, 80)
            } else if (nchar(body) > 400) {
              body <- paste0(substr(body, 1, 400), "...")
            }
            # 搜索时：显示匹配评论；否则显示最新评论
            comment_html <- ""
            if (length(search_words) > 0 && nrow(matching_comments) > 0) {
              mc <- matching_comments[matching_comments$note_id == r$id, , drop = FALSE]
              if (nrow(mc) > 0) {
                parts <- c()
                for (mi in 1:min(nrow(mc), 2)) {
                  mcc <- mc[mi, ]
                  cn <- mcc$creator_name[1] %||% "匿名"
                  ct <- .snippet(mcc$content[1], search_words, 60)
                  ct <- .hl(ct, search_words)
                  parts <- c(parts, sprintf('<div style="margin-bottom:2px;"><span style="color:#999;">%s:</span> <span>%s</span></div>', cn, ct))
                }
                if (nrow(mc) > 2) parts <- c(parts, sprintf('<div style="font-size:10px; color:#999;">... 还有 %d 条匹配评论</div>', nrow(mc) - 2))
                comment_html <- as.character(tags$div(style="font-size:11px; color:#5e6c84; margin-top:6px; padding:4px 6px; background:#f4f5f7; border-radius:3px;",
                  HTML(paste(parts, collapse = ""))))
              }
            } else {
              last_comment <- note_comment_get_last(r$id, rv$current_user)
              if (!is.null(last_comment) && nrow(last_comment) > 0) {
                ct <- last_comment$content[1]; cn <- last_comment$creator_name[1] %||% "匿名"
                if (isTRUE(nchar(ct) > 80)) ct <- paste0(substr(ct, 1, 80), "...")
                comment_html <- as.character(tags$div(style="font-size:11px; color:#5e6c84; margin-top:6px; padding:4px 6px; background:#f4f5f7; border-radius:3px;",
                  tags$span(style="color:#999;", cn, ": "), tags$span(ct)))
              }
            }
            tags$div(class = card_class, `data-id` = r$id,
              tags$div(class = "note-title",
                HTML(pin_html),
                if (isTRUE(note_no != "")) tags$span(style="color:#337ab7;font-size:11px;margin-right:6px;", note_no),
                HTML(.hl(r$title, search_words)), " ", HTML(flags)),
              if (!compact && isTRUE(nchar(body) > 0)) tags$div(class="note-body", HTML(.hl(body, search_words))) else "",
              if (!compact) tags$div(class="note-meta", style = "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                tags$span("🕐", substr(r$created_at,1,16)),
                if (!is.na(r$reminder_at) && nchar(r$reminder_at) > 10) tags$span("⏰", substr(r$reminder_at,1,16),
                  HTML(sprintf('<a href="#" onclick="Shiny.setInputValue(\'note_cancel_reminder_btn\',%d,{priority:\'event\'});return false;" style="color:#999;font-size:9px;margin-left:1px;text-decoration:none;">✕</a>', r$id)),
                  HTML(sprintf('<a href="#" onclick="Shiny.setInputValue(\'note_extend_due_btn\',%d,{priority:\'event\'});return false;" style="color:#2563eb;font-size:9px;margin-left:2px;text-decoration:none;">Ext 1D</a>', r$id))),
                if (!is.na(r$due_at) && nchar(r$due_at) > 10) tags$span("📅", substr(r$due_at,1,16))
              ),
              if (!compact) HTML(comment_html) else ""
            )
          })
        )
        tagList(header, body)
      })
      do.call(tagList, unlist(month_groups, recursive = FALSE))
    }
    
    # 待处理：前4条 + 创建框 + 全部剩余（无分页）
    pending_count <- sum(items$status == "pending", na.rm = TRUE)
    PGSZ <- 10  # 保留给搜索等模式使用

    # 创建表单（紧凑，提醒/到期为默认值不显示）
    create_form <- tags$div(style = "background:white; border-radius:10px; padding:10px; margin:6px 0; border:1px solid #e8ecf1;",
      tags$div(style = "font-size:11px; font-weight:700; color:#6c3bbf; margin-bottom:4px;", "✦ 快速添加"),
      textAreaInput("note_new_text", NULL, rows = 4, width = "100%",
        placeholder = "输入内容，第一行自动作为标题…"),
      div(style = "text-align:right; border-top:1px solid #f0f0f5; padding-top:6px; margin-top:4px;",
        # 隐藏的默认值（不显示但供 server 读取）
        div(style="display:none;",
          numericInput("note_reminder_hours", NULL, value=3, min=0, max=168, step=1),
          numericInput("note_due_hour", NULL, value=18, min=0, max=23, step=1)
        ),
        actionButton("note_add", "添加记事", class = "btn-primary btn-sm", icon = icon("plus")))
    )

    # 分割待处理：前4 + 其余
    pending_subset <- items[items$status == "pending", , drop = FALSE]
    pending_top <- if (nrow(pending_subset) > 4) pending_subset[1:4, , drop = FALSE] else pending_subset
    pending_rest <- if (nrow(pending_subset) > 4) pending_subset[5:nrow(pending_subset), , drop = FALSE] else data.frame()
    
    # 统计栏（统一白底+彩色数字）
    reminder_count <- sum(!is.na(items$reminder_at) & items$reminder_at != "" & as.POSIXct(items$reminder_at) <= Sys.time(), na.rm = TRUE)
    due_count <- sum(!is.na(items$due_at) & items$due_at != "" & as.POSIXct(items$due_at) < Sys.time(), na.rm = TRUE)

    stats_bar <- tags$div(style = "display:flex; gap:10px; margin-bottom:10px;",
      tags$div(class = "note-stat-box", style = "flex:1; cursor:pointer;",
        onclick = sprintf("Shiny.setInputValue('note_filter_click','%s',{priority:'event'})", if(flt=="") "reminder" else ""),
        tags$div(class = "stat-num", style = paste("color:#6c3bbf;", if(flt=="reminder") "background:#ede2ff;border-radius:4px;" else ""), nrow(items)),
        tags$div(class = "stat-lbl", "全部")
      ),
      tags$div(class = "note-stat-box", style = "flex:1;",
        tags$div(class = "stat-num", style = "color:#6c3bbf;", pending_count),
        tags$div(class = "stat-lbl", "待处理")
      ),
      tags$div(class = "note-stat-box", style = "flex:1;",
        tags$div(class = "stat-num", style = "color:#2563eb;", sum(items$status == "in_progress", na.rm = TRUE)),
        tags$div(class = "stat-lbl", "进行中")
      ),
      tags$div(class = "note-stat-box", style = "flex:1;",
        tags$div(class = "stat-num", style = "color:#0d7d3a;", sum(items$status == "completed", na.rm = TRUE)),
        tags$div(class = "stat-lbl", "已完成")
      ),
      if (reminder_count > 0) tags$div(class = "note-stat-box", style = "flex:1; cursor:pointer;",
        onclick = sprintf("Shiny.setInputValue('note_filter_click','%s',{priority:'event'})", if(flt=="reminder") "" else "reminder"),
        tags$div(class = "stat-num", style = paste("color:#f59e0b;", if(flt=="reminder") "background:#fef3c7;border-radius:4px;" else ""),
          tags$span(class = if(flt!="reminder") "note-blink" else "", reminder_count)),
        tags$div(class = "stat-lbl", "⏰ 提醒")
      ),
      if (due_count > 0) tags$div(class = "note-stat-box", style = "flex:1; cursor:pointer;",
        onclick = sprintf("Shiny.setInputValue('note_filter_click','%s',{priority:'event'})", if(flt=="due") "" else "due"),
        tags$div(class = "stat-num", style = paste("color:#e53e3e;", if(flt=="due") "background:#fee2e2;border-radius:4px;" else ""),
          tags$span(class = if(flt!="due") "note-blink" else "", due_count)),
        tags$div(class = "stat-lbl", "📅 到期")
      )
    )
    
    tagList(
      # 一行：统计 | 搜索 | 简约
      tags$div(style = "display:flex; gap:10px; align-items:center; margin-bottom:10px;",
        tags$div(style = "flex:1;", stats_bar),
        tags$div(style = "display:flex; gap:4px; align-items:center; flex-shrink:0;",
          if (kw != "") tags$span(style = "font-size:10px; color:#999; white-space:nowrap;",
            sprintf("「%s」%d条", kw, nrow(items))),
          textInput("note_search_input", NULL, width = "140px",
            placeholder = "搜索标题/评论…"),
          actionButton("note_search_btn", NULL, icon = icon("search"),
            class = "btn-sm btn-primary"),
          if (kw != "") actionButton("note_search_clear_btn", NULL, icon = icon("times"),
            class = "btn-sm btn-default")
        ),
        tags$button(class = "btn btn-sm btn-outline-secondary",
          onclick = "Shiny.setInputValue('note_toggle_compact', Math.random(), {priority:'event'})",
          if (compact) "📋 详细" else "📋 简约")
      ),
      # 快速关键字栏（TOP10标题关键词 | TOP5高频搜索）
      if (nrow(items) > 0) {
        cat_kw <- note_keywords_cache()
        # 历史搜索频次 → 取 TOP5（排除已出现在标题关键字中的）
        freq <- note_search_freq()
        if (length(freq) > 0) {
          freq_sorted <- sort(unlist(freq), decreasing = TRUE)
          hist_kw <- names(freq_sorted)[seq_len(min(5, length(freq_sorted)))]
          hist_kw <- setdiff(hist_kw, cat_kw)
        } else {
          hist_kw <- character(0)
        }
        kw_btn <- function(label, cls) {
          sprintf('<span class="note-kw-tag %s" onclick="Shiny.setInputValue(\'note_kw_click\',\'%s\',{priority:\'event\'})">%s <a href="#" onclick="event.stopPropagation();Shiny.setInputValue(\'note_kw_del\',\'%s\',{priority:\'event\'});return false;" style="color:#999;text-decoration:none;">✕</a></span>',
            cls, label, label, label)
        }
        if (length(cat_kw) > 0) {
          cats <- paste(sapply(cat_kw[1:min(10,length(cat_kw))], function(x) kw_btn(x, "kw-cat")), collapse = "")
        } else cats <- ""
        if (length(hist_kw) > 0) {
          hists <- paste(sapply(hist_kw, function(x) kw_btn(x, "kw-hist")), collapse = "")
        } else hists <- ""
        if (cats != "" || hists != "") {
          tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
            tags$div(style = "display:flex; gap:4px; flex-wrap:wrap; align-items:center;",
              if (cats != "") tags$span(style="font-size:10px;color:#999;margin-right:2px;","分类:"),
              if (cats != "") HTML(cats)),
            tags$div(style = "display:flex; gap:4px; flex-wrap:wrap; align-items:center;",
              if (hists != "") tags$span(style="font-size:10px;color:#999;margin-right:2px;","高频:"),
              if (hists != "") HTML(hists)))
        }
      },
      tags$div(class = "trello-board",
        tags$div(class = "trello-col pending",
          tags$h4(sprintf("📋 待处理 (%d)", pending_count)),
          # 搜索模式：全部显示；正常模式：前4 + 创建框 + 全部
          if (kw != "" || flt != "") {
            build_col("pending", "待处理", items)
          } else {
            tagList(
              if (nrow(pending_top) > 0) build_col("pending", "待处理", pending_top),
              create_form,
              if (nrow(pending_rest) > 0) build_col("pending", "待处理", pending_rest) else ""
            )
          },
        ),
      tags$div(class = "trello-col active",
        tags$h4(sprintf("🔄 进行中 (%d)", sum(items$status == "in_progress", na.rm = TRUE))),
        build_col("in_progress", "进行中", items)
      ),
      tags$div(class = "trello-col done",
        tags$h4(sprintf("✅ 已完成 (%d)", sum(items$status == "completed", na.rm = TRUE))),
        build_done_col(items)
      )
    )
  )  # closes tagList(stats_bar, trello-board)
  })
  
  ##################
  # 初始化时间默认值
  ##################
  observe({
    req(rv$logged_in)
    updateNumericInput(session, "note_reminder_hours",
      value = as.numeric(config_get_value("note_reminder_hours", "3")))
    updateNumericInput(session, "note_due_hour",
      value = as.integer(config_get_value("note_due_hour", "18")))
  })

  ##################
  # 卡片快捷操作：取消提醒 / 延长到期
  ##################
  observeEvent(input$note_cancel_reminder_btn, {
    req(rv$logged_in)
    result <- note_cancel_reminder(as.integer(input$note_cancel_reminder_btn), rv$current_user)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = if(result$success) "message" else "warning", duration = 1.5)
  })
  observeEvent(input$note_extend_due_btn, {
    req(rv$logged_in)
    result <- note_extend_due(as.integer(input$note_extend_due_btn), rv$current_user)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = if(result$success) "message" else "warning", duration = 1.5)
  })

  ##################
  # 置顶切换
  ##################
  observeEvent(input$note_pin_click, {
    req(rv$logged_in)
    result <- note_toggle_pin(as.integer(input$note_pin_click), rv$current_user)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "warning"), duration = 2)
  })

  ##################
  # 统计栏点击筛选：提醒/到期
  ##################
  observeEvent(input$note_filter_click, {
    req(rv$logged_in)
    val <- input$note_filter_click
    if (is.null(val) || val == "" || val == "reminder" || val == "due") {
      note_filter(val)
      note_pending_page(1)
    }
  })

  ##################
  # 搜索（点击按钮触发，输入框在右侧栏）
  ##################
  observeEvent(input$note_search_btn, {
    req(rv$logged_in)
    kw <- trimws(input$note_search_input %||% "")
    note_search_term(kw)
    note_pending_page(1)
    # 保存到搜索历史（去重，最多15条） + 频次
    if (kw != "") {
      hist <- note_search_history()
      hist <- unique(c(kw, hist))
      if (length(hist) > 15) hist <- hist[1:15]
      note_search_history(hist)
      freq <- note_search_freq()
      freq[[kw]] <- (freq[[kw]] %||% 0L) + 1L
      note_search_freq(freq)
    }
  })

  observeEvent(input$note_search_clear_btn, {
    req(rv$logged_in)
    note_search_term("")
    note_pending_page(1)
    updateTextInput(session, "note_search_input", value = "")
  })

  # 恢复搜索框值（renderUI 重渲染会清空 input，需要同时响应 note_trigger 和 search_term 变化）
  observe({
    note_trigger()
    req(rv$logged_in)
    kw <- note_search_term()
    if (!is.null(kw) && kw != "") {
      updateTextInput(session, "note_search_input", value = kw)
    }
  })

  # 关键字缓存（首次加载或刷新时更新TOP10）
  observe({
    note_trigger()
    req(rv$logged_in)
    if (length(note_keywords_cache()) == 0) {
      tryCatch({ note_keywords_cache(note_get_top_keywords(10, rv$current_user)) }, error = function(e) NULL)
    }
  })

  # 关键字点击 → 设搜索词触发筛选
  observeEvent(input$note_kw_click, {
    req(rv$logged_in, input$note_kw_click)
    kw <- trimws(input$note_kw_click)
    if (kw == "") return()
    note_search_term(kw)
    note_pending_page(1)
    # 加入搜索历史（去重，最多15条） + 频次
    hist <- note_search_history()
    hist <- unique(c(kw, hist))
    if (length(hist) > 15) hist <- hist[1:15]
    note_search_history(hist)
    freq <- note_search_freq()
    freq[[kw]] <- (freq[[kw]] %||% 0L) + 1L
    note_search_freq(freq)
  })

  # 关键字 X 删除
  observeEvent(input$note_kw_del, {
    req(rv$logged_in, input$note_kw_del)
    del <- trimws(input$note_kw_del)
    note_search_history(setdiff(note_search_history(), del))
    # 清除频次记录
    freq <- note_search_freq()
    freq[[del]] <- NULL
    note_search_freq(freq)
    # 也尝试清除分类缓存（如果删除的是分类关键字）
    kw <- note_keywords_cache()
    if (del %in% kw) {
      note_keywords_cache(c(setdiff(kw, del), note_get_top_keywords(1, rv$current_user)))
    }
  })

  # 刷新关键字缓存
  observeEvent(input$note_kw_refresh, {
    req(rv$logged_in)
    tryCatch({ note_keywords_cache(note_get_top_keywords(10, rv$current_user)) }, error = function(e) NULL)
  })

  ##################
  # 简约/详细切换
  ##################
  observeEvent(input$note_toggle_compact, {
    note_compact_mode(!note_compact_mode())
  })

  ##################
  # 待处理分页翻页
  ##################
  observeEvent(input$note_pending_page_btn, {
    req(rv$logged_in)
    cp <- note_pending_page()
    if (input$note_pending_page_btn == "prev") {
      note_pending_page(max(1, cp - 1))
    } else if (input$note_pending_page_btn == "next") {
      note_pending_page(cp + 1)
    }
  })

  ##################
  # 添加记事
  ##################
  observeEvent(input$note_add, {
    req(rv$logged_in, input$note_new_text)
    if (trimws(input$note_new_text) == "") return()
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- note_add(input$note_new_text, uid,
      reminder_hours = input$note_reminder_hours,
      due_hour = input$note_due_hour)
    if (result$success) {
      updateTextAreaInput(session, "note_new_text", value = "")
      note_pending_page(1)  # 添加后跳到第一页看新卡片
    }
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  ##################
  # 编辑弹窗 v4：评论不关 + footer按钮 + 编号 + 评论编辑/删除
  ##################
  observeEvent(input$note_edit_click, {
    req(rv$logged_in)
    note <- note_get_by_id(as.integer(input$note_edit_click), rv$current_user)
    if (is.null(note) || nrow(note) == 0) return()
    rv$note_edit_id <- note$id[1]
    
    note_no <- note$note_no[1] %||% sprintf("NTE%s%03d", format(Sys.Date(),"%Y%m%d"), note$id[1])
    rem_val <- note$reminder_at[1] %||% ""
    due_val <- note$due_at[1] %||% ""
    if (isTRUE(nchar(rem_val) > 16)) rem_val <- substr(rem_val, 1, 16)
    if (isTRUE(nchar(due_val) > 16)) due_val <- substr(due_val, 1, 16)
    st <- note$status[1]; imp <- note$importance[1] %||% 0
    
    # 小旗：项目风格，直接点击切换
    if (imp > 0) {
      flags_html <- sprintf('<span class="note-importance" style="font-size:18px;cursor:pointer;" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击切换 (当前: %d)">%s</span>',
        note$id[1], imp, paste(rep("🚩", imp), collapse = ""))
    } else {
      flags_html <- sprintf('<span class="note-importance-empty" style="font-size:18px;cursor:pointer;" onclick="Shiny.setInputValue(\'note_flag_click\', %d, {priority:\'event\'}); event.stopPropagation();" title="点击添加重要性">🏳</span>', note$id[1])
    }
    
    # 状态按钮 HTML（放 footer）
    status_btns <- ""
    if (st == "pending") {
      status_btns <- sprintf('<button class="btn btn-info btn-sm note-move-btn" data-id="%d" data-to="in_progress" style="margin-right:4px;">▶ 开始处理</button>', note$id[1])
    } else if (st == "in_progress") {
      status_btns <- sprintf('<button class="btn btn-success btn-sm note-move-btn" data-id="%d" data-to="completed" style="margin-right:4px;">✓ 完成</button>
                              <button class="btn btn-warning btn-sm note-move-btn" data-id="%d" data-to="pending" style="margin-right:4px;">◀ 退回</button>', note$id[1], note$id[1])
    } else {
      status_btns <- sprintf('<button class="btn btn-warning btn-sm note-move-btn" data-id="%d" data-to="pending" style="margin-right:4px;">◀ 重新打开</button>', note$id[1])
    }
    
    # 彩虹评论（嵌套 + 状态徽章 + 编辑/删除/标记/回复）
    rainbow_colors <- c("#e74c3c","#e67e22","#f1c40f","#2ecc71","#1abc9c","#3498db","#9b59b6","#e91e63")
    comments <- note_comment_get_all(note$id[1], rv$current_user)
    comment_html <- ""
    if (!is.null(comments) && nrow(comments) > 0) {
      # 分离顶层和子评论
      tops <- comments[is.na(comments$parent_id) | comments$parent_id == 0, ]
      children <- comments[!(is.na(comments$parent_id) | comments$parent_id == 0), ]
      
      render_one_comment <- function(c, children_df, level = 0, counter = 1) {
        cn <- c$creator_name[1] %||% "匿名"
        ca <- if (!is.na(c$created_at) && nchar(c$created_at) > 10) substr(c$created_at, 1, 16) else c$created_at
        clr <- rainbow_colors[((counter - 1) %% length(rainbow_colors)) + 1]
        cs <- if (is.na(c$status[1]) || is.null(c$status[1])) "" else c$status[1]
        completed_at <- c$completed_at[1] %||% ""
        status_badge <- if (isTRUE(cs == "completed")) {
          if (isTRUE(nchar(completed_at) > 10)) completed_at <- substr(completed_at, 1, 16)
          if (isTRUE(completed_at != "")) {
            sprintf(' <span class="comment-status-badge" style="background:#5cb85c; color:white; font-size:10px; padding:1px 6px; border-radius:10px; margin-left:6px;">✅ 已完成 %s</span>', completed_at)
          } else {
            ' <span class="comment-status-badge" style="background:#5cb85c; color:white; font-size:10px; padding:1px 6px; border-radius:10px; margin-left:6px;">✅ 已完成</span>'
          }
        } else ""
        mark_btn <- if (!isTRUE(cs == "completed")) {
          sprintf('<button class="btn btn-xs btn-success comment-done-btn" data-id="%d">✅</button>', c$id)
        } else {
          sprintf('<button class="btn btn-xs btn-default comment-undone-btn" data-id="%d">🔄</button>', c$id)
        }
        reply_btn <- ''
        if (level == 0) {
          reply_btn <- sprintf('<button class="btn btn-xs btn-default comment-reply-btn" data-id="%d" data-name="%s">💬 回复</button>', c$id, cn)
        }
        indent <- if (level > 0) sprintf("margin-left:%dpx;", level * 24) else ""
        
        # 子评论
        sub_html <- ""
        if (!is.null(children_df) && nrow(children_df) > 0) {
          subs <- children_df[children_df$parent_id == c$id, ]
          if (nrow(subs) > 0) {
            sub_parts <- c()
            for (si in 1:nrow(subs)) {
              sub_parts <- c(sub_parts, render_one_comment(subs[si, ], children_df, level + 1, counter + si))
            }
            sub_html <- paste(sub_parts, collapse = "")
          }
        }
        
        base <- sprintf(
          '<div class="comment-item" id="comment-%d" style="background:#fafafa; padding:6px 10px; margin-bottom:4px; border-radius:6px; border-left:4px solid %s;%s">
            <div style="display:flex; justify-content:space-between; align-items:flex-start;">
              <div style="flex:1; min-width:0;">
                <div style="font-size:11px; color:#999; margin-bottom:3px;">
                  <span style="font-weight:bold; color:%s;">%s</span>%s
                  <span style="margin-left:8px;">%s</span>
                </div>
                <div class="comment-text" style="font-size:13px; line-height:1.5; white-space:pre-wrap; word-break:break-word;">%s</div>
                <div class="comment-edit-area" style="display:none; margin-top:4px;">
                  <textarea class="form-control comment-edit-input" style="font-size:13px;" rows="2">%s</textarea>
                  <div style="margin-top:3px;">
                    <input class="form-control comment-edit-time" style="font-size:11px; width:150px; display:inline;" value="%s" placeholder="时间">
                    <button class="btn btn-xs btn-primary comment-save-btn" style="margin-top:2px;" data-id="%d">保存</button>
                    <button class="btn btn-xs btn-default comment-cancel-btn" style="margin-top:2px;">取消</button>
                  </div>
                </div>
                %s
              </div>
              <div class="comment-actions" style="margin-left:8px; white-space:nowrap; flex-shrink:0;">
                %s%s
                <button class="btn btn-xs btn-info comment-edit-btn" data-id="%d">✏</button>
                <button class="btn btn-xs btn-danger comment-del-btn" data-id="%d">🗑</button>
              </div>
            </div>
            %s
          </div>',
          c$id, clr, indent, clr, cn, status_badge, ca,
          c$content, c$content, ca, c$id,
          sprintf('<div class="comment-reply-form" style="display:none; margin-top:6px;"><textarea class="form-control comment-reply-input" rows="2" placeholder="回复 %s ..." style="font-size:12px;"></textarea><button class="btn btn-xs btn-primary comment-reply-submit" data-id="%d" style="margin-top:3px;">回复</button><button class="btn btn-xs btn-default comment-reply-cancel" style="margin-top:3px;">取消</button></div>', cn, c$id),
          mark_btn, reply_btn, c$id, c$id,
          sub_html
        )
        base
      }
      
      parts <- c()
      if (nrow(tops) > 0) {
        for (ti in 1:nrow(tops)) {
          parts <- c(parts, render_one_comment(tops[ti, ], children, 0, ti))
        }
      }
      comment_html <- paste(parts, collapse = "")
    }
    
    modal_body <- tagList(
      # 编号 + 状态/时间 + 小旗 + 阅读/关闭（同行）
      tags$div(style="background:#f5f5f5; padding:8px 10px; border-radius:6px; margin-bottom:10px;",
        tags$div(style="display:flex; justify-content:space-between; align-items:center; flex-wrap:nowrap;",
          tags$div(style="display:flex; align-items:center; gap:12px; flex-wrap:nowrap; white-space:nowrap; overflow:hidden;",
            tags$b(style="color:#337ab7; font-size:15px;", note_no),
            tags$span(style="font-size:12px; color:#999;",
              sprintf("状态: %s | 创建: %s", st, substr(note$created_at[1] %||% "", 1, 16))),
            tags$span(style="font-size:12px; color:#999;",
              "⏰ 提醒: ", rem_val,
              if (isTRUE(rem_val != "")) HTML(sprintf('<a href="#" onclick="Shiny.setInputValue(\'note_cancel_reminder_btn\',%d,{priority:\'event\'});return false;" style="color:#e53e3e;font-size:10px;margin-left:2px;text-decoration:none;">✕</a>', note$id[1])),
              if (isTRUE(rem_val != "")) HTML(sprintf('<a href="#" onclick="Shiny.setInputValue(\'note_extend_due_btn\',%d,{priority:\'event\'});return false;" style="color:#2563eb;font-size:10px;margin-left:2px;text-decoration:none;">Ext 1D</a>', note$id[1]))),
            tags$span(style="font-size:12px; color:#999;",
              "📅 到期: ", due_val)
          ),
          tags$div(style="display:flex; align-items:center; gap:6px; flex-shrink:0;",
            HTML(flags_html)
          )
        )
      ),
      
      # 内容只读（初始隐藏）
      tags$div(id = "note_content_ro", style="display:none;",
        tags$div(style="font-size:15px; font-weight:bold; margin-bottom:6px;", note$title[1]),
        tags$div(style="font-size:13px; color:#333; white-space:pre-wrap; line-height:1.6; max-height:150px; overflow-y:auto;",
          note$content[1] %||% "")
      ),
      
      # 内容编辑（默认可见）
      tags$div(id = "note_content_ed",
        fluidRow(
          column(8,
            textInput("note_edit_no_m", "编号", value = note_no),
            textAreaInput("note_edit_content_m", "内容（首行为标题）", rows = 5, value = note$content[1] %||% "")
          ),
          column(4,
            textInput("note_edit_created_m", "创建时间", value = substr(note$created_at[1] %||% format(Sys.time(),"%Y-%m-%d %H:%M"), 1, 16)),
            textInput("note_edit_reminder_m", "⏰ 提醒时间", placeholder = "YYYY-MM-DD HH:MM", value = rem_val),
            textInput("note_edit_due_m", "📅 到期时间", placeholder = "YYYY-MM-DD HH:MM", value = due_val),
            if (!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin") tagList(
              tags$hr(style = "margin:6px 0;"),
              tags$b("📨 派发给"),
              selectInput("note_edit_dispatch", NULL, choices = NULL, multiple = TRUE, width = "100%")
            )
          )
        )
      ),
      
      tags$hr(),
      
      # 评论标题 + 排序
      tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
        tags$h5("💬 评论", style = "margin:0;"),
        if (nchar(comment_html) > 0) tags$button(
          id = "note_comment_sort_btn",
          class = "btn btn-xs btn-default",
          onclick = "var $list=$('.note-comment-list');var $btn=$('#note_comment_sort_btn');if($btn.text().indexOf('最早')>=0){$list.append($list.children().get().reverse());$btn.html('🔽 最新在前');}else{$list.prepend($list.children().get().reverse());$btn.html('🔼 最早在前');}",
          "🔼 最早在前"
        ) else ""
      ),
      if (nchar(comment_html) > 0) tags$div(
        class = "note-comment-list",
        style = "max-height:500px; overflow-y:auto; margin-bottom:8px;",
        HTML(comment_html)
      ) else tags$p(class = "note-no-comment", style = "color:#999; font-size:12px;", "暂无评论"),
      
      # 评论输入
      textAreaInput("note_comment_new_m", NULL, rows = 8, placeholder = "添加评论...", width = "100%"),
      div(style="text-align:right; margin-top:4px; margin-bottom:8px;",
        actionButton("note_add_comment_m", "发表评论", class = "btn-info btn-sm", icon = icon("comment")))
    )
    
    showModal(modalDialog(
      title = tags$div(style="display:flex; justify-content:space-between; align-items:center; width:100%;",
        tags$span(icon("sticky-note"), " ", note_no, " — ", note$title[1]),
        tags$div(style="display:flex; gap:6px; flex-shrink:0;",
          tags$button(id="note_read_mode_toggle", class="btn btn-xs btn-default",
            onclick="var ro=$('#note_content_ro');var ed=$('#note_content_ed');var btn=$('#note_read_mode_toggle');var ca=$('.comment-actions');var ta=$('#note_comment_new_m');var ab=$('#note_add_comment_m');if(ed.is(':visible')){Shiny.setInputValue('note_readmode_autosave',Math.random(),{priority:'event'});ed.hide();ro.show();btn.text('📓 编辑');ca.hide();ta.hide();ab.hide();}else{ro.hide();ed.show();btn.text('📖 阅读');ca.show();ta.show();ab.show();}",
            "📖 阅读"),
          tags$button(class="btn btn-xs btn-default",
            onclick="$('#shiny-modal').modal('hide')",
            "✕ 关闭")
        )
      ),
      size = "l",
      modal_body,
      footer = tags$div(style = "display:flex; align-items:center; gap:4px; flex-wrap:nowrap; justify-content:flex-end;",
        HTML(status_btns),
        actionButton("note_do_save", "💾 保存", class = "btn-primary btn-sm"),
        tags$button(class = "btn btn-info btn-sm note-wo-btn", `data-id` = note$id[1], "📋转工单"),
        tags$button(class = "btn btn-warning btn-sm note-report-btn", `data-id` = note$id[1], "📅日报"),
        tags$button(class = "btn btn-danger btn-sm note-del-btn", `data-id` = note$id[1], "🗑删除"),
        modalButton("关闭")
      ),
      easyClose = TRUE
    ))
    # 初始化派发下拉（admin专用）
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin") {
      con <- db_connect()
      all_users <- tryCatch({
        dbGetQuery(con, "SELECT id, username, COALESCE(NULLIF(display_name,''), username) as display_name FROM users WHERE active = 1 AND role != 'admin' ORDER BY username")
      }, finally = { db_disconnect(con) })
      dispatched <- note_dispatch_get_users(note$id[1])
      if (nrow(all_users) > 0) {
        labels <- sprintf("%s", all_users$display_name)
        choices <- stats::setNames(as.character(all_users$id), labels)
        selected_vals <- if (nrow(dispatched) > 0) as.character(dispatched$id) else character(0)
        updateSelectInput(session, "note_edit_dispatch", choices = choices, selected = selected_vals)
      }
    }
  })

  ##################
  # 保存修改（不关弹窗，留在卡片内）
  ##################
  observeEvent(input$note_do_save, {
    req(rv$logged_in, rv$note_edit_id, input$note_edit_content_m)
    lines <- strsplit(trimws(input$note_edit_content_m), "\n")[[1]]
    title <- if (length(lines) > 0) lines[1] else "未命名"
    result <- note_update(rv$note_edit_id,
      title = title, content = input$note_edit_content_m,
      note_no = if (trimws(input$note_edit_no_m) != "") input$note_edit_no_m else NULL,
      created_at = if (trimws(input$note_edit_created_m) != "") input$note_edit_created_m else NULL,
      reminder_at = if (trimws(input$note_edit_reminder_m) != "") input$note_edit_reminder_m else NULL,
      due_at = if (trimws(input$note_edit_due_m) != "") input$note_edit_due_m else NULL,
      current_user = rv$current_user)
    # 保存派发（admin 专用）
    if (result$success && !is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin") {
      dispatch_uids <- input$note_edit_dispatch
      if (!is.null(dispatch_uids)) note_dispatch_set(rv$note_edit_id, dispatch_uids)
    }
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })

  # 阅读模式切换时自动保存
  observeEvent(input$note_readmode_autosave, {
    req(rv$logged_in, rv$note_edit_id, input$note_edit_content_m)
    lines <- strsplit(trimws(input$note_edit_content_m), "\n")[[1]]
    title <- if (length(lines) > 0) lines[1] else "未命名"
    note_update(rv$note_edit_id,
      title = title, content = input$note_edit_content_m,
      note_no = if (trimws(input$note_edit_no_m) != "") input$note_edit_no_m else NULL,
      created_at = if (trimws(input$note_edit_created_m) != "") input$note_edit_created_m else NULL,
      reminder_at = if (trimws(input$note_edit_reminder_m) != "") input$note_edit_reminder_m else NULL,
      due_at = if (trimws(input$note_edit_due_m) != "") input$note_edit_due_m else NULL,
      current_user = rv$current_user)
    if (!is.null(rv$current_user) && nrow(rv$current_user) > 0 && rv$current_user$role[1] == "admin") {
      dispatch_uids <- input$note_edit_dispatch
      if (!is.null(dispatch_uids)) note_dispatch_set(rv$note_edit_id, dispatch_uids)
    }
    note_trigger(note_trigger() + 1)
  })

  ##################
  # 状态移动
  ##################
  observeEvent(input$note_move_click, {
    req(rv$logged_in)
    result <- note_patch(as.integer(input$note_move_click$id), status = input$note_move_click$to)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 2)
  })

  ##################
  # 卡片小旗（循环递增）
  ##################
  observeEvent(input$note_flag_click, {
    req(rv$logged_in)
    id <- as.integer(input$note_flag_click)
    note <- note_get_by_id(id, rv$current_user)
    if (is.null(note)) return()
    imp <- (note$importance[1] %||% 0) + 1
    if (imp > 5) imp <- 0
    result <- note_patch(id, importance = imp, current_user = rv$current_user)
    note_trigger(note_trigger() + 1)
  })

  ##################
  # 弹窗内小旗（直接设置）
  ##################
  observeEvent(input$note_flag_set, {
    req(rv$logged_in)
    parts <- strsplit(as.character(input$note_flag_set), ":")[[1]]
    id <- as.integer(parts[1]); val <- as.integer(parts[2])
    result <- note_patch(id, importance = val, current_user = rv$current_user)
    note_trigger(note_trigger() + 1)
  })

  ##################
  # 添加评论（不关弹窗 — 用 JS 注入）
  ##################
  observeEvent(input$note_add_comment_m, {
    req(rv$logged_in, rv$note_edit_id, input$note_comment_new_m)
    if (trimws(input$note_comment_new_m) == "") return()
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- note_comment_add(rv$note_edit_id, input$note_comment_new_m, uid, current_user = rv$current_user)
    if (!result$success) {
      showNotification(result$message, type = "error", duration = 2)
      return()
    }
    updateTextAreaInput(session, "note_comment_new_m", value = "")
    # 构建新评论 HTML 并注入弹窗（含编辑区+时间输入）
    new_comment_html <- sprintf(
      '<div class="comment-item" id="comment-%d" style="background:#fafafa; padding:8px 12px; margin-bottom:6px; border-radius:6px; border-left:4px solid #3498db;">
        <div style="display:flex; justify-content:space-between; align-items:flex-start;">
          <div style="flex:1; min-width:0;">
            <div style="font-size:11px; color:#999; margin-bottom:4px;">
              <span style="font-weight:bold; color:#3498db;">%s</span>
              <span style="margin-left:8px;">%s</span>
            </div>
            <div class="comment-text" style="font-size:13px; line-height:1.5; white-space:pre-wrap; word-break:break-word;">%s</div>
            <div class="comment-edit-area" style="display:none; margin-top:4px;">
              <textarea class="form-control comment-edit-input" style="font-size:13px;" rows="2">%s</textarea>
              <div style="margin-top:3px;">
                <input class="form-control comment-edit-time" style="font-size:11px; width:150px; display:inline;" value="%s" placeholder="时间">
                <button class="btn btn-xs btn-primary comment-save-btn" style="margin-top:2px;" data-id="%d">保存</button>
                <button class="btn btn-xs btn-default comment-cancel-btn" style="margin-top:2px;">取消</button>
              </div>
            </div>
          </div>
          <div class="comment-actions" style="margin-left:8px; white-space:nowrap; flex-shrink:0;">
            <button class="btn btn-xs btn-success comment-done-btn" data-id="%d">✅</button>
            <button class="btn btn-xs btn-info comment-edit-btn" data-id="%d">✏</button>
            <button class="btn btn-xs btn-danger comment-del-btn" data-id="%d">🗑</button>
            <button class="btn btn-xs btn-default comment-reply-btn" data-id="%d" data-name="%s">💬 回复</button>
          </div>
        </div>
        <div class="comment-reply-form" style="display:none; margin-top:6px;">
          <textarea class="form-control comment-reply-input" rows="2" placeholder="回复 %s ..." style="font-size:12px;"></textarea>
          <button class="btn btn-xs btn-primary comment-reply-submit" data-id="%d" style="margin-top:3px;">回复</button>
          <button class="btn btn-xs btn-default comment-reply-cancel" style="margin-top:3px;">取消</button>
        </div>
      </div>',
      result$id,
      result$creator_name,
      substr(result$created_at, 1, 16),
      gsub("'", "\\'", input$note_comment_new_m),
      gsub("'", "\\'", input$note_comment_new_m),
      substr(result$created_at, 1, 16),
      result$id,
      result$id, result$id, result$id,
      result$id, result$creator_name, result$creator_name, result$id)
    session$sendCustomMessage(type = "noteInjectComment", message = list(html = new_comment_html, comment_id = result$id))
    note_trigger(note_trigger() + 1)
    rv$daily_report_refresh <- rv$daily_report_refresh + 1
    showNotification("评论已添加", type = "message", duration = 1.5)
  })

  ##################
  # 回复评论（子评论）
  ##################
  observeEvent(input$note_reply_submit, {
    req(rv$logged_in, rv$note_edit_id)
    data <- input$note_reply_submit
    pid <- as.integer(data$id)
    text <- trimws(data$text)
    if (is.null(text) || text == "") return()
    uid <- if (!is.null(rv$current_user) && nrow(rv$current_user) > 0) rv$current_user$id[1] else NULL
    result <- note_comment_add(rv$note_edit_id, text, uid, parent_id = pid, current_user = rv$current_user)
    if (result$success) {
      removeModal()
      session$sendCustomMessage("noteReopenModal", list(note_id = rv$note_edit_id))
    }
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 1.5)
  })

  ##################
  # 评论编辑/删除（JS 触发）
  ##################
  observeEvent(input$note_comment_edit, {
    req(rv$logged_in)
    parts <- strsplit(as.character(input$note_comment_edit), ":::")[[1]]
    if (length(parts) < 2) return()
    cid <- as.integer(parts[1])
    content <- parts[2]
    time <- if (length(parts) >= 3 && trimws(parts[3]) != "") trimws(parts[3]) else NULL
    result1 <- note_comment_update(cid, content, rv$current_user)
    if (!is.null(time) && result1$success) {
      note_comment_update_time(cid, time)
    }
    note_trigger(note_trigger() + 1)
    rv$daily_report_refresh <- rv$daily_report_refresh + 1
    removeModal()
    session$sendCustomMessage("noteReopenModal", list(note_id = rv$note_edit_id))
    showNotification(result1$message, type = "message", duration = 1.5)
  })

  observeEvent(input$note_comment_delete, {
    req(rv$logged_in)
    cid <- as.integer(input$note_comment_delete)
    result <- note_comment_delete(cid, rv$current_user)
    if (result$success) {
      # 用 JS 从弹窗DOM中移除评论
      session$sendCustomMessage(type = "noteRemoveComment", message = list(comment_id = cid))
    }
    note_trigger(note_trigger() + 1)
    rv$daily_report_refresh <- rv$daily_report_refresh + 1
    showNotification(result$message, type = "message", duration = 1.5)
  })

  ##################
  # 评论标记已完成/取消
  ##################
  observeEvent(input$note_comment_done, {
    req(rv$logged_in)
    cid <- as.integer(input$note_comment_done)
    result <- note_comment_mark_status(cid, "completed", rv$current_user)
    if (result$success) {
      # 取写入的 completed_at 作为徽章显示时间
      cmt <- note_comment_get_by_id(cid)
      cat <- if (!is.null(cmt) && nrow(cmt) > 0) {
        ca <- cmt$completed_at[1] %||% ""
        if (isTRUE(nchar(ca) > 10)) substr(ca, 1, 16) else ca
      } else ""
      session$sendCustomMessage(type = "noteCommentMarkDone", message = list(comment_id = cid, status = "completed", completed_at = cat))
    }
    note_trigger(note_trigger() + 1)
    rv$daily_report_refresh <- rv$daily_report_refresh + 1
    showNotification(result$message, type = "message", duration = 1.5)
  })

  observeEvent(input$note_comment_undone, {
    req(rv$logged_in)
    cid <- as.integer(input$note_comment_undone)
    result <- note_comment_mark_status(cid, "", rv$current_user)
    if (result$success) {
      session$sendCustomMessage(type = "noteCommentMarkDone", message = list(comment_id = cid, status = "", completed_at = ""))
    }
    note_trigger(note_trigger() + 1)
    rv$daily_report_refresh <- rv$daily_report_refresh + 1
    showNotification("已取消完成标记", type = "message", duration = 1.5)
  })

  ##################
  # 转工单 / 记日报 / 删除
  ##################
  observeEvent(input$note_to_wo_click, {
    req(rv$logged_in)
    result <- note_convert_to_work_order(as.integer(input$note_to_wo_click), rv$current_user)
    removeModal()
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
  observeEvent(input$note_report_click, {
    req(rv$logged_in)
    result <- note_patch(as.integer(input$note_report_click), reported_to_daily = 1, current_user = rv$current_user)
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = "message", duration = 2)
  })
  observeEvent(input$note_del_click, {
    req(rv$logged_in)
    result <- note_delete(as.integer(input$note_del_click), rv$current_user)
    removeModal()
    note_trigger(note_trigger() + 1)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
  })
}
