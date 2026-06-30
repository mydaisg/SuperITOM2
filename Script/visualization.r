# 可视化模块 v2
# 数据源：记事模块 ｜ 图表类型：词云图 + 原有图表

##################
# R 代码语法高亮（轻量版，复用 high_light.r 风格）
##################
viz_highlight_r <- function(code_lines) {
  code <- paste(code_lines, collapse = "\n")
  code <- gsub("&", "&amp;", code)
  code <- gsub("<", "&lt;", code)
  code <- gsub(">", "&gt;", code)
  lines <- strsplit(code, "\n")[[1]]
  result <- sapply(lines, function(line) {
    if (grepl("^\\s*#", line)) {
      return(sprintf('<span style="color:#6a737d;font-style:italic;">%s</span>', line))
    }
    for (kw in c("function","if","else","for","while","return","NULL","TRUE","FALSE","NA","in","next","break"))
      line <- gsub(sprintf("\\b%s\\b", kw), sprintf('<span style="color:#d73a49;font-weight:600;">%s</span>', kw), line)
    line <- gsub('("[^"]*")', '<span style="color:#22863a;">\\1</span>', line)
    line <- gsub('(\\b\\d+\\.?\\d*\\b)', '<span style="color:#005cc5;">\\1</span>', line)
    for (fn in c("library","ggplot","ggplot2","aes","geom_bar","geom_line","geom_point","geom_tile","geom_text",
                 "labs","theme_minimal","theme_void","theme","scale_fill_gradient","scale_fill_manual",
                 "scale_size_identity","scale_color_identity","coord_polar","coord_fixed",
                 "element_text","element_blank","margin","set.seed","sample","runif","seq","seq_len",
                 "nchar","nrow","paste","paste0","sprintf","as.data.frame","as.integer","as.character","order",
                 "data.frame","c","list","head","min","max","sum","abs","sqrt","rescale","log1p"))
      line <- gsub(sprintf("\\b%s\\b", fn), sprintf('<span style="color:#6f42c1;font-weight:500;">%s</span>', fn), line)
    line <- gsub('(&lt;-)', '<span style="font-weight:700;">&lt;-</span>', line)
    line
  })
  code <- paste(result, collapse = "\n")
  HTML(paste0(
    '<pre style="background:#f6f8fa;border-radius:4px;padding:12px;overflow-x:auto;font-size:11px;line-height:1.5;max-height:280px;overflow-y:auto;margin:0;">',
    '<code style="font-family:Consolas,Monaco,monospace;color:#24292e;">', code, '</code></pre>'))
}

##################
# 各图表类型的示范算法代码
##################
viz_get_algorithm_code <- function(viz_type, data_source) {
  if (viz_type == "词云图") {
    return(c(
      "# 词云图：中文二元组+三元组分词 → 螺旋布局",
      "# 1. 采集数据（记事标题+正文+评论）",
      "text <- paste(c(notes$title, notes$content, cmt), collapse=\" \")",
      "",
      "# 2. 中文 bigram + trigram 分词",
      "ch <- gsub(\"[^\\\u4e00-\\\u9fff]\", \"\", text)",
      "bigrams <- paste0(chars[-n], chars[-1])",
      "trigrams <- paste0(chars[-(1:2)], chars[-1], chars[-(1:2)])",
      "",
      "# 3. 过滤停用词 → 词频排序",
      "freq <- as.data.frame(table(words))",
      "freq <- freq[order(-freq$freq), ]",
      "",
      "# 4. 螺旋坐标 (Archimedean spiral)",
      "theta <- seq(0, 10*pi, length.out=n)",
      "r <- 0.03 + 0.11 * sqrt(seq_len(n))",
      "freq$x <- r * cos(theta)",
      "freq$y <- r * sin(theta)",
      "",
      "# 5. ggplot2 渲染",
      "ggplot(freq, aes(x,y,label=word)) +",
      "  geom_text(aes(angle=angle), fontface=\"bold\") +",
      "  theme_void() + coord_fixed()"
    ))
  } else if (viz_type == "柱状图") {
    c(
      "# 柱状图：分类汇总 → geom_bar",
      "data <- data.frame(",
      "  category = c(\"服务器\",\"网络\",\"应用\"),",
      "  value    = c(120, 85, 63))",
      "ggplot(data, aes(category, value, fill=category)) +",
      "  geom_bar(stat=\"identity\") +",
      "  labs(title=\"分类统计\", x=\"\", y=\"数量\") +",
      "  theme_minimal()"
    )
  } else if (viz_type == "折线图") {
    c(
      "# 折线图：时间序列 → geom_line + geom_point",
      "data <- data.frame(",
      "  date  = seq(as.Date(\"2024-01-01\"), by=\"day\", length.out=30),",
      "  value = cumsum(rnorm(30, 5, 2)))",
      "ggplot(data, aes(date, value)) +",
      "  geom_line(color=\"steelblue\", size=1) +",
      "  geom_point(color=\"steelblue\", size=2) +",
      "  labs(title=\"趋势图\", x=\"日期\", y=\"值\") +",
      "  theme_minimal()"
    )
  } else if (viz_type == "散点图") {
    c(
      "# 散点图：双变量分布 → geom_point",
      "data <- data.frame(",
      "  x = rnorm(100, 50, 10),",
      "  y = rnorm(100, 50, 10),",
      "  g = sample(c(\"A\",\"B\",\"C\"), 100, replace=TRUE))",
      "ggplot(data, aes(x, y, color=g)) +",
      "  geom_point(size=3, alpha=0.7) +",
      "  labs(title=\"散点分布\") + theme_minimal()"
    )
  } else if (viz_type == "饼图") {
    c(
      "# 饼图：柱状图 + 极坐标变换",
      "agg <- aggregate(value ~ category, data, sum)",
      "ggplot(agg, aes(x=\"\", y=value, fill=category)) +",
      "  geom_bar(stat=\"identity\", width=1) +",
      "  coord_polar(\"y\") +",
      "  labs(title=\"占比分布\") + theme_void()"
    )
  } else if (viz_type == "热力图") {
    c(
      "# 热力图：矩阵 → geom_tile + 渐变色",
      "mat <- matrix(data$value, nrow=5, ncol=6)",
      "df  <- as.data.frame(as.table(mat))",
      "colnames(df) <- c(\"row\", \"col\", \"val\")",
      "ggplot(df, aes(col, row, fill=val)) +",
      "  geom_tile() +",
      "  scale_fill_gradient(low=\"blue\", high=\"red\") +",
      "  labs(title=\"热力图\") + theme_minimal()"
    )
  } else { character(0) }
}

##################
# 中文/英文分词 + 词频统计
##################
viz_tokenize <- function(text) {
  # 英文词（2字母以上）
  eng_words <- unlist(regmatches(tolower(text), gregexpr("[a-zA-Z]{2,}", tolower(text))))
  # 中文二元组（bigram）
  ch <- gsub("[^\u4e00-\u9fff]", "", text)
  ch_bigram <- character(0)
  if (nchar(ch) >= 2) {
    chars <- strsplit(ch, "")[[1]]
    for (i in seq_len(length(chars) - 1)) {
      ch_bigram <- c(ch_bigram, paste0(chars[i], chars[i + 1]))
    }
  }
  # 中文三元组（trigram）
  ch_trigram <- character(0)
  if (nchar(ch) >= 3) {
    for (i in seq_len(length(chars) - 2)) {
      ch_trigram <- c(ch_trigram, paste0(chars[i], chars[i + 1], chars[i + 2]))
    }
  }
  all_words <- c(eng_words, ch_bigram, ch_trigram)
  # 过滤停用词
  stopwords <- c("the","and","for","was","are","that","this","with","from","have","been",
                 "进行","需要","一个","可以","没有","我们","他们","已经","什么","这个","不是",
                 "就是","还是","因为","所以","但是","如果","虽然","而且","或者","应该","可能",
                 "问题","处理","是否","目前","情况","相关","通过","使用","根据","关于")
  all_words <- all_words[!all_words %in% stopwords]
  all_words <- all_words[nchar(all_words) >= 2]
  if (length(all_words) == 0) return(data.frame(word = character(0), freq = integer(0)))
  freq <- as.data.frame(table(all_words), stringsAsFactors = FALSE)
  names(freq) <- c("word", "freq")
  freq <- freq[order(-freq$freq), ]
  if (nrow(freq) > 100) freq <- freq[1:100, ]
  freq
}

##################
# 词云图（ggplot2 模拟 wordcloud 风格）
##################
viz_wordcloud_ggplot <- function(freq, title = "记事词云") {
  if (nrow(freq) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0,
      label = "暂无数据", size = 8, color = "#999") + ggplot2::theme_void())
  }
  set.seed(42)
  n <- nrow(freq)
  # 字号：高频词更大，低频词更小
  freq$size <- scales::rescale(log1p(freq$freq), to = c(4, 28))
  # 颜色：10种主题色随机
  pal <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
           "#ffff33","#a65628","#f781bf","#66c2a5","#fc8d62")
  freq$color <- sample(pal, n, replace = TRUE)
  # 螺旋布局：铺满更大画布
  a <- 0.03
  b <- 0.11
  theta <- seq(0, 10 * pi, length.out = n)
  r <- a + b * sqrt(seq_len(n))
  freq$x <- r * cos(theta)
  freq$y <- r * sin(theta)
  # 高频词向中心靠拢
  top_n <- min(25, n)
  center_pull <- seq(0.45, 0, length.out = top_n)
  freq$x[1:top_n] <- freq$x[1:top_n] * (1 - center_pull)
  freq$y[1:top_n] <- freq$y[1:top_n] * (1 - center_pull)
  # 轻微扰动
  freq$x <- freq$x + runif(n, -0.05, 0.05)
  freq$y <- freq$y + runif(n, -0.05, 0.05)
  # 旋转角度：高频词水平，低频词随机
  freq$angle <- c(rep(0, top_n), runif(n - top_n, -25, 25))

  p <- ggplot2::ggplot(freq, ggplot2::aes(x = .data$x, y = .data$y,
    label = .data$word, size = .data$size, color = .data$color)) +
    ggplot2::geom_text(ggplot2::aes(angle = .data$angle),
      fontface = "bold", show.legend = FALSE, family = "sans") +
    ggplot2::scale_size_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold", color = "#333"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    ) +
    ggplot2::coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.2))
  p
}

##################
# 记事数据词云（标题+正文+所有评论）
##################
viz_note_wordcloud <- function() {
  notes <- tryCatch(note_get_all(), error = function(e) data.frame())
  # 收集所有文本：标题、正文、评论
  title_text <- if (nrow(notes) > 0) paste(notes$title, collapse = " ") else ""
  body_text  <- if (nrow(notes) > 0) paste(na.omit(notes$content), collapse = " ") else ""
  # 从数据库拉取所有评论
  cmt_text <- ""
  tryCatch({
    con <- db_connect()
    on.exit(db_disconnect(con), add = TRUE)
    cmt <- dbGetQuery(con, "SELECT content FROM note_comments WHERE content IS NOT NULL AND TRIM(content) != ''")
    if (nrow(cmt) > 0) cmt_text <- paste(cmt$content, collapse = " ")
  }, error = function(e) { cmt_text <- "" })
  all_text <- paste(title_text, body_text, cmt_text)
  if (nchar(trimws(all_text)) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0,
      label = "暂无记事数据", size = 6, color = "#aaa") + ggplot2::theme_void())
  }
  freq <- viz_tokenize(all_text)
  viz_wordcloud_ggplot(freq, title = sprintf("记事词云（%d 条记事 + 评论）", nrow(notes)))
}

##################
# 主入口：保留原有图表类型 + 数据源选择
##################
viz_generate <- function(viz_type, data_source) {
  # ★ 词云图
  if (viz_type == "词云图") {
    if (data_source == "记事数据") {
      return(viz_note_wordcloud())
    } else {
      return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0,
        label = "词云图当前仅支持记事数据源", size = 6, color = "#aaa") + ggplot2::theme_void())
    }
  }

  # 以下为原有图表逻辑（数据源暂时保留模拟数据）
  set.seed(456)
  if (data_source == "ITOM数据") {
    data <- data.frame(
      date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
      value = cumsum(rnorm(30, 5, 2)),
      category = sample(c("服务器", "网络", "应用"), 30, replace = TRUE)
    )
  } else if (data_source == "流程监控") {
    return(viz_generate_process_charts(viz_type))
  } else {
    data <- data.frame(
      model = paste0("Model", 1:10),
      accuracy = runif(10, 0.7, 0.95),
      type = sample(c("线性回归", "决策树", "随机森林", "神经网络"), 10, replace = TRUE)
    )
  }

  p <- NULL
  if (viz_type == "折线图") {
    if (data_source == "ITOM数据") {
      p <- ggplot(data, aes(x = date, y = value)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 2) +
        labs(title = "ITOM数据趋势图", x = "日期", y = "数值") + theme_minimal()
    } else {
      p <- ggplot(data, aes(x = model, y = accuracy, group = 1)) +
        geom_line(color = "darkgreen", size = 1) +
        geom_point(color = "darkgreen", size = 2) +
        labs(title = "模型准确率趋势", x = "模型", y = "准确率") + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else if (viz_type == "柱状图") {
    if (data_source == "ITOM数据") {
      p <- ggplot(data, aes(x = category, y = value, fill = category)) +
        geom_bar(stat = "identity") +
        labs(title = "ITOM数据分类统计", x = "类别", y = "数值") + theme_minimal()
    } else {
      p <- ggplot(data, aes(x = model, y = accuracy, fill = type)) +
        geom_bar(stat = "identity") +
        labs(title = "模型准确率对比", x = "模型", y = "准确率") + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else if (viz_type == "散点图") {
    if (data_source == "ITOM数据") {
      p <- ggplot(data, aes(x = date, y = value, color = category)) +
        geom_point(size = 3) +
        labs(title = "ITOM数据散点图", x = "日期", y = "数值") + theme_minimal()
    } else {
      p <- ggplot(data, aes(x = model, y = accuracy, color = type)) +
        geom_point(size = 3) +
        labs(title = "模型准确率散点图", x = "模型", y = "准确率") + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else if (viz_type == "饼图") {
    if (data_source == "ITOM数据") {
      category_data <- aggregate(value ~ category, data, sum)
      p <- ggplot(category_data, aes(x = "", y = value, fill = category)) +
        geom_bar(stat = "identity", width = 1) + coord_polar("y") +
        labs(title = "ITOM数据分类占比") + theme_void()
    } else {
      type_data <- aggregate(accuracy ~ type, data, mean)
      p <- ggplot(type_data, aes(x = "", y = accuracy, fill = type)) +
        geom_bar(stat = "identity", width = 1) + coord_polar("y") +
        labs(title = "模型类型平均准确率") + theme_void()
    }
  } else if (viz_type == "热力图") {
    if (data_source == "ITOM数据") {
      data_matrix <- matrix(data$value, nrow = 5, ncol = 6)
      colnames(data_matrix) <- paste0("Day", 1:6)
      rownames(data_matrix) <- paste0("Week", 1:5)
      data_long <- as.data.frame(as.table(data_matrix))
      colnames(data_long) <- c("Week", "Day", "Value")
      p <- ggplot(data_long, aes(x = Day, y = Week, fill = Value)) +
        geom_tile() + scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "ITOM数据热力图") + theme_minimal()
    } else {
      data_matrix <- matrix(data$accuracy, nrow = 2, ncol = 5)
      colnames(data_matrix) <- paste0("Model", 1:5)
      rownames(data_matrix) <- c("Type1", "Type2")
      data_long <- as.data.frame(as.table(data_matrix))
      colnames(data_long) <- c("Type", "Model", "Accuracy")
      p <- ggplot(data_long, aes(x = Model, y = Type, fill = Accuracy)) +
        geom_tile() + scale_fill_gradient(low = "yellow", high = "red") +
        labs(title = "模型准确率热力图") + theme_minimal()
    }
  }

  return(plotly::ggplotly(p))
}

##################
# 流程监控图表
##################
viz_generate_process_charts <- function(viz_type) {
  stats <- tryCatch(appr_stats(), error = function(e) list(total = 0, pending = 0, approved = 0, rejected = 0, tpls = 0))
  if (viz_type == "柱状图") {
    data <- data.frame(
      状态 = c("审批中", "已通过", "已驳回"),
      数量 = c(stats$pending, stats$approved, stats$rejected)
    )
    p <- ggplot(data, aes(x = 状态, y = 数量, fill = 状态)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("审批中" = "#f39c12", "已通过" = "#27ae60", "已驳回" = "#e74c3c")) +
      labs(title = "审批实例状态分布", x = "", y = "数量") + theme_minimal()
    return(plotly::ggplotly(p))
  } else if (viz_type == "饼图") {
    data <- data.frame(
      类型 = c("审批中", "已通过", "已驳回"),
      数量 = c(stats$pending, stats$approved, stats$rejected)
    )
    p <- ggplot(data, aes(x = "", y = 数量, fill = 类型)) +
      geom_bar(stat = "identity", width = 1) + coord_polar("y") +
      labs(title = "审批状态分布") +
      scale_fill_manual(values = c("审批中" = "#f39c12", "已通过" = "#27ae60", "已驳回" = "#e74c3c")) +
      theme_void()
    return(plotly::ggplotly(p))
  } else {
    return(plotly::plot_ly() %>% plotly::add_annotations(text = "审批模块统计", showarrow = FALSE, font = list(size = 20)))
  }
}
