# 可视化模块

viz_generate <- function(viz_type, data_source) {
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
        labs(title = "ITOM数据趋势图", x = "日期", y = "数值") +
        theme_minimal()
    } else {
      p <- ggplot(data, aes(x = model, y = accuracy, group = 1)) +
        geom_line(color = "darkgreen", size = 1) +
        geom_point(color = "darkgreen", size = 2) +
        labs(title = "模型准确率趋势", x = "模型", y = "准确率") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else if (viz_type == "柱状图") {
    if (data_source == "ITOM数据") {
      p <- ggplot(data, aes(x = category, y = value, fill = category)) +
        geom_bar(stat = "identity") +
        labs(title = "ITOM数据分类统计", x = "类别", y = "数值") +
        theme_minimal()
    } else {
      p <- ggplot(data, aes(x = model, y = accuracy, fill = type)) +
        geom_bar(stat = "identity") +
        labs(title = "模型准确率对比", x = "模型", y = "准确率") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else if (viz_type == "散点图") {
    if (data_source == "ITOM数据") {
      p <- ggplot(data, aes(x = date, y = value, color = category)) +
        geom_point(size = 3) +
        labs(title = "ITOM数据散点图", x = "日期", y = "数值") +
        theme_minimal()
    } else {
      p <- ggplot(data, aes(x = model, y = accuracy, color = type)) +
        geom_point(size = 3) +
        labs(title = "模型准确率散点图", x = "模型", y = "准确率") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else if (viz_type == "饼图") {
    if (data_source == "ITOM数据") {
      category_data <- aggregate(value ~ category, data, sum)
      p <- ggplot(category_data, aes(x = "", y = value, fill = category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "ITOM数据分类占比") +
        theme_void()
    } else {
      type_data <- aggregate(accuracy ~ type, data, mean)
      p <- ggplot(type_data, aes(x = "", y = accuracy, fill = type)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "模型类型平均准确率") +
        theme_void()
    }
  } else if (viz_type == "热力图") {
    if (data_source == "ITOM数据") {
      data_matrix <- matrix(data$value, nrow = 5, ncol = 6)
      colnames(data_matrix) <- paste0("Day", 1:6)
      rownames(data_matrix) <- paste0("Week", 1:5)
      data_long <- as.data.frame(as.table(data_matrix))
      colnames(data_long) <- c("Week", "Day", "Value")
      p <- ggplot(data_long, aes(x = Day, y = Week, fill = Value)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "ITOM数据热力图") +
        theme_minimal()
    } else {
      data_matrix <- matrix(data$accuracy, nrow = 2, ncol = 5)
      colnames(data_matrix) <- paste0("Model", 1:5)
      rownames(data_matrix) <- c("Type1", "Type2")
      data_long <- as.data.frame(as.table(data_matrix))
      colnames(data_long) <- c("Type", "Model", "Accuracy")
      p <- ggplot(data_long, aes(x = Model, y = Type, fill = Accuracy)) +
        geom_tile() +
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(title = "模型准确率热力图") +
        theme_minimal()
    }
  }

  return(ggplotly(p))
}

# 流程监控图表
viz_generate_process_charts <- function(viz_type) {
  stats <- process_get_monitor_metrics()

  if (viz_type == "柱状图") {
    # 流程状态分布
    con <- db_connect()
    term_count <- tryCatch(
      dbGetQuery(con, "SELECT COUNT(*) as c FROM process_instances WHERE status='terminated'")$c[1],
      error = function(e) 0, finally = { db_disconnect(con) })
    data <- data.frame(
      状态 = c("运行中", "已完成", "已终止"),
      数量 = c(stats$running_instances, stats$total_instances - stats$running_instances - term_count, term_count)
    )
    p <- ggplot(data, aes(x = 状态, y = 数量, fill = 状态)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("运行中" = "#5cb85c", "已完成" = "#5bc0de", "已终止" = "#d9534f")) +
      labs(title = "流程实例状态分布", x = "", y = "数量") +
      theme_minimal()
    return(ggplotly(p))

  } else if (viz_type == "饼图") {
    node_counts <- stats$node_counts
    if (nrow(node_counts) > 0) {
      p <- ggplot(node_counts, aes(x = "", y = cnt, fill = node_type)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "流程节点类型分布") +
        theme_void()
      return(ggplotly(p))
    }
  } else if (viz_type == "折线图") {
    # 模拟流程完成趋势（无实时日数据时用模拟）
    set.seed(123)
    days <- 14
    data <- data.frame(
      日期 = seq(Sys.Date() - days + 1, Sys.Date(), by = "day"),
      启动数 = pmax(0, round(rnorm(days, stats$today_started / max(1, days) * 2, 1))),
      完成数 = pmax(0, round(rnorm(days, stats$today_completed / max(1, days) * 2, 1)))
    )
    p <- ggplot(data, aes(x = 日期)) +
      geom_line(aes(y = 启动数, color = "启动"), size = 1) +
      geom_line(aes(y = 完成数, color = "完成"), size = 1) +
      scale_color_manual(values = c("启动" = "#337ab7", "完成" = "#5cb85c")) +
      labs(title = "近14天流程活动趋势", x = "", y = "数量", color = "") +
      theme_minimal() +
      theme(legend.position = "bottom")
    return(ggplotly(p))
  }

  # 默认返回指标卡片
  return(NULL)
}
