source("Script/auth.r")
source("Script/data_management.r")
source("Script/model_training.r")
source("Script/visualization.r")
source("Script/user_management.r")
source("Script/system_settings.r")

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    logged_in = FALSE,
    current_user = NULL
  )
  
  observeEvent(input$refresh_data, {
    output$data_table <- renderDT({
      DT::datatable(
        data_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        editable = TRUE,
        rownames = FALSE
      )
    })
  })
  
  observeEvent(input$add_data, {
    req(input$data_name, input$data_type, input$data_value)
    result <- data_add(input$data_name, input$data_type, input$data_value)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    output$data_table <- renderDT({
      DT::datatable(
        data_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        editable = TRUE,
        rownames = FALSE
      )
    })
  })
  
  output$data_table <- renderDT({
    DT::datatable(
      data_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      editable = TRUE,
      rownames = FALSE
    )
  })
  
  observeEvent(input$refresh_models, {
    output$model_table <- renderDT({
      DT::datatable(
        model_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  observeEvent(input$train_model, {
    req(input$model_name, input$model_type)
    
    withProgress(message = "正在训练模型...", value = 0, {
      result <- model_train(input$model_name, input$model_type, input$model_params)
      incProgress(1)
    })
    
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    
    model_add(input$model_name, input$model_type, input$model_params, result$accuracy)
    
    output$training_result <- renderPrint({
      cat(sprintf("模型名称: %s\n", input$model_name))
      cat(sprintf("模型类型: %s\n", input$model_type))
      cat(sprintf("准确率: %.2f%%\n", result$accuracy * 100))
    })
    
    output$model_table <- renderDT({
      DT::datatable(
        model_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  output$model_table <- renderDT({
    DT::datatable(
      model_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$generate_viz, {
    output$viz_plot <- renderPlotly({
      viz_generate(input$viz_type, input$viz_data)
    })
  })
  
  output$viz_plot <- renderPlotly({
    viz_generate(input$viz_type, input$viz_data)
  })
  
  observeEvent(input$refresh_users, {
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  observeEvent(input$add_user, {
    req(input$username, input$password, input$role)
    result <- user_add(input$username, input$password, input$role)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    output$user_table <- renderDT({
      DT::datatable(
        user_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  output$user_table <- renderDT({
    DT::datatable(
      user_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$refresh_config, {
    output$config_table <- renderDT({
      DT::datatable(
        config_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  observeEvent(input$add_config, {
    req(input$config_key, input$config_value)
    result <- config_add(input$config_key, input$config_value, input$config_desc)
    showNotification(result$message, type = ifelse(result$success, "message", "error"))
    output$config_table <- renderDT({
      DT::datatable(
        config_get_all(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
  
  output$config_table <- renderDT({
    DT::datatable(
      config_get_all(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}
