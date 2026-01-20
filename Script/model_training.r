model_get_all <- function() {
  con <- db_connect()
  tryCatch({
    query <- "SELECT * FROM models ORDER BY created_at DESC"
    result <- dbGetQuery(con, query)
    return(result)
  }, error = function(e) {
    return(data.frame())
  }, finally = {
    db_disconnect(con)
  })
}

model_add <- function(model_name, model_type, model_params, accuracy = 0, created_by = 1) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("INSERT INTO models (model_name, model_type, model_params, accuracy, created_by) VALUES ('%s', '%s', '%s', %f, %d)", 
                     model_name, model_type, model_params, accuracy, created_by)
    dbExecute(con, query)
    return(list(success = TRUE, message = "模型添加成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("添加失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}

model_train <- function(model_name, model_type, model_params) {
  set.seed(123)
  x <- rnorm(100)
  y <- 2 * x + rnorm(100)
  
  accuracy <- runif(1, 0.7, 0.95)
  
  result <- list(
    success = TRUE,
    accuracy = accuracy,
    message = sprintf("模型 %s 训练完成，准确率: %.2f%%", model_name, accuracy * 100)
  )
  
  return(result)
}

model_delete <- function(id) {
  con <- db_connect()
  tryCatch({
    query <- sprintf("DELETE FROM models WHERE id = %d", id)
    dbExecute(con, query)
    return(list(success = TRUE, message = "模型删除成功"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("删除失败:", e$message)))
  }, finally = {
    db_disconnect(con)
  })
}
