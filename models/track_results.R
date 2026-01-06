track_model <- function(X_train, y_train, X_test, y_test, method, trControl, tuneGrid, append, desc){
  # Capture the start time of the section you want to track
  section_start_time <- proc.time()
  
  
  model <- train(
    x = X_train, 
    y = y_train, 
    method = method,
    trControl = trControl
    , tuneGrid = tuneGrid
  )
  


  
  # Use the model to predict on the test set
  X_train$predictions <- predict(model, newdata = X_train)
  
  
  X_train$quality <- ifelse(round(X_train$predictions) == y_train, 3, 0)
  X_train$quality[round(X_train$predictions) != y_train & sign(round(X_train$predictions)) == sign(y_train)] <- 1
  
  
  
  # Use the model to predict on the test set
  X_test$predictions <- predict(model, newdata = X_test)
  
  X_test$quality <- ifelse(round(X_test$predictions) == y_test, 3, 0)
  X_test$quality[round(X_test$predictions) != y_test & sign(round(X_test$predictions)) == sign(y_test)] <- 1
  
  section_end_time <- proc.time()
  
  track_result <- data.frame(desc = desc
                             , method = method
                             , trControl = deparse(substitute(trControl))
                             , tuneGrid <-deparse(substitute(tuneGrid))
                             , train_result =(sum(X_train$quality) / nrow(X_train))
                             , test_result = sum(X_test$quality) / nrow(X_test)
                             , timing =  (section_end_time - section_start_time)[3]
                             )
  
  write_csv(track_result, "model_tracker.csv", col_names = !append, append = append)
  return(TRUE)
}

track_model_goals <- function(X_train, y_train, X_test, y_test, method, trControl, tuneGrid, append, desc){
  # Capture the start time of the section you want to track
  section_start_time <- proc.time()
  
  
  model <- train(
    x = X_train, 
    y = y_train, 
    method = method,
    trControl = trControl
    , tuneGrid = tuneGrid
  )
  
  
  
  
  # Use the model to predict on the test set
  X_train$predictions <- predict(model, newdata = X_train)
  
  
  X_train$quality <- ifelse(abs(round(X_train$predictions) - y_train) <= 1, 1, 0)
  X_train$quality <- ifelse(round(X_train$predictions) == y_train, 3, 0)

  
  
  
  # Use the model to predict on the test set
  X_test$predictions <- predict(model, newdata = X_test)
  
  
  X_test$quality <- ifelse(abs(round(X_test$predictions) - y_test) <= 1, 1, 0)
  X_test$quality <- ifelse(round(X_test$predictions) == y_test, 3, 0)
  
  section_end_time <- proc.time()
  
  track_result <- data.frame(desc = desc
                             , method = method
                             , trControl = deparse(substitute(trControl))
                             , tuneGrid <-deparse(substitute(tuneGrid))
                             , train_result =(sum(X_train$quality) / nrow(X_train))
                             , test_result = sum(X_test$quality) / nrow(X_test)
                             , timing =  (section_end_time - section_start_time)[3]
  )
  
  write_csv(track_result, "model_tracker.csv", col_names = !append, append = append)
  return(TRUE)
}
