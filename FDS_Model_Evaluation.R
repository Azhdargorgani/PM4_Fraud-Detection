library(caret)

evaluate_model <- function(model_path = "80_MODELS/fraud_model.rds",
                           test_data_path = "99_DATA/test_data.rds", 
                           test_labels_path = "99_DATA/test_labels.rds") {
  
  # Robustness: do all files exist?
  if (!file.exists(model_path) || 
      !file.exists(test_data_path) || 
      !file.exists(test_labels_path)) {
    return(data.frame(Message = "Model or test data not found."))
  }
  
  # Load model and test data
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  
  # Ensure test_labels$TX_FRAUD is a factor with the correct levels
  test_labels$TX_FRAUD <- factor(test_labels$TX_FRAUD, levels = c("No Fraud", "Fraud"))
  
  # Ensure test_data does NOT contain TX_FRAUD
  test_data <- test_data[, setdiff(names(test_data), "TX_FRAUD")]
  test_data <- as.data.frame(test_data)
  
  # Predict using the model
  predictions <- predict(model, newdata = test_data)
  predictions <- factor(predictions, levels = levels(test_labels$TX_FRAUD))
  
  # Create confusion matrix
  cm <- confusionMatrix(predictions, test_labels$TX_FRAUD)
  
  # Extract desired metrics as DataFrame
  metrics_df <- data.frame(
    Accuracy = unname(cm$overall["Accuracy"]),
    Precision = unname(cm$byClass["Precision"]),
    Recall = unname(cm$byClass["Sensitivity"]),
    F1_Score = unname(cm$byClass["F1"])
  )
  
  return(metrics_df)
}

# --------------------------- Execute model evaluation --------------------------- #
cm <- evaluate_model()
cm


