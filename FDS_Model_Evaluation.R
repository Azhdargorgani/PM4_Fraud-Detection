library(caret)

evaluate_model <- function(model_path = "80_MODELS/fraud_model.rds",
                           test_data_path = "99_DATA/test_data.rds", 
                           test_labels_path = "99_DATA/test_labels.rds") {
  
  if (!file.exists(model_path) || 
      !file.exists(test_data_path) || 
      !file.exists(test_labels_path)) {
    return(data.frame(Message = "Model or test data not found."))
  }
  
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  
  test_labels$TX_FRAUD <- factor(test_labels$TX_FRAUD, levels = c("No Fraud", "Fraud"))
  test_data <- test_data[, setdiff(names(test_data), "TX_FRAUD")]
  test_data <- as.data.frame(test_data)
  
  pred_class <- predict(model, newdata = test_data)
  pred_class <- factor(pred_class, levels = levels(test_labels$TX_FRAUD))
  pred_probs <- predict(model, newdata = test_data, type = "prob")[, "Fraud"]
  
  cm <- confusionMatrix(pred_class, test_labels$TX_FRAUD)
  

  auc <- pROC::auc(test_labels$TX_FRAUD, pred_probs)
  
  logloss <- MLmetrics::LogLoss(y_pred = pred_probs, y_true = ifelse(test_labels$TX_FRAUD == "Fraud", 1, 0))
  
  metrics_df <- data.frame(
    Accuracy = unname(cm$overall["Accuracy"]),
    Precision = unname(cm$byClass["Precision"]),
    Recall = unname(cm$byClass["Sensitivity"]),
    F1_Score = unname(cm$byClass["F1"]),
    AUC = as.numeric(auc),
    LogLoss = logloss
  )
  
  return(metrics_df)
}



# --------------------------- Execute model evaluation --------------------------- #
cm <- evaluate_model()
cm


