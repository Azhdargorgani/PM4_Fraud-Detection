library(caret)

evaluate_model <- function(model_path = "80_MODELS/fraud_model.rds",
                           test_data_path = "99_DATA/test_data.rds", 
                           test_labels_path = "99_DATA/test_labels.rds",
                           cutoff = 0.5) {
  
  if (!file.exists(model_path) || 
      !file.exists(test_data_path) || 
      !file.exists(test_labels_path)) {
    return(list(
      metrics = data.frame(Message = "Model or test data not found."),
      confusion = NULL
    ))
  }
  
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  
  test_labels$TX_FRAUD <- factor(test_labels$TX_FRAUD, levels = c("Legit", "Fraud"))
  pred_probs <- predict(model, newdata = test_data, type = "prob")[, "Fraud"]
  
  pred_class <- ifelse(pred_probs >= cutoff, "Fraud", "Legit")
  pred_class <- factor(pred_class, levels = c("Legit", "Fraud"))
  
  cm <- confusionMatrix(pred_class, test_labels$TX_FRAUD, mode = "prec_recall", positive = "Fraud")
  
  logloss <- MLmetrics::LogLoss(y_pred = pred_probs, y_true = ifelse(test_labels$TX_FRAUD == "Fraud", 1, 0))
  
  auc_value <- if (nrow(test_labels) == 0 || length(unique(test_labels$TX_FRAUD)) < 2) {
    NA
  } else {
    roc_obj <- pROC::roc(response = test_labels$TX_FRAUD, 
                         predictor = pred_probs, 
                         levels = c("Legit", "Fraud"), 
                         direction = "<")
    as.numeric(pROC::auc(roc_obj))
  }
  
  metrics_df <- data.frame(
    Accuracy = unname(cm$overall["Accuracy"]),
    Precision = unname(cm$byClass["Precision"]),
    Recall = unname(cm$byClass["Recall"]),
    F1_Score = unname(cm$byClass["F1"]),
    AUC = auc_value,
    LogLoss = logloss
  )
  
  return(list(
    metrics = metrics_df,
    confusion = as.table(cm$table)  # Rückgabe als Tabelle für Anzeige
  ))
}

    


# --------------------------- Execute model evaluation --------------------------- #
cm <- evaluate_model()
cm


