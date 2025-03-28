source("FDS_Model_Evaluation.R", local = TRUE)
#___________________Model 1 (Fraud / not Fraud) retraining__________________________________

retrain_model <- function(hist_data_path = "99_DATA/historical_approved_data.rds", 
                          train_data_path = "99_DATA/train_data_Fraud.rds",
                          model_path = "80_MODELS/fraud_model.rds",
                          test_data_path = "99_DATA/test_data.rds",
                          test_labels_path = "99_DATA/test_labels.rds"
                          ) {
  # Check if main training data exists
  if (!file.exists(train_data_path)) {
    return("Error: No main training data found! Retraining aborted.")
  }
  
  # Load main (large) training data
  main_train_data <- readRDS(train_data_path)
  
  # Load approved historical training data (if available)
  if (file.exists(hist_data_path)) {
    hist_data <- readRDS(hist_data_path)
    train_data <- rbind(main_train_data, hist_data)
  } else {
    warning("No historical approved data found. Training only with main dataset.")
    train_data <- main_train_data}  # Train only with main data
  
  new_model <- randomForest(TX_FRAUD ~ ., data = train_data, ntree = 100)
  
  
  # Save all 3 models
  old_model <- readRDS("80_MODELS/fraud_model.rds")
  saveRDS(new_model, "80_MODELS/new_fraud_model.rds")
  saveRDS(old_model, "80_MODELS/old_fraud_model.rds")
  
  # Modell Evaluate 
  
  old_model_eval <- evaluate_model(model_path = "80_MODELS/old_fraud_model.rds")
  new_model_eval <- evaluate_model(model_path = "80_MODELS/new_fraud_model.rds")
  
  extract_metrics <- function(cm) {
    if (is.null(cm)) {
      return(data.frame(Accuracy = NA, Precision = NA, Recall = NA, F1_Score = NA))
    }
    return(data.frame(
      Accuracy = cm$overall["Accuracy"],
      Precision = cm$byClass["Precision"],
      Recall = cm$byClass["Sensitivity"],
      F1_Score = cm$byClass["F1"]
    ))
  }
  
  old_metrics <- extract_metrics(old_model_eval)
  new_metrics <- extract_metrics(new_model_eval)
  
  return(list(old_model = old_metrics, new_model = new_metrics))
}

metrics_result <-retrain_model()

metrics_result$old_model
metrics_result$new_model











#___________________Model 2 (Fraud Scenario) retraining__________
