source("FDS_Model_Evaluation.R", local = TRUE)
#___________________Model 1 (Fraud / not Fraud) retraining__________________________________

retrain_model <- function(hist_data_path = "99_DATA/historical_approved_data.rds", 
                          train_data_path = "99_DATA/train_data_Fraud.rds",
                          model_path = "80_MODELS/fraud_model.rds",
                          test_data_path = "99_DATA/test_data.rds",
                          test_labels_path = "99_DATA/test_labels.rds",
                          ntree = 100  # << NTREE als Parameter
) {
  # Check if main training data exists
  if (!file.exists(train_data_path)) {
    return("Error: No main training data found! Retraining aborted.")
  }
  
  # Load main (large) training data
  train_data <- readRDS(train_data_path)
  
  # Train new model with user-defined ntree
  new_model <- randomForest(TX_FRAUD ~ ., data = train_data, ntree = ntree)
  
  # Save old and new model
  old_model <- readRDS(model_path)
  saveRDS(new_model, "80_MODELS/new_fraud_model.rds")
  saveRDS(old_model, "80_MODELS/old_fraud_model.rds")
  
  # Evaluate both models (returning data.frames of metrics)
  old_metrics <- evaluate_model(model_path = "80_MODELS/old_fraud_model.rds",
                                test_data_path = test_data_path,
                                test_labels_path = test_labels_path)
  
  new_metrics <- evaluate_model(model_path = "80_MODELS/new_fraud_model.rds",
                                test_data_path = test_data_path,
                                test_labels_path = test_labels_path)
  
  return(list(old_model = old_metrics, new_model = new_metrics))
}



metrics_result <-retrain_model()

metrics_result$old_model
metrics_result$new_model














#___________________Model 2 (Fraud Scenario) retraining__________
