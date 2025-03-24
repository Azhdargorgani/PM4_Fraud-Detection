
#___________________Model 1 (Fraud / not Fraud) retraining__________________________________

retrain_model <- function(hist_data_path = "99_DATA/historical_approved_data.rds", 
                          train_data_path = "99_DATA/train_data_Fraud.rds",
                          model_path = "80_MODELS/fraud_model.rds",
                          test_data_path = "99_DATA/test_data.rds",
                          test_labels_path = "99_DATA/test_labels.rds") {
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
  
  #save all 3 models
  old_model <- readRDS("80_MODELS/fraud_model.rds")
  saveRDS(new_model, "80_MODELS/new_fraud_model.rds")
  saveRDS(old_model, "80_MODELS/old_fraud_model.rds")

  
}
  
retrain_model()













#___________________Model 2 (Fraud Scenario) retraining__________
