
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

  
  #--------Only save model if better than old one---
  # Load test data for evaluation (if available)
    test_data <- readRDS(test_data_path)
    test_labels <- readRDS(test_labels_path)
    #bewertung modele noch überarbeiten evt funktion!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (file.exists(model_path)) {
      old_model <- readRDS(model_path)
      old_acc <- sum(predict(old_model, test_data) == test_labels$TX_FRAUD) / nrow(test_data)
      new_acc <- sum(predict(new_model, test_data) == test_labels$TX_FRAUD) / nrow(test_data)

      # Only save if the new model is better
      if (new_acc >= old_acc) {
        saveRDS(new_model, model_path)

        return("✅ New model trained and saved!")
        
      } else {
        return("⚠️ Old model was better. No changes made.")
        
      }

    }
  
}
  
retrain_model()













#___________________Model 2 (Fraud Scenario) retraining__________
