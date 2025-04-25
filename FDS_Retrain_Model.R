#___________________Model 1 (Fraud / No Fraud) Retraining__________________________________

train_model <- function(mode = c("initial", "retrain"),
                        start_month,
                        end_month,
                        train_data_path = "99_DATA/train_data_Fraud.rds",
                        model_path = "80_MODELS/fraud_model.rds",
                        ntree = 100,
                        test_data_path = "99_DATA/test_data.rds",
                        test_labels_path = "99_DATA/test_labels.rds") {
  
  mode <- match.arg(mode)
  
  # 📥 Load training data
  if (!file.exists(train_data_path)) {
    return("❌ Error: Training data not found.")
  }
  train_data <- readRDS(train_data_path)
  
  # 📅 Filter by month range (inclusive of start and end month)
  train_data <- train_data[
    lubridate::month(train_data$TX_Date) >= start_month &
      lubridate::month(train_data$TX_Date) <= end_month, ]
  
  # 🧹 Remove date column
  train_data <- subset(train_data, select = -c(TX_Date))
  
  # 📦 Clean factor levels (important!)
  train_data$TX_FRAUD <- factor(train_data$TX_FRAUD)
  train_data$TX_FRAUD <- droplevels(train_data$TX_FRAUD)
  
  # ❗Abort if only one class is present
  if (length(unique(train_data$TX_FRAUD)) < 2) {
    return("❌ Retraining aborted: Only one class (e.g., only 'No Fraud') in the training period.")
  }
  
  # Initial training -------------------------------------------------------------
  if (mode == "initial") {
    if (file.exists(model_path)) {
      return("⚠️ Model already exists. Please use retrain instead.")
    }
    
    # CV + model training with mtry tuning
    ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
    tune_grid <- expand.grid(mtry = c(2, 5, 10, 13, 24))  # Example values
    
    model <- train(
      TX_FRAUD ~ .,
      data = train_data,
      method = "rf",
      trControl = ctrl,
      tuneGrid = tune_grid,
      ntree = ntree
    )
    saveRDS(model, model_path)
    
    return(list(
      message = "✅ Initial model trained and saved as fraud_model.rds",
      best_tune = model$bestTune,
      ntree = ntree
    ))
  }
  
  # Retraining --------------------------------------------------------------------
  if (mode == "retrain") {
    if (!file.exists(model_path)) {
      return("❌ Error: No existing model found. Please train an initial model first.")
    }
    
    file.copy(from = model_path, to = "80_MODELS/old_fraud_model.rds", overwrite = TRUE)
    
    # Retrain (with 10-fold cv)
    ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
    tune_grid <- expand.grid(mtry = c(2, 5, 10, 13, 24))
    
    new_model <- train(
      TX_FRAUD ~ .,
      data = train_data,
      method = "rf",
      trControl = ctrl,
      tuneGrid = tune_grid,
      ntree = ntree
    )
    
    saveRDS(new_model, "80_MODELS/new_fraud_model.rds")
    
    if (file.exists(test_data_path) && file.exists(test_labels_path)) {
      old_metrics <- evaluate_model("80_MODELS/old_fraud_model.rds", test_data_path, test_labels_path)
      new_metrics <- evaluate_model("80_MODELS/new_fraud_model.rds", test_data_path, test_labels_path)
      
      return(list(
        message = "✅ Model retrained and evaluated.",
        best_tune = new_model$bestTune,
        ntree = ntree,
        old_model = old_metrics,
        new_model = new_metrics
      ))
    } else {
      return(list(
        message = "✅ Model retrained. No test data available for evaluation.",
        best_tune = new_model$bestTune
      ))
    }
  }
}
