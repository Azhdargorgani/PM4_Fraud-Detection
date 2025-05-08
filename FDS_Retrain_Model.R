library(caret)

# 📌 Hauptfunktion
train_model <- function(mode = c("initial", "retrain"),
                        start_month ,
                        end_month ,
                        train_data_path = "99_DATA/train_data_Fraud.rds",
                        model_path = "80_MODELS/fraud_model.rds",
                        ntree = 100,
                        test_data_path = "99_DATA/test_data.rds",
                        test_labels_path = "99_DATA/test_labels.rds") {
  
  mode <- match.arg(mode)
  
  # 📦 Trainingsdaten laden
  if (!file.exists(train_data_path)) {
    return("❌ Error: Training data not found.")
  }
  train_data <- readRDS(train_data_path)
  
  # 📅 Nur bei retrain nach Monat filtern
  if (mode == "retrain") {
    train_data <- train_data[
      lubridate::month(train_data$TX_Date) >= start_month &
        lubridate::month(train_data$TX_Date) <= end_month, ]
  }
  
  train_data <- subset(train_data, select = -TX_Date)
  train_data$TX_FRAUD <- droplevels(factor(train_data$TX_FRAUD))
  
  # ❗ Nur wenn beide Klassen vorhanden sind
  if (length(unique(train_data$TX_FRAUD)) < 2) {
    return("❌ Training aborted: Only one class in the data.")
  }
  
  # INITIAL TRAINING ---------------------------------------------------
  if (mode == "initial") {
    if (file.exists(model_path)) {
      return("⚠️ Model already exists. Please use retrain instead.")
    }
    ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, sampling = "smote")
    tune_grid <- expand.grid(mtry = c(2, 5, 10))
    
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
  
  # RETRAINING ---------------------------------------------------------
  if (mode == "retrain") {
    if (!file.exists(model_path)) {
      return("❌ Error: No existing model found. Please train an initial model first.")
    }
    
    file.copy(from = model_path, to = "80_MODELS/old_fraud_model.rds", overwrite = TRUE)
    print(table(train_data$TX_FRAUD))
    print("_____________-----____________")
    
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      verboseIter = TRUE,
      classProbs = TRUE,
      summaryFunction = prSummary,
      sampling = "smote"
    )
    
    model <- train(
      TX_FRAUD ~ .,
      data = train_data,
      method = "rf",
      trControl = ctrl,
      tuneLength = 5,
      ntree = ntree,
      metric = "F",
      weights = ifelse(train_data$TX_FRAUD == "Fraud", 20, 1)
    )
    
    new_model <-model
    
    saveRDS(new_model, "80_MODELS/new_fraud_model.rds")
    
    # ✅ Füge Testdaten-Check wieder hinzu!
    if (file.exists(test_data_path) && file.exists(test_labels_path)) {
      
      new_metrics <- evaluate_model(
        model_path = "80_MODELS/new_fraud_model.rds",
        test_data_path = test_data_path,
        test_labels_path = test_labels_path
      )
      
      old_metrics <- evaluate_model(
        model_path = "80_MODELS/old_fraud_model.rds",
        test_data_path = test_data_path,
        test_labels_path = test_labels_path,
      )
      
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
