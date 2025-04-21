source("FDS_Model_Evaluation.R", local = TRUE)

#___________________Model 1 (Fraud / not Fraud) retraining__________________________________

train_model <- function(mode = c("initial", "retrain"),
                        start_month,
                        end_month,
                        train_data_path = "99_DATA/train_data_Fraud.rds",
                        model_path = "80_MODELS/fraud_model.rds",
                        ntree = 100,
                        test_data_path = "99_DATA/test_data.rds",
                        test_labels_path = "99_DATA/test_labels.rds") {
  
  mode <- match.arg(mode)
  
  # 📥 Trainingsdaten einlesen
  if (!file.exists(train_data_path)) {
    return("❌ Error: Training data not found.")
  }
  train_data <- readRDS(train_data_path)
  
  # 📅 Filter nach Monatsbereich (inklusive Start- und Endmonat)
  train_data <- train_data[
    lubridate::month(train_data$TX_Date) >= start_month &
      lubridate::month(train_data$TX_Date) <= end_month, ]
  
  # 🧹 Datum entfernen
  train_data <- subset(train_data, select = -c(TX_Date))
  
  # 📦 Faktor bereinigen (wichtig!)
  train_data$TX_FRAUD <- factor(train_data$TX_FRAUD)
  train_data$TX_FRAUD <- droplevels(train_data$TX_FRAUD)
  
  # ❗Abbruch, falls nur eine Klasse vorhanden ist
  if (length(unique(train_data$TX_FRAUD)) < 2) {
    return("❌ Retraining abgebrochen: Nur eine Klasse (z. B. nur 'No Fraud') im Trainingszeitraum.")
  }
  
  if (mode == "initial") {
    if (file.exists(model_path)) {
      return("⚠️ Model already exists. Please use Retrain instead.")
    }
    
    # CV + Modelltraining mit mtry-Tuning
    ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
    tune_grid <- expand.grid(mtry = c(2, 5, 10, 13, 24))  # Beispielhafte Werte
    
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
  
  if (mode == "retrain") {
    if (!file.exists(model_path)) {
      return("❌ Error: No existing model found. Please train an initial model first.")
    }
    
    file.copy(from = model_path, to = "80_MODELS/old_fraud_model.rds", overwrite = TRUE)
    
    # training (with 10-fold CV)
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












#___________________Model 2 (Fraud Scenario) retraining__________
