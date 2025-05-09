train_model <- function(mode = c("initial", "retrain"),
                        start_month,
                        end_month,
                        train_data_path = "99_DATA/train_data_Fraud.rds",
                        ntree = 100,
                        test_data_path = "99_DATA/test_data.rds",
                        test_labels_path = "99_DATA/test_labels.rds") {
  
  mode <- match.arg(mode)
  
  if (!file.exists(train_data_path)) return("❌ Error: Training data not found.")
  train_data <- readRDS(train_data_path)
  
  if (mode == "retrain") {
    train_data <- train_data[
      lubridate::month(train_data$TX_Date) >= start_month &
        lubridate::month(train_data$TX_Date) <= end_month, ]
  }
  
  train_data <- subset(train_data, select = -TX_Date)
  train_data$TX_FRAUD <- droplevels(factor(train_data$TX_FRAUD))
  if (length(unique(train_data$TX_FRAUD)) < 2) return("❌ Only one class in data.")
  
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
    metric = "F",
    ntree = ntree,
    weights = ifelse(train_data$TX_FRAUD == "Fraud", 20, 1)
  )
  
  # ✅ Timestamped saving
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  file_name <- paste0("fraud_model_", mode, "_", timestamp, ".rds")
  archive_path <- file.path("80_MODELS", file_name)
  saveRDS(model, archive_path)
  file.copy(archive_path, "80_MODELS/fraud_model.rds", overwrite = TRUE)
  
  if (file.exists(test_data_path) && file.exists(test_labels_path)) {
    metrics <- evaluate_model(archive_path, test_data_path, test_labels_path)
    return(list(message = paste("✅ Saved:", file_name), best_tune = model$bestTune, ntree = ntree, metrics = metrics))
  } else {
    return(list(message = paste("✅ Saved:", file_name), best_tune = model$bestTune, ntree = ntree))
  }
}

