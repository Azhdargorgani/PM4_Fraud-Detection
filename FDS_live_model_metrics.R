
save_live_metrics <- function(
    current_month,  # current month from month_time()
    model_path = "80_MODELS/fraud_model.rds",
    test_data_path = "99_DATA/test_data.rds",
    test_labels_path = "99_DATA/test_labels.rds",
    output_path = "70_Performance_Hist/metrics.rds"
) {
  # Reset history in May (month 5)
  if (current_month == 5 && file.exists(output_path)) {
    file.remove(output_path)
  }
  
  # Abort if any required file is missing
  if (!file.exists(model_path) || !file.exists(test_data_path) || !file.exists(test_labels_path)) {
    return(NULL)
  }
  
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  
  # Filter to current month if exists
  current_month_data <- test_data[lubridate::month(test_data$TX_Date) == current_month, ]
  if (nrow(current_month_data) == 0) return(NULL)
  
  # Match labels to filtered transactions
  current_labels <- test_labels[test_labels$TRANSACTION_ID %in% current_month_data$TRANSACTION_ID, ]
  
  # Save temporary filtered files for evaluation
  saveRDS(current_month_data, "99_DATA/temp_test_data_month.rds")
  saveRDS(current_labels, "99_DATA/temp_test_labels_month.rds")
  
  # Evaluate model
  result <- evaluate_model(
    model_path = model_path,
    test_data_path = "99_DATA/temp_test_data_month.rds",
    test_labels_path = "99_DATA/temp_test_labels_month.rds"
  )
  metrics <- result$metrics
  
  
  # Count number of actual frauds in the month
  test_frauds <- sum(current_labels$TX_FRAUD == "Fraud", na.rm = TRUE)
  
  # Create metrics row
  acc_row <- data.frame(
    Month = current_month,
    Accuracy = round(metrics$Accuracy, 4),
    Precision = round(metrics$Precision, 4),
    Recall = round(metrics$Recall, 4),
    F1_Score = round(metrics$F1_Score, 4),
    AUC = round(metrics$AUC, 4),
    LogLoss = round(metrics$LogLoss, 4),
    n_Frauds = test_frauds
  )
  
  # Remove temporary files
  file.remove("99_DATA/temp_test_data_month.rds")
  file.remove("99_DATA/temp_test_labels_month.rds")
  
  # Append or overwrite metrics file
  if (file.exists(output_path)) {
    old_df <- readRDS(output_path)
    
    # Remove old entry for this month
    old_df <- old_df[old_df$Month != current_month, ]
    
    # Combine new and old
    combined_df <- rbind(old_df, acc_row)
    combined_df <- combined_df[order(combined_df$Month), ]
  } else {
    combined_df <- acc_row
  }
  
  # Save to output path
  saveRDS(combined_df, output_path)
  return(combined_df)
}
