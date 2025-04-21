
save_live_metrics <- function(
    current_month, #aktueller monat month_time() als input
    model_path = "80_MODELS/fraud_model.rds",
    test_data_path = "99_DATA/test_data.rds",
    test_labels_path = "99_DATA/test_labels.rds",
    output_path = "70_Performance_Hist/metrics.rds"
) {
  #Falls ein file nicht existiert abfangen
  if (!file.exists(model_path) || !file.exists(test_data_path) || !file.exists(test_labels_path)) {
    return(NULL)
  }
  
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  
  #AUf den monat filtern
  current_month_data <- test_data[lubridate::month(test_data$TX_Date) == current_month, ]
  if (nrow(current_month_data) == 0) return(NULL)
  
  # passenden labels mit der transaction ID
  current_labels <- test_labels[test_labels$TRANSACTION_ID %in% current_month_data$TRANSACTION_ID, ]
  
  # Save temporary filtered files weil evaluate einen pfad braucht
  saveRDS(current_month_data, "99_DATA/temp_test_data_month.rds")
  saveRDS(current_labels, "99_DATA/temp_test_labels_month.rds")
  
  # Evaluate
  metrics <- evaluate_model(
    model_path = model_path,
    test_data_path = "99_DATA/temp_test_data_month.rds",
    test_labels_path = "99_DATA/temp_test_labels_month.rds"
  )
  # Anzahl tatsächlicher Fraud-Fälle im Monat
  test_frauds <- sum(current_labels$TX_FRAUD == "Fraud", na.rm = TRUE)
  
  #Ablegen der metriken
  acc_row <- data.frame(
    Month = current_month,
    Accuracy = round(metrics["Accuracy"], 4),
    Precision = round(metrics["Precision"], 4),
    Recall = round(metrics["Recall"], 4),
    F1_Score = round(metrics["F1_Score"], 4),
    n_Frauds = test_frauds
  )
  
  #Temp daten entfernen
  file.remove("99_DATA/temp_test_data_month.rds")
  file.remove("99_DATA/temp_test_labels_month.rds")
  
  # Abgelegte Metriken überschreiber oder schreiben
  if (file.exists(output_path)) {
    old_df <- readRDS(output_path)
    
    # Remove old row with same month if it exists
    old_df <- old_df[old_df$Month != current_month, ]
    
    # Combine
    combined_df <- rbind(old_df, acc_row)
    combined_df <- combined_df[order(combined_df$Month), ]
  } else {
    combined_df <- acc_row
  }
  
  #Speichern
  saveRDS(combined_df, output_path)
  return(combined_df)
}

