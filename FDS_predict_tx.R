# --------------------------- Predict new transactions --------------------------- #
predict_transactions <- function(transactions, 
                                 model_path = "80_MODELS/fraud_model.rds", 
                                 pending_history_path = "99_DATA/pending_history.rds",
                                 cutoff = 0.5) {
  if (!file.exists(model_path)) {
    stop("Error: Model does not exist!")
  }
  
  rf_model <- readRDS(model_path)
  
  # Get predicted probabilities for class "Fraud"
  pred_probs <- predict(rf_model, newdata = transactions, type = "prob")[, "Fraud"]
  
  # Apply custom cutoff
  transactions$Prediction <- ifelse(pred_probs > cutoff, "Fraud", "Legit")
  
  # Initialize ManualLabel with prediction
  transactions$ManualLabel <- transactions$Prediction
  
  print(transactions$Prediction)
  
  # Append predictions to pending history
  if (file.exists(pending_history_path)) {
    pending_data <- readRDS(pending_history_path)
    pending_data <- rbind(pending_data, transactions)
  } else {
    pending_data <- transactions
  }
  
  saveRDS(pending_data, file = pending_history_path)
  return("✅ Transaction predicted and stored pending review")
}


