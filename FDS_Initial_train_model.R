
#___________________Model 1 (Fraud / not Fraud) training__________________________________
library("randomForest")

train_model <- function(train_data_path = "99_DATA/train_data_Fraud.rds", 
                        model_path = "80_MODELS/fraud_model.rds") {
  train_data_fraud <- readRDS("99_DATA/train_data_Fraud.rds")
  # Trainiere das erste Model
  rf_model <- randomForest(TX_FRAUD ~., data = train_data_fraud, ntree= 100)
  
  # Speichere das initiale Modell
  saveRDS(rf_model, model_path)
  
  return("Initiales Modell wurde erfolgreich trainiert und gespeichert!")
}

train_model() 
#___________________Model 2 (Fraud Scenario) training__________


