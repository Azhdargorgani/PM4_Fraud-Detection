library(caret)

evaluate_model <- function(model_path = "80_MODELS/fraud_model.rds",
                           test_data_path = "99_DATA/test_data.rds", 
                           test_labels_path = "99_DATA/test_labels.rds") {
  
  # Lade Modell & Testdaten
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  
  # Sicherstellen, dass test_labels$TX_FRAUD als Faktor vorliegt mit den richtigen Levels
  test_labels$TX_FRAUD <- factor(test_labels$TX_FRAUD, levels = c("No Fraud", "Fraud"))
  
  # Sicherstellen, dass test_data KEIN TX_FRAUD enthält
  test_data <- test_data[, setdiff(names(test_data), "TX_FRAUD")]
  test_data <- as.data.frame(test_data)
  
  # Vorhersagen mit dem Modell
  predictions <- predict(model, newdata = test_data)
  predictions <- factor(predictions, levels = levels(test_labels$TX_FRAUD))
  
  # Erstelle die Confusion Matrix
  cm <- confusionMatrix(predictions, test_labels$TX_FRAUD)
  return(cm)
}

#--------------------------- Modellbewertung ausführen ---------------------------#
# cm <- evaluate_model()
# cm
# cat("\n",
#     "📊 Model Evaluation Metrics:\n",
#     "--------------------------------\n",
#     "✅ Accuracy:", cm$overall["Accuracy"], "\n",
#     "🎯 Precision:", cm$byClass["Precision"], "\n",
#     "🔁 Recall:", cm$byClass["Sensitivity"], "\n",
#     "📏 F1-Score:", cm$byClass["F1"], "\n",
#     "--------------------------------\n")

