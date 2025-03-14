#object with different model pref. metrics


# Beispiel für Modellbewertung
evaluate_model <- function(model_path = "80_MODELS/fraud_model.rds",
                           test_data_path = "99_DATA/test_data.rds", 
                           test_labels_path = "99_DATA/test_labels.rds") {
  model <- readRDS(model_path)
  test_data <- readRDS(test_data_path)
  test_labels <- readRDS(test_labels_path)
  test_labels <- test_labels$TX_FRAUD
  predictions <- predict(model, newdata = test_data, type = "response")
  confusion <- confusionMatrix(factor(predictions), factor(test_labels$TX_FRAUD))
  
  return(confusion)
}



test_data_path = "99_DATA/test_data.rds"
test_data <- readRDS(test_data_path)
str(test_data)

test_labels_path = "99_DATA/test_labels.rds"
test_labels <- readRDS(test_labels_path)
str(test_labels)


cm <- evaluate_model()


cm$overall["Accuracy"]          # Zugriff auf Accuracy
cm$byClass["Sensitivity"]       # Zugriff auf Recall
cm$byClass["Specificity"]       # Zugriff auf Spezifität
cm$byClass["Precision"]         # Zugriff auf Precision
cm$byClass["F1"] 