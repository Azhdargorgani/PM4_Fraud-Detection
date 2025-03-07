
library(shiny)
library(shinydashboard)
library(randomForest)
library(dplyr)
library(DT)


source("FDS_Retrain_Model.R", local = TRUE)
source("FDS_predict_tx.R", local = TRUE)

server <- function(input, output, session) {
  # 📌 Display last model update time
  output$last_update <- renderText({
    if (file.exists("80_MODELS/fraud_model.rds")) {
      paste("Last Update:", file.info("80_MODELS/fraud_model.rds")$mtime)
    } else {
      "No model trained yet."
    }
  })
  
  # 📌 Handle Model Retraining
  observeEvent(input$retrain_model, {
    result <- tryCatch(
      {
        retrain_model()  # Calls the retraining function from external script
        "✅ Model successfully updated!"
      },
      warning = function(w) paste("⚠️ Warning:", conditionMessage(w)),
      error = function(e) paste("❌ Error:", conditionMessage(e))
    )
    
    output$update_status <- renderText(result)
  })
  
  # 📌 Display Model Accuracy
  output$model_accuracy <- renderText({
    if (file.exists("80_MODELS/fraud_model.rds")) {
      test_data <- readRDS("80_MODELS/test_data.rds")
      model <- readRDS("80_MODELS/fraud_model.rds")
      acc <- sum(predict(model, test_data) == test_data$TX_FRAUD) / nrow(test_data)
      paste("Current Model Accuracy:", round(acc * 100, 2), "%")
    } else {
      "No model trained yet."
    }
  })
  
  # 📌 Simulate a Transaction and predict
  observeEvent(input$sim_tx,{
    demo_data <- readRDS("99_DATA/demo_data.rds")
    tx_index <-sample(1:nrow(demo_data), size = sample(1:10, 1))
    tx_demo<- demo_data[tx_index,]
    demo_data <- demo_data[-c(tx_index),]
    saveRDS(demo_data, "99_DATA/demo_data.rds")
    
    predict_transactions(tx_demo)
  })
  
  # 📌 Load pending transactions for display
  output$transaction_table <- renderDataTable({
    if (file.exists("99_DATA/pending_history.rds")) {
      readRDS("99_DATA/pending_history.rds")
    } else {
      data.frame(Message = "No transactions yet")
    }
  }, options = list(
    scrollY = "400px",
    scrollX = "400px"
  ))
}










