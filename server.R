library(shinydashboard)
library(randomForest)
library(dplyr)
library(DT)
library(shiny)
library(caret)


source("FDS_Retrain_Model.R", local = TRUE)
source("FDS_predict_tx.R", local = TRUE)
source("FDS_DEMO_SIM.R", local = TRUE)
source("FDS_Model_Evaluation.R", local = TRUE)

# 📦 Globale Variable zur Speicherung der Metriken
metrics_result <- NULL

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
        withCallingHandlers(
          {
            res <- retrain_model()
            output$update_status <- renderText("⚠️ Warning: No historical approved data found. Training only with main dataset.")
            res
          },
          warning = function(w) {
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        paste("❌ Error:", conditionMessage(e))
      }
    )
    
    # Hilfsfunktion: numerische Spalten formatieren (8 Nachkommastellen)
    format_df <- function(df, digits = 8) {
      df[] <- lapply(df, function(x) {
        if (is.numeric(x)) format(round(x, digits), nsmall = digits) else x
      })
      return(df)
    }
    
    if (is.list(result)) {
      output$old_model_metrics <- renderTable({
        format_df(result$old_model, 8)
      }, rownames = TRUE)
      
      output$new_model_metrics <- renderTable({
        format_df(result$new_model, 8)
      }, rownames = TRUE)
      
    } else {
      output$update_status <- renderText(result)
      output$old_model_metrics <- renderTable(NULL)
      output$new_model_metrics <- renderTable(NULL)
    }
  })
  
  
  # Modell übernehmen
  observeEvent(input$accept_new_model, {
    new_model_path <- "80_MODELS/new_fraud_model.rds"
    final_model_path <- "80_MODELS/fraud_model.rds"
    
    if (file.exists(new_model_path)) {
      file.copy(from = new_model_path, to = final_model_path, overwrite = TRUE)
      output$update_status <- renderText("✅ New model has been accepted and is now live.")
    } else {
      output$update_status <- renderText("❌ New model not found. Please retrain first.")
    }
  })
  
  
  # 📌 Display Model Accuracy
  
  # output$model_accuracy <- renderText({
  #   if (file.exists("80_MODELS/fraud_model.rds")) {
  #     test_data <- readRDS("80_MODELS/test_data.rds")
  #     model <- readRDS("80_MODELS/fraud_model.rds")
  #     acc <- sum(predict(model, test_data) == test_data$TX_FRAUD) / nrow(test_data)
  #     paste("Current Model Accuracy:", round(acc * 100, 2), "%")
  #   } else {
  #     "No model trained yet."
  #   }
  # })

  
  # 📌 Predict Transaction Fraud (demo)
  observeEvent(
    input$sim_tx, {
      predict_transactions(demo())
    }
  )
  
  # 📌 Historical Data pending (editable)
  rv <- reactiveValues(data = {
    if (file.exists("99_DATA/pending_history.rds")) {
      readRDS("99_DATA/pending_history.rds")
    } else {data.frame(Message = "No transactions yet")  
    }
  })

  output$transaction_table <- DT::renderDataTable({
    datatable(rv$data, editable = "cell", options = list(
      scrollY = "400px",
      scrollX = "400px"
    ))
  }, server = FALSE)
  
  # Capture and save table edits
  observeEvent(input$transaction_table_cell_edit, {
    info <- input$transaction_table_cell_edit
    row <- info$row
    col <- info$col
    new_value <- info$value
    
    # Ensure the edit is in the 'Prediction' column and legit lable
    if (col == which(names(rv$data) == "Prediction") && 
        new_value %in% c("Fraud", "no Fraud")){
      rv$data[row, col] <- new_value  
      saveRDS(rv$data, "99_DATA/pending_history.rds") 
    }
  })
  
  # Refresh table when button is clicked
  observeEvent(input$refresh_pend_history, {
    rv$data <- {
      if (file.exists("99_DATA/pending_history.rds")) {
        readRDS("99_DATA/pending_history.rds")
      } else {
        data.frame(Message = "No transactions yet")
      }
    }
  })
  
}










