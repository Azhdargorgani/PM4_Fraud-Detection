

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
      },
      warning = function(w) paste("⚠️ Warning:", conditionMessage(w)),
      error = function(e) paste("❌ Error:", conditionMessage(e))
    )
    
    output$update_status <- renderText(result)
  })
  
  # 📌 Model information
  #Display model stats
  output$model_accuracy <- renderText({
    if (file.exists("80_MODELS/fraud_model.rds")) {
      print("test")
      #model accuracy!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    } else {
      "No model trained yet."
    }
  })
  
  # 📌 Predict Transaction Fraud (demo)
  observeEvent(
    input$sim_tx, {
      predict_transactions(demo())
    }
  )
  
  # 📌 Historical Data pending table (editable)
  rv <- reactiveValues(data = {
    if (file.exists("99_DATA/pending_history.rds")) {
      readRDS("99_DATA/pending_history.rds")
    } else {data.frame(Message = "No transactions yet")  
    }
  })

  output$transaction_table <- DT::renderDataTable({
    datatable(rv$pending_data, editable = "cell", options = list(
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
    
    if (col == which(names(rv$pending_data) == "Prediction") && 
        new_value %in% c("Fraud", "no Fraud")){
      rv$pending_data[row, col] <- new_value  
      saveRDS(rv$pending_data, "99_DATA/pending_history.rds") 
    }
  })
  
  # Refresh table when button is clicked
  observeEvent(input$refresh_pend_history, {
    rv$pending_data <- {
      if (file.exists("99_DATA/pending_history.rds")) {
        readRDS("99_DATA/pending_history.rds")
      } else {
        data.frame(Message = "No transactions yet")
      }
    }
  })
  
  observeEvent(input$move_to_history, {
    rv$pending_data <- move_to_history(move_count = input$move_count)
  })
  
  
}










