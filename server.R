
server <- function(input, output, session) {
  # 📌 Meta System Functions¨
  #Simulate time
  month <- factor(c(1),
                  levels = 1:12,
                  labels = month.name)
  
  # 📌 Display last model update time
  model_update_trigger <- reactiveVal(Sys.time())
  
  output$last_update <- renderText({
    model_update_trigger() 
    if (file.exists("80_MODELS/fraud_model.rds")) {
      paste("Last Update:", file.info("80_MODELS/fraud_model.rds")$mtime)
    } else {
      "No model trained yet."
    }
  })
  

  # 📌 Load Actual Model Metrics on startup
  if (file.exists("80_MODELS/fraud_model.rds") &&
      file.exists("99_DATA/test_data.rds") &&
      file.exists("99_DATA/test_labels.rds")) {
    
    metrics_df <- evaluate_model(
      model_path = "80_MODELS/fraud_model.rds",
      test_data_path = "99_DATA/test_data.rds",
      test_labels_path = "99_DATA/test_labels.rds"
    )
    
    # Optional: formatieren (8 Nachkommastellen)
    format_df <- function(df, digits = 8) {
      df[] <- lapply(df, function(x) {
        if (is.numeric(x)) format(round(x, digits), nsmall = digits) else x
      })
      return(df)
    }
    
    output$live_model_metrics <- renderTable({
      format_df(metrics_df, 8)
    }, rownames = FALSE)
  }
  
  # 📌 Handle Model Retraining
  observeEvent(input$retrain_model, {
    result <- tryCatch(
      {
        withCallingHandlers(
          {
            res <- retrain_model(ntree = input$rf_ntree)
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
      
      # ✅ Boxen ausblenden
      shinyjs::hide("box_old_model")
      shinyjs::hide("box_new_model")
      
      model_update_trigger(Sys.time())
      
    } else {
      output$update_status <- renderText("❌ New model not found. Please retrain first.")
    }
  })
  
  # Optional: wieder anzeigen bei retrain
  observeEvent(input$retrain_model, {
    shinyjs::show("box_old_model")
    shinyjs::show("box_new_model")
    
    
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

