
server <- function(input, output, session) {
  source("FDS_Retrain_Model.R", local = TRUE)
  source("FDS_Model_Evaluation.R", local = TRUE)
  source("FDS_predict_tx.R", local = TRUE)
  source("FDS_DEMO_SIM.R", local = TRUE)
  source("FDS_Shiny_Functions.R", local = TRUE)
  
  # 📌 Meta System Functions
  #Simulate time
  month_time <- 1
  #Monat anpassen wenn knopf in UI header gedrückt wird
  month_time <<- update_month()
  
  model_update_trigger <- reactiveVal(Sys.time())
  history_update_trigger <- reactiveVal(Sys.time())
  training_mode <- reactiveVal(NULL)
  
  # 📌 Display model last update time
  output$last_update <- renderText({
    model_update_trigger() 
    if (file.exists("80_MODELS/fraud_model.rds")) {
      paste("Last Update:", file.info("80_MODELS/fraud_model.rds")$mtime)
    } else {
      "No model trained yet."
    }
  })
  
  # 📌 Helper: format numeric values
  format_df <- function(df, digits = 8) {
    df[] <- lapply(df, function(x) {
      if (is.numeric(x)) format(round(x, digits), nsmall = digits) else x
    })
    return(df)
  }
  
  # 📌 Initial training
  observeEvent(input$train_initial_model, {
    if (file.exists("80_MODELS/fraud_model.rds")) {
      output$update_status <- renderText("⚠️ Model already exists. Use Retrain instead.")
      return()
    }
    
    # Modell trainieren (Initial)
    result <- train_model(mode = "initial", ntree = input$rf_ntree, month_t = month_time())
    output$update_status <- renderText(result)
    training_mode("Initial Training")
    
    # Live-Metriken UND "New Model Metrics"-Box anzeigen
    if (file.exists("80_MODELS/fraud_model.rds") &&
        file.exists("99_DATA/test_data.rds") &&
        file.exists("99_DATA/test_labels.rds")) {
      
      metrics <- evaluate_model("80_MODELS/fraud_model.rds", 
                                "99_DATA/test_data.rds", 
                                "99_DATA/test_labels.rds")
      
      formatted_metrics <- format_df(metrics, 8)
      
      # Box anzeigen wie bei Retraining
      output$new_model_metrics <- renderTable(formatted_metrics, rownames = TRUE)
      
      output$new_model_best_tune <- renderText({
        if (is.list(result) && !is.null(result$best_tune)) {
          paste0("Model Information: mtry = ", result$best_tune$mtry)
        } else {
          "Model Information: not available"
        }
      })
      
      shinyjs::show("box_new_model")
      
      # Live Metrics auch sofort aktualisieren
      output$live_model_metrics <- renderTable(formatted_metrics, rownames = TRUE)
      
      output$live_model_best_tune <- renderText({
        if (!is.null(result$best_tune)) {
          paste0("Model Information: mtry = ", result$best_tune$mtry)
        } else {
          ""
        }
      })
      
      # Last Update Triggern
      model_update_trigger(Sys.time())
    }
  })
  
  
  
  # 📌 Retraining
  observeEvent(input$retrain_model, {
    result <- train_model(mode = "retrain", ntree = input$rf_ntree, month_t = month_time())
    
    if (is.list(result)) {
      training_mode("Retraining")
      output$update_status <- renderText(result$message)
      output$old_model_metrics <- renderTable({ format_df(result$old_model, 8) }, rownames = TRUE)
      output$old_model_best_tune <- renderText({
        paste0("Model Information: mtry = ", result$best_tune$mtry)
      })
      output$new_model_metrics <- renderTable({ format_df(result$new_model, 8) }, rownames = TRUE)
      output$new_model_best_tune <- renderText({
        paste0("Model Information: mtry = ", result$best_tune$mtry)
      })
      shinyjs::show("box_old_model")
      shinyjs::show("box_new_model")
    } else {
      output$update_status <- renderText(result)
      output$old_model_metrics <- renderTable(NULL)
      output$new_model_metrics <- renderTable(NULL)
      output$old_model_best_tune <- renderText({ NULL })
      output$new_model_best_tune <- renderText({ NULL })
    }
  })

  
  # 📌 Display header: Initial Training or Retraining
  output$new_model_header <- renderUI({
    h4(training_mode())
  })
  
  # 📌 Display live model metrics
  output$live_model_metrics <- renderTable({
    model_path <- "80_MODELS/fraud_model.rds"
    test_data_path <- "99_DATA/test_data.rds"
    test_labels_path <- "99_DATA/test_labels.rds"
    
    if (file.exists(model_path) && file.exists(test_data_path) && file.exists(test_labels_path)) {
      metrics_df <- evaluate_model(model_path, test_data_path, test_labels_path)
      format_df(metrics_df, 8)
    } else {
      data.frame(Message = "No model trained yet.")
    }
  }, rownames = FALSE)
  
  output$live_model_best_tune <- renderText({
    model_path <- "80_MODELS/fraud_model.rds"
    if (file.exists(model_path)) {
      model <- readRDS(model_path)
      if (!is.null(model$bestTune$mtry)) {
        paste0("Model Information: mtry = ", model$bestTune$mtry)
      } else {
        "Model Information: not recorded"
      }
    } else {
      ""
    }
  })
  
  # 📌 Accept new model
  observeEvent(input$accept_new_model, {
    new_model_path <- "80_MODELS/new_fraud_model.rds"
    final_model_path <- "80_MODELS/fraud_model.rds"
    
    if (file.exists(new_model_path)) {
      file.copy(from = new_model_path, to = final_model_path, overwrite = TRUE)
      
      output$update_status <- renderText("✅ Neues Modell übernommen und ist jetzt live.")
      shinyjs::hide("box_old_model")
      shinyjs::hide("box_new_model")
      training_mode(NULL)
      model_update_trigger(Sys.time())  # Triggert Live Metrics
    } else {
      output$update_status <- renderText("⚠️ Kein neues Modell zum Übernehmen vorhanden. Bitte zuerst retrain ausführen.")
    }
  })
  
  
  # 📌 Predict Transaction Fraud (demo)
  observeEvent(
    input$sim_tx, {
      predict_transactions(demo(month_t = month_time()))
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
    history_update_trigger(Sys.time())  # Triggert das Neu-Laden der Historie
  })
  
  # 📌 Reactive: Lade Transaktionshistorie
  history_data <- reactive({
    history_update_trigger()  # Trigger beobachten
    if (file.exists("99_DATA/tx_history.rds")) {
      readRDS("99_DATA/tx_history.rds")
    } else {
      data.frame(Message = "No history data available.")
    }
  })
  
  
  # 📌 Render DataTable für History
  output$tx_history_table <- DT::renderDataTable({
    datatable(history_data(),
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100)
              ),
              rownames = FALSE
    )
  })
  
  
  # 📌 Dashboard
  #map
  fraud_data <- reactive({
    req(rv$pending_data)
    rv$pending_data[rv$pending_data$Prediction == "Fraud", ]
  })
  
  # Leaflet-Karte anzeigen
  output$fraud_map <- renderLeaflet({
    data <- fraud_data()
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~x_terminal_id,
        lat = ~y_terminal_id,
        radius = 6,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste(
          "TX_ID:", TRANSACTION_ID, "<br>",
          "Customer_ID:", CUSTOMER_ID, "<br>",
          "Terminal_ID:", TERMINAL_ID, "<br>",
          "Amount:", TX_AMOUNT, "<br>",
          "Prediction:", Prediction, "<br>",
          "Time:", TX_TIME
        )
      )
  })
}

