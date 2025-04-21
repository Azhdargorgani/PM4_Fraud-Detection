
server <- function(input, output, session) {
  source("FDS_Retrain_Model.R", local = TRUE)
  source("FDS_Model_Evaluation.R", local = TRUE)
  source("FDS_predict_tx.R", local = TRUE)
  source("FDS_DEMO_SIM.R", local = TRUE)
  source("FDS_Shiny_Functions.R", local = TRUE)
  
  # 📌 Meta System Functions
  
  #Monat anpassen wenn knopf in UI header gedrückt wird
  month_time <- update_month()
  
  # 📌 Reactive values and triggers
  model_update_trigger <- reactiveVal(Sys.time())
  history_update_trigger <- reactiveVal(Sys.time())
  training_mode <- reactiveVal(NULL)
  model_info_live <- reactiveVal("Model Information: not available")
  
  # 📌 Helper: format numeric values
  format_df <- function(df, digits = 8) {
    df[] <- lapply(df, function(x) {
      if (is.numeric(x)) format(round(x, digits), nsmall = digits) else x
    })
    return(df)
  }

  # 📌 Display header: Initial Training or Retraining
  output$new_model_header <- renderUI(
    h4(training_mode())
  )
  
  # 📌 Display best hyperparameters of the live model
  output$live_model_best_tune <- renderText(
    model_info_live()
  )
  
  # 📌 Display metrics of the current live model
  output$live_model_metrics <- renderTable({
    req(
      file.exists("80_MODELS/fraud_model.rds"),
      file.exists("99_DATA/test_data.rds"),
      file.exists("99_DATA/test_labels.rds")
    )
    format_df(
      evaluate_model(
        "80_MODELS/fraud_model.rds",
        "99_DATA/test_data.rds",
        "99_DATA/test_labels.rds"
      ), 8
    )
  }, rownames = FALSE)
  
  # 📌 Display last update timestamp of the live model
  output$last_update <- renderText({
    model_update_trigger()
    if (file.exists("80_MODELS/fraud_model.rds")) {
      paste("Last Update:", file.info("80_MODELS/fraud_model.rds")$mtime)
    } else {
      "No model trained yet."
    }
  })
  
  # 📌 Observe and update model information when file changes
  observe({
    model_update_trigger()
    if (!file.exists("80_MODELS/fraud_model.rds")) return()
    model <- readRDS("80_MODELS/fraud_model.rds")
    mtry_val <- tryCatch(model$bestTune$mtry, error = function(e) NA)
    ntree_val <- tryCatch(model$ntree %||% model$finalModel$ntree, error = function(e) NA)
    model_info_live(paste0("Model Information: mtry = ", mtry_val, " | ntree = ", ntree_val))
  })
  

  # 📌 Observe and update retrain range when simulation month changes
  observe({
    req(month_time())
    train_data <- readRDS("99_DATA/train_data_Fraud.rds")
    available_months <- sort(unique(lubridate::month(train_data$TX_Date)))
    selected_month <- month_time()
    filtered <- available_months[available_months < selected_month]
    end_month <- max(filtered, na.rm = TRUE)
    updateSliderTextInput(session, "retrain_range",
                          choices = month.name[if (length(filtered) > 0) filtered else available_months],
                          selected = month.name[if (length(filtered) > 0) c(min(filtered), end_month) else selected_month])
  })
  
  # 📌 Initial training
  observeEvent(input$train_initial_model, {
    if (input$rf_ntree > 200) {
      output$update_status <- renderText("⚠️ Maximal 200 Bäume erlaubt.")
      return()
    }
    if (file.exists("80_MODELS/fraud_model.rds")) {
      output$update_status <- renderText("\u26A0\uFE0F Model already exists. Use Retrain instead.")
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
        mtry_val <- if (!is.null(result$best_tune$mtry)) result$best_tune$mtry else NA
        paste0("Model Information: mtry = ", mtry_val, " | ntree = ", input$rf_ntree)
      })
      
      shinyjs::show("box_new_model")
      
      # Live Metrics auch sofort aktualisieren
      output$live_model_metrics <- renderTable(formatted_metrics, rownames = TRUE)
      model_update_trigger(Sys.time())
    }
  })
  

  # 📌 Retraining
  observeEvent(input$retrain_model, {
    if (input$rf_ntree > 200) {
      output$update_status <- renderText("⚠️ Maximal 200 Bäume erlaubt.")
      return()
    }
    
    # 📌 Training period check
    month_range <- sort(match(input$retrain_range, month.name))
    if (any(is.na(month_range)) || length(month_range) < 1) {
      showModal(modalDialog(
        title = "⚠️ Ungültiger Trainingszeitraum",
        "Bitte mindestens einen gültigen Monat auswählen.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    start_month <- month_range[1]
    end_month <- month_range[length(month_range)]
    
    result <- train_model(
      mode = "retrain",
      ntree = input$rf_ntree,
      start_month = start_month,
      end_month = end_month
    )
    
    if (is.list(result)) {
      training_mode("Retraining")
      output$update_status <- renderText(result$message)
      
      output$old_model_metrics <- renderTable({ format_df(result$old_model, 8) }, rownames = TRUE)
      output$old_model_best_tune <- renderText(model_info_live())
      
      output$new_model_metrics <- renderTable({ format_df(result$new_model, 8) }, rownames = TRUE)
      output$new_model_best_tune <- renderText({
        paste0("Model Information: mtry = ", result$best_tune$mtry, " | ntree = ", result$ntree)
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
    datatable(
      history_data(),
      options = list(
        scrollY = "650px",           # 🔧 nur Tabelle scrollt
        scrollX = TRUE,              # 🔧 horizontales Scrollen aktivieren
        scrollCollapse = TRUE,       # 🔧 Container bleibt fix
        pageLength = 100,            # Optional: viele Zeilen anzeigen
        dom = 't',                   # Optional: nur Tabelle (kein Suchfeld/Pager)
        lengthMenu = c(10, 25, 50, 100)
      ),
      rownames = FALSE,
      class = 'compact stripe hover'
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



