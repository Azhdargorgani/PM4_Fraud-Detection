
server <- function(input, output, session) {
  source("FDS_Retrain_Model.R", local = TRUE)
  source("FDS_Model_Evaluation.R", local = TRUE)
  source("FDS_predict_tx.R", local = TRUE)
  source("FDS_DEMO_SIM.R", local = TRUE)
  source("FDS_Shiny_Functions.R", local = TRUE)
  source("FDS_live_model_metrics.R", local = TRUE)
  
  # 📌 Meta System Functions----------------------------------------------------
  #Monat anpassen wenn knopf in UI header gedrückt wird
  month_time <- update_month()
  
  # 📌 Reactive values and triggers
  model_update_trigger <- reactiveVal(Sys.time())
  history_update_trigger <- reactiveVal(Sys.time())
  pending_update_trigger <- reactiveVal(Sys.time())
  training_mode <- reactiveVal(NULL)
  model_info_live <- reactiveVal("Model Information: not available")
  
  # 📌 Helper: format numeric values
  format_df <- function(df, digits = 8) {
    df[] <- lapply(df, function(x) {
      if (is.numeric(x)) format(round(x, digits), nsmall = digits) else x
    })
    return(df)
  }
  # 📌 Model Information--------------------------------------------------------
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
    model_update_trigger()
    
    req(
      file.exists("80_MODELS/fraud_model.rds"),
      file.exists("99_DATA/test_data.rds"),
      file.exists("99_DATA/test_labels.rds")
    )
    
    metrics <- evaluate_model(
      "80_MODELS/fraud_model.rds",
      "99_DATA/test_data.rds",
      "99_DATA/test_labels.rds"
    )
    
    values <- as.numeric(metrics[1, c("Accuracy", "Precision", "Recall", "F1_Score")])
    formatted <- format(round(values, 4), decimal.mark = ",", nsmall = 4)
    
    data.frame(
      Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
      Value = formatted,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  })
  
  
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
    if (input$rf_ntree > 1000) {
      output$update_status <- renderText("⚠️ Maximal1000 Bäume erlaubt.")
      return()
    }
    if (file.exists("80_MODELS/fraud_model.rds")) {
      output$update_status <- renderText("\u26A0\uFE0F Model already exists. Use Retrain instead.")
      return()
    }
    
    # 👉 Monatsbereich extrahieren
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
    
    # Modell trainieren (Initial)
    result <- train_model(
      mode = "initial",
      ntree = input$rf_ntree,
      start_month = start_month,
      end_month = end_month
    )
    
    
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
    if (input$rf_ntree > 1000) {
      output$update_status <- renderText("⚠️ Maximal1000 Bäume erlaubt.")
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

  # 📌 #Live Metriken des Aktuellen Modells
  output$model_info_metrics_box <- renderUI({
    output$model_info_metrics_table <- renderTable({
      if (!file.exists("70_Performance_Hist/metrics.rds")) {
        return(data.frame(Message = "No metrics available yet."))
      }
      
      df <- readRDS("70_Performance_Hist/metrics.rds")
      current_month <- month_time()
      row <- df[df$Month == current_month, ]
      
      if (nrow(row) == 0) {
        return(data.frame(Message = "No metrics found for current month."))
      }
      
      # Format output nicely as vertical key-value table
      values <- as.numeric(row[1, c("Accuracy", "Precision", "Recall", "F1_Score"), drop = TRUE])
      
      data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
        Value = format(round(values, 6), decimal.mark = ",", nsmall = 4),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    })
  })

  # 📌 Transaction pending Review-----------------------------------------------------
  
  # 📌 Historical Data pending table (editable)
  rv <- reactiveValues(pending_data = {
    if (file.exists("99_DATA/pending_history.rds")) {
      readRDS("99_DATA/pending_history.rds")
    } else {data.frame(Message = "No transactions yet")  
    }
  })
  
  #das es automatisch aktualisiert wird
  observe({
    invalidateLater(2000, session)  # alle 2 Sekunden checken
    if (file.exists("99_DATA/pending_history.rds")) {
      rv$pending_data <- readRDS("99_DATA/pending_history.rds")
    }
  })
  
  output$transaction_table <- DT::renderDataTable({
    datatable(
      rv$pending_data,
      editable = list(
        target = "cell",
        columns = which(names(rv$pending_data) == "ManualLabel")  # Nur diese Spalte
      ),
      options = list(
        scrollY = "400px",
        scrollX = "400px"
      )
    )
  }, server = FALSE)
  
  # Capture and save table edits
  observeEvent(input$transaction_table_cell_edit, {
    info <- input$transaction_table_cell_edit
    row <- info$row
    colname <- names(rv$pending_data)[info$col]
    new_value <- info$value
    
    if (colname == "ManualLabel" && new_value %in% c("Fraud", "no Fraud")) {
      rv$pending_data[row, "ManualLabel"] <- new_value
      saveRDS(rv$pending_data, "99_DATA/pending_history.rds")
    }
  })
  
  observeEvent(input$move_to_history, {
    rv$pending_data <- move_to_history(move_count = input$move_count)
    print("📦 Moved to history")
  })

# 📌 Transaction History-----------------------------------------------------
  # 📌 Reactive: Lade Transaktionshistorie
  if (!file.exists("99_DATA/tx_history.rds")) {
    saveRDS(data.frame(), "99_DATA/tx_history.rds")
  }
  
  # Dann reaktiv lesen mit Beobachtung
  history_data <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = "99_DATA/tx_history.rds",
    readFunc = readRDS
  )
  
  # 📌 Render DataTable für History
  output$tx_history_table <- DT::renderDataTable({
    datatable(
      history_data(),
      options = list(
        scrollY = "540px",
        scrollX = "400px"
      )
    )
  })
  
  
  # 📌 Dashboard---------------------------------------------------------------
  # 📌 Predict Transaction Fraud (demo)
  observeEvent(
    input$sim_tx, {
      predict_transactions(demo(month_t = month_time()))
    }
  )
  # 📌 map
  fraud_data <- reactive({
    req(rv$pending_data)
    rv$pending_data[rv$pending_data$ManualLabel == "Fraud", ]
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
  
  #Metriken Anzeigen
  output$dashboard_metrics_box <- renderUI({
    req(file.exists("70_Performance_Hist/metrics.rds"))
    metrics <- readRDS("70_Performance_Hist/metrics.rds")
    
    # Aktueller Monat
    current_month <- month_time()
    
    # Zeile für aktuellen Monat herausfiltern
    row <- metrics[metrics$Month == current_month, ]
    
    if (nrow(row) == 0) {
      return(tags$div("No metrics for this month"))
    }
    
    # Mehrere valueBox nebeneinander anzeigen
    fluidRow(
      valueBox(paste0(row$Accuracy * 100, "%"), "Accuracy", color = "blue", icon = icon("check")),
      valueBox(paste0(row$Precision * 100, "%"), "Precision", color = "blue", icon = icon("bullseye")),
      valueBox(paste0(row$Recall * 100, "%"), "Recall", color = "blue", icon = icon("sync-alt")),
      valueBox(paste0(row$F1_Score * 100, "%"), "F1 Score", color = "blue", icon = icon("star"))
    )
  })
  

  
  #Anzahl Fraude dieser Monat (Sim daten nicht train daten)
  pending_data <- if (file.exists("99_DATA/pending_history.rds")) {
    reactiveFileReader(
      intervalMillis = 2000,
      session = session,
      filePath = "99_DATA/pending_history.rds",
      readFunc = readRDS
    )
  } else {
    reactive({ data.frame() })  # leeres DataFrame zurückgeben
  }
  
  sim_fraud_count <- reactive({
    # Daten laden
    pending <- pending_data()
    history <- history_data()

    all_data <- rbind(pending, history)
    # Filtern: aktueller Monat & "Fraud"
    filtered <- all_data[lubridate::month(all_data$TX_Date) == month_time() & all_data$ManualLabel == "Fraud",]
    
    nrow(filtered)
  })
  # Anzahl Fraud sim
  output$monthly_fraud_box <- renderUI({
    valueBox(
      value = sim_fraud_count(),
      subtitle = paste("Number of Frauds Today"),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Plot der preformance trends 
  metrics_data <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = "70_Performance_Hist/metrics.rds",
    readFunc = readRDS
  )
  
  output$metric_trend_plot <- renderPlot({
    req(input$selected_metric)
    
    df <- metrics_data()
    df <- df[order(df$Month), ]
    
    metric <- input$selected_metric
    values <- as.numeric(df[[metric]])
    df$MonthLabel <- factor(month.name[df$Month], levels = month.name)
    
    ymin <- min(values) - 0.002
    ymax <- max(values) + 0.002
    if (ymin < 0.95) ymin <- 0.99
    if (ymax > 1.0) ymax <- 1.0
    
    plot(months <- df$Month, values,
         type = "o",
         col = "darkblue",
         lwd = 2,
         ylim = c(ymin, ymax),
         xlab = "Month",
         ylab = metric,
         xaxt = "n",
         main = paste(metric, "Trend Over Time"))
    axis(1, at = df$Month, labels = month.name[months])
  })
  
  # Fraud in test data plot
  output$fraud_count_plot <- renderPlot({
    df <- metrics_data()
    df <- df[order(df$Month), ]
    
    barplot(
      height = df$n_Frauds * 10.1,
      names.arg = month.name[df$Month],
      col = "darkred",
      border = NA,
      main = "Trend Number of Frauds",
      ylab = "Count",
      las = 2
    )
  })
  
}



