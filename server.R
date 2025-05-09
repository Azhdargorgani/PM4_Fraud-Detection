source("FDS_Retrain_Model.R", local = TRUE)
source("FDS_Model_Evaluation.R", local = TRUE)
source("FDS_predict_tx.R", local = TRUE)
source("FDS_DEMO_SIM.R", local = TRUE)
source("FDS_Shiny_Functions.R", local = TRUE)
source("FDS_live_model_metrics.R", local = TRUE)
source("credentials.R")
server <- function(input, output, session) {

  
  # 📌 Meta System Functions----------------------------------------------------

   #Secure login
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  observeEvent(res_auth$user, {
    updateTabItems(session, inputId = "tabs", selected = "dashboard")
  })
  output$sidebar_ui <- renderUI({
    tagList(
      sidebarMenu(
        id = "tabs",
        selected = "dashboard",
        menuItem("Live Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
        menuItem("Transactions Pending Review", tabName = "history_pending", icon = icon("search-dollar")),
        menuItem("Transaction History", tabName = "history", icon = icon("database")),
        if (res_auth$role == "admin") {
          menuItem("Model Information", tabName = "model_information", icon = icon("sync-alt"))
        }
      )
    )
  })
  
  
  #Monat anpassen wenn knopf in UI header gedrückt wird
  month_time <- update_month(input, output, session)

  
  # 📌 Reactive values and triggers
  model_update_trigger <- reactiveVal(Sys.time())
  history_update_trigger <- reactiveVal(Sys.time())
  pending_update_trigger <- reactiveVal(Sys.time())
  training_mode <- reactiveVal(NULL)
  model_info_live <- reactiveVal("Model Information: not available")
  model_result <- reactiveVal(NULL)
  
  # 📌 Helper: format numeric values
  format_df <- function(df, digits = 4) {
    df[] <- lapply(df, function(x) {
      if (is.numeric(x)) format(round(x, digits), nsmall = digits) else x
    })
    return(df)
  }
  # 📌 Model Information--------------------------------------------------------
  #Arch modelle aktualisieren
  observe({
    invalidateLater(2000, session)  # alle 2 Sekunden prüfen
    model_files <- list.files("80_MODELS", pattern = "^fraud_model_.*\\.rds$", full.names = FALSE)
    updateSelectInput(session, "archived_model_select", choices = model_files)
  })
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
    req(file.exists("80_MODELS/fraud_model.rds"),
        file.exists("99_DATA/test_data.rds"),
        file.exists("99_DATA/test_labels.rds"))
    
    metrics <- evaluate_model(
      "80_MODELS/fraud_model.rds",
      "99_DATA/test_data.rds",
      "99_DATA/test_labels.rds"
    )
    
    rownames(metrics) <- NULL  # <-- entfernt die automatische „1“
    format_df(metrics, 4)
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
    if (input$rf_ntree > 1000) {
      output$update_status <- renderText("⚠️ Maximum 1000 trees allowed.")
      return()
    }
    if (file.exists("80_MODELS/fraud_model.rds")) {
      output$update_status <- renderText("⚠️ Model already exists. Use Retrain instead.")
      return()
    }
    
    # 👉 Extract month range
    month_range <- sort(match(input$retrain_range, month.name))
    if (any(is.na(month_range)) || length(month_range) < 1) {
      showModal(modalDialog(
        title = "⚠️ Invalid training period",
        "Please select at least one valid month.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    start_month <- month_range[1]
    end_month <- month_range[length(month_range)]
    
    train_model(
      mode = "initial",
      ntree = input$rf_ntree,
      start_month = start_month,
      end_month = end_month
    )
    
    output$update_status <- renderText("✅ Initial model trained.")
  })
  
  observeEvent(input$activate_archived_model, {
    req(input$archived_model_select)
    src <- file.path("80_MODELS", input$archived_model_select)
    dest <- "80_MODELS/fraud_model.rds"
    
    if (file.exists(src)) {
      file.copy(from = src, to = dest, overwrite = TRUE)
      output$update_status <- renderText(paste("✅", input$archived_model_select, "is now the active model."))
      model_update_trigger(Sys.time())
    } else {
      output$update_status <- renderText("❌ Selected model file not found.")
    }
  })
    
      
  
  # 📌 Retraining
  observeEvent(input$retrain_model, {
    if (input$rf_ntree > 1000) {
      output$update_status <- renderText("⚠️ Maximum 1000 trees allowed.")
      return()
    }
    
    # 📌 Training period check
    month_range <- sort(match(input$retrain_range, month.name))
    if (any(is.na(month_range)) || length(month_range) < 1) {
      showModal(modalDialog(
        title = "⚠️ Invalid training period",
        "Please select at least one valid month.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    start_month <- month_range[1]
    end_month <- month_range[length(month_range)]
    
    # 📌 Modell trainieren
    result <- train_model(
      mode = "retrain",
      ntree = input$rf_ntree,
      start_month = start_month,
      end_month = end_month 
    )
    model_result(result)  # Ergebnis speichern
    
    # 📌 Box-Klasse setzen
    new_vals <- tryCatch(as.numeric(result$new_model[1:4, 1]), error = function(e) rep(NA, 4))
    old_vals <- tryCatch(as.numeric(result$old_model[1:4, 1]), error = function(e) rep(NA, 4))
    diffs <- new_vals - old_vals
    
    status_class <- if (any(is.na(diffs))) {
      "box-default"
    } else if (all(diffs > 0)) {
      "box-success"
    } else if (all(diffs <= 0)) {
      "box-danger"
    } else {
      "box-warning"
    }
    
    shinyjs::runjs("$('#box_new_model').removeClass('box-success box-danger box-warning box-default');")
    shinyjs::runjs(sprintf("$('#box_new_model').addClass('%s');", status_class))
    
    output$update_status <- renderText("✅ Retrained successfully.")
  })
  
    
 
  
  #aktiv modell metrik
  output$active_model_metrics <- renderTable({
    model_update_trigger()
    req(file.exists("80_MODELS/fraud_model.rds"))
    
    result <- evaluate_model(
      model_path = "80_MODELS/fraud_model.rds",
      test_data_path = "99_DATA/test_data.rds",
      test_labels_path = "99_DATA/test_labels.rds"
    )
    
    result$metrics  # Nur Metriken zurückgeben
  }, rownames = FALSE)
  
  #Neues model aktivieren
  observeEvent(input$activate_archived_model, {
    req(input$archived_model_select)
    src <- file.path("80_MODELS", input$archived_model_select)
    dest <- "80_MODELS/fraud_model.rds"
    
    if (file.exists(src)) {
      file.copy(from = src, to = dest, overwrite = TRUE)
      
      # 🟢 HIER: Modellnamen separat speichern
      writeLines(input$archived_model_select, "80_MODELS/active_model_name.txt")
      
      output$update_status <- renderText(paste("✅", input$archived_model_select, "is now the active model."))
      model_update_trigger(Sys.time())
    } else {
      output$update_status <- renderText("❌ Selected model file not found.")
    }
  })
  
  output$active_model_name <- renderText({
    model_update_trigger()  # 👉 sorgt für Reaktivität
    
    if (file.exists("80_MODELS/active_model_name.txt")) {
      paste("Active model:", readLines("80_MODELS/active_model_name.txt"))
    } else {
      "Active model: unknown"
    }
  })
  
  #Confusion Matrix
  output$active_model_confusion <- renderTable({
    model_update_trigger()
    req(file.exists("80_MODELS/fraud_model.rds"))
    
    result <- evaluate_model(
      model_path = "80_MODELS/fraud_model.rds",
      test_data_path = "99_DATA/test_data.rds",
      test_labels_path = "99_DATA/test_labels.rds"
    )
    
    as.data.frame(result$confusion)
  })
  
  
  
  
  # 📌 Archivierte model metriken
  output$archived_model_metrics <- renderTable({
    req(input$archived_model_select)
    path <- file.path("80_MODELS", input$archived_model_select)
    
    result <- evaluate_model(
      model_path = path,
      test_data_path = "99_DATA/test_data.rds",
      test_labels_path = "99_DATA/test_labels.rds"
    )
    
    format_df(result$metrics, 4)
  }, rownames = FALSE)
  
  
  output$archived_model_best_tune <- renderText({
    req(input$archived_model_select)
    path <- file.path("80_MODELS", input$archived_model_select)
    
    if (!file.exists(path)) return("Model file not found.")
    
    model <- readRDS(path)
    mtry_val <- tryCatch(model$bestTune$mtry, error = function(e) NA)
    ntree_val <- tryCatch(model$ntree %||% model$finalModel$ntree, error = function(e) NA)
    
    paste0("Model Information: mtry = ", mtry_val, " | ntree = ", ntree_val)
  })

  # 📌 #Live Metriken des Aktuellen Modells
    output$model_info_metrics_box <- renderTable({
    req(file.exists("70_Performance_Hist/metrics.rds"))
    
    df <- readRDS("70_Performance_Hist/metrics.rds")
    current_month <- month_time()
    row <- df[df$Month == current_month, ]
    
    if (nrow(row) == 0) return(data.frame(Message = "No metrics for current month."))
    
    metrics <- row[, c("Accuracy", "Precision", "Recall", "F1_Score"), drop = FALSE]
    format_df(metrics, 4)
  }, rownames = FALSE)
    
  #importance plot------
  output$feature_importance_plot <- renderPlot({
    model_update_trigger()  # automatisches Update bei Modelländerung
    
    req(file.exists("80_MODELS/fraud_model.rds"))
    
    model <- readRDS("80_MODELS/fraud_model.rds")
    
    # Extract importance from final model
    imp <- randomForest::importance(model$finalModel)
    imp_df <- data.frame(
      Feature = rownames(imp),
      Importance = imp[, "MeanDecreaseGini"]
    )
    
    
    ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "gray30") +
      coord_flip() +
      labs(
        title = "Feature Importance – Random Forest",
        x = "Feature",
        y = "Mean Decrease in Gini"
      ) +
      theme_minimal(base_size = 14)
  })
  
  #error evolution plot for number of trees
  output$rf_error_plot <- renderPlot({
    model_update_trigger()
    req(file.exists("80_MODELS/fraud_model.rds"))
    
    rf <- readRDS("80_MODELS/fraud_model.rds")$finalModel
    err <- rf$err.rate
    
    matplot(err, type = "l", lty = 1, col = c("black", "darkgreen", "red"),
            main = "Random Forest Error Over Trees", ylab = "Error", xlab = "Trees")
    legend("topright", legend = colnames(err), col = c("black", "darkgreen", "red"), lty = 1)
  })
  
  #ICE Plot------------
  # Dynamische Auswahlmöglichkeiten laden
  observe({
    req(file.exists("80_MODELS/fraud_model.rds"))
    model <- readRDS("80_MODELS/fraud_model.rds")
    
    # Entferne .outcome
    features <- setdiff(names(model$trainingData), ".outcome")
    updateSelectInput(session, "ice_variable", choices = features)
  })
  
  # ICE Plot rendern
  output$ice_plot <- renderPlot({
    req(input$ice_variable)
    req(file.exists("80_MODELS/fraud_model.rds"))
    
    model <- readRDS("80_MODELS/fraud_model.rds")
    train_data <- model$trainingData
    
    # Sicherstellen, dass .outcome entfernt ist
    if (".outcome" %in% names(train_data)) {
      train_data$Label <- train_data$.outcome
      train_data$.outcome <- NULL
    }
    
    # ICE berechnen
    ice_result <- pdp::partial(
      object = model,
      train = train_data,
      pred.var = input$ice_variable,
      which.class = "Fraud",   # <-- oder TRUE / 1 je nach Label
      prob = TRUE,
      ice = TRUE
    )
    
    plotPartial(ice_result, main = paste("ICE Plot for", input$ice_variable))
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
  
  #auswahl angezeigte reihen
  output$pending_col_selector <- renderUI({
    req(rv$pending_data)
    checkboxGroupInput(
      inputId = "pending_cols_to_show",
      label = "Select columns to display:",
      choices = names(rv$pending_data),
      selected = names(rv$pending_data),
      inline = TRUE
    )
  })
  output$transaction_table <- DT::renderDataTable({
    req(rv$pending_data)
    selected_cols <- input$pending_cols_to_show
    data <- rv$pending_data
    if (!is.null(selected_cols)) {
      data <- data[, selected_cols, drop = FALSE]
    }
    
    data[] <- lapply(data, function(x) if (is.numeric(x)) round(x, 2) else x)
    datatable(data,
              rownames = TRUE,
              editable = list(target = "cell", columns = which(names(data) == "ManualLabel")),
              options = list(scrollY = "400px", scrollX = "400px")
    )
  }, server = FALSE)

  
  # ---Capture and save table edits---
  observeEvent(input$transaction_table_cell_edit, {
    info <- input$transaction_table_cell_edit
    row <- info$row
    colname <- names(rv$pending_data)[info$col]
    new_value <- info$value
    
    if (colname == "ManualLabel" && new_value %in% c("Fraud", "Legit")) {
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
    data <- history_data()
    data[] <- lapply(data, function(x) if (is.numeric(x)) round(x, 2) else x)
    
    datatable(
      data,
      rownames = FALSE,
      options = list(
        scrollY = "540px",
        scrollX = "400px"
      )
    )
  })
  
  
  # 📌 Dashboard---------------------------------------------------------------
  #letztes modellupdate
  output$last_update_dashboard <- renderText({
    model_update_trigger()
    if (file.exists("80_MODELS/fraud_model.rds")) {
      paste("Last Update:", file.info("80_MODELS/fraud_model.rds")$mtime)
    } else {
      "No model trained yet."
    }
  })
  
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
    df <- fraud_data()
    
    # Prüfen, ob Daten existieren und Koordinatenspalten vorhanden sind
    if (is.null(df) || nrow(df) == 0 ||
        !"x_terminal_id" %in% names(df) ||
        !"y_terminal_id" %in% names(df)) {
      
      # Leere Karte mit Hinweis anzeigen
      leaflet() %>%
        addTiles() %>%
        addPopups(
          lng = 0, lat = 0,
          popup = "📭 No Fraud transaction data or coordinates available.",
          options = popupOptions(closeButton = FALSE)
        )
      
    } else {
      # Normale Karte mit Fraud-Punkten anzeigen
      leaflet(df) %>%
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
        ) %>%
        addFullscreenControl(position = "topright")
    }
  })
  
  
  # 📌 Display dashboard metrics
output$all_metrics_and_boxes <- renderUI({
  req(file.exists("70_Performance_Hist/metrics.rds"))
  req(input$metrics_to_show)

  metrics <- readRDS("70_Performance_Hist/metrics.rds")
  current_month <- month_time()
  row <- metrics[metrics$Month == current_month, ]

  if (nrow(row) == 0) return(tags$div("No metrics for this month."))

  selected <- input$metrics_to_show
  boxes <- list()

  if ("Accuracy" %in% selected) {
    boxes <- append(boxes, list(valueBox(paste0(round(row$Accuracy * 100, 2), "%"), "Accuracy", color = "blue")))
  }
  if ("Precision" %in% selected) {
    boxes <- append(boxes, list(valueBox(paste0(round(row$Precision * 100, 2), "%"), "Precision", color = "blue")))
  }
  if ("Recall" %in% selected) {
    boxes <- append(boxes, list(valueBox(paste0(round(row$Recall * 100, 2), "%"), "Recall", color = "blue")))
  }
  if ("F1_Score" %in% selected) {
    boxes <- append(boxes, list(valueBox(paste0(round(row$F1_Score * 100, 2), "%"), "F1 Score", color = "blue")))
  }

  fluidRow(boxes)
})

  
  
  # 📌 Transaction stats this month (simulated data, not training data)
#Initialisierung beider Tabellen beim Serverstart
rv <- reactiveValues(
  pending_data = {
    if (file.exists("99_DATA/pending_history.rds")) {
      readRDS("99_DATA/pending_history.rds")
    } else {
      data.frame()
    }
  },
  history_data = {
    if (file.exists("99_DATA/tx_history.rds")) {
      readRDS("99_DATA/tx_history.rds")
    } else {
      data.frame()
    }
  }
)

# Automatische Aktualisierung beider Tabellen
observe({
  invalidateLater(2000, session)
  
  if (file.exists("99_DATA/pending_history.rds")) {
    rv$pending_data <- readRDS("99_DATA/pending_history.rds")
  }
  
  if (file.exists("99_DATA/tx_history.rds")) {
    rv$history_data <- readRDS("99_DATA/tx_history.rds")
  }
})

# 📌 Kombinierte Daten aus Pending und History
combined_tx_data <- reactive({
  req(rv$pending_data, rv$history_data)
  all_data <- rbind(rv$pending_data, rv$history_data)
  
  selected_month <- month_time()
  all_data <- all_data[lubridate::month(all_data$TX_Date) == selected_month, ]
  
  return(all_data)
})


# 📌 Total Transactions
total_tx_count <- reactive({
  nrow(combined_tx_data())
})

# 📌 Fraud Rate
fraud_rate <- reactive({
  df <- combined_tx_data()
  if (nrow(df) == 0) return(NA)
  round(mean(df$ManualLabel == "Fraud", na.rm = TRUE) * 100, 2)
})

# 📌 Avg Fraud Amount (nur bei echten Fraud-Fällen)
avg_fraud_amount <- reactive({
  df <- combined_tx_data()
  frauds <- df[df$ManualLabel == "Fraud", ]
  if (nrow(frauds) == 0) return(NA)
  round(mean(frauds$TX_AMOUNT, na.rm = TRUE), 1)
})

# 📌 Manual interventions
manual_interventions <- reactive({
  df <- combined_tx_data()
  # Fälle, in denen manuell auf Fraud gesetzt wurde, obwohl das Modell "Legit" gesagt hat
  corrected_frauds <- df[df$ManualLabel == "Fraud" & df$Prediction != "Fraud", ]
  nrow(corrected_frauds)
})

# 📌 Detected Frauds (alle manuell markierten)
num_frauds <- reactive({
  sum(combined_tx_data()$ManualLabel == "Fraud", na.rm = TRUE)
})

output$transaction_stats_boxes <- renderUI({
    req(input$tx_stats_to_show)
    
    selected <- input$tx_stats_to_show
    boxes <- list()
    
    if ("total_tx" %in% selected) {
      boxes <- append(boxes, list(
        valueBox(total_tx_count(), "Total Transactions", color = "blue")
      ))
    }
    
    if ("manual_interventions" %in% selected) {
      boxes <- append(boxes, list(
        valueBox(manual_interventions(), "Manual Interventions", color = "blue")
      ))
    }
    
    if ("fraud_rate" %in% selected) {
      boxes <- append(boxes, list(
        valueBox(paste0(fraud_rate(), "%"), "Fraud Rate", color = "red")
      ))
    }
    
    if ("avg_fraud_amount" %in% selected) {
      boxes <- append(boxes, list(
        valueBox(paste0(avg_fraud_amount(), " CHF"), "Avg. Fraud Amount", color = "red")
      ))
    }
    
    if ("num_frauds" %in% selected) {
      boxes <- append(boxes, list(
        valueBox(num_frauds(), "Number of Frauds", color = "red")
      ))
    }
    
    fluidRow(boxes)
  })

  
  # 📌 Plot performance trends
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
    
    current_month <- month_time()
    df <- df[df$Month <= current_month, ] 
    
    metric <- input$selected_metric
    values <- as.numeric(df[[metric]])
    df$MonthLabel <- factor(month.name[df$Month], levels = month.name)
    
    # Dynamische y-Achse
    ymin <- min(values, na.rm = TRUE) - 0.05
    ymax <- max(values, na.rm = TRUE) + 0.05
    if (metric %in% c("Accuracy", "Precision", "Recall", "F1_Score", "AUC")) {
      ymin <- max(0.5, ymin)
      ymax <- min(1.0, ymax)
    }
    
    plot(df$Month, values,
         type = "o",
         col = "darkblue",
         lwd = 2,
         ylim = c(ymin, ymax),
         xlab = "Month",
         ylab = metric,
         xaxt = "n",
         main = paste(metric, "Trend Over Time"))
    axis(1, at = df$Month, labels = month.name[df$Month])
  })
  
  
  # 📌 Plot number of frauds in test data
  output$fraud_count_plot <- renderPlot({
    df <- metrics_data()
    df <- df[order(df$Month), ]
    
    current_month <- month_time()
    df <- df[df$Month <= current_month, ]
    
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


