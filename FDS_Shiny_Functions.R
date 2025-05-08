# 📌 Function to move data to history
move_to_history <- function(history_path = "99_DATA/tx_history.rds",
                            pending_path = "99_DATA/pending_history.rds", 
                            move_count) {
  
  # 📌 Load pending data
  if (file.exists(pending_path)) {
    pending_data <- readRDS(pending_path)
  } else {
    warning("No Pending Transactions")
    return(data.frame())
  }
  
  # 📌 Load transaction history
  if (file.exists(history_path)) {
    history_data <- readRDS(history_path)
  } else {
    history_data <- pending_data[0, ]
  }
  
  # 📌 Extract the first `num_to_move` entries
  pending_data <- pending_data[order(pending_data$TX_TIME, na.last = TRUE), ]
  entries_to_move <- pending_data[1:move_count, ]
  
  # 📌 Remove the moved entries from the pending table
  pending_data <- pending_data[-(1:move_count), ]
  
  # 📌 Append moved entries to history and sort by TX_DATE
  history_data <- rbind(history_data, entries_to_move)
  history_data <- history_data[order(history_data$TX_TIME, na.last = TRUE), ]
  history_data <- na.omit(history_data)
  
  # 📌 Save the updated tables
  saveRDS(history_data, history_path)
  saveRDS(pending_data, pending_path)
  return(pending_data)
}

update_month <- function(input, output, session) {
  month_time <- reactiveVal(5)
  
  output$month_sim <- renderUI({
    actionButton(
      inputId = "month_sim_button",
      label = paste(month.name[month_time()]),
      style = "font-size: 16px; background-color: transparent; 
               border: none; color: white; margin-top: 15px;"
    )
  })
  
  observeEvent(input$month_sim_button, {
    new_val <- month_time() + 1
    if (new_val > 12) new_val <- 5
    month_time(new_val)
    
    # nur ausführen, wenn alle Dateien existieren
    if (file.exists("99_DATA/test_data.rds") &&
        file.exists("99_DATA/test_labels.rds") &&
        file.exists("80_MODELS/fraud_model.rds")) {
      save_live_metrics(new_val)
    }
  })
  
  return(month_time)
}





