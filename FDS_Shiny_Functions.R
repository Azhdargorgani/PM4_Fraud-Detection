
# 📌 Funktion zum Verschieben von Daten nach History
move_to_history <- function(history_path = "99_DATA/tx_history.rds",
                            pending_path = "99_DATA/pending_history.rds", 
                            move_count) {
  # 📌 Lade die Historie
  if (file.exists(pending_path)) {
    pending_data <- readRDS(pending_path)
  } else {
    warning("No Pending Transactions")
  }
  
  # 📌 Lade die Historie
  if (file.exists(history_path)) {
    history_data <- readRDS(history_path)
  } else {
    history_data <- data.frame()
  }
  
  # 📌 Extrahiere die ersten `num_to_move` Einträge
  entries_to_move <- pending_data[1:move_count, ]
  
  # 📌 Entferne die verschobenen Einträge aus der Pending-Tabelle
  pending_data <- pending_data[-(1:move_count), ]
  
  # 📌 Füge die verschobenen Einträge zur Historie hinzu und sortiere nach TX_DATE
  history_data <- rbind(history_data, entries_to_move)
  history_data <- history_data[order(history_data$TX_DATE, na.last = TRUE), ]
  
  # 📌 Speichere die aktualisierten Tabellen
  saveRDS(history_data, history_path)
  saveRDS(pending_data, pending_path)
  return(pending_data)
}