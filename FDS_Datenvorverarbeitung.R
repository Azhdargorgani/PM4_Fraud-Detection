library(lubridate)
library(caret)
library(data.table)

  # ------------------- Read and preprocess data ------------------- #
  terminals <- read.table("terminals.csv", sep = ",", header = TRUE)
  terminals <- subset(terminals, select = -X)
  
  customers <- read.table("customers.csv", sep = ",", header = TRUE)
  customers <- subset(customers, select = -X)
  
  transactions <- read.table("transactions.csv", sep = ",", header = TRUE)
  transactions <- subset(transactions, select = -X)
  
  transactions$TX_FRAUD <- factor(transactions$TX_FRAUD, 
                                  levels = c(0, 1),
                                  labels = c("Legit", "Fraud"))
  transactions$TX_FRAUD_SCENARIO <- factor(transactions$TX_FRAUD_SCENARIO,
                                           levels = c(0, 1, 2, 3))
  transactions <- transactions[sample(1:nrow(transactions), 100000),]  # TEMP SAMPLE!!!!
  
  # Merge data frames into one
  tx <- merge(customers, transactions, 
              by = intersect(names(customers), names(transactions)), 
              all.y = TRUE, all.x = FALSE)
  tx <- merge(tx, terminals, 
              by = intersect(names(tx), names(terminals)), 
              all.x = TRUE, all.y = FALSE)
  
  # ------------------- Feature engineering ------------------- #
  tx$TX_TIME <- as.POSIXct(tx$TX_DATETIME, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")
  tx$TX_HOUR <- hour(tx$TX_DATETIME)
  tx$TX_WEEKDAY <- factor(lubridate::wday(tx$TX_DATETIME, label = TRUE), ordered = FALSE)
  tx$TX_Date <- as.Date(tx$TX_TIME)
  
  tx <- subset(tx, select = -c(TX_DATETIME, TX_TIME_SECONDS))
  tx$dist_cust_terminal <- sqrt((tx$x_customer_id - tx$x_terminal_id)^2 +
                                  (tx$y_customer_id - tx$y_terminal_id)^2)
  
  # --- Transactions count same day ---
  TX_count <- aggregate(TRANSACTION_ID ~ CUSTOMER_ID + TX_Date, 
                        data = tx, FUN = length)
  colnames(TX_count)[3] <- "tx_count_same_day"
  tx <- merge(tx, TX_count, 
              by = c("CUSTOMER_ID", "TX_Date"), 
              all.x = TRUE)
  rm(TX_count)
  
  # --- Transactions sum same day ---
  TX_sum <- aggregate(TX_AMOUNT ~ CUSTOMER_ID + TX_Date, 
                      data = tx, FUN = sum)
  colnames(TX_sum)[3] <- "tx_sum_same_day"
  tx <- merge(tx, TX_sum, 
              by = c("CUSTOMER_ID", "TX_Date"), 
              all.x = TRUE)
  rm(TX_sum)
  
  # --- Standardisierter Betrag ---
  tx$amount_z_score <- (tx$TX_AMOUNT - tx$mean_amount) / (tx$std_amount + 1e-6)
  
  # --- Abweichung zur erwarteten Aktivität ---
  tx$tx_count_diff <- tx$tx_count_same_day - tx$mean_nb_tx_per_day
  
  # ------------------- Erweiterte Features ------------------- #
  tx <- data.table(tx)
  setorder(tx, CUSTOMER_ID, TX_TIME)
  tx[, last_amount := shift(TX_AMOUNT, 1, type = "lag"), by = CUSTOMER_ID]
  tx[, amount_diff := TX_AMOUNT - last_amount]
  tx[, last3_mean := frollmean(TX_AMOUNT, n = 3, align = "right"), by = CUSTOMER_ID]

  
  # Wechsel des Terminals zur letzten Transaktion
  tx[, last_terminal := shift(TERMINAL_ID, 1, type = "lag"), by = CUSTOMER_ID]
  tx[, terminal_changed := as.integer(TERMINAL_ID != last_terminal)]
  tx$terminal_changed[is.na(tx$terminal_changed)] <- 0
  
  # Zeit seit letzter Transaktion in Sekunden
  tx[, last_time := shift(TX_TIME, 1, type = "lag"), by = CUSTOMER_ID]
  tx[, time_diff_secs := as.numeric(difftime(TX_TIME, last_time, units = "secs"))]
  tx$time_diff_secs[is.na(tx$time_diff_secs)] <- 999999
  
  # Anzahl verschiedener Terminals pro Kunde und Tag
  daily_terminals <- tx[, .(n_terminals = uniqueN(TERMINAL_ID)), by = .(CUSTOMER_ID, TX_Date)]
  tx <- merge(tx, daily_terminals, by = c("CUSTOMER_ID", "TX_Date"), all.x = TRUE)
  
  tx <- as.data.frame(tx)
  
  # ------------------- Datenaufteilung ------------------- #
  demo_index <- (1:(nrow(tx)*0.1))
  split_index <- createDataPartition(tx$TX_FRAUD, p = 0.8, list = FALSE)
  train_data <- tx[split_index, ]
  test_data  <- tx[-split_index, ]
  demo_data  <- tx[demo_index, ]    
  
  rm(split_index, demo_index)
  
  # --- Labels extrahieren ---
  test_labels <- data.frame(TX_FRAUD = test_data$TX_FRAUD,
                            TX_FRAUD_SCENARIO = test_data$TX_FRAUD_SCENARIO,
                            TRANSACTION_ID = test_data$TRANSACTION_ID)
  demo_labels <- data.frame(TX_FRAUD = demo_data$TX_FRAUD,
                            TRANSACTION_ID = demo_data$TRANSACTION_ID,
                            TX_FRAUD_SCENARIO = demo_data$TX_FRAUD_SCENARIO)
  
  test_data <- subset(test_data, select = -c(TX_FRAUD, TX_FRAUD_SCENARIO))
  demo_data <- subset(demo_data, select = -c(TX_FRAUD, TX_FRAUD_SCENARIO))
  
  # --- Finale Trainingsdaten für Fraud-Modell ---
  train_data_Fraud <- subset(train_data, select = c(
    TX_FRAUD, TERMINAL_ID, TX_AMOUNT, x_terminal_id, 
    y_terminal_id, TX_HOUR, TX_WEEKDAY, dist_cust_terminal, 
    tx_sum_same_day, amount_z_score, tx_count_diff, TX_Date, CUSTOMER_ID,
    last_amount, amount_diff, last3_mean,
    terminal_changed, time_diff_secs, n_terminals
  ))
  
  # ------------------- Speichern ------------------- #
  train_data_Fraud <- na.omit(train_data_Fraud)
  test_data <- na.omit(test_data)
  test_labels <- test_labels[test_labels$TRANSACTION_ID %in% test_data$TRANSACTION_ID, ]
  demo_data <- na.omit(demo_data)
  demo_labels <- demo_labels[demo_labels$TRANSACTION_ID %in% demo_data$TRANSACTION_ID, ]
  
  saveRDS(train_data_Fraud, file = "99_DATA/train_data_Fraud.rds")
  saveRDS(test_data, file = "99_DATA/test_data.rds")
  saveRDS(test_labels, file = "99_DATA/test_labels.rds")
  saveRDS(demo_data, file = "99_DATA/demo_data.rds")
  saveRDS(demo_labels, file = "99_DATA/demo_labels.rds")

