library(lubridate)
datenvorverarbeitung <- name <- function(variables) {
  #____________________Daten Vorverarbeiten__________________________
  # Daten Einlesen / erste vorverarbeitung
  terminals <- read.table("terminals.csv", sep = ",", header = T)
  terminals <- subset(terminals, select = -X)
  
  customers <- read.table("customers.csv", sep = ",", header = T)
  customers <- subset(customers, select = -X)
  
  transactions <- read.table("transactions.csv", sep = ",", header = T)
  transactions <- subset(transactions, select = -X)
  
  transactions$TX_FRAUD <- factor(transactions$TX_FRAUD, 
                                  levels = c(0,1),
                                  labels = c("No Fraud", "Fraud"))
  transactions$TX_FRAUD_SCENARIO <- factor(transactions$TX_FRAUD_SCENARIO,
                                           levels = c(0, 1, 2, 3))
  transactions <- transactions[sample(1:nrow(transactions), 30000),] #TEMP!!!!!!!!!!!!!!!!!!!!!
  
  #Merge data frames to one
  tx<-merge(customers, transactions, 
            by = intersect(names(customers), names(transactions)), 
            all.y = T, all.x = F)
  tx<-merge(tx, terminals, 
            by = intersect(names(tx), names(terminals)), 
            all.x = T, all.y = F)
  
  #___________________feature engineering___________________________
  # ---DATE TIME weekday...---
  library("lubridate")
  tx$TX_TIME <- as.POSIXct(tx$TX_DATETIME, format = "%Y-%m-%d %H:%M:%S")
  tx$TX_HOUR <- hour(tx$TX_DATETIME)
  #tx$TX_MINUTE <- minute(tx$TX_DATETIME)
  #tx$TX_SECOND <- second(tx$TX_DATETIME)
  tx$TX_WEEKDAY <- wday(tx$TX_DATETIME, label = TRUE)
  
  tx<- subset(tx, select = c(-TX_DATETIME, -TX_TIME_SECONDS))
  tx$dist_cust_terminal <- sqrt((tx$x_customer_id - tx$x_terminal_id)^2 +
                                  (tx$y_customer_id - tx$y_terminal_id)^2)
  
  #---Nr. TX last 24hrs---
  tx$TX_Date <- as.Date(tx$TX_TIME)
  TX_count <- aggregate(TRANSACTION_ID ~ CUSTOMER_ID + TX_Date, 
                        data = tx, FUN = length)
  # Rename the count column
  colnames(TX_count)[3] <- "tx_count_same_day"
  
  # Merge back to original dataset
  tx <- merge(tx, TX_count, 
              by = c("CUSTOMER_ID", "TX_Date"), 
              all.x = TRUE)
  rm(TX_count)
  
  #---Amount TX last 24hrs---
  TX_sum <- aggregate(TX_AMOUNT ~ CUSTOMER_ID + TX_Date, 
                      data = tx, FUN = sum)
  colnames(TX_sum)[3] <- "tx_sum_same_day"
  
  tx <- merge(tx, TX_sum, 
              by = c("CUSTOMER_ID", "TX_Date"), 
              all.x = TRUE)
  rm(TX_sum)
  tx<- subset(tx, select = -TX_Date)
  
  #-------------------standartisierte abweichung amount-----------------
  tx$amount_z_score <- (tx$TX_AMOUNT - tx$mean_amount) / (tx$std_amount + 1e-6)
  
  #Absolute difference (how much more or less active today?)
  tx$tx_count_diff <- tx$tx_count_same_day - tx$mean_nb_tx_per_day
  
  #___________________datenaufteilung (train/test)__________________
  demo_index <- (1:(nrow(tx)*0.1))
  
  split_index <- createDataPartition(tx$TX_FRAUD, p = 0.8, list = FALSE)
  train_data <- tx[split_index, ]
  test_data  <- tx[-split_index, ]
  demo_data  <- tx[demo_index, ]    
  
  rm(split_index)
  rm(demo_index)
  
  #Zielvariabeln von Testdata entfernen
  test_labels <- data.frame(TX_FRAUD = test_data$TX_FRAUD,
                            TX_FRAUD_SCENARIO = test_data$TX_FRAUD_SCENARIO)
  demo_labels <- data.frame(TX_FRAUD = demo_data$TX_FRAUD,
                            TX_ID = demo_data$TRANSACTION_ID,
                            TX_FRAUD_SCENARIO = demo_data$TX_FRAUD_SCENARIO)
  test_data <- subset(test_data, select = -c(TX_FRAUD, TX_FRAUD_SCENARIO))
  demo_data <- subset(demo_data, select = -c(TX_FRAUD, TX_FRAUD_SCENARIO))
  train_data_Fraud <- subset(train_data, select = c(TX_FRAUD, TERMINAL_ID, TX_AMOUNT, 
                                                    x_terminal_id, 
                                                    y_terminal_id, TX_HOUR, 
                                                    TX_WEEKDAY, dist_cust_terminal, 
                                                    tx_sum_same_day, amount_z_score))
  
  #___________________datenbalancierung (SMOTE)_____________________
  #vorerst nicht vorgesehen, eher bei random forest die wahr.
  #prozentsätze anpassen um die minderheitsklasse zu bevorzugen
  #evt noch die kosten FP, FN dabei beachten
  
  saveRDS(train_data_Scenario, file = "99_DATA/train_data_Scenario.rds")
  saveRDS(train_data_Fraud, file = "99_DATA/train_data_Fraud.rds")
  saveRDS(test_data, file = "99_DATA/test_data.rds")
  saveRDS(test_labels, file = "99_DATA/test_labels.rds")
  saveRDS(demo_data, file = "99_DATA/demo_data.rds")
  saveRDS(demo_labels, file = "99_DATA/demo_labels.rds")
  
}
datenvorverarbeitung()
