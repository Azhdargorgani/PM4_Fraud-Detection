
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
  tx$TX_MINUTE <- minute(tx$TX_DATETIME)
  tx$TX_SECOND <- second(tx$TX_DATETIME)
  tx$TX_WEEKDAY <- wday(tx$TX_DATETIME, label = TRUE)
  
  tx<- subset(tx, select = c(-TX_DATETIME, -TX_TIME_SECONDS))
  tx$TX_ID <- 1:nrow(tx)
  
  #---Nr. TX last 24hrs---
  TX_count <- aggregate(TRANSACTION_ID ~ CUSTOMER_ID + TX_FRAUD, 
                        data = tx, FUN = length)
  # Rename the count column
  colnames(TX_count)[3] <- "tx_count_same_day"
  
  # Merge back to original dataset
  tx <- merge(tx, TX_count, 
              by = c("CUSTOMER_ID", "TX_DATE"), 
              all.x = TRUE)
  rm(TX_count)
  
  #___________________datenaufteilung (train/test)__________________
  demo_index <- (1:(nrow(tx)*0.1))
  train_index <- sample(1:nrow(tx[-c(demo_index),]), size = 0.8 * nrow(tx[-c(demo_index),]))  # 80% für Training
  test_index <- setdiff(1:nrow(tx), train_index)  #20 für Test
  
  
  
  train_data <- tx[train_index, ]   
  test_data  <- tx[test_index, ]    
  demo_data  <- tx[demo_index, ]    
  rm(train_index)
  rm(test_index)
  rm(demo_index)
  
  #Zielvariabeln von Testdata entfernen
  test_labels <- data.frame(TX_FRAUD = test_data$TX_FRAUD,
                            TX_FRAUD_SCENARIO = test_data$TX_FRAUD_SCENARIO)
  demo_labels <- data.frame(TX_FRAUD = demo_data$TX_FRAUD,
                            TX_ID = demo_data$TX_ID,
                            TX_FRAUD_SCENARIO = demo_data$TX_FRAUD_SCENARIO)
  test_data <- subset(test_data, select = -c(TX_FRAUD, TX_FRAUD_SCENARIO))
  demo_data <- subset(demo_data, select = -c(TX_FRAUD, TX_FRAUD_SCENARIO))
  
  #Training Daten für beide modelle vorbereiten
  train_data_Scenario <- train_data[train_data$TX_FRAUD_SCENARIO !=0,]
  train_data_Scenario <- subset(train_data_Scenario, select = -TX_FRAUD)
  
  train_data_Fraud <- subset(train_data, select = -TX_FRAUD_SCENARIO)
  
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
