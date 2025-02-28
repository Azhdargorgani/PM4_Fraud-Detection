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
transactions <- transactions[sample(1:nrow(transactions), 30000),] #TEMP!!!!!

#Merge data frames to one
tx<-merge(customers, transactions, 
           by = intersect(names(customers), names(transactions)), 
           all.y = T, all.x = F)
tx<-merge(tx, terminals, 
           by = intersect(names(tx), names(terminals)), 
           all.x = T, all.y = F)



#___________________"Explorative Datenanalyse" temp_______________
cor(tx[,sapply(tx, is.numeric)])

plot(tx$TX_FRAUD)
plot(tx$TX_FRAUD_SCENARIO[tx$TX_FRAUD_SCENARIO != 0])
plot(tx$TX_AMOUNT, 
     col = tx$TX_FRAUD, pch = 20)
#pca
pca_dataset = subset(tx, select = c(-TX_DATETIME, -TX_FRAUD_SCENARIO, -TX_FRAUD))
library("rrcov")
pca_rob<-PcaHubert(pca_dataset, scale=T)
outlier_rows <- which(pca_rob@od >= pca_rob@cutoff.od)
plot(pca_rob@scores[,1],pca_rob@scores[,2], pch = 20, col=tx$TX_FRAUD)

rm(c(pca_rob, outlier_rows))

#___________________feature engineering___________________________
# DATE TIME weekday...
library("lubridate")
tx$TX_DATE <- as.Date(tx$TX_DATETIME)
tx$TX_HOUR <- hour(tx$TX_DATETIME)
tx$TX_MINUTE <- minute(tx$TX_DATETIME)
tx$TX_SECOND <- second(tx$TX_DATETIME)
tx$TX_WEEKDAY <- wday(tx$TX_DATETIME, label = TRUE)

tx<- subset(tx, select = c(-TX_DATETIME))

#Nr. TX last 24hrs


#___________________datenaufteilung (train/test)__________________


#___________________datenbalancierung (SMOTE)_____________________
#vorerst nicht vorgesehen, eher bei random forest die wahr.
#prozentsätze anpassen um die minderheitsklasse zu bevorzugen
#evt noch die kosten FP, FN dabei beachten

#___________________Modeltraining__________________________________

#---
#___________________Modellbewertung________________________________

#___________________RShiney UI_____________________________________