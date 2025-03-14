
demo <- function(demo_data_path = "99_DATA/demo_data.rds") {
  demo_data <- readRDS(demo_data_path)
  tx_index <-sample(1:nrow(demo_data), size = sample(1:10, 1))
  tx_demo<- demo_data[tx_index,]
  demo_data <- demo_data[-c(tx_index),]
  saveRDS(demo_data, demo_data_path)
  
  return(tx_demo)
}