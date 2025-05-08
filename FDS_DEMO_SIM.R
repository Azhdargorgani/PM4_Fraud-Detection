
demo <- function(month_t, demo_data_path = "99_DATA/demo_data.rds") {
  demo_data <- readRDS(demo_data_path)
  demo_data <- readRDS(demo_data_path)
  month_rows <- which(month(demo_data$TX_TIME) == month_t)
  
  if (length(month_rows) == 0) return(NULL)
  selected_rows <- sample(month_rows, size = sample(1:min(length(month_rows), 100), 1))
  
  tx_demo <- demo_data[selected_rows, ]
  tx_demo <- tx_demo[order(tx_demo$TX_TIME), ]
  demo_data <- demo_data[-selected_rows, ]
  saveRDS(demo_data, demo_data_path)
  
  return(tx_demo)
}


