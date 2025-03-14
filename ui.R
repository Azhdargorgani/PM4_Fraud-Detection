
ui <- dashboardPage(
  dashboardHeader(title = "Fraud Detection System"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Live Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Transactions Pending Review", tabName = "history_pending", icon = icon("search-dollar")),
      menuItem("Transaction History", tabName = "history", icon = icon("database")),
      menuItem("Model Information", tabName = "model_information", icon = icon("sync-alt"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # 📌 Dashboard Tab
      tabItem(tabName = "dashboard",
              h2("Overview"),
              actionButton("sim_tx", "simulate_transaction")
      ),
      # 📌 Model Retraining Tab
      tabItem(tabName = "model_information",
              h2("Model Information"),
              actionButton("retrain_model", "Retrain Model with New Data", icon = icon("redo")),
              textOutput("update_status"),
              fluidRow(
                box(title = "Last Model Update", width = 6, status = "primary",
                    textOutput("last_update")),
                box(title = "Current Model Accuracy", width = 6, status = "success",
                    textOutput("model_accuracy")))
      ),
      
      # 📌 Data Historisation
      tabItem(tabName = "history",
              h2("Transaction History")
      
      ),
      # 📌 Data Pending History
      tabItem(tabName = "history_pending",
              h2("Transactions Pending Review"),
              fluidRow(
              actionButton("refresh_pend_history", "Update Table", icon = icon("sync-alt")),
              box(sliderInput("move_count", "Anzahl der Einträge verschieben:", min = 1, max = 50, value = 10),
              actionButton("move_to_history", "Verschiebe in Historie", icon = icon("arrow-right"))),
              box(
                title = "Pending Transactions", 
                status = "primary", 
                solidHeader = TRUE, 
                width = 12, 
                DTOutput("transaction_table")
              )
              )
              
      )
    )
  )
)
