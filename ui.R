
ui <- dashboardPage(
  dashboardHeader(title = "Fraud Detection System"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Live Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Model Information", tabName = "model_information", icon = icon("sync-alt")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")),
      menuItem("Transactions Pending Review", tabName = "history_pending", icon = icon("search-dollar")),
      menuItem("Transaction History", tabName = "history", icon = icon("database"))
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
              actionButton("accept_new_model", "accept new Model", icon = icon("check")),
              textOutput("update_status"),
              fluidRow(
                box(title = "Old Model Metrics", width = 6, status = "warning",
                    tableOutput("old_model_metrics")),
                box(title = "New Model Metrics", width = 6, status = "success",
                    tableOutput("new_model_metrics"))
              )
      )
      ,
      
      # 📌 Settings Tab
      tabItem(tabName = "settings",
              h2("Settings"),
              p("Additional configurations can be added here.")
      ),
      
      # 📌 Data Historisation
      tabItem(tabName = "history",
              h2("Transaction History")
      
      
      ),
      # 📌 Data Pending History
      tabItem(tabName = "history_pending",
              h2("Transactions Pending Review"),
              actionButton("refresh_pend_history", "Update Table", icon = icon("sync-alt")),
              DT::dataTableOutput("transaction_table")
              
      )
    )
  )
)
