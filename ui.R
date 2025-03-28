library(shinydashboard)
library(randomForest)
library(dplyr)
library(DT)
library(shiny)
library(caret)
library(shinyjs)

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
    useShinyjs(),
    tabItems(
      
      # 📌 Dashboard Tab
      tabItem(tabName = "dashboard",
              h2("Overview"),
              actionButton("sim_tx", "simulate_transaction")
      ),
      # 📌 Model Retraining Tab
      tabItem(tabName = "model_information",
              h2("Model Information"),
              
              # 🧩 Model Training Control Panel
              box(title = "Model Training Settings", width = 12, status = "info", solidHeader = TRUE,
                  fluidRow(
                    column(width = 4,
                           numericInput("rf_ntree", "Number of Trees (Random Forest)", 
                                        value = 100, min = 10, max = 1000, step = 10)
                    ),
                    column(width = 4,
                           br(),  # bisschen Abstand nach oben
                           actionButton("retrain_model", "Retrain Model with New Data", icon = icon("redo"))
                    ),
                    column(width = 4,
                           br(),
                           actionButton("accept_new_model", "Accept New Model", icon = icon("check"))
                    )
                  )
              ),
              
              textOutput("update_status"),
              textOutput("last_update"),
              
              fluidRow(
                box(title = "Old Model Metrics", width = 6, status = "warning",
                    id = "box_old_model",
                    tableOutput("old_model_metrics")),
                box(title = "New Model Metrics", width = 6, status = "success",
                    id = "box_new_model",
                    tableOutput("new_model_metrics")),
                box(title = "Live Model Metrics", width = 12, status = "primary",
                    tableOutput("live_model_metrics"))
              )
      )
      
      ,
      
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
