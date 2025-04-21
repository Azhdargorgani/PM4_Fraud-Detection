library(shinydashboard)
library(randomForest)
library(dplyr)
library(DT)
library(shiny)
library(caret)
library(shinyjs)
library(lubridate)
library(leaflet)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "FD-System",
                  tags$li(class = "dropdown",
                          uiOutput("month_sim")  # Knopf und Ausgabe
                  )
  ),
  
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
              actionButton("sim_tx", "simulate_transaction"),
              
              fluidRow(
                box(title = "Fraud Locations", solidHeader = F, width = 5,status = "primary",
                    leafletOutput("fraud_map", height = 350),
                    br(),
                    DTOutput("transaction_table")
                ),
              box(title = "Real Time Model Performance", width = 7, status = "primary",
                    uiOutput("dashboard_metrics_box")
                ),
              uiOutput("monthly_fraud_box"),
              
              #Liniendiagramme
              box(title = "Metric Trend Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  selectInput("selected_metric", "Select metric:",
                              choices = c("Accuracy", "Precision", "Recall", "F1_Score"),
                              selected = "Accuracy"),
                  plotOutput("metric_trend_plot", height = 300)
              ),
              
              #Trend diegramm anz frauds
              box(
                title = "Test Frauds per Month", 
                width = 12, 
                status = "danger", 
                solidHeader = TRUE,
                plotOutput("fraud_count_plot", height = 300)
              ),
              )
      ),
      
      # 📌 Model Retraining Tab
      tabItem(tabName = "model_information",
              h2("Model Information"),
              
              box(title = "Model Training Settings", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(width = 4,
                           numericInput("rf_ntree", "Number of Trees (Random Forest)", value = 100, min = 10, max = 200, step = 10)
                    ),
                    column(width = 4,
                           selectInput("retrain_start_month", "Start Month for Retraining (End month is fixed)", choices = NULL)
                    ),
                    column(width = 4,
                           br(),
                           actionButton("retrain_model", "Retrain Model with New Data", icon = icon("redo"))
                    ),
                    column(width = 4,
                           br(),
                           actionButton("accept_new_model", "Accept New Model", icon = icon("check"))
                    ),
                    column(width = 4,
                           br(),
                           actionButton("train_initial_model", "Train Initial Model", icon = icon("play"))
                    )
                  ),
                  br(),
                  textOutput("selected_month_label")
              ),
              
              textOutput("update_status"),
              textOutput("last_update"),
              
              fluidRow(
                box(title = "Old Model Test Metrics", width = 6, status = "warning", id = "box_old_model",
                    tableOutput("old_model_metrics"),
                    textOutput("old_model_best_tune")
                ),
                box(title = "New Model Test Metrics", width = 6, status = "success", id = "box_new_model",
                    tableOutput("new_model_metrics"),
                    textOutput("new_model_best_tune")
                ),
                box(title = "Live Model Test Metrics", width = 6, status = "primary", height = 270,
                    tableOutput("live_model_metrics"),
                    textOutput("live_model_best_tune")
                ),
                box(title = "Real Time Model Performance", width = 6, status = "primary", height = 270,
                    tableOutput("model_info_metrics_box")
                )
              )
      ),
      
      # 📌 Data Historisation
      tabItem(tabName = "history",
              h2("Transaction History"),
              fluidRow(
                box(title = "Full Transaction History", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("tx_history_table")
                )
              )
      ),
      
      # 📌 Data Pending History
      tabItem(tabName = "history_pending",
              h2("Transactions Pending Review"),
              fluidRow(
                box(sliderInput("move_count", "Anzahl der Einträge verschieben:", min = 1, max = 50, value = 10),
                    actionButton("move_to_history", "Verschiebe in Historie", icon = icon("arrow-right"))
                ),
                box(title = "Pending Transactions", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("transaction_table")
                )
              )
      )
    )
  )
)
