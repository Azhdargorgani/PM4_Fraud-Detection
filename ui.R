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
library(ggplot2)

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
    tags$head(
      tags$style(HTML("
      .main-sidebar {
        position: fixed;
        overflow: visible;
      }

      .main-header {
        position: fixed;
        width: 100%;
        z-index: 999;
      }

      .content-wrapper, .right-side {
        margin-left: 230px;
        margin-top: 50px;
        z-index: 1;
        position: relative;
      }

      .main-header .logo, .main-header .navbar {
        height: 50px;
      }

      /* Leaflet map fix: verhindert Überlappung über Header */
      .leaflet {
        z-index: 0 !important;
      }
    "))
    ),
    tabItems(
      
      # 📌 Dashboard Tab
      tabItem(tabName = "dashboard",
              h2("Overview"),
              actionButton("sim_tx", "Simulate Transaction"),
              
              fluidRow(
                box(title = "Fraud Locations", solidHeader = FALSE, width = 5, status = "primary",
                    leafletOutput("fraud_map", height = 450),
                    br(),
                    DTOutput("transaction_table")
                ),
                uiOutput("monthly_fraud_box"),
                
                box(
                  title = tagList(
                    span("Real Time Model Performance"),
                    actionButton("toggle_metric_settings", label = NULL, icon = icon("gear"),
                                 class = "btn btn-default btn-sm pull-right")
                  ),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  # ⬇️ Einstellungen
                  conditionalPanel(
                    condition = "input.toggle_metric_settings % 2 == 1",
                    checkboxGroupInput(
                      inputId = "metrics_to_show",
                      label = "Select metrics to display:",
                      choices = c("Accuracy", "Precision", "Recall", "F1_Score", "AUC", "LogLoss"),
                      selected = c("Accuracy", "Precision", "Recall", "F1_Score"),
                      inline = TRUE
                    )
                  ),
                  
                  uiOutput("all_metrics_and_boxes")
                ),
                

                #Liniendiagramme
                box(title = "Metric Trend Over Time", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("selected_metric", "Select Metric:",
                                choices = c("Accuracy", "Precision", "Recall", "F1_Score", "AUC", "LogLoss"),
                                selected = "Accuracy"),
                    plotOutput("metric_trend_plot", height = 300)
                ),
                
                #Trend diegramm anz frauds
                box(
                  title = "Trend: Frauds per Month", 
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
              
              # 🧩 Model Training Control Panel
              box(title = "Model Training Settings", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(width = 4,
                           numericInput("rf_ntree", "Number of Trees (Random Forest)", 
                                        value = 100, min = 10, max = 1000, step = 10)
                    ),
                    column(width = 4,
                           # 📌 Monatlicher Zeitraum für Retraining (als Monatsnamen)
                           sliderTextInput(
                             inputId = "retrain_range",
                             label = "Retraining Period",
                             choices = month.name,
                             selected = month.name[1:2],
                             width = "100%",
                             grid = TRUE 
                           )
                    ),
                    column(width = 4,
                           br(),
                           actionButton("retrain_model", "Retrain Model with New Data")
                    ),
                    column(width = 4,
                           br(),
                           actionButton("accept_new_model", "Accept New Model")
                    ),
                    column(width = 4,
                           br(),
                           actionButton("train_initial_model", "Train Initial Model")
                    )
                  ),
                  br(),
                  textOutput("selected_month_label")
              ),
              
              textOutput("update_status"),
              textOutput("last_update"),
              
              fluidRow(
                box(title = "Old Model Test Metrics", width = 6, status = "warning", id = "box_old_model",
                    htmlOutput("old_model_metrics"),
                    textOutput("old_model_best_tune")
                ),
                box(title = "New Model Test Metrics", width = 6, status = "success", id = "box_new_model",
                    htmlOutput("new_model_metrics"),
                    textOutput("new_model_best_tune")
                ),
                box(title = "Live Model Test Metrics", width = 6, status = "primary", height = 200,
                    tableOutput("live_model_metrics"),
                    textOutput("live_model_best_tune")
                ),
                box(title = "Real Time Model Performance", width = 6, status = "primary", height = 200,
                    tableOutput("model_info_metrics_box")
                ),
                box(
                  title = "Feature Importance – Random Forest",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("feature_importance_plot", height = 400)
                ),
                box(
                  title = "Random Forest Error Over Trees",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("rf_error_plot", height = 400)
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
                box(sliderInput("move_count", "Number of entries to move:", min = 1, max = 50, value = 10),
                    actionButton("move_to_history", "Approve Transactions", icon = icon("arrow-right"))
                ),
                box(title = "Pending Transactions", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("transaction_table")
                )
              )
      )
    )
  )
)
