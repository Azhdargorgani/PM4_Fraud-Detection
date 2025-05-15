library(shinydashboard)
library(randomForest)
library(dplyr)
library(DT)
library(shiny)
library(caret)
library(shinyjs)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(ggplot2)
library(shinymanager)
library(pdp)

source("credentials.R")

ui <- secure_app(
  dashboardPage(
    dashboardHeader(title = "FD-System",
                    tags$li(class = "dropdown",
                            uiOutput("month_sim")
                    )
    ),
    dashboardSidebar(
      uiOutput("sidebar_ui")
    ),
    
    dashboardBody(
      useShinyjs(),
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
            padding-right: 70px; /* 👈 genug Platz für Logout oben rechts */
          }
        
          .main-header .logo, .main-header .navbar {
            height: 50px;
          }
        
          .btn.pull-right {
            margin-right: 60px !important; /* 👈 Button nach links schieben */
          }
        
          .leaflet {
            z-index: 0 !important;
          }
        ")),
      tabItems(
        
        # 📌 Dashboard Tab
        tabItem(tabName = "dashboard",
                h2("Overview"),
                actionButton("sim_tx", "Simulate Transaction"),
                
                fluidRow(
                  box(title = "Fraud Locations", solidHeader = FALSE, width = 5, status = "primary",
                      leafletOutput("fraud_map", height = "350px"),
                      br()
                  ),
                  box(
                    title = tagList(
                      span("Daily Transaction Statistics"),
                      actionButton("toggle_tx_settings", label = NULL, icon = icon("gear"),
                                   class = "btn btn-default btn-sm pull-right",
                                   style = "margin-left: 10px;")
                    ),
                    width = 7,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    # ⬇Einstellungen
                    conditionalPanel(
                      condition = "input.toggle_tx_settings % 2 == 1",
                      checkboxGroupInput(
                        inputId = "tx_stats_to_show",
                        label = "Select transaction stats to display:",
                        choices = c(
                          "Total Transactions" = "total_tx",
                          "Fraud Rate" = "fraud_rate",
                          "Avg. Fraud Amount" = "avg_fraud_amount",
                          "Manual Interventions" = "manual_interventions",
                          "Number of Frauds" = "num_frauds"
                        ),
                        selected = c("total_tx", "fraud_rate", "avg_fraud_amount", "manual_interventions", "num_frauds"),  # <-- angepasst
                        inline = TRUE
                      )
                    ),
                    uiOutput("transaction_stats_boxes")
                  ),
                  
                  box(
                    title = tagList(
                      span("Real Time Model Performance"),
                      actionButton("toggle_metric_settings", label = NULL, icon = icon("gear"),
                                   class = "btn btn-default btn-sm pull-right",
                                   style = "margin-left: 10px;")
                    ),
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                  
                    # ⬇Einstellungen
                    conditionalPanel(
                      condition = "input.toggle_metric_settings % 2 == 1",
                      checkboxGroupInput(
                        inputId = "metrics_to_show",
                        label = "Select metrics to display:",
                        choices = c("Accuracy", "Precision", "Recall", "F1_Score"),
                        selected = c("Accuracy", "Precision", "Recall", "F1_Score"),
                        inline = TRUE
                      )
                    ),
                    div(textOutput("last_update_dashboard")),
                    uiOutput("all_metrics_and_boxes")
                  ),
                  
  
                  #Liniendiagramme
                  box(title = "Metric: Trend Over Time", width = 12, status = "primary", solidHeader = TRUE,
                      selectInput("selected_metric", "Select Metric:",
                                  choices = c("Accuracy", "Precision", "Recall", "F1_Score", "AUC", "LogLoss"),
                                  selected = "Accuracy"),
                      plotOutput("metric_trend_plot", height = 300),
                      collapsible = TRUE,
                      collapsed = TRUE
                  ),
                  
                  #Trend diegramm anz frauds
                  box(
                    title = "Trend: Frauds per Month", 
                    width = 12, 
                    status = "danger", 
                    solidHeader = TRUE,
                    plotOutput("fraud_count_plot", height = 300),
                    collapsible = TRUE,
                    collapsed = TRUE
                  ),
                )
        ),
        
        # 📌 Model Retraining Tab
        tabItem(tabName = "model_information",
                h2("Model Information"),
                
                # 🧩 Model Training Control Panel
                box(title = "Model Training", width = 12, status = "primary", solidHeader = TRUE,
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
                    ),
                    br(),
                    textOutput("selected_month_label")
                ),
                
                textOutput("update_status"),
                textOutput("last_update"),
                
                fluidRow(
                  box(
                    title = "Model Selection & Evaluation",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    
                    fluidRow(
                      column(6,
                             h4("📦 Archived Model"),
                             selectInput("archived_model_select", "Choose archived model:",
                                         choices = list.files("80_MODELS", pattern = "^fraud_model_.*\\.rds$")),
                             tableOutput("archived_model_metrics"),
                             textOutput("archived_model_best_tune"),
                             actionButton("activate_archived_model", "Set as Active Model")
                      ),
                      column(6,
                             h4("✅ Active Model"),
                             textOutput("active_model_name"),
                             tableOutput("active_model_metrics"),
                             h5(tags$strong("Confusion Matrix")),
                             tableOutput("active_model_confusion"),
                             textOutput("live_model_best_tune")
                      )
                    )
                  ),
                  # box(title = "Real Time Model Performance", width = 6, status = "primary", height = 200,
                  #     tableOutput("model_info_metrics_box")
                  # ),
                  box(
                    title = "Feature Importance",
                    width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("feature_importance_plot", height = 400),
                    collapsible = TRUE,
                    collapsed = TRUE
                  ),
                  box(
                    title = "Error Over Trees",
                    width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("rf_error_plot", height = 400),
                    collapsible = TRUE,
                    collapsed = TRUE
      
                  ),
                  # box(
                  #   title = "ICE Plot (Individual Conditional Expectation)",
                  #   width = 12,
                  #   status = "primary",
                  #   solidHeader = TRUE,
                  #   collapsible = TRUE,
                  #   collapsed = TRUE,
                  #   # selectInput("ice_variable", "Select variable for ICE plot:",
                  #   #             choices = NULL,  # wird dynamisch gefüllt
                  #   #             selected = NULL),
                  #   #plotOutput("ice_plot", height = 400)
                  # )
                  
                  
        
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
                  box( 
                    numericInput("move_count", "Number of entries to approve:", value = 10, min = 1, max = 50),
                    actionButton("move_to_history", "Approve Transactions", icon = icon("arrow-right"))
                  ),
                  fluidRow(
                    box(
                      title = tagList(
                        span("Pending Transactions"),
                        actionButton("toggle_pending_cols", label = NULL, icon = icon("gear"),
                                     class = "btn btn-default btn-sm pull-right",
                                     style = "margin-left: 10px;")
                      ),
                      width = 12,
                      status = "primary",
                      solidHeader = TRUE,
                      # 👉 Auswahl der Spalten (nur sichtbar wenn gear-Button geklickt)
                      conditionalPanel(
                        condition = "input.toggle_pending_cols % 2 == 1",
                        uiOutput("pending_col_selector")
                      ),
                      
                      # 👉 Tabelle selbst
                      DTOutput("transaction_table")
                    ),
                    style = "margin: 10px;"
                  )
                )
        )
      )
    )
  )
)