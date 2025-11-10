# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tseries)
library(forecast)
library(ggplot2)
library(zoo)
library(reshape2)
library(car)
library(vars)
library(MTS)
library(gridExtra)
library(plotly)
library(lubridate)

# Define UI (keep the same UI as before)
ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data", icon = icon("upload")),
      menuItem("Model Configuration", tabName = "config", icon = icon("cogs")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Forecasts", tabName = "forecasts", icon = icon("forward"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Fixed header */
        .main-header {
          position: fixed !important;
          width: 100%;
          z-index: 1000;
        }
        
        /* Fixed sidebar */
        .main-sidebar {
          position: fixed !important;
          height: 100vh;
          overflow-y: auto;
          top: 0px; /* Below header */
        }
        
        /* Adjust content wrapper for fixed header + sidebar */
        .content-wrapper {
          margin-left: 230px;
          margin-top: 50px; /* Below header */
        }
        
        /* Scrollbar styling for sidebar */
        .main-sidebar::-webkit-scrollbar {
          width: 8px;
        }
        
        .main-sidebar::-webkit-scrollbar-track {
          background: #2c3b41;
        }
        
        .main-sidebar::-webkit-scrollbar-thumb {
          background: #1a252a;
          border-radius: 4px;
        }
      "))
    ),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .progress-text { color: #333; font-weight: bold; }
        .code-output {
          background-color: #f8f8f8;
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 10px;
          font-family: 'Courier New', monospace;
          font-size: 12px;
        }
      "))
    ),
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Upload Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fileInput("dataFile", "Choose CSV File",
                            accept = c("text/csv", ".csv")),
                  
                  checkboxInput("header", "Header", TRUE),
                  
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                               selected = ","),
                  
                  actionButton("loadData", "Load Data", class = "btn-primary"),
                  
                  hr(),
                  
                  h4("Data Configuration"),
                  
                  conditionalPanel(
                    condition = "output.dataUploaded",
                    
                    selectInput("targetColumn", "Select Target Variable:", choices = NULL),
                    
                    dateInput("startDate", "Start Date:", value = "2015-01-01"),
                    
                    selectInput("frequency", "Data Frequency:",
                                choices = list("Monthly" = 12, "Quarterly" = 4, "Daily" = 365, "Weekly" = 52),
                                selected = 12),
                    
                    h4("Exogenous Variables (for ARIMAX/SARIMAX)"),
                    
                    checkboxInput("useExogenous", "Use Exogenous Variables", FALSE),
                    
                    conditionalPanel(
                      condition = "input.useExogenous",
                      selectInput("tempColumn", "Temperature Column:", choices = NULL),
                      selectInput("rainColumn", "Rainfall Column:", choices = NULL),
                      selectInput("humColumn", "Humidity Column:", choices = NULL),
                      
                      h5("Lag Configuration"),
                      numericInput("tempLag", "Temperature Lag:", value = 4, min = 0, max = 12),
                      numericInput("rainLag", "Rainfall Lag:", value = 2, min = 0, max = 12),
                      numericInput("humLag", "Humidity Lag:", value = 1, min = 0, max = 12)
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Preview",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("dataPreview")
                )
              )
      ),
      
      # Model Configuration Tab
      tabItem(tabName = "config",
              fluidRow(
                box(
                  title = "Model Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  
                  checkboxGroupInput("models", "Select Models to Run:",
                                     choices = list(
                                       "ARIMA" = "arima",
                                       "SARIMA" = "sarima",
                                       "ARIMAX" = "arimax",
                                       "SARIMAX" = "sarimax"
                                     ),
                                     selected = "arima"
                  ),
                  
                  hr(),
                  
                  h4("Data Transformation"),
                  checkboxInput("logTransform", "Apply Log Transformation", TRUE),
                  checkboxInput("differencing", "Check for Differencing", TRUE)
                ),
                
                box(
                  title = "Model Parameters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  
                  h4("Train-Test Split"),
                  sliderInput("trainProportion", "Training Data Proportion:",
                              min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                  
                  h4("Forecast Horizon"),
                  numericInput("forecastHorizon", "Months to Forecast:",
                               value = 24, min = 1, max = 60),
                  
                  h4("Model Search Parameters"),
                  h5("Non-Seasonal Orders"),
                  sliderInput("pRange", "p range:", min = 0, max = 5, value = c(0, 2), step = 1),
                  sliderInput("dRange", "d range:", min = 0, max = 2, value = c(0, 1), step = 1),
                  sliderInput("qRange", "q range:", min = 0, max = 5, value = c(0, 2), step = 1),
                  
                  conditionalPanel(
                    condition = "input.models.includes('sarima') || input.models.includes('sarimax')",
                    h5("Seasonal Orders"),
                    sliderInput("PRange", "P range:", min = 0, max = 2, value = c(0, 2), step = 1),
                    sliderInput("DRange", "D range:", min = 0, max = 2, value = c(0, 1), step = 1),
                    sliderInput("QRange", "Q range:", min = 0, max = 2, value = c(0, 2), step = 1)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Run Analysis",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  
                  actionButton("runAnalysis", "Run Selected Models", 
                               class = "btn-success btn-lg", icon = icon("play")),
                  
                  hr(),
                  
                  textOutput("analysisStatus")
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Data Exploration",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  tabsetPanel(
                    tabPanel("Time Series Plot", plotlyOutput("tsPlot", height = "400px")),
                    tabPanel("Decomposition", plotOutput("decomposePlot", height = "500px")),
                    tabPanel("ACF/PACF", plotOutput("acfPlot", height = "400px")),
                    tabPanel("Stationarity Tests", verbatimTextOutput("stationarityTests"))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Model Diagnostics",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  
                  selectInput("selectedModel", "Select Model:", choices = NULL),
                  
                  tabsetPanel(
                    tabPanel("Residual Plots", plotOutput("residualPlots", height = "500px")),
                    tabPanel("Model Summary", verbatimTextOutput("modelSummary")),
                    tabPanel("Ljung-Box Test", verbatimTextOutput("ljungBoxTest"))
                  )
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = "Model Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  DT::dataTableOutput("modelComparison"),
                  
                  hr(),
                  
                  plotlyOutput("metricsPlot", height = "400px")
                )
              )
              
              #fluidRow(
               # box(
                #  title = "Best Models",
                 # status = "success",
                #  solidHeader = TRUE,
                #  width = 12,
                  
                #  DT::dataTableOutput("bestModels")
                #)
              #)
      ),
      
      # Forecasts Tab
      tabItem(tabName = "forecasts",
              fluidRow(
                box(
                  title = "Forecast Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  selectInput("forecastModel", "Select Model for Forecast:", choices = NULL),
                  
                  plotlyOutput("forecastPlot", height = "500px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Forecast Table",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  DT::dataTableOutput("forecastTable"),
                  
                  hr(),
                  
                  downloadButton("downloadForecast", "Download Forecast", class = "btn-primary")
                )
              )
      )
    )
  )
)

# Define Server with improved error handling
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    ts_data = NULL,
    models_results = list(),
    forecasts = list(),
    dataUploaded = FALSE
  )
  
  # Custom theme for plots
  custom_theme <- theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
    panel.background = element_rect(fill = "white", colour = "black", size = 0.2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
  
  # Data upload handler
  observeEvent(input$loadData, {
    req(input$dataFile)
    
    tryCatch({
      df <- read.csv(input$dataFile$datapath,
                     header = input$header,
                     sep = input$sep,
                     stringsAsFactors = FALSE)  # Ensure strings are not converted to factors
      
      values$data <- df
      values$dataUploaded <- TRUE
      
      # Get numeric columns only
      numeric_cols <- names(df)[sapply(df, function(x) is.numeric(x) || all(!is.na(as.numeric(as.character(x[!is.na(x)])))))]
      
      if (length(numeric_cols) == 0) {
        showNotification("No numeric columns found in the data!", type = "error", duration = 5)
        return()
      }
      
      # Update column selections with numeric columns only
      updateSelectInput(session, "targetColumn", 
                        choices = numeric_cols)
      updateSelectInput(session, "tempColumn", 
                        choices = c("None", numeric_cols))
      updateSelectInput(session, "rainColumn", 
                        choices = c("None", numeric_cols))
      updateSelectInput(session, "humColumn", 
                        choices = c("None", numeric_cols))
      
      showNotification("Data loaded successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 5)
    })
  })
  
  # Data preview
  output$dataPreview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$dataUploaded <- reactive({
    values$dataUploaded
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  # Run analysis
  observeEvent(input$runAnalysis, {
    req(values$data, input$targetColumn)
    
    withProgress(message = 'Running analysis...', value = 0, {
      
      tryCatch({
        # Prepare time series - with better type checking
        target_data <- values$data[[input$targetColumn]]
        
        # Convert to numeric if it's not already
        if (!is.numeric(target_data)) {
          target_data <- as.numeric(as.character(target_data))
        }
        
        # Remove NA values
        target_data <- na.omit(target_data)
        
        # Check if we have any data left
        if (length(target_data) == 0) {
          showNotification("No valid numeric data in selected column!", type = "error", duration = 5)
          return()
        }
        
        # Check for non-positive values before log transformation
        if (input$logTransform) {
          if (any(target_data <= 0, na.rm = TRUE)) {
            # Add constant to make all values positive
            min_val <- min(target_data, na.rm = TRUE)
            target_data <- target_data - min_val + 1
            showNotification("Data contained non-positive values. Added constant for log transformation.", 
                             type = "warning", duration = 5)
          }
          target_data <- log(target_data)
        }
        
        # Check for infinite or NaN values
        if (any(!is.finite(target_data))) {
          target_data <- target_data[is.finite(target_data)]
          showNotification("Removed infinite or NaN values from data.", type = "warning", duration = 5)
        }
        
        # Create time series
        ts_data <- ts(target_data, 
                      start = c(year(input$startDate), month(input$startDate)),
                      frequency = as.numeric(input$frequency))
        
        values$ts_data <- ts_data
        
        # Check if we have enough data
        if (length(ts_data) < 20) {
          showNotification("Warning: Very short time series. Results may be unreliable.", 
                           type = "warning", duration = 5)
        }
        
        # Train-test split
        n <- length(ts_data)
        train_size <- floor(input$trainProportion * n)
        
        if (train_size < 10) {
          showNotification("Training set too small. Please use more data or increase training proportion.", 
                           type = "error", duration = 5)
          return()
        }
        
        train_ts <- window(ts_data, end = time(ts_data)[train_size])
        test_ts <- window(ts_data, start = time(ts_data)[train_size + 1])
        
        # Prepare exogenous variables if needed
        exog_train <- NULL
        exog_test <- NULL
        exog_full <- NULL
        
        if (input$useExogenous && ("arimax" %in% input$models || "sarimax" %in% input$models)) {
          if (input$tempColumn != "None" && input$rainColumn != "None" && input$humColumn != "None") {
            # Prepare lagged exogenous variables
            temp_data <- values$data[[input$tempColumn]]
            rain_data <- values$data[[input$rainColumn]]
            hum_data <- values$data[[input$humColumn]]
            
            # Convert to numeric
            if (!is.numeric(temp_data)) temp_data <- as.numeric(as.character(temp_data))
            if (!is.numeric(rain_data)) rain_data <- as.numeric(as.character(rain_data))
            if (!is.numeric(hum_data)) hum_data <- as.numeric(as.character(hum_data))
            
            # Remove NA values
            temp_data[is.na(temp_data)] <- mean(temp_data, na.rm = TRUE)
            rain_data[is.na(rain_data)] <- mean(rain_data, na.rm = TRUE)
            hum_data[is.na(hum_data)] <- mean(hum_data, na.rm = TRUE)
            
            # Create lagged variables
            max_lag <- max(input$tempLag, input$rainLag, input$humLag)
            
            if (max_lag >= length(temp_data)) {
              showNotification("Lag values too large for data size!", type = "error", duration = 5)
              return()
            }
            
            valid_start <- max_lag + 1
            valid_end <- min(length(temp_data), length(target_data))
            
            # Apply lags
            temp_lagged <- c(rep(NA, input$tempLag), temp_data[1:(valid_end - input$tempLag)])
            rain_lagged <- c(rep(NA, input$rainLag), rain_data[1:(valid_end - input$rainLag)])
            hum_lagged <- c(rep(NA, input$humLag), hum_data[1:(valid_end - input$humLag)])
            
            # Combine into matrix
            exog_full <- cbind(
              Temp = temp_lagged[valid_start:valid_end],
              Rain = rain_lagged[valid_start:valid_end],
              Hum = hum_lagged[valid_start:valid_end]
            )
            
            # Adjust time series data to match exogenous variables
            ts_data_adj <- ts(target_data[valid_start:valid_end],
                              start = c(year(input$startDate), month(input$startDate)),
                              frequency = as.numeric(input$frequency))
            
            # Recalculate train-test split
            n_adj <- length(ts_data_adj)
            train_size_adj <- floor(input$trainProportion * n_adj)
            
            if (train_size_adj < 10) {
              showNotification("After adjusting for lags, training set too small.", 
                               type = "error", duration = 5)
              return()
            }
            
            train_ts <- window(ts_data_adj, end = time(ts_data_adj)[train_size_adj])
            test_ts <- window(ts_data_adj, start = time(ts_data_adj)[train_size_adj + 1])
            
            exog_train <- exog_full[1:train_size_adj, , drop = FALSE]
            exog_test <- exog_full[(train_size_adj + 1):n_adj, , drop = FALSE]
          }
        }
        
        incProgress(0.2, detail = "Data prepared...")
        
        # Initialize results storage
        values$models_results <- list()
        values$forecasts <- list()
        
        # Run selected models
        if ("arima" %in% input$models) {
          incProgress(0.2, detail = "Running ARIMA...")
          tryCatch({
            values$models_results$arima <- runARIMA(train_ts, test_ts, input)
          }, error = function(e) {
            showNotification(paste("ARIMA failed:", e$message), type = "warning", duration = 5)
          })
        }
        
        if ("sarima" %in% input$models) {
          incProgress(0.2, detail = "Running SARIMA...")
          tryCatch({
            values$models_results$sarima <- runSARIMA(train_ts, test_ts, input)
          }, error = function(e) {
            showNotification(paste("SARIMA failed:", e$message), type = "warning", duration = 5)
          })
        }
        
        if ("arimax" %in% input$models) {
          if (!is.null(exog_train)) {
            incProgress(0.2, detail = "Running ARIMAX...")
            tryCatch({
              values$models_results$arimax <- runARIMAX(train_ts, test_ts, exog_train, exog_test, input)
            }, error = function(e) {
              showNotification(paste("ARIMAX failed:", e$message), type = "warning", duration = 5)
            })
          } else {
            showNotification("ARIMAX requires exogenous variables to be configured.", 
                             type = "warning", duration = 5)
          }
        }
        
        if ("sarimax" %in% input$models) {
          if (!is.null(exog_train)) {
            incProgress(0.2, detail = "Running SARIMAX...")
            tryCatch({
              values$models_results$sarimax <- runSARIMAX(train_ts, test_ts, exog_train, exog_test, input)
            }, error = function(e) {
              showNotification(paste("SARIMAX failed:", e$message), type = "warning", duration = 5)
            })
          } else {
            showNotification("SARIMAX requires exogenous variables to be configured.", 
                             type = "warning", duration = 5)
          }
        }
        
        # Generate forecasts
        for (model_name in names(values$models_results)) {
          if (!is.null(values$models_results[[model_name]]) && 
              !is.null(values$models_results[[model_name]]$best_model)) {
            
            model <- values$models_results[[model_name]]$best_model
            
            tryCatch({
              if (model_name %in% c("arimax", "sarimax") && !is.null(exog_full)) {
                # For models with exogenous variables, create dummy future values
                future_exog <- matrix(
                  rep(colMeans(exog_full, na.rm = TRUE), input$forecastHorizon), 
                  nrow = input$forecastHorizon, 
                  byrow = TRUE
                )
                colnames(future_exog) <- colnames(exog_full)
                values$forecasts[[model_name]] <- forecast(model, h = input$forecastHorizon, xreg = future_exog)
              } else {
                values$forecasts[[model_name]] <- forecast(model, h = input$forecastHorizon)
              }
            }, error = function(e) {
              showNotification(paste("Forecast failed for", model_name, ":", e$message), 
                               type = "warning", duration = 5)
            })
          }
        }
        
        incProgress(0.2, detail = "Completed!")
        
        # Update model selection dropdowns
        if (length(values$models_results) > 0) {
          model_choices <- names(values$models_results)[!sapply(values$models_results, is.null)]
          updateSelectInput(session, "selectedModel", choices = model_choices)
          updateSelectInput(session, "forecastModel", choices = model_choices)
          
          showNotification("Analysis completed successfully!", type = "message", duration = 3)
        } else {
          showNotification("No models were successfully run.", type = "error", duration = 5)
        }
        
      }, error = function(e) {
        showNotification(paste("Error in analysis:", e$message), type = "error", duration = 5)
      })
    })
  })
  
  # Time series plot
  output$tsPlot <- renderPlotly({
    req(values$ts_data)
    
    tryCatch({
      df <- data.frame(
        Date = as.Date(as.yearmon(time(values$ts_data))),
        Value = as.numeric(values$ts_data)
      )
      
      p <- ggplot(df, aes(x = Date, y = Value)) +
        geom_line(color = "blue", size = 1) +
        labs(title = "Time Series Plot", x = "Date", y = "Value") +
        custom_theme
      
      ggplotly(p)
    }, error = function(e) {
      plotly_empty()
    })
  })
  
  # Decomposition plot
  output$decomposePlot <- renderPlot({
    req(values$ts_data)
    
    tryCatch({
      if (frequency(values$ts_data) > 1 && length(values$ts_data) >= 2 * frequency(values$ts_data)) {
        decomp <- decompose(values$ts_data)
        plot(decomp)
      } else {
        plot.new()
        text(0.5, 0.5, "Decomposition requires seasonal data with at least 2 periods", cex = 1.2)
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error in decomposition:", e$message), cex = 1.2)
    })
  })
  
  # ACF/PACF plot
  output$acfPlot <- renderPlot({
    req(values$ts_data)
    
    tryCatch({
      par(mfrow = c(1, 2))
      acf(values$ts_data, main = "ACF")
      pacf(values$ts_data, main = "PACF")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error in ACF/PACF:", e$message), cex = 1.2)
    })
  })
  
  # Stationarity tests
  output$stationarityTests <- renderPrint({
    req(values$ts_data)
    
    tryCatch({
      cat("Augmented Dickey-Fuller Test:\n")
      print(adf.test(values$ts_data))
      
      cat("\n\nKPSS Test:\n")
      print(kpss.test(values$ts_data))
    }, error = function(e) {
      cat("Error in stationarity tests:", e$message)
    })
  })
  
  # Model comparison table
  output$modelComparison <- DT::renderDataTable({
    req(length(values$models_results) > 0)
    
    comparison_df <- data.frame()
    
    for (model_name in names(values$models_results)) {
      if (!is.null(values$models_results[[model_name]]) && 
          !is.null(values$models_results[[model_name]]$metrics)) {
        metrics <- values$models_results[[model_name]]$metrics
        comparison_df <- rbind(comparison_df, 
                               data.frame(
                                 Model = toupper(model_name),
                                 AIC = round(metrics$AIC, 2),
                                 RMSE = round(metrics$RMSE, 4),
                                 MAE = round(metrics$MAE, 4),
                                 MAPE = round(metrics$MAPE, 2)
                               ))
      }
    }
    
    if (nrow(comparison_df) > 0) {
      DT::datatable(comparison_df, 
                    options = list(pageLength = 10),
                    rownames = FALSE) %>%
        formatStyle(columns = colnames(comparison_df), fontSize = '12pt')
    } else {
      DT::datatable(data.frame(Message = "No models run yet"), 
                    options = list(dom = 't'),
                    rownames = FALSE)
    }
  })
  
  # Metrics plot
  output$metricsPlot <- renderPlotly({
    req(length(values$models_results) > 0)
    
    metrics_df <- data.frame()
    
    for (model_name in names(values$models_results)) {
      if (!is.null(values$models_results[[model_name]]) && 
          !is.null(values$models_results[[model_name]]$metrics)) {
        metrics <- values$models_results[[model_name]]$metrics
        metrics_df <- rbind(metrics_df,
                            data.frame(
                              Model = toupper(model_name),
                              Metric = c("RMSE", "MAE", "MAPE"),
                              Value = c(metrics$RMSE, metrics$MAE, metrics$MAPE/10)  # Scale MAPE for visibility
                            ))
      }
    }
    
    if (nrow(metrics_df) > 0) {
      p <- ggplot(metrics_df, aes(x = Model, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title = "Model Performance Metrics", y = "Value") +
        custom_theme
      
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Forecast plot
  output$forecastPlot <- renderPlotly({
    req(input$forecastModel, values$forecasts[[input$forecastModel]])
    
    tryCatch({
      fc <- values$forecasts[[input$forecastModel]]
      
      historical_df <- data.frame(
        Date = as.Date(as.yearmon(time(values$ts_data))),
        Value = as.numeric(values$ts_data),
        Type = "Historical"
      )
      
      # Create forecast dates
      last_date <- max(historical_df$Date)
      forecast_dates <- seq(from = last_date, by = "month", length.out = length(fc$mean) + 1)[-1]
      
      forecast_df <- data.frame(
        Date = forecast_dates,
        Value = as.numeric(fc$mean),
        Lower80 = as.numeric(fc$lower[,1]),
        Upper80 = as.numeric(fc$upper[,1]),
        Lower95 = as.numeric(fc$lower[,2]),
        Upper95 = as.numeric(fc$upper[,2]),
        Type = "Forecast"
      )
      
      p <- ggplot() +
        geom_line(data = historical_df, aes(x = Date, y = Value, color = Type), size = 1) +
        geom_line(data = forecast_df, aes(x = Date, y = Value, color = Type), size = 1) +
        geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95), 
                    alpha = 0.2, fill = "blue") +
        geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80), 
                    alpha = 0.3, fill = "blue") +
        labs(title = paste(toupper(input$forecastModel), "Forecast"), 
             x = "Date", y = "Value") +
        custom_theme
      
      ggplotly(p)
    }, error = function(e) {
      plotly_empty()
    })
  })
  
  # Forecast table
  output$forecastTable <- DT::renderDataTable({
    req(input$forecastModel, values$forecasts[[input$forecastModel]])
    
    tryCatch({
      fc <- values$forecasts[[input$forecastModel]]
      
      # Create proper forecast dates
      last_date <- Sys.Date()
      dates <- seq(from = last_date, by = "month", length.out = length(fc$mean) + 1)[-1]
      
      if (input$logTransform) {
        # Back-transform if log was applied
        forecast_table <- data.frame(
          Date = dates,
          Forecast = round(exp(fc$mean), 2),
          Lower_80 = round(exp(fc$lower[,1]), 2),
          Upper_80 = round(exp(fc$upper[,1]), 2),
          Lower_95 = round(exp(fc$lower[,2]), 2),
          Upper_95 = round(exp(fc$upper[,2]), 2)
        )
      } else {
        forecast_table <- data.frame(
          Date = dates,
          Forecast = round(fc$mean, 2),
          Lower_80 = round(fc$lower[,1], 2),
          Upper_80 = round(fc$upper[,1], 2),
          Lower_95 = round(fc$lower[,2], 2),
          Upper_95 = round(fc$upper[,2], 2)
        )
      }
      
      DT::datatable(forecast_table, 
                    options = list(pageLength = 12),
                    rownames = FALSE)
    }, error = function(e) {
      DT::datatable(data.frame(Error = e$message), 
                    options = list(dom = 't'),
                    rownames = FALSE)
    })
  })
  
  # Model diagnostic outputs
  output$residualPlots <- renderPlot({
    req(input$selectedModel, values$models_results[[input$selectedModel]])
    
    tryCatch({
      model <- values$models_results[[input$selectedModel]]$best_model
      if (!is.null(model)) {
        checkresiduals(model)
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error in residual plots:", e$message), cex = 1.2)
    })
  })
  
  output$modelSummary <- renderPrint({
    req(input$selectedModel, values$models_results[[input$selectedModel]])
    
    tryCatch({
      model <- values$models_results[[input$selectedModel]]$best_model
      if (!is.null(model)) {
        summary(model)
      }
    }, error = function(e) {
      cat("Error in model summary:", e$message)
    })
  })
  
  output$ljungBoxTest <- renderPrint({
    req(input$selectedModel, values$models_results[[input$selectedModel]])
    
    tryCatch({
      model <- values$models_results[[input$selectedModel]]$best_model
      if (!is.null(model)) {
        res <- residuals(model)
        
        lags <- c(5, 10, 15, 20)
        for (lag in lags) {
          if (lag < length(res)) {
            cat(paste("\nLjung-Box Test at lag", lag, ":\n"))
            print(Box.test(res, lag = lag, type = "Ljung-Box"))
          }
        }
      }
    }, error = function(e) {
      cat("Error in Ljung-Box test:", e$message)
    })
  })
  
  # Download handler
  output$downloadForecast <- downloadHandler(
    filename = function() {
      paste0(input$forecastModel, "_forecast_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$forecastModel, values$forecasts[[input$forecastModel]])
      
      fc <- values$forecasts[[input$forecastModel]]
      last_date <- Sys.Date()
      dates <- seq(from = last_date, by = "month", length.out = length(fc$mean) + 1)[-1]
      
      if (input$logTransform) {
        forecast_table <- data.frame(
          Date = dates,
          Forecast = exp(fc$mean),
          Lower_80 = exp(fc$lower[,1]),
          Upper_80 = exp(fc$upper[,1]),
          Lower_95 = exp(fc$lower[,2]),
          Upper_95 = exp(fc$upper[,2])
        )
      } else {
        forecast_table <- data.frame(
          Date = dates,
          Forecast = fc$mean,
          Lower_80 = fc$lower[,1],
          Upper_80 = fc$upper[,1],
          Lower_95 = fc$lower[,2],
          Upper_95 = fc$upper[,2]
        )
      }
      
      write.csv(forecast_table, file, row.names = FALSE)
    }
  )
}

# Helper functions remain the same but with additional error handling
runARIMA <- function(train_ts, test_ts, input) {
  orders <- expand.grid(
    p = seq(input$pRange[1], input$pRange[2]),
    d = seq(input$dRange[1], input$dRange[2]),
    q = seq(input$qRange[1], input$qRange[2])
  )
  
  best_aic <- Inf
  best_model <- NULL
  best_metrics <- NULL
  
  for (i in 1:nrow(orders)) {
    tryCatch({
      model <- Arima(train_ts, 
                     order = c(orders[i,]$p, orders[i,]$d, orders[i,]$q),
                     method = "ML")
      
      if (!is.na(model$aic) && model$aic < best_aic) {
        best_aic <- model$aic
        best_model <- model
        
        # Calculate metrics
        fc <- forecast(model, h = length(test_ts))
        
        # Ensure no division by zero in MAPE
        non_zero_test <- test_ts[test_ts != 0]
        non_zero_fc <- fc$mean[test_ts != 0]
        
        best_metrics <- list(
          AIC = model$aic,
          RMSE = sqrt(mean((fc$mean - test_ts)^2, na.rm = TRUE)),
          MAE = mean(abs(fc$mean - test_ts), na.rm = TRUE),
          MAPE = ifelse(length(non_zero_test) > 0,
                        mean(abs((non_zero_fc - non_zero_test)/non_zero_test), na.rm = TRUE) * 100,
                        NA)
        )
      }
    }, error = function(e) {
      # Skip this model combination if it fails
    })
  }
  
  # Refit on full data if we found a model
  if (!is.null(best_model)) {
    tryCatch({
      full_ts <- ts(c(train_ts, test_ts), 
                    frequency = frequency(train_ts), 
                    start = start(train_ts))
      best_order <- arimaorder(best_model)
      best_model <- Arima(full_ts, order = best_order[c("p", "d", "q")])
    }, error = function(e) {
      # Keep the original model if refitting fails
    })
  }
  
  return(list(best_model = best_model, metrics = best_metrics))
}

runSARIMA <- function(train_ts, test_ts, input) {
  non_seasonal_orders <- expand.grid(
    p = seq(input$pRange[1], input$pRange[2]),
    d = seq(input$dRange[1], input$dRange[2]),
    q = seq(input$qRange[1], input$qRange[2])
  )
  
  seasonal_orders <- expand.grid(
    P = seq(input$PRange[1], input$PRange[2]),
    D = seq(input$DRange[1], input$DRange[2]),
    Q = seq(input$QRange[1], input$QRange[2])
  )
  
  best_aic <- Inf
  best_model <- NULL
  best_metrics <- NULL
  best_order <- NULL
  best_seasonal <- NULL
  
  for (i in 1:nrow(non_seasonal_orders)) {
    for (j in 1:nrow(seasonal_orders)) {
      tryCatch({
        model <- Arima(train_ts,
                       order = c(non_seasonal_orders[i,]$p, 
                                 non_seasonal_orders[i,]$d, 
                                 non_seasonal_orders[i,]$q),
                       seasonal = list(order = c(seasonal_orders[j,]$P,
                                                 seasonal_orders[j,]$D,
                                                 seasonal_orders[j,]$Q),
                                       period = frequency(train_ts)),
                       method = "ML")
        
        if (!is.na(model$aic) && model$aic < best_aic) {
          best_aic <- model$aic
          best_model <- model
          best_order <- c(non_seasonal_orders[i,]$p, 
                          non_seasonal_orders[i,]$d, 
                          non_seasonal_orders[i,]$q)
          best_seasonal <- c(seasonal_orders[j,]$P,
                             seasonal_orders[j,]$D,
                             seasonal_orders[j,]$Q)
          
          # Calculate metrics
          fc <- forecast(model, h = length(test_ts))
          
          # Ensure no division by zero in MAPE
          non_zero_test <- test_ts[test_ts != 0]
          non_zero_fc <- fc$mean[test_ts != 0]
          
          best_metrics <- list(
            AIC = model$aic,
            RMSE = sqrt(mean((fc$mean - test_ts)^2, na.rm = TRUE)),
            MAE = mean(abs(fc$mean - test_ts), na.rm = TRUE),
            MAPE = ifelse(length(non_zero_test) > 0,
                          mean(abs((non_zero_fc - non_zero_test)/non_zero_test), na.rm = TRUE) * 100,
                          NA)
          )
        }
      }, error = function(e) {
        # Skip this model combination if it fails
      })
    }
  }
  
  # Refit on full data if we found a model
  if (!is.null(best_model)) {
    tryCatch({
      full_ts <- ts(c(train_ts, test_ts), 
                    frequency = frequency(train_ts),
                    start = start(train_ts))
      best_model <- Arima(full_ts,
                          order = best_order,
                          seasonal = list(order = best_seasonal, 
                                          period = frequency(train_ts)))
    }, error = function(e) {
      # Keep the original model if refitting fails
    })
  }
  
  return(list(best_model = best_model, metrics = best_metrics))
}

runARIMAX <- function(train_ts, test_ts, exog_train, exog_test, input) {
  orders <- expand.grid(
    p = seq(input$pRange[1], input$pRange[2]),
    d = seq(input$dRange[1], input$dRange[2]),
    q = seq(input$qRange[1], input$qRange[2])
  )
  
  best_aic <- Inf
  best_model <- NULL
  best_metrics <- NULL
  best_order <- NULL
  
  for (i in 1:nrow(orders)) {
    tryCatch({
      model <- Arima(train_ts,
                     order = c(orders[i,]$p, orders[i,]$d, orders[i,]$q),
                     xreg = exog_train,
                     method = "ML")
      
      if (!is.na(model$aic) && model$aic < best_aic) {
        best_aic <- model$aic
        best_model <- model
        best_order <- c(orders[i,]$p, orders[i,]$d, orders[i,]$q)
        
        # Calculate metrics
        fc <- forecast(model, h = length(test_ts), xreg = exog_test)
        
        # Ensure no division by zero in MAPE
        non_zero_test <- test_ts[test_ts != 0]
        non_zero_fc <- fc$mean[test_ts != 0]
        
        best_metrics <- list(
          AIC = model$aic,
          RMSE = sqrt(mean((fc$mean - test_ts)^2, na.rm = TRUE)),
          MAE = mean(abs(fc$mean - test_ts), na.rm = TRUE),
          MAPE = ifelse(length(non_zero_test) > 0,
                        mean(abs((non_zero_fc - non_zero_test)/non_zero_test), na.rm = TRUE) * 100,
                        NA)
        )
      }
    }, error = function(e) {
      # Skip this model combination if it fails
    })
  }
  
  # Refit on full data if we found a model
  if (!is.null(best_model)) {
    tryCatch({
      full_ts <- ts(c(train_ts, test_ts), 
                    frequency = frequency(train_ts),
                    start = start(train_ts))
      full_exog <- rbind(exog_train, exog_test)
      best_model <- Arima(full_ts,
                          order = best_order,
                          xreg = full_exog)
    }, error = function(e) {
      # Keep the original model if refitting fails
    })
  }
  
  return(list(best_model = best_model, metrics = best_metrics))
}

runSARIMAX <- function(train_ts, test_ts, exog_train, exog_test, input) {
  non_seasonal_orders <- expand.grid(
    p = seq(input$pRange[1], input$pRange[2]),
    d = seq(input$dRange[1], input$dRange[2]),
    q = seq(input$qRange[1], input$qRange[2])
  )
  
  seasonal_orders <- expand.grid(
    P = seq(input$PRange[1], input$PRange[2]),
    D = seq(input$DRange[1], input$DRange[2]),
    Q = seq(input$QRange[1], input$QRange[2])
  )
  
  best_aic <- Inf
  best_model <- NULL
  best_metrics <- NULL
  best_order <- NULL
  best_seasonal <- NULL
  
  for (i in 1:nrow(non_seasonal_orders)) {
    for (j in 1:nrow(seasonal_orders)) {
      tryCatch({
        model <- Arima(train_ts,
                       order = c(non_seasonal_orders[i,]$p,
                                 non_seasonal_orders[i,]$d,
                                 non_seasonal_orders[i,]$q),
                       seasonal = list(order = c(seasonal_orders[j,]$P,
                                                 seasonal_orders[j,]$D,
                                                 seasonal_orders[j,]$Q),
                                       period = frequency(train_ts)),
                       xreg = exog_train,
                       method = "ML")
        
        if (!is.na(model$aic) && model$aic < best_aic) {
          best_aic <- model$aic
          best_model <- model
          best_order <- c(non_seasonal_orders[i,]$p,
                          non_seasonal_orders[i,]$d,
                          non_seasonal_orders[i,]$q)
          best_seasonal <- c(seasonal_orders[j,]$P,
                             seasonal_orders[j,]$D,
                             seasonal_orders[j,]$Q)
          
          # Calculate metrics
          fc <- forecast(model, h = length(test_ts), xreg = exog_test)
          
          # Ensure no division by zero in MAPE
          non_zero_test <- test_ts[test_ts != 0]
          non_zero_fc <- fc$mean[test_ts != 0]
          
          best_metrics <- list(
            AIC = model$aic,
            RMSE = sqrt(mean((fc$mean - test_ts)^2, na.rm = TRUE)),
            MAE = mean(abs(fc$mean - test_ts), na.rm = TRUE),
            MAPE = ifelse(length(non_zero_test) > 0,
                          mean(abs((non_zero_fc - non_zero_test)/non_zero_test), na.rm = TRUE) * 100,
                          NA)
          )
        }
      }, error = function(e) {
        # Skip this model combination if it fails
      })
    }
  }
  
  # Refit on full data if we found a model
  if (!is.null(best_model)) {
    tryCatch({
      full_ts <- ts(c(train_ts, test_ts), 
                    frequency = frequency(train_ts),
                    start = start(train_ts))
      full_exog <- rbind(exog_train, exog_test)
      best_model <- Arima(full_ts,
                          order = best_order,
                          seasonal = list(order = best_seasonal,
                                          period = frequency(train_ts)),
                          xreg = full_exog)
    }, error = function(e) {
      # Keep the original model if refitting fails
    })
  }
  
  return(list(best_model = best_model, metrics = best_metrics))
}

# Run the app
shinyApp(ui = ui, server = server)
