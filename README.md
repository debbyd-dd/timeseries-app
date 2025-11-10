Time Series Analysis Tool (Shiny R App)
A powerful, interactive time series forecasting and modeling application built with R and Shiny, designed to make time series analysis accessible to analysts, researchers, and data scientists. This tool supports multiple ARIMA-based models including ARIMA, SARIMA, ARIMAX, and SARIMAX, with full model comparison, diagnostics, and visualization capabilities.

Key Features:
    Upload & Preview: Easily upload CSV data and configure date, frequency, and target variable.
    Flexible Modeling: Choose from ARIMA, SARIMA, ARIMAX, or SARIMAX models with customizable parameter ranges.
    Exogenous Variables: Support for external regressors (e.g., temperature, rainfall) with user-defined lags.
    Data Preprocessing: Optional log transformation and automatic differencing for stationarity.
    Model Comparison: Compare models using AIC, RMSE, MAE, and MAPE with visual performance plots.
    Diagnostics: Residual analysis, ACF/PACF, stationarity tests (ADF, KPSS), and Ljung-Box tests.
    Forecasting: Generate and visualize forecasts with confidence intervals (80% and 95%).
    Export Results: Download forecast tables in CSV format.
    Interactive Visuals: Powered by plotly, ggplot2, and DT for dynamic, publication-ready charts.

Built With:
    shiny & shinydashboard – Interactive web interface
    forecast & tseries – Time series modeling and forecasting
    ggplot2 & plotly – Advanced visualizations
    DT – Interactive data tables
    zoo, lubridate, vars, MTS – Data and time handling

Ideal For:
    Forecasting sales, demand, weather, or economic indicators
    Teaching time series concepts
    Rapid prototyping of forecasting models
    Exploratory time series analysis

How to Use:
    Clone the repo
    Install required R packages
    Run shiny::runApp() to launch the dashboard
    Upload your time series data and start modeling!

No coding required for end users – just point, click, and analyze!
