# 📈 Time Series Analysis Tool (Shiny R App)

[![Live Demo](https://img.shields.io/badge/demo-live-brightgreen.svg?style=for-the-badge)](https://debbyd.shinyapps.io/TimeSeriesForecasting/)
[![R](https://img.shields.io/badge/R-4.2+-blue.svg?style=for-the-badge)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7+-blue.svg?style=for-the-badge)](https://shiny.posit.co/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)](LICENSE)

## 🔗 Live Demo
**[Click here to try the application live on ShinyApps.io](https://debbyd.shinyapps.io/TimeSeriesForecasting/)**

## 📖 Overview
A powerful, interactive time series forecasting and modeling application built with R and Shiny, designed to make time series analysis accessible to analysts, researchers, and data scientists. This tool supports multiple ARIMA-based models including **ARIMA, SARIMA, ARIMAX, and SARIMAX**, with full model comparison, diagnostics, and visualization capabilities.

## ✨ Key Features

*   **📤 Upload & Preview**: Easily upload CSV data and configure date, frequency, and target variable.
*   **🎛️ Flexible Modeling**: Choose from ARIMA, SARIMA, ARIMAX, or SARIMAX models with customizable parameter ranges.
*   **🌦️ Exogenous Variables**: Support for external regressors (e.g., temperature, rainfall) with user-defined lags.
*   **⚙️ Data Preprocessing**: Optional log transformation and automatic differencing for stationarity.
*   **📊 Model Comparison**: Compare models using **AIC, RMSE, MAE, and MAPE** with visual performance plots.
*   **🔍 Diagnostics**: Residual analysis, ACF/PACF, stationarity tests (ADF, KPSS), and Ljung-Box tests.
*   **🔮 Forecasting**: Generate and visualize forecasts with confidence intervals (80% and 95%).
*   **💾 Export Results**: Download forecast tables in CSV format.
*   **📈 Interactive Visuals**: Powered by `plotly`, `ggplot2`, and `DT` for dynamic, publication-ready charts.

## 🎯 Ideal For
*   Forecasting sales, demand, weather, or economic indicators.
*   Teaching time series concepts.
*   Rapid prototyping of forecasting models.
*   Exploratory time series analysis.

## 📸 Screenshots
<!-- Create a folder named 'screenshots' in your repo and add images of your app there -->
<div align="center">
  <img src="screenshots/dashboard_view.png" alt="Dashboard View" width="45%" />
  <img src="screenshots/analysis_view.png" alt="Forecast View" width="45%" />
  <img src="screenshots/forecast_view.png" alt="Forecast View" width="45%" />
</div>

## 🛠️ Built With

*   **[shiny](https://shiny.posit.co/) & [shinydashboard](https://rstudio.github.io/shinydashboard/)** – Interactive web interface
*   **[forecast](https://pkg.robjhyndman.com/forecast/) & [tseries](https://cran.r-project.org/web/packages/tseries/index.html)** – Time series modeling and forecasting
*   **[ggplot2](https://ggplot2.tidyverse.org/) & [plotly](https://plotly.com/r/)** – Advanced visualizations
*   **[DT](https://rstudio.github.io/DT/)** – Interactive data tables
*   **zoo, lubridate, vars, MTS** – Data and time handling

## 🚀 Installation & Local Usage

To run this app locally on your machine:

1.  **Clone the repository**:
    ```
    git clone https://github.com/debbyd-dd/timeseries-app.git
    ```

2.  **Install R Dependencies**:
    Open R or RStudio and run the following command to install the required packages:

    ```
    install.packages(c(
      "shiny", "shinydashboard", "DT", "tseries", "forecast", 
      "ggplot2", "zoo", "reshape2", "car", "vars", "MTS", 
      "gridExtra", "plotly", "lubridate"
    ))
    ```

3.  **Run the App**:
    ```
    library(shiny)
    runApp("path/to/app_folder")
    ```

## 🧠 Methodology & Technical Notes

### ARIMAX/SARIMAX Implementation
When using exogenous variables (e.g., Rainfall impacting Sales), the model requires **lags** to be statistically valid.
*   **Data Alignment**: The application automatically aligns the target variable with the lagged exogenous predictors.
*   **Truncation Logic**: To ensure a fair statistical comparison (AIC/RMSE) between models, the dataset is truncated by the maximum lag size chosen. This ensures that ARIMA and ARIMAX models are evaluated on the exact same time window.

### Error Handling
The application includes robust error handling for common R data issues, including:
*   Safe conversion of CSV columns to numeric types (preventing *"NAs introduced by coercion"*).
*   Handling of non-positive values during Log Transformations.

## 🤝 Contributing

Contributions, issues, and feature requests are welcome! Feel free to check the [issues page](https://github.com/yourusername/shiny-time-series-forecaster/issues).

## 📄 License

This project is licensed under the GNU Affero General Public License v3.0 License - see the [LICENSE](LICENSE) file for details.
