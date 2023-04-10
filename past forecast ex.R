# Load necessary libraries
library(forecast)
library(tidyverse)

# Import data
data <- read.csv("your_data.csv")

# Convert time variable to a time series object
data_ts <- ts(data$outcome, start = c(2010, 1), frequency = 12)

# Fit an ARIMA model to the time series
arima_model <- auto.arima(data_ts)

# Forecast past values at t = -1, -2, etc.
past_forecast <- forecast(arima_model, h = 12, level = c(90, 95, 99))
past_values <- tail(data_ts, 12) - past_forecast$mean

# View the predicted past values
cat("Predicted values at t = -1, -2, etc.:", past_values, "\n")


#inverted data

# Load necessary libraries
library(forecast)
library(tidyverse)

# Import data
data <- read.csv("your_data.csv")

# Convert time variable to a time series object
data_ts <- ts(data$outcome, start = c(2010, 1), frequency = 12)

# Invert the time series
inverted_data <- rev(diffinv(diff(rev(data_ts))))

# View the backcasted values
cat("Backcasted values:", inverted_data, "\n")
