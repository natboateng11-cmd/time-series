# Coursework 1 Scratchpad
# Topic: Google Trends for Drake

drake_data <- read.csv("data/time_series_GB_20210320-1217_20260320-1217.csv")
head(drake_data)
str(drake_data)
summary(drake_data)
drake_prophet_data <- data.frame(
  ds = as.Date(drake_data$Time),
  y = drake_data$Drake
)

head(drake_prophet_data)
str(drake_prophet_data)
plot(
  drake_prophet_data$ds,
  drake_prophet_data$y,
  type = "l",
  main = "Google Trends Interest in Drake",
  xlab = "Date",
  ylab = "Search Interest"
)
min(drake_prophet_data$ds)
max(drake_prophet_data$ds)
nrow(drake_prophet_data)

plot(
  drake_prophet_data$ds,
  drake_prophet_data$y,
  type = "l",
  main = "Google Trends Interest in Drake",
  xlab = "Date",
  ylab = "Search Interest"
)

abline(lm(y ~ ds, data = drake_prophet_data), lty = 2)

library(prophet)
drake_prophet_model <- prophet(drake_prophet_data)

drake_future_dates <- make_future_dataframe(drake_prophet_model, periods = 12)

drake_forecast <- predict(drake_prophet_model, drake_future_dates)

plot(drake_prophet_model, drake_forecast)

drake_prophet_model_weekly <- prophet(
    drake_prophet_data,
    weekly.seasonality = TRUE
)

drake_future_dates_weekly <- make_future_dataframe(
    drake_prophet_model_weekly,
    periods = 12
)

drake_forecast_weekly <- predict(
    drake_prophet_model_weekly,
    drake_future_dates_weekly
)

plot(drake_prophet_model_weekly, drake_forecast_weekly)

tail(drake_forecast_weekly[, c("ds", "yhat", "yhat_lower", "yhat_upper")], 12)