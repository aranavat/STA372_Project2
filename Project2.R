
library(tidyverse)
library(fpp3)
library(nortest)
library(stlplus)

file <- "walt_disney_revenue.csv"
data_table <- read_csv(file)

data_table_ts <- data_table |> 
  add_column(qtr=yearquarter("2010 Q1") + 0:59, .before = TRUE) |> 
  as_tsibble(index = qtr) |> 
  mutate(Quarter = as.numeric(sub("Q([1-4]).*", "\\1", Quarter)))
head(data_table_ts)
tail(data_table_ts)

#   Create factor for Quarter using numbers (not Jan, Feb, ...)
#
data_table_ts$Quarter <- as.factor(data_table_ts$Quarter)
head(data_table_ts)
#
#   Set colors
#
Quarter_colors <- c("black", "blue", "purple", "red")

#   Plot Revenue against Time
#
data_table_ts %>% autoplot(Revenue) +
  geom_point(aes(y=Revenue, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Walt Disney Company Revenue vs. Time") + xlab("Time") + ylab("Revenue")
#
#   Plot log(Revenue) against Time
#
data_table_ts <- data_table_ts %>% mutate(LogRevenue = log(Revenue))
data_table_ts %>% autoplot(LogRevenue) +
  geom_point(aes(y=LogRevenue, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Log(Revenue) vs. Time") + xlab("Time") + ylab("Log(Revenue)")

#   Assign NA values to Revenue and log(Revenue) and log(A) for the intervention period 
#
data_table_ts$Revenue[43:48] <- NA
data_table_ts$LogRevenue[43:48] <- NA
#
#   Print data around intervention period
#
print(data_table_ts[40:51,], n=32)
#
#   Plot Revenue against Time with observations removed
#
data_table_ts %>% autoplot(Revenue) +
  geom_point(aes(y=Revenue, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Revenue vs. Time without Intervention Period") + xlab("Time") + ylab("Revenue")

#   Plot log(Revenue) against Time with observations removed
#
data_table_ts %>% autoplot(LogRevenue) +
  geom_point(aes(y=LogRevenue, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Log(Revenue) vs. Time without Intervention Period") + xlab("Time") + ylab("Log(Revenue)")


#   Seasonally adjust LogRevenue using stlplus to allow for missing observations
#
LogRevenue_time_series <- ts(data_table_ts$LogRevenue, frequency=4)
components <- stlplus(LogRevenue_time_series, s.window=11, robust=TRUE)
data_table_ts$season <- components$data$seasonal
data_table_ts$LogA <- components$data$raw - components$data$seasonal
#
#   Print full tsibble around intervention period
#
print(data_table_ts[40:51,], n=32)
#
#   Plot LogA_stlplus against Time for comparison purposes
#
data_table_ts %>% autoplot(LogA) +
  geom_point(aes(y=LogA, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Seasonally Adjusted Log(Revenue) vs. Time") + xlab("Time") + ylab("Seasonally Adjusted Log(Revenue)")
#
#   Compute autocorrelation function of LogA
#
data_table_ts %>% ACF(LogA) %>% autoplot() +
  ggtitle("ACF Plot of Seasonally Adjusted Log(Revenue)")
#
#   Compute KPSS test for nonstationary data
#
unitroot_kpss(data_table_ts$LogA)
#
#   Plot diff_LogA against Time
#
data_table_ts <- data_table_ts %>% mutate(diff_LogA = difference(LogA,1))
mean_diff_LogA <- mean(data_table_ts$diff_LogA, na.rm=TRUE)
data_table_ts %>% autoplot(diff_LogA) +
  geom_point(aes(y=diff_LogA, color=Quarter)) +
  geom_hline(aes(yintercept=mean_diff_LogA), lty=2) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("First Differences of Seasonally-Adjusted Log(Revenue) vs. Time") + xlab("Time") + ylab("diff_LogA")
#
#   Compute autocorrelation function of diff_LogA
#
data_table_ts %>% ACF(diff_LogA) %>% autoplot() +
  ggtitle("ACF Plot of the First Differences")
#
#   Compute KPSS test for nonstationary data
#
unitroot_kpss(data_table_ts$diff_LogA)
#
#   Use ARIMA to select best ARIMA(p,d,q) model for LogA
#
result_ARIMA_LogA <- data_table_ts %>% 
  model(ARIMA(LogA ~ PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(result_ARIMA_LogA)
#
#   Check the assumptions
#
result_ARIMA_LogA %>% gg_tsresiduals()
result_ARIMA_LogA_augment <- augment(result_ARIMA_LogA)
ad.test(result_ARIMA_LogA_augment$.resid)
#
#   Compute forecasts for LogA
#
result_ARIMA_LogA_forecast <- result_ARIMA_LogA %>% forecast(h=6)
forecast_LogA <- result_ARIMA_LogA_forecast$.mean
forecast_LogA
#
#   Print and store forecast variances for LogA
#
var_forecast_errors_LogA <- distributional::variance(result_ARIMA_LogA_forecast$LogA)
var_forecast_errors_LogA
#
#   Compute forecasts for season
#
result_ARIMA_season <- data_table_ts %>% 
  model(ARIMA(season ~ 0 + pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)
result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=6)
forecast_season <- result_ARIMA_season_forecast$.mean
forecast_season
#
#   Print and store forecast variances for season
#
var_forecast_errors_season <- distributional::variance(result_ARIMA_season_forecast$season)
var_forecast_errors_season
#
#   Compute correlation between epsilon(t) and a(t)
#
result_ARIMA_LogA_augment <- augment(result_ARIMA_LogA)
result_ARIMA_season_augment <- augment(result_ARIMA_season)
correlation <- cor(result_ARIMA_LogA_augment$.resid, 
                   result_ARIMA_season_augment$.resid, use = 'complete') 
correlation
#
#   Compute variances for prediction intervals for LogRevenue
#
covariance <- sqrt(var_forecast_errors_LogA) * sqrt(var_forecast_errors_season) * correlation
variance <- var_forecast_errors_LogA + var_forecast_errors_season + 2 * covariance
variance
#
#   Compute prediction intervals for LogRevenue
#
forecast_LogRevenue <- forecast_LogA + forecast_season
forecast_LogRevenue
z_value <- qnorm(0.975, 0, 1)
PI_low_LogRevenue <- forecast_LogRevenue - z_value * sqrt(variance)
PI_up_LogRevenue  <- forecast_LogRevenue + z_value * sqrt(variance)
PI_low_LogRevenue
PI_up_LogRevenue
#
#   Compute prediction intervals for Revenue
#
forecast_Revenue <- exp(forecast_LogRevenue)
forecast_Revenue
PI_low_Revenue <- exp(PI_low_LogRevenue)
PI_up_Revenue  <- exp(PI_up_LogRevenue)
PI_low_Revenue
PI_up_Revenue
#
#   Compute in-sample forecasts for Revenue
#
data_table_ts$insample_Revenue <- exp(result_ARIMA_LogA_augment$.fitted + 
                                      result_ARIMA_season_augment$.fitted)
#
#   Create tsibble for plotting out-of-sample forecasts
#
forecast_ts <- tsibble(Quarter_index = yearquarter("2025 Q1") + 0:5,
                       forecast_Revenue = forecast_Revenue,
                       PI_low_Revenue = PI_low_Revenue,
                       PI_up_Revenue = PI_up_Revenue,
                       index = Quarter_index)
head(forecast_ts)
#
#   Plot in-sample and out-of-sample forecasts
#
data_table_ts %>% autoplot(Revenue) +
  geom_line(aes(y=insample_Revenue), color="red") +
  geom_line(data = forecast_ts, aes(x=Quarter_index,  y=forecast_Revenue), color="blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Revenue vs. Time") + xlab("Time") + ylab("Revenue")
#
#   Focus plot on out-of-sample forecasts and prediction intervals
#
data_table_ts[50:60,] %>% autoplot(Revenue) +
  geom_point(aes(y=Revenue)) +
  geom_line(aes(y=insample_Revenue), color="red") +
  geom_point(aes(y=insample_Revenue), color="red") +
  geom_point(data = forecast_ts, aes(x=Quarter_index, y=forecast_Revenue), color="blue") +
  geom_line(data = forecast_ts, aes(x=Quarter_index,  y=forecast_Revenue), color="blue") +
  geom_point(data = forecast_ts, aes(x=Quarter_index, y=PI_low_Revenue), color="blue") +
  geom_line(data = forecast_ts, aes(x=Quarter_index,  y=PI_low_Revenue), color="blue", lty=2) +
  geom_point(data = forecast_ts, aes(x=Quarter_index, y=PI_up_Revenue),  color="blue") +
  geom_line(data = forecast_ts, aes(x=Quarter_index,  y=PI_up_Revenue),  color="blue", lty=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Revenue vs. Time") + xlab("Time") + ylab("Revenue")



