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

#   Assign NA values to Revenue for the intervention period 
#
data_table_ts$Revenue[43:48] <- NA
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

### MODEL 1 ###
model1_ts <- data_table_ts
n <- nrow(model1_ts)
n_test <- 6
n_train <- n - n_test
n; n_test; n_train
model1_train <- model1_ts[1:n_train,]
model1_test <- model1_ts[(n_train+1):n,]
head(model1_train)
tail(model1_train)
head(model1_test)
tail(model1_test)

#   Seasonally adjust Revenue using stlplus to allow for missing observations
#
Revenue_time_series <- ts(model1_train$Revenue, frequency=4)
components <- stlplus(Revenue_time_series, s.window=11, robust=TRUE)
model1_train$season <- components$data$seasonal
model1_train$A <- components$data$raw - components$data$seasonal
#
#   Print full tsibble around intervention period
#
print(model1_train[40:51,], n=32)
#
#   Plot A against Time for comparison purposes
#
model1_train %>% autoplot(A) +
  geom_point(aes(y=A, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Seasonally Adjusted Revenue vs. Time") + xlab("Time") + ylab("Seasonally Adjusted Revenue")
#
#   Compute autocorrelation function of A
#
model1_train %>% ACF(A) %>% autoplot() +
  ggtitle("ACF Plot of Seasonally Adjusted Revenue")
#
#   Compute KPSS test for nonstationary data
#
unitroot_kpss(model1_train$A)
#
#   Plot diff_A against Time
#
model1_train <- model1_train %>% mutate(diff_A = difference(A,1))
mean_diff_A <- mean(model1_train$diff_A, na.rm=TRUE)
model1_train %>% autoplot(diff_A) +
  geom_point(aes(y=diff_A, color=Quarter)) +
  geom_hline(aes(yintercept=mean_diff_A), lty=2) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("First Differences of Seasonally-Adjusted Revenue vs. Time") + xlab("Time") + ylab("diff_A")
#
#   Compute autocorrelation function of diff_A
#
model1_train %>% ACF(diff_A) %>% autoplot() +
  ggtitle("ACF Plot of the First Differences")
#
#   Compute KPSS test for nonstationary data
#
unitroot_kpss(model1_train$diff_A)
#
#   Use ARIMA to select best ARIMA(p,d,q) model for A
#
result_ARIMA_A <- model1_train %>% 
  model(ARIMA(A ~ PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(result_ARIMA_A)
#
#   Check the assumptions
#
result_ARIMA_A %>% gg_tsresiduals()
result_ARIMA_A_augment <- augment(result_ARIMA_A)
ad.test(result_ARIMA_A_augment$.resid)

# 
#   Determine if outlier should be removed
# 
scale(result_ARIMA_A_augment$.resid) |> # find the z-score of all the points
  max(na.rm = TRUE) # get the outlier's z-score

#
#   Compute forecasts for A
#
result_ARIMA_A_forecast <- result_ARIMA_A %>% forecast(h=6)
forecast_A <- result_ARIMA_A_forecast$.mean
forecast_A
#
#   Print and store forecast variances for A
#
var_forecast_errors_A <- distributional::variance(result_ARIMA_A_forecast$A)
var_forecast_errors_A
#
#   Compute forecasts for season
#
result_ARIMA_season <- model1_train %>% 
  model(ARIMA(season ~ pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)
result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=6)
forecast_season <- result_ARIMA_season_forecast$.mean
forecast_season

#   Compute in-sample forecasts for Revenue
#
result_ARIMA_A_augment <- augment(result_ARIMA_A)
result_ARIMA_season_augment <- augment(result_ARIMA_season)
model1_train$insample_Revenue <- result_ARIMA_A_augment$.fitted + 
  result_ARIMA_season_augment$.fitted

#   Forecast for n_test = 6 quarters and incorporate into data_table_ts_test
#
result_ARIMA_A_forecast <- result_ARIMA_A %>% forecast(h=n_test)
result_ARIMA_A_forecast$.mean
model1_test <- model1_test %>% 
  add_column(Fcast_A = result_ARIMA_A_forecast$.mean)
#
#   Incorporate forecasts of next 6 quarters of seasonal factors
#
model1_test <- model1_test %>% 
  add_column(Fcast_season = forecast_season)

#   Compute forecast of log(Homes) and Homes for the test data set
#
model1_test <- model1_test %>% 
  mutate(Fcast_Revenue = Fcast_A + Fcast_season)
print(model1_test)
#

#   Plot in-sample and out-of-sample forecasts
#
model1_train %>% autoplot(Revenue) +
  geom_line(aes(y=insample_Revenue), color="red") +
  geom_line(data = model1_test, aes(x=qtr,  y=Fcast_Revenue), color="blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Model 1 In-sample Forecast") + xlab("Time") + ylab("Revenue")
#

#
#   Compute root mean squared error
#
model1_test <- model1_test %>% mutate(Error = Revenue - Fcast_Revenue)
print(model1_test,n=6)
RMSE <- sqrt(sum(model1_test$Error^2)/6)
RMSE
#
#   Compute mean absolute error
#
MAE <- sum(abs(model1_test$Error)/6)
MAE

### MODEL 2 ###


model2_ts <- data_table_ts[17:nrow(data_table_ts),]

n <- nrow(model2_ts)
n_test <- 6
n_train <- n - n_test
n; n_test; n_train
model2_train <- model2_ts[1:n_train,]
model2_test <- model2_ts[(n_train+1):n,]
head(model2_train)
tail(model2_train)
head(model2_test)
tail(model2_test)

#   Seasonally adjust Revenue using stlplus to allow for missing observations
#
Revenue_time_series <- ts(model2_train$Revenue, frequency=4)
components <- stlplus(Revenue_time_series, s.window=11, robust=TRUE)
model2_train$season <- components$data$seasonal
model2_train$A <- components$data$raw - components$data$seasonal
#
#   Print full tsibble around intervention period
#
print(model2_train[24:35,])
#
#   Plot A against Time for comparison purposes
#
model2_train %>% autoplot(A) +
  geom_point(aes(y=A, color=Quarter)) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("Seasonally Adjusted Revenue vs. Time") + xlab("Time") + ylab("Seasonally Adjusted Revenue")
#
#   Compute autocorrelation function of A
#
model2_train %>% ACF(A) %>% autoplot() +
  ggtitle("ACF Plot of Seasonally Adjusted Revenue")
#
#   Compute KPSS test for nonstationary data
#
unitroot_kpss(model2_train$A)
#
#   Plot diff_A against Time
#
model2_train <- model2_train %>% mutate(diff_A = difference(A,1))
mean_diff_A <- mean(model2_train$diff_A, na.rm=TRUE)
model2_train %>% autoplot(diff_A) +
  geom_point(aes(y=diff_A, color=Quarter)) +
  geom_hline(aes(yintercept=mean_diff_A), lty=2) +
  scale_color_manual(values = Quarter_colors) +
  theme_classic() +
  ggtitle("First Differences of Seasonally-Adjusted Revenue vs. Time") + xlab("Time") + ylab("diff_A")
#
#   Compute autocorrelation function of diff_A
#
model2_train %>% ACF(diff_A) %>% autoplot() +
  ggtitle("ACF Plot of the First Differences")
#
#   Compute KPSS test for nonstationary data
#
unitroot_kpss(model2_train$diff_A)
#
#   Use ARIMA to select best ARIMA(p,d,q) model for A
#
result_ARIMA_A <- model2_train %>% 
  model(ARIMA(A ~ PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(result_ARIMA_A)
#
#   Check the assumptions
#
result_ARIMA_A %>% gg_tsresiduals()
result_ARIMA_A_augment <- augment(result_ARIMA_A)
ad.test(result_ARIMA_A_augment$.resid)

#
#   Compute forecasts for A
#
result_ARIMA_A_forecast <- result_ARIMA_A %>% forecast(h=6)
forecast_A <- result_ARIMA_A_forecast$.mean
forecast_A
#
#   Print and store forecast variances for A
#
var_forecast_errors_A <- distributional::variance(result_ARIMA_A_forecast$A)
var_forecast_errors_A
#
#   Compute forecasts for season
#
result_ARIMA_season <- model2_train %>% 
  model(ARIMA(season ~ pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)
result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=6)
forecast_season <- result_ARIMA_season_forecast$.mean
forecast_season

#   Compute in-sample forecasts for Revenue
#
result_ARIMA_A_augment <- augment(result_ARIMA_A)
result_ARIMA_season_augment <- augment(result_ARIMA_season)
model2_train$insample_Revenue <- result_ARIMA_A_augment$.fitted + 
  result_ARIMA_season_augment$.fitted

#   Forecast for n_test = 24 months and incorporate into data_table_ts_test
#
result_ARIMA_A_forecast <- result_ARIMA_A %>% forecast(h=n_test)
result_ARIMA_A_forecast$.mean
model2_test <- model2_test %>% 
  add_column(Fcast_A = result_ARIMA_A_forecast$.mean)
#
#   Incorporate forecasts of next 24 months of seasonal factors
#
model2_test <- model2_test %>% 
  add_column(Fcast_season = forecast_season)

#   Compute forecast of log(Homes) and Homes for the test data set
#
model2_test <- model2_test %>% 
  mutate(Fcast_Revenue = Fcast_A + Fcast_season)
print(model2_test)
#

#   Plot in-sample and out-of-sample forecasts
#
model2_train %>% autoplot(Revenue) +
  geom_line(aes(y=insample_Revenue), color="red") +
  geom_line(data = model2_test, aes(x=qtr,  y=Fcast_Revenue), color="blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Model 2 In-sample Forecast") + xlab("Time") + ylab("Revenue")
#

#
#   Compute root mean squared error
#
model2_test <- model2_test %>% mutate(Error = Revenue - Fcast_Revenue)
print(model2_test,n=6)
RMSE <- sqrt(sum(model2_test$Error^2)/6)
RMSE
#
#   Compute mean absolute error
#
MAE <- sum(abs(model2_test$Error)/6)
MAE


### FINAL MODEL ###

# Seasonally adjust the entire data set (including the test points)
final_model_ts <- data_table_ts

Revenue_time_series <- ts(final_model_ts$Revenue, frequency=4)
components <- stlplus(Revenue_time_series, s.window=11, robust=TRUE)
final_model_ts$season <- components$data$seasonal
final_model_ts$A <- components$data$raw - components$data$seasonal

#   Use ARIMA to select best ARIMA(p,d,q) model for A
#

result_ARIMA_A <- final_model_ts %>% 
  model(ARIMA(A ~ 1 + pdq(0, 1, 0) + PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(result_ARIMA_A)

#
#   Check the assumptions
#
result_ARIMA_A %>% gg_tsresiduals()
result_ARIMA_A_augment <- augment(result_ARIMA_A)
ad.test(result_ARIMA_A_augment$.resid)

#
#   Compute forecasts for A
#
result_ARIMA_A_forecast <- result_ARIMA_A %>% forecast(h=4)
forecast_A <- result_ARIMA_A_forecast$.mean
forecast_A
#
#   Print and store forecast variances for A
#
var_forecast_errors_A <- distributional::variance(result_ARIMA_A_forecast$A)
var_forecast_errors_A
#
#   Compute forecasts for season
#
result_ARIMA_season <- final_model_ts %>% 
  model(ARIMA(season ~ pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)
result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=4)
forecast_season <- result_ARIMA_season_forecast$.mean
forecast_season

#   Compute in-sample forecasts for Revenue
#
result_ARIMA_A_augment <- augment(result_ARIMA_A)
result_ARIMA_season_augment <- augment(result_ARIMA_season)
final_model_ts$insample_Revenue <- result_ARIMA_A_augment$.fitted + 
  result_ARIMA_season_augment$.fitted


#   Compute forecast of log(Homes) and Homes for the test data set
#
result_ARIMA_A_forecast$season <- forecast_season
result_ARIMA_A_forecast <- result_ARIMA_A_forecast %>% 
  mutate(Fcast_Revenue = .mean + season)
print(result_ARIMA_A_forecast)

#
#   Print and store forecast variances for season
#
var_forecast_errors_season <- distributional::variance(result_ARIMA_season_forecast$season)
var_forecast_errors_season
#
#   Compute correlation between epsilon(t) and a(t)
#
result_ARIMA_A_augment <- augment(result_ARIMA_A)
result_ARIMA_season_augment <- augment(result_ARIMA_season)
correlation <- cor(result_ARIMA_A_augment$.resid, 
                   result_ARIMA_season_augment$.resid, use = 'complete') 
correlation
#
#   Compute variances for prediction intervals for Revenue
#
covariance <- sqrt(var_forecast_errors_A) * sqrt(var_forecast_errors_season) * correlation
variance <- var_forecast_errors_A + var_forecast_errors_season + 2 * covariance
variance
#
#   Compute prediction intervals for Revenue
#
forecast_Revenue <- forecast_A + forecast_season
forecast_Revenue
z_value <- qnorm(0.975, 0, 1)
PI_low_Revenue <- forecast_Revenue - z_value * sqrt(variance)
PI_up_Revenue  <- forecast_Revenue + z_value * sqrt(variance)
PI_low_Revenue
PI_up_Revenue

#
#   Create tsibble for plotting out-of-sample forecasts
#
forecast_ts <- tsibble(Quarter_index = yearquarter("2025 Q1") + 0:3,
                       forecast_Revenue = forecast_Revenue,
                       PI_low_Revenue = PI_low_Revenue,
                       PI_up_Revenue = PI_up_Revenue,
                       index = Quarter_index)
head(forecast_ts)
#
#   Plot in-sample and out-of-sample forecasts
#
final_model_ts %>% autoplot(Revenue) +
  geom_line(data=final_model_ts,aes( y=insample_Revenue), color="red") +
  geom_line(data = forecast_ts, aes(x=Quarter_index,  y=forecast_Revenue), color="blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Revenue vs. Time") + xlab("Time") + ylab("Revenue")
#



# fix this!
#   Focus plot on out-of-sample forecasts and prediction intervals
#
final_model_ts[50:60,] %>% autoplot(Revenue) +
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



