# ---------------------------
# VAR Forecasting Project
# inputs: FRED data:
#             - industrial production
#             - core cpi
#             - interest rates (1yr yield or fed funds rate)
# output: 12 month ahead forecast for inflation given expected rate cut
# ------------------------------

# SETUP: read in data and clean ####

#install.packages('fredr')
#detach("package:BVAR", unload = TRUE)
library(fredr)
library(dplyr)

fredr_set_key("YOUR API KEY")

# Pull Industrial Production Index
ip <- fredr(
  series_id = "INDPRO",
  observation_start = as.Date("2000-01-01"),  # Adjust start date as needed
  observation_end = as.Date(Sys.Date())
)

# Pull Core CPI (CPI less food and energy)
core_cpi <- fredr(
  series_id = "CPILFESL",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date(Sys.Date())
)

# Pull Effective Federal Funds Rate
effr <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date(Sys.Date())
)

# Add December row to IP using November's value
last_ip_date <- max(ip$date)
last_ip_value <- ip$value[ip$date == last_ip_date]

# Create December observation
ip_dec <- data.frame(
  date = as.Date("2025-12-01"),
  series_id = "INDPRO",
  value = last_ip_value
)

# Add to IP data
ip <- bind_rows(ip, ip_dec)

# Check for NA in core_cpi
core_cpi %>% filter(date >= as.Date("2025-09-01"))

# Find the September value
sept_cpi <- core_cpi$value[core_cpi$date == as.Date("2025-09-01")]

# Replace the October NA with September's value
core_cpi <- core_cpi %>%
  mutate(value = ifelse(date == as.Date("2025-10-01") & is.na(value), 
                        sept_cpi, 
                        value))

# Verify it's fixed
core_cpi %>% filter(date >= as.Date("2025-09-01"))


# Merge the datasets
data <- ip %>%
  select(date, ip = value) %>%
  left_join(core_cpi %>% select(date, core_cpi = value), by = "date") %>%
  left_join(effr %>% select(date, effr = value), by = "date")

# Create log-first differences
data <- data %>%
  arrange(date) %>%
  mutate(
    d_log_ip = c(NA, diff(log(ip))),
    d_log_core_cpi = c(NA, diff(log(core_cpi)))
  ) %>%
  # Keep effr in levels
  select(date, d_log_ip, d_log_core_cpi, effr)

# Remove the first row which will have NAs due to differencing
data <- data %>%
  filter(!is.na(d_log_ip))


# Create expected path for interest rates ----
current_ffr <- tail(data$effr, 1)
forecast_horizon <- 12  # Adjust based on your needs

# Create the conditional path for fed funds rate
# Assuming current rate is around 4.33% and you expect two 25bp cuts

# Create a vector for the conditional path
# Months 1-5: No change (Jan-May 2025, assuming last data is Dec 2024)
# Month 6: First cut in June (-0.25%)
# Months 7-8: Stays at new level (Jul-Aug)
# Month 9: Second cut in September (-0.25%)
# Months 10-12: Stays at final level (Oct-Dec)

conditional_path <- numeric(forecast_horizon)
conditional_path[1:5] <- current_ffr  # Jan-May: no change
conditional_path[6:7] <- current_ffr - 0.25  # Jun-Aug: after first cut
conditional_path[8:12] <- current_ffr - 0.50  # Sep-Dec: after second cut

# Create condition matrix
cond_matrix <- matrix(NA, nrow = forecast_horizon, ncol = 3)
# Column 3 is effr, set the conditional path
cond_matrix[, 3] <- conditional_path

# VAR ####
#install.packages('vars')
#install.packages('BVAR')
library(BVAR)

# Only select the numeric columns (exclude date)
bvar_data <- as.matrix(data[, c("d_log_ip", "d_log_core_cpi", "effr")])


## TEST different lags ####


# Test different lag lengths with pseudo out-of-sample forecasts
# Hold out last 24 months as test set
# train_data <- bvar_data[1:(nrow(bvar_data)-24), ]
# test_data <- bvar_data[(nrow(bvar_data)-23):nrow(bvar_data), ]

# Function to compute forecast errors
# test_lags <- c(2, 4, 6, 12)
# rmse_results <- data.frame(lags = test_lags, 
#                            rmse_ip = NA, 
#                            rmse_cpi = NA, 
#                            rmse_effr = NA)
# 
# for(i in 1:length(test_lags)) {
#   # Estimate model on training data
#   model_test <- bvar(train_data, 
#                      lags = test_lags[i],
#                      n_draw = 5000,
#                      n_burn = 2500)
#   
#   # Forecast 24 months ahead
#   fcast_test <- predict(model_test, horizon = 24)
#   fcast_mean_test <- apply(fcast_test$fcast, c(2,3), mean)
#   
#   # Calculate RMSE for each variable
#   rmse_results$rmse_ip[i] <- sqrt(mean((fcast_mean_test[,1] - test_data[,1])^2))
#   rmse_results$rmse_cpi[i] <- sqrt(mean((fcast_mean_test[,2] - test_data[,2])^2))
#   rmse_results$rmse_effr[i] <- sqrt(mean((fcast_mean_test[,3] - test_data[,3])^2))
# }
# 
# print(rmse_results)
# Choose the lag with lowest RMSE for your variable of interest (CPI) - 2 LAGS



# ESTIMATE BVAR model ####
set.seed(123)
bvar_model <- bvar(bvar_data, 
                   lags = 6,
                   n_draw = 10000,
                   n_burn = 5000)

# conditional forecast
cond_forecast <- predict(bvar_model, 
                         horizon = forecast_horizon,
                         cond_path = cond_matrix)

# View results
plot(cond_forecast)
summary(cond_forecast)

# Mean forecasts
fcast_mean <- cond_forecast$fcast

# Check results ####


# Get all 12 months of forecasted m/m inflation (mean across iterations)
monthly_inflation <- apply(fcast_mean[, , 2], 2, mean)

# Calculate year-over-year inflation by summing the 12 monthly log-differences
yoy_inflation_dec2025 <- sum(monthly_inflation)
yoy_inflation_pct <- yoy_inflation_dec2025 * 100

print(paste("Forecasted y/y inflation for Dec 2026:", round(yoy_inflation_pct, 2), "%"))



# Get all 12 months of forecasted m/m IP growth (mean across iterations)
# monthly_ip_growth <- apply(fcast_mean[, , 1], 2, mean)
# 
# # Calculate year-over-year IP growth by summing the 12 monthly log-differences
# yoy_ip_growth <- sum(monthly_ip_growth)
# yoy_ip_growth_pct <- yoy_ip_growth * 100
# 
# print(paste("Forecasted y/y IP growth for Dec 2025:", round(yoy_ip_growth_pct, 2), "%"))




