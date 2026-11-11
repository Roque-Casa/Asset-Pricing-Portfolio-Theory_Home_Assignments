# Libraries and data import
library(dplyr)

monthly_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_monthly_HA2-6.csv"
)

monthly_cleaned$Date <- as.Date(monthly_cleaned$Date)

# Define stock columns
stocks <- c(
  "APPLE",
  "MICROSOFT",
  "TESLA",
  "JP.MORGAN.CHASE...CO.",
  "WALMART"
)

tri_cols <- paste0(stocks, "...TOT.RETURN.IND")
cap_cols <- paste0(stocks, "...MARKET.VALUE")


## ---- exercise-1 ----
# Exercise 1

# Data preparation for Exercise 1
data_1 <- monthly_cleaned %>%
  select(Date, all_of(tri_cols), all_of(cap_cols)) %>%
  arrange(Date)

returns_1 <- data_1 %>%
  select(all_of(tri_cols)) %>%
  mutate(across(everything(), ~ .x / lag(.x) - 1))

caps_1 <- data_1 %>%
  select(all_of(cap_cols)) %>%
  mutate(across(everything(), ~ lag(.x)))

portfolio_data_1 <- data.frame(
  Date = data_1$Date,
  returns_1,
  caps_1
) %>%
  filter(
    Date >= as.Date("2011-01-01"),
    Date <= as.Date("2021-12-31")
  )

# Portfolio calculations
cap_weights_1 <- function(caps_1, max_weight_1 = 0.35) {
  
  weights_1 <- numeric(length(caps_1))
  remaining_1 <- seq_along(caps_1)
  
  while (length(remaining_1) > 0) {
    available_weight_1 <- 1 - sum(weights_1)
    new_weights_1 <- available_weight_1 *
      caps_1[remaining_1] / sum(caps_1[remaining_1])
    over_limit_1 <- new_weights_1 > max_weight_1
    
    if (!any(over_limit_1)) {
      weights_1[remaining_1] <- new_weights_1
      break
    }

    weights_1[remaining_1[over_limit_1]] <- max_weight_1
    remaining_1 <- remaining_1[!over_limit_1]
  }
  
  return(weights_1)
}

# Calculate monthly portfolio returns
portfolio_returns_1 <- numeric(nrow(portfolio_data_1))
portfolio_weights_1 <- matrix(
  NA_real_,
  nrow = nrow(portfolio_data_1),
  ncol = length(stocks)
)

for (i in seq_len(nrow(portfolio_data_1))) {
  
  stock_returns_1 <- as.numeric(portfolio_data_1[i, 2:6])
  
  market_caps_1 <- as.numeric(portfolio_data_1[i, 7:11])
  
  weights_1 <- cap_weights_1(market_caps_1)
  
  portfolio_weights_1[i, ] <- weights_1
  portfolio_returns_1[i] <- sum(weights_1 * stock_returns_1)
}

# Calculate mean return
mean_monthly_return_1 <- mean(portfolio_returns_1)

mean_annual_return_1 <- (
  (1 + mean_monthly_return_1)^12 - 1
) * 100

# Results
#print(mean_monthly_return_1 * 100)
print(mean_annual_return_1)
#print(length(portfolio_returns_1))



## ---- exercise-2 ----
# Exercise 2

# Use monthly returns from Exercise 1
returns_matrix_2 <- as.matrix(
  portfolio_data_1[, 2:6]
)

# Calculation
mean_returns_2 <- colMeans(returns_matrix_2)
Sigma_2 <- cov(returns_matrix_2)

ones_2 <- rep(1, length(mean_returns_2))

weights_gmv_2 <- as.numeric(
  solve(Sigma_2, ones_2) /
    sum(solve(Sigma_2, ones_2))
)

mean_monthly_return_2 <- sum(
  weights_gmv_2 * mean_returns_2
)

volatility_monthly_2 <- sqrt(
  as.numeric(t(weights_gmv_2) %*% Sigma_2 %*% weights_gmv_2)
)

mean_annual_return_2 <- (
  (1 + mean_monthly_return_2)^12 - 1
) * 100

# Results
#print(weights_gmv_2)
#print(mean_monthly_return_2 * 100)
#print(volatility_monthly_2 * 100)
print(mean_annual_return_2)


## ---- exercise-3 ----
# Exercise 3

# Use data from Exercise 2
mean_returns_3 <- mean_returns_2
Sigma_3 <- Sigma_2
ones_3 <- rep(1, length(mean_returns_3))

# Find the risk-free rate column (AQ)
rf_col_3 <- grep(
  "US.DOLLAR.*MIDDLE.RATE",
  names(monthly_cleaned),
  value = TRUE
)

rf_annual_3 <- monthly_cleaned %>%
  filter(Date == as.Date("2021-12-31")) %>%
  pull(all_of(rf_col_3))

# Convert annual percentage rate to monthly decimal
rf_monthly_3 <- rf_annual_3 / 100 / 12

target_return_3 <- 0.01

# Calculate A, B, C and K squared
Sigma_inv_R_3 <- solve(Sigma_3, mean_returns_3)
Sigma_inv_ones_3 <- solve(Sigma_3, ones_3)

A_3 <- as.numeric(t(mean_returns_3) %*% Sigma_inv_R_3)
B_3 <- as.numeric(t(ones_3) %*% Sigma_inv_R_3)
C_3 <- as.numeric(t(ones_3) %*% Sigma_inv_ones_3)

K2_3 <- A_3 - 2 * B_3 * rf_monthly_3 +
  C_3 * rf_monthly_3^2

# Calculate weights
weights_cml_3 <- as.numeric(
  solve(Sigma_3, mean_returns_3 - rf_monthly_3 * ones_3) *
    (target_return_3 - rf_monthly_3) / K2_3
)

weight_rf_3 <- 1 - sum(weights_cml_3)

# Calculate expected monthly return
expected_return_3 <- sum(weights_cml_3 * mean_returns_3) +
  weight_rf_3 * rf_monthly_3

# Calculate expected monthly volatility
volatility_monthly_3 <- sqrt(
  as.numeric(t(weights_cml_3) %*% Sigma_3 %*% weights_cml_3)
)

# Results
#print(rf_monthly_3 * 100)
#print(weights_cml_3)
#print(weight_rf_3)
#print(expected_return_3 * 100)
print(volatility_monthly_3 * 100)


## ---- exercise-4 ----
# Exercise 4

# Combine dates and monthly returns from Exercise 1
returns_data_4 <- data.frame(
  Date = data_1$Date,
  returns_1
)

# Investment months: February to October 2022
investment_months_4 <- seq(
  as.Date("2022-02-01"),
  as.Date("2022-10-01"),
  by = "month"
)

# Store monthly weights and expected volatilities
portfolio_weights_4 <- matrix(
  NA_real_,
  nrow = length(investment_months_4),
  ncol = length(stocks)
)

rolling_results_4 <- data.frame()

for (i in seq_along(investment_months_4)) {
  
  # Select the most recent 132 monthly returns
  window_4 <- returns_data_4 %>%
    filter(Date < investment_months_4[i]) %>%
    tail(132)
  
  # Mean returns and sample covariance matrix
  mean_returns_4 <- colMeans(window_4[, -1])
  Sigma_4 <- cov(window_4[, -1])
  ones_4 <- rep(1, length(mean_returns_4))
  
  # Risk-free rate
  previous_date_4 <- max(window_4$Date)
  
  rf_annual_4 <- monthly_cleaned %>%
    filter(Date == previous_date_4) %>%
    pull(all_of(rf_col_3))
  
  rf_monthly_4 <- rf_annual_4 / 100 / 12
  
  # Calculate A, B, C and K squared
  Sigma_inv_R_4 <- solve(Sigma_4, mean_returns_4)
  Sigma_inv_ones_4 <- solve(Sigma_4, ones_4)
  
  A_4 <- as.numeric(t(mean_returns_4) %*% Sigma_inv_R_4)
  B_4 <- as.numeric(t(ones_4) %*% Sigma_inv_R_4)
  C_4 <- as.numeric(t(ones_4) %*% Sigma_inv_ones_4)
  
  K2_4 <- A_4 - 2 * B_4 * rf_monthly_4 +
    C_4 * rf_monthly_4^2
  
  # Calculate weights
  weights_4 <- as.numeric(
    solve(Sigma_4, mean_returns_4 - rf_monthly_4 * ones_4) *
      (target_return_3 - rf_monthly_4) / K2_4
  )
  
  # Calculate expected monthly volatility
  volatility_4 <- sqrt(
    as.numeric(t(weights_4) %*% Sigma_4 %*% weights_4)
  )
  
  # Store weights and volatility
  portfolio_weights_4[i, ] <- weights_4
  
  rolling_results_4 <- rbind(
    rolling_results_4,
    data.frame(
      Date = investment_months_4[i],
      Expected_Volatility = volatility_4
    )
  )
}

# Include January result from Exercise 3
rolling_results_4 <- rbind(
  data.frame(
    Date = as.Date("2022-01-31"),
    Expected_Volatility = volatility_monthly_3
  ),
  rolling_results_4
)

# Include January portfolio weights from Exercise 3
portfolio_weights_4 <- rbind(
  weights_cml_3,
  portfolio_weights_4
)

# Calculate average expected monthly volatility
average_volatility_4 <- mean(
  rolling_results_4$Expected_Volatility
) * 100

# Results
print(rolling_results_4)
print(average_volatility_4)


## ---- exercise-5 ----
# Exercise 5

# Calculate realised portfolio returns
# Results


## ---- exercise-6 ----
# Exercise 6

# Compare Sharpe ratios
# Results