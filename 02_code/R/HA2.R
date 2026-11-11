# Home-Assignement 2

# Import datasets

library(dplyr)

# Import cleaned daily, monthly and US CPI data
daily_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_daily_HA2-6.csv"
)

monthly_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_monthly_HA2-6.csv"
)

us_cpi_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_us_cpi_HA2-6.csv"
)

# Convert date columns to Date format
daily_cleaned$Date <- as.Date(daily_cleaned$Date)
monthly_cleaned$Date <- as.Date(monthly_cleaned$Date)
us_cpi_cleaned$Date <- as.Date(us_cpi_cleaned$Date)



## ---- exercise-1 ----
# Exercise 1

# Filter daily data to include only observations up to the end of 2022 
daily_data_2022 <- daily_cleaned %>%
  filter(!is.na(Date), Date <= as.Date("2022-12-31")) %>%
  arrange(Date)

stocks <- c(
  "APPLE",
  "MICROSOFT",
  "TESLA",
  "JP.MORGAN.CHASE...CO.",
  "WALMART"
)

stock_names <- c(
  "apple",
  "microsoft",
  "tesla",
  "jp_morgan_chase",
  "walmart"
)


# Calculate simple daily returns
for (i in seq_along(stocks)) {
  
  daily_returns <- daily_data_2022 %>%
    mutate(Return = .data[[stocks[i]]] / lag(.data[[stocks[i]]]) - 1) %>%
    select(Date, Return)
  
  assign(paste0(stock_names[i], "_daily_returns"), daily_returns)
}

# Jarque-Bera test
library(tseries)

jb_apple <- jarque.bera.test(na.omit(apple_daily_returns$Return))
jb_microsoft <- jarque.bera.test(na.omit(microsoft_daily_returns$Return))
jb_tesla <- jarque.bera.test(na.omit(tesla_daily_returns$Return))
jb_jp_morgan <- jarque.bera.test(na.omit(jp_morgan_chase_daily_returns$Return))
jb_walmart <- jarque.bera.test(na.omit(walmart_daily_returns$Return))

# Display test statistics and p-values
jb_results <- data.frame(
  Stock = c("Apple", "Microsoft", "Tesla", "JP Morgan Chase", "Walmart"),
  JB_Statistic = round(c(
    jb_apple$statistic,
    jb_microsoft$statistic,
    jb_tesla$statistic,
    jb_jp_morgan$statistic,
    jb_walmart$statistic
  ), 2),
  P_Value = format.pval(c(
    jb_apple$p.value,
    jb_microsoft$p.value,
    jb_tesla$p.value,
    jb_jp_morgan$p.value,
    jb_walmart$p.value
  ), digits = 3, eps = 2.2e-16)
)

print(jb_results, row.names = FALSE)



## ---- exercise-2 ----
# Exercise 2

# Calculate average daily bid-ask spreads in percent
avg_spreads <- numeric(length(stocks))

for (i in seq_along(stocks)) {
  
  ask <- daily_data_2022[[paste0(stocks[i], "...ASK.PRICE")]]
  bid <- daily_data_2022[[paste0(stocks[i], "...BID.PRICE")]]
  
  spread <- (ask - bid) / ((ask + bid) / 2) * 100
  
  avg_spreads[i] <- mean(spread, na.rm = TRUE)
}

# Display results
spread_results <- data.frame(
  Stock = stock_names,
  Average_Spread_Percent = round(avg_spreads, 4)
)

print(spread_results)



## ---- exercise-3 ----
# Exercise 3

# Filter monthly data to include only observations up to the end of 2022
monthly_data_2022 <- monthly_cleaned %>%
  filter(!is.na(Date), Date <= as.Date("2022-12-31")) %>%
  arrange(Date)

# Calculate simple monthly returns
for (i in seq_along(stocks)) {
  
  monthly_returns <- monthly_data_2022 %>%
    mutate(Return = .data[[stocks[i]]] / lag(.data[[stocks[i]]]) - 1) %>%
    select(Date, Return)
  
  assign(paste0(stock_names[i], "_monthly_returns"), monthly_returns)
}

# Calculate average monthly and annual returns
avg_monthly_returns <- numeric(length(stocks))

for (i in seq_along(stocks)) {
  
  help_returns <- get(paste0(stock_names[i], "_monthly_returns"))$Return
  
  avg_monthly_returns[i] <- mean(help_returns, na.rm = TRUE)
}

# Create results table
return_results <- data.frame(
  Stock = stocks,
  Monthly_Return = avg_monthly_returns * 100,
  Annual_Return = avg_monthly_returns * 12 * 100
)

# Calculate annual return difference in percentage points
return_difference <- max(return_results$Annual_Return) -
  min(return_results$Annual_Return)

print(return_difference)



## ---- exercise-4 ----
# Exercise 4

# Combine monthly returns of all five stocks
monthly_returns_combined <- data.frame(
  Date = apple_monthly_returns$Date,
  Apple = apple_monthly_returns$Return,
  Microsoft = microsoft_monthly_returns$Return,
  Tesla = tesla_monthly_returns$Return,
  JP_Morgan = jp_morgan_chase_monthly_returns$Return,
  Walmart = walmart_monthly_returns$Return
)

# Remove months with missing returns
monthly_returns_combined <- na.omit(monthly_returns_combined)

# Select the best and worst stock return each month
best_monthly_returns <- apply(
  monthly_returns_combined[, -1], 1, max
)

worst_monthly_returns <- apply(
  monthly_returns_combined[, -1], 1, min
)

# Calculate average monthly and annualized returns
exercise_4_results <- data.frame(
  Strategy = c("Best Stock", "Worst Stock"),
  Monthly_Return = c(
    mean(best_monthly_returns),
    mean(worst_monthly_returns)
  ) * 100,
  Annual_Return = c(
    mean(best_monthly_returns),
    mean(worst_monthly_returns)
  ) * 12 * 100
)

# Calculate annual return difference in percentage points
return_difference_4 <- diff(
  rev(exercise_4_results$Annual_Return)
)

# Display results
print(exercise_4_results)
print(return_difference_4)



## ---- exercise-5 ----
# Exercise 5

# Calculate equally weighted portfolio returns (20% per stock)
equal_weighted_returns <- monthly_returns_combined %>%
  mutate(Portfolio_Return = rowMeans(select(., -Date)))

# Calculate average monthly and annualized portfolio returns
equal_weighted_monthly_mean <- mean(equal_weighted_returns$Portfolio_Return)

equal_weighted_annual_mean <- equal_weighted_monthly_mean * 12 * 100

# Display result
print(equal_weighted_annual_mean)



## ---- exercise-6 ----
# Exercise 6

# Combine monthly returns and market capitalizations
returns_matrix <- monthly_data_2022 %>%
  select(all_of(stocks)) %>%
  mutate(across(
    everything(),
    ~ .x / lag(.x) - 1
  ))

market_caps <- monthly_data_2022 %>%
  select(all_of(paste0(stocks, "...MARKET.VALUE")))

# Calculate portfolio weights using previous month's market capitalization
market_caps_lagged <- market_caps %>%
  mutate(across(everything(), ~ lag(.x)))

# Calculate monthly value-weighted portfolio returns
value_weighted_returns <- numeric(nrow(monthly_data_2022))

for (i in seq_len(nrow(monthly_data_2022))) {
  
  help_6_returns <- as.numeric(returns_matrix[i, ])
  caps <- as.numeric(market_caps_lagged[i, ])
  
  if (all(is.finite(help_6_returns)) &&
      all(is.finite(caps)) &&
      all(caps > 0)) {
    
    weights <- caps / sum(caps)
    value_weighted_returns[i] <- sum(weights * help_6_returns)
    
  } else {
    value_weighted_returns[i] <- NA_real_
  }
}

# Calculate average monthly and annualized returns
value_weighted_monthly_mean <- mean(value_weighted_returns, na.rm = TRUE)
value_weighted_annual_mean <- value_weighted_monthly_mean * 12 * 100

# Display result
print(value_weighted_annual_mean)



## ---- exercise-7 ----
# Exercise 7

# Select monthly stock prices
prices <- monthly_data_2022 %>%
  select(Date, all_of(stocks)) %>%
  filter(if_all(all_of(stocks), ~ !is.na(.x) & .x > 0)) %>%
  arrange(Date)

# Invest 20% in each stock at the beginning of the period
initial_weights <- rep(0.2, length(stocks))

# Calculate the number of shares purchased initially
initial_prices <- as.numeric(prices[1, stocks])
shares <- initial_weights / initial_prices

# Calculate portfolio value over time without rebalancing
portfolio_values <- as.matrix(prices[stocks]) %*% shares

# Calculate simple monthly portfolio returns
buy_hold_returns <- data.frame(
  Date = prices$Date,
  Return = as.numeric(portfolio_values / dplyr::lag(as.numeric(portfolio_values)) - 1)
)

# Calculate average monthly and annualized returns
buy_hold_monthly_mean <- mean(buy_hold_returns$Return, na.rm = TRUE)
buy_hold_annual_mean <- buy_hold_monthly_mean * 12 * 100

# Display result
print(buy_hold_annual_mean)



## ---- exercise-9 ----
# Exercise 9: Capital Allocation Line

# Monthly returns of all five stocks
monthly_returns_matrix <- as.matrix(
  monthly_returns_combined[, -1]
)

# Mean-return vector and sample covariance matrix
R <- colMeans(monthly_returns_matrix)
Sigma <- cov(monthly_returns_matrix)

# Risk-free rate and vector of ones
Rf <- 0.002
ones <- rep(1, length(R))

# Calculate A, B, C and K squared
Sigma_inv_R <- solve(Sigma, R)
Sigma_inv_ones <- solve(Sigma, ones)

A <- as.numeric(t(R) %*% Sigma_inv_R)
B <- as.numeric(t(ones) %*% Sigma_inv_R)
C <- as.numeric(t(ones) %*% Sigma_inv_ones)

K2 <- A - 2 * B * Rf + C * Rf^2

# Target monthly returns: 0% to 2% in steps of 0.1%
target_returns <- seq(0, 0.02, by = 0.001)

# Calculate CAL weights and volatilities
cal_results <- data.frame()

for (mu_p in target_returns) {
  
  # Weights of the five risky assets
  weights <- as.numeric(
    solve(Sigma, R - Rf * ones) *
      (mu_p - Rf) / K2
  )
  
  # Volatility calculated from portfolio weights
  variance_weights <- as.numeric(
    t(weights) %*% Sigma %*% weights
  )
  
  # Volatility calculated using the direct formula
  variance_direct <- (mu_p - Rf)^2 / K2
  
  # Store results
  cal_results <- rbind(cal_results, data.frame(
    Target_Return = mu_p * 100,
    Apple = weights[1],
    Microsoft = weights[2],
    Tesla = weights[3],
    JP_Morgan = weights[4],
    Walmart = weights[5],
    Sum_Weights = sum(weights),
    Risk_Free_Weight = 1 - sum(weights),
    Volatility_Weights = sqrt(variance_weights) * 100,
    Volatility_Direct = sqrt(variance_direct) * 100
  ))
}

# Check whether both volatility calculations agree
volatility_difference <- max(abs(
  cal_results$Volatility_Weights -
    cal_results$Volatility_Direct
))

# Results for a target monthly return of 1%
cal_result_1pct <- cal_results[
  which.min(abs(cal_results$Target_Return - 1)), 
]

cal_weight_sum <- cal_result_1pct$Sum_Weights

# Display results
print(cal_results)
print(cal_result_1pct)
print(cal_weight_sum)
print(volatility_difference)



## ---- exercise-10 ----
# Exercise 10: Minimum-Variance Frontier

# Calculate denominator
D <- A * C - B^2

# Define target monthly returns for the frontier
target_returns_10 <- seq(-0.01, 0.03, by = 0.0001)

# Calculate minimum-variance volatility
min_var_volatility <- sqrt(
  (C * target_returns_10^2 -
     2 * B * target_returns_10 + A) / D
)

# Target monthly return of 0.30%
target_return_10 <- 0.003

# Calculate minimum-variance portfolio weights
weights_min_var <- as.numeric(
  solve(Sigma,
        ((C * target_return_10 - B) / D) * R +
          ((A - B * target_return_10) / D) * ones)
)

# Calculate volatility using portfolio weights
min_var_variance <- as.numeric(
  t(weights_min_var) %*% Sigma %*% weights_min_var
)

min_var_volatility_03 <- sqrt(min_var_variance) * 100

# Check portfolio constraints
sum_weights_10 <- sum(weights_min_var)
portfolio_return_10 <- sum(weights_min_var * R)

# Plot minimum-variance frontier
plot(
  min_var_volatility * 100,
  target_returns_10 * 100,
  type = "l",
  lwd = 2,
  xlab = "Monthly Volatility (%)",
  ylab = "Expected Monthly Return (%)",
  main = "Minimum-Variance Frontier"
)

# Highlight target portfolio
points(
  min_var_volatility_03,
  target_return_10 * 100,
  col = "red",
  pch = 19,
  cex = 1.3
)

# Display results
print(weights_min_var)
print(sum_weights_10)
print(portfolio_return_10)
print(min_var_volatility_03)