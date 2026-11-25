# Libraries
library(dplyr)


# ---- US Market Data ----

# Import monthly stock and market data
monthly_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_monthly_HA2-6.csv"
)

monthly_cleaned$Date <- as.Date(monthly_cleaned$Date)

# Import US CPI data
us_cpi_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_us_cpi_HA2-6.csv"
)

us_cpi_cleaned$Date <- as.Date(us_cpi_cleaned$Date)


# ---- Treynor-Black Data ----

# Import Treynor-Black model parameters
treynor_black_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_treynor_black.csv"
)


# ---- Black-Litterman Data ----

# Import asset information and market weights
blm_case_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_blm_case.csv"
)

# Import covariance matrix
blm_cov_cleaned <- as.matrix(
  read.csv(
    "01_data/cleaned/data_cleaned_blm_covariance.csv",
    row.names = 1,
    check.names = FALSE
  )
)

# Import Black-Litterman model parameters
blm_parameters_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_blm_parameters.csv"
)

# Import monthly Black-Litterman data
blm_data_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_blm_monthly.csv"
)

blm_data_cleaned$Date <- as.Date(blm_data_cleaned$Date)


# ---- Define Stock and Market Columns ----

stocks <- c(
  "APPLE",
  "MICROSOFT",
  "TESLA",
  "JP.MORGAN.CHASE...CO.",
  "WALMART"
)

tri_cols <- paste0(stocks, "...TOT.RETURN.IND")
cap_cols <- paste0(stocks, "...MARKET.VALUE")

# Market index
market_col <- "S.P.500.COMPOSITE...TOT.RETURN.IND"

# Risk-free rate
rf_col <- grep(
  "US.DOLLAR.*MIDDLE.RATE",
  names(monthly_cleaned),
  value = TRUE
)

# Black-Litterman asset names
blm_assets <- blm_case_cleaned$Asset


## ---- exercise-1 ----
# Exercise 1

# Combine stock and market index columns
tri_cols_1 <- c(tri_cols, market_col)

# Calculate simple monthly returns and risk-free rates
data_1 <- monthly_cleaned %>%
  arrange(Date) %>%
  mutate(
    across(
      all_of(tri_cols_1),
      ~ .x / lag(.x) - 1
    ),
    rf_monthly_1 = lag(.data[[rf_col]]) / 100 / 12
  )

# Find CPI column
cpi_col_1 <- setdiff(names(us_cpi_cleaned), "Date")

# Calculate monthly inflation and inflation surprise
inflation_data_1 <- us_cpi_cleaned %>%
  arrange(Date) %>%
  mutate(
    inflation_1 = .data[[cpi_col_1]] /
      lag(.data[[cpi_col_1]]) - 1,
    expected_inflation_1 = lag(inflation_1),
    inflation_surprise_1 = inflation_1 -
      expected_inflation_1,
    Month = format(Date, "%Y-%m")
  ) %>%
  select(
    Month,
    inflation_1,
    expected_inflation_1,
    inflation_surprise_1
  )

# Match stock returns with inflation data
data_1 <- data_1 %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  left_join(
    inflation_data_1,
    by = "Month"
  ) %>%
  filter(
    Date >= as.Date("2011-02-01"),
    Date <= as.Date("2021-12-31")
  )

# Calculate monthly excess returns
excess_returns_1 <- as.matrix(
  data_1[, tri_cols_1]
) - data_1$rf_monthly_1

# Define stock returns and risk factors
stock_excess_1 <- excess_returns_1[, 1:5]
market_excess_1 <- excess_returns_1[, 6]
inflation_surprise_1 <- data_1$inflation_surprise_1

# Store regression models and coefficients
models_1 <- vector("list", length(stocks))
alphas_1 <- numeric(length(stocks))
market_betas_1 <- numeric(length(stocks))
inflation_betas_1 <- numeric(length(stocks))

# Estimate two-factor regressions
for (i in seq_along(stocks)) {
  
  models_1[[i]] <- lm(
    stock_excess_1[, i] ~
      market_excess_1 + inflation_surprise_1
  )
  
  alphas_1[i] <- coef(models_1[[i]])[1]
  market_betas_1[i] <- coef(models_1[[i]])[2]
  inflation_betas_1[i] <- coef(models_1[[i]])[3]
}

# Sum of inflation betas
sum_inflation_betas_1 <- sum(inflation_betas_1)

# Create results table
results_1 <- data.frame(
  Stock = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart"
  ),
  Alpha = alphas_1,
  Market_Beta = market_betas_1,
  Inflation_Beta = inflation_betas_1
)

# Results
print(results_1)
print(sum_inflation_betas_1)


summary(inflation_surprise_1)
sd(inflation_surprise_1)
sapply(models_1, nobs)



## ---- exercise-2 ----
# Exercise 2

# Calculate monthly simple returns and risk-free rates
tri_cols_2 <- c(tri_cols, market_col)

data_2 <- monthly_cleaned %>%
  arrange(Date) %>%
  mutate(
    across(
      all_of(tri_cols_2),
      ~ .x / lag(.x) - 1
    ),
    rf_monthly_2 = lag(.data[[rf_col]]) / 100 / 12
  ) %>%
  filter(
    Date >= as.Date("2011-01-01"),
    Date <= as.Date("2021-12-31")
  )

# Calculate monthly excess returns
excess_returns_2 <- as.matrix(
  data_2[, tri_cols_2]
) - data_2$rf_monthly_2

# Separate stock and market excess returns
stock_excess_2 <- excess_returns_2[, 1:5]
market_excess_2 <- excess_returns_2[, 6]

# Define rolling estimation window
window_size_2 <- 60
T_2 <- nrow(excess_returns_2)

# Store rolling betas and gamma coefficients
rolling_betas_2 <- matrix(
  NA_real_,
  nrow = T_2 - window_size_2,
  ncol = length(stocks)
)

gamma0_2 <- numeric(T_2 - window_size_2)
gamma1_2 <- numeric(T_2 - window_size_2)

# Fama-MacBeth rolling regressions
for (i in (window_size_2 + 1):T_2) {
  
  # Select previous 60 monthly observations
  window_2 <- (i - window_size_2):(i - 1)
  
  # First step: Time-series regressions
  betas_2 <- numeric(length(stocks))
  
  for (j in seq_along(stocks)) {
    
    model_2 <- lm(
      stock_excess_2[window_2, j] ~
        market_excess_2[window_2]
    )
    
    betas_2[j] <- coef(model_2)[2]
  }
  
  rolling_betas_2[i - window_size_2, ] <- betas_2
  
  # Second step: Cross-sectional regression
  cross_section_2 <- lm(
    stock_excess_2[i, ] ~ betas_2
  )
  
  gamma0_2[i - window_size_2] <- coef(cross_section_2)[1]
  gamma1_2[i - window_size_2] <- coef(cross_section_2)[2]
}

# Calculate average gamma coefficients
mean_gamma0_2 <- mean(gamma0_2)
mean_gamma1_2 <- mean(gamma1_2)

# Calculate standard errors
se_gamma0_2 <- sd(gamma0_2) / sqrt(length(gamma0_2))
se_gamma1_2 <- sd(gamma1_2) / sqrt(length(gamma1_2))

# Calculate t-statistics
t_gamma0_2 <- mean_gamma0_2 / se_gamma0_2
t_gamma1_2 <- mean_gamma1_2 / se_gamma1_2

# Calculate p-values
p_gamma0_2 <- 2 * pt(
  -abs(t_gamma0_2),
  df = length(gamma0_2) - 1
)

p_gamma1_2 <- 2 * pt(
  -abs(t_gamma1_2),
  df = length(gamma1_2) - 1
)

# Create results table
results_2 <- data.frame(
  Parameter = c("Gamma 0", "Gamma 1"),
  Estimate = c(mean_gamma0_2, mean_gamma1_2),
  Standard_Error = c(se_gamma0_2, se_gamma1_2),
  T_Statistic = c(t_gamma0_2, t_gamma1_2),
  P_Value = c(p_gamma0_2, p_gamma1_2)
)

# Results
print(results_2)
print(mean_gamma1_2)



## ---- exercise-3 ----
# Exercise 3

# Estimate CAPM betas and residual variances as in HA4
betas_3 <- numeric(length(stocks))
residual_variances_3 <- numeric(length(stocks))

for (i in seq_along(stocks)) {
  
  model_3 <- lm(
    stock_excess_2[, i] ~ market_excess_2
  )
  
  betas_3[i] <- coef(model_3)[2]
  
  residual_variances_3[i] <- var(
    residuals(model_3)
  )
}

# Construct monthly covariance matrix as in HA4 Q7
market_variance_3 <- var(market_excess_2)

cov_monthly_3 <- outer(
  betas_3,
  betas_3
) * market_variance_3 +
  diag(residual_variances_3)

# Annualise covariance matrix and residual variances
cov_annual_3 <- cov_monthly_3 * 12

residual_variances_annual_3 <- residual_variances_3 * 12

# Annual risk-free rate at the end of 2021
rf_annual_3 <- monthly_cleaned %>%
  filter(Date == as.Date("2021-12-31")) %>%
  pull(all_of(rf_col)) / 100

# Annual market volatility
market_volatility_3 <- sd(market_excess_2) * sqrt(12)

# Market equity premium from Excel cell E5
market_premium_3 <- treynor_black_cleaned$Value[
  treynor_black_cleaned$Parameter == "Market_Equity_Premium"
]

# Set risk aversion
risk_aversion_3 <- 0.5

# Annual alpha views
alpha_views_3 <- c(
  0.01,
  -0.005,
  0,
  0,
  0.02
)

# Calculate annual expected CAPM returns
expected_returns_3 <- rf_annual_3 +
  betas_3 * market_premium_3

# Calculate optimal risky asset weights
weights_3 <- as.numeric(
  solve(
    cov_annual_3,
    expected_returns_3 - rf_annual_3
  ) / risk_aversion_3
)

# Total fraction invested in risky assets
risky_weight_3 <- sum(weights_3)

# Fraction invested in the risk-free asset
rf_weight_3 <- 1 - risky_weight_3

# Calculate expected portfolio return
portfolio_return_3 <- sum(
  weights_3 * expected_returns_3
) + rf_weight_3 * rf_annual_3

# Calculate annual portfolio volatility
portfolio_volatility_3 <- sqrt(
  as.numeric(
    t(weights_3) %*% cov_annual_3 %*% weights_3
  )
)

# Create results table
results_3 <- data.frame(
  Asset = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart",
    "Risk-Free Asset"
  ),
  Weight = c(weights_3, rf_weight_3) * 100
)

# Results
print(results_3)
print(risky_weight_3)
print(rf_weight_3)



## ---- exercise-4 ----
# Exercise 4

# Calculate weights within the active portfolio
active_weights_4 <- (
  alpha_views_3 / residual_variances_annual_3
) / sum(
  alpha_views_3 / residual_variances_annual_3
)

# Calculate alpha of the active portfolio
active_alpha_4 <- sum(
  active_weights_4 * alpha_views_3
)

# Calculate beta of the active portfolio
active_beta_4 <- sum(
  active_weights_4 * betas_3
)

# Calculate residual variance of the active portfolio
active_residual_variance_4 <- sum(
  active_weights_4^2 * residual_variances_annual_3
)

# Calculate unadjusted active portfolio weight
active_weight_initial_4 <- (
  active_alpha_4 / active_residual_variance_4
) / (
  market_premium_3 / market_volatility_3^2
)

# Adjust active portfolio weight for beta
active_weight_adjusted_4 <- active_weight_initial_4 /
  (1 + (1 - active_beta_4) * active_weight_initial_4)

# Calculate passive portfolio weight
passive_weight_4 <- 1 - active_weight_adjusted_4

# Calculate allocations as fractions of total wealth
active_weight_total_4 <- risky_weight_3 *
  active_weight_adjusted_4

passive_weight_total_4 <- risky_weight_3 *
  passive_weight_4

rf_weight_4 <- rf_weight_3

# Create active portfolio weights table
active_weights_table_4 <- data.frame(
  Stock = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart"
  ),
  Alpha_View = alpha_views_3 * 100,
  Active_Weight = active_weights_4 * 100
)

# Create total wealth allocation table
results_4 <- data.frame(
  Asset = c(
    "Risk-Free Asset",
    "Passive Market Portfolio",
    "Active Portfolio"
  ),
  Weight = c(
    rf_weight_4,
    passive_weight_total_4,
    active_weight_total_4
  ) * 100
)

# Results
print(active_weights_table_4)
print(results_4)
print(active_weight_total_4)



## ---- exercise-5 ----
# Exercise 5

# Import model inputs
market_weights_5 <- blm_case_cleaned$Market_Weight
Sigma_5 <- blm_cov_cleaned

# Extract model parameters
rf_monthly_5 <- blm_parameters_cleaned$Value[
  blm_parameters_cleaned$Parameter == "Risk_Free_Rate_Monthly"
]

tau_5 <- blm_parameters_cleaned$Value[
  blm_parameters_cleaned$Parameter == "Tau"
]

lambda_5 <- 1

# Calculate equilibrium expected returns
Pi_5 <- as.numeric(
  rf_monthly_5 + lambda_5 * Sigma_5 %*% market_weights_5
)

# Define the three investor views
P_5 <- matrix(
  c(
    1,   0,   0,   0,   0,
    0.5, 0.5, -1,  0,   0,
    -1,   0,   0,   0.5, 0.5
  ),
  nrow = 3,
  byrow = TRUE
)

# Convert annual views into monthly views
V_5 <- matrix(
  c(0.10, 0.02, 0.01) / 12,
  ncol = 1
)

# Calculate full Omega matrix
Omega_full_5 <- tau_5 *
  P_5 %*% Sigma_5 %*% t(P_5)

# Retain diagonal elements as in the Excel example
Omega_5 <- diag(diag(Omega_full_5))

# Calculate revised expected returns
expected_returns_5 <- as.numeric(
  Pi_5 +
    Sigma_5 %*% t(P_5) %*%
    solve(
      Omega_5 / tau_5 +
        P_5 %*% Sigma_5 %*% t(P_5)
    ) %*%
    (V_5 - P_5 %*% Pi_5)
)

# Calculate revised covariance matrix
Sigma_revised_5 <- Sigma_5 + tau_5 * (
  Sigma_5 -
    Sigma_5 %*% t(P_5) %*%
    solve(
      tau_5 * P_5 %*% Sigma_5 %*% t(P_5) +
        Omega_5
    ) %*%
    (tau_5 * P_5 %*% Sigma_5)
)

# Revised weights using revised expected returns only
weights_returns_5 <- as.numeric(
  lambda_5 * solve(Sigma_5, expected_returns_5)
)

# Normalise portfolio weights
weights_returns_scaled_5 <- weights_returns_5 /
  sum(weights_returns_5)

# Revised weights using expected returns and covariance
weights_revised_5 <- as.numeric(
  lambda_5 * solve(
    Sigma_revised_5,
    expected_returns_5
  )
)

# Normalise revised portfolio weights
weights_revised_scaled_5 <- weights_revised_5 /
  sum(weights_revised_5)

# Extract revised expected monthly return for Nokia
nokia_return_5 <- expected_returns_5[1]

# Assign matrix names
colnames(P_5) <- blm_assets
rownames(P_5) <- paste("View", 1:3)

rownames(V_5) <- paste("View", 1:3)
colnames(V_5) <- "Monthly Return"

dimnames(Omega_5) <- list(
  paste("View", 1:3),
  paste("View", 1:3)
)

# Create expected returns table
results_5 <- data.frame(
  Asset = blm_assets,
  Equilibrium_Return = Pi_5 * 100,
  Revised_Return = expected_returns_5 * 100
)

# Create portfolio weights table
weights_table_5 <- data.frame(
  Asset = blm_assets,
  Market_Weight = market_weights_5 * 100,
  Revised_Returns = weights_returns_scaled_5 * 100,
  Revised_Returns_Cov = weights_revised_scaled_5 * 100
)

# Results
print(P_5)
print(V_5)
print(Omega_5)
print(results_5)
print(weights_table_5)
print(nokia_return_5)



## ---- exercise-6 ----
# Exercise 6

# Fully invested Black-Litterman portfolio weights
weights_6 <- weights_revised_scaled_5

# Original market capitalisation weights
market_weights_6 <- market_weights_5

# Calculate tactical asset allocation deviations
taa_6 <- weights_6 - market_weights_6

# Extract Kesko's total portfolio weight
kesko_weight_6 <- weights_6[
  blm_assets == "Kesko"
]

# Extract Kesko's TAA deviation
kesko_taa_6 <- taa_6[
  blm_assets == "Kesko"
]

# Create results table
results_6 <- data.frame(
  Asset = blm_assets,
  Market_Weight = market_weights_6 * 100,
  Revised_Weight = weights_6 * 100,
  TAA = taa_6 * 100
)

# Results
print(results_6)
print(kesko_weight_6)
print(kesko_taa_6)



## ---- exercise-7 ----
# Exercise 7

# Annualise model inputs
Sigma_annual_7 <- Sigma_5 * 12
V_annual_7 <- V_5 * 12

P_7 <- P_5
tau_7 <- tau_5
lambda_7 <- lambda_5

# Extract annual risk-free rate
rf_annual_7 <- blm_parameters_cleaned$Value[
  blm_parameters_cleaned$Parameter == "Risk_Free_Rate_Annual"
]

# Calculate annual equilibrium expected returns
Pi_annual_7 <- as.numeric(
  rf_annual_7 +
    lambda_7 * Sigma_annual_7 %*% market_weights_5
)

# Calculate annual Omega matrix
Omega_annual_7 <- diag(diag(
  tau_7 * P_7 %*% Sigma_annual_7 %*% t(P_7)
))

# Calculate revised annual expected returns
expected_returns_7 <- as.numeric(
  Pi_annual_7 +
    Sigma_annual_7 %*% t(P_7) %*%
    solve(
      Omega_annual_7 / tau_7 +
        P_7 %*% Sigma_annual_7 %*% t(P_7)
    ) %*%
    (V_annual_7 - P_7 %*% Pi_annual_7)
)

# Calculate revised annual covariance matrix
Sigma_revised_7 <- Sigma_annual_7 + tau_7 * (
  Sigma_annual_7 -
    Sigma_annual_7 %*% t(P_7) %*%
    solve(
      tau_7 * P_7 %*% Sigma_annual_7 %*% t(P_7) +
        Omega_annual_7
    ) %*%
    (tau_7 * P_7 %*% Sigma_annual_7)
)

# Calculate revised portfolio weights
weights_7 <- as.numeric(
  lambda_7 * solve(
    Sigma_revised_7,
    expected_returns_7
  )
)

# Normalise portfolio weights
weights_scaled_7 <- weights_7 / sum(weights_7)

# Define excess log return columns
return_cols_7 <- c(
  "Nokia_Excess_Log",
  "N_Renkaat_Excess_Log",
  "Kesko_Excess_Log",
  "Teleste_Excess_Log",
  "Tieto_Excess_Log"
)

# Extract October 2016 excess log returns
log_returns_7 <- blm_data_cleaned %>%
  filter(Date == as.Date("2016-10-31")) %>%
  select(all_of(return_cols_7)) %>%
  unlist(use.names = FALSE)

# Convert log returns into simple returns
simple_returns_7 <- exp(log_returns_7) - 1

# Calculate individual return contributions
return_contributions_7 <- weights_scaled_7 *
  simple_returns_7

# Calculate realised portfolio return
portfolio_return_7 <- sum(return_contributions_7)

# Create results table
results_7 <- data.frame(
  Asset = blm_assets,
  Weight = weights_scaled_7 * 100,
  Log_Return = log_returns_7 * 100,
  Simple_Return = simple_returns_7 * 100,
  Contribution = return_contributions_7 * 100
)

# Results
print(results_7)
print(portfolio_return_7)