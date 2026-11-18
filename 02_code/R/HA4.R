# Libraries and data import
library(dplyr)

monthly_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_monthly_HA2-6.csv"
)

monthly_cleaned$Date <- as.Date(monthly_cleaned$Date)

us_cpi_cleaned <- read.csv(
  "01_data/cleaned/data_cleaned_us_cpi_HA2-6.csv"
)

us_cpi_cleaned$Date <- as.Date(us_cpi_cleaned$Date)

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

## ---- exercise-2 ----
# Exercise 2

# Market index and risk-free rate columns
market_col_2 <- grep(
  "S.P.*500.*TOT.RETURN.IND",
  names(monthly_cleaned),
  value = TRUE
)

rf_col_2 <- grep(
  "US.DOLLAR.*MIDDLE.RATE",
  names(monthly_cleaned),
  value = TRUE
)

tri_cols_2 <- c(tri_cols, market_col_2)

# Calculate simple monthly returns and risk-free rates
data_2 <- monthly_cleaned %>%
  arrange(Date) %>%
  mutate(
    across(
      all_of(tri_cols_2),
      ~ .x / lag(.x) - 1
    ),
    rf_monthly_2 = lag(.data[[rf_col_2]]) / 100 / 12
  ) %>%
  filter(
    Date >= as.Date("2011-01-01"),
    Date <= as.Date("2021-12-31")
  )

# Calculate monthly excess returns
excess_returns_2 <- as.matrix(
  data_2[, tri_cols_2]
) - data_2$rf_monthly_2

mean_excess_2 <- colMeans(excess_returns_2)

sum_mean_excess_2 <- sum(mean_excess_2) * 100

# Create results table
results_2 <- data.frame(
  Asset = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart",
    "S&P 500"
  ),
  Mean_Excess_Return = as.numeric(mean_excess_2) * 100
)

# Results
#print(market_col_2)
#print(rf_col_2)
print(results_2)
print(sum_mean_excess_2)



## ---- exercise-3 ----
# Exercise 3

# Find CPI column
cpi_col_3 <- setdiff(names(us_cpi_cleaned), "Date")

# Calculate monthly inflation
inflation_data_3 <- us_cpi_cleaned %>%
  arrange(Date) %>%
  mutate(
    inflation_3 = .data[[cpi_col_3]] /
      lag(.data[[cpi_col_3]]) - 1
  ) %>%
  select(Date, inflation_3)

# Match monthly inflation with nominal returns
data_3 <- data_2 %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  left_join(
    inflation_data_3 %>%
      mutate(Month = format(Date, "%Y-%m")) %>%
      select(Month, inflation_3),
    by = "Month"
  )

# Calculate monthly real returns using Fisher equation
nominal_returns_3 <- as.matrix(
  data_3[, tri_cols_2]
)

real_returns_3 <- sweep(
  1 + nominal_returns_3,
  1,
  1 + data_3$inflation_3,
  "/"
) - 1

# Calculate arithmetic average monthly real returns
mean_real_3 <- colMeans(real_returns_3)

sum_mean_real_3 <- sum(mean_real_3) * 100

# Create results table
results_3 <- data.frame(
  Asset = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart",
    "S&P 500"
  ),
  Mean_Real_Return = as.numeric(mean_real_3) * 100
)

# Results
print(results_3)
print(sum_mean_real_3)



## ---- exercise-4 ----
# Exercise 4: 

market_excess_4 <- excess_returns_2[, 6]

stock_excess_4 <- excess_returns_2[, 1:5]

# Store regression models and coefficients
models_4 <- vector("list", length(stocks))
alphas_4 <- numeric(length(stocks))
betas_4 <- numeric(length(stocks))

# Estimate OLS regressions for each stock
for (i in seq_along(stocks)) {
  
  models_4[[i]] <- lm(
    stock_excess_4[, i] ~ market_excess_4
  )
  
  alphas_4[i] <- coef(models_4[[i]])[1]
  betas_4[i] <- coef(models_4[[i]])[2]
}

sum_alphas_4 <- sum(alphas_4)

# Create results table
results_4 <- data.frame(
  Stock = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart"
  ),
  Alpha = alphas_4,
  Beta = betas_4
)

# Results
print(results_4)
print(sum_alphas_4)



## ---- exercise-5 ----
# Exercise 5

# Sum the five betas from Exercise 4
sum_betas_5 <- sum(betas_4)

# Results
print(sum_betas_5)



## ---- exercise-6 ----
# Exercise 6

# Calculate sample residual variances (T - 1)
residuals_6 <- sapply(
  models_4,
  residuals
)

residual_variances_6 <- apply(
  residuals_6,
  2,
  var
)

sum_residual_variances_6 <- sum(residual_variances_6)

# Create results table
results_6 <- data.frame(
  Stock = c(
    "Apple",
    "Microsoft",
    "Tesla",
    "JP Morgan Chase",
    "Walmart"
  ),
  Residual_Variance = as.numeric(residual_variances_6)
)

# Results
print(results_6)
print(sum_residual_variances_6)



## ---- exercise-7 ----
# Exercise 7

market_variance_7 <- var(market_excess_4)

# Calculate systematic covariance matrix
systematic_cov_7 <- outer(
  betas_4,
  betas_4
) * market_variance_7

# Add residual variances to the diagonal
cov_matrix_7 <- systematic_cov_7 +
  diag(residual_variances_6)

# Calculate correlation matrix
cor_matrix_7 <- cov2cor(cov_matrix_7)

sum_variances_7 <- sum(diag(cov_matrix_7))

# Assign stock names
stock_names_7 <- c(
  "Apple",
  "Microsoft",
  "Tesla",
  "JP Morgan Chase",
  "Walmart"
)

dimnames(cov_matrix_7) <- list(
  stock_names_7,
  stock_names_7
)

dimnames(cor_matrix_7) <- list(
  stock_names_7,
  stock_names_7
)

# Results
print(cov_matrix_7)
print(cor_matrix_7)
print(sum_variances_7)



## ---- exercise-8 ----
# Exercise 8:

T_8 <- nrow(residuals_6)
N_8 <- length(alphas_4)

# Alpha vector from Exercise 4
alpha_vector_8 <- matrix(alphas_4, ncol = 1)

# Full residual covariance matrix (T - 2)
Sigma_u_8 <- crossprod(residuals_6) / (T_8 - 2)

mean_market_8 <- mean(market_excess_4)
sd_market_8 <- sd(market_excess_4)

theta_m_8 <- mean_market_8 / sd_market_8

# Calculate GRS statistic
GRS_8 <- (T_8 / N_8) *
  ((T_8 - N_8 - 1) / (T_8 - 2)) *
  as.numeric(
    t(alpha_vector_8) %*%
      solve(Sigma_u_8, alpha_vector_8)
  ) /
  (1 + theta_m_8^2)

# Calculate p-value using F distribution
p_value_8 <- pf(
  GRS_8,
  df1 = N_8,
  df2 = T_8 - N_8 - 1,
  lower.tail = FALSE
)

# Create results table
results_8 <- data.frame(
  Statistic = c(
    "GRS Statistic",
    "p-value",
    "Significance Level"
  ),
  Value = c(
    GRS_8,
    p_value_8,
    0.05
  )
)

# Results
print(results_8)