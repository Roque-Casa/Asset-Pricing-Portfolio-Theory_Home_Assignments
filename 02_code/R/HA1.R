# Home-Assignement 1


## ---- exercise-2 ----
# Exercise 2: Expected Utility

# Given values
R <- c(-0.80, 0.15)
p <- c(0.10, 0.90)
lambda <- 2

# Expected return
expected_return <- sum(p * R)

# Variance
variance <- sum(p * (R - expected_return)^2)

# Utility
utility <- expected_return - 0.5 * lambda * variance

# Results
expected_return
variance
utility
## ---- end-exercise-2 ----


## ---- exercise-3 ----
# Exercise 3: Insurance and Risk Aversion

# Given values
x <- 100000
premium <- 1000

# Returns without insurance
R_uninsured <- R

# Returns with insurance
R_insured <- c(-premium / x, 0.15 - premium / x)

# Expected returns
er_uninsured <- sum(p * R_uninsured)
er_insured <- sum(p * R_insured)

# Variances
var_uninsured <- sum(p * (R_uninsured - er_uninsured)^2)
var_insured <- sum(p * (R_insured - er_insured)^2)

# Indifference condition
lambda_3 <- 2 * (er_uninsured - er_insured) /
  (var_uninsured - var_insured)

lambda_3
## ---- end-exercise-3 ----


## ---- exercise-4 ----
# Exercise 4: Matrix Calculations
library(readxl)

# Read columns C-I from Sheet1
data_excel <- read_excel("01_data/raw/HA2_data_adj.xlsx",
                   sheet = "Sheet1",
                   range = "C1:I122")

# Convert index values to a matrix
prices <- as.matrix(data_excel)

# Calculate continuously compounded returns
R <- diff(log(prices))

# Calculate R transpose times R
RtR <- crossprod(R)

# Sum diagonal entries
sum_diagonal <- sum(diag(RtR))

# Results
sum_diagonal
## ---- end-exercise-4 ----


## ---- exercise-5 ----
# Exercise 5: Returns and Volatility

# Mean return of each asset
mean_returns <- colMeans(R)

# Volatility of each asset
volatility <- apply(R, 2, sd)

# Sum of all 14 values
sum_all_values <- sum(mean_returns) + sum(volatility)

# Results
mean_returns
volatility
sum_all_values
## ---- end-exercise-5 ----


## ---- exercise-7 ----
# Exercise 7: Fama-French Factors

data_ff_raw <- read.csv(
  "01_data/raw/F-F_Research_Data_Factors.csv",
  skip = 4
)

head(data_ff_raw)


# Keep only monthly observations (YYYYMM)
data_ff_m <- data_ff_raw[
  grepl("^[0-9]{6}$", data_ff_raw$X),
]

# Convert factor columns to numeric values
data_ff_m[, 2:5] <- lapply(data_ff_m[, 2:5], as.numeric)

# Convert percent to decimals
data_ff_m[, 2:5] <- data_ff_m[, 2:5] / 100

# Check results
head(data_ff_m)
tail(data_ff_m)
nrow(data_ff_m)

# Select the four monthly return series
ff_returns <- data_ff_m[, 2:5]

# Mean, standard deviation and variance
ff_mean <- colMeans(ff_returns)
ff_sd <- sapply(ff_returns, sd)
ff_var <- sapply(ff_returns, var)

# Variance-covariance matrix
ff_cov <- cov(ff_returns)

# Sum of all 16 matrix entries
sum_all_matrix <- sum(ff_cov)

# Results
ff_mean
ff_sd
ff_var
ff_cov
sum_all_matrix
## ---- end-exercise-7 ----

