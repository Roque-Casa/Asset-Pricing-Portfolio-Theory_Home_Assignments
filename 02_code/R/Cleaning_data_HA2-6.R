# Data Cleaning of HA2data_2022.xlsx for Home Assignment 2 to 6

## ---- cleaning-daily ----
# Daily cleaning code

# Import dataset daily
library(readxl)

daily_excel <- read_excel("01_data/raw/HA2data_2022.xlsx",
                         sheet = "Daily")


# Cleaned dataset daily
library(dplyr)

daily_cleaned <- daily_excel %>%
  select(-c(39, 42)) %>%
  select(where(~ !all(is.na(.)))) %>%
  setNames(make.names(as.character(unlist(.[1, ])),
                      unique = TRUE)) %>%
  slice(-1) %>%
  mutate(across(everything(), type.convert, as.is = TRUE)) %>%
  rename(Date = 1) %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))

write.csv(daily_cleaned,
          "01_data/cleaned/data_cleaned_daily_HA2-6.csv",
          row.names = FALSE)



## ---- cleaning-monthly ----
# Monthly cleaning code

# Import dataset monthly
library(readxl)

monthly_excel <- read_excel("01_data/raw/HA2data_2022.xlsx",
                         sheet = "Monthly")

# Cleaned dataset monthly
library(dplyr)

monthly_cleaned <- monthly_excel %>%
  select(-c(39, 42)) %>%
  select(where(~ !all(is.na(.)))) %>%
  setNames(make.names(as.character(unlist(.[1, ])),
                      unique = TRUE)) %>%
  slice(-1) %>%
  mutate(across(everything(), type.convert, as.is = TRUE)) %>%
  rename(Date = 1) %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))

write.csv(monthly_cleaned,
          "01_data/cleaned/data_cleaned_monthly_HA2-6.csv",
          row.names = FALSE)



## ---- cleaning-cpi ----
# CPI cleaning code

# Import dataset US CPI index
library(readxl)

us_cpi_excel <- read_excel("01_data/raw/HA2data_2022.xlsx",
                            sheet = "US CPI index")


# Cleaned dataset US CPI index
library(dplyr)
library(tidyr)

us_cpi_cleaned <- us_cpi_excel %>%
  rename(Year = 1) %>%
  select(-Average) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Month",
    values_to = "CPI"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month),
    Date = as.Date(sprintf("%04d-%02d-01", Year, Month))
  ) %>%
  filter(!is.na(CPI)) %>%
  select(Date, CPI) %>%
  arrange(Date)


write.csv(us_cpi_cleaned,
          "01_data/cleaned/data_cleaned_us_cpi_HA2-6.csv",
          row.names = FALSE)