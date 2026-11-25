## ---- exercise-10 ----

# Libraries
library(readxl)
library(dplyr)

# File path
file_path <- "01_data/raw/Chapter_7_2020.xlsm"


# Treynor-Black

# Read model parameters
treynor_black_cleaned <- data.frame(
  Parameter = c(
    "Market_Equity_Premium",
    "Risk_Free_Rate",
    "Market_Volatility",
    "Risk_Aversion"
  ),
  Value = read_excel(
    file_path,
    sheet = "Treynor-Black",
    range = "E5:E8",
    col_names = FALSE
  )[[1]]
)

# Save cleaned data
write.csv(
  treynor_black_cleaned,
  "01_data/cleaned/data_cleaned_treynor_black.csv",
  row.names = FALSE
)


# Black-Litterman Case

# Read asset names
blm_assets <- as.character(unlist(
  read_excel(
    file_path,
    sheet = "Black-Litterman Case",
    range = "B3:F3",
    col_names = FALSE
  )
))

# Read market capitalisations and weights
blm_case_cleaned <- data.frame(
  Asset = blm_assets,
  Market_Cap = as.numeric(unlist(
    read_excel(
      file_path,
      sheet = "Black-Litterman Case",
      range = "B4:F4",
      col_names = FALSE
    )
  )),
  Market_Weight = as.numeric(unlist(
    read_excel(
      file_path,
      sheet = "Black-Litterman Case",
      range = "B5:F5",
      col_names = FALSE
    )
  ))
)

# Read monthly covariance matrix
blm_cov_cleaned <- as.matrix(
  read_excel(
    file_path,
    sheet = "Black-Litterman Case",
    range = "B9:F13",
    col_names = FALSE
  )
)

colnames(blm_cov_cleaned) <- blm_assets
rownames(blm_cov_cleaned) <- blm_assets

# Read model parameters
blm_parameters_cleaned <- data.frame(
  Parameter = c(
    "Risk_Free_Rate_Monthly",
    "Risk_Free_Rate_Annual",
    "Tau",
    "Lambda",
    "Market_Variance"
  ),
  Value = sapply(
    c("C25", "I25", "C26", "E26", "E25"),
    function(cell) {
      read_excel(
        file_path,
        sheet = "Black-Litterman Case",
        range = cell,
        col_names = FALSE
      )[[1]][1]
    }
  )
)

# Save cleaned data
write.csv(
  blm_case_cleaned,
  "01_data/cleaned/data_cleaned_blm_case.csv",
  row.names = FALSE
)

write.csv(
  blm_cov_cleaned,
  "01_data/cleaned/data_cleaned_blm_covariance.csv",
  row.names = TRUE
)

write.csv(
  blm_parameters_cleaned,
  "01_data/cleaned/data_cleaned_blm_parameters.csv",
  row.names = FALSE
)


# Data for the BLM case

# Import monthly data
blm_data_raw <- read_excel(
  file_path,
  sheet = "Data for the BLM case",
  range = "A3:X135",
  col_names = FALSE,
  .name_repair = "unique_quiet"
)

# Select and rename relevant columns
blm_data_cleaned <- data.frame(
  Date = as.Date(blm_data_raw[[1]]),
  
  Market_TRI = blm_data_raw[[2]],
  Nokia_TRI = blm_data_raw[[3]],
  N_Renkaat_TRI = blm_data_raw[[4]],
  Kesko_TRI = blm_data_raw[[5]],
  Teleste_TRI = blm_data_raw[[6]],
  Tieto_TRI = blm_data_raw[[7]],
  
  RF_Annual_Percent = blm_data_raw[[9]],
  RF_Monthly = blm_data_raw[[11]],
  
  Market_Excess_Log = blm_data_raw[[12]],
  Nokia_Excess_Log = blm_data_raw[[13]],
  N_Renkaat_Excess_Log = blm_data_raw[[14]],
  Kesko_Excess_Log = blm_data_raw[[15]],
  Teleste_Excess_Log = blm_data_raw[[16]],
  Tieto_Excess_Log = blm_data_raw[[17]],
  
  Nokia_Market_Cap = blm_data_raw[[20]],
  N_Renkaat_Market_Cap = blm_data_raw[[21]],
  Kesko_Market_Cap = blm_data_raw[[22]],
  Teleste_Market_Cap = blm_data_raw[[23]],
  Tieto_Market_Cap = blm_data_raw[[24]]
) %>%
  filter(!is.na(Date)) %>%
  arrange(Date)

# Save cleaned data
write.csv(
  blm_data_cleaned,
  "01_data/cleaned/data_cleaned_blm_monthly.csv",
  row.names = FALSE)