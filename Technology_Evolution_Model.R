# Technology evolution module for predicting environmental emissions and economic parameters----
# under different SSP scenarios from 2025 to 2060.
# Environmental emissions evolution ----
# This section predicts emission factors for various technologies across SSP scenarios
# using a logistic growth model, accounting for technology maturity and minimum emission ratios.
library(openxlsx)
EF2020_data <- read.csv("/---/Tech revolution data.csv", header = TRUE, stringsAsFactors = FALSE)
# Define a function to predict emission factors over time for different technologies
# based on their maturity status and scenario-specific parameters.
predict_emissions <- function(EF_data, 
                              mature_tech, 
                              tm_mature, k_mature, EF_min_ratio_mature,
                              tm_not,    k_not,    EF_min_ratio_not,
                              years,
                              not_mature_static = FALSE) {
  
  tech_cols <- names(EF_data)[-c(1, 2)]
  result_list <- list()
  
  for (year in years) {
    df_year <- EF_data[, 1:2]  
    
    for (tech in tech_cols) {
      EF0 <- EF_data[[tech]]
      
      if (tech %in% mature_tech) {
      
        EF_min <- EF0 * EF_min_ratio_mature
        EF_t <- EF_min + (EF0 - EF_min) / (1 + exp(k_mature * (year - tm_mature)))
        
      } else {
        if (not_mature_static) {
          
          EF_t <- EF0
        } else {
        
          EF_min <- EF0 * EF_min_ratio_not
          EF_t <- EF_min + (EF0 - EF_min) / (1 + exp(k_not * (year - tm_not)))
        }
      }
      
      df_year[[tech]] <- EF_t
    }
    
    result_list[[as.character(year)]] <- df_year
  }
  
  return(result_list)
}
# Apply the predict_emissions function for different SSP scenarios (SSP126, SSP245, SSP445)
# to calculate emission factors for high-to-upper-middle income (HUMI) and lower-middle-to-low income (LUMI) regions.
result_ssp126 <- predict_emissions(
  EF_data = EF2020_data, 
  mature_tech = c("T1","T2","T3","T6","T7","T8","T9","T10","T12","T13"),
  tm_mature = 2035, k_mature = 0.3, EF_min_ratio_mature = 0.6,
  tm_not = 2040,    k_not = 0.4,    EF_min_ratio_not    = 0.4,
  years = c(2025,2030,2035, 2040,2045, 2050, 2055,2060),
  not_mature_static = FALSE
)

result_ssp245 <- predict_emissions(
  EF_data = EF2020_data, 
  mature_tech = c("T1","T2","T3","T6","T7","T8","T9","T10","T12","T13"),
  tm_mature = 2035, k_mature = 0.2, EF_min_ratio_mature = 0.6,
  tm_not = 2040,    k_not = 0.3,    EF_min_ratio_not    = 0.4,
  years = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static = FALSE
)

result_ssp445_HUMI <- predict_emissions(
  EF_data = EF2020_data, 
  mature_tech = c("T1","T2","T3","T6","T7","T8","T9","T10","T12","T13"),
  tm_mature = 2035, k_mature = 0.2, EF_min_ratio_mature = 0.6,
  tm_not = 2045,    k_not = 0.3,    EF_min_ratio_not    = 0.4,
  years = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static = FALSE
)

result_ssp445_LUMI <- predict_emissions(
  EF_data = EF2020_data, 
  mature_tech = c("T1","T2","T3","T6","T7","T8","T9","T10","T12","T13"),
  tm_mature = 2040, k_mature = 0.1, EF_min_ratio_mature = 0.6,
  tm_not = 2040,    k_not = 0.1,    EF_min_ratio_not    = 0.4,
  years = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static = TRUE
)

# Save predicted emission factors for each SSP scenario to separate Excel files,
# with each year as a worksheet.
wb <- createWorkbook()
for (year in names(result_ssp126)) {
  addWorksheet(wb, sheetName = year)
  writeData(wb, sheet = year, x = result_ssp126[[year]])
}
saveWorkbook(wb, file = "/---/predicted_emissions_ssp126.xlsx", overwrite = TRUE)

wb <- createWorkbook()
for (year in names(result_ssp245)) {
  addWorksheet(wb, sheetName = year)
  writeData(wb, sheet = year, x = result_ssp245[[year]])
}
saveWorkbook(wb, file = "/---/predicted_emissions_ssp245.xlsx", overwrite = TRUE)

wb <- createWorkbook()
for (year in names(result_ssp445_HUMI)) {
  addWorksheet(wb, sheetName = year)
  writeData(wb, sheet = year, x = result_ssp445_HUMI[[year]])
}
saveWorkbook(wb, file = "/---/predicted_emissions_ssp445_HUMI.xlsx", overwrite = TRUE)

wb <- createWorkbook()
for (year in names(result_ssp445_LUMI)) {
  addWorksheet(wb, sheetName = year)
  writeData(wb, sheet = year, x = result_ssp445_LUMI[[year]])
}
saveWorkbook(wb, file = "/---/predicted_emissions_ssp445_LUMI.xlsx", overwrite = TRUE)




# Economic parameters evolution ----
# This section predicts economic parameters (profit and industrial added value) for technologies
# across SSP scenarios, considering technology maturity, policy incentives, and growth curves.
library(dplyr)
library(tidyr)
library(openxlsx)
econo_data <- read.csv("/---/techeconomicfactor.csv", header = TRUE, stringsAsFactors = FALSE)
econo_data <- data.frame(
  ID = paste0("T", seq_len(nrow(econo_data))),
  econo_data,
  check.names = FALSE
)

# Define a function to predict economic parameters over time for different technologies,
# incorporating technology maturity, policy effects, and scenario-specific growth rates.
predict_economic <- function(econo_data,
                            
                             id_col              = "ID",
                             tech_col            = "tech",
                             profit_col          = "profit",
                             iav_col             = "industrial_added_value",
                             not_mature_ids      = c("T4","T5","T11","T14"),
                             tm_mature           = 2035,
                             k_mature            = 0.3,
                             Pmax_ratio_mature   = 1.2,
                             tm_not              = 2040,
                             k_not               = 0.4,
                             Pmax_ratio_not      = 1.0,
                             policy_theta_mature = 0.25,
                             policy_theta_not    = 0.15,
                             policy_r_mature     = 0.05,
                             policy_r_not        = 0.15,
                             policy_start_year   = 2020,
                             years               = c(2025, 2030, 2035, 2040),
                             not_mature_static   = FALSE) {
  required_cols <- c(id_col, tech_col, profit_col, iav_col)
  missing_cols  <- setdiff(required_cols, names(econo_data))
  if (length(missing_cols) > 0) {
    stop("以下列在 econo_data 中未找到：", paste(missing_cols, collapse = ", "))
  }
  
  ids      <- econo_data[[id_col]]
  techs    <- econo_data[[tech_col]]
  P0_prof  <- econo_data[[profit_col]]
  P0_iav   <- econo_data[[iav_col]]
  is_mature <- !ids %in% not_mature_ids

  compute_series <- function(P0, prefix) {
    out <- data.frame(matrix(nrow = length(P0), ncol = length(years)),
                      stringsAsFactors = FALSE)
    colnames(out) <- paste0(prefix, "_", years)
    for (i in seq_along(years)) {
      t <- years[i]
    
      Pmin <- P0
    
      Pmax <- ifelse(is_mature,
                     P0 * Pmax_ratio_mature,
                     P0 * Pmax_ratio_not)
   
      k_vec  <- ifelse(is_mature, k_mature, k_not)
      tm_vec <- ifelse(is_mature, tm_mature, tm_not)
      
   
      P_ind <- Pmin + (Pmax - Pmin) / (1 + exp(-k_vec * (t - tm_vec)))
      if (not_mature_static) {
        P_ind[!is_mature] <- P0[!is_mature]
      }
      
      theta_vec <- ifelse(is_mature, policy_theta_mature, policy_theta_not)
      r_vec     <- ifelse(is_mature, policy_r_mature, policy_r_not)
      P_pol     <- P0 * theta_vec * exp(-r_vec * (t - policy_start_year))
      
      out[[i]] <- P_ind + P_pol
    }
    return(out)
  }
  
  base_df <- data.frame(ID = ids, tech = techs, stringsAsFactors = FALSE)
  
  profit_evo <- cbind(base_df, compute_series(P0_prof, "profit"))
  iav_evo    <- cbind(base_df, compute_series(P0_iav,  "iav"))
  
  list(
    profit_evolution = profit_evo,
    iav_evolution    = iav_evo
  )
}
# Apply the predict_economic function for different SSP scenarios to calculate economic parameters
# for HUMI and LUMI regions, accounting for policy and technology differences.
#ecores_ssp126
ecores_ssp126 <- predict_economic(
  econo_data            = econo_data,
  not_mature_ids        = c("T4","T5","T11","T14"),
  tm_mature             = 2035,
  k_mature              = 0.3,
  Pmax_ratio_mature     = 1.3,
  tm_not                = 2040,
  k_not                 = 0.4,
  Pmax_ratio_not        = 1.5,
  policy_theta_mature   = 0.1,
  policy_theta_not      = 0.2,
  policy_r_mature       = 0.3,
  policy_r_not          = 0.2,
  policy_start_year     = 2020,
  years                 = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static     = FALSE
)

profit_long <- ecores_ssp126$profit_evolution %>%
  pivot_longer(
    cols      = starts_with("profit_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("profit_", "", year)),
    type = "profit"
  )

iav_long <- ecores_ssp126$iav_evolution %>%
  pivot_longer(
    cols      = starts_with("iav_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("iav_", "", year)),
    type = "iav"
  )

ecores_ssp126_combined <- bind_rows(profit_long, iav_long) %>%
  select(ID, tech, type, year, value)

wb <- createWorkbook()
addWorksheet(wb, "evolution")
writeData(wb, "evolution", ecores_ssp126_combined)
saveWorkbook(wb, "/---/ecores_ssp126_combined_evolution.xlsx", overwrite = TRUE)

#ecores_ssp245HUMI
ecores_ssp245HUMI <- predict_economic(
  econo_data            = econo_data,
  not_mature_ids        = c("T4","T5","T11","T14"),
  tm_mature             = 2035,
  k_mature              = 0.2,
  Pmax_ratio_mature     = 1.3,
  tm_not                = 2040,
  k_not                 = 0.3,
  Pmax_ratio_not        = 1.5,
  policy_theta_mature   = 0.1,
  policy_theta_not      = 0.2,
  policy_r_mature       = 0.3,
  policy_r_not          = 0.2,
  policy_start_year     = 2020,
  years                 = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static     = FALSE
)

profit_long <- ecores_ssp245HUMI$profit_evolution %>%
  pivot_longer(
    cols      = starts_with("profit_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("profit_", "", year)),
    type = "profit"
  )

iav_long <- ecores_ssp245HUMI$iav_evolution %>%
  pivot_longer(
    cols      = starts_with("iav_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("iav_", "", year)),
    type = "iav"
  )

ecores_ssp245HUMI_combined <- bind_rows(profit_long, iav_long) %>%
  select(ID, tech, type, year, value)

wb <- createWorkbook()
addWorksheet(wb, "evolution")
writeData(wb, "evolution", ecores_ssp245HUMI_combined)
saveWorkbook(wb, "/---/ecores_ssp245HUMI_combined_evolution.xlsx", overwrite = TRUE)

#ecores_ssp245LUMI
ecores_ssp245LUMI <- predict_economic(
  econo_data            = econo_data,
  not_mature_ids        = c("T4","T5","T11","T14"),
  tm_mature             = 2035,
  k_mature              = 0.3,
  Pmax_ratio_mature     = 1.3,
  tm_not                = 2040,
  k_not                 = 0.4,
  Pmax_ratio_not        = 1.5,
  policy_theta_mature   = 0,
  policy_theta_not      = 0.1,
  policy_r_mature       = 0.3,
  policy_r_not          = 0.2,
  policy_start_year     = 2020,
  years                 = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static     = FALSE
)

profit_long <- ecores_ssp245LUMI$profit_evolution %>%
  pivot_longer(
    cols      = starts_with("profit_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("profit_", "", year)),
    type = "profit"
  )

iav_long <- ecores_ssp245LUMI$iav_evolution %>%
  pivot_longer(
    cols      = starts_with("iav_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("iav_", "", year)),
    type = "iav"
  )

ecores_ssp245LUMI_combined <- bind_rows(profit_long, iav_long) %>%
  select(ID, tech, type, year, value)

wb <- createWorkbook()
addWorksheet(wb, "evolution")
writeData(wb, "evolution", ecores_ssp245LUMI_combined)
saveWorkbook(wb, "/---/ecores_ssp245LUMI_combined_evolution.xlsx", overwrite = TRUE)

#ecores_ssp445HUMI
ecores_ssp445HUMI <- predict_economic(
  econo_data            = econo_data,
  not_mature_ids        = c("T4","T5","T11","T14"),
  tm_mature             = 2035,
  k_mature              = 0.3,
  Pmax_ratio_mature     = 1.3,
  tm_not                = 2045,
  k_not                 = 0.4,
  Pmax_ratio_not        = 1.5,
  policy_theta_mature   = 0.1,
  policy_theta_not      = 0.2,
  policy_r_mature       = 0.3,
  policy_r_not          = 0.2,
  policy_start_year     = 2020,
  years                 = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static     = FALSE
)

profit_long <- ecores_ssp445HUMI$profit_evolution %>%
  pivot_longer(
    cols      = starts_with("profit_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("profit_", "", year)),
    type = "profit"
  )

iav_long <- ecores_ssp445HUMI$iav_evolution %>%
  pivot_longer(
    cols      = starts_with("iav_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("iav_", "", year)),
    type = "iav"
  )

ecores_ssp445HUMI_combined <- bind_rows(profit_long, iav_long) %>%
  select(ID, tech, type, year, value)

wb <- createWorkbook()
addWorksheet(wb, "evolution")
writeData(wb, "evolution", ecores_ssp445HUMI_combined)
saveWorkbook(wb, "/---/ecores_ssp445HUMI_combined_evolution.xlsx", overwrite = TRUE)

#ecores_ssp445LUMI----
ecores_ssp445LUMI <- predict_economic(
  econo_data            = econo_data,
  not_mature_ids        = c("T4","T5","T11","T14"),
  tm_mature             = 2040,
  k_mature              = 0.1,
  Pmax_ratio_mature     = 1.3,
  tm_not                = 2040,
  k_not                 = 0.4,
  Pmax_ratio_not        = 1.5,
  policy_theta_mature   = 0.1,
  policy_theta_not      = 0.2,
  policy_r_mature       = 0.3,
  policy_r_not          = 0.2,
  policy_start_year     = 2020,
  years                 = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060),
  not_mature_static     = TRUE
)

profit_long <- ecores_ssp445LUMI$profit_evolution %>%
  pivot_longer(
    cols      = starts_with("profit_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("profit_", "", year)),
    type = "profit"
  )

iav_long <- ecores_ssp445LUMI$iav_evolution %>%
  pivot_longer(
    cols      = starts_with("iav_"),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(sub("iav_", "", year)),
    type = "iav"
  )

ecores_ssp445LUMI_combined <- bind_rows(profit_long, iav_long) %>%
  select(ID, tech, type, year, value)

wb <- createWorkbook()
addWorksheet(wb, "evolution")
writeData(wb, "evolution", ecores_ssp445LUMI_combined)
saveWorkbook(wb, "/---/ecores_ssp445LUMI_combined_evolution.xlsx", overwrite = TRUE)
