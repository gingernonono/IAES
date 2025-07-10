
# Codes of this section is used to calculate agricultural and forestry residues (AFRs), 
# including Crop Residues (CRs), Forestry Residues (FRs), and Livestock Excrements (LEs) in sequence.

# The CRs section calculates region-specific CRs-to-grain ratios and combines them with GCAM output (regional crop yield) to compute CRs.

library(readr) 
library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)

# Read raw data for Crop Residue-to-Product Ratio (RPR) from CSV file
# Calculate RPR for different crops based on yield values using exponential functions
# Adjust RPR with crop-specific coefficients to account for residue characteristics
RPRrawdata <- read_csv("/---/RPRrawdataFAO.csv")
RPRrawdata <- RPRrawdata %>%
  mutate(
    RPR = case_when(
      Element == "Yield" & Item == "Barley" ~ 1.822*exp(-0.00049*Value)*0.6, 
      Element == "Yield" & Item == "Maize (corn)" ~ 2.656*exp(-0.000103*Value)*0.5,
      Element == "Yield" & Item == "Rice" ~ 2.45*exp(-0.000084*Value)*0.4,  
      Element == "Yield" & Item == "Beans, dry" ~ 3.869*exp(-0.000178*Value)*0.7,
      Element == "Yield" & Item == "Sugar cane" ~ 0.6*0.7,
      Element == "Yield" & Item == "Wheat" ~ 2.183*exp(-0.000127*Value)*0.6,
      TRUE ~ NA_real_ 
    )
  )
RPRrawdata <- RPRrawdata %>%
  filter(!is.na(Value) & Value != 0)

RPRrawdata_filtered <- RPRrawdata %>%
  group_by(Year, Item, `Area Code (M49)`) %>%
  filter(all(c("Yield", "Area harvested") %in% Element)) %>%
  ungroup()

# Define a list of regions with corresponding M49 area codes for regional aggregation
region_list <- list(
  reg1 = c(108, 174, 262, 232, 231, 404, 450, 480, 646, 729, 706, 800),
  reg2 = c(12, 818, 434, 504, 788),  
  reg3 = c(24, 72, 426, 508, 454, 516, 894, 716),   
  reg4 = c(204, 254, 140, 384, 120, 180, 178, 132, 266, 288, 324, 270, 624, 466, 478, 562, 566, 686, 694, 678, 148, 768),
  reg5 = c(32),
  reg6 = c(36, 554),
  reg7 = c(76),
  reg8 = c(124),
  reg9 = c(28, 44, 84, 52, 188, 192, 212, 214, 308, 320, 340, 332, 388, 558, 591, 222, 780, 670),
  reg10 = c(51, 31, 268, 398, 417, 496, 762, 795, 860),
  reg11 = c(159),
  reg12 = c(170),
  reg13 = c(100, 196, 203, 233, 348, 440, 428, 616, 642, 703, 705),
  reg14 = c(40, 56, 208, 246, 250, 276, 300, 372, 380, 442, 528, 620, 752, 724, 826),
  reg15 = c(112, 804),
  reg16 = c(578, 756),
  reg17 = c(8, 70, 191, 499, 688, 790),
  reg18 = c(356),
  reg19 = c(360),
  reg20 = c(392),
  reg21 = c(484),
  reg22 = c(784, 364, 368, 376, 400, 414, 422, 512, 275, 634, 682, 887),
  reg23 = c(586),
  reg24 = c(643),
  reg25 = c(710),
  reg26 = c(328, 740, 862),
  reg27 = c(68, 152, 218, 604, 600, 858),
  reg28 = c(4, 50, 64, 144, 524),
  reg29 = c(882, 96, 242, 116, 418, 104, 458, 608, 408, 258, 764, 626, 704, 548, 882),
  reg30 = c(410),
  reg31 = c(159),
  reg32 = c(840)
)

years <- c(2019, 2020, 2021, 2022, 2023)
items <- c("Barley", "Maize (corn)", "Rice", "Beans, dry", "Sugar cane", "Wheat")
# Function to calculate region-specific yield by aggregating yield and area harvested
calculate_region_yield <- function(df, year, item, region_name, region_codes) {
  df_filtered <- df %>%
    filter(Year == year, Item == item, `Area Code (M49)` %in% region_codes)
  
  total_area_harvested <- sum(df_filtered$Value[df_filtered$Element == "Area harvested"], na.rm = TRUE)
  weighted_yield_sum <- sum(df_filtered$Value[df_filtered$Element == "Yield"] * 
                              df_filtered$Value[df_filtered$Element == "Area harvested"], na.rm = TRUE)
  
  region_yield <- ifelse(total_area_harvested > 0, weighted_yield_sum / total_area_harvested, NA_real_)
  
  return(data.frame(region = region_name, year = year, item = item, yield = region_yield))
}

# Generate RPR data by applying calculate_region_yield function across regions, years, and items
RPRdata <- expand.grid(region = names(region_list), year = years, item = items) %>%
  pmap_dfr(~ calculate_region_yield(RPRrawdata_filtered, ..2, ..3, ..1, region_list[[..1]]))
# Calculate RPR for each crop based on region-specific yields
RPRdata <- RPRdata %>%
  mutate(RPR = case_when(
    item == "Barley"        ~ 1.822 * exp(-0.00049 * yield) ,
    item == "Maize (corn)"  ~ 2.656 * exp(-0.000103 * yield) ,
    item == "Rice"          ~ 2.45 * exp(-0.000084 * yield) ,
    item == "Beans, dry"    ~ 3.869 * exp(-0.000178 * yield) ,
    item == "Sugar cane"    ~ 0.6 * 0.7, 
    item == "Wheat"         ~ 2.183 * exp(-0.000127 * yield) ,
    TRUE ~ NA_real_  
  ))
# Summarize RPR data by region and item, calculating average, minimum, and maximum RPR
RPR_summary <- RPRdata %>%
  filter(!is.na(RPR)) %>%  
  group_by(region, item) %>%  
  summarise(
    avg_RPR = ifelse(n() > 0, mean(RPR, na.rm = TRUE), NA_real_),  
    max_RPR = ifelse(n() > 0, max(RPR, na.rm = TRUE), NA_real_),   
    min_RPR = ifelse(n() > 0, min(RPR, na.rm = TRUE), NA_real_)    
    , .groups = "drop")  

#print(RPR_summary)

region_existing_items <- RPR_summary %>%
  select(region, item) %>%
  distinct()

# Create a complete grid of region-item combinations to identify missing ones
full_region_item <- expand.grid(region = unique(RPR_summary$region), item = items)
missing_items <- full_region_item %>%
  anti_join(region_existing_items, by = c("region", "item"))

global_RPR_stats <- RPR_summary %>%
  group_by(item) %>%
  summarise(
    avg_RPR = mean(avg_RPR, na.rm = TRUE),
    min_RPR = min(min_RPR, na.rm = TRUE),
    max_RPR = max(max_RPR, na.rm = TRUE)
  )
# Fill missing region-item combinations with global RPR statistics
RPR_summary_filled <- RPR_summary %>%
  full_join(missing_items, by = c("region", "item")) %>%  
  left_join(global_RPR_stats, by = "item") %>%  
  mutate(
    avg_RPR = ifelse(is.na(avg_RPR.x), avg_RPR.y, avg_RPR.x),  
    min_RPR = ifelse(is.na(min_RPR.x), min_RPR.y, min_RPR.x),  
    max_RPR = ifelse(is.na(max_RPR.x), max_RPR.y, max_RPR.x)   
  ) %>%
  select(region, item, avg_RPR, min_RPR, max_RPR)  
# Adjust specific RPR values for sugar cane to ensure consistency
RPR_summary_filled$min_RPR[RPR_summary_filled$min_RPR == 0.42] <- 0.378
RPR_summary_filled$max_RPR[RPR_summary_filled$max_RPR == 0.42] <- 0.462
# Map crop items to standardized crop names for consistency with other datasets
RPR_summary_filled <- RPR_summary_filled %>%
  mutate(crop = case_when(
    item == "Barley" ~ "OtherGrain",
    item == "Maize (corn)" ~ "Corn",
    item == "Rice" ~ "Rice",
    item == "Beans, dry" ~ "Soybean",
    item == "Sugar cane" ~ "SugarCrop",
    item == "Wheat" ~ "Wheat",
    TRUE ~ item 
  ))

write.csv(RPR_summary_filled, "/---/RPRdata.csv")


# --- Calculate Crop Residue Quantities ----
agprod <- read_excel("/---/sspX_agprod.xlsx")  
agprod <- agprod %>%
  rename(region_code = `region code`)
# Join production data with RPR data to calculate residue production
agprod_with_RPR <- agprod %>%
  left_join(RPR_summary_filled %>% select(region = region, crop, avg_RPR, min_RPR, max_RPR),
            by = c("region_code" = "region", "crop"))
# Calculate residue production (min, avg, max) by multiplying production with RPR values
agprod_with_RPR <- agprod_with_RPR %>%
  mutate(
    residue_production = production * avg_RPR,
    residue_min = production * min_RPR,
    residue_max = production * max_RPR
  )
# Define heat values for different crops (in MJ/kg)
crop_heat_values <- tibble::tibble(
  crop = c("Soybean", "Corn", "Rice", "OtherGrain", "SugarCane", "Wheat"),
  heat_value = c(18.96, 18.6, 15.09, 14.2, 16.2, 14.86)
)

agprod_with_energy <- agprod_with_RPR %>%
  left_join(crop_heat_values, by = "crop")

# Join residue production data with crop heat values
agprod_with_energy <- agprod_with_RPR %>%
  left_join(crop_heat_values, by = "crop") %>%
  mutate(
    energy_min  = residue_min        * heat_value / 1000,   
    energy_avg  = residue_production * heat_value / 1000,
    energy_max  = residue_max        * heat_value / 1000,
    
    mass_min = residue_min,
    mass_avg = residue_production,
    mass_max = residue_max,
    
    unit_mass  = "Mt",
    unit_energy = "EJ"
  )

# Summarize residue data by region, year, and scenario
residue_summary <- agprod_with_energy %>%
  group_by(region, region_code, Year, scenario, unit_mass, unit_energy) %>%
  summarise(
    total_mass_min = sum(mass_min, na.rm = TRUE),
    total_mass_avg = sum(mass_avg, na.rm = TRUE),
    total_mass_max = sum(mass_max, na.rm = TRUE),
    total_energy_min = sum(energy_min, na.rm = TRUE),
    total_energy_avg = sum(energy_avg, na.rm = TRUE),
    total_energy_max = sum(energy_max, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(residue_summary, "/---/CR_region.csv", row.names = FALSE)

# Calculate global crop residue totals
residue_global <- residue_summary %>%
  group_by(Year, scenario, unit_mass, unit_energy) %>%
  summarise(
    total_mass_min = sum(total_mass_min, na.rm = TRUE),
    total_mass_avg = sum(total_mass_avg, na.rm = TRUE),
    total_mass_max = sum(total_mass_max, na.rm = TRUE),
    
    total_energy_min = sum(total_energy_min, na.rm = TRUE),
    total_energy_avg = sum(total_energy_avg, na.rm = TRUE),
    total_energy_max = sum(total_energy_max, na.rm = TRUE),
    .groups = "drop"
  ) 

write.csv(residue_global, "/---/CR_global.csv", row.names = FALSE)





# --- Calculate Forest Residue Quantities ----

library(readxl)
library(dplyr)
# Process ssp126 scenario
file_path <- "/---/ssp126_detail_land_allocation.xlsx"

sheet_names <- excel_sheets(file_path)

all_forest_rows <- data.frame()

for (sheet in sheet_names) {

  sheet_data <- read_excel(file_path, sheet = sheet)

  forest_rows <- sheet_data %>%
    filter(grepl("Forest_", LandLeaf)| grepl("Tree", LandLeaf))
  
  all_forest_rows <- rbind(all_forest_rows, forest_rows)
}
all_forest_rows$scenario <- "ssp126"
all_forest_rows <- all_forest_rows %>%
  group_by(region, scenario, Year) %>%
  summarise(value_sum = sum(value), .groups = "drop") %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060))

all_scenario_data <- data.frame()
all_scenario_data <- bind_rows(all_scenario_data, all_forest_rows)


# Process ssp245 scenario
file_path <- "/---/ssp245_detailed_land_allocation.xlsx"

sheet_names <- excel_sheets(file_path)

all_forest_rows <- data.frame()

for (sheet in sheet_names) {
  
  sheet_data <- read_excel(file_path, sheet = sheet)
  
  forest_rows <- sheet_data %>%
    filter(grepl("Forest_", LandLeaf)| grepl("Tree", LandLeaf))
  
  all_forest_rows <- rbind(all_forest_rows, forest_rows)
}
all_forest_rows$scenario <- "ssp245"
all_forest_rows <- all_forest_rows %>%
  group_by(region, scenario, Year) %>%
  summarise(value_sum = sum(value), .groups = "drop") %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060))

all_scenario_data <- bind_rows(all_scenario_data, all_forest_rows)

# Process ssp445 scenario
file_path <- "/---/ssp445_detailed_land_allocation.xlsx"

sheet_names <- excel_sheets(file_path)

all_forest_rows <- data.frame()

for (sheet in sheet_names) {
  
  sheet_data <- read_excel(file_path, sheet = sheet)
  
  forest_rows <- sheet_data %>%
    filter(grepl("Forest_", LandLeaf)| grepl("Tree", LandLeaf) )
  
  all_forest_rows <- rbind(all_forest_rows, forest_rows)
}
all_forest_rows$scenario <- "ssp445"
all_forest_rows <- all_forest_rows %>%
  group_by(region, scenario, Year) %>%
  summarise(value_sum = sum(value), .groups = "drop") %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060))

all_scenario_data <- bind_rows(all_scenario_data, all_forest_rows)

# Process ssp585 scenario
file_path <- "/---/ssp585_detail_land_allocation.xlsx"

sheet_names <- excel_sheets(file_path)

all_forest_rows <- data.frame()

for (sheet in sheet_names) {
  
  sheet_data <- read_excel(file_path, sheet = sheet)
  
  forest_rows <- sheet_data %>%
    filter(grepl("Forest_", LandLeaf) | grepl("Tree", LandLeaf))
  
  all_forest_rows <- rbind(all_forest_rows, forest_rows)
}

all_forest_rows$scenario <- "ssp585"
all_forest_rows <- all_forest_rows %>%
  group_by(region, scenario, Year) %>%
  summarise(value_sum = sum(value), .groups = "drop") %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060))


all_scenario_data <- bind_rows(all_scenario_data, all_forest_rows)

# Join forest data with residue factors
residue_factors <- read_excel("/---/forest residue factor.xlsx", sheet = 1)

final_result <- all_scenario_data %>%
  left_join(residue_factors, by = "region")
final_result <- final_result %>%
  mutate(forest_residues_prod_unit = "1e5 t")  
forest_residue_heat_value <- 16.85 

# Perform Monte Carlo simulation to estimate forest residue mass and energy
set.seed(123)  
n_simulations <- 1000
lower_bound <- 15.9
upper_bound <- 17.8
mean_value <- (lower_bound + upper_bound) / 2
std_dev <- (upper_bound - lower_bound) / 4


simulated_results <- list()

for (i in 1:n_simulations) {
  sampled_heat_value <- rnorm(1, mean = mean_value, sd = std_dev)
  
  result_temp <- final_result %>%
    rowwise() %>%
    mutate(
      sampled_residue_factor = rnorm(
        1,
        mean = (forest_residue_t_per_ha_lower + forest_residue_t_per_ha_upper) / 2,
        sd   = (forest_residue_t_per_ha_upper - forest_residue_t_per_ha_lower) / 4
      ),
      forest_residues_mass = value_sum * sampled_residue_factor * collection_ratio_withFPC *  1e4,       
      forest_residues_EJ   = forest_residues_mass * sampled_heat_value / 1e9                     
    ) %>%
    ungroup() %>%
    select(region, scenario, Year, forest_residues_mass, forest_residues_EJ)
  
  result_temp$sim_id <- i
  simulated_results[[i]] <- result_temp
}

simulated_data <- bind_rows(simulated_results)
# Summarize forest residue data by region, scenario, and year with 95% confidence intervals
EJ_summary_try <- simulated_data %>%
  group_by(region, scenario, Year) %>%
  summarise(
    mean_mass = mean(forest_residues_mass, na.rm = TRUE),
    lower_mass_95CI = quantile(forest_residues_mass, 0.025, na.rm = TRUE),
    upper_mass_95CI = quantile(forest_residues_mass, 0.975, na.rm = TRUE),
    
    mean_EJ = mean(forest_residues_EJ, na.rm = TRUE),
    lower_EJ_95CI = quantile(forest_residues_EJ, 0.025, na.rm = TRUE),
    upper_EJ_95CI = quantile(forest_residues_EJ, 0.975, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(EJ_summary_try,
          "/---/FR_region.csv",
          row.names = FALSE)

# Calculate global forest residue totals
global_totals <- simulated_data %>%
  group_by(scenario, Year, sim_id) %>%
  summarise(
    total_mass = sum(forest_residues_mass, na.rm = TRUE),
    total_EJ   = sum(forest_residues_EJ,   na.rm = TRUE),
    .groups = "drop"
  )


global_summary <- global_totals %>%
  group_by(scenario, Year) %>%
  summarise(
    mean_mass = mean(total_mass, na.rm = TRUE),
    lower_mass_95CI = quantile(total_mass, 0.025, na.rm = TRUE),
    upper_mass_95CI = quantile(total_mass, 0.975, na.rm = TRUE),
    
    mean_EJ = mean(total_EJ, na.rm = TRUE),
    lower_EJ_95CI = quantile(total_EJ, 0.025, na.rm = TRUE),
    upper_EJ_95CI = quantile(total_EJ, 0.975, na.rm = TRUE),
    .groups = "drop"
  )


write.csv(global_summary,"/---/FR_global.csv",row.names = FALSE)





# Calculate the quantity of livestock residues----
library(readxl)
library(dplyr)
library(tidyr)
# --- Process Livestock Data for All SSP Scenarios ---
scenarios <- c("ssp126", "ssp245", "ssp445", "ssp585")
base_path <- "/---/"
file_names <- paste0(scenarios, "/", scenarios, "_meat_and_diary.xlsx")

all_livestock_data <- list()
# Loop through each scenario to read and process livestock data
for (i in seq_along(file_names)) {

  file_path <- paste0(base_path, file_names[i])
  data_raw <- read_excel(file_path)
  
  data_filtered <- data_raw %>%
    filter(Year >= 2025, Year <= 2060) %>%
    mutate(scenario = scenarios[i]) %>%
    group_by(region, scenario, Year, output, Units) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = output,
      values_from = value,
      values_fill = 0
    ) %>%
    select(
      scenario,
      region,
      Year,
      Beef, Dairy, Pork, Poultry, SheepGoat,
      Units
    )
    all_livestock_data[[scenarios[i]]] <- data_filtered
}

final_livestock_data <- bind_rows(all_livestock_data)

# --- Save Processed Livestock Data by Region ---
library(readr)

output_dir <- "/---/"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

region_list <- split(final_livestock_data, final_livestock_data$region)

for (region_name in names(region_list)) {
  
  region_data <- region_list[[region_name]] %>%
    select(
      scenario,
      Year,
      Beef, Dairy, Pork, Poultry, SheepGoat,
      Units
    ) %>%
    arrange(Year)  
  safe_region_name <- gsub("[^a-zA-Z0-9_]", "_", region_name)  
  file_path <- paste0(output_dir, "GCAM_", safe_region_name, ".csv")
  write_csv(region_data, file_path)
}

# --- Livestock Residue Modeling for Africa_Eastern (GAMs modelling)----

library(tidyverse)
library(caret)  
library(mgcv)
data <- read.csv("/---/Africa_Eastern.csv")

# Split data into training (70%) and testing sets
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]

# Set seed for reproducibility
set.seed(123) 

# Define number of folds for k-fold cross-validation
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store performance metrics
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)

# Perform k-fold cross-validation
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  
  # Fit Generalized Additive Model (GAM) with smoothing splines for each livestock type
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  
  # Make predictions on validation fold
  predictions <- predict(gam_model, newdata=valid_fold)
  
  # Calculate performance metrics
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}

# Compute average performance metrics
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

# Fit final GAM model on full training data
set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Africa_Eastern.csv")

# Convert livestock production values to appropriate units (e.g., tons)
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
# Predict livestock residues with standard errors
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
# Convert predicted residues to energy units (EJ) using a heat value of 15.68 MJ/kg
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Africa_Eastern.csv", row.names = FALSE)

# --- Livestock Residue Modeling for Africa_Northern ----
data <- read.csv("/---/Africa_Northern.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k)
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)


set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Africa_Northern.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Africa_Northern.csv", row.names = FALSE)

# --- Livestock Residue Modeling for Africa_Southern ----
data <- read.csv("/---/Africa_Southern.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))


set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Africa_Southern.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Africa_Southern.csv", row.names = FALSE)


#Africa_Western-----------------------------------------------------------
data <- read.csv("/---/Africa_Western.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Africa_Western.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Africa_Western.csv", row.names = FALSE)

#Argentina-----------------------------------------------------------
data <- read.csv("/---/Argentina.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 5) + s(Dairy, k = 5) + s(Pork, k = 5) + s(Poultry, k = 5) + s(SheepGoat, k = 5), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 3) + s(Dairy, k = 3) + s(Pork, k = 3) + s(Poultry, k = 3) + s(SheepGoat, k = 3), data = trainData)
newData <- read.csv("/---/GCAM_Argentina.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Argentina.csv", row.names = FALSE)

#Australia_NZ-----------------------------------------------------------
data <- read.csv("/---/Australila_NZ.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))


set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Australia_NZ.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Australia_NZ.csv", row.names = FALSE)

#Brazil-----------------------------------------------------------
data <- read.csv("/---/Brazil.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Brazil.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Brazil.csv", row.names = FALSE)

#Canada-----------------------------------------------------------
data <- read.csv("/--/Canada.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Canada.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Canada.csv", row.names = FALSE)

#Central America and the Caribbean---------------------------------------------------
data <- read.csv("/---/Central America and the Caribbean.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))


set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Central_America_and_Caribbean.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Central_America_and_Caribbean.csv", row.names = FALSE)

#Central Asia-----------------------------------------------------------
data <- read.csv("/---/Central Asia.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))


set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Central_Asia.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Central_Asia.csv", row.names = FALSE)

#China --------------------------------------------------------
data <- read.csv("/---/China (except Taiwan).csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_China.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_China.csv", row.names = FALSE)

#Colombia---------------------------------------------------------
data <- read.csv("/---/Colombia.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Colombia.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Colombia.csv", row.names = FALSE)

#EU_12----------------------------------------------------------
data <- read.csv("/---/EU_12.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))


set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_EU_12.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_EU-12.csv", row.names = FALSE)

#EU_15--------------------------------------------------------
data <- read.csv("/---/EU_15.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_EU_15.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_EU-15.csv", row.names = FALSE)

#Europe_Eastern----------------------------------------------------------
data <- read.csv("/---/Europe_Eastern.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Europe_Eastern.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Europe_Eastern.csv", row.names = FALSE)

#Europe_Non_EU-----------------------------------------------------------
data <- read.csv("/---/Europe_Non_EU.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k)
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 3) + s(Dairy, k = 3) + s(Pork, k = 3) + s(Poultry, k = 3) + s(SheepGoat, k = 3), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~  s(Beef, k = 3) + s(Dairy, k = 3) + s(Pork, k = 3) + s(Poultry, k = 3) + s(SheepGoat, k = 3), data = trainData)
newData <- read.csv("/---/GCAM_Europe_Non_EU.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Europe_Non_EU.csv", row.names = FALSE)

#European Fee Trade Association-----------------------------------------------------------
data <- read.csv("/---/European Fee Trade Association.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_European_Free_Trade_Association.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_European_Free_Trade_Association.csv", row.names = FALSE)

#India-----------------------------------------------------------
data <- read.csv("/---/India.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_India.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_India.csv", row.names = FALSE)

#Indonesia---------------------------------------------------------
data <- read.csv("/---/Indonesia.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Indonesia.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Indonesia.csv", row.names = FALSE)

#Japan--------------------------------------------------------
data <- read.csv("/---/Japan.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Japan.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Japan.csv", row.names = FALSE)

#Mexico----------------------------------------------------------
data <- read.csv("/---/Mexico.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}

mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Mexico.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Mexico.csv", row.names = FALSE)

#Middle East--------------------------------------------------------
data <- read.csv("/---/Middle East.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
  
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Middle_East.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Middle_East.csv", row.names = FALSE)

#Pakistan-----------------------------------------------------------
data <- read.csv("/---/Pakistan.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) +  s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Pakistan.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Pakistan.csv", row.names = FALSE)

#Russia-----------------------------------------------------------
data <- read.csv("/---/Russia.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Russia.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Russia.csv", row.names = FALSE)

#South Africa----------------------------------------------------------
data <- read.csv("/---/South Africa.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_South_Africa.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_South_Africa.csv", row.names = FALSE)

#South America_Northern-----------------------------------------------------------
data <- read.csv("/---/South America_Northern.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_South_America_Northern.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_South_America_Northern.csv", row.names = FALSE)

#South America_Southern------------------------------------------------------
data <- read.csv("/---/South America_Southern.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_South_America_Southern.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_South_America_Southern.csv", row.names = FALSE)

#South Asia----------------------------------------------------------
data <- read.csv("/---/South Asia.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_South_Asia.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_South_Asia.csv", row.names = FALSE)

#South Korea----------------------------------------------------------
data <- read.csv("/---/South Korea.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k)
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_South_Korea.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_South_Korea.csv", row.names = FALSE)

#Southeast Asia----------------------------------------------------------
data <- read.csv("/---/Southeast Asia.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Southeast_Asia.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Southeast_Asia.csv", row.names = FALSE)

#Taiwan----------------------------------------------------------
data <- read.csv("/---/Taiwan.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123)
final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)
newData <- read.csv("/---/GCAM_Taiwan.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_Taiwan.csv", row.names = FALSE)

#USA------------------------------------------------------
data <- read.csv("/---/USA.csv")
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)
mse_values <- numeric(k) 
rmse_values <- numeric(k)
mae_values <- numeric(k)
r_squared_values <- numeric(k)
explained_deviance_values <- numeric(k)
for(i in 1:k) {
  train_fold <- trainData[folds[[i]], ]
  valid_fold <- trainData[-folds[[i]], ]
  gam_model <- gam(LRs ~ s(Beef, k = 5) + s(Dairy, k = 5) + s(Pork, k = 5) + s(Poultry, k = 5) + s(SheepGoat, k = 5), data = train_fold)
  predictions <- predict(gam_model, newdata=valid_fold)
  mse_values[i] <- mean((predictions - valid_fold$LRs)^2)
  rmse_values[i] <- sqrt(mse_values[i])
  mae_values[i] <- mean(abs(predictions - valid_fold$LRs))
  sst <- sum((valid_fold$LRs - mean(valid_fold$LRs))^2)
  ssr <- sum((predictions - valid_fold$LRs)^2)
  r_squared_values[i] <- 1 - ssr / sst
  explained_deviance_values[i] <- summary(gam_model)$r.sq
}
mean_mse <- mean(mse_values)
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)
mean_explained_deviance_values <- mean(explained_deviance_values)
print(paste("mean_MSE:", mean_mse))
print(paste("mean_RMSE:", mean_rmse))
print(paste("mean_MAE:", mean_mae))
print(paste("mean_R-squared:", mean_r_squared))
print(paste("mean_explained_deviance_values:", mean_explained_deviance_values))

set.seed(123) 
final_gam_model <- gam(LRs ~ s(Beef, k = 5) + s(Dairy, k = 5) + s(Pork, k = 5) + s(Poultry, k = 5) + s(SheepGoat, k = 5), data = trainData)
newData <- read.csv("/---/GCAM_USA.csv")
newData <- newData %>%
  mutate(across(c(Beef, Dairy, Pork, Poultry, SheepGoat),
                ~ . * 1e6))
predictions_with_se <- predict(final_gam_model, newdata = newData, se.fit = TRUE)
newData$LRs_predicted <- predictions_with_se$fit
newData$SE <- predictions_with_se$se.fit
newData$LRs_lower <- newData$LRs_predicted - 1.96 * newData$SE
newData$LRs_upper <- newData$LRs_predicted + 1.96 * newData$SE
newData <- newData %>%
  mutate(
    LRs_predicted_EJ = LRs_predicted * 15.68 / 1e12,
    LRs_lower_EJ = LRs_lower * 15.68 / 1e12,
    LRs_upper_EJ = LRs_upper * 15.68 / 1e12
  )
write.csv(newData, "/---/GCAM_USA.csv", row.names = FALSE)



# Read and merge predicted LRs data ------
LR_Africa_Eastern <- read.csv("/---/GCAM_Africa_Eastern.csv")
LR_Africa_Northern <- read.csv("/---/GCAM_Africa_Northern.csv")
LR_Africa_Western <- read.csv("/---/GCAM_Africa_Western.csv")
LR_Africa_Southern <- read.csv("/---/GCAM_Africa_Southern.csv")

LR_Argentina <- read.csv("/---/GCAM_Argentina.csv")
LR_Australia_NZ <- read.csv("/---/GCAM_Australia_NZ.csv")
LR_Brazil <- read.csv("/---/GCAM_Brazil.csv")
LR_Canada <- read.csv("/---/GCAM_Canada.csv")

LR_Central_America_and_Caribbean <- read.csv("/---/GCAM_Central_America_and_Caribbean.csv")
LR_Central_Asia <- read.csv("/---/GCAM_Central_Asia.csv")
LR_China <- read.csv("/---/GCAM_China.csv")
LR_Colombia <- read.csv("/---/GCAM_Colombia.csv")

LR_EU_12 <- read.csv("/---/GCAM_EU_12.csv")
LR_EU_15 <- read.csv("/---/GCAM_EU_15.csv")
LR_Europe_Eastern <- read.csv("/---/GCAM_Europe_Eastern.csv")
LR_Europe_Non_EU <- read.csv("/---/GCAM_Europe_Non_EU.csv")

LR_European_Free_Trade_Association <- read.csv("/---/GCAM_European_Free_Trade_Association.csv")
LR_India <- read.csv("/---/GCAM_India.csv")
LR_Indonesia <- read.csv("/---/GCAM_Indonesia.csv")
LR_Japan <- read.csv("/---/GCAM_Japan.csv")

LR_Mexico <- read.csv("/---/GCAM_Mexico.csv")
LR_Middle_East <- read.csv("/---/GCAM_Middle_East.csv")
LR_Pakistan <- read.csv("/---/GCAM_Pakistan.csv")
LR_Russia <- read.csv("/---/GCAM_Russia.csv")

LR_South_Africa <- read.csv("/---/GCAM_South_Africa.csv")
LR_South_America_Northern <- read.csv("/---/GCAM_South_America_Northern.csv")
LR_South_America_Southern <- read.csv("/---/GCAM_South_America_Southern.csv")

LR_South_Asia <- read.csv("/---/GCAM_South_Asia.csv")
LR_South_Korea <- read.csv("/---/GCAM_South_Korea.csv")

LR_Southeast_Asia <- read.csv("/---/GCAM_Southeast_Asia.csv")
LR_Taiwan <- read.csv("/---/GCAM_Taiwan.csv")

LR_USA <- read.csv("/---/GCAM_USA.csv")

library(dplyr)
df_names <- ls(pattern = "^LR_")
df_list <- lapply(df_names, function(n) {
  df <- get(n)                          
  df$region <- sub("^LR_", "", n)      
  df
})
LR_allregion <- bind_rows(df_list)
LR_allregion <- LR_allregion  %>%
  select(- Units) %>%              
  mutate(
    unit_mass = "kg",                       
    unit_energy = "EJ"  
  )

write.csv(LR_allregion,
          "/---/LR_region.csv",
          row.names = FALSE)

LR_global <- LR_allregion %>%
  group_by(scenario, Year) %>%
  summarise(
    LRs_predicted    = sum(LRs_predicted,    na.rm = TRUE),
    LRs_lower    = sum(LRs_lower,    na.rm = TRUE),
    LRs_upper    = sum(LRs_upper,    na.rm = TRUE),
    LRs_predicted_EJ = sum(LRs_predicted_EJ, na.rm = TRUE),
    LRs_lower_EJ    = sum(LRs_lower_EJ,    na.rm = TRUE),
    LRs_upper_EJ    = sum(LRs_upper_EJ,    na.rm = TRUE)
  ) 
write.csv(LR_global, "/---/LR_global.csv",  row.names = FALSE)

#over