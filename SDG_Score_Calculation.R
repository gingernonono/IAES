
# Calculate SDGs scores for agricultural and forestry residues (AFRs) across scenarios, years, and regions----
# by processing CRs, LRs, and FRs, applying proxies, and computing normalized and weighted scores.
library(dplyr)
library(readr) 
library(readxl)
library(openxlsx)
library(purrr)

# Load and preprocess crop residues (CR) data, including renaming columns, assigning development levels,
# and calculating energy potential based on scenario-specific factors.
CR_region <- read_csv("/---/CR_region.csv")
CR_region <- CR_region %>%
  select(-region_code, -unit_energy, -total_energy_avg, -total_energy_min, -total_energy_max)  %>%
  rename(
    year = Year,
    mass_lower = total_mass_min,
    mass_mean = total_mass_avg,
    mass_upper = total_mass_max)  %>%
  mutate(biomass_type = "CR",
         mass_ener = 0,
         mass_ener_lower = 0,
         mass_ener_upper = 0  )  %>% 
  mutate(region = case_when(
    region == "Central America and Caribbean" ~ "Central_America_and_Caribbean",
    region == "Central Asia" ~ "Central_Asia",
    region == "EU-12" ~ "EU_12",
    region == "EU-15" ~ "EU_15",
    region == "European Free Trade Association" ~ "European_Free_Trade_Association",
    region == "Middle East" ~ "Middle_East",
    region == "South Africa" ~ "South_Africa",
    region == "South America_Northern" ~ "South_America_Northern",
    region == "South America_Southern" ~ "South_America_Southern",
    region == "South Asia" ~ "South_Asia",
    region == "South Korea" ~ "South_Korea",
    region == "Southeast Asia" ~ "Southeast_Asia",
    TRUE              ~ region  )) %>%
  mutate(dev_level = ifelse(region %in% c("Africa_Eastern", "Africa_Northern", 
                                          "Africa_Southern", "Africa_Western",
                                          "India", "Pakistan", "South_America_Northern",
                                          "South_Asia"), "LUMI", "HUMI")) %>%
  
  select(scenario, year, region, dev_level, biomass_type, mass_mean, mass_lower, 
         mass_upper, mass_ener, mass_ener_lower, mass_ener_upper,
         unit_mass) 

CR_region <- CR_region %>%
  
    mutate(
    mass_mean  = mass_mean  * 1e6,
    mass_lower = mass_lower * 1e6,
    mass_upper = mass_upper * 1e6,
    unit_mass = "t"
  ) %>%
    mutate(
    ener_factor = case_when(
      scenario == "ssp126"                          ~ 0.30,
      scenario == "ssp245"                          ~ 0.20,
      scenario == "ssp445" & dev_level == "HUMI"    ~ 0.22,
      scenario == "ssp445" & dev_level == "LUMI"    ~ 0.18,
      scenario == "ssp585" & dev_level == "HUMI"    ~ 0.10,
      TRUE                                          ~ 0  
    ),
    mass_ener        = mass_mean  * ener_factor,
    mass_ener_lower  = mass_lower * ener_factor,
    mass_ener_upper  = mass_upper * ener_factor
  ) %>%
  select(-ener_factor)

CR_region_HL <- CR_region %>% 
  group_by(scenario, year,dev_level, biomass_type) %>%
  summarise(
    mass_ener  = sum(mass_ener,  na.rm = TRUE),
    mass_ener_lower = sum(mass_ener_lower, na.rm = TRUE),
    mass_ener_upper = sum(mass_ener_upper, na.rm = TRUE),
    .groups = "drop"
  ) 

# Load population and energy demand data and join with CR data for further analysis.
pop_ener_dem <- read_csv("/---/pop_ener_dem.csv")
pop_ener_dem <- pop_ener_dem %>%
  select(scenario, year, region, pop_1e6, pop_low_inc_1e6, final_ener_dem_EJ)
pop_ener_dem <- pop_ener_dem %>%
  mutate(year = as.character(year), scenario = as.character(scenario), region = as.character(region)) %>%
  mutate(scenario = tolower(scenario))

CR_region <- CR_region %>%
  mutate(year = as.character(year), scenario = as.character(scenario), region = as.character(region))  %>%
  mutate(scenario = tolower(scenario))
CR_region <- left_join(CR_region, pop_ener_dem, by = c("scenario", "year", "region"))

CR_global <- CR_region %>%
  group_by(scenario, year) %>%
  summarise(
    mass_mean  = sum(mass_mean,  na.rm = TRUE),
    mass_lower = sum(mass_lower, na.rm = TRUE),
    mass_upper = sum(mass_upper, na.rm = TRUE),
    mass_ener  = sum(mass_ener,  na.rm = TRUE),
    mass_ener_lower = sum(mass_ener_lower, na.rm = TRUE),
    mass_ener_upper = sum(mass_ener_upper, na.rm = TRUE),
    pop_1e6 = sum(pop_1e6,  na.rm = TRUE),
    pop_low_inc_1e6 = sum(pop_low_inc_1e6,  na.rm = TRUE),
    final_ener_dem_EJ = sum(final_ener_dem_EJ,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "global",
    dev_level = "HUMI",             
    biomass_type = "CR",      
    unit_mass = "t"
  ) %>%
  select(colnames(CR_region))   

CR_all <- bind_rows(CR_region, CR_global)

# Load and preprocess livestock residues (LR) data, adjusting units and calculating energy potential.
LR_region <- read_csv("/---/LR_region.csv")
LR_region <- LR_region %>%
  select(-Beef, -Dairy, -Pork, -Poultry, -SheepGoat, -SE, -LRs_predicted_EJ,
         -LRs_lower_EJ, -LRs_upper_EJ, -unit_energy)  %>%
  rename(
    year = Year,
    mass_lower = LRs_lower,
    mass_mean = LRs_predicted,
    mass_upper = LRs_upper)  %>%
  mutate(biomass_type = "LR",
         mass_ener = 0,
         mass_ener_lower = 0,
         mass_ener_upper = 0  )  %>% 
  mutate(dev_level = ifelse(region %in% c("Africa_Eastern", "Africa_Northern", 
                                          "Africa_Southern", "Africa_Western",
                                          "India", "Pakistan", "South_America_Northern",
                                          "South_Asia"), "LUMI", "HUMI")) %>%
  select(scenario, year, region, dev_level, biomass_type, mass_mean, mass_lower, 
         mass_upper, mass_ener, mass_ener_lower, mass_ener_upper,
         unit_mass) 

LR_region <- LR_region %>%
  mutate(
    mass_mean  = mass_mean  / 1e3,
    mass_lower = mass_lower / 1e3,
    mass_upper = mass_upper / 1e3,
    unit_mass = "t"
  ) %>%
  mutate(
    ener_factor = case_when(
      scenario == "ssp126"                          ~ 0.90,
      scenario == "ssp245"                          ~ 0.70,
      scenario == "ssp445" & dev_level == "HUMI"    ~ 0.77,
      scenario == "ssp445" & dev_level == "LUMI"    ~ 0.63,
      scenario == "ssp585" & dev_level == "HUMI"    ~ 0.50,
      TRUE                                          ~ 0  
    ),
    mass_ener        = mass_mean  * ener_factor,
    mass_ener_lower  = mass_lower * ener_factor,
    mass_ener_upper  = mass_upper * ener_factor
  ) %>%
  select(-ener_factor)

LR_region <- LR_region %>%
  mutate(year = as.character(year), scenario = as.character(scenario), region = as.character(region))  %>%
  mutate(scenario = tolower(scenario))
LR_region <- left_join(LR_region, pop_ener_dem, by = c("scenario", "year", "region"))

LR_global <- LR_region %>%
  group_by(scenario, year) %>%
  summarise(
    mass_mean  = sum(mass_mean,  na.rm = TRUE),
    mass_lower = sum(mass_lower, na.rm = TRUE),
    mass_upper = sum(mass_upper, na.rm = TRUE),
    mass_ener  = sum(mass_ener,  na.rm = TRUE),
    mass_ener_lower = sum(mass_ener_lower, na.rm = TRUE),
    mass_ener_upper = sum(mass_ener_upper, na.rm = TRUE),
    pop_1e6 = sum(pop_1e6,  na.rm = TRUE),
    pop_low_inc_1e6 = sum(pop_low_inc_1e6,  na.rm = TRUE),
    final_ener_dem_EJ = sum(final_ener_dem_EJ,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "global",
    dev_level = "HUMI",             
    biomass_type = "LR",      
    unit_mass = "t"
  ) %>%
  select(colnames(LR_region))   
LR_all <- bind_rows(LR_region, LR_global)

# Load and preprocess forestry residues (FR) data, adjusting region names and calculating energy potential.
FR_region <- read_csv("/---/FR_region.csv")
FR_region <- FR_region %>%
  select( -mean_EJ, -lower_EJ_95CI, -upper_EJ_95CI)  %>%
  rename(
    year = Year,
    mass_lower = lower_mass_95CI,
    mass_mean = mean_mass,
    mass_upper = upper_mass_95CI)  %>%
  mutate(biomass_type = "FR",
         unit_mass = "t",
         mass_ener = 0,
         mass_ener_lower = 0,
         mass_ener_upper = 0  )  %>% 
  mutate(region = case_when(
    region == "Central America and Caribbean" ~ "Central_America_and_Caribbean",
    region == "Central Asia" ~ "Central_Asia",
    region == "EU-12" ~ "EU_12",
    region == "EU-15" ~ "EU_15",
    region == "European Free Trade Association" ~ "European_Free_Trade_Association",
    region == "Middle East" ~ "Middle_East",
    region == "South Africa" ~ "South_Africa",
    region == "South America_Northern" ~ "South_America_Northern",
    region == "South America_Southern" ~ "South_America_Southern",
    region == "South Asia" ~ "South_Asia",
    region == "South Korea" ~ "South_Korea",
    region == "Southeast Asia" ~ "Southeast_Asia",
    TRUE              ~ region  )) %>%
  mutate(dev_level = ifelse(region %in% c("Africa_Eastern", "Africa_Northern", 
                                          "Africa_Southern", "Africa_Western",
                                          "India", "Pakistan", "South_America_Northern",
                                          "South_Asia"), "LUMI", "HUMI")) %>%
  select(scenario, year, region, dev_level, biomass_type, mass_mean, mass_lower, 
         mass_upper, mass_ener, mass_ener_lower, mass_ener_upper,
         unit_mass) 
FR_region <- FR_region %>% 
  mutate(
    ener_factor = case_when(
      scenario == "ssp126"                          ~ 0.30,
      scenario == "ssp245"                          ~ 0.20,
      scenario == "ssp445" & dev_level == "HUMI"    ~ 0.22,
      scenario == "ssp445" & dev_level == "LUMI"    ~ 0.18,
      scenario == "ssp585" & dev_level == "HUMI"    ~ 0.10,
      TRUE                                          ~ 0  
    ),
    mass_ener        = mass_mean  * ener_factor,
    mass_ener_lower  = mass_lower * ener_factor,
    mass_ener_upper  = mass_upper * ener_factor
  ) %>%
  select(-ener_factor)
FR_region <- FR_region %>%
  mutate(year = as.character(year), scenario = as.character(scenario), region = as.character(region))  %>%
  mutate(scenario = tolower(scenario))
FR_region <- left_join(FR_region, pop_ener_dem, by = c("scenario", "year", "region"))

FR_global <- FR_region %>%
  group_by(scenario, year) %>%
  summarise(
    mass_mean  = sum(mass_mean,  na.rm = TRUE),
    mass_lower = sum(mass_lower, na.rm = TRUE),
    mass_upper = sum(mass_upper, na.rm = TRUE),
    mass_ener  = sum(mass_ener,  na.rm = TRUE),
    mass_ener_lower = sum(mass_ener_lower, na.rm = TRUE),
    mass_ener_upper = sum(mass_ener_upper, na.rm = TRUE),
    pop_1e6 = sum(pop_1e6,  na.rm = TRUE),
    pop_low_inc_1e6 = sum(pop_low_inc_1e6,  na.rm = TRUE),
    final_ener_dem_EJ = sum(final_ener_dem_EJ,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    region = "global",
    dev_level = "HUMI",             
    biomass_type = "FR",      
    unit_mass = "t"
  ) %>%
  select(colnames(FR_region))   

FR_all <- bind_rows(FR_region, FR_global)

# Combine CR, LR, and FR data into a single AFR dataset and aggregate by development level.
AFR_region <- bind_rows(CR_all, LR_all, FR_all)

AFR_region_HL <- AFR_region %>% 
  group_by(scenario, year,dev_level, biomass_type) %>%
  summarise(
    mass_ener  = sum(mass_ener,  na.rm = TRUE),
    mass_ener_lower = sum(mass_ener_lower, na.rm = TRUE),
    mass_ener_upper = sum(mass_ener_upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate( unit_mass = "t" )
write.csv(AFR_region_HL, "/---/AFR_region_HL.csv")

write.csv(AFR_region, "/---/AFR_region.csv")



# Import genetic algorithm (GA) optimization results and process them for proxy calculations.
library(readxl)

library(purrr)

file <- "/---/GArawdata_res.xlsx"

sheets <- excel_sheets(file)
proxydata <- map_df(sheets, function(scn) {
  read_excel(
    path       = file,
    sheet      = scn,
    range      = "Q2:T17",    
    col_names  = TRUE 
  ) %>% 
    mutate(scenario = scn)     
}) %>%
  select(scenario, proxy, CR, LR, FR)


# Transform GA results into a long format, extracting scenario, year, and development level information.
library(tidyr)

proxydata2 <- proxydata %>%
  rename(raw_scenario = scenario) %>%
  extract(
    col   = raw_scenario,
    into  = c("scenario","dev_level","year"),
    regex = "^(ssp\\d+)(?:_(HUMI|LUMI))?_(\\d{4}|allyear)$",
    remove= TRUE
  ) %>%
  mutate(
    dev_level = na_if(dev_level, ""),
    year      = na_if(year,      ""),
    dev_level = coalesce(dev_level, "all"),
    year      = coalesce(year,      "all"),
    year      = if_else(year == "allyear", "all", year)
  )

proxydata3 <- proxydata2 %>%
  pivot_longer(
    cols      = c(CR, LR, FR),
    names_to  = "biomass_type",
    values_to = "proxyvalue"
  ) %>%
  select(scenario, year, dev_level, proxy, biomass_type, proxyvalue)




# Expand proxy data to cover all years and development levels for consistent application.
library(tidyr)

years      <- unique(AFR_region$year)
dev_levels <- unique(AFR_region$dev_level)

proxy_expand_year <- proxydata3 %>% 
  filter(year == "all") %>% 
  select(-year) %>%              
  crossing(year = years)     

proxy_fixed_year  <- proxydata3 %>% filter(year != "all")

proxy_year_expanded <- bind_rows(proxy_fixed_year, proxy_expand_year)

proxy_expand_dev <- proxy_year_expanded %>% 
  filter(dev_level == "all") %>%
  select(-dev_level) %>%
  crossing(dev_level = dev_levels)

proxy_fixed_dev  <- proxy_year_expanded %>% filter(dev_level != "all")

proxy_expanded <- bind_rows(proxy_fixed_dev, proxy_expand_dev)
write.csv(proxy_expanded, "/---/proxy_expanded.csv")

AFR_with_proxies <- AFR_region %>%
  left_join(proxy_expanded,
            by = c("scenario","year","dev_level","biomass_type")) %>%

    mutate(
    m = proxyvalue * mass_ener,
    l = proxyvalue * mass_ener_lower,
    u = proxyvalue * mass_ener_upper
  ) %>%
  select(-proxyvalue) %>%
  pivot_wider(
    names_from   = proxy,
    values_from  = c(m, l, u),
    values_fill  = 0,
    names_glue   = "{proxy}_{.value}"  
  )  %>% select(-NA_m, -NA_l, -NA_u,) 

write.csv(AFR_with_proxies, "/---/AFR_with_proxies.csv")

# Adjust AFR data with RAI factors and recalculate specific indicators.
RAI_region <- read_csv("/---/PPP_region.csv")

AFR_adjusted <- AFR_with_proxies %>%
  left_join(RAI_region, by = "region") %>%
  mutate(
    biomass_type = recode(biomass_type,
                          LR = "LE",
                          .default = biomass_type)
  ) %>%
  mutate(across(
    .cols = c(
      "s1_soc_added_inc_m",
      "s10_soc_added_inc_m",
      
      
      "s1_soc_added_inc_l",
      "s10_soc_added_inc_l",
     
      "s1_soc_added_inc_u",
      "s10_soc_added_inc_u",
      
    ),
    .fns  = ~ .  * new_inc_low
  )) %>% 
  mutate(across(
    c(
      "s1_soc_added_inc_m", "s10_soc_added_inc_m",
      "s1_soc_added_inc_l", "s10_soc_added_inc_l",
      "s1_soc_added_inc_u", "s10_soc_added_inc_u"
    ),
    ~ if_else(biomass_type %in% c("LE", "FR"), 0, .x)
  )) %>% 
  mutate(across(
    .cols = c(
    
      "s7_eco_prof_m",

      "s7_eco_prof_l",
     
      "s7_eco_prof_u",
      
    ),
    .fns  = ~ .  / RAI_p
  )) %>%

  mutate(across(
    .cols = c(
      "s9_soc_added_val_m",
      "s9_soc_added_val_l",
      "s9_soc_added_val_u",
    ),
    .fns  = ~ .  / RAI_i
  )) %>%
  mutate(across(
    .cols = c(
      "s15_soc_envmon_m",

      "s15_soc_envmon_l",

      "s15_soc_envmon_u",
    ),
    .fns  = ~ . 
  )) %>%
  select(-PPP, -RAI_p, -RAI_i, -new_inc_low) %>% 
  
  mutate(
    across(
      c(
        "s11_ene_elec_eth_m", "s11_env_total_m", "s12_env_biowas_m",
        "s13_env_clim_m",     "s14_env_mar_m",    "s15_env_ecosys_m",
        "s15_env_frewat_m",   "s15_soc_envmon_m",  "s3_env_total_m",
        "s6_env_frewat_m",    "s7_eco_prof_m",     "s9_soc_added_val_m"
      ),
      ~ . / pop_1e6 / 1e6,
      .names = "{.col}_ave"
    ),
    across(
      c("s1_soc_added_inc_m", "s10_soc_added_inc_m"),
      ~ . / pop_low_inc_1e6 / 1e6,
      .names = "{.col}_ave"
    ),
    s7_ene_output_m_ave = s7_ene_output_m / final_ener_dem_EJ / 1e12
  ) %>%
  mutate(
    across(
      c(
        "s11_ene_elec_eth_l", "s11_env_total_l", "s12_env_biowas_l",
        "s13_env_clim_l",     "s14_env_mar_l",    "s15_env_ecosys_l",
        "s15_env_frewat_l",   "s15_soc_envmon_l",  "s3_env_total_l",
        "s6_env_frewat_l",    "s7_eco_prof_l",     "s9_soc_added_val_l"
      ),
      ~ . / pop_1e6 / 1e6,
      .names = "{.col}_ave"
    ),
    across(
      c("s1_soc_added_inc_l", "s10_soc_added_inc_l"),
      ~ . / pop_low_inc_1e6 / 1e6,
      .names = "{.col}_ave"
    ),
    s7_ene_output_l_ave = s7_ene_output_l / final_ener_dem_EJ / 1e12
  ) %>%
  mutate(
    across(
      c(
        "s11_ene_elec_eth_u", "s11_env_total_u", "s12_env_biowas_u",
        "s13_env_clim_u",     "s14_env_mar_u",    "s15_env_ecosys_u",
        "s15_env_frewat_u",   "s15_soc_envmon_u",  "s3_env_total_u",
        "s6_env_frewat_u",    "s7_eco_prof_u",     "s9_soc_added_val_u"
      ),
      ~ . / pop_1e6 / 1e6,
      .names = "{.col}_ave"
    ),
    across(
      c("s1_soc_added_inc_u", "s10_soc_added_inc_u"),
      ~ . / pop_low_inc_1e6 / 1e6,
      .names = "{.col}_ave"
    ),
    s7_ene_output_u_ave = s7_ene_output_u / final_ener_dem_EJ / 1e12
  )

write.csv(AFR_adjusted, "/---/AFR_adjusted.csv")

# Aggregate proxy-adjusted data by scenario, year, region, and development level.
agg_proxy <- AFR_adjusted %>%
  filter(region != "global") %>%  
  group_by(scenario, year, region, dev_level) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Calculate normalization ranges for indicators based on 2025 data to standardize scores.
library(tidyr)

agg_proxy_2025 <- agg_proxy %>% 
  filter(year == 2025)
														
wanted <- c(
  "s11_ene_elec_eth_m_ave", "s11_env_total_m_ave", "s12_env_biowas_m_ave", 
  "s13_env_clim_m_ave","s14_env_mar_m_ave", "s15_env_ecosys_m_ave",
  "s15_env_frewat_m_ave","s15_soc_envmon_m_ave", "s3_env_total_m_ave",
  "s6_env_frewat_m_ave","s7_eco_prof_m_ave", "s9_soc_added_val_m_ave",
  "s1_soc_added_inc_m_ave","s10_soc_added_inc_m_ave", "s7_ene_output_m_ave"

)

ranges_with_info <- agg_proxy_2025 %>%
  pivot_longer(
    cols      = all_of(wanted),
    names_to  = "indicator",
    values_to = "value"
  ) %>%
  group_by(indicator) %>%
  summarise(
    min_value    = 0.00,
    min_scenario = scenario[which.min(value)],
    min_region   = region[which.min(value)],
    max_value    = max(value,   na.rm = TRUE),
    max_scenario = scenario[which.max(value)],
    max_region   = region[which.max(value)],
    .groups      = "drop"
  )



library(tidyr)

ranges_with_info2 <- ranges_with_info %>%
  mutate(
    max_value = max_value * 1.5
  ) %>%
  select(-min_scenario, -min_region, -max_scenario, -max_region)


library(stringr)

ranges_with_info2 <- bind_rows(
  ranges_with_info2,
  ranges_with_info2 %>%
    mutate(
      indicator = str_replace(indicator, "_m(?=_ave$)", "_l")
    )
)

ranges_with_info2 <- bind_rows(
  ranges_with_info2,
  ranges_with_info2 %>%
    filter(str_ends(indicator, "_m_ave")) %>%
    mutate(
      indicator = str_replace(indicator, "_m_ave$", "_u_ave")
    )
)

# Normalize indicator values to a 0-100 scale using predefined ranges.
normalized_long <- AFR_adjusted %>%
  pivot_longer(
    cols      = all_of(ranges_with_info2$indicator),
    names_to  = "indicator",
    values_to = "raw"
  ) %>%
  left_join(ranges_with_info2, by = "indicator") %>%
  mutate(
    score = (raw - min_value) / (max_value - min_value) * 100
  )

AFR_normalized <- normalized_long %>%
  select(-raw, -min_value, -max_value) %>%
  pivot_wider(
    names_from   = indicator,
    values_from  = score,
    names_glue   = "{indicator}_score"
  )

# Combine normalized scores with original AFR data for comprehensive analysis.
AFR_final <- AFR_adjusted %>%
  left_join(
    AFR_normalized %>%
      select(
        scenario, year, region, dev_level, biomass_type, 
        ends_with("_score")   
      ),
    by = c("scenario","year","region","dev_level","biomass_type"),
    relationship = "many-to-many"  
  )

write.csv(AFR_final, "/---/AFR_final.csv")



AFR_clamped <- AFR_final %>% 
  group_by(scenario, year, region, dev_level) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )  %>%
  mutate(across(
    .fns  = ~ case_when(
      .x <  0   ~  0,
      .x >  100 ~ 100,
      TRUE      ~ .x
    ),.cols = ends_with("_score")
  ))
write.csv(AFR_clamped, "/---/AFR_clamped.csv")

# Apply predefined weights to normalized scores to compute final weighted scores.
weight_factors <- c(
  s1_soc_added_inc_m_ave_score = 119,
  s3_env_total_m_ave_score	= 221,
  s6_env_frewat_m_ave_score	= 136,
  s7_eco_prof_m_ave_score	= 170,
  s7_ene_output_m_ave_score	= 170,
  s9_soc_added_val_m_ave_score = 136,
  s10_soc_added_inc_m_ave_score = 170,
  s11_ene_elec_eth_m_ave_score = 340,
  s11_env_total_m_ave_score = 340,
  s12_env_biowas_m_ave_score = 187,
  s13_env_clim_m_ave_score = 85,
  s14_env_mar_m_ave_score	 = 170,
  s15_env_ecosys_m_ave_score = 612,
  s15_env_frewat_m_ave_score = 612,
  s15_soc_envmon_m_ave_score = 612,
  
  s1_soc_added_inc_l_ave_score = 119,
  s3_env_total_l_ave_score	= 221,
  s6_env_frewat_l_ave_score	= 136,
  s7_eco_prof_l_ave_score	= 170,
  s7_ene_output_l_ave_score	= 170,
  s9_soc_added_val_l_ave_score = 136,
  s10_soc_added_inc_l_ave_score = 170,
  s11_ene_elec_eth_l_ave_score = 340,
  s11_env_total_l_ave_score = 340,
  s12_env_biowas_l_ave_score = 187,
  s13_env_clim_l_ave_score = 85,
  s14_env_mar_l_ave_score	 = 170,
  s15_env_ecosys_l_ave_score = 612,
  s15_env_frewat_l_ave_score = 612,
  s15_soc_envmon_l_ave_score = 612,
  
  s1_soc_added_inc_u_ave_score = 119,
  s3_env_total_u_ave_score	= 221,
  s6_env_frewat_u_ave_score	= 136,
  s7_eco_prof_u_ave_score	= 170,
  s7_ene_output_u_ave_score	= 170,
  s9_soc_added_val_u_ave_score = 136,
  s10_soc_added_inc_u_ave_score = 170,
  s11_ene_elec_eth_u_ave_score = 340,
  s11_env_total_u_ave_score = 340,
  s12_env_biowas_u_ave_score = 187,
  s13_env_clim_u_ave_score = 85,
  s14_env_mar_u_ave_score	 = 170,
  s15_env_ecosys_u_ave_score = 612,
  s15_env_frewat_u_ave_score = 612,
  s15_soc_envmon_u_ave_score = 612
)

AFR_weighted <- AFR_clamped %>%
  mutate(across(
    .cols = names(weight_factors),
    .fns  = ~ .x / weight_factors[cur_column()],
    .names = "{.col}_final"
  ))


AFR_scored_totals <- AFR_weighted %>%
  mutate(
    sdg_total_m = rowSums(across(matches("_m_ave_score_final$")), na.rm = TRUE),
    sdg_total_l = rowSums(across(matches("_l_ave_score_final$")), na.rm = TRUE),
    sdg_total_u = rowSums(across(matches("_u_ave_score_final$")), na.rm = TRUE)
  )

write.csv(AFR_scored_totals, "/---/AFR_scored_totals.csv")

# Aggregate final weighted scores by scenario, year, region, and development level.
AFR_aggregated <- AFR_scored_totals %>%
  group_by(scenario, year, region, dev_level) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
write.csv(AFR_aggregated, "/---/AFR_aggregated.csv")
