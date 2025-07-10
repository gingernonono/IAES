# Calculates for environmental impact results. This section applies the ReCiPe2016 method to quantify environmental impacts at the Midpoint level and normalize them into comparable results. -----
# Calculates environmental impacts per functional unit (1 ton of AFRs) across different scenarios and time periods, with results including uncertainty ranges.

# Load required libraries for data manipulation and analysis
library(readr)
library(tidyverse)
library(dplyr)

# --- Environmental Impact for SSP585 Scenario (2025-2060) ---
# Read emissions data for AFR technologies
emissions <- read.csv("/---/LCA_BR_Environment.csv", header = TRUE, stringsAsFactors = FALSE)
emissions <- as.matrix(emissions[ , -2])
emissions <- t(emissions)
emissions_data <- apply(emissions[-1, ], 2, as.numeric)
# Read fossil energy emissions data for comparison
Temissions <- read.csv("/---/LCA_AFR_Environment_Fossilenergy.csv", header = TRUE, stringsAsFactors = FALSE)
Temissions <- as.matrix(Temissions[ , -2])
Temissions <- t(Temissions)
Temissions_data <- apply(Temissions[-1, ], 2, as.numeric)

# Read ReCiPe2016 impact factors
impact_factors <- read.csv("/---/impact_factor.csv", header = TRUE, stringsAsFactors = FALSE)
impact_factors <- as.matrix(impact_factors[ , -2])          
impact_factors_data <- apply(impact_factors[ , -1], 2, as.numeric)
# Read normalization factors for converting impacts to comparable units
normalization_factors <- read.csv("/---/normalization_factors.csv", header = FALSE, stringsAsFactors = FALSE)
nf_raw <- as.numeric(normalization_factors[,2])
normalization_factors_data <- 1 / nf_raw
# Define technology and environmental indicator names
tech_name <- c("CHP_CR","BF_CR","Bg_CR","Be_CR","BP_CR",
               "DH_CR","CFPG_CR","BgPG_LR","Cp_LR","Bg_LR",
               "GPG_FR","BF_FR","CFPG_FR","RJF_FR" )
envi_name <- c("Global Warming","Stratospheric Ozone Depletion",
               "Photochemical Ozone Formation–human damage",
               "Fine Particulate Matter Formation",
               "Photochemical Ozone Formation–ecosystem damage",
               "Terrestrial Acidification",
               "Freshwater Eutrophication","Marine Eutrophication",
               "Fossil Resource Scarcity")
# Define parameters for lognormal uncertainty analysis
gsd     <- 2
sdlog   <- log(gsd)                  
# Calculate mean log values for emissions (adjusted for lognormal distribution)
meanlog_em <- log(emissions_data) - 0.5 * sdlog^2
meanlog_Te <- log(Temissions_data)    - 0.5 * sdlog^2
# Set seed for reproducibility and define number of Monte Carlo simulations
set.seed(123)          
n_sims <- 10000  
# Calculate original environmental impacts using ReCiPe2016 method
orig_EI   <- emissions_data %*% impact_factors_data
n_tech    <- nrow(orig_EI)
n_ind     <- ncol(orig_EI)

# Initialize array to store net environmental impact simulation results
NET_sim <- array(NA, dim = c(n_tech, n_ind, n_sims))
# Perform Monte Carlo simulation to account for uncertainty
for(i in seq_len(n_sims)) {
  samp_em <- matrix(rlnorm(length(meanlog_em), meanlog_em, sdlog),
                    nrow = nrow(emissions_data), ncol = ncol(emissions_data))
  samp_Te <- matrix(rlnorm(length(meanlog_Te), meanlog_Te, sdlog),
                    nrow = nrow(Temissions_data), ncol = ncol(Temissions_data))
  EI_s       <- samp_em %*% impact_factors_data
  EI_d_s     <- sweep(EI_s, 2, normalization_factors_data, `*`)
  T_EI_s     <- samp_Te %*% impact_factors_data
  T_EI_d_s   <- sweep(T_EI_s, 2, normalization_factors_data, `*`)
  # Calculate net environmental impacts (fossil energy - AFR)
  NET_sim[ , , i] <- T_EI_d_s - EI_d_s
}

# Compute mean and 95% confidence intervals for net impacts
mean_NET  <- apply(NET_sim, c(1,2), mean)
lower_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.025)
upper_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.975)
# Create data frame for individual environmental indicators
result_df <- bind_rows(
  lapply(seq_len(n_tech), function(j) {
    tibble(
      tech      = tech_name[j],
      indicator = envi_name,
      mean      = mean_NET[j, ],
      lower95   = lower_NET[j, ],
      upper95   = upper_NET[j, ]
    )
  })
)
# Summarize total environmental impacts across all indicators
sum_sims <- apply(NET_sim, 3, rowSums)
sum_mean  <- rowMeans(sum_sims)
sum_lower <- apply(sum_sims, 1, quantile, probs = 0.025)
sum_upper <- apply(sum_sims, 1, quantile, probs = 0.975)
# Create data frame for total environmental impacts
sum_df <- tibble(
  tech    = tech_name,
  mean    = sum_mean,
  lower95 = sum_lower,
  upper95 = sum_upper
)

EI_alltype_ssp585 <- result_df %>%
  rename(technology = tech) %>%
  mutate(
    scenario = "ssp585",
    Year     = 2020
  )

EI_total_ssp585 <- sum_df %>%
  rename(technology = tech) %>%
  mutate(
    scenario = "ssp585",
    Year     = 2020
  )

write_csv(EI_alltype_ssp585, "/---/NET_EI_indicator_uncertainty_ssp585.csv")
write_csv(EI_total_ssp585,    "/---/NET_EI_total_uncertainty_ssp585.csv")



# --- Environmental Impact for SSP126 Scenario (2025-2060) ---
library(readr)
library(tidyverse)
library(dplyr)

emissions <- read.csv("/---/LCA_AFR_Environment_ssp126.csv", header = TRUE, stringsAsFactors = FALSE)
emissions <- as.matrix(emissions[ , -2])
emissions <- t(emissions)
emissions_data <- apply(emissions[-1, ], 2, as.numeric)

Temissions <- read.csv("/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv", header = TRUE, stringsAsFactors = FALSE)
Temissions <- as.matrix(Temissions[ , -2])
Temissions <- t(Temissions)
Temissions_data <- apply(Temissions[-1, ], 2, as.numeric)

impact_factors <- read.csv("/---/impact_factor.csv", header = TRUE, stringsAsFactors = FALSE)
impact_factors <- as.matrix(impact_factors[ , -2])        
impact_factors_data <- apply(impact_factors[ , -1], 2, as.numeric)

normalization_factors <- read.csv("/---/normalization_factors.csv", header = FALSE, stringsAsFactors = FALSE)

nf_raw <- as.numeric(normalization_factors[,2])
normalization_factors_data <- 1 / nf_raw
# Define technology names for SSP126, including years 2025-2060
tech_name <- c("CHP_CR_ssp126_2025","BF_CR_ssp126_2025","Bg_CR_ssp126_2025","Be_CR_ssp126_2025","BP_CR_ssp126_2025",
               "DH_CR_ssp126_2025","CFPG_CR_ssp126_2025","BgPG_LR_ssp126_2025","Cp_LR_ssp126_2025","Bg_LR_ssp126_2025",
               "GPG_FR_ssp126_2025","BF_FR_ssp126_2025","CFPG_FR_ssp126_2025","RJF_FR_ssp126_2025",
               "CHP_CR_ssp126_2030","BF_CR_ssp126_2030","Bg_CR_ssp126_2030","Be_CR_ssp126_2030","BP_CR_ssp126_2030","DH_CR_ssp126_2030","CFPG_CR_ssp126_2030","BgPG_LR_ssp126_2030","Cp_LR_ssp126_2030","Bg_LR_ssp126_2030","GPG_FR_ssp126_2030","BF_FR_ssp126_2030","CFPG_FR_ssp126_2030","RJF_FR_ssp126_2030",
               "CHP_CR_ssp126_2035","BF_CR_ssp126_2035","Bg_CR_ssp126_2035","Be_CR_ssp126_2035","BP_CR_ssp126_2035","DH_CR_ssp126_2035","CFPG_CR_ssp126_2035","BgPG_LR_ssp126_2035","Cp_LR_ssp126_2035","Bg_LR_ssp126_2035","GPG_FR_ssp126_2035","BF_FR_ssp126_2035","CFPG_FR_ssp126_2035","RJF_FR_ssp126_2035",
               "CHP_CR_ssp126_2040","BF_CR_ssp126_2040","Bg_CR_ssp126_2040","Be_CR_ssp126_2040","BP_CR_ssp126_2040","DH_CR_ssp126_2040","CFPG_CR_ssp126_2040","BgPG_LR_ssp126_2040","Cp_LR_ssp126_2040","Bg_LR_ssp126_2040","GPG_FR_ssp126_2040","BF_FR_ssp126_2040","CFPG_FR_ssp126_2040","RJF_FR_ssp126_2040",
               "CHP_CR_ssp126_2045","BF_CR_ssp126_2045","Bg_CR_ssp126_2045","Be_CR_ssp126_2045","BP_CR_ssp126_2045","DH_CR_ssp126_2045","CFPG_CR_ssp126_2045","BgPG_LR_ssp126_2045","Cp_LR_ssp126_2045","Bg_LR_ssp126_2045","GPG_FR_ssp126_2045","BF_FR_ssp126_2045","CFPG_FR_ssp126_2045","RJF_FR_ssp126_2045",
               "CHP_CR_ssp126_2050","BF_CR_ssp126_2050","Bg_CR_ssp126_2050","Be_CR_ssp126_2050","BP_CR_ssp126_2050","DH_CR_ssp126_2050","CFPG_CR_ssp126_2050","BgPG_LR_ssp126_2050","Cp_LR_ssp126_2050","Bg_LR_ssp126_2050","GPG_FR_ssp126_2050","BF_FR_ssp126_2050","CFPG_FR_ssp126_2050","RJF_FR_ssp126_2050",
               "CHP_CR_ssp126_2055","BF_CR_ssp126_2055","Bg_CR_ssp126_2055","Be_CR_ssp126_2055","BP_CR_ssp126_2055","DH_CR_ssp126_2055","CFPG_CR_ssp126_2055","BgPG_LR_ssp126_2055","Cp_LR_ssp126_2055","Bg_LR_ssp126_2055","GPG_FR_ssp126_2055","BF_FR_ssp126_2055","CFPG_FR_ssp126_2055","RJF_FR_ssp126_2055",
               "CHP_CR_ssp126_2060","BF_CR_ssp126_2060","Bg_CR_ssp126_2060","Be_CR_ssp126_2060","BP_CR_ssp126_2060","DH_CR_ssp126_2060","CFPG_CR_ssp126_2060","BgPG_LR_ssp126_2060","Cp_LR_ssp126_2060","Bg_LR_ssp126_2060","GPG_FR_ssp126_2060","BF_FR_ssp126_2060","CFPG_FR_ssp126_2060","RJF_FR_ssp126_2060"
)
envi_name <- c("Global Warming","Stratospheric Ozone Depletion",
               "Photochemical Ozone Formation–human damage",
               "Fine Particulate Matter Formation",
               "Photochemical Ozone Formation–ecosystem damage",
               "Terrestrial Acidification",
               "Freshwater Eutrophication","Marine Eutrophication",
               "Fossil Resource Scarcity")

# Perform Monte Carlo simulation and calculations (same as SSP585)
gsd     <- 2
sdlog   <- log(gsd)                  

meanlog_em <- log(emissions_data) - 0.5 * sdlog^2
meanlog_Te <- log(Temissions_data)    - 0.5 * sdlog^2

set.seed(123)          
n_sims <- 1000  

orig_EI   <- emissions_data %*% impact_factors_data
n_tech    <- nrow(orig_EI)
n_ind     <- ncol(orig_EI)

NET_sim <- array(NA, dim = c(n_tech, n_ind, n_sims))

for(i in seq_len(n_sims)) {
  samp_em <- matrix(rlnorm(length(meanlog_em), meanlog_em, sdlog),
                    nrow = nrow(emissions_data), ncol = ncol(emissions_data))
  samp_Te <- matrix(rlnorm(length(meanlog_Te), meanlog_Te, sdlog),
                    nrow = nrow(Temissions_data), ncol = ncol(Temissions_data))
  EI_s       <- samp_em %*% impact_factors_data
  EI_d_s     <- sweep(EI_s, 2, normalization_factors_data, `*`)
  T_EI_s     <- samp_Te %*% impact_factors_data
  T_EI_d_s   <- sweep(T_EI_s, 2, normalization_factors_data, `*`)
  NET_sim[ , , i] <- T_EI_d_s - EI_d_s
}

mean_NET  <- apply(NET_sim, c(1,2), mean)
lower_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.025)
upper_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.975)

result_df <- bind_rows(
  lapply(seq_len(n_tech), function(j) {
    tibble(
      tech      = tech_name[j],
      indicator = envi_name,
      mean      = mean_NET[j, ],
      lower95   = lower_NET[j, ],
      upper95   = upper_NET[j, ]
    )
  })
)

sum_sims <- apply(NET_sim, 3, rowSums)
sum_mean  <- rowMeans(sum_sims)
sum_lower <- apply(sum_sims, 1, quantile, probs = 0.025)
sum_upper <- apply(sum_sims, 1, quantile, probs = 0.975)

sum_df <- tibble(
  tech    = tech_name,
  mean    = sum_mean,
  lower95 = sum_lower,
  upper95 = sum_upper
)
# Format results for SSP126, extracting technology, scenario, and year
EI_alltype_ssp126 <- result_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )
EI_total_ssp126 <- sum_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )

write_csv(EI_alltype_ssp126, "/---/NET_EI_indicator_uncertainty_ssp126.csv")
write_csv(EI_total_ssp126,    "/---/NET_EI_total_uncertainty_ssp126.csv")

# --- Environmental Impact for SSP245 Scenario (2025-2060) ---
library(readr)
library(tidyverse)
library(dplyr)

emissions <- read.csv("/---/LCA_AFR_Environment_ssp245.csv", header = TRUE, stringsAsFactors = FALSE)
emissions <- as.matrix(emissions[ , -2])
emissions <- t(emissions)
emissions_data <- apply(emissions[-1, ], 2, as.numeric)

Temissions <- read.csv("/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv", header = TRUE, stringsAsFactors = FALSE)
Temissions <- as.matrix(Temissions[ , -2])
Temissions <- t(Temissions)
Temissions_data <- apply(Temissions[-1, ], 2, as.numeric)

impact_factors <- read.csv("/---/impact_factor.csv", header = TRUE, stringsAsFactors = FALSE)
impact_factors <- as.matrix(impact_factors[ , -2])        
impact_factors_data <- apply(impact_factors[ , -1], 2, as.numeric)

normalization_factors <- read.csv("/---/normalization_factors.csv", header = FALSE, stringsAsFactors = FALSE)

nf_raw <- as.numeric(normalization_factors[,2])
normalization_factors_data <- 1 / nf_raw

# Define technology names for SSP245, including years 2025-2060
tech_name <- c(
  "CHP_CR_ssp245_2025","BF_CR_ssp245_2025","Bg_CR_ssp245_2025","Be_CR_ssp245_2025","BP_CR_ssp245_2025","DH_CR_ssp245_2025","CFPG_CR_ssp245_2025","BgPG_LR_ssp245_2025","Cp_LR_ssp245_2025","Bg_LR_ssp245_2025","GPG_FR_ssp245_2025","BF_FR_ssp245_2025","CFPG_FR_ssp245_2025","RJF_FR_ssp245_2025",
  "CHP_CR_ssp245_2030","BF_CR_ssp245_2030","Bg_CR_ssp245_2030","Be_CR_ssp245_2030","BP_CR_ssp245_2030","DH_CR_ssp245_2030","CFPG_CR_ssp245_2030","BgPG_LR_ssp245_2030","Cp_LR_ssp245_2030","Bg_LR_ssp245_2030","GPG_FR_ssp245_2030","BF_FR_ssp245_2030","CFPG_FR_ssp245_2030","RJF_FR_ssp245_2030",
  "CHP_CR_ssp245_2035","BF_CR_ssp245_2035","Bg_CR_ssp245_2035","Be_CR_ssp245_2035","BP_CR_ssp245_2035","DH_CR_ssp245_2035","CFPG_CR_ssp245_2035","BgPG_LR_ssp245_2035","Cp_LR_ssp245_2035","Bg_LR_ssp245_2035","GPG_FR_ssp245_2035","BF_FR_ssp245_2035","CFPG_FR_ssp245_2035","RJF_FR_ssp245_2035",
  "CHP_CR_ssp245_2040","BF_CR_ssp245_2040","Bg_CR_ssp245_2040","Be_CR_ssp245_2040","BP_CR_ssp245_2040","DH_CR_ssp245_2040","CFPG_CR_ssp245_2040","BgPG_LR_ssp245_2040","Cp_LR_ssp245_2040","Bg_LR_ssp245_2040","GPG_FR_ssp245_2040","BF_FR_ssp245_2040","CFPG_FR_ssp245_2040","RJF_FR_ssp245_2040",
  "CHP_CR_ssp245_2045","BF_CR_ssp245_2045","Bg_CR_ssp245_2045","Be_CR_ssp245_2045","BP_CR_ssp245_2045","DH_CR_ssp245_2045","CFPG_CR_ssp245_2045","BgPG_LR_ssp245_2045","Cp_LR_ssp245_2045","Bg_LR_ssp245_2045","GPG_FR_ssp245_2045","BF_FR_ssp245_2045","CFPG_FR_ssp245_2045","RJF_FR_ssp245_2045",
  "CHP_CR_ssp245_2050","BF_CR_ssp245_2050","Bg_CR_ssp245_2050","Be_CR_ssp245_2050","BP_CR_ssp245_2050","DH_CR_ssp245_2050","CFPG_CR_ssp245_2050","BgPG_LR_ssp245_2050","Cp_LR_ssp245_2050","Bg_LR_ssp245_2050","GPG_FR_ssp245_2050","BF_FR_ssp245_2050","CFPG_FR_ssp245_2050","RJF_FR_ssp245_2050",
  "CHP_CR_ssp245_2055","BF_CR_ssp245_2055","Bg_CR_ssp245_2055","Be_CR_ssp245_2055","BP_CR_ssp245_2055","DH_CR_ssp245_2055","CFPG_CR_ssp245_2055","BgPG_LR_ssp245_2055","Cp_LR_ssp245_2055","Bg_LR_ssp245_2055","GPG_FR_ssp245_2055","BF_FR_ssp245_2055","CFPG_FR_ssp245_2055","RJF_FR_ssp245_2055",
  "CHP_CR_ssp245_2060","BF_CR_ssp245_2060","Bg_CR_ssp245_2060","Be_CR_ssp245_2060","BP_CR_ssp245_2060","DH_CR_ssp245_2060","CFPG_CR_ssp245_2060","BgPG_LR_ssp245_2060","Cp_LR_ssp245_2060","Bg_LR_ssp245_2060","GPG_FR_ssp245_2060","BF_FR_ssp245_2060","CFPG_FR_ssp245_2060","RJF_FR_ssp245_2060"
               
)
envi_name <- c("Global Warming","Stratospheric Ozone Depletion",
               "Photochemical Ozone Formation–human damage",
               "Fine Particulate Matter Formation",
               "Photochemical Ozone Formation–ecosystem damage",
               "Terrestrial Acidification",
               "Freshwater Eutrophication","Marine Eutrophication",
               "Fossil Resource Scarcity")


gsd     <- 2
sdlog   <- log(gsd)                  

meanlog_em <- log(emissions_data) - 0.5 * sdlog^2
meanlog_Te <- log(Temissions_data)    - 0.5 * sdlog^2


set.seed(123)          
n_sims <- 10000  

orig_EI   <- emissions_data %*% impact_factors_data
n_tech    <- nrow(orig_EI)
n_ind     <- ncol(orig_EI)

NET_sim <- array(NA, dim = c(n_tech, n_ind, n_sims))

for(i in seq_len(n_sims)) {
  samp_em <- matrix(rlnorm(length(meanlog_em), meanlog_em, sdlog),
                    nrow = nrow(emissions_data), ncol = ncol(emissions_data))
  samp_Te <- matrix(rlnorm(length(meanlog_Te), meanlog_Te, sdlog),
                    nrow = nrow(Temissions_data), ncol = ncol(Temissions_data))
  EI_s       <- samp_em %*% impact_factors_data
  EI_d_s     <- sweep(EI_s, 2, normalization_factors_data, `*`)
  T_EI_s     <- samp_Te %*% impact_factors_data
  T_EI_d_s   <- sweep(T_EI_s, 2, normalization_factors_data, `*`)
  NET_sim[ , , i] <- T_EI_d_s - EI_d_s
}

mean_NET  <- apply(NET_sim, c(1,2), mean)
lower_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.025)
upper_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.975)

result_df <- bind_rows(
  lapply(seq_len(n_tech), function(j) {
    tibble(
      tech      = tech_name[j],
      indicator = envi_name,
      mean      = mean_NET[j, ],
      lower95   = lower_NET[j, ],
      upper95   = upper_NET[j, ]
    )
  })
)

sum_sims <- apply(NET_sim, 3, rowSums)
sum_mean  <- rowMeans(sum_sims)
sum_lower <- apply(sum_sims, 1, quantile, probs = 0.025)
sum_upper <- apply(sum_sims, 1, quantile, probs = 0.975)

sum_df <- tibble(
  tech    = tech_name,
  mean    = sum_mean,
  lower95 = sum_lower,
  upper95 = sum_upper
)
EI_alltype_ssp245 <- result_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )
EI_total_ssp245 <- sum_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )
write_csv(EI_alltype_ssp245, "/---/NET_EI_indicator_uncertainty_ssp245.csv")
write_csv(EI_total_ssp245,    "/---/NET_EI_total_uncertainty_ssp245.csv")


# --- Environmental Impact for SSP445 HUMI Regions (2025-2060) ---
library(readr)
library(tidyverse)
library(dplyr)

emissions <- read.csv("/---/LCA_AFR_Environment_ssp445HUMI.csv", header = TRUE, stringsAsFactors = FALSE)
emissions <- as.matrix(emissions[ , -2])
emissions <- t(emissions)
emissions_data <- apply(emissions[-1, ], 2, as.numeric)

Temissions <- read.csv("/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv", header = TRUE, stringsAsFactors = FALSE)
Temissions <- as.matrix(Temissions[ , -2])
Temissions <- t(Temissions)
Temissions_data <- apply(Temissions[-1, ], 2, as.numeric)

impact_factors <- read.csv("/---/impact_factor.csv", header = TRUE, stringsAsFactors = FALSE)
impact_factors <- as.matrix(impact_factors[ , -2])        
impact_factors_data <- apply(impact_factors[ , -1], 2, as.numeric)

normalization_factors <- read.csv("/---/normalization_factors.csv", header = FALSE, stringsAsFactors = FALSE)

nf_raw <- as.numeric(normalization_factors[,2])
normalization_factors_data <- 1 / nf_raw
tech_name <- c(
  "CHP_CR_ssp445HUMI_2025","BF_CR_ssp445HUMI_2025","Bg_CR_ssp445HUMI_2025","Be_CR_ssp445HUMI_2025","BP_CR_ssp445HUMI_2025","DH_CR_ssp445HUMI_2025","CFPG_CR_ssp445HUMI_2025","BgPG_LR_ssp445HUMI_2025","Cp_LR_ssp445HUMI_2025","Bg_LR_ssp445HUMI_2025","GPG_FR_ssp445HUMI_2025","BF_FR_ssp445HUMI_2025","CFPG_FR_ssp445HUMI_2025","RJF_FR_ssp445HUMI_2025",
  "CHP_CR_ssp445HUMI_2030","BF_CR_ssp445HUMI_2030","Bg_CR_ssp445HUMI_2030","Be_CR_ssp445HUMI_2030","BP_CR_ssp445HUMI_2030","DH_CR_ssp445HUMI_2030","CFPG_CR_ssp445HUMI_2030","BgPG_LR_ssp445HUMI_2030","Cp_LR_ssp445HUMI_2030","Bg_LR_ssp445HUMI_2030","GPG_FR_ssp445HUMI_2030","BF_FR_ssp445HUMI_2030","CFPG_FR_ssp445HUMI_2030","RJF_FR_ssp445HUMI_2030",
  "CHP_CR_ssp445HUMI_2035","BF_CR_ssp445HUMI_2035","Bg_CR_ssp445HUMI_2035","Be_CR_ssp445HUMI_2035","BP_CR_ssp445HUMI_2035","DH_CR_ssp445HUMI_2035","CFPG_CR_ssp445HUMI_2035","BgPG_LR_ssp445HUMI_2035","Cp_LR_ssp445HUMI_2035","Bg_LR_ssp445HUMI_2035","GPG_FR_ssp445HUMI_2035","BF_FR_ssp445HUMI_2035","CFPG_FR_ssp445HUMI_2035","RJF_FR_ssp445HUMI_2035",
  "CHP_CR_ssp445HUMI_2040","BF_CR_ssp445HUMI_2040","Bg_CR_ssp445HUMI_2040","Be_CR_ssp445HUMI_2040","BP_CR_ssp445HUMI_2040","DH_CR_ssp445HUMI_2040","CFPG_CR_ssp445HUMI_2040","BgPG_LR_ssp445HUMI_2040","Cp_LR_ssp445HUMI_2040","Bg_LR_ssp445HUMI_2040","GPG_FR_ssp445HUMI_2040","BF_FR_ssp445HUMI_2040","CFPG_FR_ssp445HUMI_2040","RJF_FR_ssp445HUMI_2040",
  "CHP_CR_ssp445HUMI_2045","BF_CR_ssp445HUMI_2045","Bg_CR_ssp445HUMI_2045","Be_CR_ssp445HUMI_2045","BP_CR_ssp445HUMI_2045","DH_CR_ssp445HUMI_2045","CFPG_CR_ssp445HUMI_2045","BgPG_LR_ssp445HUMI_2045","Cp_LR_ssp445HUMI_2045","Bg_LR_ssp445HUMI_2045","GPG_FR_ssp445HUMI_2045","BF_FR_ssp445HUMI_2045","CFPG_FR_ssp445HUMI_2045","RJF_FR_ssp445HUMI_2045",
  "CHP_CR_ssp445HUMI_2050","BF_CR_ssp445HUMI_2050","Bg_CR_ssp445HUMI_2050","Be_CR_ssp445HUMI_2050","BP_CR_ssp445HUMI_2050","DH_CR_ssp445HUMI_2050","CFPG_CR_ssp445HUMI_2050","BgPG_LR_ssp445HUMI_2050","Cp_LR_ssp445HUMI_2050","Bg_LR_ssp445HUMI_2050","GPG_FR_ssp445HUMI_2050","BF_FR_ssp445HUMI_2050","CFPG_FR_ssp445HUMI_2050","RJF_FR_ssp445HUMI_2050",
  "CHP_CR_ssp445HUMI_2055","BF_CR_ssp445HUMI_2055","Bg_CR_ssp445HUMI_2055","Be_CR_ssp445HUMI_2055","BP_CR_ssp445HUMI_2055","DH_CR_ssp445HUMI_2055","CFPG_CR_ssp445HUMI_2055","BgPG_LR_ssp445HUMI_2055","Cp_LR_ssp445HUMI_2055","Bg_LR_ssp445HUMI_2055","GPG_FR_ssp445HUMI_2055","BF_FR_ssp445HUMI_2055","CFPG_FR_ssp445HUMI_2055","RJF_FR_ssp445HUMI_2055",
  "CHP_CR_ssp445HUMI_2060","BF_CR_ssp445HUMI_2060","Bg_CR_ssp445HUMI_2060","Be_CR_ssp445HUMI_2060","BP_CR_ssp445HUMI_2060","DH_CR_ssp445HUMI_2060","CFPG_CR_ssp445HUMI_2060","BgPG_LR_ssp445HUMI_2060","Cp_LR_ssp445HUMI_2060","Bg_LR_ssp445HUMI_2060","GPG_FR_ssp445HUMI_2060","BF_FR_ssp445HUMI_2060","CFPG_FR_ssp445HUMI_2060","RJF_FR_ssp445HUMI_2060" 
)
envi_name <- c("Global Warming","Stratospheric Ozone Depletion",
               "Photochemical Ozone Formation–human damage",
               "Fine Particulate Matter Formation",
               "Photochemical Ozone Formation–ecosystem damage",
               "Terrestrial Acidification",
               "Freshwater Eutrophication","Marine Eutrophication",
               "Fossil Resource Scarcity")


gsd     <- 2
sdlog   <- log(gsd)                  

meanlog_em <- log(emissions_data) - 0.5 * sdlog^2
meanlog_Te <- log(Temissions_data)    - 0.5 * sdlog^2


set.seed(123)          
n_sims <- 10000  

orig_EI   <- emissions_data %*% impact_factors_data
n_tech    <- nrow(orig_EI)
n_ind     <- ncol(orig_EI)

NET_sim <- array(NA, dim = c(n_tech, n_ind, n_sims))

for(i in seq_len(n_sims)) {
  samp_em <- matrix(rlnorm(length(meanlog_em), meanlog_em, sdlog),
                    nrow = nrow(emissions_data), ncol = ncol(emissions_data))
  samp_Te <- matrix(rlnorm(length(meanlog_Te), meanlog_Te, sdlog),
                    nrow = nrow(Temissions_data), ncol = ncol(Temissions_data))
  EI_s       <- samp_em %*% impact_factors_data
  EI_d_s     <- sweep(EI_s, 2, normalization_factors_data, `*`)
  T_EI_s     <- samp_Te %*% impact_factors_data
  T_EI_d_s   <- sweep(T_EI_s, 2, normalization_factors_data, `*`)
  NET_sim[ , , i] <- T_EI_d_s - EI_d_s
}

mean_NET  <- apply(NET_sim, c(1,2), mean)
lower_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.025)
upper_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.975)

result_df <- bind_rows(
  lapply(seq_len(n_tech), function(j) {
    tibble(
      tech      = tech_name[j],
      indicator = envi_name,
      mean      = mean_NET[j, ],
      lower95   = lower_NET[j, ],
      upper95   = upper_NET[j, ]
    )
  })
)

sum_sims <- apply(NET_sim, 3, rowSums)
sum_mean  <- rowMeans(sum_sims)
sum_lower <- apply(sum_sims, 1, quantile, probs = 0.025)
sum_upper <- apply(sum_sims, 1, quantile, probs = 0.975)

sum_df <- tibble(
  tech    = tech_name,
  mean    = sum_mean,
  lower95 = sum_lower,
  upper95 = sum_upper
)
EI_alltype_ssp445HUMI <- result_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )
EI_total_ssp445HUMI <- sum_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )
write_csv(EI_alltype_ssp445HUMI, "/---/NET_EI_indicator_uncertainty_ssp445HUMI.csv")
write_csv(EI_total_ssp445HUMI,    "/---/NET_EI_total_uncertainty_ssp445HUMI.csv")


# --- Environmental Impact for SSP445 LIMI Regions (2025-2060) ---
library(readr)
library(tidyverse)
library(dplyr)

emissions <- read.csv("/---/LCA_AFR_Environment_ssp445LUMI.csv", header = TRUE, stringsAsFactors = FALSE)
emissions <- as.matrix(emissions[ , -2])
emissions <- t(emissions)
emissions_data <- apply(emissions[-1, ], 2, as.numeric)

Temissions <- read.csv("/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv", header = TRUE, stringsAsFactors = FALSE)
Temissions <- as.matrix(Temissions[ , -2])
Temissions <- t(Temissions)
Temissions_data <- apply(Temissions[-1, ], 2, as.numeric)

impact_factors <- read.csv("/---/impact_factor.csv", header = TRUE, stringsAsFactors = FALSE)
impact_factors <- as.matrix(impact_factors[ , -2])          
impact_factors_data <- apply(impact_factors[ , -1], 2, as.numeric)

normalization_factors <- read.csv("/---/normalization_factors.csv", header = FALSE, stringsAsFactors = FALSE)

nf_raw <- as.numeric(normalization_factors[,2])
normalization_factors_data <- 1 / nf_raw
tech_name <- c(
  "CHP_CR_ssp445LUMI_2025","BF_CR_ssp445LUMI_2025","Bg_CR_ssp445LUMI_2025","Be_CR_ssp445LUMI_2025","BP_CR_ssp445LUMI_2025","DH_CR_ssp445LUMI_2025","CFPG_CR_ssp445LUMI_2025","BgPG_LR_ssp445LUMI_2025","Cp_LR_ssp445LUMI_2025","Bg_LR_ssp445LUMI_2025","GPG_FR_ssp445LUMI_2025","BF_FR_ssp445LUMI_2025","CFPG_FR_ssp445LUMI_2025","RJF_FR_ssp445LUMI_2025",
  "CHP_CR_ssp445LUMI_2030","BF_CR_ssp445LUMI_2030","Bg_CR_ssp445LUMI_2030","Be_CR_ssp445LUMI_2030","BP_CR_ssp445LUMI_2030","DH_CR_ssp445LUMI_2030","CFPG_CR_ssp445LUMI_2030","BgPG_LR_ssp445LUMI_2030","Cp_LR_ssp445LUMI_2030","Bg_LR_ssp445LUMI_2030","GPG_FR_ssp445LUMI_2030","BF_FR_ssp445LUMI_2030","CFPG_FR_ssp445LUMI_2030","RJF_FR_ssp445LUMI_2030",
  "CHP_CR_ssp445LUMI_2035","BF_CR_ssp445LUMI_2035","Bg_CR_ssp445LUMI_2035","Be_CR_ssp445LUMI_2035","BP_CR_ssp445LUMI_2035","DH_CR_ssp445LUMI_2035","CFPG_CR_ssp445LUMI_2035","BgPG_LR_ssp445LUMI_2035","Cp_LR_ssp445LUMI_2035","Bg_LR_ssp445LUMI_2035","GPG_FR_ssp445LUMI_2035","BF_FR_ssp445LUMI_2035","CFPG_FR_ssp445LUMI_2035","RJF_FR_ssp445LUMI_2035",
  "CHP_CR_ssp445LUMI_2040","BF_CR_ssp445LUMI_2040","Bg_CR_ssp445LUMI_2040","Be_CR_ssp445LUMI_2040","BP_CR_ssp445LUMI_2040","DH_CR_ssp445LUMI_2040","CFPG_CR_ssp445LUMI_2040","BgPG_LR_ssp445LUMI_2040","Cp_LR_ssp445LUMI_2040","Bg_LR_ssp445LUMI_2040","GPG_FR_ssp445LUMI_2040","BF_FR_ssp445LUMI_2040","CFPG_FR_ssp445LUMI_2040","RJF_FR_ssp445LUMI_2040",
  "CHP_CR_ssp445LUMI_2045","BF_CR_ssp445LUMI_2045","Bg_CR_ssp445LUMI_2045","Be_CR_ssp445LUMI_2045","BP_CR_ssp445LUMI_2045","DH_CR_ssp445LUMI_2045","CFPG_CR_ssp445LUMI_2045","BgPG_LR_ssp445LUMI_2045","Cp_LR_ssp445LUMI_2045","Bg_LR_ssp445LUMI_2045","GPG_FR_ssp445LUMI_2045","BF_FR_ssp445LUMI_2045","CFPG_FR_ssp445LUMI_2045","RJF_FR_ssp445LUMI_2045",
  "CHP_CR_ssp445LUMI_2050","BF_CR_ssp445LUMI_2050","Bg_CR_ssp445LUMI_2050","Be_CR_ssp445LUMI_2050","BP_CR_ssp445LUMI_2050","DH_CR_ssp445LUMI_2050","CFPG_CR_ssp445LUMI_2050","BgPG_LR_ssp445LUMI_2050","Cp_LR_ssp445LUMI_2050","Bg_LR_ssp445LUMI_2050","GPG_FR_ssp445LUMI_2050","BF_FR_ssp445LUMI_2050","CFPG_FR_ssp445LUMI_2050","RJF_FR_ssp445LUMI_2050",
  "CHP_CR_ssp445LUMI_2055","BF_CR_ssp445LUMI_2055","Bg_CR_ssp445LUMI_2055","Be_CR_ssp445LUMI_2055","BP_CR_ssp445LUMI_2055","DH_CR_ssp445LUMI_2055","CFPG_CR_ssp445LUMI_2055","BgPG_LR_ssp445LUMI_2055","Cp_LR_ssp445LUMI_2055","Bg_LR_ssp445LUMI_2055","GPG_FR_ssp445LUMI_2055","BF_FR_ssp445LUMI_2055","CFPG_FR_ssp445LUMI_2055","RJF_FR_ssp445LUMI_2055",
  "CHP_CR_ssp445LUMI_2060","BF_CR_ssp445LUMI_2060","Bg_CR_ssp445LUMI_2060","Be_CR_ssp445LUMI_2060","BP_CR_ssp445LUMI_2060","DH_CR_ssp445LUMI_2060","CFPG_CR_ssp445LUMI_2060","BgPG_LR_ssp445LUMI_2060","Cp_LR_ssp445LUMI_2060","Bg_LR_ssp445LUMI_2060","GPG_FR_ssp445LUMI_2060","BF_FR_ssp445LUMI_2060","CFPG_FR_ssp445LUMI_2060","RJF_FR_ssp445LUMI_2060"
  )
envi_name <- c("Global Warming","Stratospheric Ozone Depletion",
               "Photochemical Ozone Formation–human damage",
               "Fine Particulate Matter Formation",
               "Photochemical Ozone Formation–ecosystem damage",
               "Terrestrial Acidification",
               "Freshwater Eutrophication","Marine Eutrophication",
               "Fossil Resource Scarcity")


gsd     <- 2
sdlog   <- log(gsd)                  

meanlog_em <- log(emissions_data) - 0.5 * sdlog^2
meanlog_Te <- log(Temissions_data)    - 0.5 * sdlog^2


set.seed(123)          
n_sims <- 10000  

orig_EI   <- emissions_data %*% impact_factors_data
n_tech    <- nrow(orig_EI)
n_ind     <- ncol(orig_EI)

NET_sim <- array(NA, dim = c(n_tech, n_ind, n_sims))

for(i in seq_len(n_sims)) {
  samp_em <- matrix(rlnorm(length(meanlog_em), meanlog_em, sdlog),
                    nrow = nrow(emissions_data), ncol = ncol(emissions_data))
  samp_Te <- matrix(rlnorm(length(meanlog_Te), meanlog_Te, sdlog),
                    nrow = nrow(Temissions_data), ncol = ncol(Temissions_data))
  EI_s       <- samp_em %*% impact_factors_data
  EI_d_s     <- sweep(EI_s, 2, normalization_factors_data, `*`)
  T_EI_s     <- samp_Te %*% impact_factors_data
  T_EI_d_s   <- sweep(T_EI_s, 2, normalization_factors_data, `*`)
  NET_sim[ , , i] <- T_EI_d_s - EI_d_s
}

mean_NET  <- apply(NET_sim, c(1,2), mean)
lower_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.025)
upper_NET <- apply(NET_sim, c(1,2), quantile, probs = 0.975)

result_df <- bind_rows(
  lapply(seq_len(n_tech), function(j) {
    tibble(
      tech      = tech_name[j],
      indicator = envi_name,
      mean      = mean_NET[j, ],
      lower95   = lower_NET[j, ],
      upper95   = upper_NET[j, ]
    )
  })
)

sum_sims <- apply(NET_sim, 3, rowSums)
sum_mean  <- rowMeans(sum_sims)
sum_lower <- apply(sum_sims, 1, quantile, probs = 0.025)
sum_upper <- apply(sum_sims, 1, quantile, probs = 0.975)

sum_df <- tibble(
  tech    = tech_name,
  mean    = sum_mean,
  lower95 = sum_lower,
  upper95 = sum_upper
)
EI_alltype_ssp445LUMI <- result_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = FALSE
  )
EI_total_ssp445LUMI <- sum_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = FALSE
  )
write_csv(EI_alltype_ssp445LUMI, "/---/NET_EI_indicator_uncertainty_ssp445LUMI.csv")
write_csv(EI_total_ssp445LUMI,    "/---/NET_EI_total_uncertainty_ssp445LUMI.csv")

list_df <- list(
  ssp445LUMI = EI_alltype_ssp445LUMI,
  ssp445HUMI = EI_alltype_ssp445HUMI,
  ssp126     = EI_alltype_ssp126,
  ssp245     = EI_alltype_ssp245,
  ssp585     = EI_alltype_ssp585
)
list_df2 <- lapply(list_df, function(df) {
  df %>% mutate(Year = as.character(Year))
})
EI_alltype <- bind_rows(list_df2, .id = "scenario")
write_csv(EI_alltype, "/---/NET_EI_alltype.csv")
ecosys_ind <- c(
  "Photochemical Ozone Formation–ecosystem damage",
  "Fine Particulate Matter Formation",
  "Terrestrial Acidification",
  "Freshwater Eutrophication",
  "Marine Eutrophication"
)
EI_ecosys <- EI_alltype %>%
  filter(indicator %in% ecosys_ind) %>%
  group_by(technology, scenario, Year) %>%
  summarise(
    ecosys_mean       = sum(mean,    na.rm = TRUE),
    ecosys_lower95    = sum(lower95, na.rm = TRUE),
    ecosys_upper95    = sum(upper95, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(
    technology = factor(
      technology,
      levels = c(
        "CHP_CR","BF_CR","Bg_CR","Be_CR","BP_CR","DH_CR","CFPG_CR",
        "BgPG_LR","Cp_LR","Bg_LR","GPG_FR","BF_FR","CFPG_FR","RJF_FR"
      )
    )
  ) %>%
  arrange(technology)
write_csv(EI_ecosys, "/---/NET_EI_ecosys.csv")
list_df <- list(
  ssp445LUMI = EI_total_ssp445LUMI,
  ssp445HUMI = EI_total_ssp445HUMI,
  ssp126     = EI_total_ssp126,
  ssp245     = EI_total_ssp245,
  ssp585     = EI_total_ssp585
)
list_df2 <- lapply(list_df, function(df) {
  df %>% mutate(Year = as.character(Year))
})
EI_total <- bind_rows(list_df2, .id = "scenario")
write_csv(EI_total, "/---/NET_EI_total.csv")



# Monetization of environmental impacts ----
# This section converts the calculated environmental impacts per functional unit into monetized loss or benefit values, including uncertainty analysis.
# --- Monetization of Environmental Impacts for SSP126 Scenario (2025-2060) ---
library(readr)
library(dplyr)
# Read emissions data for AFR technologies under SSP126
emissions <- read.csv(
  "/---/LCA_AFR_Environment_ssp126.csv",
  header = TRUE, stringsAsFactors = FALSE
)
em_mat  <- emissions[, -2] %>% as.matrix() %>% t()
em_data <- em_mat[-1, ] %>% apply(2, as.numeric)
# Read fossil energy emissions data for comparison
Te <- read.csv(
  "/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv",
  header = TRUE, stringsAsFactors = FALSE
)
Te_mat  <- Te[, -2] %>% as.matrix() %>% t()
Te_data <- Te_mat[-1, ] %>% apply(2, as.numeric)
# Read ReCiPe2016 endpoint impact factors
impact_ep    <- read.csv(
  "/---/impact_factors_endpoint.csv",
  header = TRUE, stringsAsFactors = FALSE
)
imp_ep_mat   <- impact_ep[, -2] %>% as.matrix()
imp_ep_data  <- imp_ep_mat[, -1] %>% apply(2, as.numeric)
# Read normalization factors for endpoint impacts
norm_ep      <- read.csv(
  "/---/normalization_factors_endpoint.csv",
  header = FALSE, stringsAsFactors = FALSE
)
norm_ep_vec <- as.numeric(norm_ep[, 2])
# Define technology names for SSP126, including years 2025-2060
tech_name <- c(
  "CHP_CR_ssp126_2025","BF_CR_ssp126_2025","Bg_CR_ssp126_2025","Be_CR_ssp126_2025","BP_CR_ssp126_2025","DH_CR_ssp126_2025","CFPG_CR_ssp126_2025","BgPG_LR_ssp126_2025","Cp_LR_ssp126_2025","Bg_LR_ssp126_2025","GPG_FR_ssp126_2025","BF_FR_ssp126_2025","CFPG_FR_ssp126_2025","RJF_FR_ssp126_2025",
  "CHP_CR_ssp126_2030","BF_CR_ssp126_2030","Bg_CR_ssp126_2030","Be_CR_ssp126_2030","BP_CR_ssp126_2030","DH_CR_ssp126_2030","CFPG_CR_ssp126_2030","BgPG_LR_ssp126_2030","Cp_LR_ssp126_2030","Bg_LR_ssp126_2030","GPG_FR_ssp126_2030","BF_FR_ssp126_2030","CFPG_FR_ssp126_2030","RJF_FR_ssp126_2030",
  "CHP_CR_ssp126_2035","BF_CR_ssp126_2035","Bg_CR_ssp126_2035","Be_CR_ssp126_2035","BP_CR_ssp126_2035","DH_CR_ssp126_2035","CFPG_CR_ssp126_2035","BgPG_LR_ssp126_2035","Cp_LR_ssp126_2035","Bg_LR_ssp126_2035","GPG_FR_ssp126_2035","BF_FR_ssp126_2035","CFPG_FR_ssp126_2035","RJF_FR_ssp126_2035",
  "CHP_CR_ssp126_2040","BF_CR_ssp126_2040","Bg_CR_ssp126_2040","Be_CR_ssp126_2040","BP_CR_ssp126_2040","DH_CR_ssp126_2040","CFPG_CR_ssp126_2040","BgPG_LR_ssp126_2040","Cp_LR_ssp126_2040","Bg_LR_ssp126_2040","GPG_FR_ssp126_2040","BF_FR_ssp126_2040","CFPG_FR_ssp126_2040","RJF_FR_ssp126_2040",
  "CHP_CR_ssp126_2045","BF_CR_ssp126_2045","Bg_CR_ssp126_2045","Be_CR_ssp126_2045","BP_CR_ssp126_2045","DH_CR_ssp126_2045","CFPG_CR_ssp126_2045","BgPG_LR_ssp126_2045","Cp_LR_ssp126_2045","Bg_LR_ssp126_2045","GPG_FR_ssp126_2045","BF_FR_ssp126_2045","CFPG_FR_ssp126_2045","RJF_FR_ssp126_2045",
  "CHP_CR_ssp126_2050","BF_CR_ssp126_2050","Bg_CR_ssp126_2050","Be_CR_ssp126_2050","BP_CR_ssp126_2050","DH_CR_ssp126_2050","CFPG_CR_ssp126_2050","BgPG_LR_ssp126_2050","Cp_LR_ssp126_2050","Bg_LR_ssp126_2050","GPG_FR_ssp126_2050","BF_FR_ssp126_2050","CFPG_FR_ssp126_2050","RJF_FR_ssp126_2050",
  "CHP_CR_ssp126_2055","BF_CR_ssp126_2055","Bg_CR_ssp126_2055","Be_CR_ssp126_2055","BP_CR_ssp126_2055","DH_CR_ssp126_2055","CFPG_CR_ssp126_2055","BgPG_LR_ssp126_2055","Cp_LR_ssp126_2055","Bg_LR_ssp126_2055","GPG_FR_ssp126_2055","BF_FR_ssp126_2055","CFPG_FR_ssp126_2055","RJF_FR_ssp126_2055",
  "CHP_CR_ssp126_2060","BF_CR_ssp126_2060","Bg_CR_ssp126_2060","Be_CR_ssp126_2060","BP_CR_ssp126_2060","DH_CR_ssp126_2060","CFPG_CR_ssp126_2060","BgPG_LR_ssp126_2060","Cp_LR_ssp126_2060","Bg_LR_ssp126_2060","GPG_FR_ssp126_2060","BF_FR_ssp126_2060","CFPG_FR_ssp126_2060","RJF_FR_ssp126_2060"
  
)
# Calculate endpoint environmental impacts for AFR and fossil energy
EI_ep    <- em_data   %*% imp_ep_data
T_EI_ep  <- Te_data   %*% imp_ep_data
# Normalize impacts using endpoint normalization factors
EI_ep_n   <- sweep(EI_ep,   2, norm_ep_vec, `*`)
T_EI_ep_n <- sweep(T_EI_ep, 2, norm_ep_vec, `*`)
# Calculate net environmental impacts (fossil energy - AFR)
NET_ep <- T_EI_ep_n - EI_ep_n
rownames(NET_ep) <- tech_name

# Convert impacts to monetary values using fixed conversion factors
# Columns 2:5 (human health impacts) use DALY factor (176624)
# Columns 6:11 (ecosystem/species impacts) use species factor (17891594)
NET_df <- as_tibble(NET_ep, rownames = "tech") %>%
  mutate(across(2:5,  ~ .x * 176624),
         across(6:11, ~ .x * 17891594),
         sum = rowSums(across(2:11)))

# Define parameters for uncertainty analysis (normal distribution)
daly_lower   <- 148288
daly_upper   <- 200236
spec_lower   <- 4472899
spec_upper   <- 44728985

daly_mean    <- mean(c(daly_lower, daly_upper))
daly_sd      <- (daly_upper - daly_lower) / (2 * 1.96)

spec_mean    <- mean(c(spec_lower, spec_upper))
spec_sd      <- (spec_upper - spec_lower) / (2 * 1.96)
# Set seed for reproducibility and define number of Monte Carlo simulations
set.seed(123)
n_sims       <- 10000
n_tech       <- length(tech_name)
monet_sims   <- matrix(NA, nrow = n_tech, ncol = n_sims,
                       dimnames = list(tech_name, NULL))
# Perform Monte Carlo simulation for uncertainty in monetization factors
for(i in seq_len(n_sims)) {
  daly_i <- rnorm(1, daly_mean, daly_sd)
  spec_i <- rnorm(1, spec_mean, spec_sd)
  
  mat_d <- NET_ep[, 1:4, drop = FALSE] * daly_i   # 4 列
  mat_s <- NET_ep[, 5:10, drop = FALSE] * spec_i  # 6 列
  
  monet_sims[, i] <- rowSums(mat_d) + rowSums(mat_s)
}
# Summarize Monte Carlo results (mean and 95% confidence intervals)
monet_summary <- tibble(
  tech    = tech_name,
  mean    = rowMeans(monet_sims),
  lower95 = apply(monet_sims, 1, quantile, probs = 0.025),
  upper95 = apply(monet_sims, 1, quantile, probs = 0.975)
)

# Combine deterministic and stochastic results
combined_df <- NET_df %>%
  select(tech, deterministic_sum = sum) %>%     
  left_join(monet_summary, by = "tech") %>%    
  select(tech, deterministic_sum, mean, lower95, upper95)
# Extract technology, scenario, and year from tech column
ecores_alltype_ssp126 <- combined_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )

write_csv(
  combined_df,
  "/---/eco_LCA_NET_EI_world_endpoint_ssp126.csv"
)



# --- Monetization of Environmental Impacts for SSP245 Scenario (2025-2060) ---
library(readr)
library(dplyr)

emissions <- read.csv(
  "/---/LCA_AFR_Environment_ssp245.csv",
  header = TRUE, stringsAsFactors = FALSE
)
em_mat  <- emissions[, -2] %>% as.matrix() %>% t()
em_data <- em_mat[-1, ] %>% apply(2, as.numeric)

Te <- read.csv(
  "/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv",
  header = TRUE, stringsAsFactors = FALSE
)
Te_mat  <- Te[, -2] %>% as.matrix() %>% t()
Te_data <- Te_mat[-1, ] %>% apply(2, as.numeric)

impact_ep    <- read.csv(
  "/---/impact_factors_endpoint.csv",
  header = TRUE, stringsAsFactors = FALSE
)
imp_ep_mat   <- impact_ep[, -2] %>% as.matrix()
imp_ep_data  <- imp_ep_mat[, -1] %>% apply(2, as.numeric)

norm_ep      <- read.csv(
  "/---/normalization_factors_endpoint.csv",
  header = FALSE, stringsAsFactors = FALSE
)
norm_ep_vec <- as.numeric(norm_ep[, 2])

tech_name <- c(
  "CHP_CR_ssp245_2025","BF_CR_ssp245_2025","Bg_CR_ssp245_2025","Be_CR_ssp245_2025","BP_CR_ssp245_2025","DH_CR_ssp245_2025","CFPG_CR_ssp245_2025","BgPG_LR_ssp245_2025","Cp_LR_ssp245_2025","Bg_LR_ssp245_2025","GPG_FR_ssp245_2025","BF_FR_ssp245_2025","CFPG_FR_ssp245_2025","RJF_FR_ssp245_2025",
  "CHP_CR_ssp245_2030","BF_CR_ssp245_2030","Bg_CR_ssp245_2030","Be_CR_ssp245_2030","BP_CR_ssp245_2030","DH_CR_ssp245_2030","CFPG_CR_ssp245_2030","BgPG_LR_ssp245_2030","Cp_LR_ssp245_2030","Bg_LR_ssp245_2030","GPG_FR_ssp245_2030","BF_FR_ssp245_2030","CFPG_FR_ssp245_2030","RJF_FR_ssp245_2030",
  "CHP_CR_ssp245_2035","BF_CR_ssp245_2035","Bg_CR_ssp245_2035","Be_CR_ssp245_2035","BP_CR_ssp245_2035","DH_CR_ssp245_2035","CFPG_CR_ssp245_2035","BgPG_LR_ssp245_2035","Cp_LR_ssp245_2035","Bg_LR_ssp245_2035","GPG_FR_ssp245_2035","BF_FR_ssp245_2035","CFPG_FR_ssp245_2035","RJF_FR_ssp245_2035",
  "CHP_CR_ssp245_2040","BF_CR_ssp245_2040","Bg_CR_ssp245_2040","Be_CR_ssp245_2040","BP_CR_ssp245_2040","DH_CR_ssp245_2040","CFPG_CR_ssp245_2040","BgPG_LR_ssp245_2040","Cp_LR_ssp245_2040","Bg_LR_ssp245_2040","GPG_FR_ssp245_2040","BF_FR_ssp245_2040","CFPG_FR_ssp245_2040","RJF_FR_ssp245_2040",
  "CHP_CR_ssp245_2045","BF_CR_ssp245_2045","Bg_CR_ssp245_2045","Be_CR_ssp245_2045","BP_CR_ssp245_2045","DH_CR_ssp245_2045","CFPG_CR_ssp245_2045","BgPG_LR_ssp245_2045","Cp_LR_ssp245_2045","Bg_LR_ssp245_2045","GPG_FR_ssp245_2045","BF_FR_ssp245_2045","CFPG_FR_ssp245_2045","RJF_FR_ssp245_2045",
  "CHP_CR_ssp245_2050","BF_CR_ssp245_2050","Bg_CR_ssp245_2050","Be_CR_ssp245_2050","BP_CR_ssp245_2050","DH_CR_ssp245_2050","CFPG_CR_ssp245_2050","BgPG_LR_ssp245_2050","Cp_LR_ssp245_2050","Bg_LR_ssp245_2050","GPG_FR_ssp245_2050","BF_FR_ssp245_2050","CFPG_FR_ssp245_2050","RJF_FR_ssp245_2050",
  "CHP_CR_ssp245_2055","BF_CR_ssp245_2055","Bg_CR_ssp245_2055","Be_CR_ssp245_2055","BP_CR_ssp245_2055","DH_CR_ssp245_2055","CFPG_CR_ssp245_2055","BgPG_LR_ssp245_2055","Cp_LR_ssp245_2055","Bg_LR_ssp245_2055","GPG_FR_ssp245_2055","BF_FR_ssp245_2055","CFPG_FR_ssp245_2055","RJF_FR_ssp245_2055",
  "CHP_CR_ssp245_2060","BF_CR_ssp245_2060","Bg_CR_ssp245_2060","Be_CR_ssp245_2060","BP_CR_ssp245_2060","DH_CR_ssp245_2060","CFPG_CR_ssp245_2060","BgPG_LR_ssp245_2060","Cp_LR_ssp245_2060","Bg_LR_ssp245_2060","GPG_FR_ssp245_2060","BF_FR_ssp245_2060","CFPG_FR_ssp245_2060","RJF_FR_ssp245_2060"
  
)

EI_ep    <- em_data   %*% imp_ep_data
T_EI_ep  <- Te_data   %*% imp_ep_data

EI_ep_n   <- sweep(EI_ep,   2, norm_ep_vec, `*`)
T_EI_ep_n <- sweep(T_EI_ep, 2, norm_ep_vec, `*`)

NET_ep <- T_EI_ep_n - EI_ep_n
rownames(NET_ep) <- tech_name


NET_df <- as_tibble(NET_ep, rownames = "tech") %>%
  mutate(across(2:5,  ~ .x * 176624),
         across(6:11, ~ .x * 17891594),
         sum = rowSums(across(2:11)))


daly_lower   <- 148288
daly_upper   <- 200236
spec_lower   <- 4472899
spec_upper   <- 44728985

daly_mean    <- mean(c(daly_lower, daly_upper))
daly_sd      <- (daly_upper - daly_lower) / (2 * 1.96)

spec_mean    <- mean(c(spec_lower, spec_upper))
spec_sd      <- (spec_upper - spec_lower) / (2 * 1.96)

set.seed(123)
n_sims       <- 10000
n_tech       <- length(tech_name)
monet_sims   <- matrix(NA, nrow = n_tech, ncol = n_sims,
                       dimnames = list(tech_name, NULL))

for(i in seq_len(n_sims)) {
  daly_i <- rnorm(1, daly_mean, daly_sd)
  spec_i <- rnorm(1, spec_mean, spec_sd)
  
  mat_d <- NET_ep[, 1:4, drop = FALSE] * daly_i  
  mat_s <- NET_ep[, 5:10, drop = FALSE] * spec_i 
  
  monet_sims[, i] <- rowSums(mat_d) + rowSums(mat_s)
}

monet_summary <- tibble(
  tech    = tech_name,
  mean    = rowMeans(monet_sims),
  lower95 = apply(monet_sims, 1, quantile, probs = 0.025),
  upper95 = apply(monet_sims, 1, quantile, probs = 0.975)
)


combined_df <- NET_df %>%
  select(tech, deterministic_sum = sum) %>%     
  left_join(monet_summary, by = "tech") %>%    
  select(tech, deterministic_sum, mean, lower95, upper95)

ecores_alltype_ssp245 <- combined_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )

write_csv(
  combined_df,
  "/---/eco_LCA_NET_EI_world_endpoint_ssp245.csv"
)



# --- Monetization of Environmental Impacts for SSP445HUMI Scenario (2025-2060) ---

library(readr)
library(dplyr)

emissions <- read.csv(
  "/---/LCA_AFR_Environment_ssp445HUMI.csv",
  header = TRUE, stringsAsFactors = FALSE
)
em_mat  <- emissions[, -2] %>% as.matrix() %>% t()
em_data <- em_mat[-1, ] %>% apply(2, as.numeric)

Te <- read.csv(
  "/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv",
  header = TRUE, stringsAsFactors = FALSE
)
Te_mat  <- Te[, -2] %>% as.matrix() %>% t()
Te_data <- Te_mat[-1, ] %>% apply(2, as.numeric)

impact_ep    <- read.csv(
  "/---/impact_factors_endpoint.csv",
  header = TRUE, stringsAsFactors = FALSE
)
imp_ep_mat   <- impact_ep[, -2] %>% as.matrix()
imp_ep_data  <- imp_ep_mat[, -1] %>% apply(2, as.numeric)

norm_ep      <- read.csv(
  "/---/normalization_factors_endpoint.csv",
  header = FALSE, stringsAsFactors = FALSE
)
norm_ep_vec <- as.numeric(norm_ep[, 2])

tech_name <- c(
  "CHP_CR_ssp445HUMI_2025","BF_CR_ssp445HUMI_2025","Bg_CR_ssp445HUMI_2025","Be_CR_ssp445HUMI_2025","BP_CR_ssp445HUMI_2025","DH_CR_ssp445HUMI_2025","CFPG_CR_ssp445HUMI_2025","BgPG_LR_ssp445HUMI_2025","Cp_LR_ssp445HUMI_2025","Bg_LR_ssp445HUMI_2025","GPG_FR_ssp445HUMI_2025","BF_FR_ssp445HUMI_2025","CFPG_FR_ssp445HUMI_2025","RJF_FR_ssp445HUMI_2025",
  "CHP_CR_ssp445HUMI_2030","BF_CR_ssp445HUMI_2030","Bg_CR_ssp445HUMI_2030","Be_CR_ssp445HUMI_2030","BP_CR_ssp445HUMI_2030","DH_CR_ssp445HUMI_2030","CFPG_CR_ssp445HUMI_2030","BgPG_LR_ssp445HUMI_2030","Cp_LR_ssp445HUMI_2030","Bg_LR_ssp445HUMI_2030","GPG_FR_ssp445HUMI_2030","BF_FR_ssp445HUMI_2030","CFPG_FR_ssp445HUMI_2030","RJF_FR_ssp445HUMI_2030",
  "CHP_CR_ssp445HUMI_2035","BF_CR_ssp445HUMI_2035","Bg_CR_ssp445HUMI_2035","Be_CR_ssp445HUMI_2035","BP_CR_ssp445HUMI_2035","DH_CR_ssp445HUMI_2035","CFPG_CR_ssp445HUMI_2035","BgPG_LR_ssp445HUMI_2035","Cp_LR_ssp445HUMI_2035","Bg_LR_ssp445HUMI_2035","GPG_FR_ssp445HUMI_2035","BF_FR_ssp445HUMI_2035","CFPG_FR_ssp445HUMI_2035","RJF_FR_ssp445HUMI_2035",
  "CHP_CR_ssp445HUMI_2040","BF_CR_ssp445HUMI_2040","Bg_CR_ssp445HUMI_2040","Be_CR_ssp445HUMI_2040","BP_CR_ssp445HUMI_2040","DH_CR_ssp445HUMI_2040","CFPG_CR_ssp445HUMI_2040","BgPG_LR_ssp445HUMI_2040","Cp_LR_ssp445HUMI_2040","Bg_LR_ssp445HUMI_2040","GPG_FR_ssp445HUMI_2040","BF_FR_ssp445HUMI_2040","CFPG_FR_ssp445HUMI_2040","RJF_FR_ssp445HUMI_2040",
  "CHP_CR_ssp445HUMI_2045","BF_CR_ssp445HUMI_2045","Bg_CR_ssp445HUMI_2045","Be_CR_ssp445HUMI_2045","BP_CR_ssp445HUMI_2045","DH_CR_ssp445HUMI_2045","CFPG_CR_ssp445HUMI_2045","BgPG_LR_ssp445HUMI_2045","Cp_LR_ssp445HUMI_2045","Bg_LR_ssp445HUMI_2045","GPG_FR_ssp445HUMI_2045","BF_FR_ssp445HUMI_2045","CFPG_FR_ssp445HUMI_2045","RJF_FR_ssp445HUMI_2045",
  "CHP_CR_ssp445HUMI_2050","BF_CR_ssp445HUMI_2050","Bg_CR_ssp445HUMI_2050","Be_CR_ssp445HUMI_2050","BP_CR_ssp445HUMI_2050","DH_CR_ssp445HUMI_2050","CFPG_CR_ssp445HUMI_2050","BgPG_LR_ssp445HUMI_2050","Cp_LR_ssp445HUMI_2050","Bg_LR_ssp445HUMI_2050","GPG_FR_ssp445HUMI_2050","BF_FR_ssp445HUMI_2050","CFPG_FR_ssp445HUMI_2050","RJF_FR_ssp445HUMI_2050",
  "CHP_CR_ssp445HUMI_2055","BF_CR_ssp445HUMI_2055","Bg_CR_ssp445HUMI_2055","Be_CR_ssp445HUMI_2055","BP_CR_ssp445HUMI_2055","DH_CR_ssp445HUMI_2055","CFPG_CR_ssp445HUMI_2055","BgPG_LR_ssp445HUMI_2055","Cp_LR_ssp445HUMI_2055","Bg_LR_ssp445HUMI_2055","GPG_FR_ssp445HUMI_2055","BF_FR_ssp445HUMI_2055","CFPG_FR_ssp445HUMI_2055","RJF_FR_ssp445HUMI_2055",
  "CHP_CR_ssp445HUMI_2060","BF_CR_ssp445HUMI_2060","Bg_CR_ssp445HUMI_2060","Be_CR_ssp445HUMI_2060","BP_CR_ssp445HUMI_2060","DH_CR_ssp445HUMI_2060","CFPG_CR_ssp445HUMI_2060","BgPG_LR_ssp445HUMI_2060","Cp_LR_ssp445HUMI_2060","Bg_LR_ssp445HUMI_2060","GPG_FR_ssp445HUMI_2060","BF_FR_ssp445HUMI_2060","CFPG_FR_ssp445HUMI_2060","RJF_FR_ssp445HUMI_2060" 
)

EI_ep    <- em_data   %*% imp_ep_data
T_EI_ep  <- Te_data   %*% imp_ep_data

EI_ep_n   <- sweep(EI_ep,   2, norm_ep_vec, `*`)
T_EI_ep_n <- sweep(T_EI_ep, 2, norm_ep_vec, `*`)

NET_ep <- T_EI_ep_n - EI_ep_n
rownames(NET_ep) <- tech_name


NET_df <- as_tibble(NET_ep, rownames = "tech") %>%
  mutate(across(2:5,  ~ .x * 176624),
         across(6:11, ~ .x * 17891594),
         sum = rowSums(across(2:11)))


daly_lower   <- 148288
daly_upper   <- 200236
spec_lower   <- 4472899
spec_upper   <- 44728985

daly_mean    <- mean(c(daly_lower, daly_upper))
daly_sd      <- (daly_upper - daly_lower) / (2 * 1.96)

spec_mean    <- mean(c(spec_lower, spec_upper))
spec_sd      <- (spec_upper - spec_lower) / (2 * 1.96)

set.seed(123)
n_sims       <- 10000
n_tech       <- length(tech_name)
monet_sims   <- matrix(NA, nrow = n_tech, ncol = n_sims,
                       dimnames = list(tech_name, NULL))

for(i in seq_len(n_sims)) {
  daly_i <- rnorm(1, daly_mean, daly_sd)
  spec_i <- rnorm(1, spec_mean, spec_sd)
  
  mat_d <- NET_ep[, 1:4, drop = FALSE] * daly_i 
  mat_s <- NET_ep[, 5:10, drop = FALSE] * spec_i  
  
  monet_sims[, i] <- rowSums(mat_d) + rowSums(mat_s)
}

monet_summary <- tibble(
  tech    = tech_name,
  mean    = rowMeans(monet_sims),
  lower95 = apply(monet_sims, 1, quantile, probs = 0.025),
  upper95 = apply(monet_sims, 1, quantile, probs = 0.975)
)


combined_df <- NET_df %>%
  select(tech, deterministic_sum = sum) %>%     
  left_join(monet_summary, by = "tech") %>%    
  select(tech, deterministic_sum, mean, lower95, upper95)

ecores_alltype_ssp445HUMI <- combined_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )

write_csv(
  combined_df,
  "/---/eco_LCA_NET_EI_world_endpoint_ssp445HUMI.csv"
)


# --- Monetization of Environmental Impacts for SSP445LUMI Scenario (2025-2060) ---
library(readr)
library(dplyr)

emissions <- read.csv(
  "/---/LCA_AFR_Environment_ssp445LUMI.csv",
  header = TRUE, stringsAsFactors = FALSE
)
em_mat  <- emissions[, -2] %>% as.matrix() %>% t()
em_data <- em_mat[-1, ] %>% apply(2, as.numeric)

Te <- read.csv(
  "/---/LCA_AFR_Environment_Fossilenergy_ssp126.csv",
  header = TRUE, stringsAsFactors = FALSE
)
Te_mat  <- Te[, -2] %>% as.matrix() %>% t()
Te_data <- Te_mat[-1, ] %>% apply(2, as.numeric)

impact_ep    <- read.csv(
  "/---/impact_factors_endpoint.csv",
  header = TRUE, stringsAsFactors = FALSE
)
imp_ep_mat   <- impact_ep[, -2] %>% as.matrix()
imp_ep_data  <- imp_ep_mat[, -1] %>% apply(2, as.numeric)

norm_ep      <- read.csv(
  "/---/normalization_factors_endpoint.csv",
  header = FALSE, stringsAsFactors = FALSE
)
norm_ep_vec <- as.numeric(norm_ep[, 2])

tech_name <- c(
  "CHP_CR_ssp445LUMI_2025","BF_CR_ssp445LUMI_2025","Bg_CR_ssp445LUMI_2025","Be_CR_ssp445LUMI_2025","BP_CR_ssp445LUMI_2025","DH_CR_ssp445LUMI_2025","CFPG_CR_ssp445LUMI_2025","BgPG_LR_ssp445LUMI_2025","Cp_LR_ssp445LUMI_2025","Bg_LR_ssp445LUMI_2025","GPG_FR_ssp445LUMI_2025","BF_FR_ssp445LUMI_2025","CFPG_FR_ssp445LUMI_2025","RJF_FR_ssp445LUMI_2025",
  "CHP_CR_ssp445LUMI_2030","BF_CR_ssp445LUMI_2030","Bg_CR_ssp445LUMI_2030","Be_CR_ssp445LUMI_2030","BP_CR_ssp445LUMI_2030","DH_CR_ssp445LUMI_2030","CFPG_CR_ssp445LUMI_2030","BgPG_LR_ssp445LUMI_2030","Cp_LR_ssp445LUMI_2030","Bg_LR_ssp445LUMI_2030","GPG_FR_ssp445LUMI_2030","BF_FR_ssp445LUMI_2030","CFPG_FR_ssp445LUMI_2030","RJF_FR_ssp445LUMI_2030",
  "CHP_CR_ssp445LUMI_2035","BF_CR_ssp445LUMI_2035","Bg_CR_ssp445LUMI_2035","Be_CR_ssp445LUMI_2035","BP_CR_ssp445LUMI_2035","DH_CR_ssp445LUMI_2035","CFPG_CR_ssp445LUMI_2035","BgPG_LR_ssp445LUMI_2035","Cp_LR_ssp445LUMI_2035","Bg_LR_ssp445LUMI_2035","GPG_FR_ssp445LUMI_2035","BF_FR_ssp445LUMI_2035","CFPG_FR_ssp445LUMI_2035","RJF_FR_ssp445LUMI_2035",
  "CHP_CR_ssp445LUMI_2040","BF_CR_ssp445LUMI_2040","Bg_CR_ssp445LUMI_2040","Be_CR_ssp445LUMI_2040","BP_CR_ssp445LUMI_2040","DH_CR_ssp445LUMI_2040","CFPG_CR_ssp445LUMI_2040","BgPG_LR_ssp445LUMI_2040","Cp_LR_ssp445LUMI_2040","Bg_LR_ssp445LUMI_2040","GPG_FR_ssp445LUMI_2040","BF_FR_ssp445LUMI_2040","CFPG_FR_ssp445LUMI_2040","RJF_FR_ssp445LUMI_2040",
  "CHP_CR_ssp445LUMI_2045","BF_CR_ssp445LUMI_2045","Bg_CR_ssp445LUMI_2045","Be_CR_ssp445LUMI_2045","BP_CR_ssp445LUMI_2045","DH_CR_ssp445LUMI_2045","CFPG_CR_ssp445LUMI_2045","BgPG_LR_ssp445LUMI_2045","Cp_LR_ssp445LUMI_2045","Bg_LR_ssp445LUMI_2045","GPG_FR_ssp445LUMI_2045","BF_FR_ssp445LUMI_2045","CFPG_FR_ssp445LUMI_2045","RJF_FR_ssp445LUMI_2045",
  "CHP_CR_ssp445LUMI_2050","BF_CR_ssp445LUMI_2050","Bg_CR_ssp445LUMI_2050","Be_CR_ssp445LUMI_2050","BP_CR_ssp445LUMI_2050","DH_CR_ssp445LUMI_2050","CFPG_CR_ssp445LUMI_2050","BgPG_LR_ssp445LUMI_2050","Cp_LR_ssp445LUMI_2050","Bg_LR_ssp445LUMI_2050","GPG_FR_ssp445LUMI_2050","BF_FR_ssp445LUMI_2050","CFPG_FR_ssp445LUMI_2050","RJF_FR_ssp445LUMI_2050",
  "CHP_CR_ssp445LUMI_2055","BF_CR_ssp445LUMI_2055","Bg_CR_ssp445LUMI_2055","Be_CR_ssp445LUMI_2055","BP_CR_ssp445LUMI_2055","DH_CR_ssp445LUMI_2055","CFPG_CR_ssp445LUMI_2055","BgPG_LR_ssp445LUMI_2055","Cp_LR_ssp445LUMI_2055","Bg_LR_ssp445LUMI_2055","GPG_FR_ssp445LUMI_2055","BF_FR_ssp445LUMI_2055","CFPG_FR_ssp445LUMI_2055","RJF_FR_ssp445LUMI_2055",
  "CHP_CR_ssp445LUMI_2060","BF_CR_ssp445LUMI_2060","Bg_CR_ssp445LUMI_2060","Be_CR_ssp445LUMI_2060","BP_CR_ssp445LUMI_2060","DH_CR_ssp445LUMI_2060","CFPG_CR_ssp445LUMI_2060","BgPG_LR_ssp445LUMI_2060","Cp_LR_ssp445LUMI_2060","Bg_LR_ssp445LUMI_2060","GPG_FR_ssp445LUMI_2060","BF_FR_ssp445LUMI_2060","CFPG_FR_ssp445LUMI_2060","RJF_FR_ssp445LUMI_2060"
)

EI_ep    <- em_data   %*% imp_ep_data
T_EI_ep  <- Te_data   %*% imp_ep_data

EI_ep_n   <- sweep(EI_ep,   2, norm_ep_vec, `*`)
T_EI_ep_n <- sweep(T_EI_ep, 2, norm_ep_vec, `*`)

NET_ep <- T_EI_ep_n - EI_ep_n
rownames(NET_ep) <- tech_name


NET_df <- as_tibble(NET_ep, rownames = "tech") %>%
  mutate(across(2:5,  ~ .x * 176624),
         across(6:11, ~ .x * 17891594),
         sum = rowSums(across(2:11)))


daly_lower   <- 148288
daly_upper   <- 200236
spec_lower   <- 4472899
spec_upper   <- 44728985

daly_mean    <- mean(c(daly_lower, daly_upper))
daly_sd      <- (daly_upper - daly_lower) / (2 * 1.96)

spec_mean    <- mean(c(spec_lower, spec_upper))
spec_sd      <- (spec_upper - spec_lower) / (2 * 1.96)

set.seed(123)
n_sims       <- 10000
n_tech       <- length(tech_name)
monet_sims   <- matrix(NA, nrow = n_tech, ncol = n_sims,
                       dimnames = list(tech_name, NULL))

for(i in seq_len(n_sims)) {
  daly_i <- rnorm(1, daly_mean, daly_sd)
  spec_i <- rnorm(1, spec_mean, spec_sd)
  
  mat_d <- NET_ep[, 1:4, drop = FALSE] * daly_i   
  mat_s <- NET_ep[, 5:10, drop = FALSE] * spec_i  
  
  monet_sims[, i] <- rowSums(mat_d) + rowSums(mat_s)
}

monet_summary <- tibble(
  tech    = tech_name,
  mean    = rowMeans(monet_sims),
  lower95 = apply(monet_sims, 1, quantile, probs = 0.025),
  upper95 = apply(monet_sims, 1, quantile, probs = 0.975)
)

combined_df <- NET_df %>%
  select(tech, deterministic_sum = sum) %>%     
  left_join(monet_summary, by = "tech") %>%    
  select(tech, deterministic_sum, mean, lower95, upper95)

ecores_alltype_ssp445LUMI <- combined_df %>%
  extract(
    col     = tech,
    into    = c("technology", "scenario", "Year"),
    regex   = "^(.*)_(.*)_(.*)$",
    convert = TRUE
  )

write_csv(
  combined_df,
  "/---/eco_LCA_NET_EI_world_endpoint_ssp445LUMI.csv"
)

# --- Monetization of Environmental Impacts for SSP585 Scenario ---
library(readr)
library(dplyr)

emissions <- read.csv(
  "/---/LCA_AFR_Environment.csv",
  header = TRUE, stringsAsFactors = FALSE
)
em_mat  <- emissions[, -2] %>% as.matrix() %>% t()
em_data <- em_mat[-1, ] %>% apply(2, as.numeric)

Te <- read.csv(
  "/---/LCA_AFR_Environment_Fossilenergy.csv",
  header = TRUE, stringsAsFactors = FALSE
)
Te_mat  <- Te[, -2] %>% as.matrix() %>% t()
Te_data <- Te_mat[-1, ] %>% apply(2, as.numeric)

impact_ep    <- read.csv(
  "/---/impact_factors_endpoint.csv",
  header = TRUE, stringsAsFactors = FALSE
)
imp_ep_mat   <- impact_ep[, -2] %>% as.matrix()
imp_ep_data  <- imp_ep_mat[, -1] %>% apply(2, as.numeric)

norm_ep      <- read.csv(
  "/---/normalization_factors_endpoint.csv",
  header = FALSE, stringsAsFactors = FALSE
)
norm_ep_vec <- as.numeric(norm_ep[, 2])

tech_name <- c(
  "CHP_AR","BF_AR","Bg_AR","Be_AR","BP_AR",
  "DH_AR","CFPG_AR","BgPG_LR","Cp_LR","Bg_LR",
  "GPG_WR","BF_WR","CFPG_WR","RJF_WR"
)

EI_ep    <- em_data   %*% imp_ep_data
T_EI_ep  <- Te_data   %*% imp_ep_data

EI_ep_n   <- sweep(EI_ep,   2, norm_ep_vec, `*`)
T_EI_ep_n <- sweep(T_EI_ep, 2, norm_ep_vec, `*`)

NET_ep <- T_EI_ep_n - EI_ep_n
rownames(NET_ep) <- tech_name


NET_df <- as_tibble(NET_ep, rownames = "tech") %>%
  mutate(across(2:5,  ~ .x * 176624),
         across(6:11, ~ .x * 17891594),
         sum = rowSums(across(2:11)))


daly_lower   <- 148288
daly_upper   <- 200236
spec_lower   <- 4472899
spec_upper   <- 44728985

daly_mean    <- mean(c(daly_lower, daly_upper))
daly_sd      <- (daly_upper - daly_lower) / (2 * 1.96)

spec_mean    <- mean(c(spec_lower, spec_upper))
spec_sd      <- (spec_upper - spec_lower) / (2 * 1.96)

set.seed(123)
n_sims       <- 10000
n_tech       <- length(tech_name)
monet_sims   <- matrix(NA, nrow = n_tech, ncol = n_sims,
                       dimnames = list(tech_name, NULL))

for(i in seq_len(n_sims)) {
  daly_i <- rnorm(1, daly_mean, daly_sd)
  spec_i <- rnorm(1, spec_mean, spec_sd)
  
  mat_d <- NET_ep[, 1:4, drop = FALSE] * daly_i   
  mat_s <- NET_ep[, 5:10, drop = FALSE] * spec_i  
  
  monet_sims[, i] <- rowSums(mat_d) + rowSums(mat_s)
}

monet_summary <- tibble(
  tech    = tech_name,
  mean    = rowMeans(monet_sims),
  lower95 = apply(monet_sims, 1, quantile, probs = 0.025),
  upper95 = apply(monet_sims, 1, quantile, probs = 0.975)
)


combined_df <- NET_df %>%
  select(tech, deterministic_sum = sum) %>%     
  left_join(monet_summary, by = "tech") %>%    
  select(tech, deterministic_sum, mean, lower95, upper95)

ecores_alltype_ssp585 <- combined_df %>%
  rename(technology = tech) %>%
  mutate(scenario = "ssp585", Year = 2020)

write_csv(combined_df, "/---/eco_LCA_NET_EI_world_endpoint_ssp585.csv")


list_df <- list(
  ssp126     = ecores_alltype_ssp126,
  ssp245     = ecores_alltype_ssp245,
  ssp445LUMI = ecores_alltype_ssp445LUMI,
  ssp445HUMI = ecores_alltype_ssp445HUMI,
  ssp585     = ecores_alltype_ssp585
)

ecores_alltype <- bind_rows(list_df, .id = "scenario")
write_csv(ecores_alltype, "/---/ecores_alltype.csv")
