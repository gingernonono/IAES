## Genetic algorithm optimization section----

# Load required libraries for data handling, optimization, and outputlibrary(readxl)
library(openxlsx)
library(GA)
library(dplyr)
library(tibble)
library(purrr)

# --- Sustainability Optimization for All Technologies ---
# Define a function to optimize technology allocation 
optimize_group <- function(mat, suffix){
  # Identify columns in the matrix corresponding to the specified residue type
  techs <- grep(paste0(suffix, "$"), colnames(mat), value = TRUE)
  
  # If no technologies are found, return an empty named vector
  if(length(techs)==0) return(set_names(rep(NA,0), character(0)))
  
  # Extract vectors for environmental, energy, economic, social, and sustainability metrics
  env_vec  <- mat["env",   techs]
  ener_vec <- mat["ene",   techs]
  econ_vec <- mat["eco",   techs]
  soc_vec  <- mat["soc",   techs]
  sus_vec  <- mat["sus",   techs]
  
  # Calculate average values for each metric to use as constraints
  avg_env  <- mean(env_vec)
  avg_ener <- mean(ener_vec)
  avg_econ <- mean(econ_vec)
  avg_soc  <- mean(soc_vec)
  
  # Define fitness function to maximize sustainability while enforcing constraints
  fitness <- function(x){
    x[x<0] <- 0
    if(sum(x)==0) x <- rep(1/length(x), length(x)) else x <- x/sum(x)
    
    # Calculate weighted sums for each metric
    w_env  <- sum(x * env_vec)
    w_ener <- sum(x * ener_vec)
    w_econ <- sum(x * econ_vec)
    w_soc  <- sum(x * soc_vec)
    w_sus  <- sum(x * sus_vec)
    
    # Apply penalties for constraint violations
    pen <- 0
    pen <- pen + pmax(0, avg_env  - w_env )*10
    pen <- pen + pmax(0, avg_ener - w_ener)*10
    pen <- pen + pmax(0, avg_econ - w_econ)*10
    pen <- pen + pmax(0, avg_soc  - w_soc )*10
    pen <- pen + sum(pmax(0, 0.05 - x)) * 100
    pen <- pen + sum(pmax(0, x - 0.6))  * 100
    
    return(w_sus - pen)
  }
  
  # Run genetic algorithm to find optimal weights
  ga_res <- ga(
    type       = "real-valued",                     # Optimize real-valued weights
    fitness    = fitness,                           # Use defined fitness function
    lower      = rep(0, length(techs)),             # Minimum weight is 0
    upper      = rep(1, length(techs)),             # Maximum weight is 1
    popSize    = 80,                                # Population size for GA
    maxiter    = 100,                               # Maximum number of iterations
    pcrossover = 0.8,                               # Crossover probability
    pmutation  = 0.4,                               # Mutation probability
    elitism    = 1,                                 # Number of elite solutions to retain
    run        = 20,                                # Stop if no improvement after 20 generations
    seed       = 123,                               # Set seed for reproducibility
    selection  = function(obj) gareal_tourSelection(obj, k = 3),  # Tournament selection
    optim      = TRUE,                              # Enable local optimization
    optimArgs  = list(method = "L-BFGS-B", control = list(maxit = 50))  # Local optimization 
  )
  
  # Extract and normalize the best solution
  sol <- ga_res@solution[1,]
  sol <- pmax(sol, 0)  # Ensure non-negative weights
  sol <- sol / sum(sol)  # Normalize weights to sum to 1
  names(sol) <- techs  # Assign technology names to weights
  return(sol)
  
}

# Read input data and process each sheet for sustainability optimization
file_in <- "/---/GArawdata.xlsx"
sheets  <- excel_sheets(file_in)
out_CR <- list()
out_LR <- list()
out_FR <- list()

# Loop through each sheet to process data and optimize technology allocation
for (sh in sheets) {
  # Read data, skipping first 35 rows (metadata) and ignoring column names
  raw <- read_excel(file_in, sheet = sh, skip = 35, col_names = FALSE)
  
  # Extract header and filter out NA or empty columns
  header <- raw[1, ] %>% unlist() %>% as.character()
  keep   <- which(!is.na(header) & header != "")
  raw    <- raw[, keep]
  header <- header[keep]
  
  # Process data body and assign column names
  body <- raw[-1, ]
  colnames(body) <- header
  dims <- tolower(body[[1]])  # Extract dimension names (env, ene, eco, soc, sus)
  mat <- body[, -1] %>%
    mutate(across(everything(), as.numeric)) %>%  # Convert to numeric
    as.matrix()
  rownames(mat) <- dims  # Assign dimension names to rows
  
  # Optimize for each residue type and store results
  out_CR[[sh]] <- optimize_group(mat, "CR")
  out_LR[[sh]] <- optimize_group(mat, "LR")
  out_FR[[sh]] <- optimize_group(mat, "FR")
}

# Function to convert optimization results to a data frame
make_df <- function(out_list){
  map_dfr(names(out_list), function(sh){
    sol <- out_list[[sh]]
    if(length(sol)==0){
      tibble(sheet = sh)
    } else {
      tibble(sheet = sh) %>%
        bind_cols(as_tibble(as.list(sol)))
    }
  })
}

# Create data frames for each residue type
df_CR <- make_df(out_CR)
df_LR <- make_df(out_LR)
df_FR <- make_df(out_FR)

# Save results to an Excel workbook
wb <- createWorkbook()
addWorksheet(wb, "CR"); writeData(wb, "CR", df_CR)
addWorksheet(wb, "LR"); writeData(wb, "LR", df_LR)
addWorksheet(wb, "FR"); writeData(wb, "FR", df_FR)
saveWorkbook(wb, "/---/GA_sus_opt_res.xlsx", overwrite = TRUE)



# --- Economic Optimization for All Technologies ---
optimize_group_eco_all <- function(mat, suffix){
  techs <- grep(paste0(suffix, "$"), colnames(mat), value = TRUE)
  if(length(techs)==0) return(set_names(rep(NA,0), character(0)))
  
  env_vec  <- mat["env",   techs]
  ener_vec <- mat["ene",   techs]
  econ_vec <- mat["eco",   techs]
  soc_vec  <- mat["soc",   techs]
  sus_vec  <- mat["sus",   techs]
  avg_env  <- mean(env_vec)
  avg_ener <- mean(ener_vec)
  avg_sus <- mean(sus_vec)
  avg_soc  <- mean(soc_vec)
  
  fitness <- function(x){
    x[x<0] <- 0
    if(sum(x)==0) x <- rep(1/length(x), length(x)) else x <- x/sum(x)
    
    w_env  <- sum(x * env_vec)
    w_ener <- sum(x * ener_vec)
    w_econ <- sum(x * econ_vec)
    w_soc  <- sum(x * soc_vec)
    w_sus  <- sum(x * sus_vec)
    
    pen <- 0
    pen <- pen + pmax(0, avg_env  - w_env )*10
    pen <- pen + pmax(0, avg_ener - w_ener)*10
    pen <- pen + pmax(0, avg_sus - w_sus)*10
    pen <- pen + pmax(0, avg_soc  - w_soc )*10
    pen <- pen + sum(pmax(0, 0.05 - x)) * 100
    pen <- pen + sum(pmax(0, x - 0.6))  * 100
    
    return(w_econ - pen)
  }
  
  ga_res <- ga(
    type       = "real-valued",
    fitness    = fitness,
    lower      = rep(0, length(techs)),
    upper      = rep(1, length(techs)),
    popSize    = 80,
    maxiter    = 100,
    pcrossover = 0.8,
    pmutation  = 0.4,
    elitism    = 1,
    run        = 20,
    seed       = 123,
    selection  = function(obj) gareal_tourSelection(obj, k = 3),
    optim      = TRUE,
    optimArgs  = list(method = "L-BFGS-B", control = list(maxit = 50))
  )
  
  sol <- ga_res@solution[1,]
  sol <- pmax(sol, 0)
  sol <- sol / sum(sol)
  names(sol) <- techs
  return(sol)
}

file_in <- "/---/GArawdata.xlsx"
sheets  <- excel_sheets(file_in)

out_CR <- list()
out_LR <- list()
out_FR <- list()


for(sh in sheets){
  raw <- read_excel(file_in, sheet = sh, skip = 35, col_names = FALSE)
  
  header <- raw[1, ] %>% unlist() %>% as.character()
  keep   <- which(!is.na(header) & header != "")
  
  raw    <- raw[, keep]
  header <- header[keep]
  
  body <- raw[-1, ]
  colnames(body) <- header
  
  dims <- tolower(body[[1]])
  mat  <- body[,-1] %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  rownames(mat) <- dims
  
  out_CR[[sh]] <- optimize_group_eco_all(mat, "CR")
  out_LR[[sh]] <- optimize_group_eco_all(mat, "LR")
  out_FR[[sh]] <- optimize_group_eco_all(mat, "FR")
}

make_df <- function(out_list){
  map_dfr(names(out_list), function(sh){
    sol <- out_list[[sh]]
    if(length(sol)==0){
      tibble(sheet = sh)
    } else {
      tibble(sheet = sh) %>%
        bind_cols(as_tibble(as.list(sol)))
    }
  })
}

df_CR <- make_df(out_CR)
df_LR <- make_df(out_LR)
df_FR <- make_df(out_FR)

wb <- createWorkbook()
addWorksheet(wb, "CR"); writeData(wb, "CR", df_CR)
addWorksheet(wb, "LR"); writeData(wb, "LR", df_LR)
addWorksheet(wb, "FR"); writeData(wb, "FR", df_FR)
saveWorkbook(wb, "/---/GA_eco_all_opt_res.xlsx", overwrite = TRUE)





# --- Economic Optimization for Mature Technologies ----
optimize_group_eco_mature <- function(mat, suffix, select = NULL){
  all_techs <- grep(paste0(suffix, "$"), colnames(mat), value = TRUE)
  techs     <- if(is.null(select)) all_techs else intersect(all_techs, select)
  
  if(length(techs)==0){
    return(set_names(rep(0, length(all_techs)), all_techs))
  }
  
  env_vec   <- mat["env",   techs]
  ener_vec  <- mat["ene",   techs]
  econ_vec  <- mat["eco",   techs]
  soc_vec   <- mat["soc",   techs]
  sus_vec   <- mat["sus",   techs]
  avg_env   <- mean(env_vec)
  avg_ener  <- mean(ener_vec)
  avg_sus   <- mean(sus_vec)
  avg_soc   <- mean(soc_vec)
  
  fitness <- function(x){
    x[x < 0] <- 0
    if(sum(x)==0) x <- rep(1/length(x), length(x)) else x <- x/sum(x)
    
    w_env  <- sum(x * env_vec)
    w_ener <- sum(x * ener_vec)
    w_econ <- sum(x * econ_vec)
    w_soc  <- sum(x * soc_vec)
    w_sus  <- sum(x * sus_vec)
    
    pen <- 0
    pen <- pen + pmax(0, avg_env  - w_env ) * 10
    pen <- pen + pmax(0, avg_ener - w_ener) * 10
    pen <- pen + pmax(0, avg_sus  - w_sus ) * 10
    pen <- pen + pmax(0, avg_soc  - w_soc ) * 10
    pen <- pen + sum(pmax(0, 0.05 - x))   * 100
    pen <- pen + sum(pmax(0, x - 0.8))    * 100
    
    return(w_econ - pen)
  }
  
  ga_res <- ga(
    type       = "real-valued",
    fitness    = fitness,
    lower      = rep(0, length(techs)),
    upper      = rep(1, length(techs)),
    popSize    = 80,
    maxiter    = 100,
    pcrossover = 0.8,
    pmutation  = 0.4,
    elitism    = 1,
    run        = 20,
    seed       = 123,
    selection  = function(obj) gareal_tourSelection(obj, k = 3),
    optim      = TRUE,
    optimArgs  = list(method = "L-BFGS-B", control = list(maxit = 50))
  )
  
  sol_sub <- ga_res@solution[1, ]
  sol_sub <- pmax(sol_sub, 0)
  sol_sub <- sol_sub / sum(sol_sub)
  names(sol_sub) <- techs
  
  sol_full <- set_names(rep(0, length(all_techs)), all_techs)
  sol_full[names(sol_sub)] <- sol_sub
  return(sol_full)
}


file_in <- "/---/GArawdata.xlsx"
sheets  <- excel_sheets(file_in)

out_CR <- list()
out_LR <- list()
out_FR <- list()


for(sh in sheets){
  raw <- read_excel(file_in, sheet = sh, skip = 35, col_names = FALSE)
  
  header <- raw[1, ] %>% unlist() %>% as.character()
  keep   <- which(!is.na(header) & header != "")
  
  raw    <- raw[, keep]
  header <- header[keep]
  
  body <- raw[-1, ]
  colnames(body) <- header
  
  dims <- tolower(body[[1]])
  mat  <- body[,-1] %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  rownames(mat) <- dims
  
  out_CR[[sh]] <- optimize_group_eco_mature(mat, "CR", select = c("CHP_CR","BF_CR",	"Bg_CR","DH_CR","CFPG_CR"))
  out_LR[[sh]] <- optimize_group_eco_mature(mat, "LR", select = c("BgPG_LR","Cp_LR","Bg_LR"))
  out_FR[[sh]] <- optimize_group_eco_mature(mat, "FR", select = c("BF_FR","CFPG_FR"))
}

make_df <- function(out_list){
  map_dfr(names(out_list), function(sh){
    sol <- out_list[[sh]]

    if(length(sol)==0){
      tibble(sheet = sh)
    } else {
      tibble(sheet = sh) %>%
        bind_cols(as_tibble(as.list(sol)))
    }
  })
}

df_CR <- make_df(out_CR)
df_LR <- make_df(out_LR)
df_FR <- make_df(out_FR)

wb <- createWorkbook()
addWorksheet(wb, "CR"); writeData(wb, "CR", df_CR)
addWorksheet(wb, "LR"); writeData(wb, "LR", df_LR)
addWorksheet(wb, "FR"); writeData(wb, "FR", df_FR)
saveWorkbook(wb, "/---/GA_eco_mat_opt_res.xlsx", overwrite = TRUE)

