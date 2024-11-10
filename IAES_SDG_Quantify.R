#IAES_SDG Research ####

#CRs forcasting####
library(readxl)

file_path <- "/.../ag production by crop type.xlsx"

sheet_names <- excel_sheets(file_path)

list_of_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

combined_data <- do.call(rbind, list_of_sheets)
combined_data$AR <- c(0)

Corn <- filter(combined_data,sector == "Corn")
#RPR means Residue-to-product ratio
Corn_RPR <- 1.1
#CF means Collection factor
Corn_CF <- 0.95
Corn$AR <- Corn$value * Corn_RPR * Corn_CF

FiberCrop <- filter(combined_data,sector == "FiberCrop")
FiberCrop_RPR <- 6.5
FiberCrop_CF <- 0.85
FiberCrop$AR <- FiberCrop$value * FiberCrop_RPR * FiberCrop_CF

Legumes <- filter(combined_data,sector == "Legumes")
Legumes_RPR <- 1.6
Legumes_CF <- 0.56
Legumes$AR <- Legumes$value * Legumes_RPR * Legumes_CF

OilCrop <- filter(combined_data,sector == "OilCrop")
OilCrop_RPR <- 2.7
OilCrop_CF <- 0.64
OilCrop$AR <- OilCrop$value * OilCrop_RPR * OilCrop_CF

OilPalm <- filter(combined_data,sector == "OilPalm")
OilPalm_RPR <- 1 #process
OilPalm_CF <- 0.44
OilPalm$AR <- OilPalm$value * OilPalm_RPR * OilPalm_CF

Rice <- filter(combined_data,sector == "Rice")
Rice_RPR <- 0.95
Rice_CF <- 0.8
Rice$AR <- Rice$value * Rice_RPR * Rice_CF

RootTuber <- filter(combined_data,sector == "RootTuber")
RootTuber_RPR <- 0.63
RootTuber_CF <- 0.73
RootTuber$AR <- RootTuber$value * RootTuber_RPR * RootTuber_CF

Soybean <- filter(combined_data,sector == "Soybean")
Soybean_RPR <- 1.6
Soybean_CF <- 0.56
Soybean$AR <- Soybean$value * Soybean_RPR * Soybean_CF

SugarCrop <- filter(combined_data,sector == "SugarCrop")
SugarCrop_RPR <- 0.1
SugarCrop_CF <- 0.7
SugarCrop$AR <- SugarCrop$value * SugarCrop_RPR * SugarCrop_CF

Wheat <- filter(combined_data,sector == "Wheat")
Wheat_RPR <- 1.3
Wheat_CF <- 0.65
Wheat$AR <- Wheat$value * Wheat_RPR * Wheat_CF

OtherGrain <- filter(combined_data,sector == "OtherGrain")
OtherGrain_RPR <- 1.5 
OtherGrain_CF <- 0.875 
OtherGrain$AR <- OtherGrain$value * OtherGrain_RPR * OtherGrain_CF

Agri_ <- rbind(Corn, FiberCrop, Legumes, OilCrop, OilPalm, Rice, RootTuber, Soybean, SugarCrop, Wheat, OtherGrain)
write.csv(Agri_,"/.../Agri_.csv")
AR_ <- summarise(group_by(Agri_, region, scenario, Year),sum(AR))
write.csv(AR_,"/.../AR_.csv")

------------------------------------------------------------------------------------------------------

#LEs forcasting#####
#Africa_Eastern
library(tidyverse)
library(caret)  
data <- read.csv("/.../Africa_Eastern.csv")

names(data)
#for test
index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]

set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)

library(mgcv)
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

#for forcasting
set.seed(123) 

final_gam_model <- gam(LRs ~ s(Beef, k = 4) + s(Dairy, k = 4) + s(Pork, k = 4) + s(Poultry, k = 4) + s(SheepGoat, k = 4), data = trainData)

newData <- read.csv("/.../GCAM_Africa_Eastern.csv")
new_predictions <- predict(final_gam_model, newdata=newData)

newData$LRs <- new_predictions 
newData$LRs <- newData$LRs / 1000
write.csv(newData, "/.../GCAM_Africa_Eastern.csv", row.names = FALSE)

#Africa_Northern
data <- read.csv("/.../Africa_Northern.csv")

names(data)

index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]

set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)

library(mgcv)
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

newData <- read.csv("/.../GCAM_Africa_Northern.csv")

new_predictions <- predict(final_gam_model, newdata=newData)
newData$LRs <- new_predictions 
newData$LRs <- newData$LRs / 1000
write.csv(newData, "/.../GCAM_Africa_Northern.csv", row.names = FALSE)

#Africa_Southern-
data <- read.csv("/.../Africa_Southern.csv")
names(data)

index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]

set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)

library(mgcv)

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

newData <- read.csv("/.../GCAM_Africa_Southern.csv")

new_predictions <- predict(final_gam_model, newdata=newData)
newData$LRs <- new_predictions 
newData$LRs <- newData$LRs / 1000
write.csv(newData, "/.../GCAM_Africa_Southern.csv", row.names = FALSE)


#Africa_Western-
data <- read.csv("/.../Africa_Western.csv")
names(data)

index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]
set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)

library(mgcv)
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


newData <- read.csv("/.../GCAM_Africa_Western.csv")

new_predictions <- predict(final_gam_model, newdata=newData)
newData$LRs <- new_predictions 
newData$LRs <- newData$LRs / 1000
write.csv(newData, "/.../GCAM_Africa_Western.csv", row.names = FALSE)

#Argentina---
data <- read.csv("/.../Argentina.csv")

names(data)

index <- createDataPartition(data$LRs, p=0.7, list=FALSE)
trainData <- data[index, ]

set.seed(123) 
k <- 5 
folds <- createFolds(trainData$LRs, k = k, list = TRUE, returnTrain = TRUE)

library(mgcv)
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

newData <- read.csv("/.../GCAM_Argentina.csv")

new_predictions <- predict(final_gam_model, newdata=newData)
newData$LRs <- new_predictions 
newData$LRs <- newData$LRs / 1000
write.csv(newData, "/.../GCAM_Argentina.csv", row.names = FALSE)

#the other regions all same....
--------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------
  
#CRs - Genetic Algorithm Optimization - sustainable ####
library(mco)
ARtec_obj <- function(x) {
  y <- numeric(8)
  y[1] <- -10.553*x[1] - 9.339*x[2] - 7.698*x[3] - 5.897*x[4] - 7.360*x[5] - 2.683*x[6] - 7.105*x[7]
  y[2] <- -(9289.806*x[1] + 10028.028*x[2] + 5736.79*x[3] + 2753.45*x[4] + 1655*x[5] + 1383.947*x[6] + 2244.564*x[7])
  y[3] <- -(1.576*x[1] + 1.288*x[2] + 0.626*x[3] + 0.466*x[4] + 0.703*x[5] + 0.583*x[6] + 0.756*x[7])
  y[4] <- -(8.73*x[1] + 7.14*x[2] + 15.38*x[3] + 5.56*x[4] + 12.24*x[5] + 0*x[6] + 5.06*x[7])
  y[5] <- -(32.397*x[1] + 28.259*x[2] + 32.499*x[3] + 23.145*x[4] + 27.207*x[5] + 0*x[6] + 22.733*x[7])
  y[6] <- x[1] + x[2] + x[3] + x[4] + x[5] + x[6] + x[7] - 7
  return(y)
}
ARtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3] + x[4] + x[5] + x[6] + x[7] - 1
  y2_constraint <- 4727.369 - ARtec_obj(x)[2]
  y3_constraint <- 0.857 - ARtec_obj(x)[3]
  y4_constraint <- 7.73 - ARtec_obj(x)[4]
  y5_constraint <- 23.749 - ARtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y4_constraint, y5_constraint))
}
ARres <- nsga2(ARtec_obj, 7, 8,  
               generations = 150, popsize = 200,
               
               lower.bounds = c(0, 0, 0, 0, 0, 0, 0),  
               upper.bounds = c(1, 1, 1, 1, 1, 1, 1),
               constraints = ARtec_con  
)

ARtec_sum_per_individual <- apply(ARres[["par"]], 1, sum)

ARtec_closest_to_1_10 <- order(abs(ARtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", ARtec_sum_per_individual[ARtec_closest_to_1[i]]))
  print(paste("Index:", ARtec_closest_to_1_10[i]))
}


#LEs - Genetic Algorithm Optimization- sustainable####
library(mco)
LRtec_obj <- function(x) {
  y <- numeric(4)
  y[1] <- -9.150*x[1] - 4.691*x[2] - 8.255*x[3] 
  y[2] <- -(2886.993*x[1] + 0*x[2] + 4640.079*x[3] )
  y[3] <- -(0.869*x[1] + 0.397*x[2] + 0.653*x[3] )
  y[4] <- -(8.33*x[1] + 15.87*x[2] + 13.3*x[3] )
  y[5] <- -(26.667*x[1] + 23.888*x[2] + 30.833*x[3] )
  y[6] <- x[1] + x[2] + x[3]  - 3
  
  return(y)
}
LRtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3]  - 1
  y2_constraint <- 2509.024 - LRtec_obj(x)[2]
  y3_constraint <- 0.640 - LRtec_obj(x)[3]
  y4_constraint <- 12.5 - LRtec_obj(x)[4]
  y5_constraint <- 27.129 - LRtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y4_constraint, y5_constraint))
}
LRres <- nsga2(LRtec_obj, 3, 6,  
               generations = 150, popsize = 200,

               lower.bounds = c(0, 0, 0),  
               upper.bounds = c(1, 1, 1),
               constraints = LRtec_con  
)

LRtec_sum_per_individual <- apply(LRres[["par"]], 1, sum)

LRtec_closest_to_1_10 <- order(abs(LRtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", LRtec_sum_per_individual[LRtec_closest_to_1[i]]))
  print(paste("Index:", LRtec_closest_to_1_10[i]))
} 

#FRs - Genetic Algorithm Optimization- sustainable####

library(mco)
WRtec_obj <- function(x) {
  y <- numeric(5)
  y[1] <- -6.593*x[1] - 9.724*x[2] - 7.283*x[3] -6.739*x[4]
  y[2] <- -(4616.628*x[1] + 17285.421*x[2] + 2244.564*x[3] + 1456.162*x[4] )
  y[3] <- -(0.645*x[1] + 0.913*x[2] + 0.756*x[3] - 0.104*x[4])
  y[4] <- -(4.08*x[1] + 5.83*x[2] + 5.06*x[3] + 11.77*x[4])
  y[5] <- -(14.825*x[1] + 28.023*x[2] + 17.725*x[3] + 29.643*x[4])
  y[6] <- x[1] + x[2] + x[3]  + x[4] - 4
  
  return(y)
}
WRtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3] + x[4] - 1
  y2_constraint <- 6400.694 - WRtec_obj(x)[2]
  y3_constraint <- 0.552 - WRtec_obj(x)[3]
  y4_constraint <- 6.685 - WRtec_obj(x)[4]
  y5_constraint <- 22.554 - WRtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y4_constraint, y5_constraint))
}
WRres <- nsga2(WRtec_obj, 4, 6, 
               generations = 150, popsize = 200,
               
               lower.bounds = c(0, 0, 0, 0),  
               upper.bounds = c(1, 1, 1, 1),
               constraints = WRtec_con  
)

WRtec_sum_per_individual <- apply(WRres[["par"]], 1, sum)

WRtec_closest_to_1_10 <- order(abs(WRtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", WRtec_sum_per_individual[WRtec_closest_to_1[i]]))
  print(paste("Index:", WRtec_closest_to_1_10[i]))
}   



#CRs - Genetic Algorithm Optimization - economic ####
library(mco)
ARtec_obj <- function(x) {
  y <- numeric(8)
  y[1] <- -10.553*x[1] - 9.339*x[2] - 7.698*x[3] - 5.897*x[4] - 7.360*x[5] - 2.683*x[6] - 7.105*x[7]
  y[2] <- -(9289.806*x[1] + 10028.028*x[2] + 5736.79*x[3] + 2753.45*x[4] + 1655*x[5] + 1383.947*x[6] + 2244.564*x[7])
  y[3] <- -(1.576*x[1] + 1.288*x[2] + 0.626*x[3] + 0.466*x[4] + 0.703*x[5] + 0.583*x[6] + 0.756*x[7])
  y[4] <- -(8.73*x[1] + 7.14*x[2] + 15.38*x[3] + 5.56*x[4] + 12.24*x[5] + 0*x[6] + 5.06*x[7])
  y[5] <- -(32.397*x[1] + 28.259*x[2] + 32.499*x[3] + 23.145*x[4] + 27.207*x[5] + 0*x[6] + 22.733*x[7])
  y[6] <- x[1] + x[2] + x[3] + x[4] + x[5] + x[6] + x[7] - 7
  return(y)
}
ARtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3] + x[4] + x[5] + x[6] + x[7] - 1
  y2_constraint <- 4727.369 - ARtec_obj(x)[2]
  y3_constraint <- 0.857 - ARtec_obj(x)[3]
  y1_constraint <- 7.234 - ARtec_obj(x)[1]
  y5_constraint <- 23.749 - ARtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y1_constraint, y5_constraint))
}
ARres <- nsga2(ARtec_obj, 7, 8,  
               generations = 150, popsize = 200,
               
               lower.bounds = c(0, 0, 0, 0, 0, 0, 0),  
               upper.bounds = c(1, 1, 1, 1, 1, 1, 1),
               constraints = ARtec_con  
)

ARtec_sum_per_individual <- apply(ARres[["par"]], 1, sum)

ARtec_closest_to_1_10 <- order(abs(ARtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", ARtec_sum_per_individual[ARtec_closest_to_1[i]]))
  print(paste("Index:", ARtec_closest_to_1_10[i]))
}


#LEs - Genetic Algorithm Optimization - economic ####
library(mco)
LRtec_obj <- function(x) {
  y <- numeric(4)
  y[1] <- -9.150*x[1] - 4.691*x[2] - 8.255*x[3] 
  y[2] <- -(2886.993*x[1] + 0*x[2] + 4640.079*x[3] )
  y[3] <- -(0.869*x[1] + 0.397*x[2] + 0.653*x[3] )
  y[4] <- -(8.33*x[1] + 15.87*x[2] + 13.3*x[3] )
  y[5] <- -(26.667*x[1] + 23.888*x[2] + 30.833*x[3] )
  y[6] <- x[1] + x[2] + x[3]  - 3
  
  return(y)
}
LRtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3]  - 1
  y2_constraint <- 2509.024 - LRtec_obj(x)[2]
  y3_constraint <- 0.640 - LRtec_obj(x)[3]
  y1_constraint <- 7.365 - LRtec_obj(x)[1]
  y5_constraint <- 27.129 - LRtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y1_constraint, y5_constraint))
}
LRres <- nsga2(LRtec_obj, 3, 6,  
               generations = 150, popsize = 200,
               lower.bounds = c(0, 0, 0),  
               upper.bounds = c(1, 1, 1),
               constraints = LRtec_con  
)

LRtec_sum_per_individual <- apply(LRres[["par"]], 1, sum)

LRtec_closest_to_1_10 <- order(abs(LRtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", LRtec_sum_per_individual[LRtec_closest_to_1[i]]))
  print(paste("Index:", LRtec_closest_to_1_10[i]))
} 

#FRs - Genetic Algorithm Optimization - economic ####
library(mco)
WRtec_obj <- function(x) {
  y <- numeric(5)
  y[1] <- -6.593*x[1] - 9.724*x[2] - 7.283*x[3] -6.739*x[4]
  y[2] <- -(4616.628*x[1] + 17285.421*x[2] + 2244.564*x[3] + 1456.162*x[4] )
  y[3] <- -(0.645*x[1] + 0.913*x[2] + 0.756*x[3] - 0.104*x[4])
  y[4] <- -(4.08*x[1] + 5.83*x[2] + 5.06*x[3] + 11.77*x[4])
  y[5] <- -(14.825*x[1] + 28.023*x[2] + 17.725*x[3] + 29.643*x[4])
  y[6] <- x[1] + x[2] + x[3]  + x[4] - 4
  
  return(y)
}
WRtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3] + x[4] - 1
  y2_constraint <- 6400.694 - WRtec_obj(x)[2]
  y3_constraint <- 0.552 - WRtec_obj(x)[3]
  y1_constraint <- 7.585 - WRtec_obj(x)[1]
  y5_constraint <- 22.554 - WRtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y1_constraint, y5_constraint))
}
WRres <- nsga2(WRtec_obj, 4, 6, 
               generations = 150, popsize = 200,
               
               lower.bounds = c(0, 0, 0, 0),  
               upper.bounds = c(1, 1, 1, 1),
               constraints = WRtec_con  
)

WRtec_sum_per_individual <- apply(WRres[["par"]], 1, sum)

WRtec_closest_to_1_10 <- order(abs(WRtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", WRtec_sum_per_individual[WRtec_closest_to_1[i]]))
  print(paste("Index:", WRtec_closest_to_1_10[i]))
}  


#CRs - Genetic Algorithm Optimization - economic - nonhightechnology ####
library(mco)
ARtec_obj <- function(x) {
  y <- numeric(6)
  y[1] <- -10.553*x[1] - 9.339*x[2] - 7.698*x[3]  - 2.683*x[4] - 7.105*x[5]
  y[2] <- -(9289.806*x[1] + 10028.028*x[2] + 5736.79*x[3] + 1383.947*x[4] + 2244.564*x[5])
  y[3] <- -(1.576*x[1] + 1.288*x[2] + 0.626*x[3] + 0.583*x[4] + 0.756*x[5])
  y[4] <- -(8.73*x[1] + 7.14*x[2] + 15.38*x[3] + 0*x[4] + 5.06*x[5])
  y[5] <- -(32.397*x[1] + 28.259*x[2] + 32.499*x[3] + 0*x[4] + 22.733*x[5])
  y[6] <- x[1] + x[2] + x[3] + x[4] + x[5] - 5
  return(y)
}
ARtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3] + x[4] + x[5] - 1
  y2_constraint <- 5736.627 - ARtec_obj(x)[2]
  y3_constraint <- 0.966 - ARtec_obj(x)[3]
  y1_constraint <- 7.476 - ARtec_obj(x)[1]
  y5_constraint <- 23.178 - ARtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y1_constraint, y5_constraint))
}
ARres <- nsga2(ARtec_obj, 5, 6,  
               generations = 150, popsize = 200,
               
               lower.bounds = c(0, 0, 0, 0, 0),  
               upper.bounds = c(1, 1, 1, 1, 1),
               constraints = ARtec_con  
)

ARtec_sum_per_individual <- apply(ARres[["par"]], 1, sum)


ARtec_closest_to_1_10 <- order(abs(ARtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", ARtec_sum_per_individual[ARtec_closest_to_1[i]]))
  print(paste("Index:", ARtec_closest_to_1_10[i]))
}


#FRs - Genetic Algorithm Optimization - economic - nonhightechnology ####
library(mco)
WRtec_obj <- function(x) {
  y <- numeric(5)
  y[1] <- -6.593*x[1] - 9.724*x[2] - 7.283*x[3] 
  y[2] <- -(4616.628*x[1] + 17285.421*x[2] + 2244.564*x[3]  )
  y[3] <- -(0.645*x[1] + 0.913*x[2] + 0.756*x[3] )
  y[4] <- -(4.08*x[1] + 5.83*x[2] + 5.06*x[3] )
  y[5] <- -(14.825*x[1] + 28.023*x[2] + 17.725*x[3] )
  y[6] <- x[1] + x[2] + x[3]  - 3
  
  return(y)
}
WRtec_con <- function(x) {
  sum_constraint <- x[1] + x[2] + x[3]  - 1
  y2_constraint <- 8048.871 - WRtec_obj(x)[2]
  y3_constraint <- 0.771 - WRtec_obj(x)[3]
  y1_constraint <- 7.867 - WRtec_obj(x)[1]
  y5_constraint <- 20.191 - WRtec_obj(x)[5]
  return(c(sum_constraint, y2_constraint, y3_constraint, y1_constraint, y5_constraint))
}
WRres <- nsga2(WRtec_obj, 3, 6,  
               generations = 150, popsize = 200,
               
               lower.bounds = c(0, 0, 0),  
               upper.bounds = c(1, 1, 1),
               constraints = WRtec_con  
)

WRtec_sum_per_individual <- apply(WRres[["par"]], 1, sum)

WRtec_closest_to_1_10 <- order(abs(WRtec_sum_per_individual - 1))[1:10]

for (i in 1:10) {
  print(paste("Optimization results:", WRtec_sum_per_individual[WRtec_closest_to_1[i]]))
  print(paste("Index:", WRtec_closest_to_1_10[i]))
}  

---------------------------------------------------------------------------------------------
#SDG quantify------  
  #CRs part----------------------------------------------------------------------------
constants_map <- list(
  SSP126 = list(developed = c(10.11710, 0.0001157, 0.96802, -0.00125,6905.62320, 10.15264, 0.0001157, 19.30865, 10.11710, 514.36329, 
                              0.96802, 1.00000, 0.12358, -0.03686, -0.00125,  0.27295, 364.777014),
                developing = c(10.11710, 0.0001157, 0.96802, -0.00125,6905.62320, 10.15264, 0.0001157, 19.30865, 10.11710, 514.36329, 
                               0.96802, 1.00000, 0.12358, -0.03686, -0.00125,  0.27295, 364.777014)), # 如果相同，只是重复数组
  SSP145 = list(developed = c(10.11710, 0.0001157, 0.96802, -0.00125,6905.62320, 10.15264, 0.0001157, 19.30865, 10.11710, 514.36329, 
                              0.96802, 1.00000, 0.12358, -0.03686, -0.00125,  0.27295, 364.777014),
                developing = c(10.11710, 0.0001157, 0.96802, -0.00125,6905.62320, 10.15264, 0.0001157, 19.30865, 10.11710, 514.36329, 
                               0.96802, 1.00000, 0.12358, -0.03686, -0.00125,  0.27295, 364.777014)),
  SSP226 = list(developed = c(10.1171, 0.000115734, 0.968020283, -0.001246368,6905.6232, 10.15264, 0.000115734, 19.30865, 10.1171, 
                              514.3632948, 0.968020283,  1,  0.123575353, -0.036859213, -0.001246368, 0.272951461, 364.7770118),
                developing = c(10.11710, 0.0001160, 0.88107, -0.00125, 5044.23301, 10.69923, 0.0001160, 18.71644, 10.11710, 554.38197, 
                               0.88107, 1.00000, 0.10832, -0.02442,  -0.00125, 0.25861, 327.77803)),
  SSP245 = list(developed = c(10.1171, 0.000115734, 0.968020283, -0.001246368,6905.6232, 10.15264, 0.000115734, 19.30865, 10.1171, 
                              514.3632948, 0.968020283,  1,  0.123575353, -0.036859213, -0.001246368, 0.272951461, 364.7770118), 
                developing = c(10.11710, 0.0001160, 0.88107, -0.00125, 5044.23301, 10.69923, 0.0001160, 18.71644, 10.11710, 554.38197, 
                               0.88107, 1.00000, 0.10832, -0.02442,  -0.00125, 0.25861, 327.77803)),
  SSP260 = list(developed = c(10.1171, 0.000115734, 0.968020283, -0.001246368,6905.6232, 10.15264, 0.000115734, 19.30865, 10.1171, 
                              514.3632948, 0.968020283,  1,  0.123575353, -0.036859213, -0.001246368, 0.272951461, 364.7770118), 
                developing = c(10.11710, 0.0001160, 0.88107, -0.00125, 5044.23301, 10.69923, 0.0001160, 18.71644, 10.11710, 554.38197, 
                               0.88107, 1.00000, 0.10832, -0.02442,  -0.00125, 0.25861, 327.77803)),
  SSP445 = list(developed = c(10.1171,  0.000115734, 0.968020283, -0.001246368,6905.6232,10.15264,0.000115734,19.30865,10.1171,514.3632948,
                              0.968020283, 1, 0.123575353,-0.036859213, -0.001246368,0.272951461, 364.7770118), 
                developing = c(10.117, 0.00011, 0.968,  0.000, 6823.988, 12.127, 0.00011, 26.910,  10.117, 1133.957, 0.968, 1.000, 0.129,
                               -0.067,  0.000, 0.263, 357.794)),
  SSP460 = list(developed = c(10.1171,  0.000115734, 0.968020283, -0.001246368,6905.6232,10.15264,0.000115734,19.30865,10.1171,514.3632948,
                              0.968020283, 1, 0.123575353,-0.036859213, -0.001246368,0.272951461, 364.7770118), 
                developing = c(10.117, 0.00011, 0.968,  0.000, 6823.988, 12.127, 0.00011, 26.910,  10.117, 1133.957, 0.968, 1.000, 0.129,
                               -0.067,  0.000, 0.263, 357.794)),
  SSP560 = list(developed = c(10.1171, 0.0001160, 0.88107, -0.00125,5044.23301,10.69923,0.0001160,18.71644,10.11710, 554.38197,0.88107, 1.00000,
                              0.10832,-0.02442,-0.00125,0.25861,327.77803), 
                developing = c(10.1171, 0.0001160, 0.88107, -0.00125,5044.23301,10.69923,0.0001160,18.71644,10.11710, 554.38197,0.88107, 1.00000,
                               0.10832,-0.02442,-0.00125,0.25861,327.77803))
)
library(readxl)
library(dplyr)
library(writexl)

file_path <- "/.../CRs.xlsx"

sheets <- excel_sheets(file_path)

modified_data <- list()

for (sheet_name in sheets) {
  data <- read_excel(file_path, sheet = sheet_name)
  
  for (i in 1:17) {
    new_col_name <- as.character(i)
    data <- data %>%
      mutate(!!new_col_name := case_when(
        Development == "developed" ~ constants_map[[sheet_name]][["developed"]][i] * AR_ener * 1000000,
        Development == "developing" ~ constants_map[[sheet_name]][["developing"]][i] * AR_ener * 1000000,
        TRUE ~ AR_ener * 1 
      ))
  }
  
  modified_data[[sheet_name]] <- data
}

write_xlsx(modified_data, path = "/.../CRs1.xlsx")


#population data process--------------------------------------------------
library(dplyr)
popgdp_filepath <- "/.../sspGDP.csv"
popgdp <- read.csv(popgdp_filepath)

pop_data <- filter(popgdp, Variable == "Population")  %>% filter(Scenario != "Historical Reference")
gdp_data <- filter(popgdp, Variable == "GDP|PPP") %>% filter(Scenario != "Historical Reference")

pop_data_32regions <- pop_data %>%
  group_by(Scenario, X32Region ) %>%
  summarise(
    x2025 = sum(X2025, na.rm = TRUE),
    x2030 = sum(X2030, na.rm = TRUE),
    x2035 = sum(X2035, na.rm = TRUE),
    x2040 = sum(X2040, na.rm = TRUE),
    x2045 = sum(X2045, na.rm = TRUE),
    x2050 = sum(X2050, na.rm = TRUE),
    x2055 = sum(X2055, na.rm = TRUE),
    x2060 = sum(X2060, na.rm = TRUE)
  ) 
pop_data_32regions$unit <- "milion"

library(tidyr)
library(dplyr)

pop_data_long <- pop_data_32regions %>%
  pivot_longer(
    cols = x2025:x2060,  
    names_to = "time",
    values_to = "value"
  ) %>%
  mutate(
    time = as.numeric(gsub("x", "", time))  
  )

pop_data_long <- pop_data_long %>%
  filter(X32Region != "")  
library(openxlsx)
library(dplyr)

wb <- createWorkbook()

scenarios <- unique(pop_data_long$Scenario)
for(scenario in scenarios) {
  scenario_data <- filter(pop_data_long, Scenario == scenario)
  
  addWorksheet(wb, sheetName = scenario)
  
  writeData(wb, sheet = scenario, scenario_data)
}

saveWorkbook(wb, file = "/.../popdata_ssp.xlsx", overwrite = TRUE)


gdp_data_32regions <- gdp_data %>%
  group_by(Scenario, X32Region ) %>%
  summarise(
    x2025 = sum(X2025, na.rm = TRUE),
    x2030 = sum(X2030, na.rm = TRUE),
    x2035 = sum(X2035, na.rm = TRUE),
    x2040 = sum(X2040, na.rm = TRUE),
    x2045 = sum(X2045, na.rm = TRUE),
    x2050 = sum(X2050, na.rm = TRUE),
    x2055 = sum(X2055, na.rm = TRUE),
    x2060 = sum(X2060, na.rm = TRUE)
  ) 
gdp_data_32regions$unit <- "billion USD_2017/yr"
library(tidyr)
library(dplyr)

gdp_data_long <- gdp_data_32regions %>%
  pivot_longer(
    cols = x2025:x2060,  
    names_to = "time",
    values_to = "value"
  ) %>%
  mutate(
    time = as.numeric(gsub("x", "", time))  
  )

gdp_data_long <- gdp_data_long %>%
  filter(X32Region != "")  
library(openxlsx)
library(dplyr)

wbgdp <- createWorkbook()

scenarios <- unique(gdp_data_long$Scenario)
for(scenario in scenarios) {
  scenario_data <- filter(gdp_data_long, Scenario == scenario)
  
  addWorksheet(wbgdp, sheetName = scenario)
  
  writeData(wbgdp, sheet = scenario, scenario_data)
}
saveWorkbook(wbgdp, file = "/.../gdpdata_ssp.xlsx", overwrite = TRUE)


#LEs part-----------------------------------------------------------------------------
constants_mapLR <- list(
  SSP126 = list(developed = c(12.50000, 0.0001170,0.67334,0.19759,4006.73012,12.60518,0.0001170,28.19482, 12.50000,551.47277,
                              0.67334,1.00000,0.04580,0.01897,0.19759,0.15517,163.84740), 
                developing = c(12.50000, 0.0001170,0.67334,0.19759,4006.73012,12.60518,0.0001170,28.19482, 12.50000,551.47277,
                               0.67334,1.00000,0.04580,0.01897,0.19759,0.15517,163.84740)), 
  SSP145 = list(developed = c(12.50000, 0.0001170,0.67334,0.19759,4006.73012,12.60518,0.0001170,28.19482, 12.50000,551.47277,
                              0.67334,1.00000,0.04580,0.01897,0.19759,0.15517,163.84740), 
                developing = c(12.50000, 0.0001170,0.67334,0.19759,4006.73012,12.60518,0.0001170,28.19482, 12.50000,551.47277,
                               0.67334,1.00000,0.04580,0.01897,0.19759,0.15517,163.84740)), 
  SSP226 = list(developed = c(12.5, 0.000116965,0.673335486,0.197594553,4006.730116,12.60518, 0.000116965,28.19482,12.5,551.472768,
                              0.673335486, 1, 0.045804624, 0.018971431, 0.197594553, 0.155166467, 163.8474042), 
                developing = c(12.50000, 0.0001170,0.64042,0.19228, 4106.35772, 13.28074, 0.0001170,30.76896,12.50000, 
                               166.06850,0.64042,1.00000,0.03498,0.01846,0.19228,0.15439,143.21182)),
  
  SSP245 = list(developed = c(12.5, 0.000116965,0.673335486,0.197594553,4006.730116,12.60518, 0.000116965,28.19482,12.5,551.472768,
                              0.673335486, 1, 0.045804624, 0.018971431, 0.197594553, 0.155166467, 163.8474042), 
                developing = c(12.50000, 0.0001170,0.64042,0.19228, 4106.35772, 13.28074, 0.0001170,30.76896,
                               12.50000, 166.06850,0.64042,1.00000,0.03498,0.01846,0.19228,0.15439,143.21182)),
  SSP260 = list(developed = c(12.5, 0.000116965,0.673335486,0.197594553,4006.730116,12.60518, 0.000116965,28.19482,12.5,551.472768,
                              0.673335486, 1, 0.045804624, 0.018971431, 0.197594553, 0.155166467, 163.8474042), 
                developing = c(12.50000, 0.0001170,0.64042,0.19228, 4106.35772, 13.28074, 0.0001170,30.76896,
                               12.50000, 166.06850,0.64042,1.00000,0.03498,0.01846,0.19228,0.15439,143.21182)),
  SSP445 = list(developed = c(12.5, 0.000116965,0.673335486,0.197594553,4006.730116,12.60518, 0.000116965,28.19482,12.5,551.472768,
                              0.673335486, 1, 0.045804624, 0.018971431, 0.197594553, 0.155166467, 163.8474042), 
                developing = c(12.50000, 0.0001170,0.64042,0.19228, 4106.35772, 13.28074, 0.0001170,30.76896,
                               12.50000, 166.06850,0.64042,1.00000,0.03498,0.01846,0.19228,0.15439,143.21182)),
  SSP460 = list(developed = c(12.5, 0.000116965,0.673335486,0.197594553,4006.730116,12.60518, 0.000116965,28.19482,12.5,551.472768,
                              0.673335486, 1, 0.045804624, 0.018971431, 0.197594553, 0.155166467, 163.8474042), 
                developing = c(12.50000, 0.0001170,0.64042,0.19228, 4106.35772, 13.28074, 0.0001170,30.76896,
                               12.50000, 166.06850,0.64042,1.00000,0.03498,0.01846,0.19228,0.15439,143.21182)),
  SSP560 = list(developed = c(12.50000, 0.0001170, 0.64042, 0.19228, 4106.35772, 13.28074, 0.0001170, 30.76896, 12.50000,
                              166.06850, 0.64042, 1.00000, 0.03498, 0.01846, 0.19228, 0.15439, 143.21182), 
                developing = c(12.50000, 0.0001170, 0.64042, 0.19228, 4106.35772, 13.28074, 0.0001170, 30.76896,
                               12.50000, 166.06850,  0.64042,  1.00000, 0.03498, 0.01846, 0.19228, 0.15439, 143.21182))
)
library(readxl)
library(dplyr)
library(writexl)

file_path <- "/.../LEs.xlsx"

sheets <- excel_sheets(file_path)

modified_data <- list()

for (sheet_name in sheets) {
  data <- read_excel(file_path, sheet = sheet_name)
  
  for (i in 1:17) {
    new_col_name <- as.character(i)
    data <- data %>%
      mutate(!!new_col_name := case_when(
        Development == "developed" ~ constants_mapLR[[sheet_name]][["developed"]][i] * LR_ener ,
        Development == "developing" ~ constants_mapLR[[sheet_name]][["developing"]][i] * LR_ener ,
        TRUE ~ LR_ener * 1 
      ))
  }
  
  modified_data[[sheet_name]] <- data
}

write_xlsx(modified_data, path = "/.../LEs1.xlsx")


#FRs part-------------------------------------------------------------------------------------
constants_mapWR <- list(
  SSP126 = list(developed = c(18.30000, 0.0001202, 0.74277, 0.00000, 10468.64432, 6.60041, 0.0001202,
                              16.61359, 18.30000, 380.92804,0.74277, 1.00000, 0.17922, 0.00000,
                              0.00000, 0.13129, 456.94527), 
                developing = c(18.30000,0.0001202,  0.74277, 0.00000, 10468.64432, 6.60041,  0.0001202, 16.61359,  18.30000, 
                               380.92804, 0.74277, 1.00000, 0.17922, 0.00000, 0.00000, 0.13129, 456.94527)), # 如果相同，只是重复数组
  SSP145 = list(developed = c(18.3,0.0001202,0.74277,0.00000,10468.64432,6.60041, 0.0001202,16.61359,
                              18.3, 380.92804,0.74277,1.00000,0.17922, 0.00000, 0.00000, 0.13129, 456.94527), 
                developing = c(18.3, 0.0001202, 0.74277, 0.0, 10468.64432, 6.60041, 0.0001202, 16.61359,
                               18.3,380.92804, 0.74277, 1.00000, 0.17922, 0.00000, 0.00000, 0.13129, 456.94527)), # 如果相同，只是重复数组
  SSP226 = list(developed = c(18.3,0.000120248,0.742772943,0,10468.64432, 6.60041, 0.000120248, 16.61359,18.3,
                              380.9280423,0.742772943, 1, 0.179217551, 0,0, 0.131292035, 456.9452691), 
                developing = c(18.30000, 0.0001201, 0.55187,0.00000, 7787.33488, 7.63111, 0.0001201,21.62929,
                               18.30000, 546.97545, 0.55187, 1.00000, 0.14348, 0.00000, 0.00000, 0.09584,338.72625)),
  SSP245 = list(developed = c(18.3,0.000120248,0.742772943, 0, 10468.64432, 6.60041, 0.000120248, 16.61359,
                              18.3,380.9280423,0.742772943,1, 0.179217551,0,  0, 0.131292035, 456.9452691), 
                developing = c(18.30000, 0.0001201,0.55187,0,7787.33488, 7.63111, 0.0001201,21.62929, 18.30000,
                               546.97545, 0.55187, 1.00000,0.14348,0.00000, 0.00000, 0.09584, 338.72625)),
  SSP260 = list(developed = c(18.3, 0.000120248, 0.742772943, 0, 10468.64432,6.60041, 0.000120248, 16.61359, 18.3,
                              380.9280423,0.742772943, 1,0.179217551, 0,0,0.131292035,456.9452691), 
                developing = c(18.30000, 0.0001201, 0.55187,0.00000,7787.33488, 7.63111,0.0001201, 21.62929,
                               18.30000,546.97545, 0.55187,1.00000, 0.14348, 0.00000, 0.00000, 0.09584,338.72625)),
  SSP445 = list(developed = c(18.3,0.000120248,0.742772943,0,10468.64432,6.60041,0.000120248,16.61359,18.3,380.9280423,0.742772943,
                              1,0.179217551,0,0, 0.131292035,456.9452691), 
                developing = c(18.300, 0.00012,0.900, 0.000,12694.131,5.754,0.00012,12.496,18.300,238.127,
                               0.900, 1.000,0.209, 0.000, 0.000,  0.161,  554.949)),
  SSP460 = list(developed = c(18.3,0.000120248,0.742772943,0,10468.64432, 6.60041, 0.000120248, 16.61359,18.3,
                              380.9280423, 0.742772943, 1, 0.179217551, 0,0, 0.131292035,456.9452691), 
                developing = c(18.300, 0.00012, 0.900,0.000, 12694.131, 5.754, 0.00012, 12.496, 18.300,
                               238.127,  0.900, 1.000,  0.209, 0.000, 0.000, 0.161, 554.949)),
  SSP560 = list(developed = c(18.30000, 0.0001201, 0.55187, 0.00000,7787.33488, 7.63111, 0.0001201, 21.62929,
                              18.30000, 546.97545, 0.55187, 1.00000, 0.14348, 0.00000, 0.00000, 0.09584,
                              338.72625), 
                developing = c(18.30000, 0.0001201, 0.55187, 0.00000, 7787.33488, 7.63111, 0.0001201, 21.62929,  18.30000,
                               546.97545, 0.55187, 1.00000, 0.14348,  0.00000, 0.00000, 0.09584, 338.72625))
)
library(readxl)
library(dplyr)
library(writexl)

file_path <- "/.../FRs.xlsx"
sheets <- excel_sheets(file_path)

modified_data <- list()

for (sheet_name in sheets) {
  data <- read_excel(file_path, sheet = sheet_name)
  
  for (i in 1:17) {
    new_col_name <- as.character(i)
    data <- data %>%
      mutate(!!new_col_name := case_when(
        Development == "developed" ~ constants_mapWR[[sheet_name]][["developed"]][i] * WR_ener ,
        Development == "developing" ~ constants_mapWR[[sheet_name]][["developing"]][i] * WR_ener ,
        TRUE ~ WR_ener * 1 
      ))
  }
  
  modified_data[[sheet_name]] <- data
}

write_xlsx(modified_data, path = "/.../FRs1.xlsx")



#population (job)----
library(readxl)
adultpop <- read_xlsx("/.../popjob.xlsx")

adultpop1 <- adultpop %>% group_by(SCENARIO, REGION) %>%
  summarise(
    x2025 = sum(`2025`),  
    x2030 = sum(`2030`),
    x2035 = sum(`2035`),
    x2040 = sum(`2040`),
    x2045 = sum(`2045`),
    x2050 = sum(`2050`),
    x2055 = sum(`2055`),
    x2060 = sum(`2060`)
  )

adultpop1$unit <- "million"

write.csv(adultpop1, "/.../popjob1.csv")

#low income population-----
lowpop <- read.csv("/.../lowincome.csv")
lowpop1 <- lowpop %>% group_by(SCENARIO, REGION) %>%
  summarise(
    x2025 = sum(`X2025`),  # 使用反引号来引用数字列名
    x2030 = sum(`X2030`),
    x2035 = sum(`X2035`),
    x2040 = sum(`X2040`),
    x2045 = sum(`X2045`),
    x2050 = sum(`X2050`),
    x2055 = sum(`X2055`),
    x2060 = sum(`X2060`)
  )


lowpop2 <- read.csv("/.../lowincome_country.csv")

lowpop2 <- lowpop2 %>% group_by(SCENARIO, X32region) %>%
  summarise(
    x2025 = sum(`x2025`),  
    x2030 = sum(`x2030`),
    x2035 = sum(`x2035`),
    x2040 = sum(`x2040`),
    x2045 = sum(`x2045`),
    x2050 = sum(`x2050`),
    x2055 = sum(`x2055`),
    x2060 = sum(`x2060`)
  )
write.csv(lowpop2, "/.../lowincome_32region.csv")


library(dplyr)
step3_result <- read_xlsx("/.../SDGstep3.xlsx")
step3_result_BR <- step3_result %>% group_by(region,Income_level,Development,scenario,Year) %>%
  summarise(
    sum_BR = sum(`sum(BR)`),
    BR_ener = sum(`BR_ener`),
    SDG1 = sum(`1（美元/人/年）`),
    SDG2 = sum(`2（新增就业岗位数量比例/年）`),
    SDG3 = sum(`3/...34`),
    SDG6 = sum(`6/...35`),
    SDG7_1 = sum(`7_1`),
    SDG7_2 = sum(`7_2`),
    SDG8 = sum(`8/...38`),
    SDG9 = sum(`9（美元/人/年）`),
    SDG10 = sum(`10/...40`),
    SDG11_1 = sum(`11_1`),
    SDG11_2 = sum(`11_2`),
    SDG12 = sum(`12/...43`),
    SDG13 = sum(`13/...44`),
    SDG14 = sum(`14/...45`),
    SDG15_1 = sum(`15_1`),
    SDG15_2 = sum(`15_2`),
    SDG15_3 = sum(`15_3`)
  )
write.csv(step3_result_BR,"/.../step3_result_BR.csv")
step3_result_limit <- filter(step3_result_BR, Year == 2025)
write.csv(step3_result_limit,"/.../step3_result_limit.csv")


bio_SDG_raw <- read_xlsx("/.../SDGstep3.xlsx")
bio_SDG_raw <- bio_SDG_raw %>%
  rename(
    X1.1 = `1（美元/人/年）`, 
    X2.1 = `2（新增就业岗位数量比例/年）`,
    X3.1 = `3/...34`,
    X6.1 = `6/...35`,
    X7_1 = `7_1`,
    X7_2 = `7_2`,
    X8.1 = `8/...38`,
    X9.1 = `9（美元/人/年）`,
    X10.1 = `10/...40`,
    X11_1 = `11_1`,
    X11_2 = `11_2`,
    X12.1 = `12/...43`,
    X13.1 = `13/...44`,
    X14.1 = `14/...45`,
    X15_1 = `15_1`,
    X15_2 = `15_2`,
    X15_3 = `15_3`
  )
bio_SDG_raw <- bio_SDG_raw %>%
  mutate(S1 = pmin(X1.1 / 11614.90 * 100, 100)) %>%
  mutate(S2 = pmin(X2.1 / 0.001420 * 100, 100)) %>%
  mutate(S3 = pmin(X3.1 / 5.57 * 100, 100)) %>%
  mutate(S6 = pmin(X6.1 / 0.1746 * 100, 100)) %>%
  mutate(S7_1 = pmin(X7_1 / 0.2402 * 100, 100)) %>%
  mutate(S7_2 = pmin(X7_2 / 55.12 * 100, 100)) %>%
  mutate(S7 = (S7_1 + S7_2)/2 ) %>%
  mutate(S8 = pmin(X8.1 / 0.001420 * 100, 100)) %>%
  mutate(S9 = pmin(X9.1 / 128.25 * 100, 100)) %>%
  mutate(S10 = pmin(X10.1 / 11614.90 * 100, 100)) %>%
  mutate(S11_1 = pmin(X11_1 / 2989.89 * 100, 100)) %>%
  mutate(S11_2 = pmin(X11_2 / 5.57 * 100, 100)) %>%
  mutate(S11 = (S11_1 + S11_2)/2 ) %>%
  mutate(S12 = pmin(X12.1 / 7.13 * 100, 100)) %>%
  mutate(S13 = pmin(X13.1 / 1.13 * 100, 100)) %>%
  mutate(S14 = pmin(X14.1 / 0.04187 * 100, 100)) %>%
  mutate(S15_1 = pmin(X15_1 / 0.1746 * 100, 100)) %>%
  mutate(S15_2 = pmin(X15_2 / 1.14 * 100, 100)) %>%
  mutate(S15_3 = pmin(X15_3 / 2977.62 * 100, 100)) %>%
  mutate(S15 = (S15_1 + S15_2 + S15_3)/3) %>%
  mutate(s1 = pmin(S1 /7/17, 100/7/17)) %>%
  mutate(s2 = pmin(S2 /8/17, 100/8/17)) %>%
  mutate(s3 = pmin(S3 /13/17, 100/13/17)) %>%
  mutate(s6 = pmin(S6 /8/17, 100/8/17)) %>%
  mutate(s7 = pmin(S7 /5/17, 100/5/17)) %>%
  mutate(s8 = pmin(S8 / 12/17, 100/12/17)) %>%
  mutate(s9 = pmin(S9 / 8/17, 100/8/17)) %>%
  mutate(s10 = pmin(S10 /10/17, 100/10/17)) %>%
  mutate(s11 = pmin(S11 /10/17, 100/10/17)) %>%
  mutate(s12 = pmin(S12 / 10/17, 100/10/17)) %>%
  mutate(s13 = pmin(S13 /5/17, 100/5/17)) %>%
  mutate(s14 = pmin(S14 /10/17, 100/10/17)) %>%
  mutate(s15 = pmin(S15 /12/17, 100/12/17)) %>%
  mutate(SDG = s1+ s2+ s3+ s6+ s7+ s8+ s9+ s10+ s11+ s12+ s13+ s14+ s15) %>%
  mutate(SDG_100 = (S1+ S2+ S3+ S6+ S7+ S8+ S9+ S10+ S11+ S12+ S13+ S14+ S15)/13)
write.csv(bio_SDG_raw, "/.../bio_SDG_raw_total.csv")



bio_SDG_sum <- bio_SDG_raw %>% group_by(region,	scenario,	Year) %>%
  summarise(
    SDG1 = sum(`X1.1`),  
    SDG2 = sum(`X2.1`),
    SDG3 = sum(`X3.1`),
    SDG6 = sum(`X6.1`),
    SDG7_1 = sum(`X7_1`),
    SDG7_2 = sum(`X7_2`),
    SDG8 = sum(`X8.1`),
    SDG9 = sum(`X9.1`),
    SDG10 = sum(`X10.1`),
    SDG11_1 = sum(`X11_1`),
    SDG11_2 = sum(`X11_2`),
    SDG12 = sum(`X12.1`),
    SDG13 = sum(`X13.1`),
    SDG14 = sum(`X14.1`),
    SDG15_1 = sum(`X15_1`),
    SDG15_2 = sum(`X15_2`),
    SDG15_3 = sum(`X15_3`),
    
  )

library(dplyr)

bio_SDG_sum <- bio_SDG_sum %>%
  mutate(S1 = pmin(SDG1 / 11614.9 * 100, 100)) %>%
  mutate(S2 = pmin(SDG2 / 0.01420 * 100, 100)) %>%
  mutate(S3 = pmin(SDG3 / 5.57 * 100, 100)) %>%
  mutate(S6 = pmin(SDG6 / 0.1746 * 100, 100)) %>%
  mutate(S7_1 = pmin(SDG7_1 / 0.2402 * 100, 100)) %>%
  mutate(S7_2 = pmin(SDG7_2 / 55.12 * 100, 100)) %>%
  mutate(S7 = (S7_1 + S7_2) /2) %>%
  mutate(S8 = pmin(SDG8 / 0.01420 * 100, 100)) %>%
  mutate(S9 = pmin(SDG9 / 128.25 * 100, 100)) %>%
  mutate(S10 = pmin(SDG10 / 11614.9 * 100, 100)) %>%
  mutate(S11_1 = pmin(SDG11_1 / 2989.89 * 100, 100)) %>%
  mutate(S11_2 = pmin(SDG11_2 / 5.57 * 100, 100)) %>%
  mutate(S11 = (S11_1 + S11_2)/2 ) %>%
  mutate(S12 = pmin(SDG12 / 7.13 * 100, 100)) %>%
  mutate(S13 = pmin(SDG13 / 1.13 * 100, 100)) %>%
  mutate(S14 = pmin(SDG14 / 0.04187 * 100, 100)) %>%
  mutate(S15_1 = pmin(SDG15_1 / 0.1746 * 100, 100)) %>%
  mutate(S15_2 = pmin(SDG15_2 / 1.14 * 100, 100)) %>%
  mutate(S15_3 = pmin(SDG15_3 / 2977.62 * 100, 100)) %>%
  mutate(S15 = (S15_1 + S15_2 + S15_3) / 3) %>%
  mutate(s1 = pmin(S1 /7/17, 100/7/17)) %>%
  mutate(s2 = pmin(S2 /8/17, 100/8/17)) %>%
  mutate(s3 = pmin(S3 /13/17, 100/13/17)) %>%
  mutate(s6 = pmin(S6 /8/17, 100/8/17)) %>%
  mutate(s7 = pmin(S7 /5/17, 100/5/17)) %>%
  mutate(s8 = pmin(S8 / 12/17, 100/12/17)) %>%
  mutate(s9 = pmin(S9 / 8/17, 100/8/17)) %>%
  mutate(s10 = pmin(S10 /10/17, 100/10/17)) %>%
  mutate(s11 = pmin(S11 /10/17, 100/10/17)) %>%
  mutate(s12 = pmin(S12 / 10/17, 100/10/17)) %>%
  mutate(s13 = pmin(S13 /5/17, 100/5/17)) %>%
  mutate(s14 = pmin(S14 /10/17, 100/10/17)) %>%
  mutate(s15 = pmin(S15 /12/17, 100/12/17)) %>%
  mutate(SDG = s1+ s2+ s3+ s6+ s7+ s8+ s9+ s10+ s11+ s12+ s13+ s14+ s15) %>%
  mutate(SDG_100 = (S1+ S2+ S3+ S6+ S7+ S8+ S9+ S10+ S11+ S12+ S13+ S14+ S15)/13)

write.csv(bio_SDG_sum, "/.../bio_SDG_sum_toal.csv")
