library(dplyr)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(extrafont)

#Load in R script with Kiljunen function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Kiljunen_correction.R")

#Function to calculate percentage from a particular origin assigned to any location
percentAssigned <- function(results, focus_location, location_assigned) {
  Number_assigned <- results %>% filter(True_location == focus_location, pred_test == location_assigned) %>% pull(Freq)
  Total_samples <- results %>% filter(True_location == focus_location) %>% pull(Freq) %>% sum
  (Number_assigned/Total_samples)*100
}

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Eels")
data <- read.csv("Eels_CNS_with_reruns.csv", header=TRUE)

#Calculate corrected carbon values using Kiljunen function, only if CN > 3.4
data$d13Cc <- ifelse(data$CN > 3.4, kiljunenCalculation(data), data$d13C)

### Optional - merge Severn and Parrett
# First change location to character instead of factor (to avoid error when have one less factor)
data$Location <- as.character(data$Location)

#Merge Severn and Parrett
data$Location[data$Location == "Severn" | data$Location == "Parrett"] <- "UK"

# Then change location back to factor
data$Location <- as.factor(data$Location)

#Select only columns with variables to use for classification, plus FishID for sub-sampling
rf_data <- dplyr::select(data, FishID, Location, d13Cc, d15N, d34S)

# Remove FishID column
rf_data <- dplyr::select(rf_data, -FishID)

#### Loop for leave-one-out cross validation random forest ####
samples <- nrow(rf_data)

# Create empty dataframe for assignments
Assignments <- NULL

for(i in 1:samples){ 
  
  # Take out one sample as a test sample, and put remaining samples into training subset
  test_sample <- rf_data[i,]
  subset_training <- rf_data[-i,]
  
  # Create a Random Forest model with default parameters
  random_forest <- randomForest(Location ~ ., 
                                data = subset_training, 
                                ntree = 2000, 
                                mtry = 2,
                                importance = TRUE)
  
  # Predicting on Validation set
  pred_test <- predict(random_forest, test_sample, type = "class")
  
  # See if predicted location matches true location in original test data
  Result <- pred_test == test_sample$Location
  
  # Add columns to Result for true location and predicted location
  Result <- cbind(Result, True_location = as.character(test_sample$Location), Assigned_location = as.character(pred_test))
  
  # Add result as a new row in assignments table (adding another row with each loop)
  Assignments <- as.data.frame(rbind(Assignments, Result))
  
}

# For merged locations - Reorder True locations to be from north to south
Assignments$True_location <- factor(Assignments$True_location, levels = c("UK", "Vilaine", "Oria"))

# For merged locations - Reorder Assigned locations to be from north to south
Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("UK", "Vilaine", "Oria"))

######

# Create table of number of samples assigned to each location
Results_all <- table(Assigned_location = Assignments$Assigned_location, True_location = Assignments$True_location)

# Create excel spreadsheet for assignment results
write.csv(Results_all, file = "SIA_eels_random_forest_leaveoneout_Severn&Parrett_merged.csv")
