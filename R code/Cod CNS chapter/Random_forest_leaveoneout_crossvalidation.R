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

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish")
data <- read.csv("Master_CNS_cod_noYoungsIceland.csv", header=TRUE)

#Calculate corrected carbon values using Kiljunen function, only if CN > 3.4
data$d13Cc <- ifelse(data$CN > 3.4, kiljunenCalculation(data), data$d13C)

#Take out Cornwall data
data <- subset(data, Location != "Cornwall")

#Drop unused levels
data <- droplevels(data)

##Apply Suess correction for Youngs samples
#Calculate number of years between reference data (2018) and unknown sample collection
data$Years_difference <- 2018 - (data$Year_samples_collected)

#Add correction of -0.022 per mille per year for samples before 2018
data$d13Cc <- data$d13Cc + data$Years_difference*(-0.022)

#Select only columns with variables to use for classification, plus FishID for sub-sampling
rf_data <- select(data, FishID, Location, d13Cc, d15N, d34S)

# Remove FishID column
rf_data <- select(rf_data, -FishID)

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
                                importance = TRUE,
                                sampsize = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
  )
  
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
Assignments$True_location <- factor(Assignments$True_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Irish", "Celtic"))

# For merged locations - Reorder Assigned locations to be from north to south
Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Irish", "Celtic"))

######

# Create table of number of samples assigned to each location
Results_all <- table(Assigned_location = Assignments$Assigned_location, True_location = Assignments$True_location)

# Create excel spreadsheet for assignment results
write.csv(Results_all, file = "SIA_random_forest_leaveoneout_sampsize4_ntree2000_July22_v2.csv")
