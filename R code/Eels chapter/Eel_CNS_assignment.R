library(mvtnorm)
library(dplyr)

#Load in R script with Kiljunen function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Kiljunen_correction.R")

#Create function to calculate mean, variance and covariance matrix for the specified location
locationTriProbability <- function(location, subset_training, test_data_isotopes) {
  
  ## Extract data from only one location out of the training subset
  carbon_for_location <- subset_training$d13Cc[subset_training$Location==location]
  nitrogen_for_location <- subset_training$d15N[subset_training$Location==location]
  sulfur_for_location <- subset_training$d34S[subset_training$Location==location]
  
  #Create matrix containing carbon, nitrogen and sulfur data
  data_for_location <- cbind(carbon_for_location, nitrogen_for_location, sulfur_for_location)
  
  #calculate means and create matrix
  carbon_mu <- mean(carbon_for_location)
  nitrogen_mu <- mean(nitrogen_for_location)
  sulfur_mu <- mean(sulfur_for_location)
  
  mu <- cbind(carbon_mu, nitrogen_mu, sulfur_mu)
  
  #calculate variances and create matrix
  carbon_var <- var(carbon_for_location)
  nitrogen_var <- var(nitrogen_for_location)
  sulfur_var <- var(sulfur_for_location)
  
  var <- cbind(carbon_var, nitrogen_var, sulfur_var)
  
  #calculate covariances for the three isotopes
  covariance_CN <- cov(carbon_for_location, nitrogen_for_location)
  covariance_CS <- cov(carbon_for_location, sulfur_for_location)
  covariance_NS <- cov(nitrogen_for_location, sulfur_for_location)
  
  ##Create covariance matrix
  #First create a list with values for the covariance matrix (going down columns)
  covariance_matrix <- c(carbon_var, covariance_CN, covariance_CS, covariance_CN, nitrogen_var, covariance_NS, covariance_CS, covariance_NS, sulfur_var)
  #Then convert the list into a 3x3 matrix
  dim(covariance_matrix) <- c(3,3)
  
  as.data.frame(dmvnorm(test_data_isotopes, mu, covariance_matrix))
  
}

#Calculate percentage from a particular origin assigned to any location
percentAssigned <- function(results, focus_location, location_assigned) {
  Total_assigned <- sum(with(results, True_location == focus_location & Most_likely_origin == location_assigned))
  Total_samples <- sum(with(results, True_location == focus_location))
  (Total_assigned/Total_samples)*100
}


##### MAIN SCRIPT STARTS HERE #####

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Eels")
data <- read.csv("Eels_CNS_with_reruns.csv", header=TRUE)

#Calculate corrected carbon values using Kiljunen function, only if CN > 3.4
data$d13Cc <- ifelse(data$CN > 3.4, kiljunenCalculation(data), data$d13C)

#Define list of all locations to be used for probability calculations
location_list <- c("Severn", "Parrett", "Vilaine", "Oria")
#Get number of locations
#no.locs <- length(location_list)

#Create empty data frame for each location
Severn_assignments <- NULL
Parrett_assignments <- NULL
Vilaine_assignments <- NULL
Oria_assignments <- NULL

######### REPEAT FROM HERE ############

repeats <- 1000
for(i in 1:repeats){ 
  
  #Split data into two subsets - 25% test data, 75% training data
  subset_test <- data %>% group_by(Location) %>% sample_frac(.25)
  subset_training <- anti_join(data, subset_test, by = "FishID")
  
  #join d13Cc, d15N and d34S into a separate table
  test_data_isotopes <- cbind(subset_test$d13Cc, subset_test$d15N, subset_test$d34S)
  
  ###Calculate probability for each location
  Location_probabilities <- as.data.frame(sapply(location_list, subset_training, test_data_isotopes, FUN = locationTriProbability))
  
  #Create table of probabilities as percentages
  Results_percent <- round(Location_probabilities*100, 2)
  names(Results_percent) <- location_list
  
  #Return column name with the highest percentage
  Most_likely_origin <- as.character(names(Results_percent)[apply(Results_percent, 1 , which.max)])
  
  #Create table of percentages, true locations and most likely origins
  Output <- cbind(Results_percent, True_location = as.character(subset_test$Location), Most_likely_origin)
  
  #Calculate percentage of Barents fish assigned to each location
  Severn_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Severn"), col.names = location_list)
  Parrett_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Parrett"), col.names = location_list)
  Vilaine_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Vilaine"), col.names = location_list)
  Oria_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Oria"), col.names = location_list)
  
  #Add probabilities as another row into the existing data frame
  Severn_assignments <- rbind(Severn_assignments, round(Severn_assignments_new, 2))
  Parrett_assignments <- rbind(Parrett_assignments, round(Parrett_assignments_new, 2))
  Vilaine_assignments <- rbind(Vilaine_assignments, round(Vilaine_assignments_new, 2))
  Oria_assignments <- rbind(Oria_assignments, round(Oria_assignments_new, 2))
  
}

############finish repeating here................................

#Calculate means of number of fish assigned to each location over all repeats
Severn_means <- as.data.frame(colMeans(Severn_assignments))
Parrett_means <- as.data.frame(colMeans(Parrett_assignments))
Vilaine_means <- as.data.frame(colMeans(Vilaine_assignments))
Oria_means <- as.data.frame(colMeans(Oria_assignments))

#Put means for all locations into one dataframe
Assignment_means <- as.data.frame(cbind(Severn_means, Parrett_means, Vilaine_means, Oria_means))
colnames(Assignment_means) <- {paste0(location_list, "_assignments")}

#Write all means to an excel spreadsheet
write.csv(Assignment_means, file = "Eel_Assignment_means_CNS_repeat1000_July22.csv")

#Create excel spreadsheet for all repeats of probabilities
#sapply(Assignment_list, FUN = write.csv, file = {paste0("Results", assignment_list, ".csv")})

write.csv(Severn_assignments, file = "Results_Severn_assignments_CNS_1000_July22.csv")
write.csv(Parrett_assignments, file = "Results_Parrett_assignments_CNS_1000_July22.csv")
write.csv(Vilaine_assignments, file = "Results_Vilaine_assignments_CNS_1000_July22.csv")
write.csv(Oria_assignments, file = "Results_Oria_assignments_CNS_1000_July22.csv")
