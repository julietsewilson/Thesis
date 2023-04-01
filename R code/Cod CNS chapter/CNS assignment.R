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

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish")
data <- read.csv("Master_CNS_cod_noYoungsIceland.csv", header=TRUE)

#Group together two locations into one group
#levels(data$Location)[levels(data$Location) == "Irish"] <- "Irish_Celtic"
#levels(data$Location)[levels(data$Location) == "Celtic"] <- "Irish_Celtic"

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

#Define list of all locations to be used for probability calculations
location_list <- c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Irish", "Celtic")

#Get number of locations
no.locs <- length(location_list)

#Create empty data frame for each location
Barents_assignments <- NULL
Norwegian_assignments <- NULL
Iceland_assignments <- NULL
Faroes_assignments <- NULL
North_assignments <- NULL
WScot_assignments <- NULL
Rockall_assignments <- NULL
Baltic_assignments <- NULL
Irish_assignments <- NULL
Celtic_assignments <- NULL
#Irish_Celtic_assignments <- NULL


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

#Calculate percentage of fish assigned to each location
Barents_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Barents"), col.names = location_list)
Norwegian_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Norwegian"), col.names = location_list)
Iceland_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Iceland"), col.names = location_list)
Faroes_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Faroes"), col.names = location_list)
North_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "North"), col.names = location_list)
WScot_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "West Scotland"), col.names = location_list)
Rockall_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Rockall"), col.names = location_list)
Baltic_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Baltic"), col.names = location_list)
Irish_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Irish"), col.names = location_list)
Celtic_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Celtic"), col.names = location_list)
#Irish_Celtic_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Output, focus_location = "Irish_Celtic"), col.names = location_list)

#Add probabilities as another row into the existing data frame
Barents_assignments <- rbind(Barents_assignments, round(Barents_assignments_new, 2))
Norwegian_assignments <- rbind(Norwegian_assignments, round(Norwegian_assignments_new, 2))
Iceland_assignments <- rbind(Iceland_assignments, round(Iceland_assignments_new, 2))
Faroes_assignments <- rbind(Faroes_assignments, round(Faroes_assignments_new, 2))
North_assignments <- rbind(North_assignments, round(North_assignments_new, 2))
WScot_assignments <- rbind(WScot_assignments, round(WScot_assignments_new, 2))
Rockall_assignments <- rbind(Rockall_assignments, round(Rockall_assignments_new, 2))
Baltic_assignments <- rbind(Baltic_assignments, round(Baltic_assignments_new, 2))
Irish_assignments <- rbind(Irish_assignments, round(Irish_assignments_new, 2))
Celtic_assignments <- rbind(Celtic_assignments, round(Celtic_assignments_new, 2))
#Irish_Celtic_assignments <- rbind(Irish_Celtic_assignments, round(Irish_Celtic_assignments_new, 2))

}

############finish repeating here................................

#Calculate means of number of fish assigned to each location over all repeats
Barents_means <- as.data.frame(colMeans(Barents_assignments))
Norwegian_means <- as.data.frame(colMeans(Norwegian_assignments))
Iceland_means <- as.data.frame(colMeans(Iceland_assignments))
Faroes_means <- as.data.frame(colMeans(Faroes_assignments))
North_means <- as.data.frame(colMeans(North_assignments))
WScot_means <- as.data.frame(colMeans(WScot_assignments))
Rockall_means <- as.data.frame(colMeans(Rockall_assignments))
Baltic_means <- as.data.frame(colMeans(Baltic_assignments))
Irish_means <- as.data.frame(colMeans(Irish_assignments))
Celtic_means <- as.data.frame(colMeans(Celtic_assignments))
#Irish_Celtic_means <- as.data.frame(colMeans(Irish_Celtic_assignments))

#Put means for all locations into one dataframe
Assignment_means <- as.data.frame(cbind(Barents_means,
                                        Norwegian_means,
                                        Iceland_means, 
                                        Faroes_means, 
                                        North_means, 
                                        WScot_means, 
                                        Rockall_means, 
                                        Baltic_means, 
                                        Irish_means, 
                                        Celtic_means))

colnames(Assignment_means) <- {paste0(location_list, "_assignments")}

#Write all means to an excel spreadsheet
write.csv(Assignment_means, file = "Assignment_means_CNS_repeat1000_July22_noYoungsIceland.csv")

#Create excel spreadsheet for all repeats of probabilities
write.csv(Barents_assignments, file = "Results_Barents_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Norwegian_assignments, file = "Results_Norwegian_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Iceland_assignments, file = "Results_Iceland_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Faroes_assignments, file = "Results_Faroes_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(North_assignments, file = "Results_North_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(WScot_assignments, file = "Results_WScot_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Rockall_assignments, file = "Results_Rockall_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Baltic_assignments, file = "Results_Baltic_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Irish_assignments, file = "Results_Irish_assignments_CNS_1000_July22_noYoungsIceland.csv")
write.csv(Celtic_assignments, file = "Results_Celtic_assignments_CNS_1000_July22_noYoungsIceland.csv")
#write.csv(Irish_Celtic_assignments, file = "Results_Irish_Celtic_assignments_CNS_1000_July22_noYoungsIceland.csv")
