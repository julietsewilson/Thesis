library(mvtnorm)
library(dplyr)

#Load in R script with Kiljunen function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Kiljunen_correction.R")

#Load in script for bivariate probability calculation function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Bivariate_probability_calculation.R")


# Function to calculate probability for specific location
locationProbability <- function(location, data, test_samples) {
  
  #assigning data for a specific location to a variable
  carbon_for_location <- data$d13Cc[data$Location==location] 
  nitrogen_for_location <- data$d15N[data$Location==location]
  
  #calculate means and create matrix
  carbon_mu <- mean(carbon_for_location)
  nitrogen_mu <- mean(nitrogen_for_location)
  
  mu <- cbind(carbon_mu, nitrogen_mu)
  
  #calculate variances and create matrix
  var_carbon <- var(carbon_for_location)
  var_nitrogen <- var(nitrogen_for_location)
  
  var <- cbind(var_carbon, var_nitrogen)
  
  #calculate covariance of d13Cc and d15N
  covariance <- cov(carbon_for_location, nitrogen_for_location)
  
  #calculate correlation from covariance and variances
  r <- covariance/sqrt(var_carbon*var_nitrogen)
  
  #Run assignment on unknown samples
  calulateProbability(test_samples, mu, var, r)
  
}

# Function to calculate total number of test samples assigned to each location, for a specific true location
totalAssigned <- function(results, true_location) {
  results %>% filter(Actual_location == true_location) %>% 
    group_by(Most_likely_origin) %>% 
    dplyr::summarise(count = n())
}


####Main script starts here
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish")
data <- read.csv("Master_CN_hake.csv", header=TRUE)

#Calculate corrected carbon values using Kiljunen function, only if CN > 3.4
data$d13Cc <- ifelse(data$CN > 3.4, kiljunenCalculation(data), data$d13C)

#Select only 2018 samples as reference samples
reference_data <- subset(data, Year_samples_collected == "2018")

#Select only old data as test samples
unknown <- subset(data, Year_samples_collected != "2018")

## Apply correction for suess effect to unknown samples
#Convert years from factors to numbers
unknown$Year_samples_collected <- as.numeric(as.character(unknown$Year_samples_collected))

#Calculate number of years between reference data (2018) and unknown sample collection
unknown$Years_difference <- 2018 - (unknown$Year_samples_collected)

#Add correction of -0.022 per mille per year for samples before 2018
unknown$C13_suess_corr <- unknown$d13Cc + unknown$Years_difference*(-0.022)

#Create a matrix with corrected carbon and nitrogen data for unknown samples
unknown_isotopes <- cbind(unknown$C13_suess_corr, unknown$d15N)

#Calculate probability for each location
North <- locationProbability("North", reference_data, unknown_isotopes)
WScot <- locationProbability("West Scotland", reference_data, unknown_isotopes)
Celtic <- locationProbability("Celtic", reference_data, unknown_isotopes)
BayBiscay <- locationProbability("Bay of Biscay", reference_data, unknown_isotopes)
Mediterranean <- locationProbability("Mediterranean", reference_data, unknown_isotopes)

#Write table of probabilities for all locations
Results <- as.data.frame(cbind(North, WScot, Celtic, BayBiscay, Mediterranean))
Output <- Results*100

#Pick out most likely origin and its probability for each sample
Most_likely_origin <- as.character(names(Results)[apply(Results, 1 , which.max)])
Probability_likely_origin <- apply(Results, 1, max)*100

#Round output to 2 decimal places and add columns for ID number most likely origin and highest probability
Output <- cbind(FishID = unknown$FishID,
                round(Output, 2), 
                Most_likely_origin,
                Probability_likely_origin = round(Probability_likely_origin, 2),
                Actual_location = unknown$Location)

# For each true location, calculate total number of test samples assigned to each of the possible locations
Total_assigned_Cornwall <- totalAssigned(Output, "Cornwall")
Total_assigned_Celtic <- totalAssigned(Output, "Celtic")
Total_assigned_Irish <- totalAssigned(Output, "Irish")

#Write output to an excel spreadsheet
write.csv(Output, file = "Results_oldsamples_hake_assignment_withSuess_Aug22.csv")
