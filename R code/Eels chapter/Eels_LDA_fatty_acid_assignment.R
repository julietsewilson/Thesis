library(dplyr)
library(caret)
library(ggplot2)
library(MASS)
library(mda)

#Function to calculate percentage from a particular origin assigned to any location
percentAssigned <- function(results, focus_location, location_assigned) {
  Number_assigned <- results %>% filter(True_location == focus_location, Assigned_location == location_assigned) %>% pull(Freq)
  Total_samples <- results %>% filter(True_location == focus_location) %>% pull(Freq) %>% sum
  (Number_assigned/Total_samples)*100
}


setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Eels")
data <- read.csv("Fatty acid data.csv", header=TRUE)

#Select only columns with variables to use for classification, plus FishID for sub-sampling
##dplyr::select tells R to use the dplyr package since it's masked by the MASS package
DA_data <- dplyr::select(data, -standard)

#Define list of all locations to be used for probability calculations
location_list <- c("Severn", "Parrett", "Vilaine", "Oria")

#Create empty data frame for each location
Severn_assignments <- NULL
Parrett_assignments <- NULL
Vilaine_assignments <- NULL
Oria_assignments <- NULL


####### Repeat from here

repeats <- 1000
for(i in 1:repeats){
  
  ##Split data into two subsets
  subset_test <- DA_data %>% group_by(Location) %>% sample_frac(.25)
  subset_training <- anti_join(DA_data, subset_test, by = "Sample")
  
  #Remove FishID column since is not a variable for classification
  ##dplyr::select tells R to use the dplyr package since it's masked by the MASS package
  test <- dplyr::select(subset_test, -Sample)
  training <- dplyr::select(subset_training, -Sample)
  
  # Fit the models
  ## linear discriminant analysis
  lda_model <- lda(Location~., data = training)
  
  #Name Location column in test data as True_location
  True_location <- test$Location
  
  #Or predict test data and then make data frame with test sample isotope values and predicted class
  lda_predictions <- lda_model %>% predict(test)
  
  Results <- as.data.frame(table(True_location, Assigned_location = lda_predictions$class))
  
  # Model accuracy
  #mean(lda_predictions$Assigned_location == True_location)
  
  #Calculate number of fish assigned to each location as a percentage of total from that area
  Severn_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Results, focus_location = "Severn"), col.names = location_list)
  Parrett_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Results, focus_location = "Parrett"), col.names = location_list)
  Vilaine_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Results, focus_location = "Vilaine"), col.names = location_list)
  Oria_assignments_new <- as.data.frame(lapply(location_list, FUN = percentAssigned, results = Results, focus_location = "Oria"), col.names = location_list)
  
  #Add probabilities as another row into the existing data frame
  Severn_assignments <- rbind(Severn_assignments, round(Severn_assignments_new, 2))
  Parrett_assignments <- rbind(Parrett_assignments, round(Parrett_assignments_new, 2))
  Vilaine_assignments <- rbind(Vilaine_assignments, round(Vilaine_assignments_new, 2))
  Oria_assignments <- rbind(Oria_assignments, round(Oria_assignments_new, 2))
  
}

####### Finish repeating here

#Calculate means of number of fish assigned to each location over all repeats
Severn_means <- as.data.frame(colMeans(Severn_assignments))
Parrett_means <- as.data.frame(colMeans(Parrett_assignments))
Vilaine_means <- as.data.frame(colMeans(Vilaine_assignments))
Oria_means <- as.data.frame(colMeans(Oria_assignments))

#Put means for all locations into one dataframe
Assignment_means <- as.data.frame(cbind(Severn_means,
                                        Parrett_means,
                                        Vilaine_means, 
                                        Oria_means))

colnames(Assignment_means) <- {paste0(location_list, "_assignments")}

#Write all means to an excel spreadsheet
write.csv(Assignment_means, file = "LDA_Assignment_means_eels_FA_repeat1000_July22.csv")

#Create excel spreadsheet for all repeats of probabilities
write.csv(Severn_assignments, file = "LDA_Results_Severn_eels_assignments_FA_1000_July22.csv")
write.csv(Parrett_assignments, file = "LDA_Results_Parrett_eels_assignments_FA_1000_July22.csv")
write.csv(Vilaine_assignments, file = "LDA_Results_Vilaine_eels_assignments_FA_1000_July22.csv")
write.csv(Oria_assignments, file = "LDA_Results_Oria_eels_assignments_FA_1000_July22.csv")
