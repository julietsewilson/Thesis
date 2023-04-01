library(dplyr)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(extrafont)

#Function to calculate percentage from a particular origin assigned to any location
percentAssigned <- function(results, focus_location, location_assigned) {
  Number_assigned <- results %>% filter(True_location == focus_location, pred_test == location_assigned) %>% pull(Freq)
  Total_samples <- results %>% filter(True_location == focus_location) %>% pull(Freq) %>% sum
  (Number_assigned/Total_samples)*100
}

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Eels")
data <- read.csv("Fatty acid data.csv", header=TRUE)

### Optional - merge Severn and Parrett
# First change location to character instead of factor (to avoid error when have one less factor)
data$Location <- as.character(data$Location)

#Merge Severn and Parrett
data$Location[data$Location == "Severn" | data$Location == "Parrett"] <- "UK"

# Then change location back to factor
data$Location <- as.factor(data$Location)

#Select only columns with variables to use for classification, plus FishID for sub-sampling
rf_data <- dplyr::select(data, -Sample, -standard)

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
                                mtry = 9,
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
write.csv(Results_all, file = "Eels_FA_random_forest_leaveoneout_Severn&Parrett_merged.csv")



##### Using all samples

# Calculate random forest with all data (not leave-one-out) for importance plot etc.
random_forest <- randomForest(Location ~ ., 
                              data = rf_data, 
                              ntree = 2000, 
                              mtry = 9,
                              importance = TRUE)


# Calculate importance of variables and then plot mean decrease in accuracy and mean deacrease Gini
importance(random_forest)
importance1 <- varImpPlot(random_forest) 

### To plot in ggplot2 have to convert varImpPlot data into dataframe
#Create dataframe from importance data
importance1 <- as.data.frame(importance1)
#Convert row names to column
importance1$FA <- rownames(importance1)
#Remove row names
rownames(importance1) <- NULL  

# Create excel spreadsheet for assignment results
write.csv(importance1, file = "Fatty_acid_random_forest_importance.csv")

#Bring in importance data
importance1 <- read.csv("Fatty_acid_random_forest_importance.csv", header = TRUE, fill = TRUE)

#Change names of fatty acids
importance1$FA <- recode_factor(importance1$FA, glycerol = "Glycerol",
                                                C14 = "14:0",
                                                C15 = "15:0",
                                                mono_C16_1 = "16:1 (1)",
                                                mono_C16_2 = "16:1 (2)",
                                                C16 = "16:0",
                                                mono_C17_1 = "17:1 (1)",
                                                mono_C17_2 = "17:1 (2)",
                                                C17 = "17:0",
                                                mono_C18_1 = "18:1 (1)",
                                                mono_C18_2 = "18:1 (2)",
                                                C18 = "18:0",
                                                multiply_unsaturated_C15 = "PUFA C15",
                                                multiply_unsaturated_1 = "PUFA (1)",
                                                mono_C20 = "20:1",
                                                C20 = "20:0",
                                                multiply_unsaturated_2 = "PUFA (2)",
                                                multiply_unsaturated_3 = "PUFA (3)",
                                                cholesterol = "Cholesterol")

## Now plot in ggplot
cairo_ps("Fatty_acid_random_forest_importance_plot_Sept22.eps", height = 7, width = 9)

ggplot(importance1, aes(x = MeanDecreaseAccuracy, y = reorder(FA, MeanDecreaseAccuracy))) + 
  geom_point(colour = "#440154FF", size = 3.5) +
  geom_segment(aes(x = 0, xend = (MeanDecreaseAccuracy - 0.5), y = FA, yend = FA), colour = "black", lwd = 0.5, linetype  = "dashed") +
  labs(x = "Mean decrease accuracy", title = "Fatty acids") +
  theme_bw() +
  theme(text=element_text(family = "Arial"),
        plot.title = element_text("Arial", face = "bold", size = 20),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        panel.border = element_rect(colour = "white", size = 1, fill = NA),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(vjust = -1),
        plot.margin = unit(c(0.5,0,0.5,0), "cm"))

dev.off()
