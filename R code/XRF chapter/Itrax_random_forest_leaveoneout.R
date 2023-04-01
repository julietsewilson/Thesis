library(dplyr)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(extrafont)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
data <- read.csv("Itrax final data from R with selected data - Norwegian swapped.csv", header = TRUE, fill = TRUE)

# Select rows where validity is 1, remove rows where validity is zero
itrax_data <- filter(data, validity == 1)

# Select only rows where MSE is below maximum accepted value ####################
itrax_data <- filter(itrax_data, MSE < 5)

# List selected elements to use
Selected_elements <- c("Cl",
                       "K",
                       "S",
                       "Ca",
                       "Sc",
                       "Cr",
                       "Mn",
                       "Fe",
                       "Ni",
                       "Cu",
                       "Zn",
                       "As",
                       "Se",
                       "Br",
                       "Sr",
                       "Cd",
                       "I",
                       "Cs",
                       "Pb")

# Select columns with only elements to use (realistic elements and those not co-varying)
itrax_data_selected_elements <- itrax_data %>%
  select("sample_ID", Selected_elements)

# Calculate mean values of elements for each sample 
sample_means <- itrax_data_selected_elements %>% group_by(sample_ID) %>% summarise_all(mean, na.rm = TRUE)

# Get location info from original data
location_data <- select(itrax_data, sample_ID, location)
# Remove duplicates from basin data
location_data <- distinct(location_data)

# Join location data onto sample means data
sample_means <- merge(sample_means, location_data, by = "sample_ID")

######### OPTIONAL #########

### Merge Rockall and West Scotland samples into one group + remove Irish Sea

# First change location to character instead of factor (to avoid error when have one less factor)
sample_means$location <- as.character(sample_means$location)

# Replace Rockall and West Scotland with WScot_Rockall in location column
#sample_means$location[sample_means$location == "Rockall" | sample_means$location == "Wscotland"] <- "WScot_Rockall"

#Remove both Rockall and Irish
#sample_means <- subset(sample_means, ! location == "Irish" & ! location == "Rockall")

#Remove Irish Sea samples or merge with Celtic
sample_means <- subset(sample_means, ! location == "Irish")
#sample_means$location[sample_means$location == "Irish" | sample_means$location == "Celtic"] <- "Irish_Celtic"

# Then change location back to factor
sample_means$location <- as.factor(sample_means$location)

##########

# Replace zeros with 0.0001 in element data to avoid Inf in PCA
sample_means[sample_means == 0] <- 0.0001

## Calculate log ratios of all elements for PCA
#Calculate ratios of elements in pairs
ratio_1 <- sample_means$Cl/sample_means$Sr
ratio_2 <- sample_means$Br/sample_means$Cd
ratio_3 <- sample_means$Cu/sample_means$K
ratio_4 <- sample_means$Se/sample_means$Ca
ratio_5 <- sample_means$Mn/sample_means$I
ratio_6 <- sample_means$Zn/sample_means$Fe
ratio_7 <- sample_means$As/sample_means$Pb
ratio_8 <- sample_means$Cr/sample_means$Sc
ratio_9 <- sample_means$Cs/sample_means$S

# Calculate log of all element ratios (ln ratio)
log_ratio_1 <- log(ratio_1)
log_ratio_2 <- log(ratio_2)
log_ratio_3 <- log(ratio_3)
log_ratio_4 <- log(ratio_4)
log_ratio_5 <- log(ratio_5)
log_ratio_6 <- log(ratio_6)
log_ratio_7 <- log(ratio_7)
log_ratio_8 <- log(ratio_8)
log_ratio_9 <- log(ratio_9)

# Create dataframe with log ratios and location column
log_ratio_data <- as.data.frame(cbind(log_ratio_1,
                                      log_ratio_2,
                                      log_ratio_3,
                                      log_ratio_4,
                                      log_ratio_5,
                                      log_ratio_6,
                                      log_ratio_7,
                                      log_ratio_8,
                                      log_ratio_9), stringsAsFactors=FALSE)

log_ratio_data <- cbind(log_ratio_data, location = sample_means$location)


######## Random forest with raw data #############

# Remove Sample_ID column
sample_means <- select(sample_means, -sample_ID)

## Loop for leave-one-out cross validation random forest
samples <- nrow(sample_means)

# Create empty dataframe for assignments
Assignments <- NULL

for(i in 1:samples){ 
  
  # Take out one sample as a test sample, and put remaining samples into training subset
  test_sample <- sample_means[i,]
  subset_training <- sample_means[-i,]
  
  # Create a Random Forest model with default parameters
  random_forest <- randomForest(location ~ ., 
                                data = subset_training, 
                                ntree = 2000, 
                                mtry = 4,
                                importance = TRUE,
                                sampsize = c(4, 4, 4, 4, 4, 4, 4, 4, 4)
                                )
  
  # Predicting on Validation set
  pred_test <- predict(random_forest, test_sample, type = "class")
  
  # See if predicted location matches true location in original test data
  Result <- pred_test == test_sample$location
  
  # Add columns to Result for true location and predicted location
  Result <- cbind(Result, True_location = as.character(test_sample$location), Assigned_location = as.character(pred_test))
  
  # Add result as a new row in assignments table (adding another row with each loop)
  Assignments <- as.data.frame(rbind(Assignments, Result))
  
}

# Reorder True locations to be from north to south
#Assignments$True_location <- factor(Assignments$True_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Rockall", "Baltic", "Irish", "Celtic"))

# Reorder Assigned locations to be from north to south
#Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Rockall", "Baltic", "Irish", "Celtic"))

## Using only locations kept in assignments
# For merged locations - Reorder True locations to be from north to south
Assignments$True_location <- factor(Assignments$True_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Rockall", "Baltic", "Celtic"))

# For merged locations - Reorder Assigned locations to be from north to south
Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Rockall", "Baltic", "Celtic"))

######

# Create table of number of samples assigned to each location
Results_all <- table(Assigned_location = Assignments$Assigned_location, True_location = Assignments$True_location)

# Create excel spreadsheet for assignment results
write.csv(Results_all, file = "Itrax_raw_random_forest_leaveoneout_noIrish_sampsize4_v2_noU_noSm_noEu_noSn_withS.csv")


# Calculate random forest with all data (not leave-one-out) for importance plot etc.
random_forest <- randomForest(location ~ ., 
                              data = sample_means, 
                              ntree = 2000, 
                              mtry = 4,
                              importance = TRUE,
                              sampsize = c(4, 4, 4, 4, 4, 4, 4, 4, 4)
                              )

# Calculate importance of variables and then plot mean decrease in accuracy and mean deacrease Gini
importance(random_forest)
importance1 <- varImpPlot(random_forest) 

### To plot in ggplot2 have to convert varImpPlot data into dataframe
#Create dataframe from importance data
importance1 <- as.data.frame(importance1)
#Convert row names to column
importance1$Elements <- rownames(importance1)
#Remove row names
rownames(importance1) <- NULL  

# Create excel spreadsheet for importance
write.csv(importance1, file = "Itrax_raw_random_forest_importance_Sept22.csv")

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax/Assignments/Itrax/Raw")
importance1 <- read.csv("Itrax_raw_random_forest_importance_Sept22.csv", header = TRUE, fill = TRUE)

viridis(3)
## Now plot in ggplot
cairo_ps("Itrax_raw_importance_plot_Sept22.eps", height = 7, width = 9)

ggplot(importance1, aes(x = MeanDecreaseAccuracy, y = reorder(Elements, MeanDecreaseAccuracy))) + 
  geom_point(colour = "#440154FF", size = 3.5) +
  geom_segment(aes(x = 0, xend = (MeanDecreaseAccuracy - 0.5), y = Elements, yend = Elements), colour = "black", lwd = 0.5, linetype  = "dashed") +
  labs(x = "Mean decrease accuracy", y = "", title = "Itrax") +
  theme_bw() +
  theme(text=element_text(family = "Arial"),
        plot.title = element_text("Arial", face = "bold", size = 20),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        panel.border = element_rect(colour = "white", size = 1, fill = NA),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(vjust = -1),
        plot.margin = unit(c(0.5,0,0.5,0), "cm"))

dev.off()

######## Random forest with log ratio data #############

## Loop for leave-one-out cross validation random forest
samples <- nrow(log_ratio_data)

# Create empty dataframe for assignments
Assignments <- NULL

for(i in 1:samples){ 
  
  # Take out one sample as a test sample, and put remaining samples into training subset
  test_sample <- log_ratio_data[i,]
  subset_training <- log_ratio_data[-i,]
  
  # Create a Random Forest model with default parameters
  random_forest <- randomForest(location ~ ., 
                                data = subset_training, 
                                ntree = 2000, 
                                mtry = 3, 
                                importance = TRUE,
                                sampsize = c(6, 6, 6, 6, 6, 6, 6, 6)
                                )
  
  # Predicting on Validation set
  pred_test <- predict(random_forest, test_sample, type = "class")
  
  # See if predicted location matches true location in original test data
  Result <- pred_test == test_sample$location
  
  # Add columns to Result for true location and predicted location
  Result <- cbind(Result, True_location = as.character(test_sample$location), Assigned_location = as.character(pred_test))
  
  # Add result as a new row in assignments table (adding another row with each loop)
  Assignments <- as.data.frame(rbind(Assignments, Result))
  
}

# Reorder True locations to be from north to south
#Assignments$True_location <- factor(Assignments$True_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Rockall", "Baltic", "Irish", "Celtic"))

# Reorder Assigned locations to be from north to south
#Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Rockall", "Baltic", "Irish", "Celtic"))

###### OPTIONAL #######
# For merged locations - Reorder True locations to be from north to south
Assignments$True_location <- factor(Assignments$True_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Baltic", "Celtic"))

# For merged locations - Reorder Assigned locations to be from north to south
Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "Wscotland", "Baltic", "Celtic"))

######

# Create table of number of samples assigned to each location
Results_all <- table(Assigned_location = Assignments$Assigned_location, True_location = Assignments$True_location)

# Create excel spreadsheet for assignment results
write.csv(Results_all, file = "Itrax_log_ratios_random_forest_leaveoneout_noIrish_noRockall_sampsize6_v2_noSn_etc_withS_repeat.csv")


# Calculate random forest with all data (not leave-one-out) for importance plot etc.
random_forest <- randomForest(location ~ ., 
                              data = log_ratio_data, 
                              ntree = 2000, 
                              mtry = 3, 
                              importance = TRUE,
                              sampsize = c(6, 6, 6, 6, 6, 6, 6, 6)
                              )

# Calculate importance of variables and then plot mean decrease in accuracy and mean deacrease Gini
importance(random_forest)
importance2 <- varImpPlot(random_forest) 

### To plot in ggplot2 have to convert varImpPlot data into dataframe
#Create dataframe from importance data
importance2 <- as.data.frame(importance2)
#Convert row names to column
importance2$Elements <- rownames(importance2)
#Remove row names
rownames(importance2) <- NULL

# Create excel spreadsheet for assignment results
write.csv(importance2, file = "Itrax_log_ratios_random_forest_variable_importance.csv")


setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax/Assignments/Itrax/Log ratios")
importance2 <- read.csv("Itrax_log_ratios_random_forest_variable_importance.csv", header = TRUE, fill = TRUE)

## Now plot in ggplot
cairo_ps("Itrax_logratio_importance_plot_Sept22.eps", height = 7, width = 9)

ggplot(importance2, aes(x = MeanDecreaseAccuracy, y = reorder(Elements, MeanDecreaseAccuracy))) + 
  geom_point(colour = "#440154FF", size = 3.5) +
  geom_segment(aes(x = 0, xend = (MeanDecreaseAccuracy - 0.5), y = Elements, yend = Elements), colour = "black", lwd = 0.5, linetype  = "dashed") +
  labs(x = "Mean decrease accuracy", y = "", title = "Itrax") +
  theme_bw() +
  theme(text=element_text(family = "Arial"),
        plot.title = element_text("Arial", face = "bold", size = 20),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        panel.border = element_rect(colour = "white", size = 1, fill = NA),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(vjust = -1),
        plot.margin = unit(c(0.5,0,0.5,0), "cm"))

dev.off()
