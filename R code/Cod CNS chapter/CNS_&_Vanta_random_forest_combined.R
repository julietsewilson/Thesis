library(tidyverse)
library(dplyr)
library(randomForest)

#Load in R script with Kiljunen function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Kiljunen_correction.R")


setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
data <- read.csv("Vanta data all for analysis corrected.csv", header = TRUE, fill = TRUE)
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish")
CNS_data <- read.csv("CNS cod data for combining with Vanta.csv", header=TRUE)

#Remove samples where surface is too rough (same as Itrax)
unuseable <- c("200",
               "281",
               "306",
               "381",
               "406",
               "1048",
               "1052",
               "1136")

# Remove unuseable samples from distance data and itrax data
vanta_data_filtered <- subset(data, ! Sample_ID %in% unuseable)

# Select elements to use
vanta_data <- select(vanta_data_filtered, c(Sample_ID, 
                                            Region, 
                                            Si,
                                            S,
                                            K,
                                            Ca,
                                            Mn,
                                            Fe,
                                            Cu,
                                            Zn,
                                            As,
                                            Sr,
                                            Ba,
                                            La,
                                            U))


# Calculate average values for each sample - calculate before or after doing log ratio????
sample_means <- vanta_data %>% 
                select(-Region) %>% 
                group_by(Sample_ID) %>% 
                summarise_all(mean, na.rm = TRUE)

# Get location info from original data
location_data <- select(vanta_data, Sample_ID, Region)
# Remove duplicates from basin data
location_data <- distinct(location_data)

# Join location data onto sample means data and standard deviation data
sample_means <- merge(sample_means, location_data, by = "Sample_ID")  

### Remove Irish Sea samples
# First change location to character instead of factor (to avoid error when have one less factor)
sample_means$Region <- as.character(sample_means$Region)

#Remove Irish
sample_means <- subset(sample_means, ! Region == "Irish")
#sample_means <- subset(sample_means, ! Region == "Irish" & ! Region == "Rockall")

# Then change location back to factor
sample_means$Region <- as.factor(sample_means$Region)


#Calculate corrected carbon values using Kiljunen function, only if CN > 3.4
CNS_data$d13Cc <- ifelse(CNS_data$CN > 3.4, kiljunenCalculation(CNS_data), CNS_data$d13C)

##Apply Suess correction for Youngs samples
#Calculate number of years between reference data (2018) and unknown sample collection
CNS_data$Years_difference <- 2018 - (CNS_data$Year_samples_collected)

#Add correction of -0.022 per mille per year for samples before 2018
CNS_data$d13Cc <- CNS_data$d13Cc + CNS_data$Years_difference*(-0.022)

#Select columns needed
CNS_data <- select(CNS_data, c(FishID, Location, d13Cc, d15N, d34S)) 

# Rename column "Sample_ID" to "Fish_ID" to match CNS data
names(sample_means)[names(sample_means) == "Sample_ID"] <- "FishID"

# Combine CNS and Vanta data into one dataframe
data_all <- merge(CNS_data, sample_means, by = "FishID") 

#Drop unused levels
data_all <- droplevels(data_all)

#Select only columns with variables to use for classification, plus FishID for sub-sampling
rf_data <- select(data_all, -FishID, -Region)

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
                                mtry = 6,
                                importance = TRUE,
                                sampsize = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
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
Assignments$True_location <- factor(Assignments$True_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Celtic"))

# For merged locations - Reorder Assigned locations to be from north to south
Assignments$Assigned_location <- factor(Assignments$Assigned_location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Celtic"))

######

# Create table of number of samples assigned to each location
Results_all <- table(Assigned_location = Assignments$Assigned_location, True_location = Assignments$True_location)

# Create excel spreadsheet for assignment results
write.csv(Results_all, file = "SIA&Vanta_random_forest_leaveoneout_sampsize3_ntree2000_corrected.csv")


##### Calculate random forest with all data (not leave-one-out) for importance plot etc.
random_forest <- randomForest(Location ~ ., 
                              data = rf_data, 
                              ntree = 2000, 
                              mtry = 6,
                              importance = TRUE,
                              sampsize = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
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

# Create excel spreadsheet for assignment results
write.csv(importance1, file = "SIA&Vanta_random_forest_importance_corrected.csv")

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish")
importance1 <- read.csv("SIA&Vanta_raw_random_forest_importance_corrected.csv", header = TRUE, fill = TRUE)

## Now plot in ggplot
cairo_ps("SIA&Vanta_importance_plot_corrected_Sept22.eps", height = 7, width = 9)

ggplot(importance1, aes(x = MeanDecreaseAccuracy, y = reorder(Elements, MeanDecreaseAccuracy))) + 
  geom_point(colour = "#D0C018", size = 3.5) +
  geom_segment(aes(x = 0, xend = (MeanDecreaseAccuracy - 0.5), y = Elements, yend = Elements), colour = "black", lwd = 0.5, linetype  = "dashed") +
  labs(x = "Mean decrease accuracy", y = "", title = "SIA & Vanta XRF") +
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
