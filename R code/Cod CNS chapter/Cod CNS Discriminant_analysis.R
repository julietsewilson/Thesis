library(dplyr)
library(caret)
library(ggplot2)
library(MASS)
library(mda)
library(RColorBrewer)

#Load in R script with Kiljunen function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Kiljunen_correction.R")

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/Master data spreadsheets")
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

#Reorder locations from north to south
data$Location <- factor(data$Location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Irish", "Celtic"))

#Select only columns with variables to use for classification, plus FishID for sub-sampling
##dplyr::select tells R to use the dplyr package since it's masked by the MASS package
DA_data <- dplyr::select(data, FishID, Location, d13Cc, d15N, d34S)

##Split data into two subsets
subset_test <- DA_data %>% group_by(Location) %>% sample_frac(.25)
subset_training <- anti_join(DA_data, subset_test, by = "FishID")

#Remove FishID column since is not a variable for classification
##dplyr::select tells R to use the dplyr package since it's masked by the MASS package
test <- dplyr::select(subset_test, -FishID)
training <- dplyr::select(subset_training, -FishID)

# Fit the models
## linear discriminant analysis
lda_model <- lda(Location~., data = training)
## quadratic discriminant analysis
qda_model <- qda(Location~., data = training)
## mixture discriminant analysis
mda_model <- mda(Location~., data = training)

# Make predictions - gives 3 results - class predictions, posterior probabilities and LDA values
lda_predictions <- lda_model %>% predict(test)
qda_predictions <- qda_model %>% predict(test)
mda_predictions <- mda_model %>% predict(test)

#Then look at class predictions, posterior probabilities and LDA values (LD1, LD2, LD3)
head(lda_predictions$class, 6)
head(lda_predictions$posterior, 6) 
head(lda_predictions$x, 3)

#Name Location column in test data as True_location
True_location <- test$Location

#Or predict test data and then make data frame with test sample isotope values and predicted class
lda_predictions <- data.frame(True_location, Assigned_location = predict(lda_model, test)$class)
qda_predictions <- data.frame(True_location, Assigned_location = predict(qda_model, test)$class)
mda_predictions <- data.frame(True_location, Assigned_location = predict(mda_model, test))

# Model accuracy
mean(lda_predictions$Assigned_location == True_location)

#LDA plot for training data - lda_model$x gives LDA values
lda.data <- cbind(training, predict(lda_model)$x)

cairo_ps("LDA_scatterplot_Sept22.eps", height = 10, width = 15)

ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Location), size = 4) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_blank())

dev.off()

#Calculate linear discriminant analysis for all data (not just training)
lda_data_all <- dplyr::select(DA_data, -FishID)

lda_model <- lda(Location~., data = lda_data_all)

#Calculate predictions for all data
lda_predictions <- lda_model %>% predict(lda_data_all)

#Add isotope data to prediction data
#LDA plot for all data - lda_model$x gives LDA values
lda.data2 <- cbind(lda_data_all, predict(lda_model)$x)

#colours <- c("pink3", "darkorange3", "darkblue", "olivedrab3", "indianred1", "brown", "darkslategray3", "steelblue3", "darkcyan", "orange")


cairo_ps("LDA_scatterplot_all_Jan23.eps", height = 10, width = 15)

ggplot(lda.data2, aes(LD1, LD2)) +
  geom_point(aes(color = Location), size = 5) +
  theme_bw() +
  #scale_colour_manual(values = colours) +
  scale_colour_brewer(palette = "Paired") +
  theme(text = element_text(family = "Arial"),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_blank())

dev.off()
