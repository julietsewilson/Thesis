library(tidyverse)
library(dplyr)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
distance_data <- read.csv("Sample_distance_data_unique_IDs.csv", header = TRUE, fill = TRUE)
itrax_data <- read.csv("Itrax_final_data_v2.csv", header = TRUE, fill = TRUE)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax/Sample height data")
height_data <- read.csv("Height_data_all.csv", header = TRUE, fill = TRUE)

########################## Plotting sample data ###########################################

#Select samples from one DATE only
height_data_subset <- subset(height_data, date == "19032021")
distance_data_subset <- subset(distance_data, Date == "19032021")

#Drop empty levels caused by subsetting original data (otherwise samples_list has too many factor levels)
height_data_subset <- droplevels(height_data_subset)
distance_data_subset <- droplevels(distance_data_subset)


####### Function to plot height data of a sample and draw lines for 30% either side of mean

Function_sample_subset_plot <- function(sample) {

#Extract start and stop distance for one SAMPLE only
distance_data_sample <- subset(distance_data_subset, Sample_no == sample)

Start <- distance_data_sample$Start_coordinate
Stop <- distance_data_sample$Stop_coordinate

#Select height data for that sample, using start and stop data
height_data_sample <- subset(height_data_subset, distance_mm >= (Start - 10) & distance_mm <= (Stop + 10))

#Calculate half way point between start and stop coordinates
half_distance <- mean(c(Start, Stop))

#Calculate 30% of distance of sample
percentage_distance <- (Stop - Start)*0.3

#Calculate 30% above and below mean as upper and lower limits of data
upper <- half_distance + percentage_distance
lower <- half_distance - percentage_distance

#Extract itrax data from that sample for dots
itrax_data_sample <- subset(itrax_data, sample_ID == sample)

#Tell R filename to save plot
myFilename <- file.path(paste("Sample_height_", sample, ".png", sep = ""))

###Plot distance vs height for that sample
#Start and stop coordinates in blue
#Upper and lower limits in red - 30% above and below mean
myPlot <- ggplot(height_data_sample, aes(x = distance_mm, y = height_cm)) +
          geom_line() +
          geom_point(data = itrax_data_sample, aes(x = position_mm, y = 7.5, 
                     colour = cut(MSE, c(0, 5, Inf))), size = 0.5) +
          scale_color_manual(name = "MSE",
                             values = c("(0,5]" = "blue", "(5,Inf]" = "red")) +
          ylim(6.9, 7.5) +
          coord_fixed(ratio = 40/1) +
          ggtitle(sample) +
          theme_bw() +
          #Add lines for start and stop coordindates of the sample
          geom_vline(xintercept = Start, color = "blue") +
          geom_vline(xintercept = Stop, color = "blue") +
          #Add lines for upper and lower limits
          geom_vline(xintercept = lower, color = "red", linetype = "dashed") +
          geom_vline(xintercept = upper, color = "red", linetype = "dashed")
  
#Save plot
ggsave(filename = myFilename, plot = myPlot, device = "png", width=10, height=7)

}


#Create list of samples on that date
samples_list <- distance_data_subset$Sample_no

##### Loop to apply sample height function to each sample
for(i in samples_list) { 
  
Function_sample_subset_plot(i)
  
}



########################### Subset flat part of data for each sample #######################

# Make list of unuseable samples
unuseable <- c("200",
               "281",
               "306",
               "381",
               "406",
               "1048",
               "1052",
               "1136")

# Remove unuseable samples from distance data and itrax data
itrax_data_filtered <- subset(itrax_data, ! sample_ID %in% unuseable)

# Get list of altered start and stop distances for samples needing to be altered
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
altered_samples <- read.csv("Samples_for_altering.csv", header = TRUE, fill = TRUE)


#### Split data into two parts for samples needing altering and not

# Select data that doesn't need altering (will use centre 60% of data)
samples_part1 <- filter(itrax_data_filtered, !(sample_ID %in% altered_samples$Samples_altered))
samples_part1 <- droplevels(samples_part1)

# Select data that needs altering (using new start and stop coordinates)
samples_part2 <- filter(itrax_data_filtered, sample_ID %in% altered_samples$Samples_altered)
samples_part2 <- droplevels(samples_part2)

#Create list of all samples analysed in both parts of data
samples_list_part1 <- unique(samples_part1$sample_ID)
samples_list_part2 <- unique(samples_part2$sample_ID)

#Create empty dataframe for final part1 and part 2 data
itrax_final_data_part1 <- NULL
itrax_final_data_part2 <- NULL

#### Select data in centre 60% for data part 1 (i.e. 30% either side of mean) for each sample

for (sample in samples_list_part1) {
  
  #Extract start and stop distance for one sample only
  distance_data_sample <- subset(distance_data, Sample_no == sample)
  
  Start <- distance_data_sample$Start_coordinate
  Stop <- distance_data_sample$Stop_coordinate
  
  #Calculate half way point between start and stop coordinates
  half_distance <- mean(c(Start, Stop))
  
  #Calculate 30% of distance of sample
  percentage_distance <- (Stop - Start)*0.3
  
  #Calculate 30% above and below mean as upper and lower limits of data
  upper <- half_distance + percentage_distance
  lower <- half_distance - percentage_distance
  
  #Extract itrax data from that sample for dots
  itrax_data_sample <- subset(samples_part1, sample_ID == sample & position_mm >= lower & position_mm <= upper)
  
  #Add data for that sample to final data
  itrax_final_data_part1 <- rbind(itrax_final_data_part1, itrax_data_sample)
  
}


#### Select itrax data where is between altered start and stop coordinates for samples that need altering

for (sample in samples_list_part2) {
  
  #Subset altered samples file to extract distances for that sample
  distances_sample <- subset(altered_samples, Samples_altered == sample)
  
  #Get new start and stop distances for sample
  lower <- distances_sample$New_start
  upper <- distances_sample$New_stop
  
  #Extract itrax data for that sample between start and stop distances
  itrax_data_sample <- subset(samples_part2, sample_ID == sample & position_mm >= lower & position_mm <= upper) 

  #Add data for that sample to final data
  itrax_final_data_part2 <- rbind(itrax_final_data_part2, itrax_data_sample)
  
}

#### Combine part1 and part2 data into one final dataset
itrax_final_data <- rbind(itrax_final_data_part1, itrax_final_data_part2)

#Write final itrax data to an excel spreadsheet
write.csv(itrax_final_data, file = "Itrax final data from R with selected data.csv",
          row.names=FALSE)
