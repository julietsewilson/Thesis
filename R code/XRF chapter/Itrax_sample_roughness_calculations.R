
#Bring in data
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
distance_data_all <- read.csv("Sample_distance_data_unique_IDs.csv", header = TRUE, fill = TRUE)
itrax_data <- read.csv("Itrax final data from R with selected data.csv", header = TRUE, fill = TRUE)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax/Sample height data")
height_data_all <- read.csv("Height_data_all.csv", header = TRUE, fill = TRUE)

# Remove unuseable samples from distance data
distance_data_all <- subset(distance_data_all, Sample_no %in% itrax_data$sample_ID)


### Function to calculate sample roughness

Function_sample_roughness <- function(sample) {
  
  #Subset itrax data for only one sample
  itrax_data_sample <- subset(itrax_data, sample_ID == sample)
  
  #Extract beginning and end distance used from itrax data (flat part)
  start_flat <- min(itrax_data_sample$position_mm)
  stop_flat <- max(itrax_data_sample$position_mm)
  
  #Select height data between useable start and stop
  height_data_flat <- subset(height_data, distance_mm >= start_flat & distance_mm <= stop_flat)
  
  #Calculate difference between successive rows in height data
  height_diff <- diff(height_data_flat$height_cm)
  
  #Make all height differences positive
  height_diff <- abs(height_diff)
  
  #Add up all height differences to find total height change and divide by the distance
  sample_roughness <- sum(height_diff)/(stop_flat - start_flat)
  
  sample_roughness <- round(sample_roughness, 3)
  
  return(sample_roughness)

}

#### OR calculate diff between highest and lowest height within flat part of sample



###### Select samples from each date separately and run function

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "5032021")
distance_data <- subset(distance_data_all, Date == "5032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_05032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "12032021")
distance_data <- subset(distance_data_all, Date == "12032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_12032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "16032021")
distance_data <- subset(distance_data_all, Date == "16032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_16032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "16032021_2")
distance_data <- subset(distance_data_all, Date == "16032021_2")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_16032021_2 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "16122020")
distance_data <- subset(distance_data_all, Date == "16122020")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_16122020 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "18032021")
distance_data <- subset(distance_data_all, Date == "18032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_18032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "19032021")
distance_data <- subset(distance_data_all, Date == "19032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_19032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "23032021")
distance_data <- subset(distance_data_all, Date == "23032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_23032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "25032021")
distance_data <- subset(distance_data_all, Date == "25032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_25032021 <- Sample_roughness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "26032021")
distance_data <- subset(distance_data_all, Date == "26032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_26032021 <- Sample_roughness_data

#####


#### Combine roughness data from all dates ####
Sample_roughness_data_all <- rbind(roughness_05032021,
                                   roughness_12032021,
                                   roughness_16032021,
                                   roughness_16032021_2,
                                   roughness_16122020,
                                   roughness_18032021,
                                   roughness_19032021,
                                   roughness_23032021,
                                   roughness_25032021,
                                   roughness_26032021)

#Write roughness data to csv file
write.csv(Sample_roughness_data_all, file = "Itrax sample roughness data v2.csv",
          row.names=FALSE)



#### Roughness calculation for unuseable samples ####

#Bring in data
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
distance_data_all <- read.csv("Sample_distance_data_unique_IDs.csv", header = TRUE, fill = TRUE)
itrax_data_unuseable <- read.csv("Itrax_data_unuseable.csv", header = TRUE, fill = TRUE)

#Subset unuseable sample data
distance_data <- subset(distance_data_all, Sample_no %in% itrax_data_unuseable$sample_ID)

#Make list of sample numbers on that date
#samples_list <- distance_data$Sample_no
#samples_list <- droplevels(samples_list)

samples_list <- c(281, 306, 381)
height_data <- subset(height_data_all, date == "5032021")

itrax_unuseable_data <- NULL

for (sample in samples_list) {
  
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
  itrax_data_sample <- subset(itrax_data_unuseable, sample_ID == sample & position_mm >= lower & position_mm <= upper)
  
  #Add data for that sample to final data
  itrax_unuseable_data <- rbind(itrax_unuseable_data, itrax_data_sample)
  
}

itrax_data <- itrax_unuseable_data

#Create empty dataframe for sample roughness data
Sample_roughness_data <- NULL

#Run sample roughness function for each sample on that date
for(sample_id in samples_list) { 
  
  roughness <- Function_sample_roughness(sample_id)
  
  #Put sample number and roughness together
  result <- as.data.frame(cbind(sample_ID = sample_id, roughness))
  
  #Add new row into sample roughness data for new sample
  Sample_roughness_data <- rbind(Sample_roughness_data, result)
  
}

#Save sample roughness results for that date
roughness_unuseable <- Sample_roughness_data
