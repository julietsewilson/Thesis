
#Bring in data
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
distance_data_all <- read.csv("Sample_distance_data_unique_IDs.csv", header = TRUE, fill = TRUE)
itrax_data <- read.csv("Itrax final data from R with selected data.csv", header = TRUE, fill = TRUE)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax/Sample height data")
height_data_all <- read.csv("Height_data_all.csv", header = TRUE, fill = TRUE)

# Remove unuseable samples from distance data
distance_data_all <- subset(distance_data_all, Sample_no %in% itrax_data$sample_ID)


### Function to calculate sample thickness

Function_sample_thickness <- function(sample) {
  
  #Select data on distances for one sample only
  distance_data <- subset(distance_data, Sample_no == sample)
  start_coord <- distance_data$Start_coordinate
  stop_coord <- distance_data$Stop_coordinate
  
  #Subset height data to only keep data where distance matches sample plus 5mm either side
  height_data_sample <- subset(height_data, distance_mm >= (start_coord - 10) & distance_mm <= (stop_coord + 10))
  lowest_height <- min(height_data_sample$height_cm)
  
  #Subset itrax data for only one sample
  itrax_data_sample <- subset(itrax_data, sample_ID == sample)
  
  #Extract beginning and end distance used from itrax data (flat part)
  start_flat <- min(itrax_data_sample$position_mm)
  stop_flat <- max(itrax_data_sample$position_mm)
  
  #Select height data between useable start and stop
  height_data_flat <- subset(height_data, distance_mm >= start_flat & distance_mm <= stop_flat)
  
  #Calculate minimum height (or mean height)
  flat_height <- min(height_data_flat$height_cm)
  #mean_height <- mean(height_data_sample$height_cm)
  
  #Calculate thickness of sample in cm
  sample_thickness <- round(flat_height - lowest_height, 3)
  
  return(sample_thickness)
  
}

###### Select samples from each date separately and run function

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "5032021")
distance_data <- subset(distance_data_all, Date == "5032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 

  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_05032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "12032021")
distance_data <- subset(distance_data_all, Date == "12032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_12032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "16032021")
distance_data <- subset(distance_data_all, Date == "16032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_16032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "16032021_2")
distance_data <- subset(distance_data_all, Date == "16032021_2")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_16032021_2 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "16122020")
distance_data <- subset(distance_data_all, Date == "16122020")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_16122020 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "18032021")
distance_data <- subset(distance_data_all, Date == "18032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_18032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "19032021")
distance_data <- subset(distance_data_all, Date == "19032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_19032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "23032021")
distance_data <- subset(distance_data_all, Date == "23032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_23032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "25032021")
distance_data <- subset(distance_data_all, Date == "25032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_25032021 <- Sample_thickness_data

#####

#Subset height and distance data for that date
height_data <- subset(height_data_all, date == "26032021")
distance_data <- subset(distance_data_all, Date == "26032021")

#Make list of sample numbers on that date
samples_list <- distance_data$Sample_no
samples_list <- droplevels(samples_list)

#Create empty dataframe for sample thickness data
Sample_thickness_data <- NULL

#Run sample thickness function for each sample on that date
for(sample_id in samples_list) { 
  
  thickness <- Function_sample_thickness(sample_id)
  
  #Put sample number and thickness together
  result <- as.data.frame(cbind(sample_ID = sample_id, thickness))
  
  #Add new row into sample thickness data for new sample
  Sample_thickness_data <- rbind(Sample_thickness_data, result)
  
}

#Save sample thickness results for that date
thickness_26032021 <- Sample_thickness_data


#### Combine thickness data from all dates ####
Sample_thickness_data_all <- rbind(thickness_05032021,
                                   thickness_12032021,
                                   thickness_16032021,
                                   thickness_16032021_2,
                                   thickness_16122020,
                                   thickness_18032021,
                                   thickness_19032021,
                                   thickness_23032021,
                                   thickness_25032021,
                                   thickness_26032021)

#Write thickness data to csv file
write.csv(Sample_thickness_data_all, file = "Itrax sample thickness data.csv",
          row.names=FALSE)
