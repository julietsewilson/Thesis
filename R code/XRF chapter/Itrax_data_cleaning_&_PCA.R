## Code to prep Itrax data for analysis

library(tidyverse)
library(dplyr)
library(factoextra)
library(patchwork)
library(ggfortify)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
data <- read.csv("Itrax final data from R with selected data - Norwegian swapped.csv", header = TRUE, fill = TRUE)

# Select rows where validity is 1, remove rows where validity is zero
itrax_data <- filter(data, validity == 1)

## Remove rows with Mean Squared Error over acceptable limit
# Calculate mean and standard deviation of MSE
#MSE_mean <- mean(itrax_data$MSE)
#MSE_stdev <- sd(itrax_data$MSE)

# Calculate maximum MSE value accepted - 2 standard deviations above mean
#MSE_upper <- MSE_mean + (MSE_stdev*2)

# Select only rows where MSE is below maximum accepted value ####################
#itrax_data <- filter(itrax_data, MSE < MSE_upper)

itrax_data <- filter(itrax_data, MSE < 5)

### Plot sample data for one element with line at 300cps to see if data is reliable (consistently above 300cps)

#png(filename="Itrax_data_Al.png", 
    #type="cairo",
    #height=750, width=1100, 
    #pointsize=12, 
    #res=96)

#ggplot() +
  #geom_point(data = itrax_data, aes(x = position_mm, y = Al), colour = "black") +
  #geom_hline(yintercept = 300, linetype = "dashed", size = 1.3, colour = "red") +
  #theme_bw()

#dev.off()

# Give names of elements where all rows are zero
elements_zero <- names(which(colSums(itrax_data != 0) == 0))

# List selected elements to use
Selected_elements <- c("S",
                       "Cl",
                       "K",
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

# Normalise by Mo_inc
#itrax_data_normalised <- (itrax_data_selected_elements[, 2:23])/itrax_data_selected_elements$Mo_coh
#Add sample_ID to data
#itrax_data_normalised <- cbind(sample_ID = itrax_data_selected_elements$sample_ID, itrax_data_normalised)

# Remove elements not wanted e.g. cps is not consistently over 300, co-vary or not realistic
#itrax_data <- select(itrax_data, -c(Al, Si, Ti, Zr, Au, Bi, Th))

# Create table with Sample ID column and element data
#data_for_averages <- cbind(sample_ID = itrax_data$sample_ID, itrax_data[13:34])

# Calculate mean values for each sample
sample_means <- itrax_data_selected_elements %>% 
                group_by(sample_ID) %>%
                summarise_all(mean, na.rm = TRUE)

mean(sample_means$Cl)
# Calculate standard deviation for each sample to be used in error bars
sample_stdev <- itrax_data_selected_elements %>% group_by(sample_ID) %>% summarise_all(sd, na.rm = TRUE)

# Get location info from original data
location_data <- select(itrax_data, sample_ID, location)
# Remove duplicates from basin data
location_data <- distinct(location_data)

# Join location data onto sample means data and standard deviation data
sample_means <- merge(sample_means, location_data, by = "sample_ID")  
sample_stdev <- merge(sample_stdev, location_data, by = "sample_ID") 

#Rename West Scotland
sample_means$location <- recode_factor(sample_means$location, Wscotland = "West Scotland")

# Reorder locations to be from north to south
sample_means$location <- factor(sample_means$location, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Irish", "Celtic"))

# Replace zeros with 0.0001 in element data to avoid Inf in PCA
sample_means[sample_means == 0] <- 0.0001

#Function to calculate z scores for all elements
#z_score_calculation <- function(data, element) {
  
  #mean <- mean(data[[element]])
  #stdev <- sd(data[[element]])
  
  #zscores <- (data[[element]] - mean)/stdev
  
  #return(zscores)

#} 

#Create empty dataframe for z scores
#z_scores <- NULL

#Run function for each of selected elements
#for(element in Selected_elements) {
  
  #z_scores[[element]] <- z_score_calculation(sample_means, element)
  
#}

#Convert into dataframe
#z_scores <- as.data.frame(z_scores)

#Bind z scores to sample IDs and location data
#z_scores <- cbind(z_scores, sample_ID = sample_means$sample_ID, location = sample_means$location)

# Calculate ratios of z scores
#ratio_1 <- z_scores$Cl/z_scores$Sr
#ratio_2 <- z_scores$Br/z_scores$Cd
#ratio_3 <- z_scores$Cu/z_scores$K
#ratio_4 <- z_scores$Ni/z_scores$Ca
#ratio_5 <- z_scores$Se/z_scores$U
#ratio_6 <- z_scores$Mn/z_scores$I
#ratio_7 <- z_scores$Zn/z_scores$Fe
#ratio_8 <- z_scores$As/z_scores$Pb
#ratio_9 <- z_scores$Cr/z_scores$Sc
#ratio_10 <- z_scores$Sn/z_scores$Eu
#ratio_11 <- z_scores$Cs/z_scores$Sm

#### Or do ratios first then calculate z scores????

#Make dataframe with all z score ratios
#zscores_data <- as.data.frame(cbind(ratio_1,
                                    #ratio_2,
                                    #ratio_3,
                                    #ratio_4,
                                    #ratio_5,
                                    #ratio_6,
                                    #ratio_7,
                                    #ratio_8,
                                    #ratio_9,
                                    #ratio_10,
                                    #ratio_11), stringsAsFactors=FALSE)

#Add on location data to z scores
#zscores_data <- cbind(zscores_data, location = z_scores$location)

#Cl,K,Ca,Sc,Cr,Mn,Fe,Ni,Cu,Zn,As,Se,Br,Sr,Cd,Sn,I,Cs,Sm,Eu,Pb,U

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

#ratio_1 <- sample_means$Cl/sample_means$Sr
#ratio_2 <- sample_means$Br/sample_means$Cd
#ratio_3 <- sample_means$Cu/sample_means$K
#ratio_4 <- sample_means$Se/sample_means$Ca
#ratio_5 <- sample_means$Mn/sample_means$I
#ratio_6 <- sample_means$Zn/sample_means$Fe
#ratio_7 <- sample_means$As/sample_means$Pb
#ratio_8 <- sample_means$Cr/sample_means$Sc
#ratio_9 <- sample_means$Cs/sample_means$S

#ratio_1 <- sample_means$Cl/sample_means$Sr
#ratio_2 <- sample_means$Br/sample_means$Cd
#ratio_3 <- sample_means$K/sample_means$Se
#ratio_4 <- sample_means$Ni/sample_means$Ca
#ratio_5 <- sample_means$Cu/sample_means$U
#ratio_6 <- sample_means$Cr/sample_means$I
#ratio_7 <- sample_means$Zn/sample_means$Pb
#ratio_8 <- sample_means$As/sample_means$Fe
#ratio_9 <- sample_means$Mn/sample_means$Cs
#ratio_10 <- sample_means$Sn/sample_means$Sm
#ratio_11 <- sample_means$Eu/sample_means$Sc

#ratio_1 <- sample_means$Cl/sample_means$Sr
#ratio_2 <- sample_means$Br/sample_means$Cd
#ratio_3 <- sample_means$Cu/sample_means$K
#ratio_4 <- sample_means$Ni/sample_means$Ca
#ratio_5 <- sample_means$Se/sample_means$Th
#ratio_6 <- sample_means$Mn/sample_means$I
#ratio_7 <- sample_means$Zn/sample_means$Fe
#ratio_8 <- sample_means$As/sample_means$Pb
#ratio_9 <- sample_means$Cr/sample_means$Bi

#ratio_1 <- sample_means$Cl/sample_means$Bi
#ratio_2 <- sample_means$Se/sample_means$Sr
#ratio_3 <- sample_means$Cu/sample_means$Zn
#ratio_4 <- sample_means$K/sample_means$Cd
#ratio_5 <- sample_means$Br/sample_means$Ca
#ratio_6 <- sample_means$Mn/sample_means$As
#ratio_7 <- sample_means$Ni/sample_means$Fe
#ratio_8 <- sample_means$I/sample_means$Pb
#ratio_9 <- sample_means$Cr/sample_means$Th


#function_PCA <-  function(version, ratio_1, ratio_2, ratio_3, ratio_4, ratio_5, ratio_6, ratio_7, ratio_8, ratio_9, ratio_10, ratio_11, data) {

#Calculate log of all element ratios (ln ratio)
log_ratio_1 <- log(ratio_1)
log_ratio_2 <- log(ratio_2)
log_ratio_3 <- log(ratio_3)
log_ratio_4 <- log(ratio_4)
log_ratio_5 <- log(ratio_5)
log_ratio_6 <- log(ratio_6)
log_ratio_7 <- log(ratio_7)
log_ratio_8 <- log(ratio_8)
log_ratio_9 <- log(ratio_9)

#Create dataframe with log ratios and location column
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

# Remove Inf etc from log ratio dataframe and replace with NA or zero
#log_ratio_data <- log_ratio_data %>% mutate_all(~replace(., !is.finite(.), NA))

####  PCA using log ratio data

# Remove rows with NA
#log_ratio_data <- na.omit(log_ratio_data)

#Caluculate PCA
#pca_log_ratio <- prcomp(log_ratio_data[, 1:11], scale = TRUE)

#get pca values for individuals

#res.ind <- get_pca_ind(pca_log_ratio)

#append to dataframe

#log_ratio_data$PC1 <- res.ind$coord[,1]
#log_ratio_data$PC2 <- res.ind$coord[,2]

#Plot PCA with log ratio data
#PCA_plot <- ggplot(log_ratio_data, aes(PC1, PC2, color = as.factor(location))) + 
            #geom_point(size = 3) +
            #labs(x = "PC1", y = "PC2") +
            #theme_bw()
    #stat_ellipse(type = "norm", 
               #level = 0.9, 
               #size = 1, 
               #geom = "polygon", 
               #aes(fill = location), 
               #alpha = 0)

#myFilename <- file.path(paste("PCA log ratio Cl v", version, ".png", sep = ""))

#Save plot
#ggsave(filename = myFilename, plot = PCA_plot, device = "png", width=10, height=7)

#}


### Plot two log ratios against each other

#colours <- c("steelblue3", "darkorange3", "orange", "olivedrab3", "darkblue", "darkslategray3", "indianred1", "purple", "darkcyan", "brown")

cairo_ps("Itrax_logratio_10_vs_logratio_3.eps", height = 5, width = 7)

ggplot(log_ratio_data, aes(x = log_ratio_10, y = log_ratio_3, colour = location)) +
  geom_point(size = 3) +
  theme_bw() +
  scale_colour_manual(values = colours)

dev.off()

#+
  #xlab(paste0("ln(", element_1, "/", element_2, ")")) +
  #ylab(paste0("ln(", element_3, "/", element_4, ")")) +
  #scale_colour_manual(values = colours)

ggplot(sample_means, aes(x = Fe, y = K, colour = location)) +
  geom_point(size = 3) +
  theme_bw() 
  

####### PCA using raw element data #######

## check that any zero variance columns have been removed
#varcheck <- which(apply(sample_means, 2, var) == 0)
#dataZ <- subset(sample_means, select = -varcheck[2])

#### PCA using raw data
pca_raw <- prcomp(sample_means[, 2:20], scale = TRUE)

# group datapoints (sample numbers)

#fviz_pca_ind(pca_raw, 
             #col.ind = "cos2", #colour by quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             #repel = TRUE #stop text overlapping
#)


# plot factor loadings (AAs)

#To create an EPS file
cairo_ps("Itrax_raw_PCA_variables_Sept22.eps", height = 5, width = 7)

fviz_pca_var(pca_raw, 
             col.var = "contrib", #colour by quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE #stop text overlapping
             )

dev.off()

#get pca values for individuals

res.ind <- get_pca_ind(pca_raw)

#append to dataframe

sample_means$PC1 <- res.ind$coord[,1]
sample_means$PC2 <- res.ind$coord[,2]

colours <- c("pink3", "darkorange3", "darkblue", "olivedrab3", "indianred1", "brown", "darkcyan", "steelblue3", "darkslategray3", "orange")


#Plot PCA with raw data
PCA_plot_raw <- ggplot(sample_means, aes(PC1, PC2, colour = as.factor(location))) + 
  geom_point(size = 7) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(family = "Calibri"),
        axis.title = element_text(size = 36),
        axis.text = element_text(size = 36),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 15)),
        legend.text = element_text(size = 36),
        legend.spacing.y = unit(0.5, "cm"),
        legend.spacing.x = unit(1, "cm"),
        legend.margin = margin(l = 1.5, unit='cm')) +
  guides(colour = guide_legend(byrow = TRUE)) +
  scale_colour_manual(values = colours)

# OR
PCA_plot_raw_v2 <- autoplot(pca_raw, data = sample_means, 
                            colour = "location",
                            size = 7) +
                   theme_bw() +
                   theme(legend.title = element_blank(),
                         text = element_text(family = "Arial"),
                         axis.title = element_text(size = 36),
                         axis.text = element_text(size = 36),
                         axis.title.y = element_text(margin = margin(r = 15)),
                         axis.title.x = element_text(margin = margin(t = 15)),
                         legend.text = element_text(size = 36),
                         legend.spacing.y = unit(0.3, "cm"),
                         legend.spacing.x = unit(1, "cm"),
                         legend.margin = margin(l = 1.5, unit='cm'))  +
                   guides(colour = guide_legend(byrow = TRUE)) +
                   scale_colour_manual(values = colours)

#To create an EPS file
cairo_ps("Itrax_raw_PCA_plot_Sept22_v2.eps", height = 15, width = 21)

PCA_plot_raw_v2

dev.off()



###### PCA using log ratio data ########

# Remove rows with NA
log_ratio_data <- na.omit(log_ratio_data)

pca_log_ratio <- prcomp(log_ratio_data[, 1:9], scale = TRUE)

# group datapoints (sample numbers)

fviz_pca_ind(pca_log_ratio, 
             col.ind = "cos2", #colour by quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE #stop text overlapping
)


# plot factor loadings (AAs)

cairo_ps("Itrax_logratio_PCA_variables_v2.eps", height = 5, width = 7)

fviz_pca_var(pca_log_ratio, 
             col.var = "contrib", #colour by quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE #stop text overlapping
)

dev.off()

#get pca values for individuals

res.ind <- get_pca_ind(pca_log_ratio)

#append to dataframe

log_ratio_data$PC1 <- res.ind$coord[,1]
log_ratio_data$PC2 <- res.ind$coord[,2]

#Plot PCA with log ratio data
PCA_plot <- ggplot(log_ratio_data, aes(PC1, PC2, color = as.factor(location))) + 
  geom_point(size = 3) +
  labs(x = "PC1", y = "PC2") +
  theme_bw()
PCA_plot

# OR

colours <- c("pink3", "darkorange3", "darkblue", "olivedrab3", "indianred1", "brown", "darkcyan", "steelblue3", "darkslategray3", "orange")

PCA_plot_log_ratios <- autoplot(pca_log_ratio, data = log_ratio_data, 
                                colour = "location",
                                size = 7) +
                       theme_bw() +
                       theme(legend.title = element_blank(),
                             text = element_text(family = "Arial"),
                             axis.title = element_text(size = 36),
                             axis.text = element_text(size = 36),
                             axis.title.y = element_text(margin = margin(r = 15)),
                             axis.title.x = element_text(margin = margin(t = 15)),
                             legend.text = element_text(size = 36),
                             legend.spacing.y = unit(0.3, "cm"),
                             legend.spacing.x = unit(1, "cm"),
                             legend.margin = margin(l = 1.5, unit='cm'))  +
                       guides(colour = guide_legend(byrow = TRUE)) +
                       scale_colour_manual(values = colours)

#To create an EPS file
cairo_ps("Itrax_log_ratios_PCA_plot_Sept22_v2.eps", height = 15, width = 21)

PCA_plot_log_ratios

dev.off()





#### Eel way of doing PCA ######

#Make data frame with pca values and locations
pca <- as.data.frame(pca_raw$x)
pca$group <- factor(sample_means$location, levels = c("Baltic", "Barents", "Celtic", "Faroes", "Iceland", "Irish", "North", "Norwegian", "Rockall", "Wscotland"))

#Plot PC1 vs PC2, coloured by location
pca_plot <- ggplot(pca, aes(x=PC1, y=PC2, color = group)) +
  geom_point() +
  theme_bw()

#Make data frame with pca rotations and fatty acid/SI names
pca_rotation <- as.data.frame(pca_raw$rotation)
pca_rotation$feature <- row.names(pca_rotation)

#Plot contributions of variables to PC1 and PC2 
FA_contributions <- ggplot(pca_rotation, aes(x = PC1, y = PC2, label = feature, color = feature)) +
  geom_point() + 
  theme_bw() + 
  geom_text(size=3)

#OR....
#library(ggfortify)

colours <- c("steelblue3", "darkorange3", "orange", "olivedrab3", "darkblue", "darkslategray3", "indianred1", "purple", "darkcyan")

autoplot(pca_raw, data = sample_means, 
         colour = "location",
         size = 3,
         loadings = TRUE,
         loadings.colour = "black",
         loadings.label = TRUE,
         loadings.label.colour = "black",
         loadings.label.size = 4) +
  theme_bw() +
  scale_colour_manual(values = colours)
