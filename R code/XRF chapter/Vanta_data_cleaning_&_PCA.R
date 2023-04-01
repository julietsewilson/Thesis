## Code to analyse Vanta data

library(tidyverse)
library(ggfortify)
library(factoextra)
library(extrafont)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Itrax")
data <- read.csv("Vanta data all for analysis corrected.csv", header = TRUE, fill = TRUE)

#Remove rows with NA
data <- na.omit(data)

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

# Give names of elements where all rows are zero
elements_zero <- names(which(colSums(vanta_data_filtered != 0) == 0))

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

# OR Remove elements that are all zero, co-vary or are unrealistic
#vanta_data <- select(vanta_data, -c(V, Cr, Ni, Se, Y, Nb, Cd, Sn, W, Hg, Bi))

### Plot sample data for one element with line at 300cps to see if data is reliable (consistently above 300cps)

#png(filename="Vanta_data_Mg.png", 
#type="cairo",
#height=750, width=1100, 
#pointsize=12, 
#res=96)

ggplot() +
  geom_point(data = vanta_data, aes(x = Sample_ID, y = Mg), colour = "black") +
  #geom_hline(yintercept = 300, linetype = "dashed", size = 1.3, colour = "red") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#dev.off()

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

#Rename West Scotland
sample_means$Region <- recode_factor(sample_means$Region, Wscotland = "West Scotland")

# Reorder locations to be from north to south
sample_means$Region <- factor(sample_means$Region, levels = c("Barents", "Norwegian", "Iceland", "Faroes", "North", "West Scotland", "Rockall", "Baltic", "Irish", "Celtic"))

# Replace zeros with 0.0001 in element data to avoid Inf in PCA
sample_means[sample_means == 0] <- 0.0001

## Calculate log ratios of all elements for PCA
#Calculate ratios of elements in pairs

ratio_1 <- sample_means$S/sample_means$Fe
ratio_2 <- sample_means$Ca/sample_means$Sr
ratio_3 <- sample_means$Si/sample_means$Zn
ratio_4 <- sample_means$U/sample_means$Ba
ratio_5 <- sample_means$Mn/sample_means$K
ratio_6 <- sample_means$Cu/sample_means$As

#Calculate log of all element ratios (ln ratio)
log_ratio_1 <- log(ratio_1)
log_ratio_2 <- log(ratio_2)
log_ratio_3 <- log(ratio_3)
log_ratio_4 <- log(ratio_4)
log_ratio_5 <- log(ratio_5)
log_ratio_6 <- log(ratio_6)

#Create dataframe with log ratios and location column
log_ratio_data <- as.data.frame(cbind(log_ratio_1,
                                      log_ratio_2,
                                      log_ratio_3,
                                      log_ratio_4,
                                      log_ratio_5,
                                      log_ratio_6), stringsAsFactors=FALSE)

log_ratio_data <- cbind(log_ratio_data, location = sample_means$Region)

# Remove Inf etc from log ratio dataframe and replace with NA
#log_ratio_data <- log_ratio_data %>% mutate_all(~replace(., !is.finite(.), NA))

## Plot ratio 1 vs ratio 2
ggplot(log_ratio_data, aes(x = log_ratio_1, y = log_ratio_8, colour = location)) +
  geom_point(size = 3) +
  theme_bw()+
  scale_colour_manual(values = colours)

  #xlab(paste0("ln(", element_1, "/", element_2, ")")) +
  #ylab(paste0("ln(", element_3, "/", element_4, ")"))

colours <- c("pink3", "darkorange3", "darkblue", "olivedrab3", "indianred1", "brown", "darkcyan", "steelblue3", "darkslategray3", "orange")

Plot_log_ratios4vs5 <- ggplot(log_ratio_data, aes(x = log_ratio_4, y = log_ratio_5, colour = location)) +
                             geom_point(size = 7) +
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
                                   legend.margin = margin(l = 2, unit='cm'))  +
                             scale_colour_manual(values = colours)

#To create an EPS file
cairo_ps("Vanta_log_ratios_plot_logratio_4_vs_5.eps", height = 15, width = 21)

Plot_log_ratios4vs5

dev.off()


######### PCA ############

## PCA with raw element data
pca_raw <- prcomp(sample_means[, 2:14], scale = TRUE)

# group datapoints (sample numbers)
#fviz_pca_ind(pca_raw, 
             #col.ind = "cos2", #colour by quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             #repel = TRUE #stop text overlapping
#)

#To create an EPS file
cairo_ps("Vanta_raw_PCA_variables_corrected.eps", height = 5, width = 7)

# plot factor loadings (AAs)
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

#Plot PCA with raw data
PCA_plot <- ggplot(sample_means, aes(PC1, PC2, color = as.factor(Region))) + 
  geom_point(size = 3) +
  labs(x = "PC1", y = "PC2") +
  theme_bw()
PCA_plot

# OR
colours <- c("pink3", "darkorange3", "darkblue", "olivedrab3", "indianred1", "brown", "darkcyan", "steelblue3", "darkslategray3", "orange")

PCA_plot_raw <- autoplot(pca_raw, data = sample_means, 
                            colour = "Region",
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
cairo_ps("Vanta_raw_PCA_plot_corrected_Sept22.eps", height = 15, width = 21)

PCA_plot_raw

dev.off()


## PCA with log ratio data

log_ratio_data <- na.omit(log_ratio_data)

pca_log_ratio <- prcomp(log_ratio_data[, 1:6], scale = TRUE)

# group datapoints (sample numbers)
#fviz_pca_ind(pca_log_ratio, 
             #col.ind = "cos2", #colour by quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             #repel = TRUE #stop text overlapping
#)

cairo_ps("Vanta_logratio_PCA_variables_corrected.eps", height = 5, width = 7)

# plot factor loadings (AAs)
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
cairo_ps("Vanta_log_ratios_PCA_plot_corrected_Sept22.eps", height = 15, width = 21)

PCA_plot_log_ratios

dev.off()




## Calculate log ratio of two elements
# Update elements to ones want to use in log ratio

#Elements for ratio 1
element_1 <- "Fe"
element_2 <- "K"
#Elements for ratio 2
element_3 <- "Ca"
element_4 <- "U"

# Extract data for specified elements
element_1_data <- pull(sample_means, element_1)
element_2_data <- pull(sample_means, element_2)
element_3_data <- pull(sample_means, element_3)
element_4_data <- pull(sample_means, element_4)

# Calculate ratios of elements
ratio_1 <- element_1_data/element_2_data
ratio_2 <- element_3_data/element_4_data

# Calculate log value of ratio 1 and ratio 2 - ln(ratio)
log_ratio_1 <- log(ratio_1)
log_ratio_2 <- log(ratio_2)
