library(plyr)
library(dplyr)
library(gstat)
library(sp)
library(maps)
library(mapdata)
library(lattice)
library(raster)
library(maptools)
library(rgdal)
library(scales)
library(magrittr)
library(viridis)

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape")
North_sea_data <- read.csv("North Sea isoscape data haddock.csv", header = TRUE)

#CN ratio - do we need to apply lipid corrections? This shows if you need to or not
#Linear regression of CN ratio to d13C 
plot(North_sea_data$CN,North_sea_data$d13C)
lm<-lm(North_sea_data$CN~North_sea_data$d13C,data=North_sea_data)
summary(lm)

#Load in R script with Kiljunen function
source("C:/Users/jw17g17/Documents/PhD/R_code/Github/seafood-origin/seafood-assignment/Kiljunen_correction.R")

#Calculate corrected carbon values using Kiljunen function and add as a new column
North_sea_data$d13Cc <- ifelse(North_sea_data$CN > 3.4, kiljunenCalculation(North_sea_data), North_sea_data$d13C)

#station_means <- North_sea_data %>% group_by(Station) %>% summarize(mean_S = mean(d34S))

#With Kriging we can't have more than one value in the same location
#so we need to average out the values where we have multiple samples at the same lat/lon

#Average CPUE at each identical Year, Quarter, Age, Area 
data <- ddply(North_sea_data, c("Latitude","Longitude"), function(x){
  d13C <- mean(x$d13Cc)
  d15N <- mean(x$d15N)
  d34S <- mean(x$d34S)
  Lat <- x$Latitude
  Long <- x$Longitude
  data.frame(d13C=d13C, d15N=d15N, d34S=d34S, Lat=Lat,Long=Long)
})

#Only keeps the different lat/long values 
northdata <- data %>% distinct

#Save new combined dataset:

#write.table(Jellydata, "/Users/ksjg1u18/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Seabirds_CEH/EnvironmentalData/ICES_Fish_data/ICES_CPUE_REGIONS/Pipefish/Jellydata.txt", sep="\t")

#dataO<-read.table("/Users/ksjg1u18/Desktop/OneDrive - University of Southampton/PhD/Data_Analysis_R/Seabirds_CEH/EnvironmentalData/ICES_Fish_data/ICES_CPUE_REGIONS/Pipefish/Pipefish_isoscape_data.txt", header=T)
#dataO<-na.omit(dataO)

North_sea_data <- northdata

#Making isoscape Code: 

#Just get the variables you need:
C <- North_sea_data$d13C
N <- North_sea_data$d15N
S <- North_sea_data$d34S
X <- North_sea_data$Long
Y <- North_sea_data$Lat


North <- as.data.frame(cbind(C, N, S, X, Y))
North <- na.omit(North)

#make a spatial data frame 

coordinates(North) <- ~ X + Y
proj4string(North) <- CRS("+proj=longlat +datum=WGS84") 
class(North)
str(North)

#this double checks that we don't have any duplicates at the same lat/long
zerodist(North)

###################### Carbon:----

#Fit a variogram:

v <- variogram(C~1, North) # calculates sample variogram values 
#this is a measure of how much your isotope value changes with distance from each point 
plot(v)
#We have to try and fit a model to this! 
#fit.variogram(v, vgm(c("Exp","Sph","Gau","Lin","Mat","Cir"))) #put in suggested models and the result is the one R suggests - but not always the best! 
#v.fit <- fit.variogram(v, vgm(psill=0.68,model ="Lin",range = 0,nugget = 10)) #You can chnage variance aspects of the model or keep it simple 
#v.fit <- fit.variogram(v, vgm(model ="Exp"))
#plot(v, v.fit)

v.fit <- fit.variogram(v, vgm(psill=0.68,model ="Lin",range = 1000,nugget = 0.05)) #You can chnage variance aspects of the model or keep it simple 
plot(v, v.fit)

preds = variogramLine(v.fit, maxdist = max(v$dist))

cairo_ps("Haddock_NorthSea_Carbon_semivariance_viridis.eps", height = 10, width = 15)

ggplot(v, aes(x = dist, y = gamma)) +
  geom_point(colour = "#440154FF", size = 6) +
  geom_line(data = preds, colour = "#440154FF", size = 2) +
  xlab("Distance (km)") +
  ylab("Semivariance") +
  ylim(0, 0.46) +
  ggtitle("Carbon") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(face = "bold", size = 30),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 26))

dev.off()

#set up kriging 
q = 0.1 #grid size 
grd <- expand.grid(x=seq(from=-3.8, to=1.8, by=q), y=seq(from=57.7, to=61.7, by=q) ) #dimensions you want to predict into
coordinates(grd) <- ~ x + y
proj4string(grd)<-CRS("+proj=longlat +datum=WGS84") 
C_Kriged <- krige(C ~ 1, North, grd, model=v.fit)

#Extract data - convert to spatial pixel data frame - then to raster 
q=0.1 
grd <- expand.grid(x=seq(from=-3.8, to=1.8, by=q), y=seq(from=57.7, to=61.7, by=q) )

C.pred <- C_Kriged$var1.pred #prediction values
C.var <- C_Kriged$var1.var #variance values

C.values <- cbind(grd, C.pred)
C.var.values <- cbind(grd, C.var)

#Turning into spatial dataframe - repeat with C.var values to get variance one
coordinates(C.values) <- ~ x+y
proj4string(C.values)<-CRS("+proj=longlat +datum=WGS84") 
gridded(C.values) <- TRUE
raster_C <- raster(C.values)
raster_C
plot(raster_C)

#you can then just overlap a map - but if you want to assign to it - you dont really want "data" where you have land
#So the next step is masking off the land areas:

###  READ LAND SHAPE FILE

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape/land-10m")

# To view shapefile information:
# 'land_10m' is name of the shapefile 
# '.' relates to the location of the shapefile i.e. the working directory (doesn't need changing)
ogrInfo(".", "land-10m") 

# To read in shapefile as a spatial dataframe object when files are within a folder that is set as the working directory:
# specify the shapefile name without a suffix
shpf <- readOGR(".", "land-10m") 
# note that readOGR will read the .prj file (spatial projection) if it exists
print(proj4string(shpf))

## Simple conversion of spatial dataframe to raster using the same extent ##

raster <- raster(extent(shpf)) 
# set the x/y resolution (in spatial units) of raster layer (e.g. x = 1 deg, y= 0.5 deg)
res(raster) <- c(0.1,0.1) 
land_raster <- rasterize(shpf, raster) #takes a while!
# crop raster to desired extent
land_raster_crop <- crop(land_raster, extent(-3.8, 1.8, 57.7, 61.7))

###  mask off land
land_raster_crop2 <- resample(land_raster_crop, raster_C, resample = 'bilinear')
carbon_raster <- mask(raster_C, land_raster_crop2, inverse=TRUE)
plot(carbon_raster)


library(RColorBrewer)
plot(carbon_raster, col=terrain.colors(30)) #can change this to anything or make your own colour scheme :) 
#contour(C2,nlevels=20,add=TRUE,labels=NULL,col="black",lwd=1,labcex=0.7,labcol="black")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="white", fill=TRUE, add =TRUE)


#make your own colour palette:
#colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))

#To make blue palette
#colfunc<-colorRampPalette(c("white", "navy"))

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape")

jpeg("Carbon_isoscape_viridis_haddock_v2.jpeg")

carbon_isoscape <- plot(carbon_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Carbon isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(carbon_isoscape)

dev.off()

### For carbon variance

coordinates(C.var.values) <- ~ x+y
proj4string(C.var.values)<-CRS("+proj=longlat +datum=WGS84") 
gridded(C.var.values) <- TRUE
raster_C_var <- raster(C.var.values)
raster_C_var
plot(raster_C_var)

carbon_raster_var <- mask(raster_C_var, land_raster_crop2, inverse=TRUE)
plot(carbon_raster_var)

jpeg("Carbon_variance_viridis_haddock.jpeg")

carbon_variance <- plot(carbon_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Carbon variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(carbon_variance)

dev.off()


###  write raster files
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape")

writeRaster(carbon_raster, "Carbon_isoscape_NorthSea_haddock", overwrite=TRUE)
writeRaster(carbon_raster_var, "Carbon_variance_NorthSea_haddock", overwrite=TRUE)


################################### Nitrogen:----

#Fit a variogram:

v <- variogram(N~1, North) # calculates sample variogram values 
#this is a measure of how much your isotope value chnages with distance from each point 
plot(v)
#We have to try and fit a model to this! 
fit.variogram(v, vgm(c("Exp","Sph","Gau","Lin","Mat","Cir"))) #put in suggested models and the result is the one R suggests - but not always the best! 
v.fit <- fit.variogram(v, vgm(psill = 0.68, model = "Lin", range = 0, nugget = 10)) #You can chnage variance aspects of the model or keep it simple 
#v.fit <- fit.variogram(v, vgm(model ="Nug"))
plot(v, v.fit)

preds = variogramLine(v.fit, maxdist = max(v$dist))

cairo_ps("Haddock_NorthSea_Nitrogen_semivariance_viridis.eps", height = 10, width = 15)

ggplot(v, aes(x = dist, y = gamma)) +
  geom_point(colour = "#21908CFF", size = 6) +
  geom_line(data = preds, colour = "#21908CFF", size = 2) +
  xlab("Distance (km)") +
  ylab("Semivariance") +
  ylim(0, 0.46) +
  ggtitle("Nitrogen") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(face = "bold", size = 30),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 26))

dev.off()

#set up kriging 
q = 0.1 #grid size 
grd <- expand.grid(x=seq(from=-3.8, to=1.8, by=q), y=seq(from=57.7, to=61.7, by=q) ) #dimensions you want to predict into
coordinates(grd) <- ~ x + y
proj4string(grd)<-CRS("+proj=longlat +datum=WGS84") 
N_Kriged <- krige(N ~ 1, North, grd, model=v.fit)

#Extract data - convert to spatial pixel data frame - then to raster 
q=0.1 
grd <- expand.grid(x=seq(from=-3.8, to=1.8, by=q), y=seq(from=57.7, to=61.7, by=q) )

N.pred <- N_Kriged$var1.pred #prediction values
N.var <- N_Kriged$var1.var #variance values

N.values <- cbind(grd, N.pred)
N.var.values <- cbind(grd, N.var)

#Turning into spatial dataframe - repeat with C.var values to get variance one
coordinates(N.values) <- ~ x+y
proj4string(N.values) <- CRS("+proj=longlat +datum=WGS84") 
gridded(N.values) <- TRUE
raster_N <- raster(N.values)
raster_N
plot(raster_N)

#you can then just overlap a map - but if you want to assign to it - you dont really want "data" where you have land
#So the next step is masking off the land areas:


#### Use land raster from earlier (in carbon section) to mask isoscape
nitrogen_raster <- mask(raster_N, land_raster_crop2, inverse=TRUE)
plot(nitrogen_raster)

library(RColorBrewer)
plot(nitrogen_raster, col=terrain.colors(30)) #can change this to anything or make your own colour scheme :) 
#contour(C2,nlevels=20,add=TRUE,labels=NULL,col="black",lwd=1,labcex=0.7,labcol="black")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="white", fill=TRUE, add =TRUE)

#make your own colour palette:
#colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))
#colfunc(100)

jpeg("Nitrogen_isoscape_viridis_haddock.jpeg")

nitrogen_isoscape <- plot(nitrogen_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Nitrogen isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(nitrogen_isoscape)

dev.off()

### For nitrogen variance

coordinates(N.var.values) <- ~ x+y
proj4string(N.var.values)<-CRS("+proj=longlat +datum=WGS84") 
gridded(N.var.values) <- TRUE
raster_N_var <- raster(N.var.values)
raster_N_var
plot(raster_N_var)

nitrogen_raster_var <- mask(raster_N_var, land_raster_crop2, inverse=TRUE)
plot(nitrogen_raster_var)


jpeg("Nitrogen_variance_viridis_haddock.jpeg")

nitrogen_variance <- plot(nitrogen_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Nitrogen variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(nitrogen_variance)

dev.off()

###  write raster files
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape")

writeRaster(nitrogen_raster, "Nitrogen_isoscape_NorthSea_haddock", overwrite=TRUE)
writeRaster(nitrogen_raster_var, "Nitrogen_variance_NorthSea_haddock", overwrite=TRUE)

################################### Sulfur:----

#Fit a variogram:

v <- variogram(S~1, North) # calculates sample variogram values 
#this is a measure of how much your isotope value chnages with distance from each point 
plot(v)
#We have to try and fit a model to this! 
#fit.variogram(v, vgm(c("Exp","Sph","Gau","Lin","Mat","Cir"))) #put in suggested models and the result is the one R suggests - but not always the best! 
v.fit <- fit.variogram(v, vgm(psill=0.68,model ="Exp",range = 5,nugget = 10)) #You can chnage variance aspects of the model or keep it simple 
#v.fit <- fit.variogram(v, vgm(model ="Lin"))
plot(v, v.fit)

#OR
v.fit <- fit.variogram(v, vgm(psill=0.9,model ="Lin",range = 980,nugget = 0.18)) #You can chnage variance aspects of the model or keep it simple 
plot(v, v.fit)

preds = variogramLine(v.fit, maxdist = max(v$dist))

cairo_ps("Haddock_NorthSea_Sulfur_semivariance_viridis.eps", height = 10, width = 15)

ggplot(v, aes(x = dist, y = gamma)) +
  geom_point(colour = "#D0C018", size = 6) +
  geom_line(data = preds, colour = "#D0C018", size = 2) +
  xlab("Distance (km)") +
  ylab("Semivariance") +
  ylim(0, 0.46) +
  ggtitle("Sulfur") +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(face = "bold", size = 30),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 26))

dev.off()


#set up kriging 
q = 0.1 #grid size 
grd <- expand.grid(x=seq(from=-3.8, to=1.8, by=q), y=seq(from=57.7, to=61.7, by=q) ) #dimensions you want to predict into
coordinates(grd) <- ~ x + y
proj4string(grd)<-CRS("+proj=longlat +datum=WGS84") 
S_Kriged <- krige(S ~ 1, North, grd, model=v.fit)

#Extract data - convert to spatial pixel data frame - then to raster 
q=0.1 
grd <- expand.grid(x=seq(from=-3.8, to=1.8, by=q), y=seq(from=57.7, to=61.7, by=q) )

S.pred <- S_Kriged$var1.pred #prediction values
S.var <- S_Kriged$var1.var #variance values

S.values <- cbind(grd, S.pred)
S.var.values <- cbind(grd, S.var)

#Turning into spatial dataframe - repeat with C.var values to get variance one
coordinates(S.values) <- ~ x+y
proj4string(S.values) <- CRS("+proj=longlat +datum=WGS84") 
gridded(S.values) <- TRUE
raster_S <- raster(S.values)
raster_S
plot(raster_S)

#you can then just overlap a map - but if you want to assign to it - you dont really want "data" where you have land
#So the next step is masking off the land areas:


#### Use land raster from earlier (in carbon section) to mask isoscape
sulfur_raster <- mask(raster_S, land_raster_crop2, inverse=TRUE)
plot(sulfur_raster)

library(RColorBrewer)
plot(sulfur_raster, col=terrain.colors(30)) #can change this to anything or make your own colour scheme :) 
#contour(C2,nlevels=20,add=TRUE,labels=NULL,col="black",lwd=1,labcex=0.7,labcol="black")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="white", fill=TRUE, add =TRUE)

#make your own colour palette:
#colfunc<-colorRampPalette(c("#a50026","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695"))
#colfunc(100)

jpeg("Sulfur_isoscape_viridis_haddock_v2.jpeg")

sulfur_isoscape <- plot(sulfur_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Sulfur isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(sulfur_isoscape)

dev.off()

### For sulfur variance

coordinates(S.var.values) <- ~ x+y
proj4string(S.var.values)<-CRS("+proj=longlat +datum=WGS84") 
gridded(S.var.values) <- TRUE
raster_S_var <- raster(S.var.values)
raster_S_var
plot(raster_S_var)

sulfur_raster_var <- mask(raster_S_var, land_raster_crop2, inverse=TRUE)
plot(sulfur_raster_var)


jpeg("Sulfur_variance_viridis_haddock_v2.jpeg")

sulfur_variance <- plot(sulfur_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Sulfur variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(sulfur_variance)

dev.off()

###  write raster files
setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape")

writeRaster(sulfur_raster, "Sulfur_isoscape_NorthSea_haddock", overwrite=TRUE)
writeRaster(sulfur_raster_var, "Sulfur_variance_NorthSea_haddock", overwrite=TRUE)

#### To plot isoscapes and variances in one figure

## Carbon isoscape and variance in one figure

jpeg("Carbon_isoscape&variance_viridis_haddock.jpeg", height=15, width=25, units=c("cm"), res=600)

carbon <- par(mfrow = c(1,2), oma = c(0,0,3,3))

#cairo_ps("Carbon_isoscape_viridis.jpeg", height = 7, width = 7)

#Carbon isoscape
plot(carbon_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Carbon isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)


#Carbon variance
plot(carbon_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Carbon variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(carbon)

dev.off()


## Nitrogen isoscape and variance in one figure

jpeg("Nitrogen_isoscape&variance_viridis_haddock.jpeg", height=15, width=25, units=c("cm"), res=600)

nitrogen <- par(mfrow = c(1,2), oma = c(0,0,3,3))

#Nitrogen isoscape
plot(nitrogen_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Nitrogen isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

#Nitrogen variance
plot(nitrogen_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Nitrogen variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(nitrogen)

dev.off()


## Sulfur isoscape and variance in one figure

jpeg("Sulfur_isoscape&variance_viridis_haddock.jpeg", height=15, width=25, units=c("cm"), res=600)

sulfur <- par(mfrow = c(1,2), oma = c(0,0,3,3))

#Sulfur isoscape
plot(sulfur_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Sulfur isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

#Sulfur variance
plot(sulfur_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Sulfur variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(sulfur)

dev.off()

### To plot all isoscapes and variances in one figure

setwd("C:/Users/jw17g17/OneDrive - University of Southampton/PhD/Data/Whitefish/North sea isoscape")

jpeg("All_isoscape&variance_viridis_haddock_v3.jpeg", height=25, width=17, units=c("cm"), res=600)

all <- par(mfrow = c(3,2), mar = c(1.5,1,1.5,0))

#Carbon isoscape
plot(carbon_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Carbon isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

#Carbon variance
plot(carbon_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Carbon variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

## Nitrogen isoscape and variance in one figure

#Nitrogen isoscape
plot(nitrogen_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Nitrogen isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

#Nitrogen variance
plot(nitrogen_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Nitrogen variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

## Sulfur isoscape and variance in one figure

#Sulfur isoscape
plot(sulfur_raster, col=viridis(100), axes=F, box=F, useRaster=T, main = "Sulfur isoscape")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

#Sulfur variance
plot(sulfur_raster_var, col=viridis(100), axes=F, box=F, useRaster=T, main = "Sulfur variance")
points(North, pch=16, cex=1, col="black")
map('worldHires',xlim=c(-3.8, 1.8),ylim=c(57.7, 61.7), col="grey91", fill=TRUE, add =TRUE)

print(all)

dev.off()


#click(nitrogen_variance)
