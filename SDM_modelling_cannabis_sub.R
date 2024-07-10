library(maptools)  ## For wrld_simpl
library(raster)
data(wrld_simpl)
usa<-map_data("state")
library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
library(leaflet)
library(sp)
library(USAboundaries)
library(mapdata)
library(raster)
library(rgdal)
library(dismo)
library(rspatial)
library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(maps)
library(mapdata)
library(tidyverse)
library(urbnmapr) 
library(broom)
library(viridis) 
library(sp)
library(USAboundaries)
library(mapdata)
library(dismo)
library(raster)
library(maptools)
library(rasterVis)
data(wrld_simpl)
library(readr)



#########################
#Present day 
#########################
#########################
#WorldClim 
#########################
library(dismo)
library(raster)
library(maptools)
library(rasterVis)
data(wrld_simpl)
library(readr)

can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

files <- list.files(path=("~annamccormick/R/Data/climate_data/wc2.0_30s_bio/"), pattern='tif', full.names=TRUE) 
predictors <- stack(files)

# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')


if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}

writeRaster(px,'~/R/species_distribution/redo/bioclim_30s.tif',options=c('TFW=YES'))
xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(px)


#########################
#Present day SOIL
#########################
#Load climate data 
files <- list.files(path=("~annamccormick/R/Data/climate_data/Soil_Data/"), pattern='tif', full.names=TRUE) 
predictors <- stack(files)

# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(px)

plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


###########################
#SOIL AND WORLDCLIM TOGETHER
###########################
files <- list.files(path=("~annamccormick/R/Data/climate_data/soil_and_worldclim/"), pattern='tif', full.names=TRUE) 
predictors <- stack(files)

# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(px)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


#Merge WORLDCLIM + SOIL for Anthropocene
p1 <- raster ("~annamccormick/R/species_distribution/results/Maxent_output_BIO_variables.tif")
p2 <- raster ("~annamccormick/R/species_distribution/results/Maxent_output_Soil_variables.tif")

models <- stack(p1, p2)
plot(models)
m <- mean(models)
plot(m)


#re-import rasters 
p1 <- raster ("~annamccormick/R/species_distribution/results/Maxent_output_BIO_variables.tif")
p2 <- raster ("~annamccormick/R/species_distribution/results/Maxent_output_Soil_variables.tif")

#Combine Models for Mean 
models <- stack(p1, p2)
plot(models)
m <- mean(models)
plot(m)

writeRaster(m,'~annamccormick/R/species_distribution/results/mean_soil_AND_worldclim.tif',options=c('TFW=YES'))

#Return here for SD error
msd <- overlay(models,  fun = sd, na.rm = TRUE) 
plot(msd)
writeRaster(msd,'~annamccormick/R/species_distribution/results/sd_mean_soil_AND_worldclim.tif',options=c('TFW=YES'))


##############################
#Overlap of all environmental 6x properties
##############################
#MEGA VAR CONT
library(dismo)
library(raster)
library(maptools)
library(rasterVis)
library(readr)
library("rgeos")
data(wrld_simpl)
library(sf)

can <-read.csv("~/kantar_koastore/anna/SDM/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)


# Load the shapefile using sf package
shape <- st_read("~annamccormick/R/species_distribution/shapefiles/world-administrative-boundaries.shp")

# Load climate data (all variables) wc2.0_30s_bio
files <- list.files(path=("~annamccormick/R/Data/climate_data/mega_all_data/"), 
                    pattern='tif', full.names=TRUE)

print(files[40])

# Use the first raster as a template for extent and resolution
template_raster <- raster(files[40])
desired_extent <- extent(template_raster)
desired_resolution <- res(template_raster)

# Initialize an empty list to store adjusted rasters
adjusted_rasters <- list()

# Loop over raster files, adjust extent and resolution, and add to the list
for (file in files) {
  r <- raster(file)
  
  # Reproject raster if CRS does not match that of the shapefile
  if (!compareCRS(r, crs(shape))) {
    r <- projectRaster(r, crs=crs(shape))
  }
  
  # Crop to the desired extent
  r_cropped <- crop(r, desired_extent)
  
  # Resample to match the resolution of the template raster
  r_resampled <- resample(r_cropped, template_raster)
  
  adjusted_rasters[[file]] <- r_resampled
}


# Stack all rasters
predictors <- stack(adjusted_rasters)


####
presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}

writeRaster(px,'~/kantar_koastore/anna/SDM/SDM_2024/rerun/all_variables_mega/mega_all_tiffs_30s.tif',options=c('TFW=YES'))
xme <- evaluate(pres_test, backg_test, xm, predictors)


# Plot ROC curve (AUC)
par(mfrow=c(1, 1)) # Resetting to default to ensure the plot is not side by side
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2)

# Now the next plot will be separate
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) # Variable contribution
plot(px)


##############################
#FUTURE CLIMATE
##############################
#similarly to add 


##############################
#PALEOCLIMATE
##############################

##############################
#PALEOCLIM #LH
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="grey") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#Load data (all variables) 2.5mins (5km)
#files <- list.files(path=("~annamccormick/R/Data/climate_data/Soil_Data/"), pattern='tif', full.names=TRUE) 


files <- list.files(path=("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m"), pattern='tif', full.names=TRUE) 
predictors <- stack(files) # doesnt work now??

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LH_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}

#writeRaster(px,'~/R/species_distribution/redo/bioclim_30s.tif',options=c('TFW=YES'))
writeRaster(px,'~/R/species_distribution/redo/paleoclim/LH_v1_2_5m/2_LH_v1_2_5m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution

writeRaster(msd,'~annamccormick/R/species_distribution/results/sd_paleoclim_LH_v1_2_5m_all.tif',options=c('TFW=YES'))


##############################
#PALEOCLIM #MH 
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )

library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/MH_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/MH_v1_2_5m/MH_v1_2_5m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # EH
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/EH_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/EH_v1_2_5m/EH_v1_2_5m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # YDS
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/YDS_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/YDS_v1_2_5m/YDS_v1_2_5m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # BA
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/BA_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/BA_v1_2_5m/BA_v1_2_5m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # HS1
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/HS1_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/HS1_v1_2_5m/HS1_v1_2_5m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # LIG
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/LIG_v1_2_5m/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}

writeRaster(px,'~/R/species_distribution/redo/paleoclim/LIG_v1_2_5m/LIG_v1_2_5m_result_3.tif',options=c('TFW=YES'))
plot(px)
plot(px,xlim=c(-10,160), ylim=c(0,80))
plot(wrld_simpl, add = TRUE)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # LAST INTERGLACIAL
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/chelsa_LGM_v1_2B_r10m/10min/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/chelsa_LGM_v1_2B_r10m/chelsa_LGM_v1_10m_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # Mis 19 
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_1.tif")
bio2 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_2.tif")
bio3 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_3.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_4.tif")
bio5 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_5.tif")
bio6 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_6.tif")
bio7 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_7.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mis19_2_5min/bio_19.tif")

predictors <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}

writeRaster(px,'~/R/species_distribution/redo/paleoclim/mis19_2_5min/MIS19_v1_2_5m_result_2.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # mpwp
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )


library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_1.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_4.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/mpwp_2_5min/bio_19.tif")

predictors <- stack(bio1,bio4,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/mpwp_2_5min/mpwp_2_5min_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


##############################
#PALEOCLIM # m2
##############################
can <-read.csv("~annamccormick/R/Data/climate_data/true Cannabis occurence points.csv",header=T, sep=",")
head(can) 
can <- can[complete.cases(can$lat),]
can <- can[complete.cases(can$lon),]
summary(can)
dups <- duplicated(can[,c("lat", "lon")])
sum(dups)

# keep the records that are _not_ duplicated
can <- can[!dups,]
can <- can[,1:2]
head(can) 

can<-subset(can, lon > 0)
#write.csv(can, "~annamccormick/R/species_distribution/can_occurance_points_137.csv" )

library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE, col="light yellow") 
box()
points(can$lon, can$lat, col='orange', pch=20, cex=0.75)

#INDIVIDUAL BIOCLIM LAYERS IMPORT
bio1 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_1.tif")
bio4 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_4.tif")
bio8 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_8.tif")
bio9 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_9.tif")
bio10 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_10.tif")
bio11 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_11.tif")
bio12 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_12.tif")
bio13 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_13.tif")
bio14 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_14.tif")
bio15 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_15.tif")
bio16 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_16.tif")
bio17 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_17.tif")
bio18 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_18.tif")
bio19 <- raster("~annamccormick/R/species_distribution/paleoclim_rawdata/m2_2_5min/bio_19.tif")

predictors <- stack(bio1,bio4,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)


# first layer of the RasterStack
plot(predictors, 1) # 
plot(wrld_simpl,add=TRUE)
points(can, col='blue')

presvals <- extract(predictors, can) 
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))   
head(sdmdata)

pred_nf <- dropLayer(predictors, 'biome')
group <- kfold(can, 5)
pres_train <- can[group != 1, ]
pres_test <- can[group == 1, ]

backg <- randomPoints(pred_nf , n=1000,  extf = 1.25)           
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]
r = raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(wrld_simpl, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-', cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

#Maxent
#Variable Contribution 
library(rJava)
install.packages("snow")
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')

if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train)
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

#World Suitability Map
if (file.exists(jar)) {
  response(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
}

if (file.exists(jar)) {
  xme <- evaluate(pres_test, backg_test, xm, predictors)
  px <- predict(predictors, xm, progress='')
  trxm <- threshold(xme, 'spec_sens')
  
  par(mfrow=c(1,2))
  plot(px, main='Maxent, raw values')#xlim=c(-160,-55), ylim=c(0,80), main='Maxent, raw values')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  
  plot(px > trxm, main='presence/absence')#xlim=c(-160,-55), ylim=c(0,80), main='presence/absence')
  plot(wrld_simpl, add=TRUE, border='dark grey')
  points(pres_train, pch='+')
} else {
  plot(1)
}


writeRaster(px,'~/R/species_distribution/redo/paleoclim/m2_2_5min/M2_2_5min_result.tif',options=c('TFW=YES'))
plot(px)

xme <- evaluate(pres_test, backg_test, xm, predictors)
plot(xme, 'ROC', cex.lab = 1, cex.axis =1, cex.main =2) #AUC
plot(xm, cex.lab = 2, cex.axis = 4, cex.main = 2, cex.sub = 3) #Variable contribution
plot(xm, cex.lab = 2, cex.main = 2, cex.sub = 3) #Variable contribution


