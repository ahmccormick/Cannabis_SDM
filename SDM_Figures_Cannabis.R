library(raster)
library(ggplot2)
library(rgdal)

##########################################
#Figure 1
##########################################

###############
#Overlays SOIL + WORLDCLIM
###############

species_distribution <- raster("~annamccormick/R/species_distribution/results/mean_soil_AND_worldclim.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "WorldClim and ISRIC soil") +
  theme_minimal()

ggsave("worldclim_and_soil_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


#SD error - SOIL + WORLDCLIM
species_distribution <- raster("~annamccormick/R/species_distribution/results/sd_mean_soil_AND_worldclim.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "WorldClim and ISRIC soil (sd)") +
  theme_minimal()

ggsave("SD_worldclim_and_soil_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


##########################################
#Figure 2
##########################################
###############
#FUTURE - 2050 and 2070 - RCP45
###############

#1
species_distribution <- raster("~annamccormick/R/species_distribution/results/mean_RCP45_2050.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP45 2050") +
  theme_minimal()

ggsave("RCP45_2050_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


#SD error
species_distribution <- raster("~annamccormick/R/species_distribution/results/sd_RCP45_2050.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP45 2050 (sd)") +
  theme_minimal()

ggsave("sd_RCP45_2050_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")

#2
species_distribution <- raster("~annamccormick/R/species_distribution/results/mean_RCP45_2070.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP45 2070") +
  theme_minimal()

ggsave("RCP45_2070_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


#SD error
species_distribution <- raster("~annamccormick/R/species_distribution/results/sd_RCP45_2070.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP45 2070 (sd)") +
  theme_minimal()

ggsave("sd_RCP45_2070_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")

##########################################
#Figure 3
##########################################
###############
#FUTURE - 2050 and 2070 - RCP85
###############
#3
species_distribution <- raster("~annamccormick/R/species_distribution/results/mean_RCP85_2050.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP85 2050") +
  theme_minimal()

ggsave("RCP85_2050_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


#SD error
species_distribution <- raster("~annamccormick/R/species_distribution/results/sd_RCP85_2050.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP85 2050 (sd)") +
  theme_minimal()

ggsave("sd_RCP85_2050_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


#4
species_distribution <- raster("~annamccormick/R/species_distribution/results/mean_RCP85_2070.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP85 2070") +
  theme_minimal()

ggsave("RCP85_2070_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


#SD error
species_distribution <- raster("~annamccormick/R/species_distribution/results/sd_RCP85_2070.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "RCP85 2070 (sd)") +
  theme_minimal()

ggsave("sd_RCP85_2070_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")




##########################################
#Figure 4
##########################################

#ON UH HPC
############
#Figure 4A - WORLD 
############
library(raster)
library(rworldmap)

# Start PDF device driver to save output to a file
pdf("overlay_plot.pdf", width=11, height=8.5)

# Load your rasters
r1 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/Maxent_output_BIO_variables.tif")  
ban_2050 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/mean_RCP85_2050.tif")
ban_2070 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/mean_RCP85_2070.tif")

# Create a mask for values greater than 0.2
s <- calc(r1, fun=function(x){ x[x > 0.2] <- 1; x[x <= 0.2] <- NA; return(x)} )
s1 <- calc(ban_2050, fun=function(x){ x[x > 0.2] <- 2; x[x <= 0.2] <- NA; return(x)} )
s2 <- calc(ban_2070, fun=function(x){ x[x > 0.2] <- 3; x[x <= 0.2] <- NA; return(x)} )

# Prepare a map to plot on
world_map <- getMap(resolution = "low")
plot(world_map, col="#f2f2f2", border="#a0a0a0", lwd=0.5, main="Overlay of Raster Datasets")

# Define colors and add transparency
colors <- c("#00FF7F44", "#1E90FF44", "#B2222244")  # RGBA values

# Plot the masked rasters on the world map
plot(s, col=colors[1], legend =F, add=T)
plot(s1, col=colors[2], legend =F, add=T)
plot(s2, col=colors[3], legend =F, add=T)

# Add a custom legend
legend("topright", legend=c("Present", "2050", "2070"), fill=colors, bty="n")

# Turn off the device driver (to close the PDF file)
dev.off()

############
#Figure 4B - USA CROP
############
library(raster)
library(rworldmap)
library(sf)  # For reading shapefiles

# Start PDF device driver to save output to a file
pdf("overlay_plot_us_only_2.pdf", width=11, height=8.5)

# Load the shapefile using sf
shape <- st_read("~/kantar_koastore/anna/SDM/SDM_2024/shapefiles/states_21basic/states.shp")

# Assuming your shapefile has an attribute 'country' that specifies the country
# Filter to include only USA polygons (you need to check the correct attribute name and value)
#usa_shape <- shape[shape$country == "USA", ]

# Convert the filtered shapefile to a Spatial object for compatibility with raster functions
usa_shape_sp <- as(shape, "Spatial")

# Load your rasters
r1 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/Maxent_output_BIO_variables.tif")
ban_2050 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/mean_RCP85_2050.tif")
ban_2070 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/mean_RCP85_2070.tif")

# Crop and mask rasters to the extent of the USA-only shapefile
r1_usa <- mask(crop(r1, extent(usa_shape_sp)), usa_shape_sp)
ban_2050_usa <- mask(crop(ban_2050, extent(usa_shape_sp)), usa_shape_sp)
ban_2070_usa <- mask(crop(ban_2070, extent(usa_shape_sp)), usa_shape_sp)

# Create a mask for values greater than 0.2
s <- calc(r1_usa, fun=function(x){ x[x > 0.2] <- 1; x[x <= 0.2] <- NA; return(x)} )
s1 <- calc(ban_2050_usa, fun=function(x){ x[x > 0.2] <- 2; x[x <= 0.2] <- NA; return(x)} )
s2 <- calc(ban_2070_usa, fun=function(x){ x[x > 0.2] <- 3; x[x <= 0.2] <- NA; return(x)} )

# Get the bounding box of the USA shapefile
bb <- st_bbox(shape)

# Prepare a map to plot on
plot(usa_shape_sp, col="#f2f2f2", border="#a0a0a0", xlim=c(bb["xmin"], bb["xmax"]), ylim=c(bb["ymin"], bb["ymax"]), main="Overlay of Raster Datasets")

# Define colors and add transparency
colors <- c("#00FF7F44", "#1E90FF44", "#B2222244")  # RGBA values

# Plot the masked rasters on the map
plot(s, col=colors[1], legend=F, add=T)
plot(s1, col=colors[2], legend=F, add=T)
plot(s2, col=colors[3], legend=F, add=T)

# Add a custom legend
legend("topright", legend=c("Present", "2050", "2070"), fill=colors, bty="n")

# Turn off the device driver (to close the PDF file)
dev.off()

############
#Figure 4C - California CROP
############
# Start PDF device driver to save output to a file
pdf("overlay_plot_california.pdf", width=11, height=8.5)

# Load the shapefile using sf
shape <- st_read("~/kantar_koastore/anna/SDM/SDM_2024/shapefiles/states_21basic/states.shp")

# Filter to include only California (adjust 'NAME' to the actual attribute name)
california_shape <- shape[shape$STATE_NAME == "California", ]

counties <- st_read("~/kantar_koastore/anna/SDM/SDM_2024/shapefiles/CA_counties.shp")

# Check if the subsetting returned data for California
if (nrow(california_shape) == 0) {
  stop("No features match the subsetting condition. Check the attribute name and value.")
} else {
  print("Successfully subsetted for California.")
}


# Convert the filtered shapefile to a Spatial object for compatibility with raster functions
california_shape_sp <- as(california_shape, "Spatial")

# Load your rasters
r1 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/Maxent_output_BIO_variables.tif")
ban_2050 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/mean_RCP85_2050.tif")
ban_2070 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/mean_RCP85_2070.tif")

# Crop and mask rasters to the extent of California
r1_california <- mask(crop(r1, extent(california_shape_sp)), california_shape_sp)
ban_2050_california <- mask(crop(ban_2050, extent(california_shape_sp)), california_shape_sp)
ban_2070_california <- mask(crop(ban_2070, extent(california_shape_sp)), california_shape_sp)

# Create a mask for values greater than 0.2
s_california <- calc(r1_california, fun=function(x){ x[x > 0.2] <- 1; x[x <= 0.2] <- NA; return(x)} )
s1_california <- calc(ban_2050_california, fun=function(x){ x[x > 0.2] <- 2; x[x <= 0.2] <- NA; return(x)} )
s2_california <- calc(ban_2070_california, fun=function(x){ x[x > 0.2] <- 3; x[x <= 0.2] <- NA; return(x)} )

# Get the bounding box of the California shapefile
bb_california <- st_bbox(california_shape)

# Prepare a map to plot on
plot(california_shape_sp, col="#f2f2f2", border="#a0a0a0", xlim=c(bb_california["xmin"], bb_california["xmax"]), ylim=c(bb_california["ymin"], bb_california["ymax"]), main="Overlay of Raster Datasets for California")

# Define colors and add transparency
colors <- c("#00FF7F44", "#1E90FF44", "#B2222244")  # RGBA values


# Plot the masked rasters on the map
plot(s_california, col=colors[1], legend=F, add=T)
plot(s1_california, col=colors[2], legend=F, add=T)
plot(s2_california, col=colors[3], legend=F, add=T)

# Overlay the California counties boundaries
plot(st_geometry(counties), add = TRUE, border = "darkgray", lwd = 0.5)

# Add a custom legend
legend("topright", legend=c("Present", "2050", "2070"), fill=colors, bty="n")

# Turn off the device driver (to close the PDF file)
dev.off()

############
#Figure 4D - California CROP
############
library(sf)
library(ggplot2)

# Load the shapefile using sf
soil_cal <- st_read("~annamccormick/R/Soil_SDM/data0/Cal_STATSGO2.shp")

# Convert soil order into a factor
soil_cal$order_ <- as.factor(soil_cal$order_)

# Define the color palette for each soil order
soil_palette <- c("Mollisols" = "red", "Inceptisols" = "orange", "Ultisols" = "blue", 
                  "Alfisols" = "yellow", "Vertisols" = "green", "Andisols" = "pink", 
                  "Entisols" = "black", "Aridisols" = "grey", "Histosols" = "white")

# Plot using ggplot
ggplot(data = soil_cal) +
  geom_sf(aes(fill = order_), color = NA) + # Remove border color with color = NA
  scale_fill_manual(values = soil_palette) +
  labs(title = "Plot 5", fill = "Soil Order") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) # Additional line to remove background lines

# Save the plot
ggsave("soil_CA_nobackground.pdf", plot = last_plot(), device = "pdf", width = 11, height = 8.5)

############################################################################################################################

##########################################
#Supplemental Figure 1
##########################################

###############
# WorldClim and Soil 
###############

#Supplemental Figure 1A
#worldclim
species_distribution <- raster("~annamccormick/R/species_distribution/results/Maxent_output_BIO_variables.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p1<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "WorldClim Bioclimatic variables ") +
  theme_minimal()

ggsave("BIO_variables_heatmap.pdf", plot = p1, width = 12, height = 5.39, device = "pdf")

#Supplemental Figure 1B
#soils
species_distribution <- raster("~annamccormick/R/species_distribution/results/Maxent_output_Soil_variables.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p1<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "ISRIC world soil") +
  theme_minimal()

p1
#ggsave("Soil_variables_heatmap.pdf", plot = p1, width = 10.3, height = 5.39, device = "pdf")
#ggsave("Soil_variables_heatmap2.pdf", plot = p1, width = 10.78, height = 5.39, device = "pdf")
ggsave("Soil_variables_heatmap2.pdf", plot = p1, width = 12, height = 5.39, device = "pdf")

#Supplemental Figure 1F
#Elevation 
###############
species_distribution <- raster("~annamccormick/R/species_distribution/results/elevation_30s.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p1<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Elevation 30s") +
  theme_minimal()

ggsave("elevation_30s_heatmap.pdf", plot = p1, width = 10.3, height = 5.39, device = "pdf")

#Supplemental Figure 1D
#Wind Speed
###############
species_distribution <- raster("~annamccormick/R/species_distribution/results/wind_30s.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p2<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Wind Speed (m/s) 30s") +
  theme_minimal()

ggsave("wind_30s_heatmap.pdf", plot = p2, width = 10.3, height = 5.39, device = "pdf")

#Supplemental Figure 1C
#Solar Radiation 
###############
species_distribution <- raster("~annamccormick/R/species_distribution/results/solar_radiation_30s.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p3<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Solar radiation (kJ/m2/day) 30s") +
  theme_minimal()

ggsave("solar_radiation_30s_heatmap.pdf", plot = p3, width = 10.3, height = 5.39, device = "pdf")

#Supplemental Figure 1E
#Water vapor pressure
###############
species_distribution <- raster("~annamccormick/R/species_distribution/results/vapr_30s.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Water vapor pressure (kPa) 30s") +
  theme_minimal()

ggsave("vapr_30s_heatmap.pdf", plot = p4, width = 10.3, height = 5.39, device = "pdf")


##########################################
#Supplemental Figure 2
##########################################
#VAR plots pdf

##########################################
#Supplemental Figure 3
##########################################
#AUC plots pdf


##########################################
#Supplemental Figure 4
##########################################
###############
#Overlays - all six
###############

#re-import rasters 
p1 <- raster ("~annamccormick/R/species_distribution/results/Maxent_output_BIO_variables.tif")
p2 <- raster ("~annamccormick/R/species_distribution/results/Maxent_output_Soil_variables.tif")
p3 <- raster ("~annamccormick/R/species_distribution/results/vapr_30s.tif")
p4 <- raster ("~annamccormick/R/species_distribution/results/wind_30s.tif")
p5 <- raster ("~annamccormick/R/species_distribution/results/solar_radiation_30s.tif")
p6 <- raster ("~annamccormick/R/species_distribution/results/elevation_30s.tif")

print(extent(p1))
print(resolution(p1))

print(extent(p2))
print(resolution(p2))

print(extent(p3))
print(resolution(p3))

print(extent(p4))
print(resolution(p4))

print(extent(p5))
print(resolution(p5))

print(extent(p5))
print(resolution(p5))

library(raster)

# Assuming p3's extent and resolution are the desired ones
desired_extent <- extent(p3)
desired_res <- res(p3)

# Align the extent and resolution of all rasters to match p3's
p1_aligned <- resample(extend(p1, desired_extent), p3)
p2_aligned <- resample(extend(p2, desired_extent), p3)
#p3_aligned <- resample(extend(p3, desired_extent), p3)
#p4_aligned <- resample(extend(p4, desired_extent), p3)
#p5_aligned <- resample(extend(p5, desired_extent), p3)
#p6_aligned <- resample(extend(p6, desired_extent), p3)
# p3, p4, p5, p6 are already aligned

# Now, attempt to stack the aligned rasters
models <- stack(p1_aligned, p2_aligned, p3, p4, p5, p6)

# Attempt to stack the cropped rasters
models <- stack(p1_cropped, p2_cropped, p3_cropped, p4_cropped, p5_cropped, p6_cropped)

m <- mean(models)
writeRaster(m,'~annamccormick/R/species_distribution/results/mean_all_6_merged.tif',options=c('TFW=YES'))

#SD error
msd <- overlay(models,  fun = sd, na.rm = TRUE) 
#plot(msd)
writeRaster(msd,'~annamccormick/R/species_distribution/results/sd_mean_all_6_merged.tif',options=c('TFW=YES'))

######## plot
species_distribution <- raster("~annamccormick/R/species_distribution/results/mean_all_6_merged.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "All six") +
  theme_minimal()

ggsave("mean_all_six_presentday_heatmap_3.pdf", plot = p4, width = 8.5, height = 3.82, device = "pdf", limitsize = FALSE)

#worldwide overlay + SD
species_distribution <- raster("~annamccormick/R/species_distribution/results/sd_mean_all_6_merged.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "All six") +
  theme_minimal()

ggsave("SD_mean_all_six_presentday_heatmap_4.pdf", plot = p4, width = 8.5, height = 3.82, device = "pdf", limitsize = FALSE)


##########################################
#Supplemental Figure 5  - ASIA crop
##########################################
library(sf)
library(raster)

# Load the shapefile using sf package
shape <- st_read("/Users/annamccormick/R/species_distribution/shapefiles/Asia_Russia.shp")

# Convert the sf object to Spatial object if needed
asia_shape_sp <- as(shape, "Spatial")

# Load the raster file
r1 <- raster("/Users/annamccormick/R/species_distribution/results/Maxent_output_BIO_variables.tif")

# Crop and mask the raster to the extent of the shapefile
r1_asia <- crop(r1, extent(asia_shape_sp))
r1_asia_masked <- mask(r1_asia, asia_shape_sp)

species_distribution <- raster("/Users/annamccormick/R/species_distribution/results/Maxent_output_BIO_variables_Asia_Russia.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(0, 170), ylim = c(0, 80))

p4
ggsave("Asia_redo_present.pdf", plot = p4, width = 12, height = 7, device = "pdf")


#############imported workspace for plotting remainer
library(sf)
library(ggplot2)
library(raster)
library(ggplot2)
library(viridis)  # For viridis color scales

load("/Users/annamccormick/R/species_distribution/workspaces/asia_2050_rcp45_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp45 2050") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(0, 170), ylim = c(0, 80))

p4
ggsave("Asia_redo_rcp45_2050.pdf", plot = p4, width = 12, height = 7, device = "pdf")

load("/Users/annamccormick/R/species_distribution/workspaces/asia_2070_rcp45_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp45 2070") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(0, 170), ylim = c(0, 80))

p4
ggsave("Asia_redo_rcp45_2070.pdf", plot = p4, width = 12, height = 7, device = "pdf")

load("/Users/annamccormick/R/species_distribution/workspaces/asia_2050_rcp85_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp85 2050") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(0, 170), ylim = c(0, 80))

p4
ggsave("Asia_redo_rcp85_2050.pdf", plot = p4, width = 12, height = 7, device = "pdf")

load("/Users/annamccormick/R/species_distribution/workspaces/asia_2070_rcp85_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp85 2070") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(0, 170), ylim = c(0, 80))

p4
ggsave("Asia_redo_rcp85_2070.pdf", plot = p4, width = 12, height = 7, device = "pdf")

##########################################
#Supplemental Figure 6 - EUROPE crop
##########################################

#############imported workspace for plotting Europe
load("/Users/annamccormick/R/species_distribution/workspaces/europe_present_my_workspace.RData")

library(sf)
library(ggplot2)
library(raster)
library(ggplot2)
library(viridis)  # For viridis color scales

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-30, 60), ylim = c(10, 90))

p4

ggsave("Europe_present.pdf", plot = p4, device = "pdf")


#RCP45 2050
load("/Users/annamccormick/R/species_distribution/workspaces/europe_2050_rcp45_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp45 2050") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-30, 60), ylim = c(10, 90))

p4

ggsave("Europe_rcp45_2050.pdf", plot = p4, device = "pdf")


#RCP45 2070
load("/Users/annamccormick/R/species_distribution/workspaces/europe_2070_rcp45_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp45 2070") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-30, 60), ylim = c(10, 90))

p4

ggsave("Europe_rcp45_2070.pdf", plot = p4, device = "pdf")


#RCP85 2050
load("/Users/annamccormick/R/species_distribution/workspaces/europe_2050_rcp85_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp85 2050") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-30, 60), ylim = c(10, 90))

p4

ggsave("Europe_rcp85_2050.pdf", plot = p4, device = "pdf")


#RCP85 2070
load("/Users/annamccormick/R/species_distribution/workspaces/europe_2070_rcp85_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp85 2070") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-30, 60), ylim = c(10, 90))

p4

ggsave("Europe_rcp85_2070.pdf", plot = p4, device = "pdf")




##########################################
#Supplemental Figure 7 - USA crop
##########################################
library(sf)
library(raster)
library(ggplot2)

# Load the shapefile using sf package
shape <- st_read("~/kantar_koastore/anna/SDM/SDM_2024/shapefiles/states.shp")

# Convert the sf object to Spatial object if needed
asia_shape_sp <- as(shape, "Spatial")

# Load the raster file
r1 <- raster("~/kantar_koastore/anna/SDM/SDM_2024/plots/crop_USA_present/Maxent_output_BIO_variables.tif")

# Crop and mask the raster to the extent of the shapefile
r1_asia <- crop(r1, extent(asia_shape_sp))
r1_asia_masked <- mask(r1_asia, asia_shape_sp)

# Save the cropped and masked raster as a TIFF file
writeRaster(r1_asia_masked, 
            "~/kantar_koastore/anna/SDM/SDM_2024/plots/crop_USA_present/Maxent_output_BIO_variables_USA.tif", 
            format="GTiff")


#plot
species_distribution <- raster("~/kantar_koastore/anna/SDM/SDM_2024/plots/crop_USA_present/Maxent_output_BIO_variables_USA.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4<- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Present") +
  theme_minimal()

ggsave("~/kantar_koastore/anna/SDM/SDM_2024/plots/crop_USA_present/USA_present_heatmap.pdf", plot = p4, width = 12, height = 5.39, device = "pdf")


# Save the workspace to a .RData file
save.image(file="~/kantar_koastore/anna/SDM/SDM_2024/plots/crop_USA_present/my_workspace.RData")


#RAN ON HPC FOR PLOTS EXPORT AS PDF
#############imported workspace for plotting USA
library(sf)
library(ggplot2)
library(raster)
library(ggplot2)
library(viridis)  # For viridis color scales
load("/Users/annamccormick/R/species_distribution/workspaces/usa_present_my_workspace.RData")


p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-180, -70), ylim = c(20, 80))

p4

ggsave("USA_present.pdf", plot = p4, device = "pdf")


#RCP45 2050
load("/Users/annamccormick/R/species_distribution/workspaces/usa_rcp45_2050_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp45 2050") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-180, -70), ylim = c(20, 80))

p4

ggsave("USA_rcp45_2050.pdf", plot = p4, device = "pdf")


#RCP45 2070
load("/Users/annamccormick/R/species_distribution/workspaces/usa_rcp45_2070_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp45 2070") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-180, -70), ylim = c(20, 80))

p4

ggsave("USA_rcp45_2070.pdf", plot = p4, device = "pdf")


#RCP85 2050
load("/Users/annamccormick/R/species_distribution/workspaces/usa_rcp85_2050_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp85 2050") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-180, -70), ylim = c(20, 80))

p4

ggsave("USA_rcp85_2050.pdf", plot = p4, device = "pdf")


#RCP85 2070
load("/Users/annamccormick/R/species_distribution/workspaces/usa_rcp85_2070_my_workspace.RData")
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "rcp85 2070") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-180, -70), ylim = c(20, 80))

p4

ggsave("USA_rcp85_2070.pdf", plot = p4, device = "pdf")




##########################################
#Supplemental Figure 8- By State Plots 
##########################################

######################
##### Crop to CA 
######################
library(raster)
library(sf)  # 
library(sp)

# path to your raster file and shapefile
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"

# Read the raster data
raster_data <- raster(raster_path)

# Read the shapefile
shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

# Subset the shapefile to only include California
# You may need to adjust "name" and "California" based on the actual attribute names and values in your shapefile
california_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'California', ]

# If the CRS doesn't match, reproject California shape to match the raster's CRS
if (st_crs(california_shape) != crs(raster_data)) {
  california_shape <- st_transform(california_shape, crs(raster_data))
}

# Clip the raster by the California shape
clipped_raster <- mask(raster_data, california_shape) #step can be a bit slow


# Plot the clipped raster with specified latitude and longitude bounds for California
plot(clipped_raster, 
     xlim=c(-124, -114.5), 
     ylim=c(32.5, 42), 
     main="Clipped Raster over California")


#path for the output raster
output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_CA_presentday_raster_all6.tif" 
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)

########################################
#Plotting cropped CA with viridis
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_CA_presentday_raster_all6.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) 

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")


#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.5), name = "Species Distribution") +
  labs(title = "California Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-124, -114), ylim = c(32, 42))

p4

ggsave("CA_present_redo.pdf", plot = p4, device = "pdf")


###########
#colorado
###########
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"
raster_data <- raster(raster_path)

shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

colorado_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'Colorado', ]

if (st_crs(colorado_shape) != crs(raster_data)) {
  colorado_shape <- st_transform(colorado_shape, crs(raster_data))
}

clipped_raster <- mask(raster_data, colorado_shape) #step can be a bit slow

plot(clipped_raster)

output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_CO_presentday_raster_all6.tif"  
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)

#PLotting 
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_CO_presentday_raster_all6.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")


#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.5), name = "Species Distribution") +
  labs(title = "Colorado Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-109, -102), ylim = c(37, 41))

p4

ggsave("CO_present_redo.pdf", plot = p4, device = "pdf")


###########
# Maine
###########
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"
raster_data <- raster(raster_path)

shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

maine_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'Maine', ]

if (st_crs(maine_shape) != crs(raster_data)) {
  maine_shape <- st_transform(maine_shape, crs(raster_data))
}

clipped_raster <- mask(raster_data, maine_shape) #step can be a bit slow

plot(clipped_raster)

output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_Maine_presentday_raster_all6.tif" 
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)


#PLotting 
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Maine_presentday_raster_all6.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.8), name = "Species Distribution") +
  labs(title = "Maine Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-71, -66), ylim = c(42, 47.5))

p4

ggsave("Maine_present_redo.pdf", plot = p4, device = "pdf")


###########
# Oregon
###########
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"
raster_data <- raster(raster_path)

shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

oregon_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'Oregon', ]

if (st_crs(oregon_shape) != crs(raster_data)) {
  oregon_shape<- st_transform(oregon_shape, crs(raster_data))
}

clipped_raster <- mask(raster_data, oregon_shape) #step can be a bit slow

plot(clipped_raster)

output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_Oregon_presentday_raster_all6.tif" 
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)


#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.8), name = "Species Distribution") +
  labs(title = "Oregon Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-124.6, -116.5), ylim = c(41, 46.3))

p4

ggsave("Oregon_present_redo.pdf", plot = p4, device = "pdf")


###########
# Washington
###########
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"
raster_data <- raster(raster_path)

shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

washington_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'Washington', ]

if (st_crs(washington_shape) != crs(raster_data)) {
  washington_shape<- st_transform(washington_shape, crs(raster_data))
}

clipped_raster <- mask(raster_data, washington_shape) #step can be a bit slow

plot(clipped_raster)

output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_Washintgon_presentday_raster_all6.tif" 
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)


#PLotting 
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Washintgon_presentday_raster_all6.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.8), name = "Species Distribution") +
  labs(title = "Washington Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-124.8, -116.5), ylim = c(45.5, 49))

p4

ggsave("washington_present_redo.pdf", plot = p4, device = "pdf")

###########
# Massachusetts
###########
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"
raster_data <- raster(raster_path)

shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

Massachusetts_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'Massachusetts', ]

if (st_crs(Massachusetts_shape) != crs(raster_data)) {
  Massachusetts_shape<- st_transform(Massachusetts_shape, crs(raster_data))
}

clipped_raster <- mask(raster_data, Massachusetts_shape) #step can be a bit slow

plot(clipped_raster)

output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_Massachusetts_presentday_raster_all6.tif"  
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)

#PLotting 
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Massachusetts_presentday_raster_all6.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.8), name = "Species Distribution") +
  labs(title = "Massachusetts Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-73.5, -69.9), ylim = c(41.2, 42.9))

p4

ggsave("Massachusetts_present_redo.pdf", plot = p4, device = "pdf")

###########
# Michigan
###########
raster_path <- "~annamccormick/R/species_distribution/results/mean_all_six_presentday_2.tif"
raster_data <- raster(raster_path)

shapefile_path <- "~annamccormick/R/species_distribution/shapefiles/ne_110m_admin_1_states_provinces.shp"
shapes <- st_read(shapefile_path)

Michigan_shape <- shapes[shapes$admin == 'United States of America' & shapes$name == 'Michigan', ]

if (st_crs(Michigan_shape) != crs(raster_data)) {
  Michigan_shape<- st_transform(Michigan_shape, crs(raster_data))
}

clipped_raster <- mask(raster_data, Michigan_shape) #step can be a bit slow

plot(clipped_raster)

output_raster_path <- "~annamccormick/R/species_distribution/results/clipped_Michigan_presentday_raster_all6.tif"  
writeRaster(clipped_raster, filename = output_raster_path, format = "GTiff", overwrite = TRUE)

#PLotting 
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Michigan_presentday_raster_all6.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

#SECOND
p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 0.8), name = "Species Distribution") +
  labs(title = "Michigan Present") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-90.4, -82.4), ylim = c(41.7, 48.2))

p4

ggsave("Michigan_present_redo.pdf", plot = p4, device = "pdf")

######################### PLOTTING WITHOUT EXTENT ISSUES BUT UGLY 
# CALIFORNIA
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_CA_presentday_raster_all6.tif")
plot(species_distribution, 
     xlim=c(-124.5, -114),  # Longitude limits 
     ylim=c(32.5, 42),     # Latitude limits
     main="California Present")


# COLORADO
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_CO_presentday_raster_all6.tif")
plot(species_distribution, 
     xlim=c(-109, -102),  # Longitude limits 
     ylim=c(37, 41),     # Latitude limits
     main="Colorado Present")

# MAINE
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Maine_presentday_raster_all6.tif")
plot(species_distribution, 
     xlim=c(-71, -66.95),  # Longitude limits for Maine
     ylim=c(43, 47.5),     # Latitude limits for Maine
     main="Maine Present")

# OREGON
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Oregon_presentday_raster_all6.tif")

plot(species_distribution, 
     xlim=c(-124.566, -116.463),  # Longitude limits 
     ylim=c(42.003, 46.292),     # Latitude limits
     main="Oregon Present")

# WASHINGTON
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Washintgon_presentday_raster_all6.tif")

plot(species_distribution, 
     xlim=c(-124.8, -117),  # Longitude limits 
     ylim=c(45.5, 49),     # Latitude limits
     main="Washington Present")

# Massachusetts
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Massachusetts_presentday_raster_all6.tif")

plot(species_distribution, 
     xlim=c(-73.508, -69.928),  # Longitude limits 
     ylim=c(41.238, 42.887),     # Latitude limits
     main="Massachusetts Present")


# Michigan
species_distribution <- raster("~annamccormick/R/species_distribution/results/clipped_Michigan_presentday_raster_all6.tif")
plot(species_distribution, 
     xlim=c(-90.5, -82),  # Longitude limits 
     ylim=c(41.5, 48),     # Latitude limits
     main="Michigan Present")



#########################
#Supplemental Figure 1 - map of datapoints used
#########################

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

library(ggplot2)
library(maps)
library(mapdata)

# Load the world map
world_map <- map_data("world")

# Plotting the world map with 'can' data points
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = can, aes(x = lon, y = lat), color = "blue", size = 1) +
  labs(title = "Cannabis Occurrence Points", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)  # Set aspect ratio to 1 to maintain proportions


#########################
#Supplemental Figure 10 - Paleoclim
#########################
#Paleo - pannel A1
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/m2_2_5min/M2_2_5min_result.tif")

species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Mid Pliocene warm period (ca. 3.2 Ma)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4
ggsave("M2_redo.pdf", plot = p4, device = "pdf")


#Paleo - pannel A
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/mpwp_2_5min/mpwp_2_5min_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Mid Pliocene warm period (ca. 3.2 Ma)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4
ggsave("MPWP_redo.pdf", plot = p4, device = "pdf")

#########
#Paleo - pannel B
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/mis19_2_5min/MIS19_v1_2_5m_result_2.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "MIS19 (ca. 787,000 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("MIS19_redo.pdf", plot = p4, device = "pdf")

#########
#Paleo - pannel C
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/LIG_v1_2_5m/LIG_v1_2_5m_result_2.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Last Interglacial (130,000 years ago); ") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("LIG_redo.pdf", plot = p4, device = "pdf")

#########
#Paleo - pannel D
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/chelsa_LGM_v1_2B_r10m/chelsa_LGM_v1_10m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Last Glacial Maximum (ca. 21,000 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-20, +180), ylim = c(0, 90))

p4

ggsave("LGM_redo.pdf", plot = p4, device = "pdf")

#########
#Paleo - pannel E
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/HS1_v1_2_5m/HS1_v1_2_5m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Heinrich Stadial (14,700 - 17,000 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("HS_redo.pdf", plot = p4, device = "pdf")


#########
#Paleo - pannel F
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/BA_v1_2_5m/BA_v1_2_5m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Bolling-Allerod (12,900 - 14,700 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("BA_redo.pdf", plot = p4, device = "pdf")

#########
#Paleo - pannel G
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/YDS_v1_2_5m/YDS_v1_2_5m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Younger Dryas Stadial (11,700 - 12,900 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("YDS_redo.pdf", plot = p4, device = "pdf")


#########
#Paleo - pannel H
#########
#early-Holocene, Greenlandian (11.7-8.326 ka)
species_distribution <- raster("/Users/annamccormick/R/species_distribution/redo/paleoclim/EH_V1_2_5m/EH_v1_2_5m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Early Holocene, Greenlandian (8,366 - 11,700 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("EH_redo.pdf", plot = p4, device = "pdf")


#########
#Paleo - pannel I
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/MH_v1_2_5m/MH_v1_2_5m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Mid Holocene, Northgrippian (4,200 - 8,326 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("MH_redo.pdf", plot = p4, device = "pdf")

#########
#Paleo - pannel J
#########
species_distribution <- raster("~/R/species_distribution/redo/paleoclim/LH_v1_2_5m/2_LH_v1_2_5m_result.tif")
species_distribution_reduced <- aggregate(species_distribution, fact=5, fun=mean) # 'fact' is the aggregation factor

# Convert to data frame
df_reduced <- as.data.frame(rasterToPoints(species_distribution_reduced))
names(df_reduced) <- c("Longitude", "Latitude", "Value")

p4 <- ggplot(data = df_reduced, aes(x = Longitude, y = Latitude, fill = Value)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 1), name = "Species Distribution") +
  labs(title = "Late Holocene, Meghalayan (300 - 4200 years ago)") +
  theme_minimal() +
  coord_fixed(ratio = 1 / cos(45 * pi / 180), xlim = c(-5, +180), ylim = c(0, 90))

p4

ggsave("LH_redo.pdf", plot = p4, device = "pdf")




###############################
#Supplemental Tables Calculations
###############################
####################################
#Convert pixel count to area
####################################
library(raster)

#################
#WORLDWIDE
#################
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/world_present_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #WORLDWIDE

#2050 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/rcp45_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP45

#2070 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/rcp45_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP45

#2050 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/world_rcp85_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP85

#2070 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/world_rcp85_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP85


#####################
#ASIA/RUSSIA
#####################
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/asia_present_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #WORLDWIDE

#2050 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/asis_rcp45_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP45

#2070 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/asia_rcp45_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP45

#2050 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/asis_rcp85_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP85

#2070 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/asia_rcp45_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP85


#####################
#EUROPE
#####################
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/europe_present_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #WORLDWIDE

#2050 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/europe_rcp45_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP45

#2070 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/europe_rcp45_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP45

#2050 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/europe_rcp85_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP85

#2070 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/europe_rcp85_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP85



#####################
#USA
#####################
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/usa_present_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #WORLDWIDE

#2050 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/usa_rcp45_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP45

#2070 RCP45
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/usa_rcp45_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP45

#2050 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/usa_rcp85_2050_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2050 RCP85

#2070 RCP85
r <- raster("/Users/annamccormick/R/species_distribution/results/python/above_0_4/usa_rcp85_2070_above_threshold.tif")
rr <- reclassify(r, cbind(0, NA))
a <- area(r)
aa <- mask(a, rr)
cellStats(aa, "sum") #2070 RCP85



