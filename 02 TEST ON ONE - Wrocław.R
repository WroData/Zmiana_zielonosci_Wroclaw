# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Materialy pomocnicze ####

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Odlaczeni wszytkich pakietów ####
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywolanie funkcji 

#usuniecie zmiennych
gc(reset = TRUE)
rm(list = ls())

#ustalenie seed
set.seed(1)

# bedy po angileku
Sys.setenv(LANGUAGE='en')

#nie notacja naukowa
options("scipen"=100, "digits"=4)

# dodaj mozliwosc drukowania polsich liter w ggplot2
Sys.setlocale("LC_ALL", "Polish")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Deklaracja bibliotek ####

library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(scales)
library(ggplot2)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Stale ####

MAIN_PATH <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
SHP_PATH <- paste0(MAIN_PATH, "\\SHP")
DATA_PATH <- paste0(MAIN_PATH, "\\20230906 Polska z Sientiela\\dane\\test\\")

border <- read_sf( paste0(SHP_PATH, "\\Wroclaw\\Wroclaw.shp"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Funkcje ####

load_channel <- function(dir, file_name, shp){
  
  jp2_file <- paste0(dir, file_name, ".jp2")
  
  # Open the .jp2 file
  raster_data <- raster(jp2_file)
  
  # Check the CRS of the jp2 raster
  shp <- st_transform(shp, crs = proj4string(raster_data))
  shp <- st_transform(shp, crs = st_crs(raster_data))
  
  # Clip/Crop the Raster
  raster_data_cliped <- raster::crop(raster_data, shp)
  raster_data_cliped <- raster::mask(raster_data_cliped, shp)
  
  return (raster_data_cliped)
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wczytaj dane ###
# load data
sientel_red <- load_channel(
  dir =  DATA_PATH ,
  file_name = "T33UXS_20180513T095031_B04_10m",
  shp = border )

sientel_green <- load_channel(
  dir =  DATA_PATH ,
  file_name = "T33UXS_20180513T095031_B03_10m",
  shp = border )

sientel_blue <- load_channel(
  dir =  DATA_PATH ,
  file_name = "T33UXS_20180513T095031_B02_10m",
  shp = border )

sientel_nir <- load_channel(
  dir = DATA_PATH ,
  file_name = "T33UXS_20180513T095031_B08_10m",
  shp = border )


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wykres RGB ###
# RGB
# Create an empty RGB stack
rgb_stack <- stack()

# Rescale the data in each channel to the 0-255 range manually
sientel_red_rescaled <- (sientel_red - minValue(sientel_red)) / (maxValue(sientel_red) - minValue(sientel_red)) * 255
sientel_green_rescaled <- (sientel_green - minValue(sientel_green)) / (maxValue(sientel_green) - minValue(sientel_green)) * 255
sientel_blue_rescaled <- (sientel_blue - minValue(sientel_blue)) / (maxValue(sientel_blue) - minValue(sientel_blue)) * 255


# Add the Red, Green, and Blue layers to the stack
rgb_stack$red <- sientel_red_rescaled
rgb_stack$green <- sientel_green_rescaled
rgb_stack$blue <- sientel_blue_rescaled


# Plot the RGB image
plotRGB(rgb_stack)
# Add the shapefile outline to the plot
plot(border, add = TRUE, border = "red", lwd = 2)  # Adjust border color and line width as needed


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wykres NDVI ###
# NDVI
ndvi_raster <- (sientel_nir - sientel_red) / (sientel_nir + sientel_red)

# create a copy
ndvi_raster_mask <- ndvi_raster
# Create a mask based on the condition (values below 0.4)
mask <- ndvi_raster < 0.4
# Update values below 0.4 to 0
ndvi_raster_mask[mask] <- 0
# Update values above or equal to 0.4 to 1
ndvi_raster_mask[!mask] <- 1


# Check the CRS of the jp2 raster
border <- st_transform(border, crs = proj4string(ndvi_raster))
border <- st_transform(border, crs = st_crs(ndvi_raster))
# Get the extent of your raster
raster_extent <- extent(ndvi_raster_mask)
# Subset the shapefile to match the extent of the raster
border_subset <-  st_crop(border, raster_extent)


plot(ndvi_raster_mask)
# Add the shapefile outline to the plot
plot(border, add = TRUE, border = "black", lwd = 1, col = NA)  # Adjust border color and line width as needed


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wykres NDVI - ggplot2 ###
# ggplot2 
ndvi_points <- rasterToPoints(ndvi_raster_mask)
# Convert the raster values to a data frame
ndvi_df <- as.data.frame(ndvi_raster_mask, xy = TRUE) %>%
  filter(!is.na(layer))

p <- ggplot() +
  geom_tile(data = ndvi_df, aes(x = x, y = y, fill = factor(layer))) +
  
  geom_sf(data = border, 
               fill=NA,color="grey50", size=1) +
  scale_fill_manual(
    values = c("-1" = "red", "0" = "gray", "1" = "darkgreen"),
    breaks = c(-1, 0, 1),
    labels = c("Zmniejszenie", "Brak drzew", "Zysk")
  ) 
  
p

