
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Opis ####

# Opis:
# Kroki:

# Materialy pomocnicze 
# http://geoprofesja.pl/dane-satelitarne-sentinel-2-w-r/
# https://www.youtube.com/watch?v=GWNa1-obvCE
# in python: https://bikeshbade.com.np/tutorials/Detail/?title=Calculate%20NDVI%20from%20Sentinel-2%20%20with%20Python%20API&code=18

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
library(tidyverse)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Funkcje ####


load_channel <- function(file_name, shp){
  #' Load and Process a Raster Channel
  #'
  #' This function loads a raster channel from a .jp2 file, checks its Coordinate Reference System (CRS),
  #' and clips/crops it based on a provided shapefile.
  #'
  #' @param dir The directory where the .jp2 file is located.
  #' @param file_name The name of the .jp2 file (without the file extension).
  #' @param shp A shapefile (sf object) used to clip/crop the raster channel.
  #'
  #' @return A cropped/clipped raster channel.
  #'
  #' @examples
  #' # Load a raster channel and clip it using a shapefile
  #' sientel_red <- load_channel(
  #'   dir = "path/to/directory/",
  #'   file_name = "T33UXS_20180513T095031_B04_10m",
  #'   shp = border
  #' )
  #'
  #' @import raster
  #' @import sf
  #' @importFrom sf st_transform st_crs
  #' @export
  
  # Open the .jp2 file
  raster_data <- raster(file_name)
  
  # Check the CRS of the jp2 raster
  shp <- st_transform(shp, crs = proj4string(raster_data))
  shp <- st_transform(shp, crs = st_crs(raster_data))
  
  # Clip/Crop the Raster
  raster_data_cliped <- raster::crop(raster_data, shp)
  raster_data_cliped <- raster::mask(raster_data_cliped, shp)
  
  
  return (raster_data_cliped)
}


substrRight <- function(x, n){
  # https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  substr(x, nchar(x)-n+1, nchar(x))
}


calculate_NDIV <- function(dir, file_name_red, file_name_nir){
  
  sientel_red <- load_channel(
    dir =  dir ,
    file_name = file_name_red,
    shp = border )
  
  sientel_nir <- load_channel(
    dir =  dir,
    file_name = file_name_nir,
    shp = border )
  
  # NDVI
  ndvi_raster <- (sientel_nir - sientel_red) / (sientel_nir + sientel_red)
  
}

create_folder_if_not_exists <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created:", folder_path, "\n")
  } else {
    cat("Folder already exists:", folder_path, "\n")
  }
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Stele ####
CITY <- "Wroclaw"
POINT_RADIUS <- 5000
NDVI_TRESHOLD <- 0.4
DATA_PATH <- "C:\\WroData data\\"
MAIN_PATH <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
SHP_PATH <- paste0(MAIN_PATH, "\\SHP")
OUT_PATH <- paste0(
  MAIN_PATH, 
  "\\20230906 Polska z Sientiela\\", CITY, "\\", "CHARTS\\", 
  str_pad(NDVI_TRESHOLD * 10, 2, side = "left", pad = "0"))



create_folder_if_not_exists(paste0(MAIN_PATH, "\\20230906 Polska z Sientiela\\", CITY))
create_folder_if_not_exists(paste0(MAIN_PATH, "\\20230906 Polska z Sientiela\\", CITY, "\\", "CHARTS"))
create_folder_if_not_exists(OUT_PATH)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wczytanie danych ####

# Wroclaw
border <- read_sf(paste0(SHP_PATH, "\\", CITY,  "\\", CITY, ".shp"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Metadane zdjec zatelitarnych ####
# Load the data from an RDS file
# metadata <- readRDS(paste0(DATA_PATH, "images metadata.rds")) %>%
#   filter(month %in% c(5, 6, 7, 8),
#          #clouds < 0.05
#          ) %>%
#   arrange(clouds) %>%
#   group_by(year) %>%
#   filter(row_number()==1)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Okrag analizy ####


DIC_centers = list('Wroclaw' = c(51.10850, 17.0309636), 'Krakow' = c(50.0623969, 19.9386889), 'Warszawa' = c(52.2307014, 21.0095595))
lat  <- DIC_centers[[CITY]][1] #center point latitude
long <- DIC_centers[[CITY]][2] #center point longitude
pt <- data.frame(lat = lat, long = long)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Policz NDIV dla kolejnych lat ####

nvid_across_years <- data.frame()
# wczytaj wszytkie dostepne zdjecia
for (path in list.dirs(paste0(DATA_PATH, CITY, '\\', "dane\\l2a\\"),recursive = FALSE) ){
  # które sa puste, a które pelne
  if(length(list.dirs(path, recursive = FALSE)) > 0){
    d <- substrRight(path, 10)
    print(d)
    
    # find dir with photos
    p <- list.dirs(paste0(DATA_PATH, CITY, '\\', "dane\\l2a\\", d, "\\"))
    satelite_dir <- p[endsWith(p, "R10m")][1]
    
    # file names 
    files <- list.files(satelite_dir, full.names = T, recursive = FALSE)
    file_name_red <- files[endsWith(files, "B04_10m.jp2")]
    file_name_green <- files[endsWith(files, "B03_10m.jp2")]
    file_name_blue <- files[endsWith(files, "B02_10m.jp2")]
    file_name_nir <- files[endsWith(files, "B08_10m.jp2")]
    
    # load photos
    sientel_red <- load_channel(
      file_name = file_name_red,
      shp = border )
    sientel_green <- load_channel(
      file_name = file_name_green,
      shp = border )
    sientel_blue <- load_channel(
      file_name = file_name_blue,
      shp = border )
    sientel_nir <- load_channel(
      file_name = file_name_nir,
      shp = border )
    
    # przygotój okrag
    pt <- st_transform(pt, crs = proj4string(sientel_red))
    pt <- st_transform(pt, crs = st_crs(sientel_red))
    circle <- st_buffer(pt, dist = POINT_RADIUS)
    
    # NDVI
    ndvi_raster <- (sientel_nir - sientel_red) / (sientel_nir + sientel_red)
    # Create a mask based on the condition (values below 0.4)
    mask <- ndvi_raster < NDVI_TRESHOLD
    # Update values below 0.4 to 0
    ndvi_raster[mask] <- 0
    # Update values above or equal to 0.4 to 1
    ndvi_raster[!mask] <- 1
    
    # plot not croped NDVI
    png(filename = paste0(OUT_PATH, "", "\\Wykres NDVI - ", d, " - ",Sys.Date(), " .png", sep=""),
        width = 7, height = 5, units = 'in', res = 500)
      plot(ndvi_raster, main = d )
      # Add the shapefile outline to the plot
      plot(border, add = TRUE, border = "red", lwd = 2)  # Adjust border color and line width as needed
      plot(circle, add = TRUE, border = "red", lwd = 2)  # Adjust border color and line width as needed
    dev.off() 
    
    
    # RGB
    rgb_stack <- stack()
    # Rescale the data in each channel to the 0-255 range manually
    sientel_red_rescaled <- (sientel_red - minValue(sientel_red)) / (maxValue(sientel_red) - minValue(sientel_red)) * 255
    sientel_green_rescaled <- (sientel_green - minValue(sientel_green)) / (maxValue(sientel_green) - minValue(sientel_green)) * 255
    sientel_blue_rescaled <- (sientel_blue - minValue(sientel_blue)) / (maxValue(sientel_blue) - minValue(sientel_blue)) * 255
    # Add the Red, Green, and Blue layers to the stack
    rgb_stack$red <- sientel_red_rescaled
    rgb_stack$green <- sientel_green_rescaled
    rgb_stack$blue <- sientel_blue_rescaled
    
    # PLOT not croped RGB
    png(filename = paste0(OUT_PATH, "",  "\\Wykres RGB - ", d, " - ",Sys.Date(), " .png", sep=""),
        width = 7, height = 5, units = 'in', res = 500)
      plotRGB(rgb_stack, main = d )
      # Add the shapefile outline to the plot
      plot(border, add = TRUE, border = "red", lwd = 2)  # Adjust border color and line width as needed
      plot(circle, add = TRUE, border = "red", lwd = 2)  # Adjust border color and line width as needed
      
    dev.off() 
    
    # ctop to circle
    ndvi_raster <- raster::crop(ndvi_raster, circle)
    ndvi_raster <- raster::mask(ndvi_raster, circle)
    
    # convet do DF
    # Convert the raster values to a data frame
    ndvi_df <- as.data.frame(ndvi_raster, xy = TRUE) 
    # renaming
    names(ndvi_df)[3] <- paste0('r_', d)
    
    if (nrow(nvid_across_years) == 0){
      nvid_across_years <- ndvi_df
    } else {
      nvid_across_years <- merge(
        x = nvid_across_years,
        y = ndvi_df, 
        by = c("x", "y"), 
        all = TRUE)
    }
    
  }

}

# zapisz
saveRDS(nvid_across_years, file = paste0(DATA_PATH, "NDIV across years.rds"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot NDVI ####
FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"
TEXT_BASE_SIZE <- 12



for (year in names(nvid_across_years)[-c(1, 2)]){
  print(year)
  w <- ggplot() +
    geom_tile(data = nvid_across_years, aes(x = x, y = y, fill = factor(.data[[year]]))) +
    # geom_sf(data = border, fill = NA, color="grey50", size=2) +
    scale_fill_manual(
      values = c("NA" = "grey", "NaN" = "red",  "0" = "gray", "1" = "darkgreen"),
      breaks = c(NA, NaN, 0, 1),
      labels = c("nie Wro", "?", "Brak zieleni", "Zielen")
    ) +
    labs(
      title = {{year}},
      caption = "Autor: WroData | Dane: sentinels.copernicus.eu",
    ) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      axis.title  = element_blank(),
      axis.text   = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank())
  
  # save
  png(filename = paste0(OUT_PATH, "", "\\Wykres NDVI croped - ", year, " - ", 
                        Sys.Date(), " .png", sep=""),
      bg=FILL_COL, width = 7, height = 7.5, units = 'in', res = 500)
  plot(w)
  dev.off() 
  
  
}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot change ####
DIC_wolacz = list('Wroclaw' = 'we Wroclawiu', 'Krakow' = 'w Krakowie', 'Warszawa' = 'w Warszawie')

plot_diff <- function(nvid_across_years, rok_poczatkowy, rok_koncowy, file_name_prefix = ""){
    
  NDVI_diff <- nvid_across_years[, c('x', 'y', rok_poczatkowy, rok_koncowy)] %>%
    na.omit() 
  
  mask_zielen <- NDVI_diff[rok_poczatkowy] == 1 & NDVI_diff[rok_koncowy] == 1 
  mask_zielen[is.na(mask_zielen)] <- FALSE
  mask_nowa_zielen <- NDVI_diff[rok_poczatkowy] == 0 & NDVI_diff[rok_koncowy] == 1 
  mask_nowa_zielen[is.na(mask_nowa_zielen)] <- FALSE
  mask_usunieta_zielen <- NDVI_diff[rok_poczatkowy] == 1 & NDVI_diff[rok_koncowy] == 0 
  mask_usunieta_zielen[is.na(mask_usunieta_zielen)] <- FALSE
  mask_brak_zielen <- NDVI_diff[rok_poczatkowy] == 0 & NDVI_diff[rok_koncowy] == 0 
  mask_brak_zielen[is.na(mask_brak_zielen)] <- FALSE
  
  
  
  NDVI_diff['diff'] <- 'inne'
  NDVI_diff[mask_zielen, 'diff'] <- "Zielen"
  NDVI_diff[mask_nowa_zielen, 'diff'] <- "Nowa zielen"
  NDVI_diff[mask_usunieta_zielen, 'diff'] <- "Usunieta zielen"
  NDVI_diff[mask_brak_zielen, 'diff'] <- "Brak zieleni"
  
  
  
  dif_text <- NDVI_diff %>%
    group_by(diff) %>%
    summarise(n = n()) %>% 
    mutate(Percentage = paste0(round(n/sum(n) * 100), "%"))
  
  FILL_COL <- "#dedfe0"
  TEXT_COL <- "#4e4d47"
  TEXT_BASE_SIZE <- 12
  
  my_colors        <- c("magenta4", "forestgreen",  "olivedrab2", "firebrick4",  "gray90")
  names(my_colors) <- c('inne',     "Zielen" , "Nowa zielen", "Usunieta zielen", "Brak zieleni"  )
  
  w <- ggplot() +
    geom_tile(data = NDVI_diff, aes(x = x, y = y, fill = diff)) +
    labs(
      title = paste0(
        "Porównanie zieleni ",
        DIC_wolacz[[CITY]],
        " miedzy ", 
        format(as.Date(rok_poczatkowy, format = "r_%Y_%m_%d"), format = "%m.'%y"),
        " a ",
        format(as.Date(rok_koncowy, format = "r_%Y_%m_%d"), format = "%m.'%y")),
      subtitle = "Widac wyraznie budowe nowych tras komunikacyjnych (AWW oraz TAT)",
      #subtitle = paste0(
      #  "W tym czasie " ,
      #  dif_text[dif_text$diff == "Usunieta zielen", "Percentage"],
      #  " powierzchni przestalo byc zielone, a na ", 
      #  dif_text[dif_text$diff == "Nowa zielen", "Percentage"], 
      #  " powstala nowa zielen"
      #  ),
      caption = "Autor: WroData | Dane: sentinels.copernicus.eu",
      x = "",
      y = "",
      fill = ""
    ) + 
    
    
    scale_fill_manual(
      values = my_colors,
      # values = c('inne' = "magenta4", "Zielen" = "forestgreen",  "Nowa zielen" = "olivedrab2", "Usunieta zielen" = "firebrick4", "Brak zieleni" = "pink"),
      #breaks = c('inne', "Zielen",  "Nowa zielen", "Usunieta zielen", "Brak zieleni"),
      #labels = c('inne', "Zielen",  "Nowa zielen", "Usunieta zielen", "Brak zieleni"),
      breaks = c("Zielen",  "Nowa zielen", "Usunieta zielen")
    ) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.5,  color = TEXT_COL),
      axis.text.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.0,  color = TEXT_COL),
      axis.ticks = element_blank(),
      axis.title.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
      axis.title.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
      
      
      panel.border = element_blank(),
      # panel.grid.major=element_blank(),
      panel.grid.minor = element_blank(),
      
      #tlo
      plot.background  = element_rect(fill = FILL_COL,  color = NA), 
      panel.background = element_rect(fill = FILL_COL,  color = NA),
      text = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2, color = TEXT_COL),
      
      # legenda
      legend.position = "bottom",# "none",
      legend.key.width = unit(0.9, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title.align = 0.5,
      legend.title = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),#element_blank(),
      legend.background = element_rect(fill = FILL_COL, color = NA),
      legend.key = element_rect(fill = FILL_COL),
      legend.text       = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),
      legend.direction = "horizontal",
      
      # tytuy
      plot.title    = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE + 2, hjust = 0.0,  color = TEXT_COL, face="bold"),
      plot.subtitle = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.01, face = "italic", color = TEXT_COL),
      plot.caption  = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2,  hjust = 0.99, color = TEXT_COL),
    )  +
    theme(axis.title  = element_blank(),
          axis.text   = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())
  
  
  # save
  png(filename = paste0(OUT_PATH, "", "\\", file_name_prefix ,"Wykres diff NDVI - ", rok_poczatkowy, " a ", rok_koncowy, " - ", Sys.Date(), " .png", sep=""),
      bg=FILL_COL, width = 7, height = 8.2, units = 'in', res = 500)
  plot(w)
  dev.off() 
  # return(w)
}




plot_diff(nvid_across_years, rok_poczatkowy = "r_2017_05_28", rok_koncowy = "r_2021_06_19")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2018_07_07", rok_koncowy = "r_2021_06_19")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2018_08_29", rok_koncowy = "r_2021_06_19")

plot_diff(nvid_across_years, rok_poczatkowy = "r_2017_05_28", rok_koncowy = "r_2020_07_31")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2018_07_07", rok_koncowy = "r_2020_07_31")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2018_08_29", rok_koncowy = "r_2020_07_31")




plot_diff(nvid_across_years, rok_poczatkowy = "r_2018_05_03", rok_koncowy = "r_2018_09_10", file_name_prefix = "seq ")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2018_09_10", rok_koncowy = "r_2019_08_28", file_name_prefix = "seq ")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2019_08_28", rok_koncowy = "r_2020_07_26", file_name_prefix = "seq ")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2020_07_26", rok_koncowy = "r_2021_07_11", file_name_prefix = "seq ")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2021_07_11", rok_koncowy = "r_2022_07_21", file_name_prefix = "seq ")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2022_07_21", rok_koncowy = "r_2023_09_06", file_name_prefix = "seq ")
plot_diff(nvid_across_years, rok_poczatkowy = "r_2023_09_06", rok_koncowy = "r_2023_09_16", file_name_prefix = "seq ")


