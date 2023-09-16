# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Materialy pomocnicze ####
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

library(sen2r)
library(sf)
library(dplyr)
library(lubridate)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Stale ####
DATA_PATH <- "C:\\WroData data\\"
MAIN_PATH <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
SHP_PATH <- paste0(MAIN_PATH, "\\SHP")
MIN_DATE <- "2015-04-01"
MAX_DATE <- "2023-10-01"

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Foldery ####
# create outpuf folders
create_folder_if_not_exists <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created:", folder_path, "\n")
  } else {
    cat("Folder already exists:", folder_path, "\n")
  }
}
create_folder_if_not_exists(paste0(DATA_PATH, "dane"))
create_folder_if_not_exists(paste0(DATA_PATH, "dane\\l2a"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### preparing Wroclaw ####
border <- read_sf(paste0(SHP_PATH, "\\Powiaty\\powiaty.shp")) %>%
  filter(JPT_KOD_JE == "0264")
border <- border[1, c('geometry')]

# change CRS
border <- st_transform(border, crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

# Save the sf object as a new shapefile
st_write(border[1, c('geometry')], paste0(SHP_PATH, "\\Wroclaw\\Wroclaw.shp"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Jakie zdjecia sa dostepne ####

images_list <- sen2r::s2_list(
  spatial_extent = border,
  time_interval = as.Date(c(MIN_DATE, MAX_DATE)),
  max_cloud = 0.5
)

# stwórz df do analizy i mozliwosci czytania ich pózniej
images_list_short <- as.data.frame(images_list) %>%
  mutate(month = month(sensing_datetime),
         year = year(sensing_datetime)
  )%>%
  filter(month %in% c(5, 6, 7, 8),
         #clouds < 0.5
         ) %>%
  arrange(clouds) %>%
  group_by(year) %>%
  filter(row_number() <= 2) %>%
  arrange(year) 

saveRDS(images_list_short, file = paste0(DATA_PATH, "dane\\images metadata.rds"))


# zamów po jednym
for (r in 1:nrow(images_list_short)){ 
  row <- images_list_short[r,]
  d <- as.Date(row$sensing_datetime)
  print("______________________________________")
  print(d)
  
  # create specific folder
  create_folder_if_not_exists(paste0(PATH, "dane\\l2a\\", gsub("-", "_", d)))
  
  # order file
  sen2r::sen2r(gui = FALSE, 
               timewindow = c(d, d),
               extent = border,
               list_prods = c("BOA"),
               # list_indices = c("NDVI"),
               s2_levels = "l2a", 
               path_l2a = paste0(PATH, "dane\\l2a\\", gsub("-", "_", d)), 
               path_out = paste0(PATH, "dane")
  )
}




# zamów po jednym
for (d in c("2015-08-03", "2015-08-13", "2016-05-09", "2017-08-31", "2023-06-04")){
  d <- as.Date(d)
  print("______________________________________")
  print(d)
  # create specific folder
  create_folder_if_not_exists(paste0(PATH, "dane\\l2a\\", gsub("-", "_", d)))
  # order file
  sen2r::sen2r(gui = FALSE, 
               timewindow = c(d, d),
               extent = border,
               list_prods = c("BOA"),
               # list_indices = c("NDVI"),
               s2_levels = "l2a", 
               path_l2a = paste0(PATH, "dane\\l2a\\", gsub("-", "_", d)), 
               path_out = paste0(PATH, "dane")
  )
}




