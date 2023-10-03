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
CITY <- "Wroclaw"
DATA_PATH <- "C:\\WroData data\\"
MAIN_PATH <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
SHP_PATH <- paste0(MAIN_PATH, "\\SHP")
MIN_DATE <- "2015-04-01"
MAX_DATE <- "2017-10-01"

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
create_folder_if_not_exists(paste0(DATA_PATH, CITY))
create_folder_if_not_exists(paste0(DATA_PATH, CITY, '\\', "dane"))
create_folder_if_not_exists(paste0(DATA_PATH, CITY, '\\', "dane\\l2a"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### preparing Wroclaw #### 
DIC_cities = list('Wroclaw' = '0264', 'Krakow' = '1261', 'Warszawa' = '1465')

# do pobrania z : https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/
border <- read_sf(paste0(SHP_PATH, "\\Powiaty\\powiaty.shp")) %>%
  filter(JPT_KOD_JE == DIC_cities[[CITY]])
border <- border[1, c('geometry')]

# change CRS
border <- st_transform(border, crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

# Save the sf object as a new shapefile
create_folder_if_not_exists(paste0(SHP_PATH, "\\", CITY))
st_write(border[1, c('geometry')], paste0(SHP_PATH, "\\", CITY,  "\\", CITY, ".shp"))

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
  filter(month %in% c(5, 6, 7, 8, 9),
         #clouds < 0.5
         ) %>%
  arrange(clouds) %>%
  group_by(year) %>%
  filter(row_number() <= 4) %>%
  arrange(year) 

saveRDS(  images_list_short, file = paste0(DATA_PATH, CITY, "\\dane\\", "images metadata.rds"))
write.csv(images_list_short, file = paste0(DATA_PATH, CITY, "\\dane\\","images metadata.csv"))
#images_list_short <- images_list_short %>% filter(year %in% c(2018, 2019, 2020, 2021))

# zamów po jednym
for (r in 1:nrow(images_list_short)){ 
  row <- images_list_short[r,]
  d <- as.Date(row$sensing_datetime)
  print("______________________________________")
  print(d)
  
  # create specific folder
  create_folder_if_not_exists(paste0(DATA_PATH, CITY, '\\', "dane\\l2a\\", gsub("-", "_", d)))
  
  # order file
  sen2r::sen2r(gui = FALSE, 
               timewindow = c(d, d),
               extent = border,
               list_prods = c("BOA"),
               # list_indices = c("NDVI"),
               s2_levels = "l2a", 
               path_l2a = paste0(DATA_PATH, CITY, '\\', "dane\\l2a\\", gsub("-", "_", d)), 
               path_out = paste0(DATA_PATH, CITY, '\\', "dane")
  )
}




# zamów po jednym
for (d in c("2015-08-03", "2015-08-13", "2015-08-23", "2015-09-12", "2016-05-09", "2016-05-19",
            "2016-05-29", "2016-06-08", "2017-08-31", "2017-05-28")){
  d <- as.Date(d)
  print("______________________________________")
  print(d)
  # create specific folder
  create_folder_if_not_exists(paste0(DATA_PATH, CITY, '\\', "dane\\l2a\\", gsub("-", "_", d)))
  # order file
  sen2r::sen2r(gui = FALSE, 
               timewindow = c(d, d),
               extent = border,
               list_prods = c("BOA"),
               # list_indices = c("NDVI"),
               s2_levels = "l2a", 
               path_l2a = paste0(DATA_PATH, CITY, '\\', "dane\\l2a\\", gsub("-", "_", d)), 
               path_out = paste0(DATA_PATH, CITY, '\\', "dane")
  )
}







03/08/2015
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100721.json")
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100715.json")
[1] "2015-08-13"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100730.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100722.json")
[1] "2015-08-23"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100737.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100731.json")
[1] "2015-09-12"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100744.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100738.json")
[1] "2016-05-09"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100751.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100744.json")
[1] "2016-05-19"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100757.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100751.json")
"2016-05-29"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100804.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100758.json")
[1] "2016-06-08"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100812.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100805.json")
[1] "2017-08-31"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100818.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100812.json")
[1] "2017-07-30"
# safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20231002_100833.json")
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20231002_100825.json")
















[1] "______________________________________"
[1] "2018-08-20"
# safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075241.json")
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075234.json")

[1] "______________________________________"
[1] "2018-07-01"
# safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075248.json")
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075242.json")

[1] "______________________________________"
[1] "2019-07-23"
  ¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075254.json")
  ¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075248.json")
[1] "______________________________________"
[1] "2019-09-11"
  ¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075301.json")
  ¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075255.json")
[1] "______________________________________"
[1] "2020-07-30"
  ¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075310.json")
  ¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075301.json")
[1] "______________________________________"
[1] "2020-09-18"
  ¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075315.json")
  ¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075310.json")
[1] "______________________________________"
[1] "2021-09-08"
#safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075321.json")
#sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075315.json")
[1] "______________________________________"
[1] "2021-09-25"
# safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230921_075325.json")
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230921_075321.json")
















[1] "2017-08-31"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_112414.json")
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_112403.json")
[1] "2017-09-27"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_112433.json")
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_112414.json")
[1] "2018-05-03"
# safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_112448.json")
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_112434.json")
[1] "2018-09-10"
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_112448.json")
[1] "2019-08-28"
# sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_114651.json")
[1] "2019-06-09"
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121125.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121113.json")
[1] "2020-07-26"
#safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121137.json")
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121126.json")
[1] "2020-08-12"
¦ sen2r Processing Report
¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121151.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121138.json")
[1] "2021-07-11"
¦ sen2r Processing Report
safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121203.json")
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121151.json")
[1] "2021-09-04"
¦ sen2r Processing Report
¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121216.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121203.json")
[1] "2022-08-25"
¦ sen2r Processing Report
¦   safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121228.json")
¦   sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121217.json")
[1] "2022-07-21"
¦ sen2r Processing Report
¦   s2_order("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121242.json")
#safe_is_online("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/lta_orders/lta_20230920_121242.json")
#sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121228.json")
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121228.json")
[1] "2023-09-16"
¦ sen2r Processing Report
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_121242.json")
[1] "2023-09-06"
¦ sen2r Processing Report
sen2r("C:/Users/KRZYSZ~1.KAR/ONEDRI~1/DOCUME~1/SEN2R~1/proc_par/s2proc_20230920_122842.json")








