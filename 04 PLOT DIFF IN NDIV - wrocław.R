
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



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Stale ####

DATA_PATH <- "C:\\WroData data\\"
MAIN_PATH <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
SHP_PATH <- paste0(MAIN_PATH, "\\SHP")
OUT_PATH <- paste0(MAIN_PATH, "\\20230906 Polska z Sientiela\\CHARTS")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wczytaj dane ####
nvid_across_years <- readRDS(file = paste0(DATA_PATH, "NDIV across years.rds"))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Róznica NDIV ####

names(nvid_across_years)

rok_poczatkowy <- "r_2018_07_07"
rok_koncowy <- "r_2023_05_27"

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




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot ####


FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"
TEXT_BASE_SIZE <- 12


w <- ggplot() +
  geom_tile(data = NDVI_diff, aes(x = x, y = y, fill = diff)) +
  
  
  scale_fill_manual(
    values = c('inne' = "magenta4", "Zielen" = "forestgreen",  "Nowa zielen" = "olivedrab2", "Usunieta zielen" = "firebrick4", "Brak zieleni" = "gray88"),
    breaks = c('inne', "Zielen",  "Nowa zielen", "Usunieta zielen", "Brak zieleni"),
    labels = c('inne', "Zielen",  "Nowa zielen", "Usunieta zielen", "Brak zieleni")
  ) +
  labs(
    title = paste0("Porównanie zieleni we Wroclawiu miedzy ", rok_poczatkowy, " a ", rok_koncowy),
    subtitle = "test",
    caption = "Autor: WroData | Dane: sentinels.copernicus.eu | Konsultacja merytoryczna: Polska z Sentinela",
    x = "",
    y = ""
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
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
  

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres diff NDVI - ", rok_poczatkowy, " a ", rok_koncowy, " - ", Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 8, units = 'in', res = 500)
plot(w)
dev.off() 

  
  
NDVI_diff %>%
  group_by(diff) %>%
  summarise(n = n())
  
  
  
  







