
library(sf)
library(ggmap)
library(maptools)
library(tmap)
library(dplyr)
library(tidyverse)
library(mapview)
library(future)
library(rvest)
library(raster)
library(rgeos)
library(GISTools)
library(leaflet)
library(rgdal)
library(rmapshaper)
library(ggpubr)

path = "data/"
plot_path = "plots/"

# Read data
fitness <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
                           layer = "fitness_summarised_points")
sportsfac <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                     layer = "sports_facilities")
gym <- read_sf(dsn = paste(path, "gym_facilities/", sep = ""), 
               layer = "gym_facilities")

island <- read_sf(dsn = paste(path, "land_boundary/intersection_boundary", 
                              sep = ""), layer = "island_intersect")

##############################
##### FITNESS FACILITIES #####
##############################
fitness_intersection <- st_intersection(island, fitness)
fitness_count <- as.data.frame(fitness_intersection %>% group_by(PLN_AREA_N) %>% count())[,1:2]
names(fitness_count)[names(fitness_count) == 'n'] <- "FITNESS_COUNT"

fitness_count_with_area <- merge(fitness_count, island, by="PLN_AREA_N", all=TRUE)
fitness_count_with_area$Fitness_Density <- fitness_count_with_area$FITNESS_COUNT/as.numeric(fitness_count_with_area$TOTAL_AREA)

## Density Plot based on Land Area 
tmap_mode("plot")
col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
plot_fitness_density <- tm_shape(st_as_sf(fitness_count_with_area)) +
  tm_fill("Fitness_Density", title = "Fitness Facilities Density", palette = col_fitness) + 
  tm_borders('black') +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("right", "bottom"),
    legend.width = -0.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1)

plot_fitness_density

# Save output as PNG
tmap_save(plot_fitness_density, 
          filename = paste(plot_path,"fitness_density.png", sep=""))


#############################
##### SPORTS FACILITIES #####
#############################
# Convert to same CRS (i.e. 4326)
sportsfac <- st_transform(sportsfac, st_crs(4326)) 

sportsfac_intersection <- st_intersection(island, sportsfac)
sportsfac_count <- as.data.frame(sportsfac_intersection %>% group_by(PLN_AREA_N) %>% count())[,1:2]
names(sportsfac_count)[names(sportsfac_count) == 'n'] <- "SPORTSFAC_COUNT"

sportsfac_count_with_area <- merge(sportsfac_count, island, by="PLN_AREA_N", all=TRUE)
sportsfac_count_with_area$Sportsfac_Density <- sportsfac_count_with_area$SPORTSFAC_COUNT/as.numeric(sportsfac_count_with_area$TOTAL_AREA)

## Density Plot based on Land Area 
tmap_mode("plot")
col_sports <- c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
plot_sportsfac_density <- tm_shape(st_as_sf(sportsfac_count_with_area)) +
  tm_fill("Sportsfac_Density", title = "Sports Facilities Density", palette = col_sports) + 
  tm_borders('black') +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("right", "bottom"),
    legend.width = -0.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1)

plot_sportsfac_density

# Save output as PNG
tmap_save(plot_sportsfac_density, 
          filename = paste(plot_path,"sportsfac_density.png", sep=""))


################
##### GYMS #####
################
# Convert to same CRS (i.e. 4326)
gym <- st_transform(gym, st_crs(4326)) 

gym_intersection <- st_intersection(island, gym)
gym_count <- as.data.frame(gym_intersection %>% group_by(PLN_AREA_N) %>% count())[,1:2]
names(gym_count)[names(gym_count) == 'n'] <- "GYM_COUNT"

gym_count_with_area <- merge(gym_count, island, by="PLN_AREA_N", all=TRUE)
gym_count_with_area$Gym_Density <- gym_count_with_area$GYM_COUNT/as.numeric(gym_count_with_area$TOTAL_AREA)

## Density Plot based on Land Area 
tmap_mode("plot")
col_gym <- c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')
plot_gym_density <- tm_shape(st_as_sf(gym_count_with_area)) +
  tm_fill("Gym_Density", title = "Gym Facilities Density", palette = col_gym) + 
  tm_borders('black') + 
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("right", "bottom"),
    legend.width = -0.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1)

plot_gym_density

# Save output as PNG
tmap_save(plot_gym_density, 
          filename = paste(plot_path,"gym_density.png", sep=""))

