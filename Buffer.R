
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
library(rgdal)
library(rgeos)
library(GISTools)
library(leaflet)
library(spatstat)
library(knitr)
library(rmapshaper)
library(ggpubr)
library(gstat)

install.packages("remotes") 
remotes::install_github("mtennekes/oldtmaptools")
library(oldtmaptools)

#HDB Point Processing
path = "data/"

hdb_file_path = "HDB/hdb_full.csv"
hdb_csv <- file.path(getwd(), paste(path, hdb_file_path, sep=""))
hdb_file = read.csv(hdb_csv)

names(hdb_file)

hdb_sf = st_as_sf(hdb_file, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

#Bring the sf for islands
island_sf <- read_sf(dsn = paste(path, "land_boundary/intersection_boundary/", sep = ""), 
                layer = "island_intersect")

#view the hdb points on data
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots(size = 0.01)

### Creating buffer for HDB, 500m, & make sure all same crs
temp <- st_as_sf(hdb_sf)
st_crs(temp) <- 4326
temp <- st_transform(temp, crs = 7801)

buff <- st_buffer(temp, dist = 500)

buff <- st_transform(buff, crs = 4326)
union <- st_union(buff)

#simple 500m buffer around hdb
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') +
  tm_shape(union) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01)

temp_hdb_1k <- st_as_sf(hdb_sf)
st_crs(temp_hdb_1k) <- 4326
temp_hdb_1k <- st_transform(temp_hdb_1k, crs = 7801)

buff_hdb_1k <- st_buffer(temp_hdb_1k, dist = 1000)

buff_hdb_1k <- st_transform(buff_hdb_1k, crs = 4326)
union_hdb_1k <- st_union(buff_hdb_1k)

### Map out 1k buffer for HDB
tmap_mode("plot")

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') +
  tm_shape(union_hdb_1k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01)
  

#Bring the sf for gyms & doing dissolved buffer
gym_sf <- read_sf(dsn = paste(path, "gym_facilities/", sep = ""), 
                     layer = "gym_facilities")

temp2 <- st_as_sf(gym_sf)
st_crs(temp2) <- 4326
temp2 <- st_transform(temp2, crs = 7801)
buff2 <- st_buffer(temp2, dist = 750)
buff2 <- st_transform(buff2, crs = 4326)
union2 <- st_union(buff2)

#Buffer starting from Gym
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.05) + 
  tm_shape(union2) + tm_polygons("yellow", alpha = 0.2)
  
#Bring the sf for sports complex & doing buffer
sports_complex_sf <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                  layer = "sports_facilities")

#Buffer starting from Sports Complex
temp3 <- st_as_sf(sports_complex_sf)
st_crs(temp3) <- 4326
temp3 <- st_transform(temp3, crs = 7801)
buff3 <- st_buffer(temp3, dist = 2000)
buff3 <- st_transform(buff3, crs = 4326)
union3 <- st_union(buff3)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.05) + 
  tm_shape(union3) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01)

#load MRT Stations data
mrt_station <- read_sf(dsn = paste(path, "mrt_station/", sep = ""), 
                       layer = "MRTLRTStnPtt")

#load fitness facilities data
fitness_facilities_sf <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
                                            layer = "fitness_summarised_points")



temp5 <- st_as_sf(fitness_facilities_sf)
st_crs(temp5) <- 4326
temp5 <- st_transform(temp5, crs = 7801)
buff5 <- st_buffer(temp5, dist = 500)
buff5 <- st_transform(buff5, crs = 4326)
union5 <- st_union(buff5)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.1) + 
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01)


########################## Points inside HDB Buffers ##########################
#taking the total counts
total_hdb = nrow(hdb_sf)
total_sc = nrow(sports_complex_sf)
total_ff = nrow(fitness_facilities_sf)
total_gym = nrow(gym_sf)


#Buffer starting from HDB to get gym counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_1k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2) 


gym_sf_as_sp <- as_Spatial(gym_sf)
buff_hdb_1k_sf_as_sp <- as_Spatial(union_hdb_1k)
gym_in_hdb_1k_count <- poly.counts(gym_sf_as_sp, buff_hdb_1k_sf_as_sp)
# find the % of gym inside 1k hdb polygon
perc_gym_hdb_1k =  gym_in_hdb_1k_count/total_gym * 100
perc_gym_hdb_1k
print("The percentage of gyms covered within 1km of HDB is 88.30%.")


#Buffer starting from HDB to get sports complex counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_1k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2) 

sc_sf_as_sp <- as_Spatial(sports_complex_sf)
sc_in_hdb_1k_count <- poly.counts(sc_sf_as_sp, buff_hdb_1k_sf_as_sp)
# find the % of hdb inside gym polygon
perc_sc_hdb_1k =  sc_in_hdb_1k_count/total_sc * 100
perc_sc_hdb_1k
print("The percentage of Sports Complex covered within 1km of HDB is 99.58%.")

#Buffer starting from HDB to get fitness facilities counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_1k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2) 

ff_sf_as_sp <- as_Spatial(fitness_facilities_sf)
ff_in_hdb_1k_count <- poly.counts(ff_sf_as_sp, buff_hdb_1k_sf_as_sp)
# find the % of hdb inside gym polygon
perc_ff_hdb_1k =  ff_in_hdb_1k_count/total_ff * 100
perc_ff_hdb_1k
print("The percentage of Fitness Facilities covered within 1km of HDB is 80.22%.")


#'''-------------------------------------------------------------------------'''
buff_hdb_2k <- st_buffer(temp_hdb_1k, dist = 2000)

buff_hdb_2k <- st_transform(buff_hdb_2k, crs = 4326)
union_hdb_2k <- st_union(buff_hdb_2k)


#Buffer starting from HDB to get gym counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_2k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2) 

buff_hdb_2k_sf_as_sp <- as_Spatial(union_hdb_2k)
gym_in_hdb_2k_count <- poly.counts(gym_sf_as_sp, buff_hdb_2k_sf_as_sp)
# find the % of gym inside 1k hdb polygon
perc_gym_hdb_2k =  gym_in_hdb_2k_count/total_gym * 100
perc_gym_hdb_2k
print("The percentage of gyms covered within 2km of HDB is 99.78%.")


#Buffer starting from HDB to get sports complex counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_2k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2) 

sc_in_hdb_2k_count <- poly.counts(sc_sf_as_sp, buff_hdb_2k_sf_as_sp)
# find the % of hdb inside gym polygon
perc_sc_hdb_2k =  sc_in_hdb_2k_count/total_sc * 100
perc_sc_hdb_2k
print("The percentage of Sports Complex covered within 2km of HDB is 100%.")

#Buffer starting from HDB to get fitness facilities counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_2k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2) 

ff_in_hdb_2k_count <- poly.counts(ff_sf_as_sp, buff_hdb_2k_sf_as_sp)
# find the % of hdb inside gym polygon
perc_ff_hdb_2k =  ff_in_hdb_2k_count/total_ff * 100
perc_ff_hdb_2k
print("The percentage of Fitness Facilities covered within 2km of HDB is 97.10%.")


buff_hdb_500 <- st_buffer(temp_hdb_1k, dist = 500)

buff_hdb_500 <- st_transform(buff_hdb_500, crs = 4326)
union_hdb_500 <- st_union(buff_hdb_500)

#Buffer starting from HDB to get gym counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_500) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2) 

buff_hdb_500_sf_as_sp <- as_Spatial(union_hdb_500)
gym_in_hdb_500_count <- poly.counts(gym_sf_as_sp, buff_hdb_500_sf_as_sp)
# find the % of gym inside 1k hdb polygon
perc_gym_hdb_500 =  gym_in_hdb_500_count/total_gym * 100
perc_gym_hdb_500
print("The percentage of gyms covered within 500m of HDB is 62.03%.")


#Buffer starting from HDB to get sports complex counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_500) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2) 

sc_in_hdb_500_count <- poly.counts(sc_sf_as_sp, buff_hdb_500_sf_as_sp)
# find the % of hdb inside gym polygon
perc_sc_hdb_500 =  sc_in_hdb_500_count/total_sc * 100
perc_sc_hdb_500
print("The percentage of Sports Complex covered within 500 of HDB is 96.64%.")

#Buffer starting from HDB to get fitness facilities counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_500) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2) 

ff_in_hdb_500_count <- poly.counts(ff_sf_as_sp, buff_hdb_500_sf_as_sp)
# find the % of hdb inside gym polygon
perc_ff_hdb_500 =  ff_in_hdb_500_count/total_ff * 100
perc_ff_hdb_500
print("The percentage of Fitness Facilities covered within 500m of HDB is 53.11%.")



########################## Points inside MRT Buffers ##########################
#creating buffer of 1km around mrt
temp4 <- st_as_sf(mrt_station)
buff4 <- st_buffer(temp4, dist = 1000)
buff4 <- st_transform(buff4, crs = 4326)
union4 <- st_union(buff4)


#Plotting Sports Complex on MRT with 1km buffer
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2)

#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp <- as_Spatial(union4)
sc_sf_as_sp <- as_Spatial(sports_complex_sf)

sc_in_mrt_count <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp)
# find the % of sports complex inside MRT 1km buffer polygon
mrt_sc4 =  sc_in_mrt_count/total_sc * 100
mrt_sc4 #81.09


#Plotting Gyms on MRT with 1km buffer
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

gym_sf_as_sp <- as_Spatial(gym_sf)
gym_in_mrt_count <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp)
mrt_gym5 =  gym_in_mrt_count/total_gym * 100
mrt_gym5 #88.74


#Plotting Fitness Facilities on MRT with 1km buffer
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.1) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)

ff_sf_as_sp <- as_Spatial(fitness_facilities_sf)
ff_in_mrt_count <- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp)

# find the % of Fitness Facilities inside MRT buffer polygon
mrt_ff6 =  ff_in_mrt_count/total_ff * 100
mrt_ff6 #80.77

#creating buffer of 2km around mrt
buff5 <- st_buffer(temp4, dist = 2000)
buff5 <- st_transform(buff5, crs = 4326)
union5 <- st_union(buff5)

#Plotting sports complexes on MRT with 2km buffer 
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2)

#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp_2k <- as_Spatial(union5)
sc_in_mrt_count_2k <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp_2k)

# find the % of sports complex inside MRT 1km buffer polygon
mrt_sc_2k =  sc_in_mrt_count_2k/total_sc * 100
mrt_sc_2k #99.58

#Plotting gyms on MRT with 2km buffer 
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

gym_in_mrt_2k <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp_2k)
mrt_gym_2k =  gym_in_mrt_2k/total_gym * 100
mrt_gym_2k #99.12

#Plotting Fitness Facilities on MRT with 2km buffer 
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)


ff_in_mrt_2k<- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp_2k)

# find the % of Fitness Facilities inside MRT buffer polygon

mrt_ff_2k =  ff_in_mrt_2k/total_ff * 100
mrt_ff_2k #98.34


#creating buffer of 500m around mrt
buff6 <- st_buffer(temp4, dist = 500)
buff6 <- st_transform(buff6, crs = 4326)
union6 <- st_union(buff6)


#Plotting sports complexes on MRT with 500m buffer 
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union6) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2)

#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp_500 <- as_Spatial(union6)

sc_in_mrt_count_500 <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp_500)

# find the % of sports complex inside MRT 1km buffer polygon
mrt_sc_500 =  sc_in_mrt_count_500/total_sc * 100
mrt_sc_500 #39.08


#Plotting gyms on MRT with 500m buffer 
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union6) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

gym_in_mrt_500 <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp_500)
mrt_gym_500 =  gym_in_mrt_500/total_gym * 100
mrt_gym_500 #69.76


#Plotting fitness facilities on MRT with 500m buffer 
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("red", size = 0.1) +
  tm_shape(union6) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)


ff_in_mrt_500<- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp_500)

# find the % of Fitness Facilities inside MRT buffer polygon
mrt_ff_500 =  ff_in_mrt_500/total_ff * 100
mrt_ff_500
