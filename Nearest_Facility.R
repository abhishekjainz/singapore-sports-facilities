
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
library(spatstat)

path = "data/"
plot_path = "plots/"

#####################
##### READ DATA #####
#####################
fitness <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
                   layer = "fitness_summarised_points")

sportsfac <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                     layer = "sports_facilities")
gym <- read_sf(dsn = paste(path, "gym_facilities/", sep = ""), 
               layer = "gym_facilities")

island <- read_sf(dsn = paste(path, "land_boundary/intersection_boundary", 
                              sep = ""), layer = "island_intersect")

hdb_file_path = "HDB/hdb_full.csv"
hdb_csv <- file.path(getwd(), paste(path, hdb_file_path, sep=""))
hdb_file = read.csv(hdb_csv)
names(hdb_file)
hdb_sf = st_as_sf(hdb_file, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

mrt_station <- read_sf(dsn = paste(path, "mrt_station/", sep = ""), 
                       layer = "MRTLRTStnPtt")

tm_shape(sportsfac) + tm_dots("purple")

#####################################
##### NEAREST FACILITY FROM HDB #####
#####################################
nearest_fitness <- st_nearest_feature(hdb_sf, fitness)
dist_fitness = st_distance(hdb_sf, fitness[nearest_fitness,], by_element=TRUE)

dist_fitness_df <- dist_fitness %>% as.data.frame()
hdb_df <- hdb_sf %>% as.data.frame()
fitness_df <- fitness %>% as.data.frame()

hdb_fitness_join = cbind(hdb_df, fitness_df[nearest_fitness,])

HDB_coord <- st_coordinates(hdb_fitness_join[,12])
fitness_coord <- st_coordinates(hdb_fitness_join[,19])
# hdb_fitness_join[,12]
# hdb_fitness_join[,19]

### Test Out
test_hdb <- hdb_sf %>% as.data.frame()
test_hdb <- test_hdb[3350,]

tmap_mode("view")
tm_shape(st_as_sf(test_hdb)) + tm_dots("red") + 
  tm_shape(fitness) + tm_dots("black")


##############################################
##### NEAREST SPORTS FACILITIES FROM HDB #####
##############################################
# Convert to same CRS (i.e. 4326)
sportsfac <- st_transform(sportsfac, st_crs(4326)) 

nearest_sportsfac <- st_nearest_feature(hdb_sf, sportsfac)
dist_sportsfac = st_distance(hdb_sf, sportsfac[nearest_sportsfac,], by_element=TRUE)

dist_sportsfac_df <- dist_sportsfac %>% as.data.frame()
hdb_df <- hdb_sf %>% as.data.frame()
sportsfac_df <- sportsfac %>% as.data.frame()

hdb_sportsfac_join = cbind(hdb_df, sportsfac_df[nearest_sportsfac,])

sportsfac_coord <- st_coordinates(hdb_sportsfac_join[,18])


#################################
##### NEAREST GYMS FROM HDB #####
#################################
# Convert to same CRS (i.e. 4326)
gym <- st_transform(gym, st_crs(4326))

nearest_gym <- st_nearest_feature(hdb_sf, gym)
dist_gym = st_distance(hdb_sf, gym[nearest_gym,], by_element=TRUE)

dist_gym_df <- dist_gym %>% as.data.frame()
hdb_df <- hdb_sf %>% as.data.frame()
gym_df <- gym %>% as.data.frame()

hdb_gym_join = cbind(hdb_df, gym_df[nearest_gym,])

gym_coord <- st_coordinates(hdb_gym_join[,18])

#########################################
##### COMBINE ALL FEATURES TOGETHER #####
#########################################
hdb_to_facility_combine_coord <- cbind(HDB_coord, 
                                       fitness_coord, 
                                       sportsfac_coord, 
                                       gym_coord)
hdb_to_facility_combine_coord <- cbind(hdb_to_facility_combine_coord, 
                                       dist_fitness_df, 
                                       dist_sportsfac_df, 
                                       dist_gym_df)

colnames(hdb_to_facility_combine_coord) <- 
  c("hdb_long", "hdb_lat", "fitness_long", "fitness_lat", 
    "sportsfac_long", "sportsfac_lat", "gym_long", "gym_lat",
    "dist_to_fitness", "dist_to_sports", "dist_to_gym")

full_hdb_to_facility <- cbind(hdb_sf, hdb_to_facility_combine_coord)
full_hdb_to_facility

#########################
##### MEAN DISTANCE #####
#########################
mean(dist_sportsfac)
mean(dist_gym)
mean(dist_fitness)

island_df <- island %>% as.data.frame()
island_geo <- island_df[,c(1,10)]

avg_dist_to_facilities <- full_hdb_to_facility %>% group_by(town) %>%
  summarise(avg_dist_to_fitness = mean(dist_to_fitness),
            avg_dist_to_gym = mean(dist_to_gym),
            avg_dist_to_sports = mean(dist_to_sports)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

merged_df <- merge(island_geo, avg_dist_to_facilities, island_geo, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)
merged_df

col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
col_sports <- c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
col_gym <- c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

plot_fitness_avg_dist <- tm_shape(st_as_sf(merged_df)) + tm_borders("black") + 
  tm_fill("avg_dist_to_fitness", by="PLN_AREA_N", palette = col_fitness)

plot_gym_avg_dist <- tm_shape(st_as_sf(merged_df)) + tm_borders("black") + 
  tm_fill("avg_dist_to_gym", by="PLN_AREA_N", palette = col_gym)

plot_sports_avg_dist <- tm_shape(st_as_sf(merged_df)) + tm_borders("black") + 
  tm_fill("avg_dist_to_sports", by="PLN_AREA_N", palette = col_sports)

# Save output as PNG
tmap_save(plot_fitness_avg_dist, 
          filename = paste(plot_path,"fitness_avg_dist.png", sep=""))
tmap_save(plot_gym_avg_dist, 
          filename = paste(plot_path,"gym_avg_dist.png", sep=""))
tmap_save(plot_sports_avg_dist, 
          filename = paste(plot_path,"sports_avg_dist.png", sep=""))


#######################
##### MONTE CARLO #####
#######################
#creating the boundaries
st_geometry(island$geometry)
island_coords <- st_coordinates(island) %>% as.data.frame()
projcrs <- "+proj=longlat +init=epsg:4326 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
island_bound <- st_as_sf(x=island_coords, coords = c("X", "Y"), crs = projcrs)


island_bound_new <- as(st_geometry(island_bound$geometry), "Spatial")
island_bound_sp_new= spTransform(island_bound_new, CRS("+init=epsg:32748"))


island_coords

#w <- as.owin(island)

island_boundary <- as(island_bound, "Spatial")
island_boundary_sp= spTransform(island_boundary, CRS("+init=epsg:32748"))
island_boundary_sp= spTransform(island_boundary, CRS("+init=epsg:4326"))
island_sf_new = island[-c(44,53, 20, 51, 52, 45,11, 27,9, 46, 21, 42, 38),]

island_bound_new <- as(st_geometry(island_sf_new$geometry), "Spatial")
# island_boundary_sp_new= spTransform(island_boundary_new, CRS("+init=epsg:32748"))
island_bound_sp_new = spTransform(island_bound_new, CRS("+init=epsg:4326"))

total_gym = nrow(gym)

#gym monte carlo
n2     <- 500L               
hdb_to_gym.r <- vector(length = n2) 

for (i in 1:n2){
  gym_rand.p <- rpoint(n=total_gym, win=island_bound_sp_new )
  gym_rand.p <- st_as_sf(gym_rand.p, crs = projcrs)
  curr_nearest_gym <- st_nearest_feature(hdb_sf, gym_rand.p)
  curr_dist_gym = st_distance(hdb_sf, gym[nearest_gym,], by_element=TRUE)
  hdb_to_gym.r[i] <- mean(curr_dist_gym)
}

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






