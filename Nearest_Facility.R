
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
library(matrixStats)
library(SpatialAcc)

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

tm_shape(hdb_sf) + tm_dots("purple")


#####################################
##### NEAREST FACILITY FROM HDB #####
#####################################
# fitness <- st_transform(fitness, st_crs(3414)) 

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
    "dist_to_fitness", "dist_to_sportsfac", "dist_to_gym")

full_hdb_to_facility <- cbind(hdb_sf, hdb_to_facility_combine_coord) %>%
  relocate(POSTAL)

qtm(full_hdb_to_facility)

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
            avg_dist_to_sports = mean(dist_to_sportsfac)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

merged_df <- merge(island_geo, avg_dist_to_facilities, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)
merged_df

col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
col_sports <- c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
col_gym <- c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

plot_fitness_avg_dist <- tm_shape(st_as_sf(merged_df)) + tm_borders("black") + 
  tm_fill("avg_dist_to_fitness", by="PLN_AREA_N", palette = col_fitness)

plot_gym_avg_dist <- tm_shape(st_as_sf(merged_df)) + tm_borders("black") + 
  tm_fill("avg_dist_to_gym", by="PLN_AREA_N", palette = col_gym)

plot_sports_avg_dist <- tm_shape(st_as_sf(merged_df)) + tm_borders("black") + 
  tm_fill("avg_dist_to_sportsfac", by="PLN_AREA_N", palette = col_sports)

# Save output as PNG
tmap_save(plot_fitness_avg_dist, 
          filename = paste(plot_path,"fitness_avg_dist.png", sep=""))
tmap_save(plot_gym_avg_dist, 
          filename = paste(plot_path,"gym_avg_dist.png", sep=""))
tmap_save(plot_sports_avg_dist, 
          filename = paste(plot_path,"sports_avg_dist.png", sep=""))


# #######################
# ##### MONTE CARLO #####
# #######################
# #creating the boundaries
# st_geometry(island$geometry)
# island_coords <- st_coordinates(island) %>% as.data.frame()
# projcrs <- "+proj=longlat +init=epsg:4326 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# island_bound <- st_as_sf(x=island_coords, coords = c("X", "Y"), crs = projcrs)
# 
# 
# island_coords
# 
# w <- as.owin(island)
# 
# island_boundary <- as(island_bound, "Spatial")
# island_boundary_sp= spTransform(island_boundary, CRS("+init=epsg:32748"))
# island_boundary_sp= spTransform(island_boundary, CRS("+init=epsg:4326"))
# island_sf_new = island[-c(44,53, 20, 51, 52, 45,11, 27,9, 46, 21, 42, 38),]
# 
# island_boundary_new <- as(st_geometry(island_sf_new$geometry), "Spatial")
# # island_boundary_sp_new= spTransform(island_boundary_new, CRS("+init=epsg:32748"))
# island_boundary_sp_new= spTransform(island_boundary_new, CRS("+init=epsg:4326"))
# 
# total_gym = nrow(gym)
# 
# #gym monte carlo
# n2     <- 500L               
# hdb_to_gym.r <- vector(length = n2) 
# 
# for (i in 1:n2){
#   gym_rand.p <- rpoint(n=total_gym, win=island_boundary)
#   gym_rand.p <- st_as_sf(gym_rand.p, crs = projcrs)
#   curr_nearest_gym <- st_nearest_feature(hdb_sf, gym_rand.p)
#   curr_dist_gym = st_distance(hdb_sf, gym[nearest_gym,], by_element=TRUE)
#   hdb_to_gym.r[i] <- mean(curr_dist_gym)
# }

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


####################################################
##### HDBS WITHIN BUFFER FROM VARIOUS FACILITY #####
####################################################
avg_unit_per_hdb <- 108
avg_resident_per_household <- 3.1
population_per_hdb <- avg_unit_per_hdb * avg_resident_per_household

### Convert df to crs = 4326
sportsfac_with_planning <- st_intersection(sportsfac, island)
gym_with_planning <- st_intersection(gym, island)
fitness_with_planning <- st_intersection(fitness, island)

finalsportsfac <- sportsfac_with_planning %>% st_transform(crs = 3414)
finalgym <- gym_with_planning %>% st_transform(crs = 3414)
finalfitness <- fitness_with_planning %>% st_transform(crs = 3414)

### Convert data to crs = 7801 for accurate distance measurement
finalhdb <- full_hdb_to_facility %>% st_transform(crs = 3414)
d0_sportsfac = 1000
d0_gym = 500
d0_fitness = 500

### Number of HDBs within 1000m of each Sports Facility
finalsportsfac <- finalsportsfac %>% 
  mutate(num_hdb_within_buffer = lengths(st_is_within_distance(x = .,
                                                             y = finalhdb,
                                                             dist = d0_sportsfac)),
         total_population_within_buffer = 
           round(num_hdb_within_buffer * population_per_hdb),
         R = 1 / total_population_within_buffer)

finalsportsfac$R <- ifelse(is.finite(finalsportsfac$R), finalsportsfac$R, 0)

A_value_sportsfac_summed <- finalsportsfac %>% 
  as.data.frame() %>%
  group_by(PLN_AREA_N) %>%
  summarise(a_value = sum(R))
A_value_sportsfac_summed
### Number of HDBs within 500m of each Gym Facility
finalgym <- gym_with_planning %>% 
  mutate(num_hdb_within_buffer = lengths(st_is_within_distance(x = .,
                                                            y = finalhdb,
                                                            dist = d0_gym)),
         total_population_within_buffer = 
           round(num_hdb_within_buffer * population_per_hdb),
         R = 1 / total_population_within_buffer)

finalgym$R <- ifelse(is.finite(finalgym$R), finalgym$R, 0)

A_value_gym_summed <- finalgym %>% 
  as.data.frame() %>%
  group_by(PLN_AREA_N) %>%
  summarise(a_value = sum(R))


### Number of HDBs within 500m of each Fitness Facility
finalfitness <- finalfitness %>% 
  mutate(num_hdb_within_buffer = lengths(st_is_within_distance(x = .,
                                                            y = finalhdb,
                                                            dist = d0_fitness)),
         total_population_within_buffer = 
           round(num_hdb_within_buffer * population_per_hdb),
         R = 1 / total_population_within_buffer)


finalfitness$R <- ifelse(is.finite(finalfitness$R), finalfitness$R, 0)

A_value_fitness_summed <- finalfitness %>% 
  as.data.frame() %>%
  group_by(PLN_AREA_N) %>%
  summarise(a_value = sum(R))


##### CALCULATE ACCESSIBLITY INDEX #####

# Distance-Decay Function
w <- function(shortest_dist, d_0) {
  beta = d_0 / 2
  power_val = -(shortest_dist^2) / beta^2
  result = exp(power_val)
  return (result)
}


# Final Spatial Accessibility Index Function
A <- function(a, w) {
  result = a / w
  cat(w)
  return (result)
}

x <- w(1169, 500)
A(0.0005535, x)
 
# Combine the pre-adjusted a_value for all facility to full_hdb_to_facility
full_hdb_to_facility <- full_hdb_to_facility %>%
  left_join(y=A_value_sportsfac_summed, by=c("town" = "PLN_AREA_N")) %>%
  rename("pre_a_value_sportsfac" = "a_value") %>%
  left_join(y=A_value_gym_summed, by=c("town" = "PLN_AREA_N")) %>%
  rename("pre_a_value_gym" = "a_value") %>%
  left_join(y=A_value_fitness_summed, by=c("town" = "PLN_AREA_N")) %>%
  rename("pre_a_value_fitness" = "a_value")


newTestHDB <- full_hdb_to_facility %>% 
  mutate(a_value_sportsfac = 
           A(pre_a_value_sportsfac, w(as.numeric(dist_to_sportsfac), d0_sportsfac)),
         a_value_gym = 
           A(pre_a_value_gym, w(as.numeric(dist_to_gym), d0_gym)),
         a_value_fitness = 
           A(pre_a_value_fitness, w(as.numeric(dist_to_fitness), d0_fitness)))


a_value_hdb <- newTestHDB %>% group_by(town) %>%
  summarise(avg_a_value_sportsfac = mean(a_value_sportsfac),
            avg_a_value_gym = mean(a_value_gym),
            avg_a_value_fitness = mean(a_value_fitness)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

merged_df2 <- merge(island_geo, a_value_hdb, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)

tm_shape(st_as_sf(merged_df2)) + tm_borders("black") + 
  tm_fill("avg_a_value_sportsfac", by = "PLN_AREA_N")


pre_a_value_hdb <- full_hdb_to_facility %>% group_by(town) %>%
  summarise(avg_prev_a_value_sportsfac = mean(pre_a_value_sportsfac),
            avg_prev_a_value_gym = mean(pre_a_value_gym),
            avg_prev_a_value_fitness = mean(pre_a_value_fitness)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

merged_df2 <- merge(island_geo, pre_a_value_hdb, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)

tm_shape(st_as_sf(merged_df2)) + tm_borders("black") + 
  tm_fill(col = "avg_prev_a_value_sportsfac", 
          style = "jenks",
          palette = "Reds", 
          border.alpha = 0.01, 
          title = "2SFCA")


summary(full_hdb_to_facility)


##### TESTING #####
testrow = 502
test <- finalgym %>% as.data.frame() %>%
  filter(ID == testrow)

temp_single <- st_as_sf(test)
# st_crs(temp_single) <- 4326
temp_single <- st_transform(temp_single, crs = 3414)
buff_single <- st_buffer(temp_single, dist = 500) %>% st_transform(crs = 3414)

tmap_mode("view")
tm_shape(island) + tm_borders("black") +
  # tm_add_legend("fill", 
  #               labels = c("Shopping Malls", "Local Attractions", "Current Listing"),
  #               col = c("blue", "purple", "red"), 
  #               title = "Features") + 
  tm_shape(buff_single) + tm_polygons("yellow", alpha = 0.6) +
  tm_shape(finalhdb) + tm_dots("blue", size = 0.02) +
  tm_shape(st_as_sf(test)) + tm_dots("red", size = 0.04) 


sprintf("Facility %s has %.0f HDBs within 500m buffer", 
        test$ID, test$num_hdb_within_buffer)


hdb1.coords <- st_coordinates(finalhdb)
fac2.coords <- st_coordinates(finalsportsfac)

dist.matrix <- distance(hdb1.coords, fac2.coords, type = "euclidean")
class(dist.matrix)


TSFCA <- ac(p = finalsportsfac$total_population_within_buffer,
            n = 1,
            D = dist.matrix, d0 = 1000, family = "2SFCA")

test2 <- full_hdb_to_facility %>%
  mutate(TSFCA=TSFCA)

tm_shape(island) + tm_borders("black") +
tm_shape(test2, unit = "m") +
  tm_dots(col = "TSFCA", style = "jenks",palette = "Reds", 
              border.alpha = 0, title = "2SFCA")






