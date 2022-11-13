
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
library(spdep)

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

# projcrs <- "+proj=tmerc +lat_0=1.36666666666667 +lon_0=103.833333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
hdb_file_path = "HDB/hdb_full.csv"
hdb_csv <- file.path(getwd(), paste(path, hdb_file_path, sep=""))
hdb_file <- read.csv(hdb_csv)
names(hdb_file)
hdb_sf <- st_as_sf(hdb_file, 
                   coords = c("LONGITUDE", "LATITUDE"), 
                   crs = "WGS84") %>%
  relocate(POSTAL)

mrt_station <- read_sf(dsn = paste(path, "mrt_station/", sep = ""), 
                       layer = "MRTLRTStnPtt")

tm_shape(mrt_station) + tm_dots("purple") +
tm_shape(mrt_station2) + tm_dots("red")
nrow(hdb_sf)
#####################################
##### NEAREST FACILITY FROM HDB #####
#####################################
fitness <- st_transform(fitness, "WGS84")

# st_is_longlat(fitness)
# compareCRS(hdb_sf, fitness)

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
sportsfac <- st_transform(sportsfac, "WGS84")

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
gym <- st_transform(gym, "WGS84")

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
                                       sportsfac_coord, 
                                       gym_coord,
                                       fitness_coord)
hdb_to_facility_combine_coord <- cbind(hdb_to_facility_combine_coord, 
                                       dist_sportsfac_df, 
                                       dist_gym_df,
                                       dist_fitness_df)

colnames(hdb_to_facility_combine_coord) <- 
  c("hdb_long", "hdb_lat", "sportsfac_long", "sportsfac_lat", 
    "gym_long", "gym_lat", "fitness_long", "fitness_lat", 
    "dist_to_sportsfac", "dist_to_gym", "dist_to_fitness")

full_hdb_to_facility <- cbind(hdb_sf, hdb_to_facility_combine_coord)


#########################
##### MEAN DISTANCE #####
#########################
mean(dist_sportsfac)
mean(dist_gym)
mean(dist_fitness)

island_df <- island %>% as.data.frame()
island_geo <- island_df[,c(1,10)]

avg_dist_to_facilities <- full_hdb_to_facility %>% group_by(town) %>%
  summarise(avg_dist_to_sportsfac = mean(dist_to_sportsfac),
            avg_dist_to_gym = mean(dist_to_gym),
            avg_dist_to_fitness = mean(dist_to_fitness)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

merged_df <- merge(island_geo, avg_dist_to_facilities, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)

col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
col_sports <- c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
col_gym <- c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

top_5_avgdist.sportsfac <- merged_df %>% 
  arrange(desc(avg_dist_to_sportsfac)) %>% 
  top_n(5, avg_dist_to_sportsfac)

top_5_avgdist.gym <- merged_df %>% 
  arrange(desc(avg_dist_to_gym)) %>% 
  top_n(5, avg_dist_to_gym)

top_5_avgdist.fitness <- merged_df %>% 
  arrange(desc(avg_dist_to_fitness)) %>% 
  top_n(5, avg_dist_to_fitness)


plot_sportsfac_avg_dist <- tm_shape(st_as_sf(merged_df)) + 
  tm_borders("black") + 
  tm_fill("avg_dist_to_sportsfac", by="PLN_AREA_N", palette = col_sports) +
  tm_shape(st_as_sf(top_5_avgdist.sportsfac)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.6, style = "pretty")

plot_gym_avg_dist <- tm_shape(st_as_sf(merged_df)) + 
  tm_borders("black") + 
  tm_fill("avg_dist_to_gym", by="PLN_AREA_N", palette = col_gym) + 
  tm_shape(st_as_sf(top_5_avgdist.gym)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.6, style = "pretty")

plot_fitness_avg_dist <- tm_shape(st_as_sf(merged_df)) + 
  tm_borders("black") + 
  tm_fill("avg_dist_to_fitness", by="PLN_AREA_N", palette = col_fitness) +
  tm_shape(st_as_sf(top_5_avgdist.fitness)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.5, style = "pretty", auto.placement = TRUE)

# Save output as PNG
tmap_save(plot_sportsfac_avg_dist, 
          filename = paste(plot_path,"sportsfac_avg_dist.png", sep=""))
tmap_save(plot_gym_avg_dist, 
          filename = paste(plot_path,"gym_avg_dist.png", sep=""))
tmap_save(plot_fitness_avg_dist, 
          filename = paste(plot_path,"fitness_avg_dist.png", sep=""))


# tmap_mode("plot")
# col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
# plot_fitness_density <- tm_shape(st_as_sf(fitness_count_with_area)) +
#   tm_fill("Fitness_Density", title = "Fitness Facilities Density", palette = col_fitness) + 
#   tm_borders('black') +
#   tm_layout(
#     legend.title.size = 1,
#     legend.text.size = 0.8,
#     legend.position = c("right", "bottom"),
#     legend.width = -0.3,
#     legend.bg.color = "white",
#     legend.bg.alpha = 1)


####################################################
##### HDBS WITHIN BUFFER FROM VARIOUS FACILITY #####
####################################################
avg_unit_per_hdb <- 108
avg_resident_per_household <- 3.1
population_per_hdb <- avg_unit_per_hdb * avg_resident_per_household
island <- st_transform(island, "WGS84")

### Convert df to crs = 4326
sportsfac_with_planning <- st_intersection(sportsfac, island)
gym_with_planning <- st_intersection(gym, island)
fitness_with_planning <- st_intersection(fitness, island)

qtm(fitness_with_planning)

finalsportsfac <- sportsfac_with_planning 
finalgym <- gym_with_planning 
finalfitness <- fitness_with_planning 


### Convert data to crs = 7801 for accurate distance measurement
finalhdb <- full_hdb_to_facility 

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


########################################
##### CALCULATE ACCESSIBLITY INDEX #####
########################################
# Combine the pre-adjusted a_value for all facility to full_hdb_to_facility
full_hdb_to_facility <- full_hdb_to_facility %>%
  left_join(y=A_value_sportsfac_summed, by=c("town" = "PLN_AREA_N")) %>%
  rename("pre_a_value_sportsfac" = "a_value") %>%
  left_join(y=A_value_gym_summed, by=c("town" = "PLN_AREA_N")) %>%
  rename("pre_a_value_gym" = "a_value") %>%
  left_join(y=A_value_fitness_summed, by=c("town" = "PLN_AREA_N")) %>%
  rename("pre_a_value_fitness" = "a_value")


pre_a_value_hdb <- full_hdb_to_facility %>% group_by(town) %>%
  summarise(avg_prev_a_value_sportsfac = mean(pre_a_value_sportsfac),
            avg_prev_a_value_gym = mean(pre_a_value_gym),
            avg_prev_a_value_fitness = mean(pre_a_value_fitness)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

merged_df2 <- merge(island_geo, pre_a_value_hdb, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)

top_5_A.sportsfac <- merged_df2 %>% 
  arrange(desc(avg_prev_a_value_sportsfac)) %>% 
  top_n(5, avg_prev_a_value_sportsfac)

top_5_A.gym <- merged_df2 %>% 
  arrange(desc(avg_prev_a_value_gym)) %>% 
  top_n(5, avg_prev_a_value_gym)

top_5_A.fitness <- merged_df2 %>% 
  arrange(desc(avg_prev_a_value_fitness)) %>% 
  top_n(5, avg_prev_a_value_fitness)


### A-value for SportsFac
plot_sportsfac_A_index <- tm_shape(st_as_sf(merged_df2)) + tm_borders("black") + 
  tm_fill(col = "avg_prev_a_value_sportsfac", 
          style = "jenks",
          palette = "Purples", 
          border.alpha = 0.01, 
          title = "2SFCA") +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("right", "bottom"),
    legend.width = -0.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1) +
  tm_shape(st_as_sf(top_5_A.sportsfac)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.6, style = "pretty")

### A-value for Gym
plot_gym_A_index <- tm_shape(st_as_sf(merged_df2)) + tm_borders("black") + 
  tm_fill(col = "avg_prev_a_value_gym", 
          style = "jenks",
          palette = "Blues", 
          border.alpha = 0.01, 
          title = "2SFCA") +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("right", "bottom"),
    legend.width = -0.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1) + 
  tm_shape(st_as_sf(top_5_A.gym)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.6, style = "pretty")

### A-value for Fitness
plot_fitness_A_index <- tm_shape(st_as_sf(merged_df2)) + tm_borders("black") + 
  tm_fill(col = "avg_prev_a_value_fitness", 
          style = "jenks",
          palette = "Greens", 
          border.alpha = 0.01, 
          title = "2SFCA") +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("right", "bottom"),
    legend.width = -0.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1) +
  tm_shape(st_as_sf(top_5_A.fitness)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.6, style = "pretty") 
# + tm_scale_bar(position=c("right", "bottom"))
  
tmap_mode("plot")
plot_sportsfac_A_index
plot_gym_A_index
plot_fitness_A_index

# Save output as PNG
tmap_save(plot_sportsfac_A_index, 
          filename = paste(plot_path,"sportsfac_A_score.png", sep=""))
tmap_save(plot_gym_A_index, 
          filename = paste(plot_path,"gym_A_score.png", sep=""))
tmap_save(plot_fitness_A_index, 
          filename = paste(plot_path,"fitness_A_score.png", sep=""))

###################################
##### SPATIAL AUTOCORRELATION #####
###################################

# Null Hypothesis: 
# There is no spatial clustering of the values associated with the geographic 
# features in the study area 
# (i.e. attribute values are randomly distributed across the study area)

ac <- merged_df2 %>% 
  replace(is.na(.), 0)

# Adopting Contiguous Neighbour --> Share Same Vertex
nb <- poly2nb(st_as_sf(ac), queen=TRUE)

# Check if the neighbours are accurate
nb[[4]] # 17, 18, 33
merged_df2$PLN_AREA_N[4] # "BOON LAY"
merged_df2$PLN_AREA_N[c(17,18,33)] # "JURONG EAST" "JURONG WEST" "PIONEER"  

# Assigning Weights
lw <- nb2listw(nb, style="W", zero.policy=TRUE) # style="w" means equal weight
lw$weights[4] # Check the weights of neighbours for Polygon 4
set.ZeroPolicyOption(TRUE)


##### SPORTS FACILITIES #####
A.sportsfac.lag <- lag.listw(lw, ac$avg_prev_a_value_sportsfac, NAOK=TRUE)

### Monte Carlo Method
M <- lm(A.sportsfac.lag ~ ac$avg_prev_a_value_sportsfac)
plot( A.sportsfac.lag ~ ac$avg_prev_a_value_sportsfac, pch=20, asp=1, las=1)

coef(M)[2]

n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector

for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(ac$avg_prev_a_value_sportsfac, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(lw, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(M)[2], col="red")

N.greater <- sum(coef(M)[2] > I.r)
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p

### Statistical Method
# Get the Moran's I value
moran.test(ac$avg_prev_a_value_sportsfac, lw)
MC.sportsfac <- moran.mc(ac$avg_prev_a_value_sportsfac, lw, nsim=599)
MC.sportsfac
# Plot the distribution (Density Plot)
plot(MC.sportsfac, main="", las=1)


##### GYM #####
A.gym.lag <- lag.listw(lw, ac$avg_prev_a_value_gym, NAOK=TRUE)

# Statistical Method
# Get the Moran's I value
moran.test(ac$avg_prev_a_value_gym, lw)
MC.gym <- moran.mc(ac$avg_prev_a_value_gym, lw, nsim=599)
MC.gym

# Plot the distribution (Density Plot)
plot(MC.gym, main="", las=1)


moran <- moran.plot(ac$avg_prev_a_value_gym, lw)
local <- localmoran(x = ac$avg_prev_a_value_gym, lw)

# binds results to our polygon shapefile
moran.map <- cbind(ac, local)

tmap_mode("plot")
tm_shape(st_as_sf(moran.map)) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic") 


##### FITNESS FACILITIES #####
A.fitness.lag <- lag.listw(lw, ac$avg_prev_a_value_fitness, NAOK=TRUE)

# Statistical Method
# Get the Moran's I value
moran.test(ac$avg_prev_a_value_fitness, lw)
MC.fitness <- moran.mc(ac$avg_prev_a_value_fitness, lw, nsim=599)
MC.fitness

# Plot the distribution (Density Plot)
plot(MC.fitness, main="", las=1)


sprintf("Sports Facilities has a Global Moran's I value of %.4f, and a p-value of %.4f", 
        MC.sportsfac$statistic, MC.sportsfac$p.value)
sprintf("Gym has a Global Moran's I value of %.4f, and a p-value of %.4f", 
        MC.gym$statistic, MC.gym$p.value)
sprintf("Fitness Facilities has a Global Moran's I value of %.4f, and a p-value of %.4f", 
        MC.fitness$statistic, MC.fitness$p.value)


##### TESTING #####
testrow = 188
test <- finalgym %>% as.data.frame() %>%
  filter(ID == testrow)

temp_single <- st_as_sf(test)
# st_crs(temp_single) <- 4326
temp_single <- st_transform(temp_single, crs = "WGS84")
buff_single <- st_buffer(temp_single, dist = 500) %>% st_transform(crs = "WGS84")

tmap_mode("view")
tm_shape(island) + tm_borders("black") +
  tm_add_legend("fill",
                labels = c("HDB", "Facility"),
                col = c("blue", "red"),
                title = "Features") +
  tm_shape(buff_single) + tm_polygons("yellow", alpha = 0.6) +
  tm_shape(finalhdb) + tm_dots("blue", size = 0.02) +
  tm_shape(st_as_sf(test)) + tm_dots("red", size = 0.04) 


sprintf("Facility %s has %.0f HDBs within 500m buffer", 
        test$ID, test$num_hdb_within_buffer)



# ############################## EXTRA CODES ##############################
# # Distance-Decay Function [NOT USED]
# w <- function(shortest_dist, d_0) {
#   beta = d_0 / 2
#   power_val = -(shortest_dist^2) / beta^2
#   result = exp(power_val)
#   return (result)
# }
# 
# 
# # Final Spatial Accessibility Index Function [NOT USED]
# A <- function(a, w) {
#   result = a / w
#   return (result)
# }
# 
# 
# 
# # newTestHDB <- full_hdb_to_facility %>% 
# #   mutate(a_value_sportsfac = 
# #            A(pre_a_value_sportsfac, w(as.numeric(dist_to_sportsfac), d0_sportsfac)),
# #          a_value_gym = 
# #            A(pre_a_value_gym, w(as.numeric(dist_to_gym), d0_gym)),
# #          a_value_fitness = 
# #            A(pre_a_value_fitness, w(as.numeric(dist_to_fitness), d0_fitness))) %>%
# #   relocate(POSTAL)
# #   
# 
# tm_shape(newTestHDB) + tm_dots(col="black") + 
#   tm_shape(fitness) + tm_dots(col="red")
# 
# a_value_hdb <- newTestHDB %>% group_by(town) %>%
#   summarise(avg_a_value_sportsfac = mean(a_value_sportsfac),
#             avg_a_value_gym = mean(a_value_gym),
#             avg_a_value_fitness = mean(a_value_fitness)) %>% 
#   as.data.frame() %>% 
#   dplyr::select(-geometry)
# 
# merged_df2 <- merge(island_geo, a_value_hdb, by.x="PLN_AREA_N", by.y="town", all.x=TRUE)
# 
# tm_shape(st_as_sf(merged_df2)) + tm_borders("black") + 
#   tm_fill("avg_a_value_gym", by = "PLN_AREA_N") + 

