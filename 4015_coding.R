
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

#
#island_file_path = "intersection_boundary/island_intersect/island_intersect.shp"
#island_shp <- file.path(getwd(), paste(path, island_file_path, sep=""))
#islands_sf = read_sf(island_shp)


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

# MRT Stations
mrt_station <- read_sf(dsn = paste(path, "mrt_station/", sep = ""), 
                       layer = "MRTLRTStnPtt")

#fitness facilities
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


##################### Varying HDB and MRT buffer size ########################
total_hdb = nrow(hdb_sf)
total_sc = nrow(sports_complex_sf)
total_ff = nrow(fitness_facilities_sf)
total_gym = nrow(gym_sf)
#Buffer starting from HDB to get gym counts
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(union_hdb_1k) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.1) 


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
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.2) 

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
  tm_shape(gym_sf) + tm_dots("blue", size = 0.1) 

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
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.2) 

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
  tm_shape(gym_sf) + tm_dots("blue", size = 0.1) 

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
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.2) 

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



############################## Points inside buffer ##########################
temp4 <- st_as_sf(mrt_station)
buff4 <- st_buffer(temp4, dist = 1000)
buff4 <- st_transform(buff4, crs = 4326)
union4 <- st_union(buff4)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.2)

#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp <- as_Spatial(union4)
sc_sf_as_sp <- as_Spatial(sports_complex_sf)

sc_in_mrt_count <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp)

# find the % of sports complex inside MRT 1km buffer polygon
mrt_sc4 =  sc_in_mrt_count/total_sc * 100
mrt_sc4 #81.09


tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

gym_sf_as_sp <- as_Spatial(gym_sf)
gym_in_mrt_count <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp)
mrt_gym5 =  gym_in_mrt_count/total_gym * 100
mrt_gym5 #88.74


#--------------------------------------------------------------
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)

ff_sf_as_sp <- as_Spatial(fitness_facilities_sf)

ff_in_mrt_count <- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp)

# find the % of Fitness Facilities inside MRT buffer polygon

mrt_ff6 =  ff_in_mrt_count/total_ff * 100
mrt_ff6 #80.77


buff5 <- st_buffer(temp4, dist = 2000)
buff5 <- st_transform(buff5, crs = 4326)
union5 <- st_union(buff5)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.2)

#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp_2k <- as_Spatial(union5)

sc_in_mrt_count_2k <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp_2k)

# find the % of sports complex inside MRT 1km buffer polygon
mrt_sc_2k =  sc_in_mrt_count_2k/total_sc * 100
mrt_sc_2k #99.58


tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

gym_in_mrt_2k <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp_2k)
mrt_gym_2k =  gym_in_mrt_2k/total_gym * 100
mrt_gym_2k #99.12


#--------------------------------------------------------------
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)


ff_in_mrt_2k<- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp_2k)

# find the % of Fitness Facilities inside MRT buffer polygon

mrt_ff_2k =  ff_in_mrt_2k/total_ff * 100
mrt_ff_2k #98.34


buff6 <- st_buffer(temp4, dist = 500)
buff6 <- st_transform(buff6, crs = 4326)
union6 <- st_union(buff6)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union6) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("red", size = 0.2)

#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp_500 <- as_Spatial(union6)

sc_in_mrt_count_500 <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp_500)

# find the % of sports complex inside MRT 1km buffer polygon
mrt_sc_500 =  sc_in_mrt_count_500/total_sc * 100
mrt_sc_500 #39.08


tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.2) +
  tm_shape(union6) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

gym_in_mrt_500 <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp_500)
mrt_gym_500 =  gym_in_mrt_500/total_gym * 100
mrt_gym_500 #69.76


#--------------------------------------------------------------
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.03) +
  tm_shape(union6) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)


ff_in_mrt_500<- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp_500)

# find the % of Fitness Facilities inside MRT buffer polygon

mrt_ff_500 =  ff_in_mrt_500/total_ff * 100
mrt_ff_500

###############################################################################
########################## Monte Carlo & ANN Analysis #########################
###############################################################################
fitness_longlat <- data.frame(st_coordinates(fitness_facilities_sf)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

fitness_facilities_sf <- 
  fitness_facilities_sf %>%
  bind_cols(bind_rows(fitness_longlat))

##ANN
#Fitness Facilities ANN
ff_cord.dec = SpatialPoints(cbind(fitness_facilities_sf$Longitude, fitness_facilities_sf$Latitude), proj4string = CRS("+proj=longlat"))

ff_cord.UTM <- spTransform(ff_cord.dec, CRS("+init=epsg:32748"))

ff_ppp = as.ppp.SpatialPoints(ff_cord.UTM)

ff_ann.p <- mean(nndist(ff_ppp, k=1)) 
ff_ann.p


##Gyms ANN
gym_cord.dec = SpatialPoints(cbind(gym_sf$Longitude, gym_sf$Latitude), proj4string = CRS("+proj=longlat"))

gym_cord.UTM <- spTransform(gym_cord.dec, CRS("+init=epsg:32748"))

gym_ppp = as.ppp.SpatialPoints(gym_cord.UTM)

gym_ann.p <- mean(nndist(gym_ppp, k=1)) 
gym_ann.p

#Sports_Complex ANN
#process the point data here
sports_complex_sf_ap <- read_sf(dsn = paste(path, "sports_facilities_points/", sep = ""), 
                             layer = "sports_facilities_points")
sc_longlat <- data.frame(st_coordinates(sports_complex_sf_ap)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

sc_sf_attr <- 
  sports_complex_sf_ap %>%
  bind_cols(bind_rows(sc_longlat))

sc_cord.dec = SpatialPoints(cbind(sc_sf_attr$Longitude, sc_sf_attr$Latitude), proj4string = CRS("+proj=longlat"))

sc_cord.UTM <- spTransform(sc_cord.dec, CRS("+init=epsg:32748"))

sc_ppp = as.ppp.SpatialPoints(sc_cord.UTM)

sc_ann.p <- mean(nndist(sc_ppp, k=1)) 
sc_ann.p

#ANN Plot for Sports Complex
sc_ANN <- apply(nndist(sc_ppp, k=1:35),2,FUN=mean)
plot(sc_ANN ~ eval(1:35), type="b", las=1, main = "", xlab="", ylab="",
     col.axis="blue")
title(main = "ANN Plot for Sports Complex Points",
      xlab = "Neighbour Order", ylab = "ANN",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 1, font.sub = 1, col.sub = "green",
      col.lab ="darkblue"
)


#ANN Plot for Gyms
gym_ANN <- apply(nndist(gym_ppp, k=1:159),2,FUN=mean)
plot(gym_ANN ~ eval(1:159), type="b", las=1, main = "", xlab="", ylab="",
     col.axis="blue")
title(main = "ANN Plot for Gym Points",
      xlab = "Neighbour Order", ylab = "ANN",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 1, font.sub = 1, col.sub = "green",
      col.lab ="darkblue"
)

#ANN Plot for Fitness Facilities (all points)
ff_ANN <- apply(nndist(ff_ppp, k=1:723),2,FUN=mean)
plot(ff_ANN ~ eval(1:723), type="b", las=1, main = "", xlab="", ylab="",
     col.axis="blue")
title(main = "ANN Plot for Fitness Facilities Points",
      xlab = "Neighbour Order", ylab = "ANN",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 1, font.sub = 1, col.sub = "green",
      col.lab ="darkblue"
)

##Monte Carlo
#creating the boundaries
island_boundary <- as(st_geometry(island_sf$geometry), "Spatial")
island_boundary_sp= spTransform(island_boundary, CRS("+init=epsg:32748"))
island_sf_new = island_sf[-c(44,53, 20, 51, 52, 45,11, 27,9, 46, 21, 42, 38),]

island_boundary_new <- as(st_geometry(island_sf_new$geometry), "Spatial")
island_boundary_sp_new= spTransform(island_boundary_new, CRS("+init=epsg:32748"))


#sports complex monte carlo
n     <- 1000L               
sc_ann.r <- vector(length = n) 
for (i in 1:n){
  rand.p   <- rpoint(n=total_sc, win=island_boundary_sp_new)  
  sc_ann.r[i] <- mean(nndist(rand.p, k=1))  
}

#plotting on map
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0.5,0.5,1))

hist(sc_ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(sc_ann.p, sc_ann.r))
abline(v=sc_ann.p, col="blue")

#gym monte carlo
n2     <- 500L        #lowered the number of simulations to save run time        
gym_ann.r <- vector(length = n2) 
for (i in 1:n2){
  gym_rand.p   <- rpoint(n=total_gym, win=island_boundary_sp_new)  
  gym_ann.r[i] <- mean(nndist(gym_rand.p, k=1))  
}

#plotting on map
plot(gym_rand.p, pch=16, main=NULL, cols=rgb(0,0.5,0.5,1))

hist(gym_ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(gym_ann.p, gym_ann.r))
abline(v=gym_ann.p, col="blue")


#fitness facilities monte carlo
n3     <- 500L        #lowered the number of simulations to save run time        
ff_ann.r <- vector(length = n3) 
for (i in 1:n3){
  ff_rand.p   <- rpoint(n=total_ff, win=island_boundary_sp_new)  
  ff_ann.r[i] <- mean(nndist(ff_rand.p, k=1))  
}

#plotting on map
plot(ff_rand.p, pch=16, main=NULL, cols=rgb(0,0.5,0.5,1))

hist(ff_ann.r, main=NULL, las=1, breaks=100, col="bisque", xlim=range(ff_ann.p, ff_ann.r))
abline(v=ff_ann.p, col="blue")


###############################################################################
########################## Hypothesis Testing #################################
###############################################################################
n_sc.greater <- sum(sc_ann.r > sc_ann.p)

p_sc <- min(n_sc.greater + 1, n + 1 - n_sc.greater) / (n +1)
p_sc
#p-value = 0.02297702, <0.05, meaning significant that distribution is nt random

n_gym.greater <- sum(gym_ann.r > gym_ann.p)

p_gym <- min(n_gym.greater + 1, n2 + 1 - n_gym.greater) / (n2 +1)
p_gym
#p-value = 0.001996008, <0.05, meaning significant that distribution is nt random

n_ff.greater <- sum(ff_ann.r > ff_ann.p)

p_ff <- min(n_ff.greater + 1, n3 + 1 - n_ff.greater) / (n3 +1)
p_ff
#p-value = 0.001996008, <0.05, meaning significant that distribution is nt random


###############################################################################
########################## poisson point process model ########################
###############################################################################

hdb_cord.dec = SpatialPoints(cbind(hdb_file$LONGITUDE, hdb_file$LATITUDE), proj4string = CRS("+proj=longlat"))

hdb_cord.UTM <- spTransform(hdb_cord.dec, CRS("+init=epsg:32748"))

hdb_ppp = as.ppp.SpatialPoints(hdb_cord.UTM)

hdb_pop = as.im(hdb_ppp)

#sports facilities and hdb:referencing population
PPM1 <- ppm(sc_ppp ~ hdb_pop )
PPM1

PPM0 <- ppm(sc_ppp ~ 1) 
PPM0

sstable = anova(PPM0, PPM1, test="LRT")

kable(sstable, digits = 3)
#Pr(>Chi)| = 0.272, actually suggest that the location of sports facilities are related to population density


#sports facilities and hdb:referencing population

PPM4 <- ppm(ff_ppp ~ hdb_pop)
PPM4

anova(PPM4, test="Chi") #P = 0.4258

PPM6 <- ppm(gym_ppp ~ hdb_pop)
PPM6

anova(PPM6, test="Chi") #P = 0.02633







###############################################################################
##################### Local Density and Global Density ########################
###############################################################################
###ppp : sc_ppp and the two others
boundary = as.owin(island_boundary_sp)
plot(boundary)

sc_ppp = sc_ppp[boundary]

K1 <- density(sc_ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1, win=island_boundary_sp)
contour(K1, add=TRUE)


##For gyms
gym_ppp1 = gym_ppp[boundary]

K2 <- density(gym_ppp1) # Using the default bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)


##For fitness facilities
ff_ppp1 = ff_ppp[boundary]

K3 <- density(ff_ppp1) # Using the default bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)


##quadrat
Q <- quadratcount(sc_ppp, nx= 20, ny=10)

plot(sc_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid

# Compute the density for each quadrat
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(sc_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

##quadrat
Q2 <- quadratcount(gym_ppp1, nx= 20, ny=10)

plot(gym_ppp1, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q2, add=TRUE)  # Add quadrat grid

# Compute the density for each quadrat
Q2.d <- intensity(Q2)

# Plot the density
plot(intensity(Q2, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(gym_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

##quadrat
Q3 <- quadratcount(ff_ppp1, nx= 20, ny=10)

plot(ff_ppp1, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q3, add=TRUE)  # Add quadrat grid

# Compute the density for each quadrat
Q3.d <- intensity(Q3)

# Plot the density
plot(intensity(Q3, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(ff_ppp1, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points


###############################################################################
############################### Interpolation #################################
###############################################################################
#temperature_rainfall_data
tr_file_path = "temp_rain.csv"
tr_csv <- file.path(getwd(), paste(path, tr_file_path, sep=""))
tr_file = read.csv(tr_csv)

names(tr_file)

tr_cord.dec = SpatialPoints(cbind(tr_file$Longitude, tr_file$Latitude), proj4string = CRS("+proj=longlat"))

tr_cord.UTM <- spTransform(tr_cord.dec, CRS("+init=epsg:32748"))

tr_cord.UTM@bbox <- island_boundary@bbox

tr_cord.UTM$Temperature <- format(round(tr_file$Temperature, 2), nsmall = 2)

tm_shape(island_boundary_sp) + tm_polygons() +
  tm_shape(tr_cord.UTM) +
  tm_dots(col="Temperature", palette = "YlOrRd",
          title="Sampled Temperature", size=0.7) +
  tm_text("Temperature", xmod=.5, size = 0.7) +
  tm_legend(show = FALSE)












# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(tr_cord.UTM, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(tr_cord.UTM) <- proj4string(tr_cord.UTM) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(tr_cord.UTM)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
tr_cord.UTM.idw <- gstat::idw(Temperature ~ 1, tr_cord.UTM, newdata=grd, idp=2.0)

# Convert to raster object then clip to SG
r       <- raster(tr_cord.UTM.idw)
r.m     <- mask(r, island_boundary_sp)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "YlOrRd",
            title="Predicted Temperature") + 
  tm_shape(tr_cord.UTM) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)












#start of kriging

f.1 <- as.formula(Temperature ~ X + Y) 
var.smpl <- variogram(f.1, tr_cord.UTM, cloud = FALSE, cutoff=1000000, width=89900)












