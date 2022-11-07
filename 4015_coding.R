
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


island_file_path = "intersection_boundary/island_intersect/island_intersect.shp"
island_shp <- file.path(getwd(), paste(path, island_file_path, sep=""))
islands_sf = read_sf(island_shp)

### Creating buffer for HDB, 500m, & make sure all same crs
temp <- st_as_sf(hdb_sf)
st_crs(temp) <- 4326
temp <- st_transform(temp, crs = 7801)

buff <- st_buffer(temp, dist = 500)

buff <- st_transform(buff, crs = 4326)
union <- st_union(buff)
### Map out the buffer for HDB
tmap_mode("plot")

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') +
  tm_shape(union) + tm_polygons("yellow", alpha = 0.2) +
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

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.05) + 
  tm_shape(union2) + tm_polygons("yellow", alpha = 0.2)

#Bring the sf for sports complex & doing buffer
sports_complex_sf <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                  layer = "sports_facilities")

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

temp4 <- st_as_sf(mrt_station)
#st_crs(temp4) <- 4326
#temp4 <- st_transform(temp4, crs = 7801)
buff4 <- st_buffer(temp4, dist = 1000)
buff4 <- st_transform(buff4, crs = 4326)
union4 <- st_union(buff4)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.03) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(sports_complex_sf) + tm_dots("blue", size = 0.2)
  
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.03) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

fitness_facilities_sf <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
                                 layer = "fitness_facilities")

#buffer mrt and fitness facilities
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.03) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("blue", size = 0.2)

#fitness facilities
temp5 <- st_as_sf(fitness_facilities_sf)
st_crs(temp5) <- 4326
temp5 <- st_transform(temp5, crs = 7801)
buff5 <- st_buffer(temp5, dist = 500)
buff5 <- st_transform(buff5, crs = 4326)
union5 <- st_union(buff5)

tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(fitness_facilities_sf) + tm_dots("red", size = 0.05) + 
  tm_shape(union5) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(hdb_sf) + tm_dots("black", size = 0.01)


############################## Points inside buffer ##########################
#######Sports Complex
#count the number of hdb points in the sports complex polygon
buffer_sc_sf_as_sp <- as_Spatial(union3)
hdb_sf_as_sp <- as_Spatial(hdb_sf)
hdb_in_sc_count <- poly.counts(hdb_sf_as_sp, buffer_sc_sf_as_sp)

# find the % of hdb inside sports complex polygon
total_hdb = nrow(hdb_sf)
perc_sc =  hdb_in_sc_count/total_hdb * 100 #~75.69274
perc_sc
print("The percentage of HDB blocks covered within 2km of sports facilities is 75.69%.")

#######Gyms
#count the number of hdb points in the gym polygon (buffer for gym is smaller as we expect ppl to go gyms nearer to them)
buffer_gym_sf_as_sp <- as_Spatial(union2)

hdb_in_gym_count <- poly.counts(hdb_sf_as_sp, buffer_gym_sf_as_sp)

# find the % of hdb inside gym polygon
perc_sc2 =  hdb_in_gym_count/total_hdb * 100
perc_sc2
print("The percentage of HDB blocks covered within 2km of gyms is 21.44%.")

#######Fitness facilities
#count the number of hdb points in the fitness facilities polygon
buffer_ff_sf_as_sp <- as_Spatial(union5)

hdb_in_ff_count <- poly.counts(hdb_sf_as_sp, buffer_ff_sf_as_sp)

# find the % of hdb inside Fitness facilities polygon
perc_sc3 =  hdb_in_ff_count/total_hdb * 100
perc_sc3
print("The percentage of HDB blocks covered within 2km of fitness facilities is 24.61%.")


############################## Points outside MRT buffer ##########################
#count the number of sports complex points in the MRT buffer polygon
buffer_mrt_sf_as_sp <- as_Spatial(union4)
sc_sf_as_sp <- as_Spatial(sports_complex_sf)

sc_in_mrt_count <- poly.counts(sc_sf_as_sp, buffer_mrt_sf_as_sp)

# find the % of sports complex inside MRT buffer polygon
total_sc = nrow(sports_complex_sf)
mrt_sc4 =  sc_in_mrt_count/total_sc * 100
mrt_sc4 #94.29


#count the number of gyms in the MRT buffer polygon
gym_sf_as_sp <- as_Spatial(gym_sf)

gym_in_mrt_count <- poly.counts(gym_sf_as_sp, buffer_mrt_sf_as_sp)

# find the % of gyms inside MRT buffer polygon
total_gym = nrow(gym_sf)
mrt_gym5 =  gym_in_mrt_count/total_gym * 100
mrt_gym5 #93.71

#count the number of Fitness Facilities in the MRT buffer polygon
ff_sf_as_sp <- as_Spatial(fitness_facilities_sf)

ff_in_mrt_count <- poly.counts(ff_sf_as_sp, buffer_mrt_sf_as_sp)

# find the % of Fitness Facilities inside MRT buffer polygon
total_ff = nrow(fitness_facilities_sf)
mrt_ff6 =  ff_in_mrt_count/total_ff * 100
mrt_ff6 #68.92


###############################################################################
########################## Monte Carlo & ANN Analysis #########################
###############################################################################

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
library(tidyverse)
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
ff_ANN <- apply(nndist(ff_ppp, k=1:3108),2,FUN=mean)
plot(ff_ANN ~ eval(1:3108), type="b", las=1, main = "", xlab="", ylab="",
     col.axis="blue")
title(main = "ANN Plot for Fitness Facilities Points",
      xlab = "Neighbour Order", ylab = "ANN",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 1, font.sub = 1, col.sub = "green",
      col.lab ="darkblue"
)

#ANN Plot for Fitness Facilities (first 1000 points)
ff_ANN2 <- apply(nndist(ff_ppp, k=1:1000),2,FUN=mean)
plot(ff_ANN2 ~ eval(1:1000), type="b", las=1, main = "", xlab="", ylab="",
     col.axis="blue")
title(main = "ANN Plot for Fitness Facilities Points(First 1000)",
      xlab = "Neighbour Order", ylab = "ANN",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 1, font.sub = 1, col.sub = "green",
      col.lab ="darkblue"
)