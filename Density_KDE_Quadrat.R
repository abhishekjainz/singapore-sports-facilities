
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

#Load the sf for islands
island_sf <- read_sf(dsn = paste(path, "land_boundary/intersection_boundary/", sep = ""), 
                     layer = "island_intersect")

#Load the sf for sports complex
sports_complex_sf <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                             layer = "sports_facilities")

#Load the sf for gyms
gym_sf <- read_sf(dsn = paste(path, "gym_facilities/", sep = ""), 
                  layer = "gym_facilities")


#load MRT Stations data
mrt_station <- read_sf(dsn = paste(path, "mrt_station/", sep = ""), 
                       layer = "MRTLRTStnPtt")

#load fitness facilities data
fitness_facilities_sf <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
                                 layer = "fitness_summarised_points")

col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
col_sports <- c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
col_gym <- c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

#Updating fitness facilities data with latitude and longitude by transforming the geometry
fitness_longlat <- data.frame(st_coordinates(fitness_facilities_sf)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

fitness_facilities_sf <- 
  fitness_facilities_sf %>%
  bind_cols(bind_rows(fitness_longlat))

#Fitness Facilities Datatype conversion
ff_cord.dec = SpatialPoints(cbind(fitness_facilities_sf$Longitude, fitness_facilities_sf$Latitude), proj4string = CRS("+proj=longlat"))

ff_cord.UTM <- spTransform(ff_cord.dec, CRS("+init=epsg:32748"))

ff_ppp = as.ppp.SpatialPoints(ff_cord.UTM)

#sports complex Datatype conversion
sports_complex_sf_ap <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                                layer = "sports_facilities")

sc_longlat <- data.frame(st_coordinates(sports_complex_sf_ap)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

sc_sf_attr <- 
  sports_complex_sf_ap %>%
  bind_cols(bind_rows(sc_longlat))

sc_cord.dec = SpatialPoints(cbind(sc_sf_attr$Longitude, sc_sf_attr$Latitude), proj4string = CRS("+proj=longlat"))

sc_cord.UTM <- spTransform(sc_cord.dec, CRS("+init=epsg:32748"))

sc_ppp = as.ppp.SpatialPoints(sc_cord.UTM)


#gyms Datatype conversion
gym_sf_ap <- read_sf(dsn = paste(path, "gym_facilities/", sep = ""), 
                     layer = "gym_facilities")

gym_longlat <- data.frame(st_coordinates(gym_sf_ap)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")


gym_attr <- 
  gym_sf_ap %>%
  bind_cols(bind_rows(gym_longlat))

gym_cord.dec = SpatialPoints(cbind(gym_attr$Longitude, gym_attr$Latitude), proj4string = CRS("+proj=longlat"))

gym_cord.UTM <- spTransform(gym_cord.dec, CRS("+init=epsg:32748"))

gym_ppp = as.ppp.SpatialPoints(gym_cord.UTM)


#island shapefile conversion
island_boundary <- as(st_geometry(island_sf$geometry), "Spatial")
island_boundary_sp= spTransform(island_boundary, CRS("+init=epsg:32748"))
island_sf_new = island_sf[-c(44,53, 20, 51, 52, 45,11, 27,9, 46, 21, 42, 38),]

island_boundary_new <- as(st_geometry(island_sf_new$geometry), "Spatial")
island_boundary_sp_new= spTransform(island_boundary_new, CRS("+init=epsg:32748"))



hdb_cord.dec = SpatialPoints(cbind(hdb_file$LONGITUDE, hdb_file$LATITUDE), proj4string = CRS("+proj=longlat"))

hdb_cord.UTM <- spTransform(hdb_cord.dec, CRS("+init=epsg:32748"))

hdb_ppp = as.ppp.SpatialPoints(hdb_cord.UTM)

hdb_pop = as.im(hdb_ppp)




###############################################################################
##################### Local Density and Global Density ########################
###############################################################################

##Performing KDE operations on the three different facilities
boundary = as.owin(island_boundary_sp)
plot(boundary)

sc_ppp = sc_ppp[boundary]

K1 <- density(sc_ppp, palette = col_sports) # Using the default bandwidth
plot(K1, main=NULL, las=1, win=island_boundary_sp, palette = col_sports)
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


##Performing Quadrat density for the three different facilities
#Sports complex
Q <- quadratcount(sc_ppp, nx= 10, ny=7)
plot(sc_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid
# Compute the density for each quadrat
Q.d <- intensity(Q)
# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(sc_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

#Gyms
Q2 <- quadratcount(gym_ppp1, nx= 20, ny=10)
plot(gym_ppp1, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q2, add=TRUE)  # Add quadrat grid
# Compute the density for each quadrat
Q2.d <- intensity(Q2)
# Plot the density
plot(intensity(Q2, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(gym_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

#Fitness Facilities
Q3 <- quadratcount(ff_ppp1, nx= 20, ny=10)
plot(ff_ppp1, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q3, add=TRUE)  # Add quadrat grid
# Compute the density for each quadrat
Q3.d <- intensity(Q3)
# Plot the density
plot(intensity(Q3, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(ff_ppp1, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points