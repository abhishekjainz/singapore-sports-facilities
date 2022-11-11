
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


###############################################################################
########################## Monte Carlo & ANN Analysis #########################
###############################################################################
col_fitness <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
col_sports <- c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
col_gym <- c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

#Updating fitness facilities data with latitude and longitude by transforming the geometry
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
ff_ann.p #116.75


##Gyms ANN

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

gym_ann.p <- mean(nndist(gym_ppp, k=1)) 
gym_ann.p #284.88

#Sports_Complex ANN
#process the point data here
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

sc_ann.p <- mean(nndist(sc_ppp, k=1)) 
sc_ann.p #420.25

#ANN Plot for Sports Complex
sc_ANN <- apply(nndist(sc_ppp, k=1:nrow(sports_complex_sf_ap)),2,FUN=mean)
plot(sc_ANN ~ eval(1:nrow(sports_complex_sf_ap)), type="b", las=1, main = "", palette = col_sports, xlab="", ylab="",
     col.axis="blue")
title(main = "ANN Plot for Sports Complex Points",
      xlab = "Neighbour Order", ylab = "ANN",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 1, font.sub = 1, col.sub = "green",
      col.lab ="darkblue"
)


#ANN Plot for Gyms
gym_ANN <- apply(nndist(gym_ppp, k=1:nrow(gym_sf_ap)),2,FUN=mean)
plot(gym_ANN ~ eval(1:nrow(gym_sf_ap)), type="b", las=1, main = "", palette = col_gym, xlab="", ylab="",
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
n     <- 500L               
sc_ann.r <- vector(length = n) 
for (i in 1:n){
  rand.p   <- rpoint(n=total_sc, win=island_boundary_sp_new)  
  sc_ann.r[i] <- mean(nndist(rand.p, k=1))  
}

#plotting on map
plot(rand.p, pch=16, main=NULL, cols="red")

hist(sc_ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(sc_ann.p, sc_ann.r))
abline(v=sc_ann.p, col="blue")

#gym monte carlo
n2     <- 500L               
gym_ann.r <- vector(length = n2) 
for (i in 1:n2){
  gym_rand.p   <- rpoint(n=total_gym, win=island_boundary_sp_new)  
  gym_ann.r[i] <- mean(nndist(gym_rand.p, k=1))  
}

#plotting on map
plot(gym_rand.p, pch=16, main=NULL, cols="blue")

hist(gym_ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(gym_ann.p, gym_ann.r))
abline(v=gym_ann.p, col="blue")


#fitness facilities monte carlo
n3     <- 500L                
ff_ann.r <- vector(length = n3) 
for (i in 1:n3){
  ff_rand.p   <- rpoint(n=total_ff, win=island_boundary_sp_new)  
  ff_ann.r[i] <- mean(nndist(ff_rand.p, k=1))  
}

#plotting on map
plot(ff_rand.p, pch=16, main=NULL, cols="green")

hist(ff_ann.r, main=NULL, las=1, breaks=100, col="bisque", xlim=range(ff_ann.p, ff_ann.r))
abline(v=ff_ann.p, col="blue")


###############################################################################
########################## Hypothesis Testing #################################
###############################################################################
n_sc.greater <- sum(sc_ann.r > sc_ann.p)

p_sc <- min(n_sc.greater + 1, n + 1 - n_sc.greater) / (n +1)
p_sc
#p-value = 0.001996008, <0.05, meaning significant that distribution is nt random

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
#Convert the hdb points as SpatialPoints to be used in the poisson point process model
hdb_cord.dec = SpatialPoints(cbind(hdb_file$LONGITUDE, hdb_file$LATITUDE), proj4string = CRS("+proj=longlat"))

hdb_cord.UTM <- spTransform(hdb_cord.dec, CRS("+init=epsg:32748"))

hdb_ppp = as.ppp.SpatialPoints(hdb_cord.UTM)

hdb_pop = as.im(hdb_ppp)

#sports facilities and hdb:referencing population
PPM1 <- ppm(sc_ppp ~ hdb_pop )
PPM1

PPM0 <- ppm(sc_ppp ~ 1) 
PPM0

anova(PPM0, PPM1, test="LRT")
#Pr(>Chi)| = <2.2e-16
#suggesting that we are able to reject null hypothesis
#which means that the sports facilities distribution is a function of hdb density


#fitness facilities and hdb:referencing population
PPM4 <- ppm(ff_ppp ~ hdb_pop)
PPM4

PPM3 <- ppm(ff_ppp ~ 1)
PPM3

anova(PPM3, PPM4, test="LRT")
anova(PPM4, test="Chi") #P = 0.4258, 
#suggesting that we are unable to reject null hypothesis
#which means that the fitness facilities distribution is not a function of hdb density


#gym facilities and hdb:referencing population
PPM6 <- ppm(gym_ppp ~ hdb_pop)
PPM6

PPM5 <- ppm(gym_ppp ~ 1)
PPM5

anova(PPM5, PPM6, test="LRT")
anova(PPM6, test="Chi") #P = 0.3711
#suggesting that we are unable to reject null hypothesis
#which means that the fitness facilities distribution is not a function of hdb density


###############################################################################
############################### Interpolation #################################
###############################################################################
#temperature_rainfall_data
#load the scraped temperature and rainfall data
tr_file_path = "temp_rain.csv"
tr_csv <- file.path(getwd(), paste(path, tr_file_path, sep=""))
tr_file = read.csv(tr_csv)

names(tr_file)

#changing the datafile into SpatialPointDataFrame
tr_cord.dec = SpatialPoints(cbind(tr_file$Longitude, tr_file$Latitude), proj4string = CRS("+proj=longlat"))
tr_cord.UTM <- spTransform(tr_cord.dec, CRS("+init=epsg:32748"))
tr_cord.UTM@bbox <- island_boundary@bbox
tr_cord.UTM$Temperature <- format(round(tr_file$Temperature, 2), nsmall = 2)

#Creating a new palette as we want to use red for higher temperature and blue for lower temperature
inv_RdBu = c("#2166AC", "#4393C3","#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")

#plot the points on singapore map
tm_shape(island_boundary_sp) + tm_polygons() +
  tm_shape(tr_cord.UTM) +
  tm_dots(col="Temperature", palette = inv_RdBu,
          title="Sampled Temperature", size=0.7) +
  tm_text("Temperature", xmod=.5, size = 0.7) +
  tm_legend(show = FALSE)

# Create an empty grid where n is the total number of cells
tr_cord.UTM2 <- spTransform(tr_cord.dec, CRS("+init=epsg:32748"))
tr_cord.UTM2$Temperature <- format(round(tr_file$Temperature, 2), nsmall = 2)

grd              <- as.data.frame(spsample(tr_cord.UTM2, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(tr_cord.UTM2) <- proj4string(tr_cord.UTM2) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(tr_cord.UTM2)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
tr_cord.UTM.idw <- gstat::idw(Temperature ~ 1, tr_cord.UTM2, newdata=grd, idp=2.0)

# Convert to raster object then clip to SG
r       <- raster(tr_cord.UTM.idw)
r.m     <- mask(r, island_boundary_sp)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = inv_RdBu,
            title="Predicted Temperature") + 
  tm_shape(tr_cord.UTM) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


################################################################################
# Define the 1st order polynomial equation
f.1 <- as.formula(Temperature ~ X + Y) 

# Add X and Y to tr_cord.UTM2
tr_cord.UTM2$X <- coordinates(tr_cord.UTM2)[,1]
tr_cord.UTM2$Y <- coordinates(tr_cord.UTM2)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=tr_cord.UTM2)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, island_boundary_sp)

# Plot the interpolation map with gym facilities
tm_shape(r.m) + 
  tm_raster(n=10,palette = inv_RdBu,
            title="Predicted Temperature") + 
  tm_shape(tr_cord.UTM2) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE) +
  tm_shape(gym_sf) + tm_dots("blue", size = 0.2)

# Plot the interpolation map with sports complex
tm_shape(r.m) + 
  tm_raster(n=10,palette = inv_RdBu,
            title="Predicted Temperature") + 
  tm_shape(tr_cord.UTM2) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE) +
  tm_shape(sports_complex_sf) + tm_dots("purple", size = 0.2)


# Plot the interpolation map with fitness facilities
tm_shape(r.m) + 
  tm_raster(n=10,palette = inv_RdBu,
            title="Predicted Temperature") + 
  tm_shape(tr_cord.UTM2) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE) +
  tm_shape(fitness_facilities_sf) + tm_dots("green", size = 0.2)




############################################################################
########Performing the same interpolation techniques for rainfall
rf_cord.UTM <- spTransform(tr_cord.dec, CRS("+init=epsg:32748"))

rf_cord.UTM@bbox <- island_boundary@bbox

rf_cord.UTM$Rainfall <- format(round(tr_file$Rainfall, 2), nsmall = 2)

tm_shape(island_boundary_sp) + tm_polygons() +
  tm_shape(rf_cord.UTM) +
  tm_dots(col="Rainfall", palette = inv_RdBu,
          title="Sampled Rainfall", size=0.7) +
  tm_text("Rainfall", xmod=.5, size = 0.7) +
  tm_legend(show = FALSE)


# Create an empty grid where n is the total number of cells
rf_cord.UTM2 <- spTransform(tr_cord.dec, CRS("+init=epsg:32748"))
rf_cord.UTM2$Rainfall <- format(round(tr_file$Rainfall, 2), nsmall = 2)

grd              <- as.data.frame(spsample(rf_cord.UTM2, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(rf_cord.UTM2) <- proj4string(rf_cord.UTM2) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(rf_cord.UTM2)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
rf_cord.UTM.idw <- gstat::idw(Rainfall ~ 1, rf_cord.UTM2, newdata=grd, idp=2.0)

# Convert to raster object then clip to SG
r2       <- raster(rf_cord.UTM.idw)
r.m2     <- mask(r2, island_boundary_sp)

# Plot
tm_shape(r.m2) + 
  tm_raster(n=10,palette = "RdBu",
            title="Predicted Rainfall") + 
  tm_shape(rf_cord.UTM) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)




################################################################################
# Define the 1st order polynomial equation
f.2 <- as.formula(Rainfall ~ X + Y) 

# Add X and Y to tr_cord.UTM2
rf_cord.UTM2$X <- coordinates(rf_cord.UTM2)[,1]
rf_cord.UTM2$Y <- coordinates(rf_cord.UTM2)[,2]

# Run the regression model
lm.2 <- lm( f.2, data=rf_cord.UTM2)

# Use the regression model output to interpolate the surface
dat.1st2 <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r3   <- raster(dat.1st2)
r.m3 <- mask(r3, island_boundary_sp)

# Plot the map
tm_shape(r.m3) + 
  tm_raster(n=10,palette = "RdBu",
            title="Predicted Rainfall") + 
  tm_shape(rf_cord.UTM2) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)




################################################################################
######################## Interpolation for participation########################
################################################################################
s_file_path = "sports_participation.csv"
s_csv <- file.path(getwd(), paste(path, s_file_path, sep=""))
s_file = read.csv(s_csv)

names(s_file)

s_cord.dec = SpatialPoints(cbind(s_file$Longitude, s_file$Latitude), proj4string = CRS("+proj=longlat"))

s_cord.UTM <- spTransform(s_cord.dec, CRS("+init=epsg:32748"))

s_cord.UTM@bbox <- island_boundary@bbox

s_cord.UTM$Participation <- format(round(s_file$Participation, 2), nsmall = 2)

inv_RdBu = c("#2166AC", "#4393C3","#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")

tm_shape(island_boundary_sp) + tm_polygons() +
  tm_shape(s_cord.UTM) +
  tm_dots(col="Participation",palette = inv_RdBu,
          title="Sampled Participation", size=0.7) +
  tm_text("Participation", xmod=.5, size = 0.7) +
  tm_legend(show = FALSE)

# Create an empty grid where n is the total number of cells
s_cord.UTM2 <- spTransform(s_cord.dec, CRS("+init=epsg:32748"))
s_cord.UTM2$Temperature <- format(round(s_file$Participation, 2), nsmall = 2)

grd              <- as.data.frame(spsample(s_cord.UTM2, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(s_cord.UTM2) <- proj4string(s_cord.UTM2) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(s_cord.UTM2)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
s_cord.UTM.idw <- gstat::idw(s_file$Participation ~ 1, s_cord.UTM2, newdata=grd, idp=2.0)

# Convert to raster object then clip to SG
r       <- raster(s_cord.UTM.idw)
r.m     <- mask(r, island_boundary_sp)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = inv_RdBu,
            title="Predicted Participation") + 
  tm_shape(s_cord.UTM) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
