
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


################################################################################
######################## Interpolation for participation########################
################################################################################



#Data set loading and tuning
s_file_path = "scraped_data/sports-participation.csv"
s_csv <- file.path(getwd(), paste(path, s_file_path, sep=""))
s_file = read.csv(s_csv)

names(s_file)

s_cord.dec = SpatialPoints(cbind(s_file$Longitude, s_file$Latitude), proj4string = CRS("+proj=longlat"))

s_cord.UTM <- spTransform(s_cord.dec, CRS("+init=epsg:32748"))

s_cord.UTM@bbox <- island_boundary@bbox

s_cord.UTM$Participation <- format(round(s_file$Participation.Rate, 2), nsmall = 2)

inv_RdBu = c("#2166AC", "#4393C3","#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")


### Density Plot for Participation Rate
part_sf <- s_file %>% 
  dplyr::select(-Latitude, -Longitude) %>% 
  merge(island_sf, . , by="PLN_AREA_N", all.x = TRUE)

part_top5 <- part_sf %>%
  arrange(desc(Participation.Rate)) %>%
  top_n(5, Participation.Rate)

tmap_mode("plot")
tm_shape(st_as_sf(part_sf)) + tm_borders("black") +
  tm_fill(col="Participation.Rate",
          style = "jenks",
          palette = rev(inv_RdBu), 
          border.alpha = 0.01, 
          title = "Participation Rate") +
  tm_layout(legend.width = -0.2) +
  tm_shape(st_as_sf(part_top5)) + tm_borders() +
  tm_text("PLN_AREA_N", size=0.5, style = "pretty") 


#IDW technique
# Create an empty grid where n is the total number of cells
s_cord.UTM2 <- spTransform(s_cord.dec, CRS("+init=epsg:32748"))
s_cord.UTM2$Participation <- format(round(s_file$Participation, 2), nsmall = 2)

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
tmap_mode("plot")

result_participation <- tm_shape(r.m) + 
  tm_raster(n=10,palette = rev(inv_RdBu),
            title="Predicted Participation") + 
  tm_shape(s_cord.UTM) + tm_dots(size=0.01) +
  tm_legend(legend.outside=TRUE)

result_participation

# Save output as PNG
tmap_save(result_participation, 
          filename = paste("Interpolation/","Sports_participation_interpolation.png", sep=""))


IDW.out <- vector(length = length(s_cord.UTM))

for (i in 1:length(s_cord.UTM)) {
  IDW.out[i] <- idw(Participation ~ 1, s_cord.UTM[-i,], s_cord.UTM[i,], idp=2.0)$var1.pred
}

OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ s_cord.UTM$Participation, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
    abline(lm(IDW.out ~ s_cord.UTM$Participation), col="red", lw=2,lty=2)
    abline(0,1)
par(OP)

RMSE = sqrt(sum((IDW.out - as.numeric(s_cord.UTM$Participation))^2) / length(s_cord.UTM))
RMSE #0.07888852 when n = 2


#using a different value of n
IDW.out2 <- vector(length = length(s_cord.UTM))

for (i in 1:length(s_cord.UTM)) {
  IDW.out2[i] <- idw(Participation ~ 1, s_cord.UTM[-i,], s_cord.UTM[i,], idp=15.0)$var1.pred
}

OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out2 ~ s_cord.UTM$Participation, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out2 ~ s_cord.UTM$Participation), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

RMSE = sqrt(sum((IDW.out2 - as.numeric(s_cord.UTM$Participation))^2) / length(s_cord.UTM))
RMSE #0.09765187 when n = 15


#using a different value of n
IDW.out3 <- vector(length = length(s_cord.UTM))

for (i in 1:length(s_cord.UTM)) {
  IDW.out3[i] <- idw(Participation ~ 1, s_cord.UTM[-i,], s_cord.UTM[i,], idp=1)$var1.pred
}

OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out3 ~ s_cord.UTM$Participation, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out3 ~ s_cord.UTM$Participation), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

RMSE = sqrt(sum((IDW.out3 - as.numeric(s_cord.UTM$Participation))^2) / length(s_cord.UTM))
RMSE #0.07349721 when n = 1


#0.07349721 n = 1
#0.07888852 n = 2
#0.08537619 n = 3
#0.0898234 n = 4

# NEW RMSE:
# 0.0716 when n = 1
# 0.0772 when n = 2
# 0.0835 when n = 3
# 0.0876 when n = 4
# 0.0945 when n = 15

