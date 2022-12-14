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


install.packages("remotes") 
remotes::install_github("mtennekes/oldtmaptools")
library(oldtmaptools)

path = "data/"

##################################
##### PLANNING AREA BOUNDARY #####
##################################
planbound_file_path = "original_data/master-plan-2019-planning-area-boundary-no-sea/planning-boundary-area.kml"
planbound_kml <- file.path(getwd(), paste(path, planbound_file_path, sep=""))
planbound_sf <- read_sf(planbound_kml)

# Watch the data
planbound_sf %>%
  glimpse()

# See Map
mapviewOptions(fgb = FALSE)
mapview(planbound_sf)

# Get attributes for each observation
attributes <- lapply(X = 1:nrow(planbound_sf), 
                     FUN = function(x) {
                       
                       planbound_sf %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

# Bind attributes to each observation as new columns
planbound_sf_attr <- 
  planbound_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  dplyr::select(-Description)

# Watch new data
planbound_sf_attr %>%
  glimpse()

# New map layers
mapview(planbound_sf_attr, 
        zcol = "REGION_N", 
        layer.name = "Region Name") +
  mapview(planbound_sf_attr,
          zcol = "PLN_AREA_N",
          layer.name = "Planning Area Name")

# Save output as SHP files
planbound_shp <- as_Spatial(st_zm(planbound_sf_attr))
raster::shapefile(planbound_shp, 
                  paste(path,
                        "land_boundary/planning_boundary/planning_boundaries", 
                        sep=""))


############################
##### SUBZONE BOUNDARY #####
############################
subzone_file_path = 'original_data/master-plan-2019-subzone-boundary-no-sea/master-plan-2019-subzone-boundary-no-sea-kml.kml'
subzone_kml <- file.path(getwd(), paste(path, subzone_file_path, sep=""))
subzone_sf <- read_sf(subzone_kml)

# Watch the data
subzone_sf %>%
  glimpse()

# See Map
mapviewOptions(fgb = FALSE)
mapview(subzone_sf)

# Get attributes for each observation
attributes <- lapply(X = 1:nrow(subzone_sf), 
                     FUN = function(x) {
                       
                       subzone_sf %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

# Bind attributes to each observation as new columns
subzone_sf_attr <- 
  subzone_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  dplyr::select(-Description)

# Watch new data
subzone_sf_attr %>%
  glimpse()

# New map layers
mapview(subzone_sf_attr)

# Save output as SHP files
subzone_shp <- as_Spatial(st_zm(subzone_sf_attr))
raster::shapefile(subzone_shp, 
                  paste(path,
                        "land_boundary/subzone_boundary/subzone_boundaries", 
                        sep=""))


#########################################################################
##### INTERSECTION BETWEEN SUBZONE AND PLANNING BOUDNARY MAP LAYERS #####
#########################################################################
combined <- subzone_sf_attr %>% 
  group_by(PLN_AREA_N) %>%
  mutate(TOTAL_AREA_KM2 = sum(Area_km2))

keeps <- c("PLN_AREA_N", "TOTAL_AREA_KM2")
combined <- unique(combined[keeps]) 

df <- inner_join(planbound_sf_attr %>% as.data.frame(), combined2 %>% as.data.frame(), by = "PLN_AREA_N")
df <- subset(df, select = -c(geometry.x, geometry.y))
island <- merge(subzone_agg_planarea, df, 
                by.x="Group.1", by.y="PLN_AREA_N", all.x=TRUE)
island <- subset(island, select = -c(Area_km2))
qtm(island)

# Save output as SHP files
island_shp <- as_Spatial(st_zm(st_as_sf(island)))
raster::shapefile(island_shp, 
                  paste(path,
                        "land_boundary/intersection_boundary/island_intersect", 
                        sep=""))

tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
qtm(planbound_sf_attr)
qtm(subzone_sf_attr)
qtm(island)

###########################
##### FITNESS CORNERS #####
###########################
fitness_file_path = 'original_data/nparks-playfitness-equipment/nparks-play-fitness-equipment-kml.kml'
fitness_kml <- file.path(getwd(), paste(path, fitness_file_path, sep=""))
fitness_sf <- read_sf(fitness_kml)

# Watch the data
fitness_sf %>%
  glimpse()

# See Map
mapviewOptions(fgb = FALSE)
mapview(fitness_sf) # Point Data

# Get attributes for each observation
attributes <- lapply(X = 1:nrow(fitness_sf), 
                     FUN = function(x) {
                       
                       fitness_sf %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

fitness_longlat <- data.frame(st_coordinates(fitness_sf)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

# Bind attributes to each observation as new columns
fitness_sf_attr <- 
  fitness_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  bind_cols(bind_rows(fitness_longlat)) %>%
  dplyr::select(-Description)

# Watch new data
fitness_sf_attr %>%
  glimpse()

# New map layers
mapview(fitness_sf_attr)

# Save output as SHP files
fitness_shp <- as_Spatial(st_zm(fitness_sf_attr))
raster::shapefile(fitness_shp, 
                  paste(path,
                        "fitness_facilities/fitness_facilities", 
                        sep=""), overwrite=TRUE)

new_fitness <- read_sf(dsn = paste(path, "fitness_facilities/", sep=""), 
                                  layer = "fitness_summarised_points")

fitness_export <- new_fitness %>%
  st_drop_geometry()

write.csv(fitness_export, paste(path,"fitness_facilities/fitness_facilities.csv",sep = ""))

#############################
##### SPORTS FACILITIES #####
#############################
# Layers with KML only
sportsfac_file_path = 'original_data/sportsg-sport-facilities/sportsg-sport-facilities-kml.kml'
dus_sportsfac_path = 'original_data/sportsg-dus-sport-facilities/sportsg-dus-sport-facilities-kml.kml'

sportsfac_kml <- file.path(getwd(), paste(path, sportsfac_file_path, sep=""))
sportsfac_sf <- read_sf(sportsfac_kml)

dus_sportsfac_kml <- file.path(getwd(), paste(path, dus_sportsfac_path, sep=""))
dus_sportsfac_sf <- read_sf(dus_sportsfac_kml)
dus_sportsfac_sf

# Layers with SHP files
dusschool_sportsfac_path = 'original_data/dus-schools-sports-facilities/dus-schools-sports-facilities-shp/'

dusschool_sportsfac_sf <- read_sf(dsn = paste(path, dusschool_sportsfac_path, sep=""), 
                               layer = "DUS_School_Sports_Facilities")

# Watch the data
sportsfac_sf %>%
  glimpse() 

# See Map
mapviewOptions(fgb = FALSE)
mapview(sportsfac_sf) # Polygon Data

# Get attributes for each observation
attributes <- lapply(X = 1:nrow(sportsfac_sf), 
                     FUN = function(x) {
                       
                       sportsfac_sf %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

attributes <- lapply(X = 1:nrow(dus_sportsfac_sf), 
                     FUN = function(x) {
                       
                       dus_sportsfac_sf %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

dus_sportsfac_longlat <- data.frame(st_coordinates(dus_sportsfac_sf)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

# Bind attributes to each observation as new columns
sportsfac_sf_attr <- 
  sportsfac_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  dplyr::select(-Description)

dus_sportsfac_sf_attr <- 
  dus_sportsfac_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  bind_cols(bind_rows(dus_sportsfac_longlat)) %>%
  dplyr::select(-Description)

# Watch new data
sportsfac_sf_attr %>%
  glimpse()

# New map layers
mapview(sportsfac_sf_attr)

tm_shape(dus_sportsfac_sf_attr) + tm_dots("black", size = 0.01) +
  tm_shape(dusschool_sportsfac_sf) + tm_dots("red", size = 0.01) +
  tm_shape(sportsfac_as_point) + tm_dots("blue", size = 0.01)

# Save output as SHP files
sportsfac_shp <- as_Spatial(st_zm(sportsfac_sf_attr))
raster::shapefile(sportsfac_shp, 
                  paste(path,
                        "sports_facilities/sports_facilities", 
                        sep=""), overwrite=TRUE)

dus_sportsfac_shp <- as_Spatial(st_zm(dus_sportsfac_sf_attr))
raster::shapefile(dus_sportsfac_shp, 
                  paste(path,
                        "dus_sports_facilities/dus_sports_facilities", 
                        sep=""))


# Function: Get the centroid in the polygon
st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}

st_coordinates(sportsfac_as_point)

sportsfac_as_point = st_centroid_within_poly(sportsfac_sf_attr)
qtm(sportsfac_as_point)

sportsfac_longlat <- data.frame(st_coordinates(sportsfac_as_point)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

sportsfac_as_point_attr <- 
  sportsfac_as_point %>%
  bind_cols(bind_rows(sportsfac_longlat))


# Save output as SHP files
sportsfac_point_shp <- as_Spatial(st_zm(sportsfac_as_point_attr))
raster::shapefile(sportsfac_point_shp, 
                  paste(path,
                        "sports_facilities_points/sports_facilities_points", 
                        sep=""), overwrite=TRUE)


export <- dus_sportsfac_sf_attr %>%
  st_drop_geometry()

export2 <- dusschool_sportsfac_sf %>%
  st_drop_geometry()

export3 <- sportsfac_as_point_attr %>%
  st_drop_geometry()

export4 <- sportsfield_sf %>%
  st_drop_geometry()


write.csv(export, paste(path,"dus_sports_facilities/dus_sports_facilities.csv",sep = ""))
write.csv(export2, paste(path,"dus_school_sports_facilities/dus_school_sports_facilities.csv",sep = ""))
write.csv(export3, paste(path,"sports_facilities/sports_facilities_points.csv",sep = ""))
write.csv(export4, paste(path,"sports_field_facilities/sports_field_facilities.csv",sep = ""))


# Re-import combined Sports Facilities data and export as SHP
full_sportsfac <- read.csv(file = paste(path, "sports_facilities/sports_facilities.csv", sep=""))

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

full_sportsfac_sf <- st_as_sf(x=full_sportsfac, coords = c("Longitude", "Latitude"), crs = projcrs)

full_sportsfac_shp <- as_Spatial(st_zm(full_sportsfac_sf))
raster::shapefile(full_sportsfac_shp, 
                  paste(path,
                        "sports_facilities/sports_facilities", 
                        sep=""))

################
##### GYMS #####
################
gym_file_path = 'original_data/gymssg/gyms-sg-kml.kml'
gym_kml <- file.path(getwd(), paste(path, gym_file_path, sep=""))
gym_sf <- read_sf(gym_kml)

# Watch the data
gym_sf %>%
  glimpse()

# See Map
mapviewOptions(fgb = FALSE)
mapview(gym_sf) # Point Data

# Get attributes for each observation
attributes <- lapply(X = 1:nrow(gym_sf), 
                     FUN = function(x) {
                       
                       gym_sf %>% 
                         slice(x) %>%
                         pull(Description) %>%
                         read_html() %>%
                         html_node("table") %>%
                         html_table(header = TRUE, trim = TRUE, dec = ".", fill = TRUE) %>%
                         as_tibble(.name_repair = ~ make.names(c("Attribute", "Value"))) %>% 
                         pivot_wider(names_from = Attribute, values_from = Value)
                       
                     })

gym_longlat <- data.frame(st_coordinates(gym_sf)) %>%
  rename("Longitude" = "X", "Latitude" = "Y")

# Bind attributes to each observation as new columns
gym_sf_attr <- 
  gym_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  bind_cols(bind_rows(gym_longlat)) %>%
  dplyr::select(-Description)

# Watch new data
gym_sf_attr %>%
  glimpse()

# New map layers
mapview(gym_sf_attr)

# Save output as SHP files
gym_shp <- as_Spatial(st_zm(gym_sf_attr))
raster::shapefile(gym_shp, 
                  paste(path,
                        "gym_facilities/gym_facilities", 
                        sep=""), overwrite=TRUE)

export5 <- gym_sf_attr %>%
  st_drop_geometry()

write.csv(export5, paste(path,"gym_facilities/gym_facilities.csv",sep = ""))


### NEW GYM INFO
gyms_addon <- read.csv(file = paste(path, "/scraped_data/gyms_scraped.csv", sep=""))
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
new_gyms <- st_as_sf(x=gyms_addon, 
                     coords = c("longitude", "latitude"), 
                     crs = projcrs)

tmap_mode("view")
tmap_options(check.and.fix = TRUE)
tm_shape(island) + tm_borders("black") +
  tm_shape(gym_sf_attr) + tm_dots("black", size = 0.01) +
  tm_shape(new_gyms) + tm_dots("blue", size = 0.01)

qtm(sportsfac)


# Re-import combined Gym Facilities data and export as SHP
full_gymfac <- read.csv(file = paste(path, "gym_facilities/gym_facilities.csv", sep=""))

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

full_gymfac_sf <- st_as_sf(x=full_gymfac, coords = c("Longitude", "Latitude"), crs = projcrs)

full_gymfac_shp <- as_Spatial(st_zm(full_gymfac_sf))
raster::shapefile(full_gymfac_shp, 
                  paste(path,
                        "gym_facilities/gym_facilities", 
                        sep=""))

