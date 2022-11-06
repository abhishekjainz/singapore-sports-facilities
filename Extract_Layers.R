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

#############################
##### SPORTS FACILITIES #####
#############################
sportsfac_file_path = 'original_data/sportsg-sport-facilities/sportsg-sport-facilities-kml.kml'
sportsfac_kml <- file.path(getwd(), paste(path, sportsfac_file_path, sep=""))
sportsfac_sf <- read_sf(sportsfac_kml)

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

# Bind attributes to each observation as new columns
sportsfac_sf_attr <- 
  sportsfac_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  dplyr::select(-Description)

sportsfac_sf_attr
# Watch new data
sportsfac_sf_attr %>%
  glimpse()

# New map layers
mapview(sportsfac_sf_attr)

# Save output as SHP files
sportsfac_shp <- as_Spatial(st_zm(sportsfac_sf_attr))
raster::shapefile(sportsfac_shp, 
                  paste(path,
                        "sports_facilities/sports_facilities", 
                        sep=""), overwrite=TRUE)


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

sportsfac_as_point = st_centroid_within_poly(sportsfac_sf_attr)
qtm(sportsfac_as_point)

# Save output as SHP files
sportsfac_point_shp <- as_Spatial(st_zm(sportsfac_as_point))
raster::shapefile(sportsfac_point_shp, 
                  paste(path,
                        "sports_facilities_points/sports_facilities_points", 
                        sep=""), overwrite=TRUE)

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

