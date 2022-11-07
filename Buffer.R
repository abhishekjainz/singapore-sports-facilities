
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
island_shp <- file.path(getwd(), paste(path, hdb_file_path, sep=""))
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
  tm_shape(union) + tm_polygons("grey", alpha = 0.2) +
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
  tm_shape(union2) + tm_polygons("grey", alpha = 0.2)

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
  tm_shape(buff3) + tm_polygons("yellow", alpha = 0.2) +
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

#buffer mrt and fitness facilities
tm_shape(st_as_sf(island_sf)) +
  tm_borders("black") + tm_fill('white') + 
  tm_shape(mrt_station) + tm_dots("purple", size = 0.03) +
  tm_shape(union4) + tm_polygons("yellow", alpha = 0.2) +
  tm_shape(fitness_facilities_sf) + tm_dots("blue", size = 0.05)


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



############################## SUBZONE BOUNDARY ##############################

#####################
##### READ DATA #####
#####################

subzone_kml <- file.path(getwd(), 'master-plan-2019-subzone-boundary-no-sea-kml.kml')

subzone_sf <- read_sf(subzone_kml)

################################
##### ATTRIBUTES RETRIEVAL #####
################################

# Convert subzone_sf to dataframe
subzone_df <- data.frame(subzone_sf)

# Test out regex to clean up and retrieve attributes information
desc <- subzone_df$Description[1]
pattern <- "<.*?>"
plain.text <- gsub(pattern, "/", desc)
x <- strsplit(plain.text, "/+")
attributes <- lapply(x, function(z) {z[!is.na(z) & z != "" & z != " "]})

before = data.frame(attr=attributes)

# Convert first row to header and remove 1st row
new_header = before[1,]
names(before) <- new_header
before = before$Attributes[2:21]
before = data.frame(before)
colnames(before) <- new_header

# Extract attributes title
row_odd <- seq_len(nrow(before)) %% 2
attributes_title <- before[row_odd == 1, ]
attributes_title 


# Clean up all descriptions by removing HTML elements
pattern <- "<.*?>"
subzone_df$Attributes <- lapply(strsplit(gsub(pattern, "/", subzone_df$Description),"/+"),
                        function(z) {z[!is.na(z) & z != "" & z != " "]})

# Add new attributes column
subzone_df[attributes_title] <- NA 

# Split attributes to respective columns
subzone_df$SUBZONE_NO <- lapply(subzone_df$Attributes, '[[', 3)
subzone_df$SUBZONE_N <- lapply(subzone_df$Attributes, '[[', 5)
subzone_df$SUBZONE_C <- lapply(subzone_df$Attributes, '[[', 7)
subzone_df$CA_IND <- lapply(subzone_df$Attributes, '[[', 9)
subzone_df$PLN_AREA_N <- lapply(subzone_df$Attributes, '[[', 11)
subzone_df$PLN_AREA_C <- lapply(subzone_df$Attributes, '[[', 13)
subzone_df$REGION_N <- lapply(subzone_df$Attributes, '[[', 15)
subzone_df$REGION_C <- lapply(subzone_df$Attributes, '[[', 17)
subzone_df$INC_CRC <- lapply(subzone_df$Attributes, '[[', 19)
subzone_df$FMEL_UPD_D <- lapply(subzone_df$Attributes, '[[', 21)


###############################
##### MAP OUT INFORMATION #####
###############################

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
  bind_cols(bind_rows(attributes))

# Watch new data
subzone_sf_attr %>%
  glimpse()


########################### PLANNING AREA BOUNDARY ###########################

#####################
##### READ DATA #####
#####################

planbound_kml <- file.path(getwd(), 'planning-boundary-area.kml')

planbound_sf <- read_sf(planbound_kml)

################################
##### ATTRIBUTES RETRIEVAL #####
################################

# Convert planbound_sf to dataframe
planbound_df <- data.frame(planbound_sf)

# Test out regex to clean up and retrieve attributes information
pb_desc <- planbound_df$Description[1]
pattern <- "<.*?>"
pb_plain.text <- gsub(pattern, "/", pb_desc)
pb_x <- strsplit(pb_plain.text, "/+")
pb_attributes <- lapply(pb_x, function(z) {z[!is.na(z) & z != "" & z != " "]})

pb_before = data.frame(attr=pb_attributes)

# Convert first row to header and remove 1st row
pb_new_header = pb_before[1,]
names(pb_before) <- pb_new_header
pb_before = pb_before$Attributes[2:15]
pb_before = data.frame(pb_before)
colnames(pb_before) <- pb_new_header

# Extract attributes title
row_odd <- seq_len(nrow(pb_before)) %% 2
pb_attributes_title <- pb_before[row_odd == 1, ]
pb_attributes_title 


# Clean up all descriptions by removing HTML elements
pattern <- "<.*?>"
planbound_df$Attributes <- lapply(strsplit(gsub(pattern, "/", planbound_df$Description),"/+"),
                                function(z) {z[!is.na(z) & z != "" & z != " "]})

# Add new attributes column
planbound_df[pb_attributes_title] <- NA 

# Split attributes to respective columns
planbound_df$PLN_AREA_N <- lapply(planbound_df$Attributes, '[[', 3)
planbound_df$PLN_AREA_C <- lapply(planbound_df$Attributes, '[[', 5)
planbound_df$CA_IND <- lapply(planbound_df$Attributes, '[[', 7)
planbound_df$REGION_N <- lapply(planbound_df$Attributes, '[[', 9)
planbound_df$REGION_C <- lapply(planbound_df$Attributes, '[[', 11)
planbound_df$INC_CRC <- lapply(planbound_df$Attributes, '[[', 13)
planbound_df$FMEL_UPD_D <- lapply(planbound_df$Attributes, '[[', 15)

# Drop unnecessary columns
planbound_df2 <- planbound_df[ , !names(planbound_df) %in% 
                                 c("Description","geometry","Attributes")]

# Convert List to Character in order to parse to CSV file
planbound_df2$PLN_AREA_N <- vapply(planbound_df2$PLN_AREA_N, paste, collapse = ", ", character(1L))
planbound_df2$PLN_AREA_C <- vapply(planbound_df2$PLN_AREA_C, paste, collapse = ", ", character(1L))
planbound_df2$CA_IND <- vapply(planbound_df2$CA_IND, paste, collapse = ", ", character(1L))
planbound_df2$REGION_N <- vapply(planbound_df2$REGION_N, paste, collapse = ", ", character(1L))
planbound_df2$REGION_C <- vapply(planbound_df2$REGION_C, paste, collapse = ", ", character(1L))
planbound_df2$INC_CRC <- vapply(planbound_df2$INC_CRC, paste, collapse = ", ", character(1L))
planbound_df2$FMEL_UPD_D <- vapply(planbound_df2$FMEL_UPD_D, paste, collapse = ", ", character(1L))

# Write to CSV file
write.csv(planbound_df2, "Planning Boundary Attributes.csv")



###############################
##### MAP OUT INFORMATION #####
###############################

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
  bind_cols(bind_rows(attributes)) 

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



############################## FITNESS CORNERS ##############################

#####################
##### READ DATA #####
#####################
fitness_kml <- file.path(getwd(), 'nparks-play-fitness-equipment-kml.kml')

fitness_sf <- read_sf(fitness_kml)

# Watch the data
fitness_sf %>%
  glimpse()

# See Map
mapviewOptions(fgb = FALSE)
mapview(fitness_sf)

############################################
##### ATTRIBUTES RETRIEVAL/CALCULATION #####
############################################

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

fitness_longlat <- data.frame(st_coordinates(fitness_sf)) 
#%>%
#  rename("Longitude" = "X", "Latitude" = "Latitude")

colnames(fitness_longlat)

# Bind attributes to each observation as new columns
fitness_sf_attr <- 
  fitness_sf %>%
  bind_cols(bind_rows(attributes)) %>%
  bind_cols(bind_rows(fitness_longlat)) 

# Watch new data
fitness_sf_attr %>%
  glimpse()




###############################
##### MAP OUT INFORMATION #####
###############################

mapview(planbound_sf_attr, 
        zcol = "REGION_N", 
        layer.name = "Region Name") +
  mapview(planbound_sf_attr,
          zcol = "PLN_AREA_N",
          layer.name = "Planning Area Name") +
  mapview(fitness_sf_attr,
          layer.name = "Subtype") +
  mapview(subzone_sf_attr,
          zcol = "SUBZONE_N",
          layer.name = "Subzone") +
  mapview(subzone_sf_attr,
          zcol = "REGION_N",
          layer.name = "Subzone Region")


#############################################################
##### COMBINE POLYGON LINES BY SUBZONE TO PLANNING AREA #####
#############################################################

###### Combine adjacent polygon lines together #####
planbound_agg <- raster::aggregate(planbound_sf_attr, by=list(planbound_sf_attr$REGION_N), FUN=mean)
subzone_agg <- raster::aggregate(subzone_sf_attr, by=list(subzone_sf_attr$REGION_N), FUN=mean)
subzone_agg_planarea <- raster::aggregate(subzone_sf_attr, by=list(subzone_sf_attr$PLN_AREA_N), FUN=mean)


###### Polygon Area Calculations #####
sf_use_s2(FALSE)
subzone_agg$Area_km2 <- st_area(subzone_agg)/1000000
subzone_agg

subzone_sf_attr$Area_km2 <- st_area(subzone_sf)/1000000
planbound_sf_attr$Area_km2 <- st_area(planbound_sf_attr)/1000000

subzone_agg_planarea$Area_km2 <- st_area(subzone_agg_planarea)/1000000

# Test out the Area Attributes by mapping
qtm(subzone_agg, fill = "Area_km2", fill.title = "Area of Regions") +
  qtm(fitness_sf_attr) 


qtm(subzone_sf_attr, fill = "Area_km2", fill.title = "Area of Subzones") +
  qtm(fitness_sf_attr) 

############################## DENSITY (KDE) ##############################

############################################
##### ATTRIBUTES RETRIEVAL/CALCULATION #####
############################################

## Planning Area
sf_use_s2(FALSE)
out <- st_intersection(planbound_sf_attr, fitness_sf_attr)

tm_shape(planbound_sf_attr) + tm_polygons(col="pink") + tm_shape(out)+tm_dots("red") 

fitness_count <- as.data.frame(out %>% group_by(PLN_AREA_N) %>% count())[,1:2]
names(fitness_count)[names(fitness_count) == 'n'] <- "FITNESS_COUNT"

fitness_count_with_area <- merge(fitness_count, planbound_sf_attr, by="PLN_AREA_N", all=TRUE)
fitness_count_with_area$Fitness_Density <- fitness_count_with_area$FITNESS_COUNT/fitness_count_with_area$Area_km2

# fitness_count_with_area[is.na(fitness_count_with_area)] <- 0 #Fill missing values with 0

  
#########################################################################
##### INTERSECTION BETWEEN SUBZONE AND PLANNING BOUDNARY MAP LAYERS #####
#########################################################################

out2 <- st_intersection(subzone_sf_attr, fitness_sf_attr)

tm_shape(subzone_sf_attr) + tm_polygons(col="pink") + tm_shape(out2)+tm_dots("red") 

fitness_count2 <- as.data.frame(out2 %>% group_by(PLN_AREA_N) %>% count())[,1:2]
names(fitness_count2)[names(fitness_count2) == 'n'] <- "FITNESS_COUNT"

fitness_count_with_area2 <- merge(fitness_count2, subzone_sf_attr, by="PLN_AREA_N", all=TRUE)
fitness_count_with_area2$Fitness_Density <- fitness_count_with_area2$FITNESS_COUNT/fitness_count_with_area2$Area_km2

# fitness_count_with_area2[is.na(fitness_count_with_area2)] <- 0

combined <- fitness_count_with_area2 %>% 
  group_by(PLN_AREA_N) %>%
  mutate(TOTAL_AREA = sum(Area_km2)) 


keeps <- c("PLN_AREA_N", "FITNESS_COUNT", "TOTAL_AREA")
combined <- unique(combined[keeps])
combined
island <- merge(combined, planbound_sf_attr, by="PLN_AREA_N", all.x=TRUE)
island <- merge(island, subzone_agg_planarea, by.x="PLN_AREA_N", by.y="Group.1", all.x=TRUE)
island = subset(island, select = -c(geometry.x))

island$Fitness_Density <- island$FITNESS_COUNT/island$TOTAL_AREA

subzone_agg_planarea <- subzone_agg_planarea["Group.1"]

###############################
##### MAP OUT INFORMATION #####
###############################
tmap_mode("view")

tmap_options(max.categories = 55)
tmap_options(check.and.fix = TRUE)

tm_shape(subzone_agg_planarea) + 
  tm_borders('black') + 
  tm_shape(st_as_sf(fitness_count_with_area2)) +
  tm_fill("Fitness_Density", title = "Fitness Corner by Planning Area") + 
  # tm_bubbles(size="FITNESS_COUNT", col="blue") +
  tm_borders('black') + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


#tm_shape(fitness_count_with_area2) +
#tm_borders('black') + 
#  tm_fill("Group.1")


###############################
##### MAP OUT INFORMATION #####
###############################

tmap_mode("plot")

### Padding for the box surrounding the static graph [NOT IN USE]
bbox_new <- st_bbox(st_as_sf(island))
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon


## Density Plot based on Land Area 
tm_shape(st_as_sf(island)) +
  tm_fill("Fitness_Density", title = "Fitness Equipment Density") + 
  tm_bubbles(size="Fitness_Density", col="blue") +
  tm_borders('black') + 
  tm_layout(
    legend.title.size = 1,
            legend.text.size = 0.8,
            legend.outside = TRUE,
            main.title = "NParks Plays and Fitness Equipment Density by Planning Area",
            main.title.position = 0.02,
            main.title.size = 1.4) + 
  tm_compass(type="8star", show.labels = 3, size=5, text.size=0.5) +
  tm_scale_bar(position=c("right", "bottom"))


qtm(planbound_sf_attr)
choose_bw <- function(spdf) { 
  X <- coordinates(spdf) 
  sigma <- c(sd(X[,1]),sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6) 
  return(sigma/1000) 
  }


tmap_mode('view')
fitness_dens <- smooth_map(fitness_sf_attr,cover=planbound_sf_attr$geometry, bandwidth = choose_bw(fitness_sf_attr))

############################## MRT DATA FILE + SPORTS COMPLEX DATA FILE #############
mrt_kml <- file.path(getwd(), 'mrt_station_points.kml')

mrt_sf <- read_sf(mrt_kml)

mrt_sf %>%
  glimpse()

sport_complex_kml <- file.path(getwd(), 'sportsg-sport-facilities-kml.kml')

sport_complex_sf <- read_sf(sport_complex_kml)

sport_complex_sf %>%
  glimpse()

############################## SPORTS COMPLEX ############################
#Manually changing the naming location (turns out it does not show on a static map so prob nt important)
#sport_complex_sf['Name'][3,] = 'Queenstown Sports and Recreation Centre'

tm_shape(st_as_sf(island)) +
  tm_borders("grey") + tm_fill("REGION_N.x", title = "Regions") + 
  tm_shape(sport_complex_sf) + tm_polygons("black", alpha = 0.2)

############################## BUFFER ##############################

############################################
##### ATTRIBUTES RETRIEVAL/CALCULATION #####
############################################

### Convert distance in Degrees to Meters
temp2 <- st_as_sf(mrt_sf)
st_crs(temp2) <- 4326
temp2 <- st_transform(temp2, crs = 7801)
plot(st_geometry(temp2))

buff2 <- st_buffer(temp2, dist = 1000)
plot(st_geometry(buff2), add = TRUE)

buff2 <- st_transform(buff2, crs = 4326)


###############################
##### MAP OUT INFORMATION #####
###############################

### Map out the buffer for MRT & Sports Complex
tm_shape(st_as_sf(island)) + 
  tm_borders("black") + tm_fill("REGION_N.x", title = "Regions") +
  tm_shape(buff2) + tm_polygons("grey", alpha = 0.2) +
  tm_shape(mrt_sf) + tm_dots("blue", size = 0.05) +
  tm_shape(sport_complex_sf) + tm_fill("red") + tm_polygons("red", alpha = 0.2)


############################## EXTRA CODES ##############################

##### Alternate way to Retrieve  all Long and Lat Coordinates
# Convert fitness_sf to dataframe
# fitness_df <- data.frame(fitness_sf)
# 
# separated_coord <- fitness_df %>%
#   mutate(long = unlist(map(fitness_df$geometry,1)),
#          lat = unlist(map(fitness_df$geometry,2)))
# 
# separated_coord


##### Alternate way to display maps
# st_layers(planbound_kml)
# tmap_options(check.and.fix = TRUE)
# st_is_valid(planbound_sf[3])

# qtm(subzone_agg_planarea, style = "natural", projection = "+proj=eck4")
# qtm("Singapore")















