
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

path = "data/"
plot_path = "plots/"

# Read data
# fitness <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
#                    layer = "fitness_facilities")

fitness_summary <- read_sf(dsn = paste(path, "fitness_facilities/", sep = ""), 
                           layer = "fitness_summarised_points")
sportsfac <- read_sf(dsn = paste(path, "sports_facilities/", sep = ""), 
                     layer = "sports_facilities")
gym <- read_sf(dsn = paste(path, "gym_facilities/", sep = ""), 
               layer = "gym_facilities")



tm_shape(fitness_summary) + tm_dots(col="black") +
  tm_shape(fitness_sf) + tm_dots(col="red") + 
  tm_add_legend("fill",
                labels = c("Before", "After"),
                col = c("Red", "Black"),
                title = "Fitness Points") +# Compare full and new fitness layer

###################################
##### FREQUENCY OF FACILITIES #####
###################################

freq_table <- data.frame(facility_type = c("fitness_facilities", 
                                           "sports_facilities", 
                                           "gym_facilities"),
                         frequency = c(nrow(fitness_summary %>% as.data.frame()), 
                                       nrow(sportsfac %>% as.data.frame()), 
                                       nrow(gym %>% as.data.frame())))

freq_table


####################################
##### DISPERSION OF FACILITIES #####
####################################

# Fitness Corners
fitness_coord <- fitness_summary %>% 
  as.data.frame() %>%
  dplyr::select(geometry)

fitness_points = st_coordinates(st_as_sf(fitness_coord)) %>% as.data.frame()
fitness_points$facility_type = "fitness_facilities"
fitness_scatter = ggplot(fitness_points, aes(x=X, y=Y)) + 
  coord_map() + 
  geom_point(color="#74c476") + 
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
fitness_scatter

# Sports Facility
sportsfac_coord <- sportsfac %>% 
  as.data.frame() %>%
  dplyr::select(geometry)

sportsfac = st_coordinates(st_as_sf(sportsfac_coord)) %>% as.data.frame()
sportsfac$facility_type = "sports_facilities"
sportsfac_scatter = ggplot(sportsfac, aes(x=X, y=Y)) + 
  geom_point(color="#9e9ac8") +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
sportsfac_scatter

# Gyms
gym_coord <- gym %>% 
  as.data.frame() %>%
  dplyr::select(geometry)

gym_points = st_coordinates(st_as_sf(gym_coord)) %>% as.data.frame()
gym_points$facility_type = "gym_facilities"
gym_scatter = ggplot(gym_points, aes(x=X, y=Y)) + 
  geom_point(color="#6baed6") +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
gym_scatter

# All Facilities
all_facility <- rbind(fitness_points, sportsfac, gym_points)
all_facility_scatter = ggplot(all_facility, aes(x=X, y=Y, color=facility_type)) + 
  geom_point() + scale_colour_manual(values = c("#74c476", "#9e9ac8", "#6baed6")) +
  geom_smooth(method='lm') + theme(legend.position="bottom") +
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
all_facility_scatter

# Combined Plots
combined_facility_scatter <- ggarrange(fitness_scatter, sportsfac_scatter, gym_scatter, all_facility_scatter,
                    labels = c("fitness", "sports", "gym", "all"),
                    ncol = 2, nrow = 2,
                    common.legend=TRUE,
                    legend="bottom",
                    font.label=list(size = 10, face = "bold", color ="black"))
combined_facility_scatter

# Save plots
ggsave(path = plot_path, filename = "descriptive_analysis/fitness_facilities_dispersion.png", plot = fitness_scatter)
ggsave(path = plot_path, filename = "descriptive_analysis/sports_facilities_dispersion.png", plot = sportsfac_scatter)
ggsave(path = plot_path, filename = "descriptive_analysis/gym_facilities_dispersion.png", plot = gym_scatter)
ggsave(path = plot_path, filename = "descriptive_analysis/all_facilities_dispersion.png", plot = all_facility_scatter)
ggsave(path = plot_path, filename = "descriptive_analysis/combined_facilities_dispersion.png", plot = combined_facility_scatter)

