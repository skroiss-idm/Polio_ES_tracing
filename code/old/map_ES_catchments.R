# Purpose: 
# Inputs:  
# Output:  
# Author:  Steve Kroiss 
# Date:    November 2019


# Clear objects from R workspace
rm(list=ls())  



#############################
# Load libraries
#############################

library(sf)
library(tidyverse)
options(tibble.width = Inf)



#############################
# Load data
#############################

# Need to go into Google Earth & save each layer as a KML
file_dl <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/ES catchment maps from Novel-T/Karachi Drainage Lines (Est. pop.).kml"
shp_dl <- st_read(file_dl)
plot(shp_dl)

file_dp <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/ES catchment maps from Novel-T/Karachi Drainage Points (Est. pop.).kml"
shp_dp <- st_read(file_dp)
plot(shp_dp)

file_dist <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/Pakistan/pakistan_polis_admin2_20190710.shp"
shp_dist <- st_read(file_dist)
# # Shapefiles
# filename_shapes = "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/WHO_POLIO_GLOBAL_GEODATABASE_20190710.gdb"
# source("C:/Users/skroiss/Dropbox (IDM)/SteveK/Code/Polio/Polio_cessation/code/polis_shapes_query.R")
# shapes_district = polis_shapefile_extract(filename_shapes, 2, criteria_field = "ADM0_NAME", criteria = c("PAKISTAN"), current_only = T)
# sf_districts = st_as_sf(shapes_district)
# st_write(sf_districts, "pakistan_polis_admin2_20190710.shp")
# # shapes_province = polis_shapefile_extract(filename_shapes, 1, criteria_field = "ADM0_NAME", criteria = c("PAKISTAN","AFGHANISTAN"), current_only = T)
# # shapes_country = polis_shapefile_extract(filename_shapes, 0, criteria_field = "ADM0_NAME", criteria = c("PAKISTAN","AFGHANISTAN"), current_only = T)
# # shapes_disputed = polis_disputed_extract(filename_shapes, current_only = T)
# # Convert to sf object
# # Export
# # st_write(sf_districts, "pakistan_polis_admin2_20190710.shp")


shp_gadap <- shp_dist %>% 
  filter(ADM2_NA == 'KHIGADAP')
shp_gadap



#############################
# Plot
#############################

ggplot() +
  geom_sf(data = shp_gadap, color = 'purple', fill = '#7F224E') + 
  geom_sf(data = shp_dl, color = '#FC9D62') +
  geom_sf(data = shp_dp, color = '#E55383') +
  theme_bw()
ggsave('figures/gadap_es_catchment_novelt.png',
       height = 11, width = 7)
# FDC68C # light orange
# E55383 # pink  
# FC9D62 # orange
# 7F224E # purple






