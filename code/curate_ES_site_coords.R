# Purpose: Use ES site coordinates from Novel-T instead of POLIS (when available)
# Inputs:  POLIS & Novel-T ES coordinates, karachi shapefiles
# Output:  Curated ES coordinates
# Author:  Steve Kroiss 
# Date:    December 2019


# Clear objects from R workspace
rm(list=ls())  



#############################
# Load libraries
#############################

library(tidyverse)
options(tibble.width = Inf)
library(janitor)
library(sf)
library(lubridate)
library(tmap)



#############################
# Load data
#############################

# Previously curated ES sites
load('results/es_coords_khi.RData')
# es_coords_khi

# Watersheds
load('results/watersheds_in_karachi.Rdata')

# Admin boundaries
load('admin2_karachi.RData')
# shp_khi

pop <- raster('results/worldpop_ppp_2018_karachi.tif', values = T)


# Load ES data to plot ES points on map
es <- read_csv('Q:/Data/Polis/Polis_Environmental-201911110000.csv')
es <- es %>%
  clean_names() %>%
  rename(polio_virus_types = virus_type_s) %>%
  mutate(dot_name = tolower(paste(place_admin_0, place_admin_1, place_admin_2, sep=':')),
         sample_date = as.Date(collection_date, format = '%d-%m-%Y'),
         sample_mo = floor_date(sample_date, unit = 'month'),
         year = year(sample_date),
         wpv1 = grepl('WILD 1', polio_virus_types)) %>%
  filter(place_admin_0 %in% c('AFGHANISTAN', 'PAKISTAN'))
head(es)
# Extract coordinates for Karachi ES sites
es_coords_khi <- es %>% 
  filter(grepl('KHI', place_admin_2)) %>% 
  filter(reporting_year >= 2019) %>%
  arrange(place_admin_2) %>%
  mutate(environmental_site = toupper(environmental_site)) %>%
  distinct(environmental_site, .keep_all = T) %>%
  mutate(environmental_site2 = environmental_site) %>%
  separate(environmental_site2, into = c('site_id', 'site_name'), sep = ' - ') %>%
  rename(lon = x, lat = y)
# Convert to simple feature
proj <- st_crs(shp_khi)$proj4string
es_coords_khi <- es_coords_khi %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = proj)
# save(es_coords_khi, file = 'results/es_coords_khi.RData')

uc <- st_read('C:/Users/skroiss/Dropbox (IDM)/AMUG/Data/Pakistan/GIS and GPS/PAK_adm_EOC_20160228/PAK_adm4_EOC_20160228.shp')
uc_khi <- uc %>% 
  filter(grepl('SINDH', WHO_PROV_N),
         grepl('KHI', WHO_DIST_N))



#############################
# Curate sites using Novel-T data
#############################

novelt <- read_csv('data/es_coords_novelt_karachi.csv') %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = proj)  %>%
  mutate(environmental_site2 = environmental_site) %>%
  separate(environmental_site2, into = c('site_id', 'site_name'), sep = ' - ')
novelt


#############################
# Plot
#############################

# Streams & sample points + WATERSHEDS + ES sites
p <- tm_shape(shp_khi) +
  tm_polygons(col = '#FDC68C', alpha = .2, border.col = NULL) +
  tm_shape(shp_basins) +
  tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1) +
  tm_shape(es_coords_khi) +
  tm_dots(col = '#E55383', size = .5) +
  tm_text(text = 'site_name', auto.placement = T, size = .5) +
  tm_shape(novelt) +
  tm_dots(col = '#FC9D62', size = .5) 
  tm_text(text = 'site_name', auto.placement = T, size = .5)
p
# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue

tmap_mode("view")
p

tmap_mode("plot")
p

png('figures/map_streams_pourpts_basins_essites.png', width = 7, height = 7, units = 'in', res = 300)
p
dev.off()


#############################
# Combine site coords
#############################

novelt
es_coords_khi

# Filter to NovelT coords that have sites in POLIS
nt <- novelt %>%
  filter(site_name %in% es_coords_khi$site_name)%>%
  dplyr::select(environmental_site, site_name, geometry)
# Drop POLIS sites that have duplicates in NovelT
pf <- es_coords_khi %>%
  filter( !(site_name %in% novelt$site_name)) %>%
  dplyr::select(environmental_site, site_name, geometry)
es_coords_novelt_polis <- rbind(nt, pf)
# Export
save(es_coords_novelt_polis, file = 'results/es_coords_novelt_khi.RData')



# Streams & sample points + WATERSHEDS + ES sites
tm_shape(pop) +
  tm_raster(n=20, palette = '-magma') +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0) +
  # tm_shape(shp_khi) +
  # tm_polygons(alpha = 0) +
  tm_shape(shp_basins) +
  tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1) +
  # tm_shape(es_coords_khi) +
  # tm_dots(col = '#E55383', size = .5) +
  # tm_text(text = 'site_name', auto.placement = T, size = .5) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#FC9D62', size = .5) +
  tm_text(text = 'site_name', auto.placement = T, size = .5)
p
# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue

tmap_mode("view")
p
