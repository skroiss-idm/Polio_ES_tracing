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

library(tidyverse)
options(tibble.width = Inf)
library(sf)
library(raster)
library(tmap)



#############################
# Load data
#############################

load('results/streams_in_karachi.RData')
# streams
# stream_coords
# dem


load('results/watersheds_in_karachi.Rdata')
# shp_basins
# basin_pop_raster_index
# streams
# stream_coords


load('admin2_karachi.RData')
# shp_khi


load('results/es_coords_khi.RData')
# es_coords_khi


pop <- raster('results/worldpop_ppp_2018_karachi.tif', values = T)



#############################
# Sum pop in each watershed
#############################

v <- getValues(pop)
pop_sum <- c()
for(i in 1:length(basin_pop_raster_index)){
  pop_sum[i] <- sum(v[basin_pop_raster_index[[i]]], na.rm = T)
}
shp_basins$pop_sum <- pop_sum
sum_basin_pop <- shp_basins %>%
  group_by(id) %>%
  summarize(pop_sum = sum(pop_sum)) %>%
  st_drop_geometry
sum_basin_pop



stream_coords <- stream_coords %>%
  left_join(sum_basin_pop)




#############################
# Plot pop in each watershed
#############################


# Streams & sample points + WATERSHEDS + ES sites
p <- tm_shape(pop) +
  tm_raster(n=20, palette = '-magma') +
  tm_shape(shp_khi) +
  tm_polygons(alpha = 0)+
  tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1) +
  tm_shape(es_coords_khi) +
  tm_dots(col = 'black', size = .5) +
  tm_text(text = 'site_name', auto.placement = T, size = .5) 
tmap_mode("view")
p







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
  tm_text(text = 'site_name', auto.placement = T, size = .5) 
p

tmap_mode("view")
p

tmap_mode("plot")
p

png('figures/map_streams_pourpts_basins_essites.png', width = 7, height = 7, units = 'in', res = 300)
p
dev.off()


















#############################
# Manipulate dem
#############################

#' Afghan Basti   67.177264, 25.037075
#' Gulshan-e-Zia  66.984982, 24.997464
#' Manghopir
#' Machar colony: 66.980453, 24.864253
#' Orangi town: 67.0023 24.9517
#' Pehlwan Basti 
#' Sohrab Goth  67.099725, 24.951216
slums <- data.frame(name = c('Afghan Basti',
                             'Gulshan-e-Zia',
                             'Machar colony', 
                             'Orangi town', 
                             'Sohrab Goth'),
                    lon = c(67.177264,
                            66.984982,
                            66.980453,
                            67.0023,
                            67.099725),
                    lat = c(25.037075,
                            24.997464,
                            24.864253,
                            24.9517,
                            24.951216))
# Convert to simple feature
proj <- st_crs(shp_khi)$proj4string
slums <- slums %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = proj)
# tm_shape(dem) +
#   tm_raster() +
png('figures/map_streams_essites_slums.png', width = 7, height = 7, units = 'in', res = 300)
tm_shape(shp_khi) +
  tm_polygons(col = '#FDC68C', alpha = .2)+
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(slums) +
  tm_dots(col = '#E55383', size = .5) +
  # tm_text(text = 'name') +
  tm_shape(es_coords_khi) +
  tm_dots(col = '#7F224E', size = .5) +
  tm_text(text = 'site_name', auto.placement = T, size = .5) 
dev.off()
  
# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue



# District outlines with streams & sample points
png('figures/map_streams_pourpts.png', width = 7, height = 7, units = 'in', res = 300)
tm_shape(shp_khi) +
  tm_polygons(col = '#FDC68C', alpha = .2, border.col = NULL) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1)
dev.off()
# # Streams & sample points - No district outline
# png('figures/map_streams_pourpts.png', width = 7, height = 7, units = 'in', res = 300)
# tm_shape(shp_khi) +
#   tm_polygons(col = '#FDC68C', alpha = .2, border.col = NULL) +
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(stream_coords) +
#   tm_dots(col = '#7F224E', size = .1)
# dev.off()
# Streams & sample points + WATERSHEDS
png('figures/map_streams_pourpts_basins.png', width = 7, height = 7, units = 'in', res = 300)
tm_shape(shp_khi) +
  tm_polygons(col = '#FDC68C', alpha = .2, border.col = NULL) +
  tm_shape(shp_basins) +
  tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1) 
dev.off()












library(nngeo)





# PREP POP RASTER
pop  # it's already loaded and clipped
tm_shape(pop) +
  tm_raster(n=20, palette = '-magma') +
  tm_shape(shp_khi) +
  tm_polygons(alpha = 0)
# PICK ONE SPOT TO PROVIDE INFECTIONS
cases <- dem
cases@data$dem <- 0
dist_from_center <- 0.01
index <- unique(c(which(abs(coordinates(dem)[,1] - 67.099725) < dist_from_center & 
                          abs(coordinates(dem)[,2] - 24.951216) < dist_from_center)))
cases@data$dem[index] <- rpois(n = length(index), lambda = 200 / length(index))
cases <- raster(cases)
tm_shape(cases) +
  tm_raster(n=5, palette = '-magma') +
  tm_shape(shp_khi) +
  tm_polygons(alpha = 0)
# SUM POP WITHIN EACH WATERSHED
# https://luisdva.github.io/rstats/GIS-with-R/
basin_pop <- shp_basins %>%
  mutate(pop_sum = raster_extract(pop, shp_basins, fun = sum, na.rm = TRUE))
basin_pop$pop_sum <- as.numeric(basin_pop$pop_sum)
save(basin_pop, file = 'results/basin_pop.RData')
# SUM CASES WITHIN EACH WATERSHED
basin_cases <- shp_basins %>%
  mutate(case_sum = raster_extract(cases, shp_basins, fun = sum, na.rm = TRUE))
# ATTACH POP AND CASE COUNTS TO POUR POINTS




# raster_extract is incredibly slow. Suggestions:
# Drop polygons & sampling points that are outside of Karachi

shp_khi


tmp <- st_intersects(shp_basins, shp_khi)
shp_basins$intersects_khi <- sapply(tmp, length) > 0


tm_shape(shp_khi) +
  tm_polygons(col = '#FDC68C', alpha = .2, border.col = NULL) +
  tm_shape(shp_basins %>% filter(intersects_khi == T)) +
  tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80')


# Try indexing which raster cells are within each basin

