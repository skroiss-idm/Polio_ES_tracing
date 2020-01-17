# Purpose: Delineate watersheds based on DEM (digital elevation map)
# Inputs:  DEM, streams, pour points
# Output:  Watershed shapefile
# Author:  Steve Kroiss 
# Date:    December 2019


# Clear objects from R workspace
rm(list=ls())  



#############################
# Load libraries
#############################

library(sf)
library(tidyverse)
options(tibble.width = Inf)
library(janitor)
library(lubridate)
library(raster)
library(elevatr)
library(openSTARS)
# account for error with loading GRASS GIS results: https://gis.stackexchange.com/questions/341451/rgrass7-no-stars-import-yet
library(sp)  
use_sp()
library(rgdal)
library(tmap)



#############################
# Load data
#############################

# Load data
load('results/streams_in_karachi.RData')
# streams
# stream_coords
# dem
plot(dem)

# Load pop data
pop <- raster('results/worldpop_ppp_2018_karachi.tif')

load('data/admin2_karachi.RData')
# shp_khi

load('results/es_coords_novelt_khi.RData')
# es_coords_novelt_polis



#############################
# Snap ES points to nearest stream point
#############################

library(rgeos)
set1sp <- SpatialPoints(es_coords_novelt_polis)
set2sp <- SpatialPoints(set2)
set1$nearest_in_set2 <- apply(gDistance(set1sp, set2sp, byid=TRUE), 1, which.min)

head(set1)


gDistance(es_coords_novelt_polis, es_coords_novelt_polis)



tmp <- st_as_sf(streams)
bob <- st_nearest_points(es_coords_novelt_polis, tmp, pairwise = F)
bob

points <- st_as_sf(streams) %>%
  st_cast('POINT')
nearest <- points[which.min(st_distance(es_coords_novelt_polis, points)),]

st_snap(es_coords_novelt_polis, tmp)





reach <- st_as_sf(streams)
points <- reach %>% 
  st_sf() %>%
  st_cast('POINT')
nearest <- points[which.min(st_distance(site, points)),]




points
reach_index <- st_nearest_feature(es_coords_novelt_polis, points)
reach_index
i = 2
nearest = st_nearest_points(es_coords_novelt_polis[i,], points[reach_index[i],])



library(mapview)
mapview(nearest) + es_coords_novelt_polis + reach













r = sqrt(2)/10
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))
pt3 = st_point(c(.9,.1))
b1 = st_buffer(pt1, r)
b2 = st_buffer(pt2, r)
b3 = st_buffer(pt3, r)
(ls0 = st_nearest_points(b1, b2)) # sfg
(ls = st_nearest_points(st_sfc(b1), st_sfc(b2, b3))) # sfc
plot(b1, xlim = c(-.2,1.2), ylim = c(-.2,1.2), col = NA, border = 'green')
plot(st_sfc(b2, b3), add = TRUE, col = NA, border = 'blue')
plot(ls, add = TRUE, col = 'red')

nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
plot(st_geometry(nc))
ls = st_nearest_points(nc[1,], nc)
plot(ls, col = 'red', add = TRUE)
pts = st_cast(ls, "POINT") # gives all start & end points
# starting, "from" points, corresponding to x:
plot(pts[seq(1, 200, 2)], add = TRUE, col = 'blue')
# ending, "to" points, corresponding to y:
plot(pts[seq(2, 200, 2)], add = TRUE, col = 'green')




# Streams & sample points + WATERSHEDS + ES sites
p <- tm_shape(shp_khi) +
  tm_polygons(col = '#FDC68C', alpha = .2, border.col = NULL) +
  # tm_shape(shp_basins) +
  # tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .5) +
  tm_text(text = 'site_name', auto.placement = T, size = .5) 
p

tmap_mode("view")
p












#############################
# Delineate watersheds
#############################

# https://grasswiki.osgeo.org/wiki/Creating_watersheds
# https://irapoenya.wordpress.com/2019/07/28/rstudio-tutorial-watershed-delineation-part-1/


#You need to do this and manually set up the mapset projection by this command, and then create a new location with the name of newLocation, use the reference from our dem data
# Point interface to: C:/Users/skroiss/Dropbox (IDM)/SteveK/Code/Polio/Polio_stool_survey/results/elevation_raster_karachi.tif
system('"C:/Program Files/GRASS GIS 7.6/grass76.bat"')

# Step 1 - Initiate the GRASS GIS within R studio
initGRASS(gisBase = 'C:/Program Files/GRASS GIS 7.6',
          gisDbase = 'C:/Users/skroiss/Documents/grassdata', 
          location='newLocation', 
          mapset='PERMANENT', 
          override = TRUE)


# Step 2 - Test that GRASS can read it
execGRASS("r.info", map = "elevation_raster_karachi")


# Step 3 - Set the region based on the mapset
execGRASS("g.region", raster = "elevation_raster_karachi")
# out_raster <- readRAST("elevation_raster_karachi")
# str(out_raster)
# plot(out_raster)


# Step 4 - Delineate watersheds
execGRASS("r.watershed", 
          flags="overwrite", 
          parameters=list(elevation="elevation_raster_karachi", threshold=5000,
                          drainage= "fdir",stream="upstream", basin="rbasin"))
fdir <- readRAST("fdir")
plot(fdir)


# Step 5 - Extract watershed basins from a drainage direction map
# for each sampling point...
for (i in seq_len(nrow(stream_coords))) {
  # extract respective point
  tmp_coords <- stream_coords[i,] 
  message(paste0(i, ' out of ', nrow(stream_coords)))
  # calculate drainage area
  execGRASS("r.water.outlet", 
            flags="overwrite", 
            parameters=list(input = "fdir",
                            output = paste0("watershed_", i),
                            coordinates = st_coordinates(stream_coords)[i,]))
  # make raster to vector
  execGRASS('r.to.vect', flags='overwrite',
            parameters =  list(input=paste0("watershed_", i), 
                               output=paste0("basin_", i), type="area"))
  # Export shapefile
  execGRASS('v.out.ogr', flags=c('overwrite'),
            parameters=list(input=paste0("basin_", i),
                            output=paste0("results/watershed_shapefiles/watershed_",
                                          i, 
                                          ".shp"),
                            type="area", format="ESRI_Shapefile"))
}



# Load all those shapefiles and combine
files <- list.files("results/watershed_shapefiles", pattern = "watershed_[0-9]{,3}.shp")
shp_basins <- st_read(file.path("results/watershed_shapefiles", files[1])) %>%
  mutate(file = files[1])
for(i in 2:length(files)){
  shp_tmp <- st_read(file.path("results/watershed_shapefiles", files[i])) %>%
    mutate(file = files[i])
  shp_basins <- rbind(shp_basins, shp_tmp)
}
shp_basins <- shp_basins %>%
  mutate(id = file,
         id = as.numeric(gsub('watershed_|\\.shp', '', id)))
table(table(shp_basins$id))  # Some basins have 'danglers' that get counted as another feature
plot(shp_basins %>% filter(id ==5))


# Plot
tm_shape(shp_basins) +
  tm_polygons(col = '#FDC68C', alpha = .05) +
  # tm_polygons(col = '#FDC68C', alpha = .05, border.col = NULL) +
  tm_shape(streams) +
  tm_lines(col = '#E55383', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1)
# FDC68C # light orange
# E55383 # pink  
# FC9D62 # orange
# 7F224E # purple  






#############################
# Clip shapefiles to those with basins inside Karachi
#############################

# Drop polygons & sampling points that are outside of Karachi
tmp <- st_intersects(shp_basins, shp_khi)
shp_basins$intersects_khi <- sapply(tmp, length) > 0
basin_in_khi <- shp_basins %>% 
  group_by(id) %>%
  summarize(in_khi = sum(intersects_khi) > 0) %>%
  filter(in_khi == T) %>%
  pull(id)
streams <- st_as_sf(streams) %>% filter(stream %in% basin_in_khi)
stream_coords <- stream_coords %>% filter(id %in% basin_in_khi)
shp_basins <- shp_basins %>% filter(id %in% basin_in_khi)



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
# Identify which cells in raster lie within each watershed
#############################




# The process of identifying which raster cells lie within each polygon is very slow.
# so let's just do that once & save the cell indeces. 
# Then we can use the indeces to call the values in the rasters
tmp <- as(shp_basins, "Spatial")
index <- extract(pop, tmp, cellnumbers = T)
basin_pop_raster_index <- lapply(index, function(x) x[,'cell'])






# t1 <- system.time(sum(dem@data@values[index[[1]][,'cell']], na.rm = T))
# library(nngeo)
# t2 <- system.time(raster_extract(dem, shp_basins[1,], fun = sum, na.rm = TRUE))

test <- pop
test@data@values
test@data@values[basin_pop_raster_index[[20]]] <- 5000
tm_shape(pop) +
  tm_raster() +
  tm_shape(shp_basins[20,]) +
  tm_polygons(col = '#FDC68C', alpha = .05) 
tm_shape(test) +
  tm_raster() +
  tm_shape(shp_basins[20,]) +
  tm_polygons(col = '#FDC68C', alpha = .05) 






#############################
# Export
#############################

save(shp_basins,
     basin_pop_raster_index,
     streams,
     stream_coords,
     file = 'results/watersheds_in_karachi.Rdata')

load('results/watersheds_in_karachi.Rdata')


