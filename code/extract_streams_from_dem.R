# Purpose: Extract streams/drainage lines from DEM (digital elevation map)
# Inputs:  Pak shapefiles, DEM, Novel-T pour points for calibrating
# Output:  Stream shapefile
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



#############################
# Load data
#############################

# Load shapes for Pakistan
file_dist <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/Pakistan/pakistan_polis_admin2_20190710.shp"
shp_dist <- st_read(file_dist)
# Filter to just Karchi
shp_khi <- shp_dist %>%
  filter(grepl('KHI', ADM2_NA))
# filter(ADM2_NA == 'KHIGADAP')
shp_khi
save(shp_khi, file = 'admin2_karachi.RData')


# Download elevation data
# Sauce: https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
dem_original <- get_elev_raster(shp_khi, z = 9, clip = 'bbox')
# Clip dem along coastline
elevation_khi <- mask(dem_original, shp_dist)
plot(elevation_khi)
plot(shp_khi, add = TRUE)
# Export elevation data as tif
writeRaster(elevation_khi,
            "results/elevation_raster_karachi.tif",
            format="GTiff", overwrite=TRUE)


# Load Novel-T Drainage points to calibrate model
# Need to go into Google Earth & save each layer as a KML
file_dl <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/ES catchment maps from Novel-T/Karachi Drainage Lines (Est. pop.).kml"
shp_dl <- st_read(file_dl)
plot(shp_dl)
file_dp <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/ES catchment maps from Novel-T/Karachi Drainage Points (Est. pop.).kml"
shp_dp <- st_read(file_dp)
st_write(shp_dp, "results/novelt_sites_khi.shp")
plot(shp_dp)
sp_pts <- as(shp_dp, "Spatial")
writeOGR(obj=sp_pts, dsn="results/novelt_sites_khi", layer="novelt_sites_khi", driver="ESRI Shapefile") # this is in geographical projection



#############################
# Extract streams
#############################

# https://cran.r-project.org/web/packages/openSTARS/openSTARS.pdf
# Needed to install the stand-alone stable old version (7.6)
# Then install the following addons in the GUI as an ADMIN using the Settings > Manage addons extensions
# r.stream.basins, r.stream.distance, r.stream.order, and r.hydrodem is needed
# Initiate GRASS session
if(.Platform$OS.type == "windows"){
  gisbase = "C:/Program Files/GRASS GIS 7.6"
} else {
  gisbase = "/usr/lib/grass76/"
}
initGRASS(gisBase = gisbase,
          home = tempdir(),
          override = TRUE)
# Load files into GRASS
dem_path <- file.path(getwd(), "results/elevation_raster_karachi.tif")
sites_path <- file.path(getwd(), "results/novelt_sites_khi/novelt_sites_khi.shp")
setup_grass_environment(dem = dem_path)
import_data(dem = dem_path, sites = sites_path)
gmeta()
# Derive streams from DEM
derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
dem <- readRAST('dem', ignore.stderr = TRUE)
sites <- readVECT('sites_o', ignore.stderr = TRUE)
streams <- readVECT('streams_v', ignore.stderr = TRUE)
dirs <- readRAST('dirs', ignore.stderr = TRUE)
# Plot
plot(dem, col = terrain.colors(20))
lines(streams, col = 'blue', lwd = 2)
points(sites, pch = 4)
plot(shp_khi, col = NA, add = TRUE)



#############################
# Extract pour points or drainage points for each stream just above confluence
#############################

# Extract points just above stream confluences
stream_coords <- matrix(NA, nrow = length(streams), ncol = 2)
for(i in 1:length(streams)){
  tmp_coords <- streams@lines[[i]]@Lines[[1]]@coords
  # row_index <- ifelse(nrow(tmp_coords) > 6,
  #                     nrow(tmp_coords) - 5,
  #                     nrow(tmp_coords))
  stream_coords[i,] <- tmp_coords[nrow(tmp_coords) - 1,]  # nrow(tmp_coords)
}
proj4 <- st_crs(streams)$proj4string
stream_coords <- data.frame(stream_coords) %>%
  mutate(id = row_number()) %>%
  rename(lon = X1, lat = X2) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = proj4)


# Plot
png('figures/map_dem_streams_pourpts.png', width = 7, height = 7, units = 'in', res = 300)
tm_shape(dem) +
  tm_raster(palette = terrain.colors(50)[5:50], alpha = .8, n = 45, legend.show = F) +
  tm_shape(shp_khi) +
  tm_polygons(alpha = 0, lwd = 2)+
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#E55383', size = .1)
dev.off()
# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue



#############################
# Export
#############################

save(streams,
     stream_coords,
     dem,
     file = 'results/streams_in_karachi.RData')
