# Purpose: Download digital elevation map for Karachi
# Inputs:  
# Output:  
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

# Also consider the SSN package - spatial modeling on stream networks
# https://cran.r-project.org/web/packages/SSN/SSN.pdf
# https://www.fs.fed.us/rm/boise/AWAE/projects/SSN_STARS/downloads/SSN/SSNvignette2014.pdf



# #############################
# # Demo
# #############################
# 
# # https://cran.r-project.org/web/packages/openSTARS/openSTARS.pdf
# # Needed to install the stand-alone stable old version (7.6)
# # Then install the following addons in the GUI as an ADMIN using the Settings > Manage addons extensions
# # r.stream.basins, r.stream.distance, r.stream.order, and r.hydrodem is needed
# # Initiate GRASS session
# if(.Platform$OS.type == "windows"){
#   gisbase = "C:/Program Files/GRASS GIS 7.6"
# } else {
#   gisbase = "/usr/lib/grass76/"
# }
# initGRASS(gisBase = gisbase,
#           home = tempdir(),
#           override = TRUE)
# # Load files into GRASS
# dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
# sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
# setup_grass_environment(dem = dem_path)
# import_data(dem = dem_path, sites = sites_path)
# gmeta()
# # Derive streams from DEM
# derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
# dem <- readRAST('dem', ignore.stderr = TRUE)
# sites <- readVECT('sites_o', ignore.stderr = TRUE)
# streams <- readVECT('streams_v', ignore.stderr = TRUE)
# plot(dem, col = terrain.colors(20))
# lines(streams, col = 'blue', lwd = 2)
# points(sites, pch = 4)



#############################
# Gadap
#############################

# Load shapes for Karachi
file_dist <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/Pakistan/pakistan_polis_admin2_20190710.shp"
shp_dist <- st_read(file_dist)
shp_khi <- shp_dist %>%
  filter(grepl('KHI', ADM2_NA))
  # filter(ADM2_NA == 'KHIGADAP')
shp_khi
save(shp_khi, file = 'admin2_karachi.RData')

# Download elevation data
# Sauce: https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
elevation_khi <- get_elev_raster(shp_khi, z = 9, clip = 'bbox')
plot(elevation_khi)
plot(shp_khi, add = TRUE)
# Export elevation data as tif
writeRaster(elevation_khi,
            "results/elevation_raster_karachi.tif",
            format="GTiff", overwrite=TRUE)

# Need to go into Google Earth & save each layer as a KML
file_dl <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/ES catchment maps from Novel-T/Karachi Drainage Lines (Est. pop.).kml"
shp_dl <- st_read(file_dl)
plot(shp_dl)
#
file_dp <- "C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/Shapefiles/ES catchment maps from Novel-T/Karachi Drainage Points (Est. pop.).kml"
shp_dp <- st_read(file_dp)
st_write(shp_dp, "results/novelt_sites_khi.shp")
plot(shp_dp)
library(rgdal)
sp_pts <- as(shp_dp, "Spatial")
sp_pts
class(sp_pts)
writeOGR(obj=sp_pts, dsn="results/novelt_sites_khi", layer="novelt_sites_khi", driver="ESRI Shapefile") # this is in geographical projection


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







plot(dirs)
lines(streams, col = 'blue', lwd = 2)


plot(dem, col = terrain.colors(20))
plot(shp_dl, add = T,  col = 'blue', lwd = 2)







png(file = "figures/bluelines_novelt.png", width = 1000, height = 1000)
plot(dem, col = terrain.colors(20))
plot(shp_khi, col = NA, add = TRUE, lwd = 2)
plot(shp_dl, add = T,  col = 'blue', lwd = 2)
dev.off()



png(file = "figures/bluelines_derive_streams.png", width = 1000, height = 1000)
plot(dem, col = terrain.colors(20))
plot(shp_khi, col = NA, add = TRUE, lwd = 2)
lines(streams, col = 'blue', lwd = 2)
dev.off()




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
save(es_coords_khi, file = 'results/es_coords_khi.RData')


library(tmap)
png(file = "figures/bluelines_both_approaches.png", width = 1000, height = 1000)
tm_shape(shp_khi) +
  tm_polygons(border.col = 'white', lwd = 2) +
  tm_shape(es_coords_khi) +
  tm_dots(col = '#FC9D62', size = 1.5) +
  tm_text(text = 'site_name') +
  tm_shape(streams) +
  tm_lines(col = '#7F224E', lwd = 2) +
  tm_shape(shp_dl) +
  tm_lines(col = '#E55383', lwd = 1.5) +
  tm_add_legend(type = 'line',
                labels = c('New approach',
                           'Novel-T'),
                col = c('#7F224E',
                        '#E55383'),
                lwd = c(2, 1.5)) +
  tm_layout(legend.bg.color = "white",
            legend.text.size = 2)
dev.off()

# FDC68C # light orange
# E55383 # pink  
# FC9D62 # orange
# 7F224E # purple  







# NEED TO PICK SAMPLING POINTS JUST ABOVE CONFLUENCES
# To find sample points above confluences...
# find intersections
# Trace up from there by some linear distance
# Label streams
tm_shape(streams) +
  tm_lines(col = '#FC9D62', lwd = 2) +
  tm_text(text = 'stream')

# MODEL SUB-CATCHMENTS/SUB-WATERSHEDS/SUBBASINS
# https://rdrr.io/github/tpilz/lumpR/man/calc_subbas.html
# https://www.geosci-model-dev.net/10/3001/2017/gmd-10-3001-2017.pdf

# MEASURE POPULATION FLOWING INTO EACH SUBBASIN




# Plot points just above stream confluences
stream_coords <- matrix(NA, nrow = length(streams), ncol = 2)
for(i in 1:length(streams)){
  tmp_coords <- streams@lines[[i]]@Lines[[1]]@coords
  row_index <- ifelse(nrow(tmp_coords) > 6,
         nrow(tmp_coords) - 5,
         nrow(tmp_coords))
  stream_coords[i,] <- tmp_coords[row_index,]  # nrow(tmp_coords)
}
proj4 <- st_crs(streams)$proj4string
stream_coords <- data.frame(stream_coords) %>%
  mutate(id = row_number()) %>%
  rename(lon = X1, lat = X2) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = proj4)
# Plot
tm_shape(streams) +
  tm_lines(col = '#FC9D62', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1)
save(streams,
     stream_coords,
     dem,
     file = 'results/streams_in_karachi.RData')
