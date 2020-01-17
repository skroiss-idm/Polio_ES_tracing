# Purpose: Experiment with watershed delineation
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
library(rgrass7)
library(raster)
library(tmap)



#############################
# Load libraries
#############################

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
# dem_path <- file.path(getwd(), "results/elevation_raster_karachi.tif")
# sites_path <- file.path(getwd(), "results/novelt_sites_khi/novelt_sites_khi.shp")
# setup_grass_environment(dem = dem_path)
# import_data(dem = dem_path, sites = sites_path)
# gmeta()
# # Derive streams from DEM
# derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
# dem <- readRAST('dem', ignore.stderr = TRUE)






# Should be able to input flow raster to measure accumulated upland flow
# If flow is omitted, a value of one (1) is assumed.
# Need to get flow accumulation for each stream cell based on population & infecteds
# OR do it in R 
# This may be more flexible 
# Extract polygons & find drain points
# Sum pop raster cells within polygons
# Sum infections within polygons

# https://grasswiki.osgeo.org/wiki/Creating_watersheds
# https://irapoenya.wordpress.com/2019/07/28/rstudio-tutorial-watershed-delineation-part-1/









# Step 1 - Initiate the GRASS GIS within R studio
initGRASS(gisBase = 'C:/Program Files/GRASS GIS 7.6',
          gisDbase = 'C:/Users/skroiss/Documents/grassdata', 
          location='newLocation', 
          mapset='PERMANENT', 
          override = TRUE)

# Step 2 - MANUALLY DROP TIF INTO FOLDER
# LITERALLY. JUST DO IT.

# Step 3 - Test that GRASS can read it
execGRASS("r.info", map = "elevation_raster_karachi")

# Step 4 - Set the region based on the mapset
execGRASS("g.region", raster = "elevation_raster_karachi")
# out_raster <- readRAST("elevation_raster_karachi")
# str(out_raster)
# plot(out_raster)

# Step 5 - Delineate watersheds
execGRASS("r.watershed", 
          flags="overwrite", 
          parameters=list(elevation="elevation_raster_karachi", threshold=5000,
                          drainage= "fdir",stream="upstream", basin="rbasin"))
flowdir <- readRAST("fdir")
# save(draindir, file = 'results/flowdir.')
execGRASS('r.thin', flags='overwrite',
          parameters =  list(input='upstream', 
                             output='riv_thin'))
execGRASS("r.to.vect", flags='overwrite',
          parameters = list(input="riv_thin", 
                            output="streams", type="line"))
execGRASS('r.to.vect', flags='overwrite',
          parameters =  list(input='rbasin', 
                             output='catchments', type="area"))
execGRASS('v.out.ogr', flags=c('overwrite'),
          parameters=list(input='catchments',
                          output="area.shp",type="area",
                          format="ESRI_Shapefile"))
execGRASS('v.out.ogr',flags=c('overwrite'),
          parameters=list(input='streams',
                          output="streams.shp",type="line",format="ESRI_Shapefile"))
shapefile("streams.shp") -> s
shapefile("area.shp") -> a
plot(r)
plot(a, add=T)
plot(s, add=T, col="blue")


asf <- st_as_sf(a)
plot(r)
plot(asf %>% filter(cat == 15), add=T)




tm_shape(r) +
  tm_raster() +
  tm_shape(s) +
  tm_lines(col = 'blue') +
  tm_shape(a) +
  tm_polygons(alpha = 0) +
  tm_text(text = 'cat') 
  
  
tm_polygons(border.col = 'white', lwd = 2)














#You need to do this and manually set up the mapset projection by this command, and then create a new location with the name of newLocation, use the reference from our dem data
# system('"C:/Program Files/GRASS GIS 7.6/grass76.bat"')
#Initiate the GRASS GIS within R studio
initGRASS(gisBase = 'C:/Program Files/GRASS GIS 7.6',
          gisDbase = 'C:/Users/skroiss/Documents/grassdata', 
          location='newLocation', 
          mapset='PERMANENT', 
          override = TRUE)
#read raster image
dem_path <- file.path(getwd(), "results/elevation_raster_karachi.tif")
r <- raster(dem_path)
plot(r)
# r
rast <- as(r, "SpatialGridDataFrame")
writeRAST(rast, "rast_img", flags = c("overwrite"))



library(sp)
write.asciigrid(rast, "rast_img")

SpatialGridDataFrame(rast, data, proj4string = CRS(as.character(NA)))



writeRAST(, "rast_img")


### MANUALLY DROP TIF INTO FOLDER
execGRASS("r.info", map = "elevation_raster_karachi")

#set the region based on the mapset
execGRASS("g.region", raster = "elevation_raster_karachi")
out_raster <- readRAST("elevation_raster_karachi")
str(out_raster)
plot(out_raster)




# #You need to do this and manually set up the mapset projection by this command, and then create a new location with the name of newLocation, use the reference from our dem data
# system('"C:/Program Files/GRASS GIS 7.4.4/grass74.bat"')
# 
# #Initiate the GRASS GIS within R studio
# initGRASS(gisBase='C:/Program Files/GRASS GIS 7.4.4', 
#           gisDbase='C:/Users/ira syarif/Documents/grassdata', 
#           location='newLocation', 
#           mapset='PERMANENT', 
#           override = TRUE)
# 
# setwd("C:/Users/ira syarif/Documents/grassdata/raster")
# 
# #read raster image
# raster("S07452E110232_S07804E110567_UM_DSM.tif") -> r
# #plot(r)
# r
# 
# #create spatial polygons to crop the raster
# extent(430000,448000,9145000,9170000) -> e
# p <- as(e, "SpatialPolygons") plot(p, add=T) crop(r, p) -> c
# extent(c) -> ex
# r2 <- raster(ex,nrow=nrow(c)/2, ncol=ncol(c)/2)

#resample the cell size if necessary
new_c <- resample(c, r2, method="ngb")
rast <- as(new_c, "SpatialGridDataFrame")
writeRAST(rast, "rast_img", flags = c("overwrite"))
execGRASS("r.info", map = "rast_img")

#set the region based on the mapset
execGRASS("g.region", raster = "rast_img")
out_raster <- readRAST("rast_img")
str(out_raster)
plot(out_raster)

#watershed delineation
execGRASS("r.watershed", flags="overwrite", 
          parameters=list(elevation="rast_img", threshold=50000,
                          drainage= "fdir",stream="upstream", basin="rbasin"))

execGRASS('r.thin', flags='overwrite',
          parameters =  list(input='upstream', 
                             output='riv_thin'))
execGRASS("r.to.vect", flags='overwrite',
          parameters = list(input="riv_thin", 
                            output="streams", type="line"))
execGRASS('r.to.vect', flags='overwrite',
          parameters =  list(input='rbasin', 
                             output='catchments', type="area"))
execGRASS('v.out.ogr', flags=c('overwrite'),
          parameters=list(input='catchments',
                          output="area.shp",type="area",
                          format="ESRI_Shapefile"))
execGRASS('v.out.ogr',flags=c('overwrite'),parameters=list(input='streams',
                                                           output="streams.shp",type="line",format="ESRI_Shapefile"))

shapefile("streams.shp") -> s
shapefile("area.shp") -> a
plot(out_raster)
plot(a, add=T)
plot(s, add=T, col="blue")