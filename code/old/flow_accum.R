
# Load data
load('results/streams_in_karachi.RData')
streams
stream_coords
dem


# Plot
tm_shape(dem) +
  tm_raster() +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1)











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
fdir <- readRAST("fdir")


# Step 6 - Extract watershed basins from a drainage direction map


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
                            output=paste0("results/shapefiles/watershed_",
                                          i, 
                                          ".shp"),
                            type="area", format="ESRI_Shapefile"))
}



# Load all those shapefiles and combine
files <- list.files("results/shapefiles", pattern = "watershed_[0-9]{,3}.shp")
shp_basins <- st_read(file.path("results/shapefiles", files[1])) %>%
  mutate(file = files[1])
for(i in 2:length(files)){
  shp_tmp <- st_read(file.path("results/shapefiles", files[i])) %>%
    mutate(file = files[i])
  shp_basins <- rbind(shp_basins, shp_tmp)
}
shp_basins <- shp_basins %>%
  mutate(id = file,
         id = as.numeric(gsub('watershed_|\\.shp', '', id)))

# Plot
tm_shape(shp_basins) +
  tm_polygons(col = '#FDC68C', alpha = .05,  border.col = NULL) +
  tm_shape(streams) +
  tm_lines(col = '#E55383', lwd = 2) +
  tm_shape(stream_coords) +
  tm_dots(col = '#7F224E', size = .1)
# FDC68C # light orange
# E55383 # pink  
# FC9D62 # orange
# 7F224E # purple  



save(shp_basins,
     file = 'results/watersheds_in_karachi.Rdata')

