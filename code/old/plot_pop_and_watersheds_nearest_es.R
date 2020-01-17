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
library(readxl)
library(viridis)


#############################
# Load data
#############################


# Load streams
load('results/streams_in_karachi.RData')
# streams
# stream_coords
# dem


# Load all potential watersheds
load('results/watersheds_in_karachi.Rdata')
# shp_basins
# basin_pop_raster_index
# streams
# stream_coords


# Load current ES watersheds
load('results/es_watersheds_in_karachi_pop.Rdata')
# es_shp_basins
# streams
# es_nearest_pts
# es_coords_near
# es_coords_far


# Load Karachi admin 4 boundaries     
load('data/admin2_karachi.RData')
# shp_khi

# Load ES site coordinates
load('results/es_coords_novelt_khi.RData')
es_coords_novelt_polis
# load('results/es_coords_khi.RData')
# es_coords_khi


# Load pop raster
pop <- raster('results/worldpop_ppp_2018_karachi.tif', values = T)


# UC Map
uc <- st_read('C:/Users/skroiss/Dropbox (IDM)/AMUG/Data/Pakistan/GIS and GPS/PAK_adm_EOC_20160228/PAK_adm4_EOC_20160228.shp')
uc_khi <- uc %>% 
  filter(grepl('SINDH', WHO_PROV_N),
         grepl('KHI', WHO_DIST_N))
tm_shape(uc_khi) +
  tm_polygons()


# Load and curate country AFP data for Pakistan
# afp_old <- read_xlsx('C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/AFP/Pakistan/AFP_wUC_20170619/AFP_Data.xlsx',
#                  sheet = 'Sheet1')
# table(afp_old$YRONSET, afp_old$P1)
afp <- read_csv('C:/Users/skroiss/Dropbox (IDM)/SMUG Folder/Polio/EMRO_RRL/AFP/AFP_Cases_20191222.CSV') %>%
  mutate(wpv1 = ifelse(P1 == 1, T, F),
         sabin = ifelse(P1 != 1 & (P1 == 2 | P2 == 2 | P3 ==2), T, F),
         polio_type = case_when(
           P1 == 1 ~ 'WPV1',
           P1 %in% c(6,7,8) | P2 %in% c(6,7,8) | P3 %in% c(6,7,8) ~ 'VDPV',
           P1 == 2 | P2 == 2 | P3 == 2 ~ 'Sabin',
           TRUE ~ 'No polio'
         )) 
table(afp$YRONSET, afp$P1)
# Filter to Karachi
afp_khi <- afp %>% 
  filter(grepl('SINDH', PROVINCE),
         grepl('KHI', DISTRICT),
         wpv1 == T) %>%
  rename(WHO_UC_C = UCODE) %>%
  group_by(UC, WHO_UC_C) %>%
  summarize(n_wpv1 = n())
head(afp_khi)
table(afp_khi$UC)
afp_uc_code <- unique(afp_khi$WHO_UC_C)
afp_uc_code %in% uc_khi$WHO_UC_C
# Attach case count to UC map
uc_khi <- uc_khi %>% 
  left_join(afp_khi)
tm_shape(uc_khi) +
  tm_polygons(col = 'n_wpv1')



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
# Maps
#############################

# change back to the plotting mode
# tmap_mode("plot")
# tmaptools::palette_explorer()

# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue
# 58C9B9  # teal


# zoom to ES sites
es_bbox <- st_bbox(st_union(es_shp_basins, es_coords_novelt_polis))
x_zoom = 0.002 
y_zoom = 0.001
es_bbox[1] <- es_bbox[1] * (1-x_zoom)
es_bbox[3] <- es_bbox[3] * (1+x_zoom)
es_bbox[2] <- es_bbox[2] * (1-y_zoom)
es_bbox[4] <- es_bbox[4] * (1+y_zoom)


map_dem_uc_sites <- tm_shape(dem) +
  tm_raster(n=20, 
            palette = terrain.colors(30)[10:30],
            # palette = '-magma',
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            # palette = gray.colors(20, start = .95, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0, border.col = 'grey60')+
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .25)


map_dem_uc_sites_streams <- tm_shape(dem) +
  tm_raster(n=20, 
            palette = terrain.colors(30)[10:30],
            # palette = '-magma',
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            # palette = gray.colors(20, start = .95, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0, border.col = 'grey60') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .25)


map_dem_uc_sites_streams_zoom <- tm_shape(dem, bbox = es_bbox) +
  tm_raster(n=20, 
            palette = terrain.colors(30)[10:30],
            # palette = '-magma',
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            # palette = gray.colors(20, start = .95, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0, border.col = 'grey60') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .25)


map_dem_uc_sites_streams_basins_zoom <- tm_shape(dem, bbox = es_bbox) +
  tm_raster(n=20, 
            palette = terrain.colors(30)[10:30],
            # palette = '-magma',
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            # palette = gray.colors(20, start = .95, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0, border.col = 'grey60') +
  tm_shape(es_shp_basins) +
  tm_polygons(col = '#FC9D62', alpha = 0,  border.col = '#7F224E', lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .25)


# map_pop_uc_sites_streams_basins <- tm_shape(pop, bbox = es_bbox) +
#   tm_raster(n=20, 
#             # palette = 'viridis',
#             # palette = '-magma',
#             palette = viridis(20, begin = 1, end = 0, option = 'magma'),
#             # palette = gray.colors(20, start = 0.1, end = 0.95),
#             # palette = gray.colors(20, start = .90, end = 0),
#             legend.show = F) +
#   tm_shape(uc_khi) +
#   tm_polygons(alpha = 0, border.col = 'grey60') +
#   tm_shape(es_shp_basins) +
#   tm_polygons(col = '#FC9D62', alpha = 0,  border.col = '#7F224E') +
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25)
# map_pop_uc_sites_streams_basins


# plot
map_pop_uc_sites_streams_basins_zoom <- tm_shape(pop, bbox = es_bbox) +
  tm_raster(n=20, 
            # palette = 'viridis',
            # palette = '-magma',
            # palette = viridis(20, begin = 1, end = 0, option = 'magma'),
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            palette = gray.colors(20, start = 1, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0, border.col = 'grey60') +
  tm_shape(es_shp_basins) +
  tm_polygons(col = '#FC9D62', alpha = 0,  border.col = '#7F224E', lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .25)
map_pop_uc_sites_streams_basins_zoom

# library(viridis)
# library(scales)
# show_col(viridis(20, begin = 0.9, end = 0, option = 'magma'))


png('figures/map_dem_uc_sites.png', width = 7, height = 7, units = 'in', res = 300)
map_dem_uc_sites
dev.off()

png('figures/map_dem_uc_sites_streams.png', width = 7, height = 7, units = 'in', res = 300)
map_dem_uc_sites_streams
dev.off()

# map_dem_uc_sites_streams_zoom
# map_dem_uc_sites_streams_basins_zoom
# map_pop_uc_sites_streams_basins_zoom

png('figures/map_dem_uc_sites_streams_zoom.png', width = 7, height = 7, units = 'in', res = 300)
map_dem_uc_sites_streams_zoom
dev.off()

png('figures/map_dem_uc_sites_streams_basins_zoom.png', width = 7, height = 7, units = 'in', res = 300)
map_dem_uc_sites_streams_basins_zoom
dev.off()

png('figures/map_pop_uc_sites_streams_basins_zoom.png', width = 7, height = 7, units = 'in', res = 300)
map_pop_uc_sites_streams_basins_zoom
dev.off()





# m1 <- tm_shape(pop) +  #  bbox = st_bbox(filter(uc_khi, n_wpv1 >=1))
#   tm_raster(n=20, 
#             # palette = '-magma',
#             # palette = gray.colors(20, start = 0.1, end = 0.95),
#             palette = gray.colors(20, start = .95, end = 0),
#             
#             legend.show = F) +
#   tm_shape(uc_khi) +
#   tm_polygons(alpha = 0, border.col = 'grey60') +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25)
# m1
# 
# # Might want to zoom here
# m2 <- m1 +
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25)
# m2  
# 
# 
# tm_shape(pop) +  #  bbox = st_bbox(filter(uc_khi, n_wpv1 >=1))
#   tm_raster(n=20, 
#             # palette = '-magma',
#             # palette = gray.colors(20, start = 0.1, end = 0.95),
#             palette = gray.colors(20, start = .95, end = 0),
#             
#             legend.show = F) +
#   tm_shape(es_shp_basins) +
#   tm_polygons(col = '#FDC68C', alpha = 1,  border.col = '#7F224E') +
#   tm_shape(uc_khi) +
#   tm_polygons(alpha = 0, border.col = 'grey60') +
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25)
# 
# 
# 
# tm_shape(pop) +  #  bbox = st_bbox(filter(uc_khi, n_wpv1 >=1))
#   tm_raster(n=20, 
#             # palette = '-magma',
#             # palette = gray.colors(20, start = 0.1, end = 0.95),
#             palette = gray.colors(20, start = .95, end = 0),
#             
#             legend.show = F) +
#   tm_shape(uc_khi) +
#   tm_polygons(alpha = 0, border.col = 'grey60') +
#   tm_shape(es_shp_basins) +
#   tm_polygons(col = '#FDC68C', alpha = 0,  border.col = '#FC9D62') +
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25)
# 
# 
# 
# 
# 
# # basic map
# m_pop <- tm_shape(pop) +  #  bbox = st_bbox(filter(uc_khi, n_wpv1 >=1))
#   tm_raster(n=20, 
#             # palette = '-magma',
#             # palette = gray.colors(20, start = 0.1, end = 0.95),
#             palette = gray.colors(20, start = 0.95, end = 0),
#             
#             legend.show = F) +
#   tm_shape(uc_khi) +
#   tm_polygons(alpha = 0) 
# m_pop
# 
# m_pop_stream_es <- m_pop + 
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25)
# m_pop_stream_es
# 
# m_pop_stream_es_cases <- m_pop + 
#   tm_shape(filter(uc_khi, n_wpv1 >=1)) +
#   # tm_polygons(alpha = 0, lwd = 3) +  # col = 'n_wpv1', colorNA = NULL,
#   # tm_polygons(col = 'n_wpv1', palette = '-magma', colorNA = NULL, lwd = 3) +  # col = 'n_wpv1', colorNA = NULL,
#   tm_polygons(col = 'n_wpv1', palette = 'Reds', alpha = .8, colorNA = NULL, lwd = 3) +  # col = 'n_wpv1', colorNA = NULL,
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = '#E55383', size = .25) 
# m_pop_stream_es_cases
# 
# m_pop_stream_es_cases +
#   tm_shape(es_nearest_pts) +
#   tm_dots(col = 'black', size = .25) 
# 
# m_pop_stream_es_cases +
#   tm_shape(es_shp_basins) +
#   tm_polygons(col = '#FDC68C', alpha = 0.5,  border.col = 'grey80')
# 
# 
# 
# 
# png('figures/map_khi_pop_uc.png', width = 7, height = 7, units = 'in', res = 300)
# m_pop
# dev.off()
# 
# png('figures/map_khi_pop_uc_stream_es.png', width = 7, height = 7, units = 'in', res = 300)
# m_pop_stream_es
# dev.off()
# 
# png('figures/map_khi_pop_uc_stream_es_cases.png', width = 7, height = 7, units = 'in', res = 300)
# m_pop_stream_es_cases
# dev.off()



#############################
# Interactive pop in each watershed
#############################


# change back to the plotting mode
tmap_mode("view")

m_pop_stream_es














#############################
# Plot pop in each watershed
#############################


# # Streams & sample points + WATERSHEDS + ES sites
# p <- tm_shape(pop) +
#   tm_raster(n=20, palette = '-magma') +
#   tm_shape(shp_khi) +
#   tm_polygons(alpha = 0)+
#   tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
#   tm_shape(streams) +
#   tm_lines(col = '#30A9DE', lwd = 2) +
#   tm_shape(stream_coords) +
#   tm_dots(col = '#7F224E', size = .1) +
#   tm_shape(es_coords_novelt_polis) +
#   tm_dots(col = 'black', size = .5) +
#   tm_text(text = 'site_name', auto.placement = T, size = .5) 
# tmap_mode("view")
# p





upstream_index <- c(224, 268, 256, 252, 195, 238, 216, 236, 186, 231, 226, 96)
subset(stream_coords, id %in% upstream_index)$pop_sum
hist(subset(stream_coords, id %in% upstream_index)$pop_sum, 
     xlim = c(0, max(es_shp_basins$pop_sum) * 1.1))
hist(es_shp_basins$pop_sum, breaks = 20, 
     col = 'red',
     add = T)
lines(x = rep(min(es_shp_basins$pop_sum), 2), y = c(0,10),
      lty = 'dashed', col = 'red')
lines(x = rep(max(es_shp_basins$pop_sum), 2), y = c(0,10),
      lty = 'dashed', col = 'red')









# ES zoom
tm_shape(pop) +  #  bbox = st_bbox(filter(uc_khi, n_wpv1 >=1))
  tm_raster(n=20, palette = '-magma', legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(alpha = 0) +
  
  tm_shape(filter(uc_khi, n_wpv1 >=1)) +
  tm_polygons(alpha = 0, lwd = 3) +  # col = 'n_wpv1', colorNA = NULL, 
  
  # tm_shape(shp_khi) +
  # tm_polygons(alpha = 0) +
  # tm_shape(shp_basins) +
  # tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  # tm_shape(stream_coords) +
  # tm_dots(col = '#7F224E', size = .1) +
  # tm_shape(es_coords_khi) +
  # tm_dots(col = '#E55383', size = .5) +
  # tm_text(text = 'site_name', auto.placement = T, size = .5) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#58C9B9', size = .25)


# Inset
p_inset <- tm_shape(pop) +
  tm_raster(n=20, palette = '-magma', legend.show = F) +
  # tm_shape(uc_khi) +
  # tm_polygons(col = 'n_wpv1') +
  tm_shape(shp_khi) +
  tm_polygons(alpha = 0)
  # tm_shape(shp_basins) +
  # tm_polygons(col = '#FDC68C', alpha = 0,  border.col = 'grey80') +
  # tm_shape(streams) +
  # tm_lines(col = '#30A9DE', lwd = 2) +
  # tm_shape(stream_coords) +
  # tm_dots(col = '#7F224E', size = .1) +
  # tm_shape(es_coords_khi) +
  # tm_dots(col = '#E55383', size = .5) +
  # tm_text(text = 'site_name', auto.placement = T, size = .5) +
  # tm_shape(es_coords_novelt_polis) +
  # tm_dots(col = '#58C9B9', size = .25) 













# # change back to the plotting mode
# tmap_mode("plot")
# 
# # plot contiguous US
# tm_shape(US_cont, projection=2163) +
#   tm_polygons(border.col = "grey50", border.alpha = .5, title = "", showNA = TRUE) +
#   tm_shape(US_states) +
#   tm_borders(lwd=1, col = "black", alpha = .5) +
#   tm_credits("Data @ Unites States Department of Agriculture\nShape @ Unites States Census Bureau", position = c("right", "bottom")) +
#   tm_layout(title.position = c("center", "top"), 
#             legend.position = c("right", "bottom"), 
#             frame = FALSE, 
#             inner.margins = c(0.1, 0.1, 0.05, 0.05))
# 
# # Alaska inset
# m_AK <- tm_shape(US_AK, projection = 3338) +
#   tm_polygons(border.col = "grey50", border.alpha = .5, breaks = seq(10, 50, by = 5)) +
#   tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)
# 
# # Hawaii inset
# m_HI <- tm_shape(US_HI, projection = 3759) +
#   tm_polygons(border.col = "grey50", border.alpha = .5, breaks=seq(10, 50, by = 5)) +
#   tm_layout(legend.show = FALSE, bg.color=NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame=FALSE)
# 
# # print insets
# print(m_AK, vp=viewport(x= 0.15, y= 0.15, width= 0.3, height= 0.3))
# print(m_HI, vp=viewport(x= 0.4, y= 0.1, width= 0.2, height= 0.1))






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


