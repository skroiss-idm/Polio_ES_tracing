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

load('results/tracing_watersheds_in_karachi.Rdata')
# tracing_basins
# tracing_basin_pop_raster_index


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
afp_old <- read_xlsx('C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/AFP/Pakistan/AFP_wUC_20170619/AFP_Data.xlsx',
                 sheet = 'Sheet1') %>%
  mutate(wpv1 = ifelse(P1 == 1, T, F),
         sabin = ifelse(P1 != 1 & (P1 == 2 | P2 == 2 | P3 ==2), T, F),
         polio_type = case_when(
           P1 == 1 ~ 'WPV1',
           P1 %in% c(6,7,8) | P2 %in% c(6,7,8) | P3 %in% c(6,7,8) ~ 'VDPV',
           P1 == 2 | P2 == 2 | P3 == 2 ~ 'Sabin',
           TRUE ~ 'No polio'
         )) %>%
  rename(WHO_UC_C = CodeUC) %>%
  filter(YRONSET <= 2014)
table(afp_old$YRONSET, afp_old$P1)
afp_new <- read_csv('C:/Users/skroiss/Dropbox (IDM)/SMUG Folder/Polio/EMRO_RRL/AFP/AFP_Cases_20191222.CSV') %>%
  mutate(wpv1 = ifelse(P1 == 1, T, F),
         sabin = ifelse(P1 != 1 & (P1 == 2 | P2 == 2 | P3 ==2), T, F),
         polio_type = case_when(
           P1 == 1 ~ 'WPV1',
           P1 %in% c(6,7,8) | P2 %in% c(6,7,8) | P3 %in% c(6,7,8) ~ 'VDPV',
           P1 == 2 | P2 == 2 | P3 == 2 ~ 'Sabin',
           TRUE ~ 'No polio'
         )) %>%
  rename(WHO_UC_C = UCODE)
table(afp_new$YRONSET, afp_new$P1)
# Filter to Karachi

afp <- bind_rows(afp_old %>%
            dplyr::select(PROVINCE, DISTRICT, WHO_UC_C, YRONSET, wpv1), 
            afp_new %>%
            dplyr::select(PROVINCE, DISTRICT, WHO_UC_C, YRONSET, wpv1))
afp_khi <- afp %>% 
  filter(grepl('SINDH', PROVINCE),
         grepl('KHI', DISTRICT),
         wpv1 == T) %>%
  group_by(WHO_UC_C) %>%
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


v <- getValues(pop)
pop_sum <- c()
for(i in 1:length(tracing_basin_pop_raster_index)){
  pop_sum[i] <- sum(v[tracing_basin_pop_raster_index[[i]]], na.rm = T)
}
tracing_basins$pop_sum <- pop_sum
tracing_basins_pop <- shp_basins %>%
  group_by(id) %>%
  summarize(pop_sum = sum(pop_sum)) %>%
  st_drop_geometry
sum_basin_pop

# tracing_basins
# tracing_basin_pop_raster_index



#############################
# Basic maps of elevation, streams, ES sites, & population
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
  tm_dots(col = '#E55383', size = .5)


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
  tm_dots(col = '#E55383', size = .5)


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
  tm_polygons(col = '#FC9D62', alpha = 0,  border.col = '#E55383', lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .5)

# E55383  # pink - current ES
# FC9D62  # orange - confluences
# E7B800  # goldenrod - tracing

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
  tm_polygons(col = '#E55383', alpha = 0,  border.col = '#E55383', lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .5)
map_pop_uc_sites_streams_basins_zoom

# E55383  # pink - current ES
# FC9D62  # orange - confluences
# E7B800  # goldenrod - tracing

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



#############################
# Interactive pop in each watershed
#############################

# # change back to the plotting mode
# tmap_mode("view")
# m_pop_stream_es




#############################
# Map venn diagram of cases & current catchments to ID areas to add permanent ES sites
#############################

# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue
# 58C9B9  # teal

# zoom to ES sites
case_bbox <- st_bbox(st_union(es_shp_basins, 
                            uc_khi %>% filter(n_wpv1 >= 1)))
x_zoom = 0.0001
y_zoom = 0.002
case_bbox[1] <- case_bbox[1] * (1-x_zoom)
case_bbox[3] <- case_bbox[3] * (1+x_zoom)
case_bbox[2] <- case_bbox[2] * (1-y_zoom)
# case_bbox[4] <- case_bbox[4] * (1+y_zoom)

# Map venn diagram of cases & current catchments
map_venn_cases_catchments <- tm_shape(pop, bbox = case_bbox) +  # , bbox = es_bbox
  tm_raster(n=20, 
            # palette = 'viridis',
            # palette = '-magma',
            # palette = viridis(20, begin = 1, end = 0, option = 'magma'),
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            palette = gray.colors(20, start = .95, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(col = '#FDC68C', alpha = 0, border.col = 'grey60', colorNA = NULL,
              palette = '#FDC68C', legend.show = F) +
  # tm_polygons(col = 'n_wpv1', alpha = .3, border.col = 'grey60', colorNA = NULL,
  #             palette = '#FDC68C', legend.show = F) +
  tm_shape(uc_khi %>% filter(n_wpv1 >= 1)) +
  tm_polygons(col = 'n_wpv1', alpha = 0.4, border.col = '#7F224E', colorNA = NULL,
              palette = '#7F224E',
              legend.show = F, lwd = 2) +
  tm_shape(es_shp_basins) +
  tm_polygons(col = '#E55383', alpha = 0.07,  border.col = '#E55383', lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .5)
map_venn_cases_catchments

# E55383  # pink - current ES
# FC9D62  # orange - confluences
# E7B800  # goldenrod - tracing

png('figures/map_venn_cases_catchments.png', width = 7, height = 7, units = 'in', res = 300)
map_venn_cases_catchments
dev.off()



#############################
# Map points above confluence points as potential permanent ES sites
#############################

tmap_mode("plot")

# FDC68C  # light orange
# E55383  # pink  
# FC9D62  # orange
# 7F224E  # purple  
# 30A9DE  # blue
# 58C9B9  # teal

# E55383  # pink - current ES
# FC9D62  # orange - upstream
# E7B800  # goldenrod - tracing

# zoom to ES sites
basin_bbox <- st_bbox(stream_coords %>% filter(id %in% upstream_index ))
x_zoom = 0.0007
y_zoom = 0.0007
basin_bbox[1] <- basin_bbox[1] * (1-x_zoom*2)
basin_bbox[3] <- basin_bbox[3] * (1+x_zoom/2)
basin_bbox[2] <- basin_bbox[2] * (1-y_zoom)
basin_bbox[4] <- basin_bbox[4] * (1+y_zoom)
# basin_bbox <- st_bbox(es_shp_basins)
# x_zoom = 0.002
# # y_zoom = 0.002
# basin_bbox[1] <- basin_bbox[1] * (1-x_zoom)
# basin_bbox[3] <- basin_bbox[3] * (1+x_zoom)
# # basin_bbox[2] <- basin_bbox[2] * (1-y_zoom)
# # basin_bbox[4] <- basin_bbox[4] * (1+y_zoom)

# Map venn diagram of cases & current catchments
map_catchments_confluence_pts <- tm_shape(pop, bbox = basin_bbox) +  # , bbox = es_bbox
  tm_raster(n=20, 
            # palette = 'viridis',
            # palette = '-magma',
            # palette = viridis(20, begin = 1, end = 0, option = 'magma'),
            # palette = gray.colors(20, start = 0.1, end = 0.95),
            palette = gray.colors(20, start = 1, end = 0),
            legend.show = F) +
  tm_shape(uc_khi) +
  tm_polygons(col = '#FDC68C', alpha = 0, border.col = 'grey60', colorNA = NULL,
              palette = '#FDC68C', legend.show = F) +
  # tm_polygons(col = 'n_wpv1', alpha = .3, border.col = 'grey60', colorNA = NULL,
  #             palette = '#FDC68C', legend.show = F) +
  tm_shape(es_shp_basins) +
  tm_polygons(col = '#E55383', alpha = 0.07,  border.col = '#E55383', lwd = 2) +
  # tm_shape(uc_khi %>% filter(n_wpv1 >= 1)) +
  # tm_polygons(col = 'n_wpv1', alpha = 0.4, border.col = '#FDC68C', colorNA = NULL,
  #             palette = '#FDC68C', legend.show = F, lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .75) +
  tm_shape(stream_coords %>% filter(id %in% upstream_index )) +
  tm_dots(col = '#FC9D62', size = .75) 
map_catchments_confluence_pts

# E55383  # pink - current ES
# FC9D62  # orange - confluences
# E7B800  # goldenrod - tracing

png('figures/map_catchments_confluence_pts.png', width = 7, height = 7, units = 'in', res = 300)
map_catchments_confluence_pts
dev.off()



#############################
# Map potential tracing points
#############################



# Map points at start, midpoint, and end of each stream
upstream_index <- c(224, 268, 256, 252, 195, 238, 216, 236, 186, 231, 226, 96, 281, 271, 278)
# Convert to simple feature
proj <- st_crs(streams)$proj4string
tracing_pts <- streams %>%
  st_as_sf() %>%
  filter(stream %in% c(288, 322, upstream_index)) %>%
  st_coordinates() %>%
  data.frame() %>%
  rename(id = L1,
         lon = X,
         lat = Y) %>%
  group_by(id) %>%
  mutate(n = n(),
         id_row = row_number(),
         stream_location = case_when(
           id_row == min(id_row) ~ 'start',
           id_row == max(id_row) ~ 'end',
           id_row == round(mean(id_row),0) ~ 'midpoint')) %>%
  filter(!is.na(stream_location))%>% 
  st_as_sf(coords = c('lon', 'lat'), crs = proj) %>%
  filter(stream_location != 'end') %>%
  # drop the start points for main channel cuz they've got permaments above the confluence
  filter(!(id %in% c(12, 10, 9, 8, 7) & stream_location == 'start')) 
  
  
  
# zoom to ES sites
tracing_bbox <- st_bbox(tracing_pts %>%
                        filter(stream_location != 'end') %>%
                        # drop the start points for main channel cuz they've got permaments above the confluence
                        filter(!(id %in% c(12, 10, 9, 8, 7) & stream_location == 'start')))
x_zoom = 0.0009
y_zoom = 0.0009
tracing_bbox[1] <- tracing_bbox[1] * (1-x_zoom*2)
tracing_bbox[3] <- tracing_bbox[3] * (1+x_zoom/2)
tracing_bbox[2] <- tracing_bbox[2] * (1-y_zoom)
tracing_bbox[4] <- tracing_bbox[4] * (1+y_zoom)

# tmap_mode("plot")


m_tracing_mid_ends <- tm_shape(pop, bbox = tracing_bbox) +
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
  tm_polygons(col = '#E55383', alpha = 0,  border.col = '#E55383', lwd = 2) +
  tm_shape(streams) +
  tm_lines(col = '#30A9DE', lwd = 2) +
  tm_shape(es_coords_novelt_polis) +
  tm_dots(col = '#E55383', size = .75)  +
  tm_shape(stream_coords %>% filter(id %in% upstream_index )) +
  tm_dots(col = '#FC9D62', size = .75) +
  tm_shape(tracing_pts )+
           # %>%
           #   filter(stream_location != 'end') %>%
           #   # drop the start points for main channel cuz they've got permaments above the confluence
           #   filter(!(id %in% c(12, 10, 9, 8, 7) & stream_location == 'start'))) +
           #   # filter(!(id %in% c(16,17) & stream_location == 'end'))) +
  tm_dots(col = '#E7B800', size = .75) 
m_tracing_mid_ends

# E55383  # pink - current ES
# FC9D62  # orange - confluences
# E7B800  # goldenrod - tracing

png('figures/m_tracing_mid_ends.png', width = 7, height = 7, units = 'in', res = 300)
m_tracing_mid_ends
dev.off()




tmap_mode("plot")
m_tracing_mid_ends

# # E55383  # pink - current ES
# # FC9D62  # orange - upstream
# # E7B800  # goldenrod - tracing



#############################
# Plot histogram of pop in each watershed
#############################

upstream_index <- c(224, 268, 256, 252, 195, 238, 216, 236, 186, 231, 226, 96, 281, 271, 278)

# # Tracing points
# tracing_index <- stream_coords %>% 
#   filter(pop_sum > 1000) %>%
#   st_crop(case_bbox) %>% 
#   pull(id)

tracing_basins



tmp <- bind_rows(stream_coords %>%
                   mutate(type = case_when(
                     id %in% upstream_index ~ 'confluences')) %>%  # id %in% tracing_index ~ 'tracing')
                   data.frame() %>%
                   dplyr::select(id, type, pop_sum),
                 tracing_basins %>%
                   mutate(type = 'tracing') %>%
                   data.frame() %>%
                   dplyr::select(id,type, pop_sum)) %>%
  bind_rows(es_shp_basins %>%
              mutate(type = 'current ES') %>%
              data.frame() %>%
              dplyr::select(id, type, pop_sum))
tmp$type <- factor(tmp$type, 
                   levels = c('current ES', 'confluences', 'tracing'))


# FDC68C  # light orange
# E55383  # pink - current ES
# FC9D62  # orange - upstream
# 7F224E  # purple  
# 30A9DE  # blue
# 58C9B9  # teal
# 00AFBB  # ggplot teal
# E7B800  # goldenrod - tracing
# FC4E07  # orangy red

# E55383  # pink - current ES
# FC9D62  # orange - upstream
# E7B800  # goldenrod - tracing

h_es <- tmp %>%
  filter(!is.na(type), type != 'tracing') %>%
  filter(type == 'current ES') %>%
  ggplot(aes(x=pop_sum / 1000000, fill = type)) +
  geom_histogram(binwidth = .1) +
  lims(x = c(0,2.5),
       y = c(0,5)) +
  scale_fill_manual(values = c("#E55383", "#FC9D62")) +  # , "#FC4E07"
  labs(x = 'Total population (millions)',
       y = 'Count',
       fill = NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  # theme_bw() +
  theme(legend.position=c(.8,.75))
h_es_confluences <- tmp %>%
  filter(!is.na(type), type != 'tracing') %>%
  ggplot(aes(x=pop_sum / 1000000, fill = type)) +
  geom_histogram(binwidth = .1) +
  lims(x = c(0,2.5),
       y = c(0,5)) +
  scale_fill_manual(values = c("#E55383", "#FC9D62")) +  # , "#FC4E07"
  labs(x = 'Total population (millions)',
       y = 'Count',
       fill = NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(legend.position=c(.8,.75))
h_es_confluences_tracing <- tmp %>%
  filter(!is.na(type)) %>%
  ggplot(aes(x=pop_sum / 1000000, fill = type)) +
  geom_histogram(binwidth = .1) +
  lims(x = c(-.1,2.5)) +
       # y = c(0,5)) +
  # lims(x = c(-100000,2500000)) +
  #      y = c(0,5)) +
  scale_fill_manual(values = c("#E55383", "#FC9D62", "#E7B800")) +  # 
  labs(x = 'Total population (millions)',
       y = 'Count',
       fill = NULL) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(legend.position=c(.8,.75))


ggsave('figures/hist_pop_es.png', 
       plot = h_es, 
       width = 5, height = 3)
ggsave('figures/hist_pop_es_confluences.png', 
       plot = h_es_confluences, 
       width = 5, height = 3)
ggsave('figures/hist_pop_es_confluences_tracing.png', 
       plot = h_es_confluences_tracing, 
       width = 5, height = 3)
h_es
h_es_confluences
h_es_confluences_tracing









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












