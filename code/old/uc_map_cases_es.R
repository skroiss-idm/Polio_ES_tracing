


uc <- st_read('C:/Users/skroiss/Dropbox (IDM)/AMUG/Data/Pakistan/GIS and GPS/PAK_adm_EOC_20160228/PAK_adm4_EOC_20160228.shp')

uc_khi <- uc %>% 
  filter(grepl('SINDH', WHO_PROV_N),
         grepl('KHI', WHO_DIST_N))
tm_shape(uc_khi) +
  tm_polygons()


library(readxl)
# Load and curate country AFP data for Pakistan
afp <- read_xlsx('C:/Users/skroiss/Dropbox (IDM)/AMUG_Data/AFP/Pakistan/AFP_wUC_20170619/AFP_Data.xlsx',
                 sheet = 'Sheet1') %>%
  mutate(wpv1 = ifelse(P1 == 1, T, F),
         sabin = ifelse(P1 != 1 & (P1 == 2 | P2 == 2 | P3 ==2), T, F),
         polio_type = case_when(
           P1 == 1 ~ 'WPV1',
           P1 %in% c(6,7,8) | P2 %in% c(6,7,8) | P3 %in% c(6,7,8) ~ 'VDPV',
           P1 == 2 | P2 == 2 | P3 == 2 ~ 'Sabin',
           TRUE ~ 'No polio'
         )) 


afp_khi <- afp %>% 
  filter(grepl('SINDH', PROVINCE),
         grepl('KHI', DISTRICT),
         wpv1 == T) %>%
  rename(WHO_UC_C = CodeUC) %>%
  group_by(UC, WHO_UC_C) %>%
  summarize(n_wpv1 = n())
head(afp_khi)
table(afp_khi$UC)

afp_uc_code <- unique(afp_khi$CodeUC)
afp_uc_code %in% uc_khi$WHO_UC_C

uc_khi <- uc_khi %>% 
  left_join(afp_khi)

tm_shape(uc_khi) +
  tm_polygons(col = 'n_wpv1')



# Streams & sample points + WATERSHEDS + ES sites
tm_shape(pop) +
  tm_raster(n=20, palette = '-magma') +
  # tm_shape(uc_khi) +
  # tm_polygons(col = 'n_wpv1') +
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
  # tm_text(text = 'site_name', auto.placement = T, size = .5)
