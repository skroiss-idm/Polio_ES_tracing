# Purpose: 
# Inputs:  
# Output:  
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

# Load ES data to plot ES points on map
es <- read_csv('Q:/Data/Polis/Polis_Environmental-201911110000.csv') %>%
  clean_names() %>%
  rename(polio_virus_types = virus_type_s) %>%
  mutate(dot_name = tolower(paste(place_admin_0, place_admin_1, place_admin_2, sep=':')),
         sample_date = as.Date(collection_date, format = '%d-%m-%Y'),
         sample_mo = floor_date(sample_date, unit = 'month'),
         year = year(sample_date),
         wpv1 = grepl('WILD 1', polio_virus_types)) %>%
  filter(place_admin_0 %in% c('AFGHANISTAN', 'PAKISTAN')) %>% 
  filter(grepl('KHI', place_admin_2)) %>% 
  filter(reporting_year >= 2012) %>%
  arrange(place_admin_2) %>%
  mutate(environmental_site = toupper(environmental_site)) %>%
  mutate(environmental_site2 = environmental_site) %>%
  separate(environmental_site2, into = c('site_id', 'site_name'), sep = ' - ') %>%
  rename(lon = x, lat = y)
head(es)


#############################
# Curate
#############################

# Khamiso
# Sohrab
# Rashid Minas Rd Lay

table(es$site_name)

tmp <- es %>%
  filter( !(site_name %in% c('BALDIA COMPOSITE', 'SAJJAN GOTH'))) %>%
  dplyr::select(site_name, sample_mo, wpv1) %>%
  # mutate(wpv1 = ifelse(wpv1 == T, 1, 0)) %>% 
  group_by(site_name, sample_mo) %>%
  summarize(wpv1 = ifelse(sum(wpv1) > 0, 1, 0)) %>%
  spread(site_name, wpv1) %>%
  data.frame()
row.names(tmp) <- tmp$sample_mo
tmp <- tmp[,2:ncol(tmp)]
tmp

library(corrplot)
res <- cor(tmp, use = 'pairwise.complete.obs')
res
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)





#############################
# Curate
#############################


