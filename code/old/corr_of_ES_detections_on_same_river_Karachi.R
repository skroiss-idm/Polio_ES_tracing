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
libs <- c('tidyverse','lubridate','ape', 'data.table', 'treedater', 'ggthemes') 
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(l in libs) library(l,character.only = TRUE)
options(tibble.width = Inf)



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




es %>%
  group_by(site_name) %>%
  summarize(n = n(),
            n_wpv1 = sum(wpv1),
            prop_wpv1 = n_wpv1 / n)

















#############################
# Examine genetics
#############################

load('C:/Users/skroiss/Dropbox (IDM)/SteveK/Genetics/Tree impairment/tree_dataAFPES_dropNONE.RData')
timeTree

load('C:/Users/skroiss/OneDrive - IDMOD/Tree impairment/Results/Compiled tree impairment metrics and orphans - 20190321.RData')
df_impairment
list_dataAFPES_dropNONE_nr_sum

source('C:/Users/skroiss/Dropbox (IDM)/SteveK/Genetics/Tree impairment/Polio_genetics_tree_impairment/Function - Extract relatives from timetrees - 20190221.R')












# #######################################
# # Set paths
# #######################################
# 
# user     <- Sys.info()['login']
# root     <- sprintf('C:/Users/%s/OneDrive - IDMOD', user) 
# curated_data_path  <- sprintf('%s/Tree impairment/Curated data',root) 
# results_path <- sprintf('%s/Tree impairment/Results', root)
# ref_path <- sprintf('%s/dataAFPES_dropNONE/tree_dataAFPES_dropNONE.RData', results_path)
# impair_path <- sprintf('%s/impair_AFPorES_retain_other', results_path)
# out_path <- results_path
# 
# 
# 
# #####################################
# ## Import sequence and sample data ##
# #####################################								 
# 
# # Load curated sequences
# seq <- read.dna(sprintf('%s/Curated_AFGPAK_seq_2012to2018 - 20190319.fas',
#                         curated_data_path),
#                 format="fasta", as.matrix=TRUE)  
# 
# 
# 
# # Load sample data
# seq_info <- read_csv(sprintf('%s/Curated metadata for AFGPAK seq from CDC and RRL 2012to2018 - 20190319.csv',
#                              curated_data_path)) %>%
#   # Drop any extras that aren't in seq database
#   filter(SEQNAME %in% row.names(seq)) %>%
#   dplyr::select(SEQNAME, region, dot_name, envsite, sample_date_num, sample_type) %>%
#   rename(label = SEQNAME,
#          # district = dot_name,
#          environmental_site = envsite,
#          num_date = sample_date_num) %>%
#   as.data.table()
# 
# # Filter to Karachi & count cases by district
# seq_info %>%
#   filter(grepl('sindh:khi', dot_name),
#          sample_type == 'AFP') %>%
#   group_by(dot_name) %>%
#   summarize(n = n())
# 
# 
# 
# # ref_tree <- fn_relatives_from_timetree(seq = seq, seq_info = seq_info)
# # ref_tree <- tmp
# # head(ref_tree)
# 
# 
# 
# 
# 
# 
# 
# 
# ## Calculate genetic distance matrix ##
# seqDist <- dist.dna(seq, 
#                     model="TN93", 
#                     as.matrix=TRUE)
# # seqDist[1:3,1:3]
# 
# ## Calculate neighbor-joining phylogenetic tree ##
# phyTree <- bionj(seqDist)
# # phyTree
# 
# library(treedater)
# timeTree <- dater(tre = phyTree,
#                   sts = setNames(seq_info$num_date, seq_info$label))
# rate <- timeTree$meanRate
# class(timeTree) <- 'phylo'
# 
# # class(timeTree)
# 
# 
# # # Plot
# # if(save_plots == T) {
# #   p_tree <- ggtree(timeTree, mrsd = date_decimal(max(seq_info$num_date))) %<+% seq_info +
# #     geom_tippoint() + 
# #     theme_tree2(legend.position="right") +
# #     xlab('year') + 
# #     ggtitle('time-scaled tree')
# #   ggsave(p_tree, filename = paste0('Plot - Subset tree - ', file_suffix, '.png'))
# # }
# 
# # Extract patristic distance between all pairs - Source: http://blog.phytools.org/2015/10/new-reasonably-fast-method-to-compute.html
# relatives_pd <- as.data.table(as.table(cophenetic.phylo(timeTree)))
# 
# # Rename columns - seq1 = origin; # seq2 - current detection
# setnames(relatives_pd, 
#          old = c("V1", "V2", 'N'), 
#          new = c("seq1", "seq2", 'patdist'))
# 
# # Attach tip info
# # Sauce: https://stackoverflow.com/questions/34598139/left-join-using-data-table/34600831
# # Sauce: https://stackoverflow.com/questions/29865512/how-to-merge-two-data-table-by-different-column-names
# # dates
# relatives_pd[seq_info, on = c(seq1 = 'label'), date_num_seq1 := i.num_date]
# relatives_pd[seq_info, on = c(seq2 = 'label'), date_num_seq2 := i.num_date]
# # district
# relatives_pd[seq_info, on = c(seq1 = 'label'), dot_name_seq1 := i.dot_name]
# relatives_pd[seq_info, on = c(seq2 = 'label'), dot_name_seq2 := i.dot_name]
# # sample type
# relatives_pd[seq_info, on = c(seq1 = 'label'), sample_type_seq1 := i.sample_type]
# relatives_pd[seq_info, on = c(seq2 = 'label'), sample_type_seq2 := i.sample_type]
# # es site
# relatives_pd[seq_info, on = c(seq1 = 'label'), environmental_site_seq1 := i.environmental_site]
# relatives_pd[seq_info, on = c(seq2 = 'label'), environmental_site_seq2 := i.environmental_site]
# 
# 
# # Filter to the past links
# relatives_pd_all <- relatives_pd %>%
#   filter(date_num_seq2 >= 2016) %>%
#   mutate(date_diff_21 = date_num_seq2 - date_num_seq1) %>%
#   filter(seq1 != seq2) %>%          # Eliminate 1 to 1 matches
#   filter(date_diff_21 > 0) %>%    # Eliminate same day & negative matches
#   as.data.table()
# 
# 
# 
# relatives_top <- relatives_pd_all %>%
#   arrange(seq2, patdist) %>%
#   group_by(seq2) %>%
#   mutate(rownum = row_number()) %>%
#   filter(rownum == 1)
# head(relatives_top)
# save(relatives_top, file = 'results/nearest_relative.Rdata')




#############################
# Examine genetics
#############################

load('results/nearest_relative.Rdata')
relatives_top


# Filter to KHI ES sites
khi_es1 <- relatives_top %>%
  filter(grepl('sindh:khi', dot_name_seq2),
         environmental_site_seq2 != 'AFP',
         environmental_site_seq1 != 'AFP') %>%
  ungroup() %>%
  dplyr::select(environmental_site_seq1, environmental_site_seq2, patdist)
# Make copy & reverse site names so all are represented
khi_es2 <- khi_es1 %>%
  rename(environmental_site_seq2 = environmental_site_seq1,
         environmental_site_seq1 = environmental_site_seq2)
khi_es <- bind_rows(khi_es1, khi_es2)
khi_es_sum <- khi_es %>% 
  group_by(environmental_site_seq1, environmental_site_seq2) %>%
  summarize(n = n(),
            pd_mean = mean(patdist)) %>%
  filter(n > 2)
khi_es_sum %>%
  ggplot(aes(x = environmental_site_seq1, 
             y = environmental_site_seq2,
             col = pd_mean,
             size = n)) +
  geom_point()











relatives_top %>%
  filter(grepl('sindh:khi', dot_name_seq2),
         environmental_site_seq2 != 'AFP') %>%
  group_by(environmental_site_seq2) %>%
  summarize(n = n(),
            )



