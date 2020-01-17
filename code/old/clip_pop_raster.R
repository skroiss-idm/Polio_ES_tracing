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
library(sf)
library(raster)
library(tmap)



#############################
# Load data
#############################

# load Karachi admin boundaries
load('admin2_karachi.RData')
shp_khi

# load worldpop data
pop_pak <- raster('C:/Users/skroiss/Dropbox (IDM)/SMUG Folder/Worldpop/Pakistan/total/pak_ppp_2018.tif')
pop <- crop(x = pop_pak, 
                 y = extent(shp_khi), 
                 filename = 'results/worldpop_ppp_2018_karachi.tif',
            overwrite = T)
plot(pop)
