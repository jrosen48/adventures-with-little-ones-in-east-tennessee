source("gaia-trails.R")

library(ggmap)
library(sf)
library(tidyverse)
library(ggmap)
library(osmdata)
library(geosphere)
library(ggsn)
library(slopes)
library(showtext)
library(cowplot)
library(here)
library(ggspatial)

name = "Abrams Falls" 
file_name_1 = "abrams-little-bottoms.gpx"
auxiliary_trail_files = "GRSM_TRAILS.shp"
long_multiplier = .5
lat_multiplier = 1
zoom = 15
lon_denom = 50
lat_denom = 15
grid_plot_height = .65
grid_slope_height = .35
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35

t <- create_and_save_trailmap(name = "Abrams Falls", 
                              file_name_1 = "abrams-little-bottoms.gpx",
                              auxiliary_trail_files = "GRSM_TRAILS.shp")

t

for_elev_raster <- bb %>% 
    t() %>% 
    as.data.frame()

r <- get_elev_raster(locations = for_elev_raster,
                prj = "EPSG:4326", # WGS84
                z = 10)
