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
library(terra)
library(ggrepel)
library(ggsflabel) # devtools::install_github("yutannihilation/ggsflabel")

set.seed(20210122)

source("gaia-trails-sp.R")
source("gaia-trails.R")

# tn valley
source("create-seven-islands-map.R")

# smokies
source("create-tremont-trail-map.R")
source("create-abrams-trail-map.R")

# emory falls
source("create-emory-falls-map.R")