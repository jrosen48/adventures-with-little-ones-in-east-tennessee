# Libraries

## credit to Isabella Velásquez for the template for this code: https://github.com/ivelasq

library(ggmap)
library(sf)
library(tidyverse)

# Open files

f <- 
    here::here("data", "raw", "GRSM_TRAILS", "GRSM_TRAILS.shp")

shp <- st_read(f)

coords <-
    st_coordinates(shp) %>% 
    as_data_frame()

# Let's try it with the first trail

t1 <- coords %>% 
    filter(L1 == 1)

# Plot

bb <- getbb("")
bb[1, 1] <- -83.98 # x min - how far west
bb[1, 2] <- -83.80 # x max - how far east
bb[2, 1] <- 35.55 # y min - how far south
bb[2, 2] <- 35.65 # ymax - how far north

m <- get_stamenmap(bb, maptype = "terrain", zoom = 13)

ggmap(m) +
    geom_point(data = t1, 
               aes(x = X,
                   y = Y), 
               size = .30,
               color = "#A0522D") +
    ggtitle("Abrams Falls Trail") +
    theme_void()

# Save file

ggsave(here::here("output", "abrams-falls-map.png"), dpi = "retina", width = 4, height = 4, units = "in")
