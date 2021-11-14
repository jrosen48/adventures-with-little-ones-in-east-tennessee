# Libraries

## credit and thanks to Isabella Velásquez for the template for this code: https://github.com/ivelasq

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
    filter(L1 == 203)

# Plot

bb <- getbb("")
bb[1, 1] <- min(t1$X) + 2*(min(t1$X) - max(t1$X))  # x min - how far west
bb[1, 2] <- max(t1$X) - 2*(min(t1$X) - max(t1$X)) # x max - how far east
bb[2, 1] <- min(t1$Y) + (min(t1$Y) - max(t1$Y)) # y min - how far south
bb[2, 2] <- max(t1$Y) - (min(t1$Y) - max(t1$Y))# ymax - how far north

m <- get_stamenmap(bb, maptype = "terrain", zoom = 14)

mt_wash <- data.frame(x = -71.3036, y = 44.2700)
ll_prj <- "EPSG:4326"

elev <- get_elev_point(locations = as.data.frame(t1[, ]), prj = ll_prj)

elev <- elev %>% 
    as_tibble() %>% 
    mutate(elevation = elevation * 3.28084)

t1 <- t1 %>% 
    bind_cols(elev)
    
ggmap(m) +
    geom_point(data = t1, 
               aes(x = X,
                   y = Y,
                   color = elevation), 
               size = .30) +
    geom_point(data = slice(t1, 1),
               aes(x = X, 
                   y = Y),
               color = "black") +
    ggrepel::geom_label_repel(data = slice(t1, 1),
               aes(x = X, 
                   y = Y), label = "Start") +
    geom_point(data = slice(t1, nrow(t1) - 1),
               aes(x = X, 
                   y = Y),
               color = "black") +
    ggrepel::geom_label_repel(data = slice(t1, nrow(t1) - 1),
                              aes(x = X, 
                                  y = Y), label = "End") +
    ggtitle("Alum Cave Trail") +
    scale_color_gradient("Elevation (ft.)", 
                         low = "gray",
                         high = "orchid4") + 
    theme_void()

# Save file

ggsave(here::here("output", "boulevard-map.png"), dpi = "retina", width = 8, height = , units = "in")
