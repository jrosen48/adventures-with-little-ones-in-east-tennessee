# Libraries

## credit and thanks to Isabella Velásquez for the template for this code: https://github.com/ivelasq

name <- "Abrams"
long_multiplier <- 1.25
lat_multiplier <- 1.25
zoom = 15

library(ggmap)
library(sf)
library(tidyverse)
library(ggmap)
library(osmdata)
library(elevatr)
library(geosphere)

# Open files

# f <- 
#     here::here("data", "raw", "GRSM_TRAILS", "GRSM_TRAILS.shp")


f <- 
    here::here("data", "raw", "gaia", 
               "abrams-trailhead-to-campground.gpx")

shp <- st_read(f, layer = "routes")

?st_layers(f)

# my_trail_id <- shp %>% 
#     filter(str_detect(TRAILNAME, name)) %>% 
#     pull(OBJECTID)
# 
# my_trail_id
# 
# my_trail_id <- shp[my_trail_id, ] %>% 
#     arrange(desc(Shape__Len)) %>% 
#     slice(1) %>% 
#     pull(OBJECTID) # gets the longest segment, for now

t1 <-
    st_coordinates(shp) %>% 
    as_tibble()

# Let's try it with the first trail

# t1 <- coords %>% 
#     filter(L1 %in% my_trail_id)

start = as.matrix(t1[1, c(1, 2)]) # first row

# random_point <- as.matrix(t1[1043, c(1, 2)])

all_points <- t1[, c(1, 2)]

dist_matrix <- distm(all_points) %>% 
    as_tibble()

dist_matrix_proc <- dist_matrix %>% 
    mutate(row = row_number()) %>% 
    gather(key, val, -row) %>% 
    mutate(key = str_sub(key, 2)) %>% 
    mutate(key = as.numeric(key)) %>% 
    filter(key == row + 1 | (row == 1 & key == 1))

dist_matrix_final <- dist_matrix_proc %>% 
    rename(distance_m = val) %>% 
    mutate(distance_ft = distance_m * 3.28084) %>% 
    mutate(cumulative_distance_ft = cumsum(distance_ft)) %>% 
    mutate(cumulative_distance_mi = cumulative_distance_ft/5280)

mile_markers <- dist_matrix_final %>% 
    mutate(cumulative_distance_mi_trunc = trunc(cumulative_distance_mi)) %>% 
    mutate(mile_marker = ifelse(lead(cumulative_distance_mi_trunc) > cumulative_distance_mi_trunc, 
                                lead(cumulative_distance_mi_trunc), 0))

# sf::st_distance(start, random_point)

# t_other <- t1 %>% 
#     filter(!(L1 %in% my_trail_id))

# Plot

bb <- getbb("")
bb[1, 1] <- min(t1$X) + long_multiplier*(min(t1$X) - max(t1$X))  # x min - how far west
bb[1, 2] <- max(t1$X) - long_multiplier*(min(t1$X) - max(t1$X)) # x max - how far east
bb[2, 1] <- min(t1$Y) + lat_multiplier*(min(t1$Y) - max(t1$Y)) # y min - how far south
bb[2, 2] <- max(t1$Y) - lat_multiplier*(min(t1$Y) - max(t1$Y))# ymax - how far north

m <- get_stamenmap(bb, maptype = "terrain", zoom = zoom)

ll_prj <- "EPSG:4326"

# elev <- get_elev_point(locations = as.data.frame(t1), prj = ll_prj)

elev_proc <- shp$ele %>% 
    as_tibble() %>% 
    mutate(elevation = value * 3.28084) %>% 
    select(elevation)

t1 <- t1 %>% 
    bind_cols(elev_proc)

mile_markers <- mile_markers %>% 
    mutate(mile_marker = ifelse(mile_marker == 0, NA, str_c("Mile: ", mile_marker)))

t1 <- t1 %>%
    bind_cols(mile_markers)

# for specific trail - place_marker

# t1 <- t1 %>% 
#     mutate(place_marker = ifelse(row == 1037, "Mt. LeConte Summit",
#                                  ifelse(row == 465, "Alum Cave", NA)))
# 
# t1 <- t1 %>% 
#     mutate(label = ifelse(!is.na(mile_marker), mile_marker,
#                           ifelse(!is.na(place_marker), place_marker,
#                                   ifelse(key == 2, "Start", NA))))

t1

p <- ggmap(m) +
    # geom_point(data = t_other, 
    #            aes(x = X,
    #                y = Y),
    #            color = "gray90", 
    #            alpha = .50,
    #            size = .05) +
    geom_sf(data = shp, inherit.aes = FALSE) +
    # ≥for waypoints
    # geom_point(data = slice(t1, c(1), aes(x = X, y = Y)) +
               # aes(x = X,
               #     y = Y,
               #     color = elevation), 
               #size = .40,
               #alpha = .9) +
    
    # place markers
    # geom_point(data = filter(t1, !is.na(label)),
    #            aes(x = X, 
    #                y = Y),
    #            color = "black",
    #            alpha = .65) +
    # ggrepel::geom_label_repel(data = t1,
    #                           aes(x = X, 
    #                               y = Y,
    #                               label = label),
    #                           alpha = .825,
    #                           box.padding = .75) +

    ggtitle(str_c(name, " Trail (", 
                  mile_markers %>% slice(nrow(.)) %>% pull(cumulative_distance_mi) %>% round(2),
                  " mi.)")) +
    labs(subtitle = "Great Smoky Mountains National Park") +
    scale_color_gradient("Elevation (ft.)", 
                         low = "#dadaeb",
                         high = "#756bb1") + 
    # theme(text = element_text(family = "Montserrat")) +
    theme_void()

p

# Save file

ggsave(here::here("output", str_c(tolower(name), ".png")), dpi = "retina", width = 8, height = , units = "in")
