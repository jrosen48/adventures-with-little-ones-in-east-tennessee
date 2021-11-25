name <- "Abrams"
long_multiplier <- .5
lat_multiplier <- 1
zoom = 15
lon_denom <- 50
lat_denom <- 15
grid_plot_height = .60
grid_slope_height = .40
fig_height = 9.47
fig_width = 8.92

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

font_add_google("Special Elite", family = "special")

showtext_auto()

f <- 
    here::here("data", "raw", "gaia", 
               "abrams-little-bottoms.gpx")

shp <- st_read(f, layer = "routes")

shp = elevation_add(shp)

t <-
    st_coordinates(shp) %>% 
    as_tibble()

# ll_prj <- "EPSG:4326"
# elev <- get_elev_point(locations = shp, prj = ll_prj)

dist_matrix <- distm(t[, c(1, 2)]) %>% 
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

bb <- getbb("")
bb[1, 1] <- min(t$X) + long_multiplier*(min(t$X) - max(t$X))  # x min - how far west
bb[1, 2] <- max(t$X) - long_multiplier*(min(t$X) - max(t$X)) # x max - how far east
bb[2, 1] <- min(t$Y) + lat_multiplier*(min(t$Y) - max(t$Y)) # y min - how far south
bb[2, 2] <- max(t$Y) - lat_multiplier*(min(t$Y) - max(t$Y))# ymax - how far north

m <- get_stamenmap(bb, maptype = "terrain", zoom = zoom)

mile_markers <- mile_markers %>% 
    mutate(mile_marker = ifelse(mile_marker == 0, NA, str_c("Mile: ", mile_marker)))

t <- t %>%
    bind_cols(mile_markers)

t <- t %>%
    mutate(place_marker = ifelse(row == nrow(.) - 1, "End", NA))

t <- t %>%
    mutate(label = ifelse(!is.na(mile_marker), mile_marker,
                          ifelse(!is.na(place_marker), place_marker,
                                 ifelse(key == 1, "Start", NA))))

total_long <- bb[1, 2] - bb[1, 1]
total_lat <- bb[2, 1] - bb[2, 2]

v <- c(bb[1, 2] + total_long/lon_denom, bb[2, 1] - total_lat/lat_denom)

names(v) <- c("x", "y")

t$elev <- st_coordinates(shp) %>% as_tibble() %>% pull(Z)

my_ymin <- min(t$elev * 3.28084) - 200
my_ymax <- max(t$elev * 3.28084) + 200

p_slope <- ggplot(t, aes(x = cumulative_distance_mi, y = elev * 3.28084)) +
    geom_line() +
    theme_minimal() +
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    ylab("Elev. (ft.)") +
    xlab("Distance (mi.)") +
    theme(text = element_text(family = "special"))

p_path <- ggmap(m) +
    # geom_point(data = t_other, 
    #            aes(x = X,
    #                y = Y),
    #            color = "gray90", 
    #            alpha = .50,
    #            size = .05) +
    geom_sf(data = shp, inherit.aes = FALSE) +
    ggsn::north(shp, scale = .5, location = "bottomright", anchor = v) +
    geom_point(data = filter(t, !is.na(label)),
               aes(x = X,
                   y = Y),
               color = "black") +
    ggrepel::geom_label_repel(data = filter(t, !is.na(label)),
                              aes(x = X,
                                  y = Y,
                                  label = label),
                              alpha = .825,
                              box.padding = .75,
                              family = "special") +
    theme_minimal() +
    theme(text = element_text(family = "special")) +
    xlab(NULL) + 
    ylab(NULL)

plot_grid(p_path, p_slope, ncol = 1,
          rel_widths = c(1, 1),
          rel_heights = c(grid_plot_height, grid_slope_height),
          align = "v")

ggsave(here::here("output", str_c(tolower(name), ".png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")
