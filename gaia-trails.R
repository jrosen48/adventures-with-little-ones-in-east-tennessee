library(ceramic)
roi <- raster::extent(100, 160, -50, 10)
im <- cc_location(roi)
raster::plotRGB(im)

tmap::
e <- cc_elevation(roi)

library(tmap)

tm_shape(poly) +
    tm_borders() +
    tmap::tm_raster(e)

poly <- bb %>% 
    t() %>% 
    nominatimlite::bbox_to_poly()

library(maptiles)

ei_tiles = get_tiles(bb, provider = "Stamen.Toner", zoom = 12, crop = TRUE)

tmap_mode("plot")
#> tmap mode set to plotting
tm_shape(ei_tiles) + 
    tm_rgb() + 
    tm_shape(ei_borders) +
    tm_borders(lwd = 5, col = "lightblue") +
    tm_credits(get_credit("Stamen.Toner"),
               bg.color = "white")

# combine this and the next function into one
find_cumulative_distance_for_one_point <- function(markers, t, index) {
    
    one_point <- st_point(as.matrix(markers[index, c(3, 2)]))
    
    all_points <- st_multipoint(as.matrix(t[, c(1:2)]))
    
    nearest_point <- st_nearest_points(all_points, one_point) %>%
        st_cast("POINT") %>%
        st_coordinates() %>%
        as_tibble() %>%
        slice(1)
    
    left_join(nearest_point, t) %>%
        pull(cumulative_distance_mi)
    
}

find_elev_for_one_point <- function(markers, t, index) {
    
    one_point <- st_point(as.matrix(markers[index, c(3, 2)]))
    
    all_points <- st_multipoint(as.matrix(t[, c(1:2)]))
    
    nearest_point <- st_nearest_points(all_points, one_point) %>%
        st_cast("POINT") %>%
        st_coordinates() %>%
        as_tibble() %>%
        slice(1)
    
    left_join(nearest_point, t) %>%
        pull(elev)
    
}

create_and_save_trailmap <- function(name,
                                     file_name_1, 
                                     auxiliary_trail_files,
                                     long_multiplier = .5,
                                     lat_multiplier = 1,
                                     zoom = 15,
                                     lon_denom = 50,
                                     lat_denom = 15,
                                     grid_plot_height = .65,
                                     grid_slope_height = .35,
                                     fig_height = 9.47,
                                     fig_width = 8.92,
                                     min_distance_space = 35) {
    
    font_add_google("Special Elite", family = "special")
    
    showtext_auto()
    
    f <- here::here("data", "raw", "gaia", 
                    file_name_1)
    
    shp <- st_read(f, layer = "routes")
    
    f_aux <- here::here("data", "raw", "GRSM_TRAILS", 
                        auxiliary_trail_files)
    
    shp_aux <- st_read(f_aux)
    
    t_aux <-
        st_coordinates(shp_aux) %>% 
        as_data_frame()
    
    shp = elevation_add(shp) # possibly cut

    t <-
        st_coordinates(shp) %>% 
        as_tibble()
    
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
        bind_cols(mile_markers) # removed for now
    
    total_long <- bb[1, 2] - bb[1, 1]
    total_lat <- bb[2, 1] - bb[2, 2]
    
    v <- c(bb[1, 2] + total_long/lon_denom, bb[2, 1] - total_lat/lat_denom)
    
    names(v) <- c("x", "y")
    
    t$elev <- st_coordinates(shp) %>% as_tibble() %>% pull(Z)
    
    # will have to abstract this out
    markers <- tribble(
        ~label, ~Y,  ~X,
        "Start", pull(t[1, "Y"]), pull(t[1, "X"]), 
        "Little Turn-Around", 35.61608, -83.92791,
        "Medium Turn-Around", 35.61816, -83.92236,
        "Big Turn-Around", pull(t[nrow(t) - 1, "Y"]), pull(t[nrow(t) - 1, "X"]),
    )
    
    markers <- markers %>% 
        mutate(cumulative_distance_mi = seq(nrow(markers)) %>% 
                   map_dbl(find_cumulative_distance_for_one_point, markers = markers, t = t))
    
    markers <- markers %>% 
        mutate(elev = seq(nrow(markers)) %>% 
                   map_dbl(find_elev_for_one_point, markers = markers, t = t))
    
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
        xlim(0, 5) + 
        geom_point(data = markers,
                   aes(x = cumulative_distance_mi,
                       y = elev * 3.28084),
                   color = "black",
                   size = 2) +
        geom_label(data = markers,
                   aes(x = cumulative_distance_mi,
                       y = elev * 3.28084 - 25,
                       label = label)) +
    ylab("Elev. (ft.)") +
        xlab("Distance (mi.)") +
        theme(text = element_text(family = "special"))
    
    p_path <- ggmap(m) +
        geom_point(data = t_aux, 
                   aes(x = X,
                       y = Y),
                   color = "gray90",
                   alpha = .50,
                   size = .05) +
        geom_sf(data = shp, inherit.aes = FALSE) +
        geom_point(data = markers,
                   aes(x = X,
                       y = Y),
                   color = "black",
                   size = 2) +
        geom_label(data = markers,
                   aes(x = X,
                       y = Y - .00125,
                       label = label),
                   # alpha = .825,
                   # box.padding = .75,
                   family = "special") +
        labs(subtitle = name) +
        theme_minimal() +
        theme(text = element_text(family = "special")) +
        annotation_scale(location = "tl", unit_category = "imperial", style = "ticks")  +
        annotation_north_arrow(location = "br", which_north = "true",
                               height = unit(1.0, "cm"),
                               width = unit(1.0, "cm")) +
        xlab(NULL) + 
        ylab(NULL)
    
    return(p_path)
    
    p <- plot_grid(p_path, p_slope, ncol = 1,
                   rel_widths = c(1, .75),
                   rel_heights = c(grid_plot_height, grid_slope_height),
                   align = "v")
    
    p
    
    ggsave(here::here("output", str_c(name, " Trailmap.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")
    
    return(p)
    
}
