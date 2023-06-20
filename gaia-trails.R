# # combine this and the next function into one
# find_cumulative_distance_for_one_point <- function(markers, t, index) {
#     
#     one_point <- st_point(as.matrix(markers[index, c(3, 2)]))
#     
#     all_points <- st_multipoint(as.matrix(t[, c(1:2)]))
#     
#     nearest_point <- st_nearest_points(all_points, one_point) %>%
#         st_cast("POINT") %>%
#         st_coordinates() %>%
#         as_tibble() %>%
#         slice(1)
#     
#     left_join(nearest_point, t) %>%
#         pull(cumulative_distance_mi)
#     
# }
# 
# find_elev_for_one_point <- function(markers, t, index) {
#     
#     one_point <- st_point(as.matrix(markers[index, c(3, 2)]))
#     
#     all_points <- st_multipoint(as.matrix(t[, c(1:2)]))
#     
#     nearest_point <- st_nearest_points(all_points, one_point) %>%
#         st_cast("POINT") %>%
#         st_coordinates() %>%
#         as_tibble() %>%
#         slice(1)
#     
#     left_join(nearest_point, t) %>%
#         pull(elev)
#     
# }

create_and_save_trailmap <- function(my_name,
                                     file_name, 
                                     lon_multiplier = .5,
                                     lat_multiplier = 1,
                                     zoom = 15,
                                     lon_denom = 50,
                                     lat_denom = 15,
                                     fig_height = 9.47,
                                     fig_width = 8.92,
                                     min_distance_space = 35,
                                     markers = NULL,
                                     loop_trail = TRUE,
                                     turn_around_is_end = TRUE,
                                     include_roads = FALSE) {
    
    font_add_google("Special Elite", family = "special")
    
    showtext_auto()
    
    f <- here::here("data", "raw", "gaia", 
                    file_name)
    
    shp <- st_read(f, layer = "routes")
    
    #shp = elevation_add(shp) # possibly cut
    
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
    bb[1, 1] <- min(t$X) + lon_multiplier*(min(t$X) - max(t$X))  # x min - how far west
    bb[1, 2] <- max(t$X) - lon_multiplier*(min(t$X) - max(t$X)) # x max - how far east
    bb[2, 1] <- min(t$Y) + lat_multiplier*(min(t$Y) - max(t$Y)) # y min - how far south
    bb[2, 2] <- max(t$Y) - lat_multiplier*(min(t$Y) - max(t$Y))# ymax - how far north
    
    m <- get_stamenmap(bb, maptype = "terrain", zoom = zoom)
    
    mile_markers <- mile_markers %>% 
        mutate(mile_marker = ifelse(mile_marker == 0, NA, str_c("Mile: ", mile_marker)))
    
    t <- t %>%
        bind_cols(mile_markers) # removed for now
    
    # print(count(t, L1))
    
    total_long <- bb[1, 2] - bb[1, 1]
    total_lat <- bb[2, 1] - bb[2, 2]
    
    # v <- c(bb[1, 2] + total_long/lon_denom, bb[2, 1] - total_lat/lat_denom)
    # 
    # names(v) <- c("x", "y")
    
    # t$elev <- st_coordinates(shp) %>% as_tibble() %>% pull(Z)
    
    my_markers <- tribble(
        ~label, ~Y,  ~X,
        "Start", pull(t[1, "Y"]), pull(t[1, "X"]), 
    )
    
    my_markers_loop <- tribble(
        ~label, ~Y,  ~X,
        "Start/end (and parking)", pull(t[1, "Y"]), pull(t[1, "X"]), 
    )
    
    my_markers_turn_around <- tribble(
        ~label, ~Y,  ~X,
        "Turn-around spot", pull(t[nrow(t) - 1, "Y"]), pull(t[nrow(t) - 1, "X"]),
    )
    
    if (loop_trail == TRUE) {
        markers <- bind_rows(my_markers_loop, markers) # could add end, but probably better this way
    } else {
        markers <- bind_rows(my_markers, markers)
    }
    
    if (turn_around_is_end == TRUE) {
        markers <- bind_rows(markers, my_markers_turn_around)
    }
    
    # markers <- markers %>% 
    #     mutate(cumulative_distance_mi = seq(nrow(markers)) %>% 
    #                map_dbl(find_cumulative_distance_for_one_point, markers = markers, t = t))
    # 
    # markers <- markers %>% 
    #     mutate(elev = seq(nrow(markers)) %>% 
    #                map_dbl(find_elev_for_one_point, markers = markers, t = t))
    
    my_ymin <- min(t$elev * 3.28084) - 200
    my_ymax <- max(t$elev * 3.28084) + 200
    
    highway = opq(as.vector(bb)) %>% 
        add_osm_feature("highway") %>% 
        osmdata_sf()
    
    highway_lines = st_transform(highway$osm_lines)
    
    trails = highway_lines %>% 
        filter(highway %in% c("path","bridleway"))
    
    # labeling other trails
    
    ## not sure why this is necessary for L1 later 
    
    trails_coords <- st_coordinates(trails) %>% as_tibble()
    
    trails <- trails %>% mutate(L1 = unique(trails_coords$L1))
    
    trails_coords <- trails_coords %>% left_join(select(trails, L1, name))
    
    trails_coords <- filter(trails_coords,
                            X > bb[1, 1] & X < bb[1, 2] &
                                Y > bb[2, 1] & Y < bb[2, 2])
    
    trails_coords <- group_by(trails_coords, L1) %>%
        filter(row_number()==ceiling(n()/2))
    
    trails_coords <- trails_coords %>% select(X, Y, L1, name) %>% ungroup()
    
    # labeling roads
    
    # not used
    
    roads = highway_lines %>% 
        filter(highway %in% c("unclassified", "secondary", "tertiary", "residential", "service"))
    
    road_coords <- st_coordinates(roads) %>% as_tibble()
    
    roads <- roads %>% mutate(L1 = unique(road_coords$L1))
    
    road_coords <- road_coords %>% left_join(select(trails, L1, name))
    
    road_coords <- filter(road_coords, 
                          X > bb[1, 1] & X < bb[1, 2] &
                              Y > bb[2, 1] & Y < bb[2, 2]) 
    
    road_coords <- group_by(road_coords, L1) %>% 
        filter(row_number()==ceiling(n()/2))
    
    road_coords <- road_coords %>% select(X, Y, L1, name) %>% ungroup()
    
    # footpaths
    
    footpaths = highway_lines %>% 
        filter(highway %in% c("footway"))
    
    # water
    
    water_lines = opq(as.vector(bb)) %>% 
        add_osm_feature("waterway") %>% 
        osmdata_sf()
    
    water_lines = st_transform(water_lines$osm_lines)
    
    # polygons
    
    parking = opq(as.vector(bb)) %>% 
        add_osm_feature("parking") %>% 
        osmdata_sf() 
    
    building = opq(as.vector(bb)) %>% 
        add_osm_feature("building") %>% 
        osmdata_sf() 
    
    tourism = opq(as.vector(bb)) %>% 
        add_osm_feature("tourism") %>% 
        osmdata_sf() 
    
    parking_poly = st_transform(parking$osm_polygons)
    building_poly = st_transform(building$osm_polygons)
    tourism_poly = st_transform(tourism$osm_polygons)
    # return(highway_lines)
    # trails_points = st_transform(highway_lines$osm_points)
    
    # sites_poly = tourism_poly %>% 
    #     filter(tourism %in% c("picnic_site", "camp_site"))
    
    p_path <- ggmap(m) +
        # additional details
        geom_sf(data = water_lines, size = 1.25, color = "lightblue", inherit.aes = FALSE) +
        geom_sf(data = trails, size = 1.25, color = "white", inherit.aes = FALSE, linetype = 5) +
        geom_sf(data = footpaths, size = 1.25, color = "white", inherit.aes = FALSE) +
        {if(include_roads)geom_sf(data = roads, color = "gray20", inherit.aes = FALSE, alpha = 1)}+
        geom_sf(data = parking_poly, color = "darkgreen", inherit.aes = FALSE) +
        geom_sf(data = building_poly, color = "darkred", inherit.aes = FALSE) +
        geom_sf(data = tourism_poly, color = "grey30", inherit.aes = FALSE) +
        # trail
        # geom_sf(data = shp, size = 1.25, linetype = "dashed", color = "white", inherit.aes = FALSE) +
        geom_sf(data = shp, size = 3, color = "red", inherit.aes = FALSE) +
        # trail details
        geom_point(data = markers,
                   size = 2.5,
                   aes(x = X,
                       y = Y),
                   color = "black") +
        # geom_text_repel(data = road_coords, aes(x = X, y = Y, label = name),
        #                 min.segment.length = 0,
        #                 alpha = .60,
        #                 family = "special", color = "black", size = 12,
        #                 arrow = arrow(length = unit(0.015, "npc")),
        #                 segment.linetype = .65,
        #                 segment.alpha = .65,
        #                 box.padding = 1) +
        geom_text_repel(data = trails_coords, aes(x = X, y = Y, label = name),
                        min.segment.length = 0,
                        alpha = .60,
                        family = "special", color = "black", size = 12,
                        arrow = arrow(length = unit(0.015, "npc")),
                        segment.linetype = .65,
                        segment.alpha = .65,
                        box.padding = 1) +
        geom_label_repel(data = markers,
                         aes(x = X,
                             y = Y,
                             label = label),
                         alpha = .9,
                         box.padding = .33,
                         size = 16,
                         min.segment.length = 0,
                         segment.color = NA,
                         # arrow = arrow(length = unit(0.025, "npc")),
                         family = "special") +
        labs(title = my_name) +
        theme_minimal() +
        theme(text = element_text(family = "special")) +
        annotation_scale(location = "tr", unit_category = "imperial", style = "ticks", text_cex = 2)  +
        annotation_north_arrow(location = "br", which_north = "true",
                               height = unit(.75, "cm"),
                               width = unit(.75, "cm"),
                               style = north_arrow_orienteering(text_size = 20)) +
        xlab(NULL) + 
        ylab(NULL) +
        theme(text = element_text(size = 32))
    
    p_path
    
    return(p_path)
    
}
