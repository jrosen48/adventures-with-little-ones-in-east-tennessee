my_name = "Whiteak Sink" 
file_name = "whiteoak-sinks.gpx"
lon_multiplier = .7
lat_multiplier = .7
zoom = 14
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35

loop_trail <- TRUE
turn_around_is_end <- FALSE
keep_roads <- c(21)
keep_trails <- (c(1:3, 5:7))
is_state_park <- FALSE

my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Turn-off", 35.63639, -83.73624,
    "Rainbow Cave Falls", 35.63608, -83.74493,
    "Bat Cave", 35.63647, -83.74862
)
    
t <- create_and_save_trailmap(
    my_name = my_name,
    file_name = file_name, 
    lon_multiplier = lon_multiplier,
    lat_multiplier = lat_multiplier,
    zoom = zoom,
    lon_denom = lon_denom,
    lat_denom = lat_denom,
    fig_height = fig_height,
    fig_width = fig_width,
    min_distance_space = min_distance_space,
    markers = my_markers,
    loop_trail = loop_trail,
    turn_around_is_end = turn_around_is_end,
    include_roads = TRUE,
    is_track = TRUE,
    keep_roads = keep_roads,
    keep_trails = keep_trails)

t[[1]]

t[[2]] %>% 
    mutate(my_index = 1:nrow(.)) %>% 
    as_data_frame()

t[[3]] %>% 
    mutate(my_index = 1:nrow(.)) %>% 
    as_data_frame()

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), 
       dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)
