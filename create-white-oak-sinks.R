my_name = "White Oak Sinks" 
file_name = "whiteoak-sinks.gpx"
lon_multiplier = .7
lat_multiplier = .7
zoom = 14
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35

my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Lynn Camp Falls (Turn-Around)", 35.61596,-83.66135,
    "Indian Flats Falls (Longer Option)", 35.59244, -83.63291
)

loop_trail <- TRUE
turn_around_is_end <- FALSE

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
    include_roads = TRUE)

t

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), 
       dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)
