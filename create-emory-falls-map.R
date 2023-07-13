my_name = "Emory Falls Trail" 
file_name = "emory-falls.gpx"
lon_multiplier = 1.1
lat_multiplier = 1.4
zoom = 15
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35
my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Emory Falls", 36.14404, -84.47163,
    "Stairs to Debord Falls", 36.13954, -84.47867,
)
include_roads <- TRUE
loop_trail <- FALSE
turn_around_is_end <- FALSE

is_state_park <- TRUE

### need to make it end at Debord Falls

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
    include_roads = include_roads,
    is_state_park = is_state_park)

t

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)
