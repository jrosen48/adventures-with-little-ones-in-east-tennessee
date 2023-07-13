my_name = "Abrams Creek" 
file_name = "abrams-creek.gpx"
lon_multiplier = .4
lat_multiplier = 1.1
zoom = 15
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35
my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Creek", 35.61608, -83.92791,
    "Backcountry Site #1", 35.61917, -83.92329, 
    "Backcountry Site #17", 35.61157, -83.90491,
    "Campground", 35.61091, -83.93321
)
loop_trail <- FALSE
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
    turn_around_is_end = turn_around_is_end)

t

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)