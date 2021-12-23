name = "Abrams Falls" 
file_name_1 = "abrams-little-bottoms.gpx"
# auxiliary_trail_files = "GRSM_TRAILS.shp"
long_multiplier = .5
lat_multiplier = 1
zoom = 15
lon_denom = 50
lat_denom = 15
grid_plot_height = .65
grid_slope_height = .35
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35
my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Creek", 35.61608, -83.92791,
    "Backcountry Site", 35.619, -83.925,
)

create_and_save_trailmap(name = "Abrams Creek", 
                         file_name_1 = "abrams-little-bottoms.gpx",
                         markers = my_markers)