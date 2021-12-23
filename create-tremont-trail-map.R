name = "Tremont" 

file_name_1 = "tremont.gpx"

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
    
    "Creek", 35.61608, -83.92791
    
)

create_and_save_trailmap(name = name,
                         file_name_1 = "abrams-little-bottoms.gpx",
                         markers = my_markers)