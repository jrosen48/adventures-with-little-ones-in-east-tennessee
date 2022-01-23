my_name = "Knox/Blount Greenway" 
file_name = "knoxblount-greenway.gpx"
lon_multiplier = 2.5
lat_multiplier = 1
zoom = 15
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35
my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Emory Falls", 36.14404, -84.47163,
    "Stairs to Debord Falls", 36.13954, -84.47867
)
include_roads <- TRUE
loop_trail <- FALSE
turn_around_is_end <- FALSE
include_water = FALSE

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
    include_roads = include_roads)

t +
    geom_label(data = data.frame(x = -84.47944979126, y = 36.1378335792984,
                                label = "Emory Gap Trail"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
              fill = "lightgray") +
    geom_label(data = data.frame(x = -84.4912012780368, y = 36.1426199407821, label = "W. Lookout Tower Trail"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
              fill = "lightgray") +
    geom_label(data = data.frame(x = -84.4702529755216, y = 36.1370083445598, label = "Panther Gap Trail"),               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +
    geom_label(data = data.frame(x = -84.4860919359599, y = 36.1331297412886, label = "North Old Mac Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +
    geom_text(data = data.frame(x = -84.4947778174906, y = 36.1343675933964, label = "Flat Fork Road"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 3) +
    geom_curve(data = data.frame(x = -84.4854788149107, y = 36.1379161027723, xend = -84.483839451763, yend = 36.1379986262461),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
                                         "last", "closed"),
               inherit.aes = FALSE, color = "#595959") +
    geom_curve(data = data.frame(x = -84.4839460122876, y = 36.1366782506644, xend = -84.4856831885938, yend = 36.1366306802428),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
                                         "last", "closed"),
               inherit.aes = FALSE, color = "#595959")

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)
