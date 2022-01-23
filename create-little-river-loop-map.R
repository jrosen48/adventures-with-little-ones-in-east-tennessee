my_name = "Little River Loop Trail" 
file_name = "little-river-loop.gpx"
lon_multiplier = 1
lat_multiplier = 1
zoom = 14
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35
my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Lynn Camp Falls", 35.61596,-83.66135,
    "Indian Flats Falls", 35.59244, -83.63291
)
loop_trail <- TRUE
turn_around_is_end <- TRUE

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

t +
    geom_label(data = data.frame(x = -83.6566864708576, y = 35.6174180056092,
                                label = "Middle Prong Trail"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
              fill = "lightgray") +
    geom_label(data = data.frame(x = -83.6234821260246, y = 35.6124743997979, label = "Panther Creek Trail"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
              fill = "lightgray") +
    geom_label(data = data.frame(x = -83.619088635711, y = 35.6000673539071, label = "Lynn Camp Prong Trail"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
              fill = "lightgray") +
    geom_label(data = data.frame(x = -83.6445198822968, y = 35.5844446477579, label = "Greenbrier Ridge Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +
    geom_text(data = data.frame(x = -83.6845028248249, y = 35.6313272224582, label = "Tremont Road"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 3) +
    geom_curve(data = data.frame(x = -83.6420415031455, y = 35.6142870404668, xend = -83.6393378167987, yend = 35.6132374845026),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
                                         "last", "closed"),
               inherit.aes = FALSE, color = "#595959") +
    geom_curve(data = data.frame(x = -83.64181619595, y = 35.6104737780747, xend = -83.6445198822968, yend = 35.6115728900031),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
                                         "last", "closed"),
               inherit.aes = FALSE, color = "#595959")

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)
