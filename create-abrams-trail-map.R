my_name = "Abrams Creek" 
file_name = "abrams-creek-revised.gpx"
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
    "Backcountry Site #17", 35.61157, -83.90491
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

t + geom_label(data = data.frame(x = -83.9298996151657, y = 35.6120295619088,
                               label = "Cooper Road Trail"),
             mapping = aes(x = x, y = y, label = label),
             inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
             fill = "lightgray") +
    geom_label(data = data.frame(x = -83.9242907451768, y = 35.6060706744292, label = "Rabbit Creek Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +
    geom_label(data = data.frame(x = -83.8983192384895, y = 35.6163832009815, label = "Hannah Mountain Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +
    geom_label(data = data.frame(x = -83.9178283514941, y = 35.6253044748786, label = "Cooper Road Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +
    geom_label(data = data.frame(x = -83.9178283514941, y = 35.6253044748786, label = "Hannah Mountian Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "#595959", alpha = .8, size = 2.25,
               fill = "lightgray") +    
    geom_text(data = data.frame(x = -83.9392883757991, y = 35.6216368400543, label = "Happy Valley Road"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 3) +
    geom_text(data = data.frame(x = -83.9428244025312, y = 35.6088549292086, label = "Abrams Creek Road"),
              mapping = aes(x = x, y = y, label = label),
              inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 3) +
    geom_curve(data = data.frame(x = -83.9274609760401, y = 35.6179692052299, xend = -83.9259977925647, yend = 35.6188613326196),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
                                         "last", "closed"),
               inherit.aes = FALSE, color = "#595959") +
    geom_curve(data = data.frame(x = -83.9244126771331, y = 35.6174735789023, xend = -83.9256319966959, yend = 35.6167797020436),
               mapping = aes(x = x, y = y, xend = xend, yend = yend),
               angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
                                         "last", "closed"),
               inherit.aes = FALSE,  color = "#595959")

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)