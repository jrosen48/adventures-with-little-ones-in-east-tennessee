my_name = "Seven Islands Island Loop" 
file_name = "seven-islands-loop.gpx"
lon_multiplier = .5
lat_multiplier = 1.4
zoom = 15
fig_height = 9.47
fig_width = 8.92
min_distance_space = 35
my_markers <- tribble(
    ~label, ~Y,  ~X,
    "Bridge", 35.94866,-83.69653,
    "Bathrooms", 35.95244, -83.68828
)
loop_trail <- TRUE
turn_around_is_end <- FALSE

t <- create_and_save_trailmap_sp(
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

t + geom_text(data = data.frame(x = -83.6903293217033, y = 35.9507100559683,
                                 label = "Bobwhite ADA Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 2.25,
               fill = "white") +
    geom_text(data = data.frame(x = -83.7017017717154, y = 35.9500023954001, label = "Island Loop Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 2.25,
               fill = "white") +
    geom_text(data = data.frame(x = -83.6885336717014, y = 35.9559015701754, label = "Upland Trail Inner Loop"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 2.25,
               fill = "white") +
    geom_text(data = data.frame(x = -83.6917829431334, y = 35.9432342755102, label = "Seclusion Bend Trail"),
               mapping = aes(x = x, y = y, label = label),
               inherit.aes = FALSE, family = "special", color = "black", alpha = .8, size = 2.25,
               fill = "white",
               arrow = arrow(length = unit(0.015, "npc"))) +
    # geom_curve(data = data.frame(x = -83.6928945359917, y = 35.9502331752106, xend = -83.6920394645622, yend = 35.9499486338846),
    #            mapping = aes(x = x, y = y, xend = xend, yend = yend),
    #            angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
    #                                      "last", "closed"),
    #            inherit.aes = FALSE, color = "black") +
    # geom_curve(data = data.frame(x = -83.6927235217058, y = 35.9490487714221, xend = -83.6935785931353, yend = 35.9493948723692),
    #            mapping = aes(x = x, y = y, xend = xend, yend = yend),
    #            angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
    #                                      "last", "closed"),
    #            inherit.aes = FALSE, color = "black") +
    # geom_curve(data = data.frame(x = -83.7000771359994, y = 35.9483565695278, xend = -83.700675686, yend = 35.9489103310432),
    #            mapping = aes(x = x, y = y, xend = xend, yend = yend),
    #            angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
    #                                      "last", "closed"),
    #            inherit.aes = FALSE, color = "black") + 
    # geom_curve(data = data.frame(x = -83.7008467002859, y = 35.9509869367261, xend = -83.6998206145705, yend = 35.9507100559683),
    #            mapping = aes(x = x, y = y, xend = xend, yend = yend),
    #            angle = 0L, arrow = arrow(30L, unit(0.1, "inches"),
    #                                      "last", "closed"),
    #            inherit.aes = FALSE, color = "black") +
    theme(text = element_text(size = 12))

ggsave(here::here("output", str_c(str_sub(file_name, end = -5L), "-map.png")), dpi = "retina", width = fig_width, height = fig_height, units = "in")

# ggannotate::ggannotate(t)