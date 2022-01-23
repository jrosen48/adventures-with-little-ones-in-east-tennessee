
# p_slope <- ggplot(t, aes(x = cumulative_distance_mi, y = elev * 3.28084)) +
#     geom_line() +
#     theme_minimal() +
#     theme(axis.line = element_line(color='black'),
#           plot.background = element_blank(),
#           panel.grid.major = element_blank(),
#           # panel.grid.minor = element_blank(),
#           panel.border = element_blank()) +
#     xlim(0, 6) + 
#     ylim(1000, 2000) + 
#     geom_point(data = markers,
#                aes(x = cumulative_distance_mi,
#                    y = elev * 3.28084),
#                color = "black",
#                size = 3) +
#     ggrepel::geom_label_repel(data = markers,
#                               aes(x = cumulative_distance_mi,
#                                   y = elev * 3.28084,
#                                   label = label),
#                               size = 2.5,
#                               min.segment.length = 0,
#                               color = "#595959") +
#     ylab("Elev. (ft.)") +
#     xlab("Distance (mi.)") +
#     theme(text = element_text(family = "special")) +
#     theme(plot.margin=unit(c(1.25,1.25,1.25,1.25),"cm")) +
#     scale_y_continuous(label = scales::comma)