corn_plotter <- function(data, title, annotation_file) {
  data %>%
    annotation_join(annotation_file) %>%
    dplyr::mutate(island = Relation_to_UCSC_CpG_Island == "Island") %>%
    dplyr::group_by(CHR, island) %>%
    dplyr::mutate(diff = location - dplyr::lag(location, default = 0),
           diff_c = cut(diff, breaks = c(0, 1:20 * 50, Inf)),
           value_lag = dplyr::lag(value, default = 0)) %>%
    dplyr::group_by(diff_c, island) %>%
    dplyr::summarise(cor = stats::cor(value, value_lag)) %>%
    ggplot(aes(diff_c, cor, color = island)) +
    geom_point() +
    theme_minimal() +
    scale_color_discrete(labels = c("No","Yes")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylim(0, 1) +
    labs(x = "",
         y = "Correlation",
         title = "Differences in neighbor correlation by distance",
         subtitle = title)
}
