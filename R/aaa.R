utils::globalVariables(c("CHR", "plate", "density", "value", "Relation_to_UCSC_CpG_Island",
                         "location", "rvar", "missing_ind", "name", "island", "diff_c",
                         "value_lag", "cor"))

fansy_acf <- function(x, n = 50, data = FALSE, points = NULL) {
  y <- as.numeric(stats::acf(x,
                             na.action = stats::na.pass,
                             plot = FALSE,
                             lag.max = n)$acf)[-1]
  if (data) return(y)

  out <- data.frame(x = seq_len(n),
                    y = y) %>%
    ggplot(aes(x, y)) +
    labs(x = "Lag", y = "ACF") +
    theme_minimal()

  if (n > 1000) {
    out +
      geom_point()
  } else {
    out +
      geom_segment(aes(xend = x, yend = 0))
  }
}

downlaod_plot <- function(prefix, plot, input) {
  downloadHandler(
    filename = gsub(" " , "",
                    glue("{prefix}_plate-{input$plate_global}_sample-{input$well_global}.png")),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 10, height = 7,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot, device = device)
    }
  )
}


# Rolling functions
left_right_bin <- function(x, na.rm) {
  mean(x < 0.125, na.rm = na.rm) + mean(x > 0.875, na.rm = na.rm)
}

bimodality_coefficient <- function(x, na.rm) {
  n <- length(x)
  (e1071::skewness(x, na.rm = na.rm) ^ 2 + 1) /
    ((e1071::kurtosis(x, na.rm = na.rm)) + (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3)))
}

# Join a prepped dataset with the annotation file
annotation_join <- function(x, annotation_file) {
  x %>%
    dplyr::mutate(CHR = as.character(CHR)) %>%
    dplyr::left_join(annotation_file, by = c("CHR" = "CHR", "location" = "MAPINFO"))
}
