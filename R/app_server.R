#' @import shiny ggplot2
app_server <- function(data_path, meta_path, annotation_path) {
  # List the first level callModules here

  # Data loadinng
  function(input, output, session) {

  meta_info <- readr::read_csv(meta_path)

  meta_list <- split(meta_info, factor(meta_info$plate)) %>%
    lapply(function(x) x$well)

  annotation_file <- readr::read_csv(annotation_path)

  # Custom colors
  colors <- c("#FFAD3D", "#008600", "#86FF86", "#860086", "#FF86FF", "#1E33A8")

  #### Global ####
  output$well_ui <- renderUI({
    selectInput('well_global', 'Select Well / Sample', meta_list[[input$plate_global]])
  })

  data_subset <- reactive({
    paste0(data_path, isolate(input$plate_global), "-", input$well_global, ".csv") %>%
      readr::read_csv(col_types = readr::cols(
        CHR      = readr::col_character(),
        location = readr::col_double(),
        plate    = readr::col_double(),
        well     = readr::col_character(),
        sample   = readr::col_character(),
        value    = readr::col_double()
      ))
  })

  #### Dashboard ####
  output$dash_platecount = renderDT(
    meta_info %>% dplyr::count(plate), options = list(lengthChange = FALSE)
  )

  #### Table tab ####
  output$table_table = renderDT(
    data_subset()[1:1000, ], options = list(lengthChange = FALSE)
  )

  #### Distribution Plot ####
  dist_plot_fun <- reactive({
    data <- data_subset()

    n_missing <- sum(is.na(data))

    if (input$dist_select == "Count") {
      g_density <- aes()
    }
    if (input$dist_select == "Density") {
      g_density <- aes(y = stat(density))
    }

    if (input$dist_stratify == "Combine") {
      p <- ggplot(tidyr::drop_na(data), aes(value)) +
        geom_histogram(bins = input$dist_nbreaks)
    }
    if (input$dist_stratify == "Stratify") {
      data <- data %>%
        annotation_join(annotation_file = annotation_file) %>%
        dplyr::filter(Relation_to_UCSC_CpG_Island %in% input$dist_checkGroup)
      p <- ggplot(tidyr::drop_na(data), aes(value)) +
        geom_freqpoly(aes(color = Relation_to_UCSC_CpG_Island),
                      bins = input$dist_nbreaks,
                      size = 1, alpha = 0.7) +
        scale_color_manual(values = colors)
    }

    p <- p + g_density +
      theme_minimal() +
      labs(
        title = glue("Distribution of methylation for plate {input$plate_global} sample {input$well_global}"),
        subtitle = glue("{round(n_missing/nrow(data), 3)}% missing values were removed."),
        y = input$dist_select
      )
  })

  output$dist_plot <- renderPlot({
    print(dist_plot_fun())
  })

  output$dist_download <- downlaod_plot("distribution", dist_plot_fun(), input)

  #### Distribution Plot ####
  dist_multi_data <- reactive({
    fs::dir_ls(data_path, regexp = input$plate_global) %>%
      purrr::map_df(readr::read_csv, col_types = readr::cols(
        CHR      = readr::col_character(),
        location = readr::col_double(),
        plate    = readr::col_double(),
        well     = readr::col_character(),
        sample   = readr::col_character(),
        value    = readr::col_double()
      ))
  })

  dist_multi_plot_fun <- reactive({
    data <- dist_multi_data()
    n_missing <- sum(is.na(data))

    p <- ggplot(tidyr::drop_na(data), aes(value)) +
      geom_histogram(bins = input$dist_multi_nbreaks) +
      facet_wrap(~ well) +
      theme_minimal() +
      labs(
        title = glue("Distributions of methylation for plate {input$plate_global}")
      )
  })

  output$dist_multi_plot <- renderPlot({
    print(dist_multi_plot_fun())
  })

  output$dist_multi_download <- downlaod_plot("distribution-multi", dist_multi_plot_fun(), input)


  #### Acf plot tab ####
  acf_plot_fun <- reactive({
    p <- data_subset() %>%
      dplyr::pull(value) %>%
      fansy_acf(input$acf_nlag) +
      labs(
        title = glue("ACF plot of methylation for plate {input$plate_global} sample {input$well_global}")
      )
  })

  output$acf_plot <- renderPlot({
    print(acf_plot_fun())
  })

  output$acf_download <- downlaod_plot("acf", acf_plot_fun(), input)

  #### Combined Acf plot tab ####
  #combacf_plot_fun <- reactive({
  #  p <- data_subset() %>%
  #    split(factor(paste(data_tidy$well, data_tidy$sample))) %>%
  #    map_dfc(~ fansy_acf(.x$value, n = input$combacf_nlag, data = TRUE)) %>%
  #    dplyr::mutate(n = row_number()) %>%
  #    pivot_longer(-n) %>%
  #    ggplot(aes(n, value, group = n)) +
  #    geom_boxplot() +
  #    theme_minimal() +
  #    labs(
  #      title = glue("Combined ACF plot of methylation")
  #    )
  #})
  #
  #output$combacf_plot <- renderPlot({
  #  print(combacf_plot_fun())
  #})

  #### correlation plot ####
  cor_plot_fun <- reactive({
    p <- data_subset() %>%
      corn_plotter(glue::glue("for plate {input$plate_global} sample {input$well_global}"),
                   annotation_file)
  })

  output$cor_plot <- renderPlot({
    print(cor_plot_fun())
  })

  output$cor_download <- downlaod_plot("cor", cor_plot_fun(), input)

  #### Time-series plot ####
  ts_data <- reactive({
    data_subset() %>%
      dplyr::slice(seq(dplyr::n() / 100 * input$ts_range[1],
                       dplyr::n() / 100 * input$ts_range[2]))
  })

  ts_plot_fun <- reactive({
    p <- ts_data() %>%
      ggplot(aes(location, value)) +
      geom_line(alpha = 0.3) +
      theme_minimal() +
      labs(
        title = glue("Time-series plot of methylation for plate {input$plate_global} sample {input$well_global}")
      )
  })

  output$ts_plot <- renderPlot({
    print(ts_plot_fun())
  })

  output$ts_download <- downlaod_plot("ts", ts_plot_fun(), input)

  #### Rolling plot ####
  roll_data <- reactive({

    roll_var <- tibbletime::rollify(function(x) get(input$roll_fun)(x, na.rm = TRUE), input$roll_n)

    data_subset() %>%
      dplyr::slice(seq(dplyr::n() / 100 * input$roll_range[1],
                       dplyr::n() / 100 * input$roll_range[2])) %>%
      dplyr::mutate(rvar = roll_var(value),
             missing_ind = is.na(rvar),
             rvar = dplyr::coalesce(rvar, rvar[min(which(!is.na(rvar)))]))

  })

  roll_plot_fun <- reactive({
    p <- roll_data() %>%
      ggplot(aes(location, rvar, color = missing_ind)) +
      geom_line() +
      scale_color_manual(values = c("black", "red")) +
      guides(color = "none") +
      theme_minimal() +
      labs(title = glue("Rolling {input$roll_fun} of methylation for plate {input$plate_global} sample {input$well_global}"),
           y = "Rolling value")
  })

  output$roll_plot <- renderPlot({
    print(roll_plot_fun())
  })

  output$roll_download <- downlaod_plot("roll", roll_plot_fun(), input)

  #### Heatmap plot ####
  heat_data <- reactive({
    data_subset() %>%
      dplyr::slice(seq(dplyr::n() / 100 * input$heat_range[1],
                       dplyr::n() / 100 * input$heat_range[2])) %>%
      dplyr::mutate(location = dplyr::row_number()) %>%
      tidyr::drop_na()
  })

  heat_plot_fun <- reactive({
    p <- heat_data() %>%
      ggplot(aes(location, value)) +
      geom_bin2d(bins = input$heat_res) +
      labs(x = "site") +
      theme_minimal() +
      scale_fill_viridis_c(option = "C") +
      labs(title = glue::glue("heatmap of methylation for plate {input$plate_global} sample {input$well_global}"),
           subtitle = "where location isn't taken into account")
  })


  output$heat_plot <- renderPlot({
    print(heat_plot_fun())
  })

  output$heat_download <- downlaod_plot("heat", heat_plot_fun(), input)
}
}
