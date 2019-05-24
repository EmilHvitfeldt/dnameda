#' @import shiny shinydashboard
#' @importFrom DT DTOutput renderDT
app_ui <- function(meta_path) {

  meta_list <- list()
  if (!missing(meta_path)) {
    meta_info <- readr::read_csv(meta_path)
    meta_list <- split(meta_info, factor(meta_info$plate)) %>%
      lapply(function(x) x$well)
  }

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(skin = "black",
                  dashboardHeader(title = "DNA Methylation EDA"),
                  ## Sidebar content
                  dashboardSidebar(
                    sidebarMenu(
                      selectInput(inputId = "plate_global", label = "Select Plate",
                                  choices = names(meta_list)),
                      uiOutput('well_ui'),
                      hr(),
                      menuItem("Dashboard",             tabName = "dashboard",    icon = icon("dashboard")),
                      menuItem("Table",                 tabName = "table",        icon = icon("table")),
                      menuItem("Individual", icon = icon("chart-line"),
                               menuSubItem("Distribution Plot",     tabName = "distplot",     icon = icon("chart-area")),
                               menuSubItem("ACF Plot",              tabName = "acfplot",      icon = icon("chart-bar")),
                               menuSubItem("Correlation Plot",      tabName = "corplot",      icon = icon("chart-line")),
                               menuSubItem("Time-series Plot",      tabName = "tsplot",       icon = icon("chart-line")),
                               menuSubItem("Heatmap Plot",          tabName = "heatplot",     icon = icon("fire")),
                               menuSubItem("Rolling Plot",          tabName = "rollplot",     icon = icon("chart-line"))
                      ),
                      menuItem("Multi", icon = icon("chart-line")#,
                               #menuSubItem("Distribution Plot",     tabName = "distmultiplot",     icon = icon("chart-area")),
                               #menuSubItem("Combined rolling Plot", tabName = "combrollplot", icon = icon("chart-line"))
                      )
                      #menuItem("Combined ACF Plot",     tabName = "combacfplot",  icon = icon("chart-bar")),
                    )
                  ),
                  ## Body content
                  dashboardBody(
                    tabItems(
                      # Dashboard
                      tabItem(
                        tabName = "dashboard",
                        h1("Analysis of 'matrix for geo.csv'"),
                        DTOutput("dash_platecount")
                      ),

                      # Table
                      tabItem(
                        tabName = "table",
                        fluidRow(DTOutput('table_table'))
                      ),

                      # Distribution Plot
                      tabItem(
                        tabName = "distplot",
                        plotOutput("dist_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("dist_nbreaks", "Number of breaks:",
                                             min = 10, max = 500, value = 200),
                                 radioButtons("dist_select", label = "",
                                              choices = list("Count" = "Count",
                                                             "Density" = "Density"),
                                              selected = "Count")
                          ),
                          column(4,
                                 checkboxGroupInput("dist_checkGroup", label = h3("Select island relations"),
                                                    choices = list("Island" = "Island",
                                                                   "N_Shelf" = "N_Shelf",
                                                                   "S_Shelf" = "S_Shelf",
                                                                   "N_Shore" = "N_Shore",
                                                                   "S_Shore" = "S_Shore",
                                                                   "Sea" = "Sea"),
                                                    selected = c("Sea", "Island", "N_Shelf", "N_Shore", "S_Shelf", "S_Shore")),
                                 radioButtons("dist_stratify", label = "",
                                              choices = list("Combine" = "Combine",
                                                             "Stratify" = "Stratify"),
                                              selected = "Combine")
                          ),
                          column(4,
                                 downloadButton(outputId = "dist_download", label = "Download the plot")
                          )
                        )
                      ),

                      # Distribution Plot
                      tabItem(
                        tabName = "distmultiplot",
                        plotOutput("dist_multi_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("dist_multi_nbreaks", "Number of breaks:",
                                             min = 10, max = 500, value = 200)
                          ),
                          column(4),
                          column(4,
                                 downloadButton(outputId = "dist_multi_download", label = "Download the plot")
                          )
                        )
                      ),

                      # ACT plot
                      tabItem(
                        tabName = "acfplot",
                        plotOutput("acf_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("acf_nlag", "Number of lags:",
                                             min = 1, max = 5000, value = 50)
                          ),
                          column(4),
                          column(4,
                                 downloadButton(outputId = "acf_download", label = "Download the plot")
                          )
                        )
                      ),

                      # combind ACT plot
                      tabItem(
                        tabName = "combacfplot",
                        plotOutput("combacf_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("combacf_nlag", "Number of lags:",
                                             min = 1, max = 200, value = 50)
                          ),
                          column(4),
                          column(4)

                        )
                      ),

                      # Correlation plot
                      tabItem(
                        tabName = "corplot",
                        plotOutput("cor_plot"),
                        hr(),
                        fluidRow(
                          column(4),
                          column(4),
                          column(4,
                                 downloadButton(outputId = "cor_download", label = "Download the plot")
                          )
                        )
                      ),

                      # Time-series plot
                      tabItem(
                        tabName = "tsplot",
                        plotOutput("ts_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("ts_range", "Select percentiles:",
                                             min = 0, max = 100 ,
                                             value = c(0, 10))
                          ),
                          column(4),
                          column(4,
                                 downloadButton(outputId = "ts_download", label = "Download the plot")
                          )
                        )
                      ),

                      # Rolling plot
                      tabItem(
                        tabName = "rollplot",
                        plotOutput("roll_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("roll_n", "Width of window:",
                                             min = 1, max = 1000, value = 50),
                                 sliderInput("roll_range", "Select percentiles",
                                             min = 0, max = 100 ,
                                             value = c(0, 1))
                          ),
                          column(4,
                                 selectInput(inputId = "roll_fun", label = "Select Function:",
                                             choices = c("mean",
                                                         "var",
                                                         "IQR",
                                                         "left_right_bin",
                                                         "bimodality_coefficient"), selected = "mean")
                          ),
                          column(4,
                                 downloadButton(outputId = "roll_download", label = "Download the plot")
                          )
                        )
                      ),

                      # Combined Rolling plot
                      tabItem(
                        tabName = "combrollplot",
                        plotOutput("combroll_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("combroll_range", "Number of observations:",
                                             min = 1, max = 1e+7,
                                             value = 5e+6)
                          ),
                          column(4),
                          column(4,
                                 downloadButton(outputId = "combroll_download", label = "Download the plot")
                          )
                        )
                      ),

                      # Heatmap plot
                      tabItem(
                        tabName = "heatplot",
                        plotOutput("heat_plot"),
                        hr(),
                        fluidRow(
                          column(4,
                                 sliderInput("heat_res", "Select resolution:",
                                             min = 10, max = 100,
                                             value = 40),
                                 sliderInput("heat_range", "Select percentile:",
                                             min = 0, max = 100 ,
                                             value = c(0, 100))
                          ),
                          column(4),
                          column(4,
                                 downloadButton(outputId = "heat_download", label = "Download the plot"))
                        )
                      )
                    )
                  )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'dnameda')
  )

  tags$head(
    golem::js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
