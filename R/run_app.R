#' Run the Shiny Application
#'
#' @param data_path character, path to folder containing data.
#' @param meta_path character, path to the data meta file.
#' @param annotation_path character, path to the annotation file.
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(data_path = NULL,
                    meta_path = NULL,
                    annotation_path = NULL) {
  shiny::shinyApp(ui = app_ui(meta_path = meta_path),
                  server = app_server(data_path = data_path,
                                      meta_path = meta_path,
                                      annotation_path = annotation_path))
}
