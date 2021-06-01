#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {

  # List reactive input values
  rvals <- reactiveValues(
    datasetInput = NULL,
    queryInput = NULL,
    mediaInput = NULL,
    fdrInput = NULL,
    posColInput = NULL,
    negColInput = NULL,
    labelsInput = NULL,
    typeInput = NULL,
    lineInput = NULL,
    resultsOutput = NULL
  )

  # Sidebar module
  mod_sidebar_server("sidebar", rvals, dataset)

  # Main panel module
  mod_main_server("main", rvals, dataset)
}
