#' sidebar UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom colourpicker colourInput
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id, dataset) {
  ns <- NS(id)
  tagList(
    h4("Input selection"),
    # Dataset select input
    selectInput(
      inputId = ns("datasetInput"),
      label = "Select dataset:",
      choices = names(dataset),
      multiple = FALSE,
      selected = NULL
    ),
    # Query select input
    selectInput(
      inputId = ns("queryInput"),
      label = "Select screens (choose at least two):",
      choices = colnames(dataset[[1]][["qGI"]]),
      multiple = TRUE,
      selected = NULL
    ),
    # Media select input
    selectInput(
      inputId = ns("mediaInput"),
      label = "Select media condition(s):",
      choices = colnames(dataset[[1]][["fc_singlePhenotype"]]),
      multiple = TRUE,
      selected = NULL
    ),
    # FDR slider input
    sliderInput(
      inputId = ns("fdrInput"),
      label = "Select FDR threshold:",
      min = 0,
      max = 1,
      value = 0.2
    ),
    rep_br(1),
    h4("Output selection"),
    # Plot darkest positive GI colour
    colourInput(
      inputId = ns("posColInput"),
      label = "Select darkest positive colour:",
      value = "#826f03"
    ),
    # Plot darkest negative GI colour
    colourInput(
      inputId = ns("negColInput"),
      label = "Select darkest negative colour:",
      value = "#014e7a"
    ),
    # Plot label text
    textAreaInput(
      inputId = ns("labelsInput"),
      label = "List plot labels (character sensitive):",
      value = NULL,
      height = "110px"
    ),
    # Plot label type
    radioButtons(
      inputId = ns("typeInput"),
      label = "Select label type:",
      choices = list("Text", "Padded box"),
      selected = "Text"
    ),
    # Plot reference line(s)
    checkboxGroupInput(
      inputId = ns("lineInput"),
      label = "Select reference line(s):",
      choices = list("y=x", "x=0", "y=0"),
      selected = NULL
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, rvals, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update reactive inputs based on selected dataset
    observe({
      x <- input$datasetInput
      # Query select input
      updateSelectInput(
        session,
        inputId = "queryInput",
        choices = colnames(dataset[[x]][["qGI"]])
      )
      # Media select input
      updateSelectInput(
        session,
        inputId = "mediaInput",
        choices = colnames(dataset[[x]][["fc_singlePhenotype"]])
      )
    })

  #  observe({print(head(rvals$queryInput))})
  #  observe({print(head(rvals$labelsInput))})
  #  observe({print(length(rvals$queryInput))})
  #  observe({print(length(rvals$labelsInput))})

    # Update reactive outputs based on selected inputs
    observe({
      x <- rvals$labelsInput
      # Plot label text input
      updateTextAreaInput(
        session,
        inputId = "labelsInput",
        value = x
      )
    })

    # Store input variables in rvals object to share between modules
    # Reactive dataset input
    observeEvent(input$datasetInput, {
      rvals$datasetInput = input$datasetInput
    })
    # Reactive query input
    observeEvent(input$queryInput, {
      rvals$queryInput = input$queryInput
    })
    # Reactive media input
    observeEvent(input$mediaInput, {
      rvals$mediaInput = input$mediaInput
    })
    # Reactive FDR input
    observeEvent(input$fdrInput, {
      rvals$fdrInput = input$fdrInput
    })
    # Reactive positive plot colour input
    observeEvent(input$posColInput, {
      rvals$posColInput = input$posColInput
    })
    # Reactive negative plot colour input
    observeEvent(input$negColInput, {
      rvals$negColInput = input$negColInput
    })
    # Reactive plot label type input
    observeEvent(input$typeInput, {
      rvals$typeInput = input$typeInput
    })
    # Reactive plot reference line input
    observeEvent(input$lineInput, {
      rvals$lineInput = input$lineInput
    })
  })
}
