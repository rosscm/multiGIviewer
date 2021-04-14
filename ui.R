# multiGIviewer UI
# Load packages
library(shiny)
library(shinythemes)
library(DT)

# Define UI function
ui <- fluidPage(
  # App theme via shinythemes
  theme = shinytheme("cosmo"),
  # App title
  titlePanel("Multi-screen genetic interaction viewer", windowTitle = "mutliGIviewer"),
  sidebarLayout(
    # User inputs in left side panel
    sidebarPanel(
      uiOutput("datasetOutput"),
      uiOutput("queryOutput"),
      uiOutput("mediaOutput"),
      uiOutput("qgiOutput"),
      uiOutput("fdrOutput"),
      colourpicker::colourInput("posCol", "Select darkest positive colour:", "#826f03"),
      colourpicker::colourInput("negCol", "Select darkest negative colour:", "#014e7a"),
      uiOutput("labelOutput"),
      uiOutput("typeOutput"),
      uiOutput("lineOutput")
    ),
    # Outputs in main panel
    mainPanel(
      downloadButton("downloadPlot", "Download plot"),
      downloadButton("downloadData", "Download table"),
      h2(textOutput("text")),
      br(),
      plotOutput("plot") %>% shinycssloaders::withSpinner(),
      br(), br(),
      dataTableOutput("results")
    )
  ),
  hr(),
  # HTML encoded footer
  div(
    class = "footer",
    includeHTML("footer.html")
  )
)
