# multiGIviewer UI
# Load packages
library(shiny)
library(shinythemes)
library(DT)

# Define UI function
ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Multi-screen genetic interaction viewer", windowTitle = "mutliGIviewer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("datasetOutput"),
      uiOutput("queryOutput"),
      uiOutput("mediaOutput"),
      uiOutput("qgiOutput"),
      uiOutput("fdrOutput"),
      uiOutput("labelOutput"),
      uiOutput("typeOutput"),
      uiOutput("lineOutput")
    ),
    mainPanel(
      downloadButton("downloadPlot", "Download plot"),
      downloadButton("downloadData", "Download table"),
      h2(textOutput("text")),
      br(),
      plotOutput("plot"),
      br(), br(),
      dataTableOutput("results")
    )
  ),
  hr(),
  div(
       class = "footer",
       includeHTML("footer.html")
   )
)
