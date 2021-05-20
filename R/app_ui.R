#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @import shinythemes
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      theme = shinytheme("cosmo"),
      titlePanel("Multi-screen genetic interaction viewer", windowTitle = "multiGIviewer"),
      sidebarLayout(
        sidebarPanel(
          mod_sidebar_ui("sidebar", dataset)
        ),
        mainPanel(
          mod_main_ui("main")
        )
      ),
      hr(),
      div(
        class = "footer",
        includeHTML(path = app_sys("app/www/footer.html"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'multiGIviewer'
    )
  )
}
