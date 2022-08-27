#' @name launchApp
#'
#' @title Launch the MDMAPR app.
#'
#' @description This function runs the MDMAPR Shiny web application.
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @usage launchApp()
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
