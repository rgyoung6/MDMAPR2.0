#' @name launchMDMAPR
#'
#' @title Launch the MDMAPR app.
#'
#' @description This function runs the MDMAPR Shiny web application.
#'
#' @export launchMDMAPR
#'
#' @return shiny application object
#'
#' @usage launchMDMAPR()
#'


# wrapper for shiny::shinyApp()
launchMDMAPR <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer, options = list(launch.browser = TRUE))
#  shiny::shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
