#source('./R/dashboardBodyComponent.R')
# Define UI for application ---------------------------
shinyAppUI <- shinydashboard::dashboardPage(
  #Skin color of app
  skin = "blue",

  ##Title content
  shinydashboard::dashboardHeader(title ="MDMAPR 2.0"),

  ##Sidebar content
  shinydashboard::dashboardSidebar(width=230,
                                   shinydashboard::sidebarMenu(
                     htmltools::img(src="mdmaprlogo2.png",height=230,width=230),
                     #To allow app to use functions from shinyjs package
                     shinyjs::useShinyjs(),

                     #ID values for sidebar menu items
                     id = "tab_being_displayed",

                     #Icons for sidebar were obtained from https://fontawesome.com/icons?from=io
                     shinydashboard::menuItem("Welcome", tabName = "welcomepage", icon = shiny::icon("door-open")),
                     shinydashboard::menuItem("Data Import/Export", tabName = "dataImport", icon = shiny::icon("database")),
                     shinydashboard::menuItem("Standard Curve Analysis", tabName = "stdCurve", icon = shiny::icon("calculator")),
                     shinydashboard::menuItem("Mapping Dashboard", tabName = "dashboard", icon = shiny::icon("map")),
                     shinydashboard::menuItem("Data Overview", tabName = "qPCRDataOverviewPage", icon = shiny::icon("chart-bar"))
                   )
  ),
  ##Body content
  dashBoardBodyComponent()
)



