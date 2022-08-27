#' Mapping Dash Component
#' @import shinydashboard
#' @import shiny
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @import leaflet
#' @import leaflet.extras
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyjs reset
#' @importFrom shinyjs alert
#' @importFrom shinyjs useShinyjs
#' @import ggplot2
#' @import dplyr
#' @import readxl
#' @import reactable
#' @import writexl
#' @importFrom xfun file_ext
#' @importFrom berryFunctions is.error
#' @importFrom plotly plotlyOutput
#' @importFrom plotly style
#' @importFrom plotly layout
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @import htmltools
#' @importFrom shiny div
#' @importFrom shiny downloadHandler
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny need
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveVal
#' @importFrom shiny renderText
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny validate
#' @importFrom shiny a
#' @importFrom shiny actionButton
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny downloadLink
#' @importFrom shiny em
#' @importFrom shiny fileInput
#' @importFrom shiny fluidRow
#' @importFrom shiny h1
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny p
#' @importFrom shiny radioButtons
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny strong
#' @importFrom shiny tabPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny shinyApp
#' @importFrom shiny textOutput
#' @import methods
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils zip
#' @importFrom stats family
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats na.exclude
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats residuals
#' @importFrom bslib is_bs_theme
NULL


mappingDashboard <- function() {

  shinydashboard::tabItem(tabName = "dashboard",

          shiny::h1(shiny::strong("Mapping Dashboard")),

          shiny::p("Perform geospatial analysis based on collection locations for run qPCR samples.",
            style = "font-size:16px;"),

          # fluidRow(
          #
          #   #Adding static valueboxes
          #   valueBoxOutput("sampleBox",  width = 3),
          #   valueBoxOutput("platformBox", width = 3),
          #   valueBoxOutput("taxonBox", width = 3),
          #   valueBoxOutput("assayBox", width = 3)),

          shiny::fluidRow(

            shiny::column(width = 12,
                   shinydashboard::box(width = NULL, solidHeader = TRUE,
                       shinycssloaders::withSpinner(leaflet::leafletOutput("mymap", height = 700))))),
          shiny::br(),

          #Second Row on page for filter functions
          shiny::fluidRow(

            htmltools::div(style='height:400px; overflow-y: scroll',
                shinydashboard::box( width = 12,
                     title = "Filter Options",
                     status = "warning",
                     solidHeader = TRUE,
                     collapsible = T,
                     shiny::column(2,

                            #Radio button to select what type of Cq value you want to based on threshold vaue (User provided or system calculated)
                            shiny::radioButtons("thresholdValueButton",
                                         "Select Threshold Value to view associated Cq Value:",
                                         c("User Provided Threshold" = 10,
                                           "System Calculated Threshold" = 12)),

                            #Slider for Cq intensity
                            shiny::sliderInput("range", "Select the Cq Intensity",
                                        min = 0,
                                        max = 40,
                                        step = 0.10, value=c(0,40)),

                            #Slider for date
                            shiny::sliderInput("date_input",
                                        "Select Event Date Range",
                                        min =as.Date("2010-01-01", "%Y-%m-%d"),
                                        max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                        value = range(c(as.Date("2010-01-01", "%Y-%m-%d"), as.Date(Sys.Date(), "%Y-%m-%d"))),
                                        timeFormat = "%Y-%m-%d",
                                        step = 1),

                            #Dropdown menu for Continent
                            shinyWidgets::pickerInput(inputId = "continent_input",
                                        "Continent",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for countrys
                            shinyWidgets::pickerInput(inputId = "country_input", "Country",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for state or province
                            shinyWidgets::pickerInput(inputId = "stateProvince_input",
                                        "State or Province",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for locality
                            shinyWidgets::pickerInput(inputId = "locality_input",
                                        "Locality",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE)


                     ),

                     shiny::column(4, offset = 1,


                            #Dropdown menu for family type
                            shinyWidgets::pickerInput(inputId = "family_input",
                                        "Family",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for genus type
                            shinyWidgets::pickerInput(inputId = "genus_input",
                                        "Genus",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for species type
                            shinyWidgets::pickerInput(inputId = "species_input",
                                        "Species",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for ct intensity
                            shinyWidgets::pickerInput(inputId = "CqIntensity_input",
                                        "Cq Intensity",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for Machine type
                            shinyWidgets::pickerInput(inputId = "machine_input",
                                        "Machine Type",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE)

                     ),

                     shiny::column(4, offset = 1,

                            #Dropdown menu for Target gene
                            shinyWidgets::pickerInput(inputId = "targetGene_input",
                                        "Target Gene",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for project ID
                            shinyWidgets::pickerInput(inputId = "projectID_input", "Project",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),


                            #Dropdown menu for assay
                            shinyWidgets::pickerInput(inputId = "assay_input", "Assay",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE),

                            #Dropdown menu for establishment means
                            shinyWidgets::pickerInput(inputId = "establishmentMeans_input",
                                        "Establishment Means",
                                        choices = "None",
                                        selected = "None",
                                        options = list('actions-box' = TRUE),
                                        multiple = TRUE))

                ),

                #Download Mapped data
                shiny::p(shiny::strong("Mapped Markers Metadata"),
                  style = "font-size:25px"),
                shiny::downloadLink("downloadFilteredData",
                             shiny::p("Download Mapped Markers Metadata",
                               style = "font-size:16px;
                                      color:#F4412E;
                                       text-decoration: underline;" )),
                #Data table for mapped data.
                shinycssloaders::withSpinner(DT::dataTableOutput("mapping_data")))
          ))
}
